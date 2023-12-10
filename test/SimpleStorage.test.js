const assert = require('assert');
const ganache = require('ganache');
const { Web3 } = require('web3');

const web3 = new Web3(ganache.provider())

const { abi, bytecode } = require('../contracts/SimpleStorage.json');

describe('SimpleStorage', async () => {
  let accounts;
  let contract;
  let from;

  beforeEach(async () => {
    accounts = await web3.eth.getAccounts();
    from = accounts[0];

    contract = await new web3.eth.Contract(JSON.parse(JSON.stringify(abi)))
      .deploy({ data: bytecode })
      .send({ from: accounts[0], gas: 1000000 });

    await contract.methods.set(100).send({ from: from });
  });

  it('should deploy', () => {
    assert.ok(contract.options.address);
  });

  it('getting value', async () => {
    const v = await contract.methods.get().call();
    console.log(v);
    assert.equal(100, v);
  });

  it('setting value', async () => {
    await contract.methods.set(200).send({ from: from });
    const v = await contract.methods.get().call();
    assert.equal(200, v);
  });

  it('increment value', async () => {
    await contract.methods.incr().send({ from: from });
    const v = await contract.methods.get().call();
    assert.equal(101, v);
  });

  it('twice arg', async () => {
    const v = await contract.methods.twice(300).call();
    assert.equal(600, v);
  });

  it('anormal test', async () => {
    const v = await contract.methods.anormaltest(7).call();
    assert.equal(221, v);
  });

  it('anormal test2', async () => {
    await contract.methods.anormaltest(7).send({ from: from });
    const v = await contract.methods.get().call();
    assert.equal(221, v);
  })
});