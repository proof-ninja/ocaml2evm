const assert = require('assert');
const ganache = require('ganache');
const { Web3 } = require('web3');

const web3 = new Web3(ganache.provider())

const { abi, bytecode } = require('../contracts/SimpleHash.json');

describe('SimpleHash', async () => {
  let accounts;
  let contract;
  let from;

  beforeEach(async () => {
    accounts = await web3.eth.getAccounts();
    from = accounts[0];

    contract = await new web3.eth.Contract(JSON.parse(JSON.stringify(abi)))
      .deploy({ data: bytecode })
      .send({ from: accounts[0], gas: 1000000 });

    await contract.methods.set(from, 100).send({ from: from });
  });

  it('should deploy', () => {
    assert.ok(contract.options.address);
  });

  it('getting value', async () => {
    const v = await contract.methods.get(from).call();
    assert.equal(100, v);
  });

  it('setting value', async () => {
    let address = accounts[1];
    await contract.methods.set(address, 200).send({ from: from });
    const v = await contract.methods.get(address).call();
    assert.equal(200, v);
  });

  it('setting value to caller', async () => {
    let address = accounts[2];
    await contract.methods.set_caller(200).send({ from: address });
    const v = await contract.methods.get(address).call();
    assert.equal(200, v);
  });
});