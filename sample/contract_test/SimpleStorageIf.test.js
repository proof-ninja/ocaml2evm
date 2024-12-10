const assert = require('assert');
const ganache = require('ganache');
const { Web3 } = require('web3');

const web3 = new Web3(ganache.provider())

const { abi, bytecode } = require('../contracts/SimpleStorageIf.json');

describe('SimpleStorageIf', async () => {
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
    const v = await contract.methods.get(from).call();
    assert.equal(100, v);
  });

  it('setting value', async () => {
    await contract.methods.set(200).send({ from: from });
    const v = await contract.methods.get().call();
    assert.equal(200, v);
  });

  it('is less than value', async () => {
    const v = await contract.methods.lt(1, 2).call();
    assert.equal(true, v);
  });

  it('is greater than value', async () => {
    const v = await contract.methods.gt(1, 2).call();
    assert.equal(false, v);
  });

  it('is less than equal value', async () => {
    const v = await contract.methods.lte(1, 1).call();
    assert.equal(true, v);
  });

  it('is greater than equal value', async () => {
    const v = await contract.methods.gte(1, 1).call();
    assert.equal(true, v);
  });

  it('calculate xor', async () => {
    const v = await contract.methods.xor(true, true).call();
    assert.equal(false, v);
  });

  it('equalize xor2 to xor', async () => {
    const v = await contract.methods.xor(false, true).call();
    const v2 = await contract.methods.xor2(false, true).call();
    assert.equal(v, v2);
  });

  it('calculate max', async () => {
    const v = await contract.methods.max(5, 3).call();
    assert.equal(5, v);
  });
});