const assert = require('assert');
const ganache = require('ganache');
const { Web3 } = require('web3');

const web3 = new Web3(ganache.provider())

const { abi, bytecode } = require('../contracts/TwoStorage.json');

describe('TwoStorage', async () => {
  let accounts;
  let contract;
  let from;

  beforeEach(async () => {
    accounts = await web3.eth.getAccounts();
    from = accounts[0];

    contract = await new web3.eth.Contract(JSON.parse(JSON.stringify(abi)))
      .deploy({ data: bytecode })
      .send({ from: accounts[0], gas: 1000000 });

    await contract.methods.set(100, 200).send({ from: from });
  });

  it('should deploy', () => {
    assert.ok(contract.options.address);
  });

  it('getting value', async () => {
    const v = await contract.methods.get().call();
    assert.equal(100, v['0']);
    assert.equal(200, v['1']);
  });

  it('getting first & second value', async () => {
    const v1 = await contract.methods.get_fst().call();
    const v2 = await contract.methods.get_snd().call();
    assert.equal(100, v1);
    assert.equal(200, v2);
  });

  it('setting value', async () => {
    await contract.methods.set(300, 400).send({ from: from });
    const v = await contract.methods.get().call();
    assert.equal(300, v['0']);
    assert.equal(400, v['1']);
  });

  it('setting first value', async () => {
    await contract.methods.set_fst(300).send({ from: from });
    const v = await contract.methods.get().call();
    assert.equal(300, v['0']);
    assert.equal(200, v['1']);
  });

  it('setting second value', async () => {
    await contract.methods.set_snd(400).send({ from: from });
    const v = await contract.methods.get().call();
    assert.equal(100, v['0']);
    assert.equal(400, v['1']);
  });

  it('swapping value', async () => {
    await contract.methods.swap().send({ from: from });
    const v = await contract.methods.get().call();
    assert.equal(200, v['0']);
    assert.equal(100, v['1']);
  });

  it('total value', async () => {
    const v = await contract.methods.total().call();
    assert.equal(300, v);
  });

  it('fibonacci', async () => {
    await contract.methods.set(0, 1).send({ from: from });
    for (let i = 0; i < 10; i++) {
      await contract.methods.swap().send({ from: from });
      const v = await contract.methods.total().call();
      await contract.methods.set_snd(v).send({ from: from });
    }
    const v = await contract.methods.get().call();
    assert.equal(55, v['0']);
    assert.equal(89, v['1']);
  });

  it('function application: set2', async () => {
    await contract.methods.set_fst_snd(300, 400).send({ from: from });
    const v = await contract.methods.get().call();
    assert.equal(300, v['0']);
    assert.equal(400, v['1']);
  });

  it('function application: get2', async () => {
    const v = await contract.methods.get_fst_snd().call();
    assert.equal(100, v['0']);
    assert.equal(200, v['1']);
  });
});