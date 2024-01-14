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

  it('add numeral', async () => {
    const v = await contract.methods.add(5, 8).call();
    assert.equal(13, v);
  });

  it('add revert', async () => {
    try {
      const v = await contract.methods.add(web3.utils.toBigInt('115792089237316195423570985008687907853269984665640564039457584007913129639935'), 8).call();
      assert.equal(web3.utils.toBigInt('115792089237316195423570985008687907853269984665640564039457584007913129639943'), v);
    } catch (e) {
      assert.equal("ContractExecutionError", e.name);
      assert.equal("CallError", e.innerError.name);
    }
  });

  it('sub numeral', async () => {
    const v = await contract.methods.sub(8, 5).call();
    assert.equal(3, v);
  });

  it('sub revert', async () => {
    try {
      const v = await contract.methods.sub(5, 8).call();
      assert.equal(-3, v);
    } catch (e) {
      assert.equal("ContractExecutionError", e.name);
      assert.equal("CallError", e.innerError.name);
    }
  });

  it('mul numeral', async () => {
    const v = await contract.methods.mul(5, 8).call();
    assert.equal(40, v);
  });

  it('mul revert', async () => {
    try {
      const v = await contract.methods.mul(web3.utils.toBigInt('340282366920938463463374607431768211456'), web3.utils.toBigInt('340282366920938463463374607431768211456')).call();
      assert.equal(web3.utils.toBigInt('115792089237316195423570985008687907853269984665640564039457584007913129639936'), v);
    } catch (e) {
      assert.equal("ContractExecutionError", e.name);
      assert.equal("CallError", e.innerError.name);
    }
  });

  it('div numeral', async () => {
    const v = await contract.methods.div(8, 5).call();
    assert.equal(1, v);
  });

  it('div revert', async () => {
    try {
      await contract.methods.div(8, 0).call();
    } catch (e) {
      assert.equal("ContractExecutionError", e.name);
      assert.equal("CallError", e.innerError.name);
    }
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