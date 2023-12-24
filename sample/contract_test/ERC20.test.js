const assert = require('assert');
const ganache = require('ganache');
const { Web3 } = require('web3');

const web3 = new Web3(ganache.provider())

const { abi, bytecode } = require('../contracts/ERC20.json');

describe('ERC20', async () => {
  let accounts;
  let contract;
  let from;

  beforeEach(async () => {
    accounts = await web3.eth.getAccounts();
    from = accounts[0];

    contract = await new web3.eth.Contract(JSON.parse(JSON.stringify(abi)))
      .deploy({ data: bytecode })
      .send({ from: accounts[0], gas: 1000000 });

    await contract.methods.mint(100).send({ from: from });
  });

  it('should deploy', () => {
    assert.ok(contract.options.address);
  });

  it('balance of account', async () => {
    const v = await contract.methods.balance_of(from).call();
    assert.equal(100, v);
  });

  it('new mint', async () => {
    let account = accounts[1];
    await contract.methods.mint(200).send({ from: account });
    const v = await contract.methods.balance_of(account).call();
    assert.equal(200, v);
  });

  it('additional mint', async () => {
    await contract.methods.mint(200).send({ from: from });
    const v = await contract.methods.balance_of(from).call();
    assert.equal(300, v);
  });

  it('burn', async () => {
    await contract.methods.burn(30).send({ from: from });
    const v = await contract.methods.balance_of(from).call();
    assert.equal(70, v);
  });

  it('total supply', async () => {
    await contract.methods.mint(200).send({ from: accounts[0] });
    await contract.methods.mint(100).send({ from: accounts[1] });
    await contract.methods.burn(120).send({ from: accounts[0] });
    await contract.methods.mint(300).send({ from: accounts[2] });
    await contract.methods.burn(280).send({ from: accounts[2] });
    const v = await contract.methods.total_supply().call();
    assert.equal(300, v);
  });

  it('transfer', async () => {
    let account0 = from;
    let account1 = accounts[1];
    let account2 = accounts[2];
    await contract.methods.transfer(account1, 30).send({ from: account0 });
    await contract.methods.transfer(account2, 20).send({ from: account1 });
    const v0 = await contract.methods.balance_of(account0).call();
    const v1 = await contract.methods.balance_of(account1).call();
    const v2 = await contract.methods.balance_of(account2).call();
    assert.equal(70, v0);
    assert.equal(10, v1);
    assert.equal(20, v2);
  });
});