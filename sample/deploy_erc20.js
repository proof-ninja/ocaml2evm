const { Web3 } = require('web3');
const { abi, bytecode } = require('./contracts/ERC20.json');

async function deploy() {
  // connecting to an ethereum node
  const network = process.env.ETHEREUM_NETWORK;
  const web3 = new Web3(
    new Web3.providers.HttpProvider(
      `https://eth-${network}.g.alchemy.com/v2/${process.env.ALCHEMY_API_KEY}`,
    ),
  );

  // generating a signing account from a private key
  const signer = web3.eth.accounts.privateKeyToAccount(
    '0x' + process.env.SIGNER_PRIVATE_KEY,
  );
  web3.eth.accounts.wallet.add(signer);

  // deploying the contract
  const contract = new web3.eth.Contract(abi);
  const deployed = contract.deploy({ data: bytecode });
  let gas = await deployed.estimateGas();
  const diff = web3.utils.toBigInt(100000);
  const deployedContract = await deployed
    .send({
      from: signer.address,
      gas: gas + diff,
    })
    .once("transactionHash", (txhash) => {
      console.log(`Etherscan url: https://${network}.etherscan.io/tx/${txhash}`);
    });

  console.log(`Deployed contract address: ${deployedContract.options.address}`);
}

require("dotenv").config();
deploy()
  .then(() => process.exit(0))
  .catch(error => {
    console.error(error);
    process.exit(1);
  });