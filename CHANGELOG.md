# Changelog

### Features

* a finite string of 0 (up to 136) is always parse as the Zen Asset ([b84e44d](https://github.com/zenprotocol/Zulib/commit/b84e44dbfc8d9c8f1669c613b161470d1c981371))
* added tryMapT ([82f1bca](https://github.com/zenprotocol/Zulib/commit/82f1bca2d5784efe0568ed6eb07d5cd18683448d))
* Array.fold + Array.foldT ([0628e72](https://github.com/zenprotocol/Zulib/commit/0628e72026b7d591a4dfaa238fb2cdc2c01f195b))
* check for validity of extracted fsharp files from consistency tests ([9bb340c](https://github.com/zenprotocol/Zulib/commit/9bb340c542831dbf9b6c8e1c95d4c2fc3177702f))
* ConsistencyClean command ([0c6800e](https://github.com/zenprotocol/Zulib/commit/0c6800ee35232a17383538040d314f0237e9cfbd))
* Dictionary.foldT ([86c19b4](https://github.com/zenprotocol/Zulib/commit/86c19b432de44a886e6f480d2cbe60c808fe3828))
* Dictionary.mapT ([ff8f116](https://github.com/zenprotocol/Zulib/commit/ff8f1165957b9e6410210a4f4388b0c9022943d1))
* generalized application operator to support dependent types ([943ccd2](https://github.com/zenprotocol/Zulib/commit/943ccd2a414104062ed326253cb9736c8e07c317))
* List.chooseT ([c92b734](https://github.com/zenprotocol/Zulib/commit/c92b734e1f5d959046e18e340ba283d5b4feab7d))
* removed EX from Result ([58d2881](https://github.com/zenprotocol/Zulib/commit/58d2881f2c34f48484fcfbad7b1f05d57ba51df1))
* Sha3.updateArrayWith ([5f3741e](https://github.com/zenprotocol/Zulib/commit/5f3741ec4654dec2154267397dcd378cd540ee21))
* Sha3.updateContractId ([5614d61](https://github.com/zenprotocol/Zulib/commit/5614d61f0959cb9b49823b6fbf036af37691ccef))
* Sha3.updateLock ([a4915cd](https://github.com/zenprotocol/Zulib/commit/a4915cd2781aa506a6b031d9642751cb7c3ea2c0))
* TxSkeleton.safeMint ([e440635](https://github.com/zenprotocol/Zulib/commit/e440635752052e15ef7cd25101f1ecb0fc04b09a))
* updateListWith ([723aa83](https://github.com/zenprotocol/Zulib/commit/723aa832c09046bd2c75b71450314c038615ad8b))
* Zen.List.filterT + Zen.List.unzip ([6a360dd](https://github.com/zenprotocol/Zulib/commit/6a360dd94b52d7bd0617b70d6669c89584d39490))
* Zen.List.product ([cf7cb12](https://github.com/zenprotocol/Zulib/commit/cf7cb12e5d15a4d5b34beaf9e2d8d4119155ca78))
* Zen.List.take + Zen.List.drop ([f790e33](https://github.com/zenprotocol/Zulib/commit/f790e337fda730e730fe9e752c9e15a69dfb6684))
* Zen.List.zip ([b0ada85](https://github.com/zenprotocol/Zulib/commit/b0ada8552f8085cb5bffa2aa28d4b64501f973e6))
* Zen.List.zipWithT ([1a21eff](https://github.com/zenprotocol/Zulib/commit/1a21effb5b1f8071d8f232052cbeafda4930c1a5))
* Zen.OptionT.maybeT ([8400038](https://github.com/zenprotocol/Zulib/commit/84000380a1b0ee8ce0d6c05402a14db366416fb2))
* Zen.PublicKey ([12db585](https://github.com/zenprotocol/Zulib/commit/12db585b896d461d0ceb9e94f3effa1c5ea6e664))
* Zen.Result.handle + Zen.ResultT.handleT ([52293a4](https://github.com/zenprotocol/Zulib/commit/52293a48797ec947e24eaa2d29aec1b2e679e147))
* Zen.Sha3.Extracted ([e5be0bd](https://github.com/zenprotocol/Zulib/commit/e5be0bdf03faf6cd77aebf9723da7cc431b19098))
* Zen.Sha3.Extracted.hashWith ([469795e](https://github.com/zenprotocol/Zulib/commit/469795ea5cc421abc3c1b46a624e2b70dc72ee18))
* Zen.TxSkeleton.lockToPublicKey ([17296a0](https://github.com/zenprotocol/Zulib/commit/17296a01adc02e8d2038bf19cac7b12f800d0b21))


### Bug Fixes

* build.fsx Consistency no longer runs clearHints() ([3da883b](https://github.com/zenprotocol/Zulib/commit/3da883bd2b4c3506a96cc9def97a86945757711e))
* error when using int64 literals ([b3df06d](https://github.com/zenprotocol/Zulib/commit/b3df06dcef73d2c37f8ff86e629b148bce8d4a67))
* getAvailableTokens integer underflow ([d6264d7](https://github.com/zenprotocol/Zulib/commit/d6264d7edbc4c50d361f67343c9ce287be3170a0))
* issue with ci ([5ecb7ea](https://github.com/zenprotocol/Zulib/commit/5ecb7ea1bed4a47cc512317bce25cdd588c75452))
* metatdatafix reverted ([4e2aaf8](https://github.com/zenprotocol/Zulib/commit/4e2aaf81863013415d393b34fc3ead16579d63e3))
* numeric tests stack overflow ([4fd9dfc](https://github.com/zenprotocol/Zulib/commit/4fd9dfc2181312dc665a0283b77afa45405f67c6))
* numeric to_string and of_string ([1b1ece6](https://github.com/zenprotocol/Zulib/commit/1b1ece6cbd435251f40c5f869c44a0a653043fb4))
* removed Test from default build ([9573134](https://github.com/zenprotocol/Zulib/commit/9573134d4f745c4e8ee24faced732e86ce05f4ae))
* Sha3.updateByteArray ([905f073](https://github.com/zenprotocol/Zulib/commit/905f073c6224e245c96f8b41e6ca5b4d155658a4))


