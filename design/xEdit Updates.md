# xEdit Updates

## Various

- [x] Added `wbAdvancedNAVI` - [2cbe7e](https://github.com/matortheeternal/xedit-lib/commit/2cbe7ea6390ced0c89b5c3ca9a5e2312e7c09ef3#diff-884f2fb29b253fc525156c1bd50cd538)
- [x] Added support for 64-bit build

## wbImplementation.pas

- [ ] Improved finding records by EditorID/Name - [a919e6](https://github.com/matortheeternal/xedit-lib/commit/a919e68c918b8b5320e23a4f0168775c4f8907df#diff-884f2fb29b253fc525156c1bd50cd538), [e28176](https://github.com/matortheeternal/xedit-lib/commit/e28176370451ae47eaaddfa77f9946beb025d92e#diff-884f2fb29b253fc525156c1bd50cd538)
- [ ] Added `AddGroup` method to `IwbGroupRecord` - [149342](https://github.com/matortheeternal/xedit-lib/commit/149342425b1d8985195dbe1775e4ec0be0549032#diff-884f2fb29b253fc525156c1bd50cd538)
- [x] Added `wbAllowErrors` - [271bbf](https://github.com/matortheeternal/xedit-lib/commit/271bbf77aa86682587a70f3750ae1935f8aac032#diff-884f2fb29b253fc525156c1bd50cd538)
- [x] Improved handling of unordered subrecords - [ff0afb](https://github.com/matortheeternal/xedit-lib/commit/ff0afb3ea4cb13bfaf194e99e2976ed7694094b5#diff-884f2fb29b253fc525156c1bd50cd538), [ffaf14](https://github.com/matortheeternal/xedit-lib/commit/ffaf14ff474692acec310f4609931f1bfba2c172#diff-1c52c6c99b8608acd96b2af323687072)
- [x] Added `TwbFile.SetFileName` - [ccdf6f](https://github.com/matortheeternal/xedit-lib/commit/ccdf6fab0f5ae60aaf5f98053a7d95fb63f3cd01#diff-884f2fb29b253fc525156c1bd50cd538), [3153e9](https://github.com/matortheeternal/xedit-lib/commit/3153e9d2708e91b9920942c7633b80eb11b0676e#diff-1c52c6c99b8608acd96b2af323687072)
- [x] Added `TwbFile.SetIsEditable` - [4b023f](https://github.com/matortheeternal/xedit-lib/commit/4b023f2ebef9e79a1ea83104b46f929da4173d3d#diff-884f2fb29b253fc525156c1bd50cd538)

- [x] Fixed bug in `TwbFile.Create` which caused `fsIsHardcoded` to not be set in file states properly - [0eeb6e](https://github.com/matortheeternal/xedit-lib/commit/0eeb6e8b767820886d9e5df4809bd4ab58c5a9db#diff-884f2fb29b253fc525156c1bd50cd538) 
- [ ] Added custom element filtering system - [1731a8](https://github.com/matortheeternal/xedit-lib/commit/1731a8f6fe3efc5739562ce78516adc28e531e99#diff-884f2fb29b253fc525156c1bd50cd538), [ab4a74](https://github.com/matortheeternal/xedit-lib/commit/ab4a74a1a36a9e2f29250312374df71232abc9e8#diff-884f2fb29b253fc525156c1bd50cd538)
- [x] Assigning `TwbFormID` elements now works through native values - [3e1501](https://github.com/matortheeternal/xedit-lib/commit/3e150117d23840d736030fe475419ac60c47b253#diff-884f2fb29b253fc525156c1bd50cd538)
- [ ] Fixed resolving references to hardcoded forms - [3fa086](https://github.com/matortheeternal/xedit-lib/commit/3fa0866e708247f03a395267e840c921d83482d8#diff-1c52c6c99b8608acd96b2af323687072), [8c98b8](https://github.com/matortheeternal/xedit-lib/commit/8c98b83bc11e3b860c0814ae20a6397f8d813eee#diff-1c52c6c99b8608acd96b2af323687072)
- [x] Now returning `nil` when `TwbContainer.GetElement` is passed a negative index - [cb0969](https://github.com/matortheeternal/xedit-lib/commit/cb0969c7681a5713213be781d6679425ac776667#diff-1c52c6c99b8608acd96b2af323687072)
- [ ] Added a `searchMasters` param to `TwbFileGetRecordByFormID` - [6d0fb2](https://github.com/matortheeternal/xedit-lib/commit/6d0fb206f0419b1a504db995d88368149464987e#diff-884f2fb29b253fc525156c1bd50cd538)
- [ ] Added `TwbMainRecord.GetInjectionTarget` - [700c8d](https://github.com/matortheeternal/xedit-lib/commit/700c8dc85ca18b59ccafd2f5cc8910374ce3db50#diff-884f2fb29b253fc525156c1bd50cd538)
- [x] `TwbByteArrayDef.FromNativeValue` now accepts unsigned 32-bit integers - [27777b](https://github.com/matortheeternal/xedit-lib/commit/2777b856511f7c6e73403904489eb53c0896d649#diff-884f2fb29b253fc525156c1bd50cd538)
- [ ] Improved `wbFileForceClosed` - [8d444f](https://github.com/matortheeternal/xedit-lib/commit/8d444f233986f0e5ec2724f82fe16f76a7395b5a#diff-1c52c6c99b8608acd96b2af323687072), [2381b8](https://github.com/matortheeternal/xedit-lib/commit/2381b80521a32b2834108ea3681433d064061735#diff-1c52c6c99b8608acd96b2af323687072), [fe95b2](https://github.com/matortheeternal/xedit-lib/commit/fe95b204206b1e3939d6de7b67f50caff1a6988f#diff-1c52c6c99b8608acd96b2af323687072)
- [ ] Added first element resolution (pipe syntax) - [7eb79c](https://github.com/matortheeternal/xedit-lib/commit/7eb79ce7591f898b66e3cb5d8129bec0c4bd0fab#diff-1c52c6c99b8608acd96b2af323687072), [0ffe08](https://github.com/matortheeternal/xedit-lib/commit/0ffe08edb85f28fd7b490e07551639ec26e8b9e4#diff-1c52c6c99b8608acd96b2af323687072)
- [x] Fixed `TwbMainRecord.Add` not working with fully qualified element names - [8d6546](https://github.com/matortheeternal/xedit-lib/commit/8d654661baaa0f3cfa51e38ead0dea5d6bd53391#diff-1c52c6c99b8608acd96b2af323687072)
- [ ] Added support for setting Version Control Info - [a77e66](https://github.com/matortheeternal/xedit-lib/commit/a77e66b46a7a2bb61bda65fd24841760d0e4eac7#diff-1c52c6c99b8608acd96b2af323687072)
- [ ] Made `TwbContainer.ResolveElementName` case sensitive - [51a36a](https://github.com/matortheeternal/xedit-lib/commit/51a36ac602f76582ba986da677d75d08910820fa#diff-1c52c6c99b8608acd96b2af323687072)
- [ ] `TwbFile.BuildRef` now exits if references have already been built - [a69929](https://github.com/matortheeternal/xedit-lib/commit/a69929fa0be249dcd8895a7350e4a917c0c94594#diff-1c52c6c99b8608acd96b2af323687072)
- [ ] Added and improved indexed path support - [0b1ec4](https://github.com/matortheeternal/xedit-lib/commit/0b1ec4d1809355c128318e0b5a1ab3c7af5fc675#diff-1c52c6c99b8608acd96b2af323687072), [932ae3](https://github.com/matortheeternal/xedit-lib/commit/932ae3a67a066e2d2fcc4fdd96f700cc5a70dd54#diff-1c52c6c99b8608acd96b2af323687072), [41aa05](https://github.com/matortheeternal/xedit-lib/commit/41aa0514f135537b4fda6b4b06b4a83597b80f7b#diff-1c52c6c99b8608acd96b2af323687072), [c733ea](https://github.com/matortheeternal/xedit-lib/commit/c733ea4cbea8eaea2f340af22800d296d5b31164#diff-1c52c6c99b8608acd96b2af323687072)

## wbHelpers.pas

- [x] Fixed issue with `wbCounterContainerAfterSet` not deleting counter fields when their corresponding array gets deleted - [3c1400](https://github.com/matortheeternal/xedit-lib/commit/3c14008f7fc022967cea7baed7ec7e89f5f848f2#diff-2e19e9aa97d57dbad9528ff622aac594)

## wbBSA.pas

- [ ] Added `TwbContainerHandler.GetResourceContainer` - [ccf465](https://github.com/matortheeternal/xedit-lib/commit/ccf46582f7c3af4c7bf7b10c2277fbb87a3d3ec8#diff-884f2fb29b253fc525156c1bd50cd538), [68e618](https://github.com/matortheeternal/xedit-lib/commit/68e61813542d04b3df5ccad9ba60f6476d02c88f#diff-f56ae82a15344fb47d1e480de31553bb)
- [ ] Fixed BA2 internal file path parsing (handling forward slashes) - [9d94e7](https://github.com/matortheeternal/xedit-lib/commit/9d94e72358d3ba1874db734a7c73e21a872fd38e#diff-f56ae82a15344fb47d1e480de31553bb)
- [ ] Container order now properly corresponds to priority - [86393e](https://github.com/matortheeternal/xedit-lib/commit/86393e43b06031d64f9fac920e479e2f7218f501#diff-f56ae82a15344fb47d1e480de31553bb), [ec6e27](https://github.com/matortheeternal/xedit-lib/commit/ec6e279ef41fa8c08a9466b13161ee5ee1579500#diff-f56ae82a15344fb47d1e480de31553bb)