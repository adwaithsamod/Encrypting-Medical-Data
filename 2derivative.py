import time
import avalanche

class Main :
    # Substitution boxes each string is a 32 bit hexadecimal value.
    S = [["d1310ba6", "98dfb5ac", "2ffd72db", "d01adfb7", "b8e1afed", "6a267e96", "ba7c9045", "f12c7f99", "24a19947", "b3916cf7", "0801f2e2", "858efc16", "636920d8", "71574e69", "a458fea3", "f4933d7e", "0d95748f", "728eb658", "718bcd58", "82154aee", "7b54a41d", "c25a59b5", "9c30d539", "2af26013", "c5d1b023", "286085f0", "ca417918", "b8db38ef", "8e79dcb0", "603a180e", "6c9e0e8b", "b01e8a3e", "d71577c1", "bd314b27", "78af2fda", "55605c60", "e65525f3", "aa55ab94", "57489862", "63e81440", "55ca396a", "2aab10b6", "b4cc5c34", "1141e8ce", "a15486af", "7c72e993", "b3ee1411", "636fbc2a", "2ba9c55d", "741831f6", "ce5c3e16", "9b87931e", "afd6ba33", "6c24cf5c", "7a325381", "28958677", "3b8f4898", "6b4bb9af", "c4bfe81b", "66282193", "61d809cc", "fb21a991", "487cac60", "5dec8032", "ef845d5d", "e98575b1", "dc262302", "eb651b88", "23893e81", "d396acc5", "0f6d6ff3", "83f44239", "2e0b4482", "a4842004", "69c8f04a", "9e1f9b5e", "21c66842", "f6e96c9a", "670c9c61", "abd388f0", "6a51a0d2", "d8542f68", "960fa728", "ab5133a3", "6eef0b6c", "137a3be4", "ba3bf050", "7efb2a98", "a1f1651d", "39af0176", "66ca593e", "82430e88", "8cee8619", "456f9fb4", "7d84a5c3", "3b8b5ebe", "e06f75d8", "85c12073", "401a449f", "56c16aa6", "4ed3aa62", "363f7706", "1bfedf72", "429b023d", "37d0d724", "d00a1248", "db0fead3", "49f1c09b", "075372c9", "80991b7b", "25d479d8", "f6e8def7", "e3fe501a", "b6794c3b", "976ce0bd", "04c006ba", "c1a94fb6", "409f60c4", "5e5c9ec2", "196a2463", "68fb6faf", "3e6c53b5", "1339b2eb", "3b52ec6f", "6dfc511f", "9b30952c", "cc814544", "af5ebd09", "bee3d004", "de334afd", "660f2807", "192e4bb3", "c0cba857", "45c8740f", "d20b5f39", "b9d3fbdb", "5579c0bd", "1a60320a", "d6a100c6", "402c7279", "679f25fe", "fb1fa3cc", "8ea5e9f8", "db3222f8", "3c7516df", "fd616b15", "2f501ec8", "ad0552ab", "323db5fa", "fd238760", "53317b48", "3e00df82", "9e5c57bb", "ca6f8ca0", "1a87562e", "df1769db", "d542a8f6", "287effc3", "ac6732c6", "8c4f5573", "695b27b0", "bbca58c8", "e1ffa35d", "b8f011a0", "10fa3d98", "fd2183b8", "4afcb56c", "2dd1d35b", "9a53e479", "b6f84565", "d28e49bc", "4bfb9790", "e1ddf2da", "a4cb7e33", "62fb1341", "cee4c6e8", "ef20cada", "36774c01", "d07e9efe", "2bf11fb4", "95dbda4d", "ae909198", "eaad8e71", "6b93d5a0", "d08ed1d0", "afc725e0", "8e3c5b2f", "8e7594b7", "8ff6e2fb", "f2122b64", "8888b812", "900df01c", "4fad5ea0", "688fc31c", "d1cff191", "b3a8c1ad", "2f2f2218", "be0e1777", "ea752dfe", "8b021fa1", "e5a0cc0f", "b56f74e8", "18acf3d6", "ce89e299", "b4a84fe0", "fd13e0b7", "7cc43b81", "d2ada8d9", "165fa266", "80957705", "93cc7314", "211a1477", "e6ad2065", "77b5fa86", "c75442f5", "fb9d35cf", "ebcdaf0c", "7b3e89a0", "d6411bd3", "ae1e7e49", "00250e2d", "2071b35e", "226800bb", "57b8e0af", "2464369b", "f009b91e", "5563911d", "59dfa6aa", "78c14389", "d95a537f", "207d5ba2", "02e5b9c5", "83260376", "6295cfa9", "11c81968", "4e734a41", "b3472dca", "7b14a94a", "1b510052", "9a532915", "d60f573f", "bc9bc6e4", "2b60a476", "81e67400", "08ba6fb5", "571be91f", "f296ec6b", "2a0dd915", "b6636521", "e7b9f9b6", "ff34052e", "c5855664", "53b02d5d", "a99f8fa1", "08ba4799", "6e85076a"], ["4b7a70e9", "b5b32944", "db75092e", "c4192623", "ad6ea6b0", "49a7df7d", "9cee60b8", "8fedb266", "ecaa8c71", "699a17ff", "5664526c", "c2b19ee1", "193602a5", "75094c29", "a0591340", "e4183a3e", "3f54989a", "5b429d65", "6b8fe4d6", "99f73fd6", "a1d29c07", "efe830f5", "4d2d38e6", "f0255dc1", "4cdd2086", "8470eb26", "6382e9c6", "021ecc5e", "09686b3f", "3ebaefc9", "3c971814", "6b6a70a1", "687f3584", "52a0e286", "b79c5305", "aa500737", "3e07841c", "7fdeae5c", "8e7d44ec", "5716f2b8", "b03ada37", "f0500c0d", "f01c1f04", "0200b3ff", "ae0cf51a", "3cb574b2", "25837a58", "dc0921bd", "d19113f9", "7ca92ff6", "94324773", "22f54701", "3ae5e581", "37c2dadc", "c8b57634", "9af3dda7", "a9446146", "0fd0030e", "ecc8c73e", "a4751e41", "e238cd99", "3bea0e2f", "3280bba1", "183eb331", "4e548b38", "4f6db908", "6f420d03", "f60a04bf", "2cb81290", "24977c79", "5679b072", "bcaf89af", "de9a771f", "d9930810", "b38bae12", "dccf3f2e", "5512721f", "2e6b7124", "501adde6", "9f84cd87", "7a584718", "7408da17", "bc9f9abc", "e94b7d8c", "ec7aec3a", "db851dfa", "63094366", "c464c3d2", "ef1c1847", "3215d908", "dd433b37", "24c2ba16", "12a14d43", "2a65c451", "50940002", "133ae4dd", "71dff89e", "10314e55", "81ac77d6", "5f11199b", "043556f1", "d7a3c76b", "3c11183b", "5924a509", "f28fe6ed", "97f1fbfa", "9ebabf2c", "1e153c6e", "86e34570", "eae96fb1", "860e5e0a", "5a3e2ab3", "771fe71c", "4e3d06fa", "2965dcb9", "99e71d0f", "803e89d6", "5266c825", "2e4cc978", "9c10b36a", "c6150eba", "94e2ea78", "a5fc3c53", "1e0a2df4", "f2f74ea7", "361d2b3d", "1939260f", "19c27960", "5223a708", "f71312b6", "ebadfe6e", "eac31f66", "e3bc4595", "a67bc883", "b17f37d1", "018cff28", "c332ddef", "be6c5aa5", "65582185", "68ab9802", "eecea50f", "db2f953b", "2aef7dad", "5b6e2f84", "1521b628", "29076170", "ecdd4775", "619f1510", "13cca830", "eb61bd96", "0334fe1e", "aa0363cf", "b5735c90", "4c70a239", "d59e9e0b", "cbaade14", "eecc86bc", "60622ca7", "9cab5cab", "b2f3846e", "648b1eaf", "19bdf0ca", "a02369b9", "655abb50", "40685a32", "3c2ab4b3", "319ee9d5", "c021b8f7", "9b540b19", "875fa099", "95f7997e", "623d7da8", "f837889a", "97e32d77", "11ed935f", "16681281", "0e358829", "c7e61fd6", "96dedfa1", "7858ba99", "57f584a5", "1b227263", "9b83c3ff", "1ac24696", "cdb30aeb", "532e3054", "8fd948e4", "6dbc3128", "58ebf2ef", "34c6ffea", "fe28ed61", "ee7c3c73", "5d4a14d9", "e864b7e3", "42105d14", "203e13e0", "45eee2b6", "a3aaabea", "db6c4f15", "facb4fd0", "c742f442", "ef6abbb5", "654f3b1d", "41cd2105", "d81e799e", "86854dc7", "e44b476a", "3d816250", "cf62a1f2", "5b8d2646", "fc8883a0", "c1c7b6a3", "7f1524c3", "69cb7492", "47848a0b", "5692b285", "095bbf00", "ad19489d", "1462b174", "23820e00", "58428d2a", "0c55f5ea", "1dadf43e", "233f7061", "3372f092", "8d937e41", "d65fecf1", "6c223bdb", "7cde3759", "cbee7460", "4085f2a7", "ce77326e", "a6078084", "19f8509e", "e8efd855", "61d99735", "a969a7aa", "c50c06c2", "5a04abfc", "800bcadc", "9e447a2e", "c3453484", "fdd56705", "0e1e9ec9", "db73dbd3", "105588cd", "675fda79", "e3674340", "c5c43465", "713e38d8", "3d28f89e", "f16dff20", "153e21e7", "8fb03d4a", "e6e39f2b", "db83adf7"], ["e93d5a68", "948140f7", "f64c261c", "94692934", "411520f7", "7602d4f7", "bcf46b2e", "d4a20068", "d4082471", "3320f46a", "43b7d4b7", "500061af", "1e39f62e", "97244546", "14214f74", "bf8b8840", "4d95fc1d", "96b591af", "70f4ddd3", "66a02f45", "bfbc09ec", "03bd9785", "7fac6dd0", "31cb8504", "96eb27b3", "55fd3941", "da2547e6", "abca0a9a", "28507825", "530429f4", "0a2c86da", "e9b66dfb", "68dc1462", "d7486900", "680ec0a4", "27a18dee", "4f3ffea2", "e887ad8c", "b58ce006", "7af4d6b6", "aace1e7c", "d3375fec", "ce78a399", "406b2a42", "20fe9e35", "d9f385b9", "ee39d7ab", "3b124e8b", "1dc9faf7", "4b6d1856", "26a36631", "eae397b2", "3a6efa74", "dd5b4332", "6841e7f7", "ca7820fb", "fb0af54e", "d8feb397", "454056ac", "ba489527", "55533a3a", "20838d87", "fe6ba9b7", "d096954b", "55a867bc", "a1159a58", "cca92963", "99e1db33", "a62a4a56", "3f3125f9", "5ef47e1c", "9029317c", "fdf8e802", "04272f70", "80bb155c", "05282ce3", "95c11548", "e4c66d22", "48c1133f", "c70f86dc", "07f9c9ee", "41041f0f", "404779a4", "5d886e17", "325f51eb", "d59bc0d1", "f2bcc18f", "41113564", "257b7834", "602a9c60", "dff8e8a3", "1f636c1b", "0e12b4c2", "02e1329e", "af664fd1", "cad18115", "6b2395e0", "333e92e1", "3b240b62", "eebeb922", "85b2a20e", "e6ba0d99", "de720c8c", "2da2f728", "d0127845", "95b794fd", "647d0862", "e7ccf5f0", "5449a36f", "877d48fa", "c39dfd27", "f33e8d1e", "0a476341", "992eff74", "3a6f6eab", "f4f8fd37", "a812dc60", "a1ebddf8", "991be14c", "db6e6b0d", "c67b5510", "6d672c37", "2765d43b", "dcd0e804", "f1290dc7", "cc00ffa3", "b5390f92", "690fed0b", "667b9ffb", "cedb7d9c", "a091cf0b", "d9155ea3", "bb132f88", "515bad24", "7b9479bf", "763bd6eb", "37392eb3", "cc115979", "8026e297", "f42e312d", "6842ada7", "c66a2b3b", "12754ccc", "782ef11c", "6a124237", "b79251e7", "06a1bbe6", "4bfb6350", "1a6b1018", "11caedfa", "3d25bdd8", "e2e1c3c9", "44421659", "0a121386", "d90cec6e", "d5abea2a", "64af674e", "da86a85f", "bebfe988", "64e4c3fe", "9dbc8057", "f0f7c086", "60787bf8", "6003604d", "d1fd8346", "f6381fb0", "7745ae04", "d736fccc", "83426b33", "f01eab71", "b0804187", "3c005e5f", "77a057be", "bde8ae24", "55464299", "bf582e61", "4e58f48f", "f2ddfda2", "f474ef38", "8789bdc2", "5366f9c3", "c8b38e74", "b475f255", "46fcd9b9", "7aeb2661", "8b1ddf84", "846a0e79", "915f95e2", "466e598e", "20b45770", "8cd55591", "c902de4c", "b90bace1", "bb8205d0", "11a86248", "7574a99e", "b77f19b6", "e0a9dc09", "662d09a1", "c4324633", "e85a1f02", "09f0be8c", "4a99a025", "1d6efe10", "1ab93d1d", "0ba5a4df", "a186f20f", "2868f169", "dcb7da83", "573906fe", "a1e2ce9b", "4fcd7f52", "50115e01", "a70683fa", "a002b5c4", "0de6d027", "9af88c27", "773f8641", "c3604c06", "61a806b5", "f0177a28", "c0f586e0", "006058aa", "30dc7d62", "11e69ed7", "2338ea63", "53c2dd94", "c2c21634", "bbcbee56", "90bcb6de", "ebfc7da1", "ce591d76", "6f05e409", "4b7c0188", "39720a3d", "7c927c24", "86e3725f", "724d9db9", "1ac15bb4", "d39eb8fc", "ed545578", "08fca5b5", "d83d7cd3", "4dad0fc4", "1e50ef5e", "b161e6f8", "a28514d9", "6c51133c", "6fd5c7e7", "56e14ec4", "362abfce", "ddc6c837", "d79a3234", "92638212", "670efa8e", "406000e0"], ["3a39ce37", "d3faf5cf", "abc27737", "5ac52d1b", "5cb0679e", "4fa33742", "d3822740", "99bc9bbe", "d5118e9d", "bf0f7315", "d62d1c7e", "c700c47b", "b78c1b6b", "21a19045", "b26eb1be", "6a366eb4", "5748ab2f", "bc946e79", "c6a376d2", "6549c2c8", "530ff8ee", "468dde7d", "d5730a1d", "4cd04dc6", "2939bbdb", "a9ba4650", "ac9526e8", "be5ee304", "a1fad5f0", "6a2d519a", "63ef8ce2", "9a86ee22", "c089c2b8", "43242ef6", "a51e03aa", "9cf2d0a4", "83c061ba", "9be96a4d", "8fe51550", "ba645bd6", "2826a2f9", "a73a3ae1", "4ba99586", "ef5562e9", "c72fefd3", "f752f7da", "3f046f69", "77fa0a59", "80e4a915", "87b08601", "9b09e6ad", "3b3ee593", "e990fd5a", "9e34d797", "2cf0b7d9", "022b8b51", "96d5ac3a", "017da67d", "d1cf3ed6", "7c7d2d28", "1f9f25cf", "adf2b89b", "5ad6b472", "5a88f54c", "e029ac71", "e019a5e6", "47b0acfd", "ed93fa9b", "e8d3c48d", "283b57cc", "f8d56629", "79132e28", "785f0191", "ed756055", "f7960e44", "e3d35e8c", "15056dd4", "88f46dba", "03a16125", "0564f0bd", "c3eb9e15", "3c9057a2", "97271aec", "a93a072a", "1b3f6d9b", "1e6321f5", "f59c66fb", "26dcf319", "7533d928", "b155fdf5", "03563482", "8aba3cbb", "28517711", "c20ad9f8", "abcc5167", "ccad925f", "4de81751", "3830dc8e", "379d5862", "9320f991", "ea7a90c2", "fb3e7bce", "5121ce64", "774fbe32", "a8b6e37e", "c3293d46", "48de5369", "6413e680", "a2ae0810", "dd6db224", "69852dfd", "09072166", "b39a460a", "6445c0dd", "586cdecf", "1c20c8ae", "5bbef7dd", "1b588d40", "ccd2017f", "6bb4e3bb", "dda26a7e", "3a59ff45", "3e350a44", "bcb4cdd5", "72eacea8", "fa6484bb", "8d6612ae", "bf3c6f47", "d29be463", "542f5d9e", "aec2771b", "f64e6370", "740e0d8d", "e75b1357", "f8721671", "af537d5d", "4040cb08", "4eb4e2cc", "34d2466a", "0115af84", "e1b00428", "95983a1d", "06b89fb4", "ce6ea048", "6f3f3b82", "3520ab82", "011a1d4b", "277227f8", "611560b1", "e7933fdc", "bb3a792b", "344525bd", "a08839e1", "51ce794b", "2f32c9b7", "a01fbac9", "e01cc87e", "bcc7d1f6", "cf0111c3", "a1e8aac7", "1a908749", "d44fbd9a", "d0dadecb", "d50ada38", "0339c32a", "c6913667", "8df9317c", "e0b12b4f", "f79e59b7", "43f5bb3a", "f2d519ff", "27d9459c", "bf97222c", "15e6fc2a", "0f91fc71", "9b941525", "fae59361", "ceb69ceb", "c2a86459", "12baa8d1", "b6c1075e", "e3056a0c", "10d25065", "cb03a442", "e0ec6e0e", "1698db3b", "4c98a0be", "3278e964", "9f1f9532", "e0d392df", "d3a0342b", "8971f21e", "1b0a7441", "4ba3348c", "c5be7120", "c37632d8", "df359f8d", "9b992f2e", "e60b6f47", "0fe3f11d", "e54cda54", "1edad891", "ce6279cf", "cd3e7e6f", "1618b166", "fd2c1d05", "848fd2c5", "f6fb2299", "f523f357", "a6327623", "93a83531", "56cccd02", "acf08162", "5a75ebb5", "6e163697", "88d273cc", "de966292", "81b949d0", "4c50901b", "71c65614", "e6c6c7bd", "327a140a", "45e1d006", "c3f27b9a", "c9aa53fd", "62a80f00", "bb25bfe2", "35bdd2f6", "71126905", "b2040222", "b6cbcf7c", "cd769c2b", "53113ec0", "1640e3d3", "38abbd60", "2547adf0", "ba38209c", "f746ce76", "77afa1c5", "20756060", "85cbfe4e", "8ae88dd8", "7aaaf9b0", "4cf9aa7e", "1948c25c", "02fb8a8c", "01c36ae4", "d6ebe1f9", "90d4f869", "a65cdea0", "3f09252d", "c208e69f", "b74e6132", "ce77e25b", "578fdfe3", "3ac372e6"]]

    S1=[
    [
	   "D1310BA6","98DFB5AC","2FFD72DB","D01ADFB7","B8E1AFED","6A267E96","BA7C9045","F12C7F99",
	   "24A19947","B3916CF7","0801F2E2","858EFC16","636920D8","71574E69","A458FEA3","F4933D7E",
	   "0D95748F","728EB658","718BCD58","82154AEE","7B54A41D","C25A59B5","9C30D539","2AF26013",
	   "C5D1B023","286085F0","CA417918","B8DB38EF","8E79DCB0","603A180E","6C9E0E8B","B01E8A3E",
	   "D71577C1","BD314B27","78AF2FDA","55605C60","E65525F3","AA55AB94","57489862","63E81440",
	   "55CA396A","2AAB10B6","B4CC5C34","1141E8CE","A15486AF","7C72E993","B3EE1411","636FBC2A",
	   "2BA9C55D","741831F6","CE5C3E16","9B87931E","AFD6BA33","6C24CF5C","7A325381","28958677",
	   "3B8F4898","6B4BB9AF","C4BFE81B","66282193","61D809CC","FB21A991","487CAC60","5DEC8032",
	   "EF845D5D","E98575B1","DC262302","EB651B88","23893E81","D396ACC5","0F6D6FF3","83F44239",
	   "2E0B4482","A4842004","69C8F04A","9E1F9B5E","21C66842","F6E96C9A","670C9C61","ABD388F0",
	   "6A51A0D2","D8542F68","960FA728","AB5133A3","6EEF0B6C","137A3BE4","BA3BF050","7EFB2A98",
	   "A1F1651D","39AF0176","66CA593E","82430E88","8CEE8619","456F9FB4","7D84A5C3","3B8B5EBE",
	   "E06F75D8","85C12073","401A449F","56C16AA6","4ED3AA62","363F7706","1BFEDF72","429B023D",
	   "37D0D724","D00A1248","DB0FEAD3","49F1C09B","075372C9","80991B7B","25D479D8","F6E8DEF7",
	   "E3FE501A","B6794C3B","976CE0BD","04C006BA","C1A94FB6","409F60C4","5E5C9EC2","196A2463",
	   "68FB6FAF","3E6C53B5","1339B2EB","3B52EC6F","6DFC511F","9B30952C","CC814544","AF5EBD09",
	   "BEE3D004","DE334AFD","660F2807","192E4BB3","C0CBA857","45C8740F","D20B5F39","B9D3FBDB",
	   "5579C0BD","1A60320A","D6A100C6","402C7279","679F25FE","FB1FA3CC","8EA5E9F8","DB3222F8",
	   "3C7516DF","FD616B15","2F501EC8","AD0552AB","323DB5FA","FD238760","53317B48","3E00DF82",
	   "9E5C57BB","CA6F8CA0","1A87562E","DF1769DB","D542A8F6","287EFFC3","AC6732C6","8C4F5573",
	   "695B27B0","BBCA58C8","E1FFA35D","B8F011A0","10FA3D98","FD2183B8","4AFCB56C","2DD1D35B",
	   "9A53E479","B6F84565","D28E49BC","4BFB9790","E1DDF2DA","A4CB7E33","62FB1341","CEE4C6E8",
	   "EF20CADA","36774C01","D07E9EFE","2BF11FB4","95DBDA4D","AE909198","EAAD8E71","6B93D5A0",
	   "D08ED1D0","AFC725E0","8E3C5B2F","8E7594B7","8FF6E2FB","F2122B64","8888B812","900DF01C",
	   "4FAD5EA0","688FC31C","D1CFF191","B3A8C1AD","2F2F2218","BE0E1777","EA752DFE","8B021FA1",
	   "E5A0CC0F","B56F74E8","18ACF3D6","CE89E299","B4A84FE0","FD13E0B7","7CC43B81","D2ADA8D9",
	   "165FA266","80957705","93CC7314","211A1477","E6AD2065","77B5FA86","C75442F5","FB9D35CF",
	   "EBCDAF0C","7B3E89A0","D6411BD3","AE1E7E49","00250E2D","2071B35E","226800BB","57B8E0AF",
	   "2464369B","F009B91E","5563911D","59DFA6AA","78C14389","D95A537F","207D5BA2","02E5B9C5",
	   "83260376","6295CFA9","11C81968","4E734A41","B3472DCA","7B14A94A","1B510052","9A532915",
	   "D60F573F","BC9BC6E4","2B60A476","81E67400","08BA6FB5","571BE91F","F296EC6B","2A0DD915",
	   "B6636521","E7B9F9B6","FF34052E","C5855664","53B02D5D","A99F8FA1","08BA4799","6E85076A"
	],
	[
	   "4B7A70E9","B5B32944","DB75092E","C4192623","AD6EA6B0","49A7DF7D","9CEE60B8","8FEDB266",
	   "ECAA8C71","699A17FF","5664526C","C2B19EE1","193602A5","75094C29","A0591340","E4183A3E",
	   "3F54989A","5B429D65","6B8FE4D6","99F73FD6","A1D29C07","EFE830F5","4D2D38E6","F0255DC1",
	   "4CDD2086","8470EB26","6382E9C6","021ECC5E","09686B3F","3EBAEFC9","3C971814","6B6A70A1",
	   "687F3584","52A0E286","B79C5305","AA500737","3E07841C","7FDEAE5C","8E7D44EC","5716F2B8",
	   "B03ADA37","F0500C0D","F01C1F04","0200B3FF","AE0CF51A","3CB574B2","25837A58","DC0921BD",
	   "D19113F9","7CA92FF6","94324773","22F54701","3AE5E581","37C2DADC","C8B57634","9AF3DDA7",
	   "A9446146","0FD0030E","ECC8C73E","A4751E41","E238CD99","3BEA0E2F","3280BBA1","183EB331",
	   "4E548B38","4F6DB908","6F420D03","F60A04BF","2CB81290","24977C79","5679B072","BCAF89AF",
	   "DE9A771F","D9930810","B38BAE12","DCCF3F2E","5512721F","2E6B7124","501ADDE6","9F84CD87",
	   "7A584718","7408DA17","BC9F9ABC","E94B7D8C","EC7AEC3A","DB851DFA","63094366","C464C3D2",
	   "EF1C1847","3215D908","DD433B37","24C2BA16","12A14D43","2A65C451","50940002","133AE4DD",
	   "71DFF89E","10314E55","81AC77D6","5F11199B","043556F1","D7A3C76B","3C11183B","5924A509",
	   "F28FE6ED","97F1FBFA","9EBABF2C","1E153C6E","86E34570","EAE96FB1","860E5E0A","5A3E2AB3",
	   "771FE71C","4E3D06FA","2965DCB9","99E71D0F","803E89D6","5266C825","2E4CC978","9C10B36A",
	   "C6150EBA","94E2EA78","A5FC3C53","1E0A2DF4","F2F74EA7","361D2B3D","1939260F","19C27960",
	   "5223A708","F71312B6","EBADFE6E","EAC31F66","E3BC4595","A67BC883","B17F37D1","018CFF28",
	   "C332DDEF","BE6C5AA5","65582185","68AB9802","EECEA50F","DB2F953B","2AEF7DAD","5B6E2F84",
	   "1521B628","29076170","ECDD4775","619F1510","13CCA830","EB61BD96","0334FE1E","AA0363CF",
	   "B5735C90","4C70A239","D59E9E0B","CBAADE14","EECC86BC","60622CA7","9CAB5CAB","B2F3846E",
	   "648B1EAF","19BDF0CA","A02369B9","655ABB50","40685A32","3C2AB4B3","319EE9D5","C021B8F7",
	   "9B540B19","875FA099","95F7997E","623D7DA8","F837889A","97E32D77","11ED935F","16681281",
	   "0E358829","C7E61FD6","96DEDFA1","7858BA99","57F584A5","1B227263","9B83C3FF","1AC24696",
	   "CDB30AEB","532E3054","8FD948E4","6DBC3128","58EBF2EF","34C6FFEA","FE28ED61","EE7C3C73",
	   "5D4A14D9","E864B7E3","42105D14","203E13E0","45EEE2B6","A3AAABEA","DB6C4F15","FACB4FD0",
	   "C742F442","EF6ABBB5","654F3B1D","41CD2105","D81E799E","86854DC7","E44B476A","3D816250",
	   "CF62A1F2","5B8D2646","FC8883A0","C1C7B6A3","7F1524C3","69CB7492","47848A0B","5692B285",
	   "095BBF00","AD19489D","1462B174","23820E00","58428D2A","0C55F5EA","1DADF43E","233F7061",
	   "3372F092","8D937E41","D65FECF1","6C223BDB","7CDE3759","CBEE7460","4085F2A7","CE77326E",
	   "A6078084","19F8509E","E8EFD855","61D99735","A969A7AA","C50C06C2","5A04ABFC","800BCADC",
	   "9E447A2E","C3453484","FDD56705","0E1E9EC9","DB73DBD3","105588CD","675FDA79","E3674340",
	   "C5C43465","713E38D8","3D28F89E","F16DFF20","153E21E7","8FB03D4A","E6E39F2B","DB83ADF7"
	],
	[
	   "E93D5A68","948140F7","F64C261C","94692934","411520F7","7602D4F7","BCF46B2E","D4A20068",
	   "D4082471","3320F46A","43B7D4B7","500061AF","1E39F62E","97244546","14214F74","BF8B8840",
	   "4D95FC1D","96B591AF","70F4DDD3","66A02F45","BFBC09EC","03BD9785","7FAC6DD0","31CB8504",
	   "96EB27B3","55FD3941","DA2547E6","ABCA0A9A","28507825","530429F4","0A2C86DA","E9B66DFB",
	   "68DC1462","D7486900","680EC0A4","27A18DEE","4F3FFEA2","E887AD8C","B58CE006","7AF4D6B6",
	   "AACE1E7C","D3375FEC","CE78A399","406B2A42","20FE9E35","D9F385B9","EE39D7AB","3B124E8B",
	   "1DC9FAF7","4B6D1856","26A36631","EAE397B2","3A6EFA74","DD5B4332","6841E7F7","CA7820FB",
	   "FB0AF54E","D8FEB397","454056AC","BA489527","55533A3A","20838D87","FE6BA9B7","D096954B",
	   "55A867BC","A1159A58","CCA92963","99E1DB33","A62A4A56","3F3125F9","5EF47E1C","9029317C",
	   "FDF8E802","04272F70","80BB155C","05282CE3","95C11548","E4C66D22","48C1133F","C70F86DC",
	   "07F9C9EE","41041F0F","404779A4","5D886E17","325F51EB","D59BC0D1","F2BCC18F","41113564",
	   "257B7834","602A9C60","DFF8E8A3","1F636C1B","0E12B4C2","02E1329E","AF664FD1","CAD18115",
	   "6B2395E0","333E92E1","3B240B62","EEBEB922","85B2A20E","E6BA0D99","DE720C8C","2DA2F728",
	   "D0127845","95B794FD","647D0862","E7CCF5F0","5449A36F","877D48FA","C39DFD27","F33E8D1E",
	   "0A476341","992EFF74","3A6F6EAB","F4F8FD37","A812DC60","A1EBDDF8","991BE14C","DB6E6B0D",
	   "C67B5510","6D672C37","2765D43B","DCD0E804","F1290DC7","CC00FFA3","B5390F92","690FED0B",
	   "667B9FFB","CEDB7D9C","A091CF0B","D9155EA3","BB132F88","515BAD24","7B9479BF","763BD6EB",
	   "37392EB3","CC115979","8026E297","F42E312D","6842ADA7","C66A2B3B","12754CCC","782EF11C",
	   "6A124237","B79251E7","06A1BBE6","4BFB6350","1A6B1018","11CAEDFA","3D25BDD8","E2E1C3C9",
	   "44421659","0A121386","D90CEC6E","D5ABEA2A","64AF674E","DA86A85F","BEBFE988","64E4C3FE",
	   "9DBC8057","F0F7C086","60787BF8","6003604D","D1FD8346","F6381FB0","7745AE04","D736FCCC",
	   "83426B33","F01EAB71","B0804187","3C005E5F","77A057BE","BDE8AE24","55464299","BF582E61",
	   "4E58F48F","F2DDFDA2","F474EF38","8789BDC2","5366F9C3","C8B38E74","B475F255","46FCD9B9",
	   "7AEB2661","8B1DDF84","846A0E79","915F95E2","466E598E","20B45770","8CD55591","C902DE4C",
	   "B90BACE1","BB8205D0","11A86248","7574A99E","B77F19B6","E0A9DC09","662D09A1","C4324633",
	   "E85A1F02","09F0BE8C","4A99A025","1D6EFE10","1AB93D1D","0BA5A4DF","A186F20F","2868F169",
	   "DCB7DA83","573906FE","A1E2CE9B","4FCD7F52","50115E01","A70683FA","A002B5C4","0DE6D027",
	   "9AF88C27","773F8641","C3604C06","61A806B5","F0177A28","C0F586E0","006058AA","30DC7D62",
	   "11E69ED7","2338EA63","53C2DD94","C2C21634","BBCBEE56","90BCB6DE","EBFC7DA1","CE591D76",
	   "6F05E409","4B7C0188","39720A3D","7C927C24","86E3725F","724D9DB9","1AC15BB4","D39EB8FC",
	   "ED545578","08FCA5B5","D83D7CD3","4DAD0FC4","1E50EF5E","B161E6F8","A28514D9","6C51133C",
	   "6FD5C7E7","56E14EC4","362ABFCE","DDC6C837","D79A3234","92638212","670EFA8E","406000E0"
	],
	[
	   "3A39CE37","D3FAF5CF","ABC27737","5AC52D1B","5CB0679E","4FA33742","D3822740","99BC9BBE",
	   "D5118E9D","BF0F7315","D62D1C7E","C700C47B","B78C1B6B","21A19045","B26EB1BE","6A366EB4",
	   "5748AB2F","BC946E79","C6A376D2","6549C2C8","530FF8EE","468DDE7D","D5730A1D","4CD04DC6",
	   "2939BBDB","A9BA4650","AC9526E8","BE5EE304","A1FAD5F0","6A2D519A","63EF8CE2","9A86EE22",
	   "C089C2B8","43242EF6","A51E03AA","9CF2D0A4","83C061BA","9BE96A4D","8FE51550","BA645BD6",
	   "2826A2F9","A73A3AE1","4BA99586","EF5562E9","C72FEFD3","F752F7DA","3F046F69","77FA0A59",
	   "80E4A915","87B08601","9B09E6AD","3B3EE593","E990FD5A","9E34D797","2CF0B7D9","022B8B51",
	   "96D5AC3A","017DA67D","D1CF3ED6","7C7D2D28","1F9F25CF","ADF2B89B","5AD6B472","5A88F54C",
	   "E029AC71","E019A5E6","47B0ACFD","ED93FA9B","E8D3C48D","283B57CC","F8D56629","79132E28",
	   "785F0191","ED756055","F7960E44","E3D35E8C","15056DD4","88F46DBA","03A16125","0564F0BD",
	   "C3EB9E15","3C9057A2","97271AEC","A93A072A","1B3F6D9B","1E6321F5","F59C66FB","26DCF319",
	   "7533D928","B155FDF5","03563482","8ABA3CBB","28517711","C20AD9F8","ABCC5167","CCAD925F",
	   "4DE81751","3830DC8E","379D5862","9320F991","EA7A90C2","FB3E7BCE","5121CE64","774FBE32",
	   "A8B6E37E","C3293D46","48DE5369","6413E680","A2AE0810","DD6DB224","69852DFD","09072166",
	   "B39A460A","6445C0DD","586CDECF","1C20C8AE","5BBEF7DD","1B588D40","CCD2017F","6BB4E3BB",
	   "DDA26A7E","3A59FF45","3E350A44","BCB4CDD5","72EACEA8","FA6484BB","8D6612AE","BF3C6F47",
	   "D29BE463","542F5D9E","AEC2771B","F64E6370","740E0D8D","E75B1357","F8721671","AF537D5D",
	   "4040CB08","4EB4E2CC","34D2466A","0115AF84","E1B00428","95983A1D","06B89FB4","CE6EA048",
	   "6F3F3B82","3520AB82","011A1D4B","277227F8","611560B1","E7933FDC","BB3A792B","344525BD",
	   "A08839E1","51CE794B","2F32C9B7","A01FBAC9","E01CC87E","BCC7D1F6","CF0111C3","A1E8AAC7",
	   "1A908749","D44FBD9A","D0DADECB","D50ADA38","0339C32A","C6913667","8DF9317C","E0B12B4F",
	   "F79E59B7","43F5BB3A","F2D519FF","27D9459C","BF97222C","15E6FC2A","0F91FC71","9B941525",
	   "FAE59361","CEB69CEB","C2A86459","12BAA8D1","B6C1075E","E3056A0C","10D25065","CB03A442",
	   "E0EC6E0E","1698DB3B","4C98A0BE","3278E964","9F1F9532","E0D392DF","D3A0342B","8971F21E",
	   "1B0A7441","4BA3348C","C5BE7120","C37632D8","DF359F8D","9B992F2E","E60B6F47","0FE3F11D",
	   "E54CDA54","1EDAD891","CE6279CF","CD3E7E6F","1618B166","FD2C1D05","848FD2C5","F6FB2299",
	   "F523F357","A6327623","93A83531","56CCCD02","ACF08162","5A75EBB5","6E163697","88D273CC",
	   "DE966292","81B949D0","4C50901B","71C65614","E6C6C7BD","327A140A","45E1D006","C3F27B9A",
	   "C9AA53FD","62A80F00","BB25BFE2","35BDD2F6","71126905","B2040222","B6CBCF7C","CD769C2B",
	   "53113EC0","1640E3D3","38ABBD60","2547ADF0","BA38209C","F746CE76","77AFA1C5","20756060",
	   "85CBFE4E","8AE88DD8","7AAAF9B0","4CF9AA7E","1948C25C","02FB8A8C","01C36AE4","D6EBE1F9",
	   "90D4F869","A65CDEA0","3F09252D","C208E69F","B74E6132","CE77E25B","578FDFE3","3AC372E6"
	]
]




    # Subkeys initialisation with digits of pi.
    P = ["243f6a88", "85a308d3", "13198a2e", "03707344", "a4093822", "299f31d0", "082efa98", "ec4e6c89", "452821e6", "38d01377", "be5466cf", "34e90c6c", "c0ac29b7", "c97c50dd", "3f84d5b5", "b5470917", "9216d5d9", "8979fb1b"]
    # to store 2^32(for addition modulo 2^32).
    modVal = 1
    # to convert hexadecimal to binary.



    def  xor(self, a,  b) :
        a=int(a,16)
        b=int(b,16)
        ans=a^b
        return (hex(ans)[2:])


    # addition modulo 2^32 of two hexadecimal strings.
    def  addBin(self, a,  b, plainText) :
        n1 = int(a,16)
        n2 = int(b,16)
        n1 = (n1 + n2) % self.modVal
        ans = hex(n1)[2:]
        while(len(ans)<len(plainText)):
            # print(ans,len(ans))
            ans="0"+ans
        # print(ans,len(ans),plainText,len(plainText))
        return ans

      # generate subkeys.
    def keyGenerate(self, key) :
        j = 0
        i = 0
        while (i < len(self.P)) :
            self.P[i] = self.xor(self.P[i], key[j:j + 8])
            # print(len(key))
            while(len(self.P[i])<len(key)/2):
                self.P[i]="0"+self.P[i]
            # print("subkey " + str((i + 1)) + ": " + (self.P[i]))
            j = (j + 8) % len(key)
            i += 1


    # function F explained above.
    def  f(self, plainText) :
        a = [None] * (4)
        ans = ""
        i = 0
        while (i < 8) :
            # the column number for S-box
            # is 8-bit value(8*4 = 32 bit plain text)
            col = int(plainText[i:i+2],16)
            a[int(i / 2)] = self.S[int(i / 2)][int(col)]
            i += 2
        ans = self.addBin(a[0], a[1],plainText)
        ans = self.xor(ans, a[2])
        ans = self.addBin(ans, a[3],plainText)


        # ans1 = self.xor(a[0],a[2])
        # ans2 = self.xor(a[3],a[1])
        # ans1 = self.addBin(ans1,ans2,plainText)
        # ans = self.xor(ans,ans1)
        # print(len(a[0]),len(a[1]),len(a[2]),len(a[3]))
        return ans
    def  f1(self, plainText) :
        a = [None] * (4)
        ans = ""
        i = 0
        while (i < 8) :
            # the column number for S-box
            # is 8-bit value(8*4 = 32 bit plain text)
            col = int(plainText[i:i+2],16)
            if(i/2==1):
                a[int(i / 2)] = str(int(self.S[int(i / 2)][int(col)],16)<<1)
            elif(i/2==2):
                a[int(i / 2)] = str(int(self.S[int(i / 2)][int(col)],16)>>1)
            elif(i/2==3):
                a[int(i / 2)] = self.S[int(i / 2)][(int(col)<<1)%256]
            else:
                a[int(i / 2)] = self.S[int(i / 2)][int(col)]
            i += 2
        ans = self.addBin(a[0], a[1],plainText)
        ans = self.xor(ans, a[2])
        ans = self.addBin(ans, a[3],plainText)
        return ans

    def  f2(self, plainText) :
        a = [None] * (4)
        ans = ""
        i = 0
        while (i < 8) :
            # the column number for S-box
            # is 8-bit value(8*4 = 32 bit plain text)
            col = int(plainText[i:i+2],16)
            if(i/2==1):
                a[int(i / 2)] = str(int(self.S1[int(i / 2)][int(col)],16)<<1)
            elif(i/2==2):
                a[int(i / 2)] = str(int(self.S1[int(i / 2)][int(col)],16)>>1)
            elif(i/2==3):
                a[int(i / 2)] = self.S1[int(i / 2)][(int(col)<<1)%256]
            else:
                a[int(i / 2)] = self.S1[int(i / 2)][int(col)]
            i += 2
        ans = self.addBin(a[0], a[1],plainText)
        ans = self.xor(ans, a[2])
        ans = self.addBin(ans, a[3],plainText)

        # ans1 = self.xor(a[0],a[2])
        # ans2 = self.xor(a[3],a[1])
        # ans1 = self.addBin(ans1,ans2,plainText)
        # ans = self.xor(ans,ans1)
        # print(len(a[0]),len(a[1]),len(a[2]),len(a[3]))
        return ans

            
    # round function
    def  round(self, time,  plainText) :
        left = None
        right = None
        # print(plainText)
        left = plainText[0:16]
        right = plainText[16:32]
        left = self.xor(left, self.P[time])

        # left = self.xor(left, self.P[time])
        while(len(left)<len(plainText)/2):
            left="0"+left
        
        fOut1 = self.f1(left[0:8])
        fOut2 = self.f2(left[8:16])
        fOut = fOut1 + fOut2
        # fOut = self.f(left)
        # print("Hey",right)
        # output from F function
        right = self.xor(fOut, right)
        while(len(right)<len(plainText)/2):
            right="0"+right
        # print(len(plainText))
        # print("round " + str(time) + ": " + right + left)
        # swap left and right
        return right + left


    def encrypt(self, plainText) :
        for i in range(16):
            plainText = self.round(i, plainText)
        #postprocessing
        right = plainText[0:16]
        left = plainText[16:32]
        right = self.xor(right, self.P[16])
        left = self.xor(left, self.P[17])
        return left + right
	
    # decryption
    def  decrypt(self, cipherText) :
        i = 17
        while (i > 1) :
            cipherText = self.round(i, cipherText)
            i -= 1
        # postprocessing
        right = cipherText[0:16]
        left = cipherText[16:32]
        right = self.xor(right, self.P[1])
        left = self.xor(left, self.P[0])
        return left + right


    def __init__(self) :
        # storing 2^32 in modVal
        # (<<1 is equivalent to multiply by 2)
        i = 0
        while (i < 32) :
            self.modVal = self.modVal << 1
            i += 1
        text = "123456789"
        key = "bb09182736ccddaa"
        self.keyGenerate(key)
        list=[]
        enctime=[]
        dectime=[]
        encsum=0
        decsum=0
        for i in range(10):
            text=str(int(text)+1)        
            plainText=str(text).encode().hex()

            print("Text:" + str(text))
            print("Plain Text:" + plainText)
            

        
            
            print("-----Encryption-----")
            t0=time.time_ns()
            cipherText = self.encrypt(plainText)
            t1=time.time_ns()
            # print(t0,t1)
            enctime.append(t1-t0)
            encsum=encsum+(t1-t0)
            # cipherText = self.encrypt(text)
            print("Cipher Text: " + cipherText)
            list.append(cipherText)
            # print(bytes.fromhex(cipherText).decode())
        
            
            print("-----Decryption-----")
            t0=time.time_ns()
            plainText = self.decrypt(cipherText)
            t1=time.time_ns()
            dectime.append(t1-t0)
            decsum=decsum+(t1-t0)

            print("Plain Text:" + plainText)
            plainText=bytes.fromhex(plainText).decode()
            print("Text: " + plainText)
        # print(list)
        aveff=[]
        sum=0
        for i in range(len(list)-1):
            a=(avalanche.Aeffect(list[i],list[i+1]))
            aveff.append(a)
            sum+=a
        print("Avalanche effects=",aveff)
        print("Average=",sum/len(aveff))
        print("Encryption times in nanoseconds=",enctime)
        print("Average encryption time=",encsum/len(enctime),"ns")
        print("Decryption times in nanoseconds=",dectime)
        print("Average decryption time=",decsum/len(dectime),"ns")


    @staticmethod
    def main( args) :
        Main()
    

if __name__=="__main__":
    Main.main([])