const Functies = new Map([
["f1", new Functie("f1", "func1", "", "zzz.R", 473, 169, false, ["f3", "f4"] , ["f6", "f7"], 15)],
["f2", new Functie("f2", "func2", "f1", "zz.R", 473, 169, true, [] , [], 1)],
["f3", new Functie("f3", "func3", "", "z.R", 473, 169, false, ["f6"] , ["f1", "f7"], 5)],
["f4", new Functie("f4", "func4", "", "xyz.R", 473, 169, false, ["f7", "f8"] , ["f1"], 10)],
["f5", new Functie("f5", "func5", "", "mpz.R", 473, 169, false, [] , [], 1)],
["f6", new Functie("f6", "func6", "", "OIO.R", 473, 169, false, ["f1"] , ["f3"], 8)],
["f7", new Functie("f7", "func7", "", "sed.dre.R", 473, 169, false, ["f1", "f3"] , ["f4"], 12)],
["f8", new Functie("f8", "func8", "", "por.R", 473, 169, false, [] , ["f4"], 6)]
]);
const PkgInfo = {name: "lavaan", version: "0.7.2.3123", date: "2026-07-20"}
