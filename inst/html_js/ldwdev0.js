// Constructor Function for Functie objects
function Functie(id, naam, synonym, definedin, atline, lines, exported, calls, calledby, complexity) {
  this.id = id;
  this.naam = naam;
  this.synonym = synonym;
  this.definedin = definedin;
  this.atline = atline;
  this.lines = lines;
  this.complexity = complexity;
  this.exported = exported;
  this.calls = calls;
  this.nbcalls = calls.length;
  this.calledby = calledby;
  this.nbcalledby = calledby.length;
  this.name = naam;
  if (exported) this.name = naam + " *";
  this.calledbyindirect = function(allfunc) {
    let ignore = [this.id];
    let directs = ignore.concat(this.calledby);
    let result = [];
    this.calledby.forEach(function(calledbyid) {
      if (!ignore.includes(calledbyid)) {
        ignore.push(calledbyid);
        let calledbyfunc = allfunc.get(calledbyid);
        let calledbyindirect = calledbyfunc.calledbyindirect2(allfunc, ignore, directs);
        result = result.concat(calledbyindirect);
      }
    });
    return(result);
  }
  this.calledbyindirect2 = function(allfunc, ignore, directs) {
    let result = [];
    this.calledby.forEach(function(calledbyid) {
      if (!ignore.includes(calledbyid)) {
        ignore.push(calledbyid);
        if (!directs.includes(calledbyid)) {
          result.push(calledbyid);
        }
        let calledbyfunc = allfunc.get(calledbyid);
        let calledbyindirect = calledbyfunc.calledbyindirect2(allfunc, ignore, directs);
        result = result.concat(calledbyindirect);
      }
    });
    return(result);
  }
  this.callsindirect = function(allfunc) {
    let ignore = [this.id];
    let directs = ignore.concat(this.calls);
    let result = []; 
    this.calls.forEach(function(calledid) {
      if (!ignore.includes(calledid)) {
        ignore.push(calledid);
        let calledfunc = allfunc.get(calledid);
        let calledindirect = calledfunc.callsindirect2(allfunc, ignore, directs);
        result = result.concat(calledindirect);
      }
    });
    return(result);
  }
  this.callsindirect2 = function(allfunc, ignore, directs) {
    let result = [];
    this.calls.forEach(function(calledid) {
      if (!ignore.includes(calledid)) {
        ignore.push(calledid);
        if (!directs.includes(calledid)) {
          result.push(calledid);
        }
        let calledfunc = allfunc.get(calledid);
        let calledindirect = calledfunc.callsindirect2(allfunc, ignore, directs);
        result = result.concat(calledindirect);
      }
    });
    return(result);
  } 
}
