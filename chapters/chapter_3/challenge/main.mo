import Result "mo:base/Result";
import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Option "mo:base/Option";
import Types "types";
actor {

    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;

    var ledger = HashMap.HashMap<Principal, Nat>(0, Principal.equal, Principal.hash);

    public query func tokenName() : async Text {
        return "4brek Token";
    };

    public query func tokenSymbol() : async Text {
        return "4bk";
    };

    public func mint(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balance = Option.get(ledger.get(owner), 0);
        ledger.put(owner, balance + amount);
        return #ok();
    };

    public func burn(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balance = Option.get(ledger.get(owner), 0);
        if (balance - amount < 0) {
            return #err("balance not enough..");
        };

        ledger.put(owner, balance - amount);
        return #ok();
    };

    public shared ({ caller }) func transfer(from : Principal, to : Principal, amount : Nat) : async Result<(), Text> {
        if (from == to) {
            return #err("can not transfer to self..");
        };

        let balanceFrom = Option.get(ledger.get(from), 0);
        if (balanceFrom - amount < 0) {
            return #err("balance not enough..");
        };

        ledger.put(from, balanceFrom - amount);

        let balanceTo = Option.get(ledger.get(to), 0);

        ledger.put(to, balanceTo + amount);

        return #ok();
    };

    public query func balanceOf(account : Principal) : async Nat {
        let balance = Option.get(ledger.get(account), 0);
        return balance;
    };

    public query func totalSupply() : async Nat {
        var total = 0;
        for (balance in ledger.vals()) {
            total += balance;
        };

        return total;
    };

};