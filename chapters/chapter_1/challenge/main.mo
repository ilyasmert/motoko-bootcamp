import Buffer "mo:base/Buffer";
import Text "mo:base/Text";
actor {

    let name : Text = "HouseExpense";
    var manifesto : Text = "Let's track the expenses of our house";
    var goals = Buffer.Buffer<Text>(0);

    public shared query func getName() : async Text {
        return name;
    };

    public shared query func getManifesto() : async Text {
        return manifesto;
    };

    public func setManifesto(newManifesto : Text) : async () {
        manifesto := newManifesto;
    };

    public func addGoal(newGoal : Text) : async () {
        goals.add(newGoal);
    };

    public shared query func getGoals() : async [Text] {
        return Buffer.toArray<Text>(goals);
    };
};