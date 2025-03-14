import Result "mo:base/Result";
import Buffer "mo:base/Buffer";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Principal "mo:base/Principal";
import Option "mo:base/Option";
import Nat64 "mo:base/Nat64";
import Hash "mo:base/Hash";
import Time "mo:base/Time";
import Array "mo:base/Array";
import Types "types";
actor {
    // For this level we need to make use of the code implemented in the previous projects.
    // The voting system will make use of previous data structures and functions.
    /////////////////
    //   TYPES    //
    ///////////////
    type Member = Types.Member;
    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;
    type Proposal = Types.Proposal;
    type ProposalContent = Types.ProposalContent;
    type ProposalId = Types.ProposalId;
    type Vote = Types.Vote;

    /////////////////
    // PROJECT #1 //
    ///////////////
    let goals = Buffer.Buffer<Text>(0);
    let name = "Motoko Bootcamp";
    var manifesto = "Empower the next generation of builders and make the DAO-revolution a reality";

    public shared query func getName() : async Text {
        return name;
    };

    public shared query func getManifesto() : async Text {
        return manifesto;
    };

    public func setManifesto(newManifesto : Text) : async () {
        manifesto := newManifesto;
        return;
    };

    public func addGoal(newGoal : Text) : async () {
        goals.add(newGoal);
        return;
    };

    public shared query func getGoals() : async [Text] {
        Buffer.toArray(goals);
    };

    /////////////////
    // PROJECT #2 //
    ///////////////
    let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);

    public shared ({ caller }) func addMember(member : Member) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                members.put(caller, member);
                return #ok();
            };
            case (?member) {
                return #err("Member already exists");
            };
        };
    };

    public shared ({ caller }) func updateMember(member : Member) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                members.put(caller, member);
                return #ok();
            };
        };
    };

    public shared ({ caller }) func removeMember() : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                members.delete(caller);
                return #ok();
            };
        };
    };

    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch (members.get(p)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                return #ok(member);
            };
        };
    };

    public query func getAllMembers() : async [Member] {
        return Iter.toArray(members.vals());
    };

    public query func numberOfMembers() : async Nat {
        return members.size();
    };

    /////////////////
    // PROJECT #3 //
    ///////////////
    let ledger = HashMap.HashMap<Principal, Nat>(0, Principal.equal, Principal.hash);

    public query func tokenName() : async Text {
        return "Motoko Bootcamp Token";
    };

    public query func tokenSymbol() : async Text {
        return "MBT";
    };

    public func mint(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balance = Option.get(ledger.get(owner), 0);
        ledger.put(owner, balance + amount);
        return #ok();
    };

    public func burn(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balance = Option.get(ledger.get(owner), 0);
        if (balance < amount) {
            return #err("Insufficient balance to burn");
        };
        ledger.put(owner, balance - amount);
        return #ok();
    };

    public shared ({ caller }) func transfer(from : Principal, to : Principal, amount : Nat) : async Result<(), Text> {
        let balanceFrom = Option.get(ledger.get(from), 0);
        let balanceTo = Option.get(ledger.get(to), 0);
        if (balanceFrom < amount) {
            return #err("Insufficient balance to transfer");
        };
        ledger.put(from, balanceFrom - amount);
        ledger.put(to, balanceTo + amount);
        return #ok();
    };

    public query func balanceOf(owner : Principal) : async Nat {
        return (Option.get(ledger.get(owner), 0));
    };

    public query func totalSupply() : async Nat {
        var total = 0;
        for (balance in ledger.vals()) {
            total += balance;
        };
        return total;
    };
    /////////////////
    // PROJECT #4 //
    ///////////////
    var proposalId : Nat64 = 0;
    let proposals = HashMap.HashMap<ProposalId, Proposal>(0, Nat64.equal, Nat64.toNat32);

    public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
        switch(members.get(caller)) {
            case(null) {
                return #err("not a member..");
            };
            case(?member) {
                let balance = Option.get(ledger.get(caller), 0);
                if (balance < 1) {
                    return #err("insufficient balance..");
                };

                let proposal : Proposal = {
                    id = proposalId;
                    content;
                    creator = caller;
                    created = Time.now();
                    executed = null;
                    votes = [];
                    voteScore = 0;
                    status = #Open;
                };

                proposals.put(proposalId, proposal);
                proposalId += 1;
                ledger.put(caller, balance - 1);
                return #ok(proposalId - 1);
            };
        };
    };

    public query func getProposal(proposalId : ProposalId) : async ?Proposal {
        return proposals.get(proposalId);
    };

    private func hasVoted(votes : [Vote], member : Principal) : Bool {
        let voted = Array.find<Vote>(votes, func(x : Vote) { x.member == member });
        if (voted != null) {
            return true;
        } else return false;
    };

    private func executeProposal(content : ProposalContent) : () {
        switch(content) {
            case(#ChangeManifesto(newManifesto)) {
                manifesto := newManifesto;
            };
            case(#AddGoal(newGoal)) {
                goals.add(newGoal);
            }
        }
    };

    public shared ({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
        switch(members.get(caller)) {
            case(null) {
                return #err("not a member..");
            };
            case(?member) {
                switch(proposals.get(proposalId)) {
                    case(null) {
                        return #err("no valid proposal..");
                    };
                    case(?proposal) {
                        if (proposal.status != #Open) {
                            return #err("Proposal is not open..");
                        };

                        if (hasVoted(proposal.votes, caller)) {
                            return #err("a member can vote only once..");
                        };

                        let balance = Option.get(ledger.get(caller), 0);
                        if (balance == 0) {
                            return #err("no voting power..");
                        };

                        let votingMultiplier = switch(yesOrNo) {
                            case(true) {1};
                            case(false) {-1};
                        };

                        let votePower = balance * votingMultiplier;
                        let votesBuffer = Buffer.fromArray<Vote>(proposal.votes);
                        let newVote : Vote = {
                            member = caller;
                            votingPower = balance;
                            yesOrNo;
                            };
                        votesBuffer.add(newVote);
                        let votesArray = Buffer.toArray(votesBuffer);
                        let newScore = proposal.voteScore + votePower;
                        var executeTime : ?Time.Time = null;
                        var newStatus = if (newScore <= -100) { 
                                #Rejected;
                            } else if (newScore >= 100) {
                                #Accepted;
                            } else {
                                #Open;
                            };

                        if (newStatus == #Accepted) {
                            executeProposal(proposal.content);
                            executeTime := ?Time.now();
                        };

                        let newProposal : Proposal = {
                            id = proposal.id;
                            content = proposal.content;
                            creator = proposal.creator;
                            created = proposal.created;
                            executed = executeTime;
                            votes = votesArray;
                            voteScore = newScore;
                            status = newStatus;
                        };

                        proposals.put(proposal.id, newProposal);
                        return #ok();
                    };
                };
            };
        };
    };

    public query func getAllProposals() : async [Proposal] {
        return Iter.toArray(proposals.vals());
    };
};