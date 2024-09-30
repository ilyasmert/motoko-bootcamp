import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import HashMap "mo:base/HashMap";
import Nat64 "mo:base/Nat64";
import Hash "mo:base/Hash";
import Time "mo:base/Time";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Buffer "mo:base/Buffer";
import Types "types";
actor {

        type Result<A, B> = Result.Result<A, B>;
        type Member = Types.Member;
        type ProposalContent = Types.ProposalContent;
        type ProposalId = Types.ProposalId;
        type Proposal = Types.Proposal;
        type Vote = Types.Vote;
        type HttpRequest = Types.HttpRequest;
        type HttpResponse = Types.HttpResponse;

        // The principal of the Webpage canister associated with this DAO canister (needs to be updated with the ID of your Webpage canister)
        stable let canisterIdWebpage : Principal = Principal.fromText("aaaaa-aa");
        stable var manifesto = "Your manifesto";
        stable let name = "Your DAO";
        stable var goals = ["my goal"];

        var proposalID : Nat = 0;

        let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);
        let proposals = HashMap.HashMap<ProposalId, Proposal>(0, Nat.equal, Hash.hash);

        let MBToken = actor("bkyz2-fmaaa-aaaaa-qaaaq-cai") : actor {
                mint : (owner : Principal, amount : Nat) -> async Result.Result<(), Text>;
                burn : (owner : Principal, amount : Nat) -> async Result.Result<(), Text>;
                transfer : (from : Principal, to : Principal, amount : Nat) -> async Result.Result<(), Text>;
                balanceOf : (owner : Principal) -> async Nat;
        };

        // Returns the name of the DAO
        public query func getName() : async Text {
                return name;
        };

        // Returns the manifesto of the DAO
        public query func getManifesto() : async Text {
                return manifesto;
        };

        // Returns the goals of the DAO
        public query func getGoals() : async [Text] {
                return goals;
        };

        // Register a new member in the DAO with the given name and principal of the caller
        // Airdrop 10 MBC tokens to the new member
        // New members are always Student
        // Returns an error if the member already exists
        public shared ({ caller }) func registerMember(member : Member) : async Result<(), Text> {
                switch(members.get(caller)) {
                        case(null) {
                                let newMember : Member = {
                                        name = member.name;
                                        role = #Student;
                                };
                                members.put(caller, newMember);
                                ignore await MBToken.mint(caller, 10);
                                return #ok();
                        };
                        case(?member) {
                                return #err("member already exists..");
                        };
                };

                return #ok();
        };

        // Get the member with the given principal
        // Returns an error if the member does not exist
        public query func getMember(p : Principal) : async Result<Member, Text> {
                switch(members.get(p)) {
                        case(null) {
                                return #err("member does not exist..");
                        };
                        case(?member) {
                                return #ok(member);
                        };
                };
        };

        // Graduate the student with the given principal
        // Returns an error if the student does not exist or is not a student
        // Returns an error if the caller is not a mentor
        public shared ({ caller }) func graduate(student : Principal) : async Result<(), Text> {
                switch(members.get(caller)) {
                        case(null) {
                                return #err("caller is not a member..");
                        };
                        case(?member) {
                                if (member.role != #Mentor) {
                                        return #err("caller is not a mentor..");
                                };

                                switch(members.get(student)) {
                                        case(null) {
                                                return #err("student does not exist..");
                                        };
                                        case(?std) {
                                                let graduatedMember : Member = {
                                                        name = std.name;
                                                        role = #Graduate;
                                                };
                                                members.put(student, graduatedMember);
                                                return #ok();
                                        };
                                };
                        };
                };
        };

        // Create a new proposal and returns its id
        // Returns an error if the caller is not a mentor or doesn't own at least 1 MBC token
        public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
                switch(members.get(caller)) {
                        case(null) {
                                return #err("caller is not a member..");
                        };
                        case(?member) {
                                if (member.role != #Mentor) {
                                        return #err("caller is not a mentor..");
                                };

                                let balance = await MBToken.balanceOf(caller);
                                if (balance < 1) {
                                        return #err("not enough token..");
                                };

                                switch(content) {
                                        case(#AddMentor(p)) {
                                                switch(members.get(p)) {
                                                        case(null) {
                                                                return #err("candidate is not a member");
                                                        };
                                                        case(?member) {
                                                                if (member.role != #Graduate) {
                                                                        return #err("member is not a graduate..");
                                                                };
                                                        };
                                                };
                                        };
                                        case(_) {};
                                };

                                let proposal : Proposal = {
                                        id = proposalID;
                                        content;
                                        creator = caller;
                                        created = Time.now();
                                        executed = null;
                                        votes = [];
                                        voteScore = 0;
                                        status = #Open;
                                };

                                proposals.put(proposalID, proposal);
                                proposalID += 1;
                                ignore await MBToken.burn(caller, 1);
                                return #ok(proposalID - 1);
                        };
                };
        };

        // Get the proposal with the given id
        // Returns an error if the proposal does not exist
        public query func getProposal(id : ProposalId) : async Result<Proposal, Text> {
                switch(proposals.get(id)) {
                        case(null) {
                                return #err("proposal does not exist..");
                        };
                        case(?proposal) {
                                return #ok(proposal);
                        };
                };
        };

        // Returns all the proposals
        public query func getAllProposal() : async [Proposal] {
                return Iter.toArray(proposals.vals());
        };

        private func hasVoted(votes : [Vote], caller : Principal) : Bool {
                if (Array.find<Vote>(votes, func x { x.member ==  caller}) == null) {
                        return false;
                } else {
                        return true;
                };
        };

        private func executeProposal(content : ProposalContent) : () {
                switch(content) {
                        case(#ChangeManifesto(newManifesto)) {
                                manifesto := newManifesto;
                        };
                        case(#AddGoal(newGoal)) {
                                let goalsBuffer = Buffer.fromArray<Text>(goals);
                                goalsBuffer.add(newGoal);
                                goals := Buffer.toArray<Text>(goalsBuffer);
                        };
                        case(#AddMentor(p)) {
                                switch(members.get(p)) {
                                        case(?member) {
                                                let newMentor : Member = {
                                                        name = member.name;
                                                        role = #Mentor;
                                                };
                                                members.put(p, newMentor);
                                        };
                                        case(_) {};
                                };
                        };
                };
        };

        // Vote for the given proposal
        // Returns an error if the proposal does not exist or the member is not allowed to vote
        public shared ({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
                switch(members.get(caller)) {
                        case(null) {
                                return #err("caller is not a member..");
                        };
                        case(?member) {
                                if (member.role == #Student) {
                                        return #err("students are not allowed to vote..");
                                };
                                
                                switch(proposals.get(proposalId)) {
                                        case(null) {
                                                return #err("proposal does not exist..");
                                        };
                                        case(?proposal) {
                                                if (hasVoted(proposal.votes, caller)) {
                                                        return #err("a member can vote only once..");
                                                };

                                                let balance = await MBToken.balanceOf(caller);
                                                let votingPower = switch(member.role) {
                                                        case(#Graduate) {
                                                                balance;
                                                        };
                                                        case(#Mentor) {
                                                                balance * 5;
                                                        };
                                                        case(_) {
                                                                0;
                                                        };
                                                };
                                                let votingMultiplier = if (yesOrNo) {
                                                        1;
                                                } else {
                                                        -1;
                                                };
                                                let votingScore = votingPower * votingMultiplier;
                                                let newVoteScore = proposal.voteScore + votingScore;
                                                var executeTime : ?Time.Time = null;
                                                let newStatus = if (newVoteScore >= 100) {
                                                        #Accepted;
                                                } else if (newVoteScore <= -100) {
                                                        #Rejected;
                                                } else {
                                                        #Open;
                                                };

                                                switch(newStatus) {
                                                        case(#Accepted) {
                                                                executeProposal(proposal.content);
                                                                executeTime := ?Time.now();
                                                        };
                                                        case(_) {};
                                                };
                                        };
                                };
                        };
                };
                return #ok();
        };

        // Returns the Principal ID of the Webpage canister associated with this DAO canister
        public query func getIdWebpage() : async Principal {
                return canisterIdWebpage;
        };

};
