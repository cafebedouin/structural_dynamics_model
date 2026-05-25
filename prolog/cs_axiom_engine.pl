% ============================================================================
% CS AXIOM ENGINE
% ============================================================================
% Axiom-level commitment-system analysis.
%
% cs_axiom/3 (Constraint, Role, Atom) — asserts that Constraint holds axiom
%   Atom in Role. Role is a free label (e.g., foundational, instrumental).
%   Atom is an opaque atom naming the axiom content.
%
% cs_axiom_status/2 (Atom, Status) — Status ∈ {holdable, overridden, foreclosed}.
%   holdable:   the axiom can be coherently held given current structural data.
%   overridden: another axiom has displaced it (still named, not active).
%   foreclosed: the axiom is structurally incompatible with at least one
%               cs_axiom_contradiction/2 partner currently held by some constraint.
%
% cs_axiom_contradiction/2 (Atom1, Atom2) — declares that Atom1 and Atom2
%   are mutually exclusive: no single constraint can coherently hold both.
%   The pair is symmetric; each clause covers one direction.
%
% Consumers:
%   cs_axiom_inconsistent/2 (C, Pair) — Type B finding.
%     Fires when one constraint holds both sides of a declared contradiction.
%
%   cs_kernel_axiom_conflict/4 (K, C1, C2, Pair) — cross-reading finding.
%     Fires when two readings of the same kernel hold contradictory axioms.
%     This is a FINDING, not an error — contested kernels SHOULD exhibit axiom
%     conflicts; that is the structural signature of a genuine reading contest.
%
% Exported:
%   cs_axiom_inconsistent/2
%   cs_kernel_axiom_conflict/4
%   cs_has_axioms/1
%
% ============================================================================

:- module(cs_axiom_engine, [
    cs_axiom_inconsistent/2,
    cs_kernel_axiom_conflict/4,
    cs_has_axioms/1
]).

:- use_module(narrative_ontology).
:- use_module(cs_kernel_registry).

% cs_axiom/3 and cs_axiom_status/2 and cs_axiom_contradiction/2 are multifile
% so constraint testsets and test fixtures can extend them.
:- multifile
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_contradiction/2.

/* ================================================================
   FIELD ACCESSORS
   ================================================================ */

%% cs_has_axioms(+C)
%  Succeeds iff constraint C has at least one cs_axiom/3 fact.
cs_has_axioms(C) :-
    narrative_ontology:cs_axiom(C, _, _), !.

/* ================================================================
   TYPE B FINDING: cs_axiom_inconsistent/2
   ================================================================
   Fires when a single constraint holds both Atom1 and Atom2, and
   cs_axiom_contradiction(Atom1, Atom2) is declared.
   Pair is the canonical sorted pair atom1-atom2 (alphabetical, so
   each inconsistency is reported once per constraint).
   ================================================================ */

%% cs_axiom_inconsistent(+C, -Pair)
%  C holds both sides of a declared contradiction.
cs_axiom_inconsistent(C, Atom1-Atom2) :-
    narrative_ontology:cs_axiom(C, _, Atom1),
    narrative_ontology:cs_axiom(C, _, Atom2),
    Atom1 @< Atom2,
    (   narrative_ontology:cs_axiom_contradiction(Atom1, Atom2)
    ;   narrative_ontology:cs_axiom_contradiction(Atom2, Atom1)
    ), !.

/* ================================================================
   CROSS-READING FINDING: cs_kernel_axiom_conflict/4
   ================================================================
   Fires when two readings of the same kernel hold contradictory
   axioms. C1 @< C2 prevents symmetric duplicates.
   This is a structural finding — contested kernels are EXPECTED to
   have cross-reading axiom conflicts; the finding surface confirms
   that the readings are genuinely structurally distinct.
   ================================================================ */

%% cs_kernel_axiom_conflict(+K, -C1, -C2, -Pair)
%  Two readings of kernel K hold contradictory axioms Atom1 and Atom2.
%  Contradiction is an INDEPENDENT authored signal (cs_axiom_contradiction/2),
%  declared selectively by SCOPE — not derived from the forecloses edge.
%  This preserves the licensed-plurality vs real-closure distinction:
%    cs_kernel_axiom_conflict fires + coexists_with edge → licensed plurality
%    cs_kernel_axiom_conflict fires + forecloses edge    → real closure
%    forecloses edge, no contradiction declared           → structural pressure only
%  If contradiction were derived from forecloses, licensed plurality would be
%  structurally impossible and the negative case would be unreachable.
cs_kernel_axiom_conflict(K, C1, C2, Atom1-Atom2) :-
    cs_kernel_registry:cs_readings_for_kernel(K, Readings),
    member(C1, Readings), member(C2, Readings), C1 @< C2,
    once((
        narrative_ontology:cs_axiom(C1, _, Atom1),
        narrative_ontology:cs_axiom(C2, _, Atom2),
        (   narrative_ontology:cs_axiom_contradiction(Atom1, Atom2)
        ;   narrative_ontology:cs_axiom_contradiction(Atom2, Atom1)
        )
    )).
