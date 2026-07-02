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
%   cs_kernel_axiom_conflict/4 (K, UID1-C1, UID2-C2, Pair) — cross-reading finding.
%     Fires when two readings of the same kernel hold contradictory axioms.
%     UID-keyed: UID1 @< UID2 ordering distinguishes instances sharing a name.
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
    cs_has_axioms/1,
    cs_axiom_foreclosed/2
]).

:- use_module(narrative_ontology).
:- use_module(cs_kernel_registry).

% cs_axiom/3 and cs_axiom_status/2 and cs_axiom_contradiction/2 are multifile
% so constraint testsets and test fixtures can extend them.
:- multifile
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_axiom_contradiction/2.

/* ================================================================
   FIELD ACCESSORS
   ================================================================ */

%% cs_has_axioms(+UID)
%  Succeeds iff story UID has at least one cs_axiom/3 fact.
%  KEY IS THE story_uid SURROGATE (UUIDv4), NOT the constraint name —
%  cs_axiom/3 facts are UID-keyed (like cs_drift_state/3), so calling this
%  with a constraint name never fires, SILENTLY (witnessed 0/119 at the
%  constraint-name key while cs_axiom facts exist; OQ-137 sweep 2026-07-02).
cs_has_axioms(UID) :-
    narrative_ontology:cs_axiom(UID, _, _), !.

/* ================================================================
   TYPE B FINDING: cs_axiom_inconsistent/2
   ================================================================
   Fires when a single constraint holds both Atom1 and Atom2, and
   cs_axiom_contradiction(Atom1, Atom2) is declared.
   Pair is the canonical sorted pair atom1-atom2 (alphabetical, so
   each inconsistency is reported once per constraint).
   ================================================================ */

%% cs_axiom_inconsistent(+UID, -Pair)
%  Story UID holds both sides of a declared contradiction.
%  UID-keyed like cs_has_axioms/1 above (same silent wrong-key trap).
cs_axiom_inconsistent(UID, Atom1-Atom2) :-
    narrative_ontology:cs_axiom(UID, _, Atom1),
    narrative_ontology:cs_axiom(UID, _, Atom2),
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
cs_kernel_axiom_conflict(K, UID1-C1, UID2-C2, Atom1-Atom2) :-
    cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
    member(UID1-C1, Pairs), member(UID2-C2, Pairs), UID1 @< UID2,
    once((
        narrative_ontology:cs_axiom(UID1, _, Atom1),
        narrative_ontology:cs_axiom(UID2, _, Atom2),
        (   narrative_ontology:cs_axiom_contradiction(Atom1, Atom2)
        ;   narrative_ontology:cs_axiom_contradiction(Atom2, Atom1)
        )
    )).

/* ================================================================
   COMPUTED FORECLOSURE: cs_axiom_foreclosed/2
   ================================================================
   An axiom routes to foreclosed-for-classification when THREE
   authored conditions compound (each authored without a truth claim):
     1. grounding_type = empirically_contingent
        (authored by generator: is this axiom's legitimacy empirically testable?)
     2. reading's drift direction = axiom_overriding
        (authored in drift_state: the foundational premise has been substantially challenged)
     3. drift is unacknowledged
        (authored in drift_state: the authority structure has not absorbed the challenge)
   The generator correctly refuses to author "foreclosed" directly —
   that is a truth claim it cannot certify. Instead it authors three
   structural facts; the engine composes them.
   Minor magnitude is excluded: minor drift self-corrects (stable_pattern attractor)
   and does not constitute the evidential weight needed for foreclosure routing.
   ================================================================ */

%% cs_axiom_foreclosed(+UID, -Atom)
%  Computed (NOT authored). Atom is foreclosed-for-classification in reading UID when:
%  its grounding is empirically_contingent AND UID's drift is axiom_overriding +
%  non-minor magnitude + unacknowledged.
%  UID is the story_uid surrogate (UUIDv4).
cs_axiom_foreclosed(UID, Atom) :-
    narrative_ontology:cs_axiom(UID, _, Atom),
    narrative_ontology:cs_axiom_grounding(UID, Atom, empirically_contingent),
    narrative_ontology:cs_drift_state(UID, _, gap(axiom_overriding, Magnitude, false)),
    Magnitude \= minor.
