:- module(narrative_ontology, [
    % Core ontology
    entity/2,
    interval/3,
    event/4,

    % CE v2.0 constraint layer
    constraint_claim/2,
    human_readable/2,
    topic_domain/2,
    recommendation/2,
    affects_constraint/2,
    cs_story_uid/2,          % cs_story_uid(+C, +UID) — name→UUID identity map
    cs_reading_relation/3,  % Typed sibling edge: cs_reading_relation(+UID, +SiblingName, +Rel)
    veto_actor/1,
    veto_exposed/2,
    constraint_metric/3,
    omega_variable/3,

    % Optional measurement layer (v3.1 coercion metrics)
    measurement/5,
    suppression_profile/2,  % OQ-46 bucketed-Backed (2026-06-11): compiler-stamped sanction that suppression was authored scalar-only by design

    % Optional intent evidence layer
    intent_viable_alternative/3,
    intent_alternative_rejected/3,
    intent_beneficiary_class/2,
    intent_power_change/3,
    intent_suppression_level/4,
    intent_resistance_level/4,
    intent_norm_strength/3,

    % Tangled rope category (Added January 2026)
    constraint_type/1,
    constraint_type_name/2,
    is_tangled_rope/1,
    has_coordination_function/1,
    has_asymmetric_extraction/1,

    % Agency-filtered beneficiary view (June 2026 — FSM agency gate)
    agent_beneficiary/2,
    non_agent_beneficiary/1,

    % Scaffold
    has_sunset_clause/1,
    
    % Boltzmann compliance layer (v5.0)
    coupling_profile/2,
    coordination_type/2,
    boltzmann_floor_override/2,

    % Coordination vitality (v7.0 — piton gate revision)
    coordination_vitality/2,

    % Validation entry point
    validate_ontology/0
]).

:- use_module(config). % Added to allow access to configuration parameters

/* ============================================================
   1. MULTIFILE & DYNAMIC DECLARATIONS
   ============================================================ */

:- multifile
    entity/2, interval/3, event/4,
    constraint_claim/2, human_readable/2, topic_domain/2, recommendation/2, affects_constraint/2,
    veto_actor/1, veto_exposed/2, constraint_metric/3, omega_variable/3,
    measurement/5, intent_viable_alternative/3, intent_alternative_rejected/3,
    measurement_basis/2,     % measurement_basis(+MID, +Basis) — OQ-102(a) per-time-point provenance; Basis ∈ {observed, projected}; compiler-emitted ONLY when authored (absent = unspecified, NEVER imputed/defaulted — the provenance bit travels with the value or not at all)
    intent_beneficiary_class/2, intent_power_change/3,
    intent_suppression_level/4, intent_resistance_level/4,
    intent_norm_strength/3, theater_ratio/2,
    suppression_profile/2,   % suppression_profile(+C, static) — compiler-stamped (generate_constraint_pl.py section 8) when other series are authored but suppression deliberately is not; an UNMARKED seriesless constraint fails closed to Backed=false (OQ-46/OQ-44)
    constraint_beneficiary/2, constraint_victim/2, input_vector/2,
    constraint_vindicates/2,  % OQ-64 split (2026-06-05): propositions a constraint vindicates are NOT beneficiaries; never feeds d/chi or beneficiary gates
    coupling_profile/2, coordination_type/2, boltzmann_floor_override/2,
    coordination_vitality/2,
    cs_story_uid/2,          % cs_story_uid(+C, +UID) — name→UUID identity map; C repeatable, UID unique per generation event
    cs_reading_relation/3,  % cs_reading_relation(+UID, +SiblingName, +Rel) — source UID-keyed; target name-keyed (stateless generation)
    cs_axiom/3,             % cs_axiom(+UID, +Role, +Atom) — normative claim held by a reading instance
    cs_axiom_status/2,      % cs_axiom_status(+Atom, +Status) — axiom-level (not UID-keyed); Status ∈ {holdable, overridden}
    cs_axiom_grounding/3,   % cs_axiom_grounding(+UID, +Atom, +GroundingType) — GroundingType ∈ {empirically_contingent, deontological, conventional, theological, instrumental}
    cs_axiom_contradiction/2, % cs_axiom_contradiction(+AtomA, +AtomB) — symmetric contradiction pair (axiom-level)
    cs_reference_frame/2,   % cs_reference_frame(+UID, +Atom) — t0: committer-axis's declared reference state
    cs_drift_state/3,       % cs_drift_state(+UID, +Moment, +gap(Direction, Magnitude, Acknowledged)) — t1 gap
    cs_created_at/2,        % cs_created_at(+UID, +Timestamp) — ISO 8601 UTC generation timestamp ('' for migrated stories)
    % Stakeholder layer (OQ-83 Phase A; compiled from stakeholders[]/six_questions)
    constraint_stakeholder/7,      % constraint_stakeholder(+C, +Name, +Role, +Power, +T, +E, +S)
    stakeholder_secondary_role/3,  % stakeholder_secondary_role(+C, +Name, +Role)
    stakeholder_non_agent/2,       % stakeholder_non_agent(+C, +Name) — excluded from beneficiary/victim derivation and d
    disappearance_verdict/2,       % disappearance_verdict(+C, +V) — Q5; V ∈ {world_rearranges, world_unchanged, contested}
    founding_problem_status/2,     % founding_problem_status(+C, +S) — R5; S ∈ {live, dead, contested}; NEVER consumed as a claim, mismatch-only
    % OQ-92 receipt surface (step 3 Stage B; tri-valued: named-seat/diffuse/ABSENT-fail-closed)
    stakeholder_gain_flow/2,       % stakeholder_gain_flow(+C, +Receiver) — Receiver = seat name (capturer) | diffuse (authored no-capture). NEVER synthesized (fabrication ban, OQ-92 Rulings; data_repair.pl is the named door). Receipt ≠ beneficiary-role.
    fixing_cost_class/2.           % fixing_cost_class(+C, +Class) — Class ∈ {cheap, prohibitive}; distinct field from gain_flow by ruling (b); cost never demotes capture

:- dynamic
    attribute/3, has_mandatrophy_declaration/1,
    entity/2, interval/3, event/4,
    constraint_claim/2, human_readable/2, topic_domain/2, recommendation/2, affects_constraint/2,
    cs_reading_relation/3,
    veto_actor/1, veto_exposed/2, constraint_metric/3, omega_variable/3,
    measurement/5, has_sunset_clause/1,
    measurement_basis/2,
    suppression_profile/2,
    intent_viable_alternative/3, intent_alternative_rejected/3,
    intent_beneficiary_class/2, intent_power_change/3,
    intent_suppression_level/4, intent_resistance_level/4,
    intent_norm_strength/3, constraint_claim/3,
    constraint_beneficiary/2, constraint_victim/2, input_vector/2,
    constraint_vindicates/2,  % OQ-64 split (2026-06-05): propositions a constraint vindicates are NOT beneficiaries; never feeds d/chi or beneficiary gates
    coupling_profile/2, coordination_type/2, boltzmann_floor_override/2,
    coordination_vitality/2,
    cs_story_uid/2,
    cs_axiom_grounding/3,
    cs_reference_frame/2,
    cs_drift_state/3,
    cs_created_at/2,
    constraint_stakeholder/7,
    stakeholder_secondary_role/3,
    stakeholder_non_agent/2,
    disappearance_verdict/2,
    founding_problem_status/2,
    stakeholder_gain_flow/2,
    fixing_cost_class/2.

/* ============================================================
   2. VALIDATION LOGIC
   ============================================================ */
%% attribute(+Subject, +Key, +Value)
%  Generic metadata getter/setter used for indexical resolution.
%  This links the metadata check in is_indexical_resolution_declared/1 
%  to the actual stored metrics.
attribute(S, K, V) :- 
    narrative_ontology:constraint_metric(S, K, V).

%% has_mandatrophy_declaration(+Constraint)
%  A manual override flag used by check_indexical_relativity/1.
%  If a constraint is explicitly marked as 'mandatrophy' in its metadata,
%  it passes the indexical relativity gate.
has_mandatrophy_declaration(C) :-
    attribute(C, lifecycle, mandatrophy).
% OQ-83 R5 rewire (2026-06-07): the genealogy mismatch IS the mandatrophy
% declaration — founding problem dead + world rearranges on removal = the
% mandate has outlived its function (capture/zombie). This grounds the
% dangling intent of the never-emitted mandatrophy_resolved schema field
% (born dangling at the JSON-template migration, 3641ae71; see ISSUES.md
% OQ-83 A7 + KNOWN_STATE 2026-06-07). Extend-don't-fork: same consumer the
% attribute/3 path feeds. The founding-problem NARRATIVE is never read here —
% only the two authored atoms (mismatch-only consumption, R5).
has_mandatrophy_declaration(C) :-
    founding_problem_status(C, dead),
    disappearance_verdict(C, world_rearranges).

%% validate_ontology
%  Master entry point for checking Knowledge Base integrity.
validate_ontology :-
    (   validate_entities,
        validate_intervals,
        validate_events,
        validate_constraint_claims,
        validate_constraint_metrics,
        validate_measurements,
        validate_omegas,
        validate_intent
    ).

validate_entities :-
    forall(entity(ID,Type),
        ( atom(ID),
          member(Type, [powerless, powerful, 
                        institutional, analytical, class])
        -> true
        ;  format('ERROR: Invalid entity(~w,~w)~n',[ID,Type]), fail
        )).

validate_intervals :-
    forall(interval(ID,Start,End),
        ( atom(ID), integer(Start), integer(End), Start =< End
        -> true
        ;  format('ERROR: Invalid interval(~w,~w,~w)~n',[ID,Start,End]), fail
        )).

validate_events :-
    forall(event(ID,Time,Actor,Type),
        ( atom(ID), integer(Time), (entity(Actor,_) ; atom(Actor)), atom(Type)
        -> true
        ;  format('ERROR: Invalid event(~w,~w,~w,~w)~n',[ID,Time,Actor,Type]), fail
        )).

%% validate_constraint_claims
%  Updated for v3.2.4 schema expansion.
%  Added tangled_rope category (January 2026) based on empirical validation of 467 constraints.
validate_constraint_claims :-
    forall(constraint_claim(Name, Type),
        ( % Skip list-wrapped legacy claims (data artifacts from older testset format)
          (is_list(Name) ; is_list(Type))
        -> true
        ; member(Type, [mountain, rope, tangled_rope, snare, scaffold, piton, naturalized])
        -> true
        ;  format('ERROR: Ontological Violation in ~w: "~w" is not a valid constraint type.~n', [Name, Type]),
           fail
        )).

validate_constraint_metrics :-
    forall(constraint_metric(Name,Metric,Val),
        ( (constraint_claim(Name,_) ; true),
          atom(Metric),
          number(Val), Val >= 0.0, Val =< 1.0
        -> true
        ;  format('ERROR: Invalid constraint_metric(~w,~w,~w)~n',[Name,Metric,Val]), fail
        )).

validate_measurements :-
    forall(measurement(ID,Target,Metric,Time,Val),
        ( atom(ID),
          (entity(Target,_) ; interval(Target,_,_)),
          (atom(Metric) ; compound(Metric)),
          integer(Time),
          number(Val), Val >= 0.0, Val =< 1.0
        -> true
        ;  format('ERROR: Invalid measurement(~w,~w,~w,~w,~w)~n',
                  [ID,Target,Metric,Time,Val]), fail
        )).

validate_omegas :-
    forall(omega_variable(ID, Type, Desc),
        ( atom(ID),
          member(Type, [empirical, conceptual, preference]),
          (atom(Desc) ; string(Desc))
        -> true
        ;  format('ERROR: Invalid omega_variable(~w,~w,~w)~n',[ID,Type,Desc]), fail
        )).

validate_intent :-
    forall(intent_viable_alternative(I,S,A),
        ( (interval(I,_,_) ; atom(I)), (entity(S,_) ; atom(S)), atom(A)
        -> true
        ;  format('ERROR: Invalid intent_viable_alternative(~w,~w,~w)~n',[I,S,A]), fail
        )).

/* ==========================================================================
   TANGLED ROPE CATEGORY - EMPIRICAL VALIDATION (Added January 2026)
   ========================================================================== */

/**
 * TANGLED ROPE CATEGORY - EMPIRICAL VALIDATION
 *
 * Added based on corpus analysis of 467 constraints (January 2026):
 * - 168 constraints (36%) show hybrid coordination/extraction pattern
 * - Cannot be explained by indexing alone (structural hybridity is real)
 * - Pattern mining confirmed empirical necessity
 *
 * Examples from corpus:
 * - carbon_credit_markets (0.55 extraction, 0.60 suppression)
 * - academic_tenure_system (0.75 extraction, 0.60 suppression)
 * - platform_network_effects (coordination + extraction)
 *
 * Key distinction from pure types:
 * - NOT pure rope (extraction too high, >0.40)
 * - NOT pure snare (has genuine coordination function)
 * - Requires surgical reform: preserve coordination, cut extraction
 */

%% constraint_type(?Type)
%  Valid constraint types in the framework.
constraint_type(mountain).
constraint_type(rope).
constraint_type(tangled_rope).  % Validated by corpus analysis (168/467 constraints, 36%)
constraint_type(snare).
constraint_type(scaffold).
constraint_type(piton).
constraint_type(naturalized).

%% constraint_type_name(?Type, ?Name)
%  Human-readable names for constraint types.
constraint_type_name(mountain, 'Mountain (Natural Constraint)').
constraint_type_name(rope, 'Rope (Pure Coordination)').
constraint_type_name(tangled_rope, 'Tangled Rope (Hybrid Coordination/Extraction)').
constraint_type_name(snare, 'Snare (Pure Extraction)').
constraint_type_name(scaffold, 'Scaffold (Temporary Support)').
constraint_type_name(piton, 'Piton (Degraded Coordination)').
constraint_type_name(naturalized, 'Naturalized (Structurally Pervasive Extraction)').


%% is_tangled_rope(+ConstraintID)
%  Tangled rope signature detection (ontological quick-check).
%  Uses §5B epsilon thresholds for base extraction check.
%  NOTE: For full dual-threshold classification (χ AND ε), use drl_core:dr_type/3.
%  This predicate checks ontological properties only (no power-scaling).
is_tangled_rope(ConstraintID) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ConstraintID, ExtMetricName, E),
    config:param(tangled_rope_epsilon_floor, TR_E_Floor),
    config:param(tangled_rope_chi_ceil, TR_E_Ceil),
    E >= TR_E_Floor, E =< TR_E_Ceil,
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(ConstraintID, SuppMetricName, S),
    config:param(tangled_rope_suppression_floor, TR_S_Floor),
    S >= TR_S_Floor,
    domain_priors:requires_active_enforcement(ConstraintID),
    % Must have both coordination function AND extraction
    has_coordination_function(ConstraintID),
    has_asymmetric_extraction(ConstraintID).

%% constraint_captured(+C)
%  OQ-92 Stage D: a seat CAPTURES this constraint's extraction — computed
%  POSITIVELY from the authored receipt surface, true iff gain_flow names an
%  existing seat. Authored 'diffuse' or ABSENT surface => false: absence never
%  blocks benignity certification, only witnessed capture does (tri-valued
%  provenance, OQ-92 Rulings block). NEVER derived from metrics (fabrication
%  ban; data_repair.pl is the named door). Consumers: the benignity gates —
%  drl_core scaffold clause, maxent scaffold spec, signature_detection CI_Rope
%  + pure_coordination (OQ-94 rows 1-3, all ruled GATE).
constraint_captured(C) :-
    stakeholder_gain_flow(C, Receiver),
    Receiver \== diffuse,
    constraint_stakeholder(C, Receiver, _, _, _, _, _), !.

%% uncaptured(+C)
%  OQ-90: the piton-side dual of constraint_captured/1. POSITIVE authored — true
%  iff gain_flow is the literal atom 'diffuse'. NOT \+constraint_captured/1: an
%  ABSENT receipt surface is neither captured nor uncaptured (fail-closed both
%  ways — it stays FCR-subsumed, never promoted to piton). Validated cut:
%  audits/2026-06-10_gain_flow_prototype/ (PASS 8/8).
uncaptured(C) :-
    stakeholder_gain_flow(C, diffuse).

%% piton_candidate(+C)
%  OQ-90: uncaptured AND prohibitive to fix => piton-flavored (a structural
%  pin nobody profits from removing, too costly to remove). Consumed by the
%  signature_detection FCR-branch refinement (resolve_with_perspectival_check,
%  guarded by config:param(piton_refinement_enabled, 1)).
piton_candidate(C) :-
    uncaptured(C),
    fixing_cost_class(C, prohibitive).

%% transient_neglect(+C)
%  OQ-90: uncaptured AND cheap to fix => not a piton, transient neglect (no one
%  has bothered, but removal is cheap). Diagnostic predicate + evidence
%  annotation ONLY (operator ruling, 2026-06-11) — NOT a new type and NOT a
%  classification driver. The corpus cell is currently EMPTY (all live diffuse
%  claims are prohibitive); its only witness is prototype control 5.
transient_neglect(C) :-
    uncaptured(C),
    fixing_cost_class(C, cheap).

%% has_coordination_function(?ConstraintID)
%  Check if constraint solves a collective action problem.
%  Evidence: Has multiple beneficiaries or provides network effects.
%  Note: constraint_beneficiary/2 is defined in individual test files.
%  No cut — callers handle deduplication (sort/set/if-then-else).
%  A bare cut or once/1 here prevents enumeration with unbound C.
has_coordination_function(ConstraintID) :-
    constraint_beneficiary(ConstraintID, _).

%% has_asymmetric_extraction(?ConstraintID)
%  Check for asymmetric beneficiary distribution.
%  Evidence: Has victims or concentrated benefits.
%  Note: constraint_victim/2 is defined in individual test files.
%  No cut — callers handle deduplication (sort/set/if-then-else).
%  A bare cut or once/1 here prevents enumeration with unbound C.
has_asymmetric_extraction(ConstraintID) :-
    constraint_victim(ConstraintID, _).

/* ==========================================================================
   AGENCY-FILTERED BENEFICIARY VIEW (June 2026 — FSM agency gate)
   ==========================================================================
   constraint_beneficiary/2 is overloaded: most values name agents (actors or
   actor-classes that can want and gain — persons, institutions, industries,
   states), but a few name PROPOSITIONS the constraint vindicates (a doctrine,
   hypothesis, framework). A proposition cannot capture rent from enforcement,
   so proposition-kind values must not satisfy beneficiary-presence gates that
   read presence as evidence of constructed extraction (FSM,
   natural_law_signature's BeneficiaryCount==0).

   Agency test is ONTOLOGICAL (ruling 2026-06-03): is the referent agent-kind
   (a thing that can want and gain) or proposition-kind (a claim the constraint
   vindicates or entails)? Authorial purpose (detector-bait, omega-routing),
   counterfactual-ness, and placeholder-ness are generator-scoping properties
   and NEVER flip an agency tag.

   TWO-GATE PRINCIPLE for adding a non_agent_beneficiary/1 entry (standing
   rule, ruled 2026-06-03): the two tag directions fail asymmetrically. An
   AGENT tag withholds natural-law certification (fail-safe; needs gate 1
   only). A NON-AGENT entry RELEASES a certification on its host, so it needs
   BOTH gates:
     Gate 1 — ontology-true: the referent is proposition-kind.
     Gate 2 — the host independently deserves the certification the entry
       releases: host metrics AND narrative/omegas converge on genuine-law.
   Default for unlisted values: AGENT (fail-open to status quo). A new
   proposition-kind value not yet ruled keeps current FSM behavior rather than
   silently granting a natural-law certification.

   Held at gate 2 (gate-1-pass, gate-2-fail — see ISSUES.md OQ):
     - technological_inevitability_interpretation (press_reformation_causality:
       authored-OPEN omegas, self-described "suspicious" uniformity)
     - constitutional_supremacy_doctrine (statutory_debt_ceiling: scoped out;
       maxwell's metric twin — separation needs a different lever)
   A later constraint_vindicates/2 split (proposition-vindication as its own
   authored field) is the continuation of this view, not a rewrite.
   ========================================================================== */

%% non_agent_beneficiary(?Value)
%  Curated registry of proposition-kind beneficiary values. Each entry carries
%  its two-gate provenance. Do NOT add entries without the gate-2 convergence
%  read (host ε/omega/narrative) — see comment block above.

% Gate 1: a hypothesis (the entropic-universe worldview), proposition-kind.
% Gate 2: host maxwell_demon_impossibility is the witnessed genuine law —
%   MaxEnt shadow 0.990 mountain / entropy 0.031, omegas authored empty,
%   narrative asserts the Second Law. Ruled 2026-06-03.
non_agent_beneficiary(entropic_universe_hypothesis).

% Gate 1: a body of norms (legal framework), proposition-kind — ":231 the IHL
%   framework benefits from having a stable standard" is vindication-talk.
% Gate 2: moot by inertness — host humane_treatment_standard__absolute_
%   prohibition keeps an agent co-beneficiary (detainees_under_armed_conflict),
%   so FSM keeps firing; this entry releases nothing. Ruled 2026-06-03.
non_agent_beneficiary(international_humanitarian_law_framework).

%% agent_beneficiary(?ConstraintID, ?Beneficiary)
%  constraint_beneficiary/2 filtered to agent-kind referents. Consumers that
%  read beneficiary-presence as evidence of constructed extraction should read
%  THIS view; consumers whose use is agency-independent (epistemic visibility,
%  coordination-function evidence) keep the raw fact. No cut — same
%  enumeration contract as has_coordination_function/1.
agent_beneficiary(ConstraintID, Beneficiary) :-
    constraint_beneficiary(ConstraintID, Beneficiary),
    \+ non_agent_beneficiary(Beneficiary).

/* ==========================================================================
   INDEXICAL GATE (v3.1)
   ========================================================================== */

% Indexical Gate: Flags Mountains that are actually Shadow Nooses.
check_indexical_relativity(ID) :-
    constraint_indexing:constraint_classification(ID, mountain, _),
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ID, ExtMetricName, E),
    E > 0.7,
    % Require a manual "Mandatrophy Acknowledgement" in the file metadata
    \+ has_mandatrophy_declaration(ID).

% Every domain must be indexed to power-perspectives (mountain/rope/tangled_rope/snare).
% Note: With tangled_rope addition, we now have 4 categories but validation remains
% focused on ensuring at least the traditional 3-pillar coverage is maintained.
validate_indexical_completeness(ID) :-
    constraint_indexing:constraint_classification(ID, mountain, _),
    constraint_indexing:constraint_classification(ID, rope, _),
    constraint_indexing:constraint_classification(ID, snare, _),
    !.
validate_indexical_completeness(ID) :-
    format('ERROR: Perspectival Gap in ~w. Missing 3-pillar coverage.~n', [ID]),
    fail.

/* ==========================================================================
   MANDATROPHY RECONCILIATION (v3.1)
   ========================================================================== */

% is_mandatrophy_resolved/1: Explicitly standardizes the 2 residual Omegas.
is_mandatrophy_resolved(gale_shapley).        % The Algorithm is the Mandate.
is_mandatrophy_resolved(planetary_boundaries). % The Biological Limit is the Mandate.

% detect_omega/2: Identifies logical friction points.
% This rule is updated to exempt "Hardened Mandatrophy."
detect_omega(Name, mandatrophy) :-
    constraint_indexing:constraint_classification(Name, mountain, _),
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(Name, ExtMetricName, E),
    E > 0.7,
    % The Paradox: A Mountain (Fact) behaves like a Snare (Trap).
    % If the domain is recognized as Mandatrophic, it is no longer an Omega.
    \+ is_mandatrophy_resolved(Name).

% System Insight: Logic for the Parsing Suite
count_unresolved_omegas(Count) :-
    aggregate_all(count, detect_omega(_, _), Count).

% Mandatrophy Detector: Flags Mountains that function as Traps.
detect_mandatrophy_omega(ID) :-
    constraint_indexing:constraint_classification(ID, mountain, _),
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ID, ExtMetricName, E),
    E > 0.7,
    % Check for the explicit resolution marker in the file
    \+ is_indexical_resolution_declared(ID).

is_indexical_resolution_declared(ID) :-
    % Looks for the standardized resolution hook in the metadata
    attribute(ID, indexical_resolution, resolved).

% Add-ons

has_sunset_clause(_) :- fail. % Default fail if not explicitly defined in instance

% v7.0: Coordination vitality for piton gate revision.
% Status is one of: dead, degrading, active.
% Default: fail (coordination presumed alive unless declared otherwise).
% Declared in testset .pl files for pitons with dead/degrading coordination.
coordination_vitality(_, _) :- fail.

% Maps claim/2 into claim/3 for context-indexed lookups
constraint_claim(ID, Type, _Context) :- 
    constraint_claim(ID, Type).
