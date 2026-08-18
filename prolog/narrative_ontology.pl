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

    % Institutional revision authority (OQ-153) — who may amend the kernel and by
    % what procedure. Auth ∈ {licensed_revisable, frozen, absent_diffuse}. OPTIONAL:
    % an authored fact or nothing; ABSENCE ≠ absent_diffuse (unauthored is "not
    % looked at"; absent_diffuse is the substantive "nobody owns the kernel"). No
    % default is ever imputed — the value travels with the fact or not at all. No
    % consumer yet (step 2 of the OQ-153 staging).
    update_authority/2,

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
    update_authority/2,   % OQ-153 institutional revision authority (optional; absence ≠ absent_diffuse; no default)
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
    founding_problem_corroboration_class/2, % founding_problem_corroboration_class(+C, +Class) — R5 "survives daylight" axis; Class ∈ {independent, interested, ambiguous}; an AUTHORED atom (human/R5-interview judgment, NEVER parsed from the corroboration prose); absent ⇒ daylight(unstated). A SEPARATE qualifier axis: never changes the q6 crosscheck Cell (stakeholder_seats:q6_crosscheck/3).
    % OQ-92 receipt surface (step 3 Stage B; tri-valued: named-seat/diffuse/ABSENT-fail-closed)
    stakeholder_gain_flow/2,       % stakeholder_gain_flow(+C, +Receiver) — Receiver = seat name (capturer) | diffuse (authored no-capture). NEVER synthesized (fabrication ban, OQ-92 Rulings; data_repair.pl is the named door). Receipt ≠ beneficiary-role.
    fixing_cost_class/2,           % fixing_cost_class(+C, +Class) — Class ∈ {cheap, prohibitive}; distinct field from gain_flow by ruling (b); cost never demotes capture
    story_provenance/8, story_seed/3,  % Phase C provenance facts (compiler-emitted per story). MUST be multifile like every sibling above — declared dynamic-only, they loaded 1/N ("Redefined static procedure" on each testset consult, last-file-wins). Fixed 2026-06-13.
    epsilon_provenance/5,   % epsilon_provenance(+C, +ValueAsWritten, +Author, +GenerationRunId, +Route) — OQ-205 spec §3 (R2 ratified 2026-07-03). ValueAsWritten redundantly records the authored ε so drift vs constraint_metric/3 is a checkable inequality; Author = authoring-model atom or human; Route ∈ {direct, scope_bin(Bin), hand_authored, seed_inherited} (every fabrication source its own token). Compiler-emitted (generate_constraint_pl.py); generator-forward by ruling — the pre-build corpus is the declared loud-null stratum, corpus-complete arrives at rebuild.
    % OQ-68 (2026-08-18): the last two corpus-schema predicates held ONLY by writer
    % convention. Both were correct on disk — every testset that writes them also emits a
    % local `:- multifile narrative_ontology:P.` — but that is a generation-time habit, not
    % an engine guarantee: one generator revision that drops the local declaration and the
    % predicate silently reverts to the story_provenance/8 failure above (last-file-wins,
    % loading 1/N). Declared here so correctness stops depending on every future writer.
    flat_control_of/2,       % flat_control_of(+ControlC, +TargetC) — 28 facts, testsets/ only.
                             % NOTE: no engine consumer exists (checked 2026-08-18: zero
                             % non-corpus references). Declared for load-correctness, NOT
                             % wired — an authored field awaiting a reader, tracked as such.
                             % CONSEQUENCE, witnessed across all five legs: on the four twin
                             % legs (0 writers) this predicate was UNDEFINED before this
                             % declaration, so a call threw existence_error; it is now
                             % defined-but-empty and a call FAILS SILENTLY. Nothing observes
                             % that today (no consumer), but the direction is loud -> quiet,
                             % against the fail-closed default. A future consumer must treat
                             % an empty result as "no data authored on this leg", NOT as
                             % "no flat control exists" — the two are the same token here.
    has_sunset_clause/1.

:- dynamic
    attribute/3, has_mandatrophy_declaration/1,
    entity/2, interval/3, event/4,
    constraint_claim/2, human_readable/2, topic_domain/2, recommendation/2, affects_constraint/2,
    cs_reading_relation/3,
    veto_actor/1, veto_exposed/2, constraint_metric/3, omega_variable/3,
    measurement/5, has_sunset_clause/1, flat_control_of/2,
    story_provenance/8, story_seed/3,  % story_seed(C, SeededFrom, Draw) — forward-authored identity link (cohort ruling)            % story_provenance(C, PromptCommit, SchemaCommit, Date, SourceEssay, OneShotExample, Model, SamplingParams) — cohort metadata (Phase C growth rule + determinism-frontier ruling 2026-06-12)
    epsilon_provenance/5,
    measurement_basis/2,
    suppression_profile/2,
    intent_viable_alternative/3, intent_alternative_rejected/3,
    intent_beneficiary_class/2, intent_power_change/3,
    intent_suppression_level/4, intent_resistance_level/4,
    intent_norm_strength/3, constraint_claim/3,
    constraint_beneficiary/2, constraint_victim/2, input_vector/2,
    constraint_vindicates/2,  % OQ-64 split (2026-06-05): propositions a constraint vindicates are NOT beneficiaries; never feeds d/chi or beneficiary gates
    coupling_profile/2, coordination_type/2, boltzmann_floor_override/2,
    update_authority/2,   % OQ-153 institutional revision authority (optional; absence ≠ absent_diffuse; no default)
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
    founding_problem_corroboration_class/2,
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

   CLOSED 2026-07-25 (OQ-66) — both gate-2 holds disposed MOOT-BY-RESET. The
   2026-06-05 corpus reset removed both hosts:
     - technological_inevitability_interpretation (press_reformation_causality)
       is absent from all five live legs.
     - constitutional_supremacy_doctrine (statutory_debt_ceiling) — the names
       surviving in haiku/flash are NEW DRAWS, not the measured story
       (generation is stochastic; a regenerated story is a new draw, not a
       re-measurement). The 2026-06-03 reads cannot be re-witnessed.
   Substrate was already correct and is UNCHANGED: both values stay unlisted
   => default-agent => FSM keeps firing. A future revisit is a FRESH
   MEASUREMENT, not a resumption of these two records.

   GATE-2 FOR entropic_universe_hypothesis WAS RE-TAKEN 2026-07-25 (OQ-248).
   The June basis is VOID and the entry now rests on a NEW, DATED ruling — not a
   better-sourced version of the old one. Detail at the entry below; discriminator
   evidence in audits/2026-07-25_oq66_nlwb_filter_cutover/GATE2_REWITNESS.md.

   METHOD WARNING FOR ANY FUTURE GATE-2 READ — the June pass cited evidence that
   could not discriminate. maxwell (PASS) and press_reformation_causality__
   technological_inevitability (HELD) had IDENTICAL ε=0.08 and suppression=0.02,
   and maxwell-identical shadows. So metrics and shadow were DECORATIVE in that
   ruling: they appear in the pass citation but were satisfied by the case that
   failed. Gate 2's stated procedure ("host metrics AND narrative/omegas
   converge") overstated its own rigor — only the narrative/omega half ever did
   work. When taking a gate-2 read, state which surface DISCRIMINATES and check
   it against a known gate-2 FAIL; a surface shared with the failing case is
   corroboration at best.

   A later constraint_vindicates/2 split (proposition-vindication as its own
   authored field) is the continuation of this view, not a rewrite.
   ========================================================================== */

%% non_agent_beneficiary(?Value)
%  Curated registry of proposition-kind beneficiary values. Each entry carries
%  its two-gate provenance. Do NOT add entries without the gate-2 convergence
%  read (host ε/omega/narrative) — see comment block above.

% Gate 1: a hypothesis (the entropic-universe worldview), proposition-kind.
%   Ruled 2026-06-03; UNCHANGED and never in question.
%
% Gate 2: RE-RULED 2026-07-25 (OQ-248). This is a NEW ruling on a NEW basis, not
%   a re-citation of the June one. The entry was NOT continuously certified
%   between those dates — the June basis expired 2026-06-05 and nobody re-checked.
%
%   THE JUNE BASIS IS VOID, on two independent counts:
%     (a) "MaxEnt shadow 0.990 mountain / entropy 0.031" — the engine that
%         produced it certified maxwell natural_law, and no longer does. At HEAD
%         natural_law_signature does not fire for maxwell; it reads
%         coupling_invariant_rope / rope. Single-variable isolation (same
%         kernel_v1 corpus, era engine vs HEAD engine) confirms ENGINE regime,
%         not corpus. The digits also never reproduced: measured in-era is 0.95 /
%         entropy 0.1557 (0.95 is the saturation ceiling).
%
%         *** CORRECTION 2026-08-17 (OQ-251 audit; this paragraph previously
%         attributed the un-certification to OQ-70 / 72ec2cdd, 2026-06-05, "via a
%         claimed_natural/2 source that OQ-70 DELIBERATELY REMOVED"). THAT
%         ATTRIBUTION WAS WRONG on three independent counts, each witnessed in
%         audits/2026-08-17_oq251_natural_law_reachability/:
%           1. MECHANISM. claimed_natural/2 was never on the natural_law path, in
%              the era engine or at HEAD. Both producers of the natural_law atom
%              (constraint_signature/2 :114-117, classify_by_signature/3 :323-324)
%              gate on natural_law_signature/1 — the profile path. era-wide, the
%              ONLY executable consumer of claimed_natural/2 is false_natural_law/2.
%              The era dependency arrow in fact runs the other way: claimed_natural
%              clause 3 CALLS natural_law_signature.
%           2. CHRONOLOGY (the behavioral witness, three-point bisect over the same
%              byte-identical kernel_v1 corpus): natural_law FIRES at f600599b
%              (pre-both) AND at 8b5a34b8^ = a4297632 (post-72ec2cdd), and is
%              ABSENT at 8b5a34b8. The binding commit is `8b5a34b8` (2026-06-11,
%              OQ-43/OQ-44 fail-close: has_viable_alternatives/2 default
%              false -> unknown), six days after OQ-70. The earlier era-vs-HEAD
%              isolation SPANNED both commits and could not discriminate them —
%              that span is the whole defect, and this bisect is its repair.
%           3. maxwell's OWN DATA. maxwell authors an explicit story-level claim
%              (kernel_v1/maxwell_demon_impossibility.pl:114,
%              constraint_claim(..., mountain)), so claimed_natural/2 source 1
%              fires for it regardless; removing source 2 was INERT for maxwell
%              even on the claims side. At HEAD claimed_natural returns
%              explicit_mountain_claim.
%         The un-certification was a CHOSEN ruling with an accepted casualty
%         (thermal_dissipation_constraint; ISSUES OQ-43/OQ-44, GAP-08), later
%         re-ruled as documented builder-unreachability (OQ-113 fork (b),
%         2026-06-18). At HEAD exactly ONE conjunct blocks maxwell —
%         HasAlternatives == false — dead-by-range; every other conjunct passes on
%         maxwell's authored fields. Reverting that single line at HEAD restores
%         the certification end-to-end (30 kernel_v1 constraints certify). ***
%     (b) "omegas authored empty" — FACTUALLY WRONG. maxwell authors 11
%         omega_variable facts.
%
%   THE NEW BASIS — narrative, and DISCRIMINATING (verified against the gate-2
%   FAIL case, which is the check the June ruling skipped):
%     - maxwell's omegas are about the PHYSICS'S GROUNDING (is the second law
%       fundamental or an emergent statistical property; does the
%       information-theoretic route independently confirm it). None bears on
%       whether an agent benefits.
%     - the HELD case's omegas bear DIRECTLY on agency — "does deployment require
%       intentional beneficiary strategy?", "is this a law of physics or a
%       contingent outcome?" — which is exactly gate 2's question.
%     So the discriminator is WHAT THE OMEGAS ARE ABOUT, not whether they exist,
%     and not the metrics or the shadow (both identical across pass and fail).
%
%   ADMITTED AS HOST TESTIMONY, NOT INDEPENDENT EVIDENCE: the file's own
%   "no human agent benefits / hypotheses are not agents with exit options" is
%   authored by the story that gains from the release — the same authored surface
%   the HELD case failed on. It corroborates; it does not establish.
%
%   KILL CONDITION: if the detector-coverage question (OQ-251) finds that a path
%   to natural_law certification DOES exist post-OQ-70 and maxwell still fails
%   it, this narrative basis is called into question and gate 2 returns to the
%   operator's seat.
%
%   KILL-CONDITION DISPOSITION 2026-08-17 (OQ-251 resolved): EVALUATED, DID NOT
%   TRIP. No path exists — natural_law_signature/1 is unsatisfiable by
%   construction (has_viable_alternatives/2's two clauses bind arg 2 to the head
%   literals `true` and `unknown`; `false` is emitted by neither, so the
%   `HasAlternatives == false` leg is dead on EVERY corpus, not merely
%   unauthored). Since no path exists, the condition's antecedent is false and
%   this narrative basis stands as ruled. Witness:
%   audits/2026-08-17_oq251_natural_law_reachability/ (P5a pre-injection clean;
%   P6(a) clause read + two-sided control; P6(b) corpus range [unknown], 0 false).
%
%   FOR THE OPERATOR (surfaced, NOT decided here — an OQ-252 instance): this
%   ruling's own stated evidential basis was CORRECTED post-hoc by that audit
%   (see the *** CORRECTION *** block above). The ruling's SUBSTANCE — the
%   narrative/omega-aboutness discriminator — is untouched by the correction, and
%   the kill condition did not trip. Whether a ruling whose recorded basis needed
%   correcting warrants an explicit re-affirmation of
%   non_agent_beneficiary(entropic_universe_hypothesis) is the operator's call.
%   Executor's default recommendation: ruling stands.
%
%   Blast radius of this ruling: zero observable. maxwell classifies rope at HEAD
%   either way, and all five live legs carry zero registered beneficiary values.
%
%   Licenses: non_agent_beneficiary(entropic_universe_hypothesis) — this registry
%     entry, and nothing else. If THIS ruling is later amended or withdrawn, that
%     entry must be re-checked in the same change.
%     (First forward instance of the OQ-252 convention, ruled 2026-07-25:
%     Licenses: is added when a ruling is CREATED OR REVISITED from that date
%     onward. It is NOT backfilled — absence on an older ruling means "predates
%     the convention," never "licenses nothing.")
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

% RETIRED (OQ-109 B3, 2026-06-12 — empty-table census A3; adjudicated by
% contribution): check_indexical_relativity/1 and validate_indexical_completeness/1
% were v3.1 apparatus reading the authored classification table (Phase C retires
% it), had ZERO consumers (grep positive-controlled,
% audits/2026-06-11_oq109_phase_b/), and their exemption legs read dead inputs
% (hardcoded is_mandatrophy_resolved/1 facts for two non-corpus ids;
% attribute/3 lifecycle facts nothing produces). Successors on surviving
% inputs: claimed-mountain-but-extractive → FSM + T17 + the R5 Q6 synchronic
% crosscheck (stakeholder_seats:q6_crosscheck/3, consumed by report_generator;
% completed to the full status×signature matrix 2026-06-16); 3-pillar/coverage validation → the
% linter role-coverage rules (B3).

/* ==========================================================================
   MANDATROPHY RECONCILIATION (v3.1)
   ========================================================================== */

% RETIRED (OQ-35, 2026-06-21 — wiring-gap census row 1; adjudicated by
% contribution): is_mandatrophy_resolved/1 held 2 hardcoded facts (gale_shapley,
% planetary_boundaries) for non-corpus ids. ZERO goal-body/meta-call readers
% anywhere in non-archive code (grep witness, audits/2026-06-21_oq35_field_counterfactual/);
% their only would-be consumers (detect_omega/count_unresolved_omegas/
% detect_mandatrophy_omega) were already retired by OQ-109 B3 (below). The
% surviving mandatrophy analytical surface (report_generator:format_mandatrophy_gap/3
% -> compute_chi_v6/6) computes delta_chi independently of these facts, so removing
% them is output-neutral by construction. Revival cost rose post-OQ-109 (would need
% rebuilding the consumer, not just re-emitting the fact). See design_gaps.md GAP-18
% for the separate dangling-consumer note on the gap surface itself.

% RETIRED (OQ-109 B3, 2026-06-12 — empty-table census A4; same adjudication
% as above): detect_omega(_, mandatrophy), count_unresolved_omegas/1, and
% detect_mandatrophy_omega/1 — zero consumers, authored-table reads, dead
% exemption legs. The genealogy-based mandatrophy product lives in the R5
% zombie crosscheck (authored founding_problem_status=dead ∧
% disappearance_verdict=world_rearranges × computed piton), wired into the
% Section-7 report surface 2026-06-12.

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
