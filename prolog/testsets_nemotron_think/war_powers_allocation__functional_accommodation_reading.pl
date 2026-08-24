% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: Functional Accommodation Reading of War Powers Allocation
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   The functional accommodation reading holds that war powers allocation
 *   varies by operational context: the President may act unilaterally against
 *   imminent threats, but sustained military campaigns require congressional
 *   authorization. This reading dominates post-WWII practice and OLC
 *   doctrine. It coordinates by preventing paralysis in genuine emergencies
 *   while preserving legislative control over major wars. However, the
 *   'imminent threat' and 'prolonged campaign' boundaries are contested,
 *   creating an ambiguity zone where both branches claim authority. The
 *   executive benefits from this ambiguity — it can characterize
 *   discretionary actions as responses to imminence — while Congress and the
 *   public bear the costs of unauthorized or poorly authorized conflicts. The
 *   constraint requires active enforcement through judicial review, political
 *   conflict, and the War Powers Resolution, but enforcement is episodic and
 *   politically contingent.
 *
 * KEY AGENTS:
 *   - executive_branch: Primary agenda-setter in imminent threats; primary beneficiary in ambiguity zone (institutional/arbitrage)
 *   - congress: Agenda-setter for prolonged campaigns; payer in ambiguity zone (institutional/constrained)
 *   - public: Pays costs of military action in blood and treasure; constrained exit (organized/constrained)
 *   - courts: Observer/arbitrator; analytical exit (institutional/analytical)
 *   - foreign_populations: Excluded from the constitutional conversation; trapped (powerless/trapped)
 *   - military: Implementer; not a primary constitutional stakeholder
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.55).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.45).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "Functional Accommodation Reading of War Powers Allocation").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, '972a3fae-c980-4771-8cc7-b6ef5d7fde84').
narrative_ontology:cs_kernel_codification('972a3fae-c980-4771-8cc7-b6ef5d7fde84', fixed_text).
narrative_ontology:cs_authority_grounding('972a3fae-c980-4771-8cc7-b6ef5d7fde84', lineage).
narrative_ontology:cs_interpretation_layer_present('972a3fae-c980-4771-8cc7-b6ef5d7fde84').
narrative_ontology:cs_reading_relation('972a3fae-c980-4771-8cc7-b6ef5d7fde84', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('972a3fae-c980-4771-8cc7-b6ef5d7fde84', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_axiom('972a3fae-c980-4771-8cc7-b6ef5d7fde84', foundational, contextual_allocation_of_war_powers).
narrative_ontology:cs_axiom_status(contextual_allocation_of_war_powers, holdable).
narrative_ontology:cs_axiom_grounding('972a3fae-c980-4771-8cc7-b6ef5d7fde84', contextual_allocation_of_war_powers, conventional).
narrative_ontology:cs_axiom('972a3fae-c980-4771-8cc7-b6ef5d7fde84', secondary, temporal_distinction_governs_authority).
narrative_ontology:cs_axiom_status(temporal_distinction_governs_authority, holdable).
narrative_ontology:cs_axiom_grounding('972a3fae-c980-4771-8cc7-b6ef5d7fde84', temporal_distinction_governs_authority, instrumental).
narrative_ontology:cs_reference_frame('972a3fae-c980-4771-8cc7-b6ef5d7fde84', constitutional_original_understanding).
narrative_ontology:cs_drift_state('972a3fae-c980-4771-8cc7-b6ef5d7fde84', post_911_authorizations, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('972a3fae-c980-4771-8cc7-b6ef5d7fde84', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congress).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, public).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, constitutional_flexibility_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, operational_necessity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls threat assessment and initiation of military action; characterizes situations as 'imminent' to access unilateral authority; collects flexibility dividend in the ambiguity zone; can shift between functional accommodation and inherent executive framings as convenient; exit is arbitrage-grade — it controls the constraint's application.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, executive_branch, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, executive_branch, beneficiary).

% Holds formal war declaration and funding powers; sets agenda for prolonged campaigns through authorizations and appropriations; loses institutional authority when executive acts in ambiguity zone; political costs of challenging executive are high (accused of undermining troops); exit is constrained — can pass legislation but faces veto and political backlash.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congress, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, congress, payer).

% Bears human, fiscal, and moral costs of military action; benefits from security provided by effective defense; has no direct decision authority; exit limited to voting, protest, and cultural pressure — all slow and indirect; organized through veterans' groups, anti-war movements, but structurally excluded from the imminent/prolonged determination.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, public, payer,
    organized, biographical, constrained, national).

% Arbitrates war powers disputes when justiciable (rare — political question doctrine); issues opinions that shape the ambiguity zone boundaries (e.g., Youngstown, Hamdi); no enforcement power over executive non-compliance; analytical exit — observes and rules but does not participate in the constraint's operation.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, courts, observer,
    institutional, generational, analytical, national).

% Subject to military action decisions with zero constitutional voice; bears disproportionate costs (death, displacement, infrastructure destruction); no exit from the constraint's effects; structurally excluded from the U.S. constitutional conversation that authorizes force against them.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, foreign_populations, excluded,
    powerless, biographical, trapped, global).

% Produce the interpretive frameworks (congressional primacy, functional accommodation, inherent executive) that structure the constraint; compete for influence in OLC, courts, Congress; analytical exit — they choose which reading to advance but do not bear the constraint's costs directly.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__functional_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for allocating war powers between branches based on operational urgency and duration, avoiding paralysis in genuine emergencies while maintaining legislative control over sustained conflicts.
% TRANSFER_FUNCTION: Moves decision authority from Congress to the Executive in imminent threat scenarios; moves accountability costs to the public and affected populations; moves institutional authority from Congress to Executive in the ambiguity zone where 'imminent' and 'prolonged' are contested.
% ABSENT_VOICES: Foreign populations affected by military decisions; future generations bearing long-term consequences of unauthorized wars; rank-and-file service members who execute orders without constitutional voice; state governments bearing domestic costs of federal war decisions.
% DISAPPEARANCE_RATIONALE: If the functional accommodation constraint vanished overnight, the constitutional vacuum would be filled by either congressional primacy (requiring authorization for all uses of force) or inherent executive authority (unilateral presidential power), fundamentally restructuring how military decisions are made and which branch controls them.
% FOUNDING_PROBLEM: The Constitution's division of war powers (Congress declares war, President commands) created a gap for situations requiring immediate action but not rising to declared war — the founding problem was how to reconcile legislative control with executive speed in the nuclear/jet age.
% FOUNDING_PROBLEM_CORROBORATION: Historical practice from Jefferson (Barbary Wars) through Truman (Korea) to Obama (Libya) attests the problem's persistence. Congressional primacy scholars (e.g., Louis Fisher, Bruce Ackerman) and War Powers Resolution legislative history attest the problem is substantially solved by modern communications and rapid congressional convening — the urgency justification has eroded. No consensus outside the executive branch's own OLC opinions.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects the executive's consistent advantage in the ambiguity zone — it initiates action and defines the threat. Suppression (0.45) is the constraint's displacement of categorical rules (declare war / no unilateral action) with a contextual test that the executive controls. Theater ratio (0.4) captures performative consultations and reporting that rarely constrain outcomes. Accessibility collapse (0.5) and resistance (0.55) are moderate: categorical alternatives exist (congressional primacy, inherent executive) but are politically difficult to sustain; Congress and courts resist but inconsistently. The measurement series shows extractiveness and theater rising through the Cold War and post-9/11, with slight recent stabilization.
 *
 * PERSPECTIVAL GAP:
 *   From the executive seat, the constraint is a rope — genuine coordination solving the speed-democracy dilemma. From Congress's seat, it is a snare — the ambiguity zone extracts legislative authority. From the public seat, it is a tangled rope — some coordination value (preventing paralysis) but substantial extraction (wars without accountability). The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive is the structural beneficiary (d ~0.2): it controls threat characterization, initiates action, and collects the flexibility dividend. Congress is a payer (d ~0.75): it loses institutional authority in the ambiguity zone and bears political costs of challenging the executive. The public is a payer (d ~0.65): bears human and fiscal costs with constrained exit (voting, protest). Courts are near-symmetric observers (d ~0.5): they arbitrate but lack enforcement power. Foreign populations are trapped (d ~0.9): fully subject to decisions with zero voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constitutional gap for immediate action) remains live but contested — modern communications and rapid deployment reduce the genuine urgency justification. The constraint persists because it benefits the executive and Congress prefers ambiguity to accountability. Mandatrophy is unresolved: the arrangement has outlived its strict necessity but no coalition can replace it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the functional accommodation reading a distinct constraint from its sibling readings, or a contextual application of a single war powers principle?',
    'Compare the ε values and beneficiary/victim structures across readings: if ε differs materially between functional accommodation and inherent executive readings, they are distinct constraints per ε-invariance.',
    'If distinct, each reading gets its own classification; if not, the kernel has a single ε and the readings are interpretive variants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether functional accommodation instantiates a separate constraint from congressional primacy and inherent executive readings.').

omega_variable(
    contextual_extractiveness_variance,
    'Does the constraint''s extractiveness genuinely vary by operational context (imminent vs prolonged), or is the contextual distinction a cover for consistent executive advantage?',
    'Measure executive unilateral action frequency and congressional authorization rates across threat types; if ''imminent'' designation correlates with executive preference rather than objective threat criteria, the variance is extractive cover.',
    'If cover, the constraint is a snare with contextual theater; if genuine, it is a tangled rope with real coordination zones.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contextual_extractiveness_variance, empirical, 'Whether the imminent/prolonged distinction tracks real operational differences or executive convenience.').

omega_variable(
    sibling_boundary_ambiguity,
    'Where does the functional accommodation reading''s ''prolonged campaign'' threshold end and the inherent executive reading''s ''national interest'' authority begin?',
    'Analyze OLC opinions and congressional authorizations post-9/11: if administrations cite functional accommodation to justify actions that inherent executive proponents would claim as inherent, the boundary is porous.',
    'Porous boundary means the readings influence each other structurally; sharp boundary means they occupy distinct constraint spaces.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_boundary_ambiguity, conceptual, 'Structural boundary between functional accommodation and inherent executive readings in the ambiguity zone.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of categorical war powers rules structural (institutional incentives, precedent) or internalized (constitutional culture accepting ambiguity as necessary)?',
    'Track congressional war powers assertions after executive overreach: if Congress reasserts categorical rules when politically viable, suppression is structural; if Congress has internalized flexibility as constitutional, suppression is internalized.',
    'If internalized, effective suppression exceeds structural measures — the constraint travels with the agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of categorical war powers rules.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war_powers_functional_accommodation_tr_t0, war_powers_allocation__functional_accommodation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_tr_t0, observed).
narrative_ontology:measurement(war_powers_functional_accommodation_tr_t70, war_powers_allocation__functional_accommodation_reading, theater_ratio, 70, 0.2).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_tr_t70, observed).
narrative_ontology:measurement(war_powers_functional_accommodation_tr_t150, war_powers_allocation__functional_accommodation_reading, theater_ratio, 150, 0.3).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_tr_t150, observed).
narrative_ontology:measurement(war_powers_functional_accommodation_tr_t200, war_powers_allocation__functional_accommodation_reading, theater_ratio, 200, 0.38).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_tr_t200, observed).
narrative_ontology:measurement(war_powers_functional_accommodation_tr_t220, war_powers_allocation__functional_accommodation_reading, theater_ratio, 220, 0.42).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_tr_t220, observed).
narrative_ontology:measurement(war_powers_functional_accommodation_tr_t235, war_powers_allocation__functional_accommodation_reading, theater_ratio, 235, 0.4).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_tr_t235, observed).

% Extraction over time
narrative_ontology:measurement(war_powers_functional_accommodation_be_t0, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_be_t0, observed).
narrative_ontology:measurement(war_powers_functional_accommodation_be_t70, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 70, 0.35).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_be_t70, observed).
narrative_ontology:measurement(war_powers_functional_accommodation_be_t150, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 150, 0.45).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_be_t150, observed).
narrative_ontology:measurement(war_powers_functional_accommodation_be_t200, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 200, 0.52).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_be_t200, observed).
narrative_ontology:measurement(war_powers_functional_accommodation_be_t220, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 220, 0.58).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_be_t220, observed).
narrative_ontology:measurement(war_powers_functional_accommodation_be_t235, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 235, 0.55).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_be_t235, observed).

% Suppression requirement over time
narrative_ontology:measurement(war_powers_functional_accommodation_su_t0, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_su_t0, observed).
narrative_ontology:measurement(war_powers_functional_accommodation_su_t70, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 70, 0.3).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_su_t70, observed).
narrative_ontology:measurement(war_powers_functional_accommodation_su_t150, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 150, 0.35).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_su_t150, observed).
narrative_ontology:measurement(war_powers_functional_accommodation_su_t200, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 200, 0.42).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_su_t200, observed).
narrative_ontology:measurement(war_powers_functional_accommodation_su_t220, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 220, 0.48).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_su_t220, observed).
narrative_ontology:measurement(war_powers_functional_accommodation_su_t235, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 235, 0.45).
narrative_ontology:measurement_basis(war_powers_functional_accommodation_su_t235, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__functional_accommodation_reading, 0.1).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_resolution_enforcement).

% DUAL FORMULATION NOTE:
% Part of the war_powers_allocation constraint family (3 readings). Functional accommodation differs from congressional primacy by accepting unilateral action for imminence, and from inherent executive by requiring authorization for prolonged campaigns. ε is higher than congressional primacy (which has near-zero extraction) and lower than inherent executive (which has higher extraction but less coordination). The readings form a gradient: congressional primacy (mountain/rope) → functional accommodation (tangled rope) → inherent executive (snare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__functional_accommodation_reading, institutional, 0.2).
constraint_indexing:directionality_override(war_powers_allocation__functional_accommodation_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
