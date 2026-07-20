% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__dependency_trap_reading, []).

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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: income_support_commitment__dependency_trap_reading
 *   human_readable: Unconditional Income Support: Dependency Trap Reading
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the dependency-trap reading of the
 *   income_support_commitment kernel. It treats unconditional transfers not
 *   as benign social protection but as a structurally extractive arrangement
 *   that moves resources from productive taxpayers to idle non-workers while
 *   degrading the human capital of poor recipients. The arrangement requires
 *   active enforcement (taxation) to hold, and the victim and beneficiary
 *   classes are structurally distinct in this reading: those who enjoy
 *   subsidized non-work versus those who fund it and those whose skills
 *   decay. The state apparatus administers the transfer and grows its mandate
 *   through the scale of dependency. Seat divergence is expected: the
 *   agenda-setter and beneficiaries experience coordination (income
 *   security), while taxpayers and the poor experiencing atrophy experience
 *   extraction.
 *
 * KEY AGENTS:
 *   - idle_non_workers (beneficiary, constrained exit)
 *   - working_taxpayers (payer, constrained exit)
 *   - poor_skills_atrophy (payer, trapped)
 *   - welfare_state_admin (agenda_setter, analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.58).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.42).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support: Dependency Trap Reading").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, 'ec2886e4-c871-470b-9e39-2d08fa708802').
narrative_ontology:cs_kernel_codification('ec2886e4-c871-470b-9e39-2d08fa708802', formalized).
narrative_ontology:cs_authority_grounding('ec2886e4-c871-470b-9e39-2d08fa708802', lineage).
narrative_ontology:cs_interpretation_layer_present('ec2886e4-c871-470b-9e39-2d08fa708802').
narrative_ontology:cs_reading_relation('ec2886e4-c871-470b-9e39-2d08fa708802', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec2886e4-c871-470b-9e39-2d08fa708802', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('ec2886e4-c871-470b-9e39-2d08fa708802', foundational, unconditionality_generates_dependency).
narrative_ontology:cs_axiom_status(unconditionality_generates_dependency, holdable).
narrative_ontology:cs_axiom_grounding('ec2886e4-c871-470b-9e39-2d08fa708802', unconditionality_generates_dependency, empirically_contingent).
narrative_ontology:cs_axiom('ec2886e4-c871-470b-9e39-2d08fa708802', foundational, taxpayer_burden_demands_reciprocal_effort).
narrative_ontology:cs_axiom_status(taxpayer_burden_demands_reciprocal_effort, holdable).
narrative_ontology:cs_axiom_grounding('ec2886e4-c871-470b-9e39-2d08fa708802', taxpayer_burden_demands_reciprocal_effort, deontological).
narrative_ontology:cs_reference_frame('ec2886e4-c871-470b-9e39-2d08fa708802', reciprocal_social_protection).
narrative_ontology:cs_drift_state('ec2886e4-c871-470b-9e39-2d08fa708802', contemporary_universal_basic_income_debate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ec2886e4-c871-470b-9e39-2d08fa708802', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, idle_non_workers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, poor_skills_atrophy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income transfers without labor-market participation. Their material consumption is funded by taxation on working populations. Exit from this arrangement would require entering a labor market where wages are often unattractive relative to the transfer and where prolonged absence has eroded relevant skills.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, idle_non_workers, beneficiary,
    moderate, biographical, constrained, national).

% Fund the unconditional transfer system through taxation. They experience the constraint as a persistent fiscal transfer from productive activity to non-participation. Individual exit from the tax jurisdiction is theoretically possible but involves severe personal and professional disruption, and collective political exit has high coordination costs.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, working_taxpayers, payer,
    moderate, biographical, constrained, national).

% Are poor individuals who remain in the transfer system for extended periods and suffer depreciation of human capital due to labor-market detachment. Their long-term earnings capacity declines, making exit from dependency increasingly difficult over time even if the nominal transfer is unconditionally available.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, poor_skills_atrophy, payer,
    powerless, biographical, trapped, national).

% Administers the tax collection and unconditional distribution machinery. Enforces compliance to maintain revenue. Grows its operational scope and political mandate with the size of the recipient population, and justifies the arrangement as social protection regardless of employment status.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, welfare_state_admin, agenda_setter,
    institutional, generational, analytical, national).

% Are not yet voting or earning but will inherit the accumulated fiscal obligations and dependency structure created by the current arrangement. They have no seat at the policy table and no ability to opt out of the inter-temporal transfer.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, future_taxpayers, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__dependency_trap_reading, idle_non_workers).
narrative_ontology:fixing_cost_class(income_support_commitment__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Eliminates absolute destitution and the administrative complexity of means-testing by guaranteeing a material floor regardless of employment status, preventing the stigma and error rates associated with conditionality.
% TRANSFER_FUNCTION: Moves purchasing power from working taxpayers to non-working recipients; imposes an implicit human-capital cost on poor individuals whose skills atrophy during prolonged labor-market detachment.
% ABSENT_VOICES: Future taxpayers who will bear the accumulated fiscal burden; poor individuals who would prefer employment but face effective marginal tax rates and skill depreciation that make non-participation rational; emigrant professionals who exit the jurisdiction to avoid the tax burden.
% DISAPPEARANCE_RATIONALE: Labor supply and household budgets would reorganize immediately; the fiscal contract between the state and citizens would collapse and force renegotiation; poor individuals trapped in dependency would face acute material distress; the political coalition sustaining the transfers would fracture.
% FOUNDING_PROBLEM: Industrial displacement and cyclical unemployment creating large populations without subsistence income, combined with political demand for social protection that does not condition survival on employability.
% FOUNDING_PROBLEM_CORROBORATION: Labor historians and social-policy scholars outside the beneficiary class attest to the historical problem of industrial poverty. Public-choice economists and classical-liberal critics corroborate that the problem has structurally mutated into one of dependency and taxpayer burden, while social-democratic policy analysts contest this and maintain that material insecurity remains live.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__dependency_trap_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the transfer is real and significant but not total; suppression (0.42) reflects the coercive tax enforcement required to sustain universal transfers; theater_ratio (0.32) captures the performative bureaucracy of labor-market programs that accompany unconditional support without altering the dependency dynamic. Accessibility_collapse (0.48) acknowledges that private and mutual-aid alternatives are partially crowded out by state provision but not eliminated. Resistance (0.52) reflects persistent political contestation from taxpayer coalitions and pro-work policy advocates. The measurement series share a single grid to prevent misaligned temporal inference.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (welfare state administration) experiences the constraint as a legitimate social-protection coordination mechanism; the beneficiary seat (idle non-workers) experiences it as a sustainable income floor. The payer seats (working taxpayers and the poor whose skills atrophy) experience the same structure as fiscal extraction and human-capital destruction, respectively. The engine will compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Idle non-workers are declared beneficiaries (low d): the constraint subsidizes their non-participation. Working taxpayers and poor individuals facing skill atrophy are declared victims (high d): the constraint extracts from them, the former fiscally and the latter through human-capital depreciation. The welfare state admin sits near symmetric but with analytical exit: it does not personally bear the costs or capture the gains in the same direct sense, but its institutional survival is tied to the arrangement. No directionality overrides are needed: the structural derivation from beneficiary/victim declarations and exit options captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The dependency-trap reading prevents mislabeling by insisting on a beneficiary/victim split: if the constraint were pure coordination (rope), there would be no identifiable victim class paying net costs. If it were pure extraction (snare), there would be no genuine coordination function (the destitution-prevention floor). The tangled_rope classification captures that the coordination (poverty prevention) and extraction (dependency creation, taxpayer burden) are inseparable in the same structure, and the constraint requires active enforcement (taxation) to persist. If the founding problem of industrial destitution were dead and the structure persisted purely by inertia with no beneficiary, it would be a piton; but the presence of a concentrated beneficiary class (idle non-workers) and active enforcement rules this out.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dependency_effect_magnitude,
    'Does unconditional income support actually produce significant work disincentives and skill atrophy at scale, or are observed effects small and transient?',
    'Longitudinal randomized controlled trials of unconditional cash transfers and natural experiments from Alaska Permanent Fund dividends or lottery winnings.',
    'If the effect is small, the extractiveness of this constraint is lower than claimed and it may function more like a rope; if large, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_effect_magnitude, empirical, 'Empirical magnitude of dependency and skill-atrophy effects under unconditional transfers').

omega_variable(
    beneficiary_victim_overlap,
    'Are the ''idle non-workers'' who benefit from transfers structurally distinct from the ''poor individuals whose skills atrophy,'' or are they the same population observed at different time horizons?',
    'Longitudinal panel data tracking individual labor-market trajectories, skill assessments, and transfer receipt over multi-year horizons.',
    'If the beneficiary and victim sets are largely overlapping, the constraint may be better understood as a snare or scaffold rather than a tangled rope with distinct classes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_victim_overlap, empirical, 'Overlap between beneficiary and victim populations over time').

omega_variable(
    kernel_reading_contest,
    'Is the dependency trap an inherent feature of unconditional transfers or a contingent result of transfer levels and labor market structure?',
    'Comparative analysis across jurisdictions with varying benefit levels, tax structures, and labor-market institutions.',
    'If contingent, the constraint''s epsilon varies by context and may not generalize; if inherent, the reading''s structural claims are robust across implementations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contingency of the dependency trap mechanism across institutional contexts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__dependency_trap_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__dependency_trap_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__dependency_trap_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__dependency_trap_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__dependency_trap_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__dependency_trap_reading, base_extractiveness, 30, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__dependency_trap_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__dependency_trap_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__dependency_trap_reading, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% The income_support_commitment kernel decomposes into three structurally distinct constraints: the dependency_trap_reading (tangled_rope, extractive transfer with victim/beneficiary split), the freedom_floor_reading (rope or scaffold, coordination of autonomy), and the targeting_efficiency_reading (rope or tangled_rope, coordination via means-testing). Each reading carries a different epsilon and different stakeholder directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
