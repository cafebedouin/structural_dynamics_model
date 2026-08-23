% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy/welfare_state
 *
 * SUMMARY:
 *   This constraint story instantiates the dependency_trap_reading of the
 *   income_support_commitment kernel. It frames unconditional income support
 *   as a structure that coordinates poverty reduction while extracting from
 *   workers and atrophying recipient skills. The coordination function
 *   (universal floor) is real but the extraction function (high effective
 *   marginal tax rates, skill decay) is substantial and growing. The
 *   constraint persists through active enforcement: eligibility rules,
 *   sanction regimes, and bureaucratic complexity that maintain the
 *   withdrawal-rate trap. The claimed type is tangled_rope — genuine
 *   coordination fused with asymmetric extraction. The engine will compute
 *   per-seat types from the structural data; the dependency_trap_reading's
 *   axioms and drift_state are recorded in cs_structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.55).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.45).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/social_policy/welfare_state").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, '149210b3-8526-4216-9fa5-b2a161d53005').
narrative_ontology:cs_kernel_codification('149210b3-8526-4216-9fa5-b2a161d53005', formalized).
narrative_ontology:cs_authority_grounding('149210b3-8526-4216-9fa5-b2a161d53005', extraction).
narrative_ontology:cs_interpretation_layer_present('149210b3-8526-4216-9fa5-b2a161d53005').
narrative_ontology:cs_reading_relation('149210b3-8526-4216-9fa5-b2a161d53005', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('149210b3-8526-4216-9fa5-b2a161d53005', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('149210b3-8526-4216-9fa5-b2a161d53005', foundational, unconditional_transfer_erodes_work_ethic).
narrative_ontology:cs_axiom_status(unconditional_transfer_erodes_work_ethic, holdable).
narrative_ontology:cs_axiom_grounding('149210b3-8526-4216-9fa5-b2a161d53005', unconditional_transfer_erodes_work_ethic, empirically_contingent).
narrative_ontology:cs_axiom('149210b3-8526-4216-9fa5-b2a161d53005', foundational, skill_atrophy_creates_permanent_dependency).
narrative_ontology:cs_axiom_status(skill_atrophy_creates_permanent_dependency, holdable).
narrative_ontology:cs_axiom_grounding('149210b3-8526-4216-9fa5-b2a161d53005', skill_atrophy_creates_permanent_dependency, empirically_contingent).
narrative_ontology:cs_reference_frame('149210b3-8526-4216-9fa5-b2a161d53005', post_war_welfare_settlement).
narrative_ontology:cs_drift_state('149210b3-8526-4216-9fa5-b2a161d53005', contemporary_activation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('149210b3-8526-4216-9fa5-b2a161d53005', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, non_working_recipients).
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, idle_population).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, skill_atrophy_poor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, idle_population).
narrative_ontology:constraint_vindicates(income_support_commitment__dependency_trap_reading, work_ethic_doctrine).
narrative_ontology:constraint_vindicates(income_support_commitment__dependency_trap_reading, fiscal_responsibility_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income support without labor market participation. The transfer covers basic needs but offers no pathway to skill acquisition or wage growth. Exit requires taking low-wage work that loses the benefit without replacing its security, creating a high effective marginal tax rate on labor entry.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, non_working_recipients, beneficiary,
    powerless, biographical, constrained, national).

% Long-term recipients whose identity and social networks have fused with the benefit status. They receive the transfer but pay through skill atrophy, social isolation, and eroded labor market attachment. Exit is psychologically and structurally blocked — leaving the benefit feels like abandoning their community and admitting failure.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, idle_population, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__dependency_trap_reading, idle_population, payer).

% Fund the transfer through income and consumption taxes. They experience the constraint as extraction: their labor supports non-participation while they bear full employment risk. Their exit options are limited — tax avoidance is criminalized, emigration is costly, and political voice is diluted by the beneficiary coalition.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, working_taxpayers, payer,
    organized, biographical, constrained, national).

% Poor individuals who remain in or cycle through low-wage work while watching peers exit to unconditional support. They bear the dynamic cost: their skills degrade relative to a labor market that increasingly demands credentials and digital fluency, while the support system offers no skill-building counterpart. They are trapped by the very structure that claims to help them.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, skill_atrophy_poor, payer,
    powerless, generational, trapped, national).

% Design and enforce eligibility rules, activation requirements, and sanction regimes. They justify the system as a poverty floor but maintain work disincentives through high withdrawal rates and bureaucratic complexity. They benefit from institutional expansion and can move between administrative, academic, and consultancy roles regardless of outcomes.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, policy_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Individuals barred from both adequate wages and adequate support — undocumented workers, those with sanction histories, care-givers whose labor is uncounted. They would object to both the extraction from workers and the skill-atrophy trap for recipients, but they hold no seat in the policy conversation.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, labor_market_excluded, excluded,
    powerless, biographical, trapped, national).

% Study labor supply elasticities, marginal effective tax rates, and long-term dependency dynamics across regimes. They see the full structure: the coordination function (poverty reduction), the extraction function (worker-to-non-worker transfer), and the atrophy mechanism. Their exit is analytical — they can change frameworks but not the constraint itself.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal income floor that prevents absolute destitution, eliminates stigma of means-testing, and insures against labor market volatility in an era of precarious work.
% TRANSFER_FUNCTION: Moves resources from current workers (via income/consumption taxes) to non-working recipients (via unconditional transfers). The transfer is structured by withdrawal rates that create effective marginal tax rates of 60-80% on early labor market earnings.
% ABSENT_VOICES: The labor_market_excluded — undocumented workers, sanctioned claimants, unpaid care-givers — would object to both the extraction from vulnerable workers and the atrophy trap for recipients. They are structurally excluded from the policy debate by citizenship rules, administrative status, and the invisibility of their labor.
% DISAPPEARANCE_RATIONALE: If the unconditional support vanished overnight, labor force participation would rise among current recipients (some voluntarily, some by necessity), poverty depth would spike for those unable to find work, fiscal pressure on workers would ease, and the political coalition sustaining the transfer would dissolve — the welfare state architecture would reorganize around targeted or contributory schemes.
% FOUNDING_PROBLEM: Post-war mass unemployment and poverty among demographic groups excluded from contributory insurance (single parents, disabled, long-term unemployed). The commitment was built to solve the gap between insurance-based protection and universal need.
% FOUNDING_PROBLEM_CORROBORATION: Original architects (Beveridge Commission, 1942) attested the problem was universal coverage gaps in contributory systems. Contemporary labor economists (OECD 2023, Autor et al.) attest the founding problem has transformed: the gap is now precarious work and skill mismatch, not insurance exclusion — a shift the unconditional transfer does not address. Anti-poverty NGOs corroborate the transformed problem; fiscal conservatives corroborate the original problem is largely solved.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__dependency_trap_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.55) reflects the transfer scale and the effective marginal tax rates on labor entry. Suppression (0.45) captures the active enforcement needed to maintain withdrawal rates and sanction non-compliance — not maximal coercion but persistent structural pressure. Theater (0.35) measures the growing share of 'activation' spending that performs compliance theater rather than building skills. Accessibility collapse (0.55) and resistance (0.5) are moderate: alternatives exist (targeted schemes, negative income tax, job guarantees) but are politically constrained; resistance comes from both taxpayer revolts and recipient advocacy. The measurement series share one time grid (0, 10, 20, 30, 40, 50) so drift detection has aligned snapshots.
 *
 * PERSPECTIVAL GAP:
 *   From the policy_administrator seat, the constraint is a rope (coordination with manageable overhead). From working_taxpayers and skill_atrophy_poor seats, it computes as snare/tangled_rope (extraction with suppressed alternatives). From idle_population seat, it may compute as piton (identity-locked dependence with performative activation). The engine computes this divergence; the reading does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Non_working_recipients and idle_population are beneficiaries (d near 0.0) — they receive net transfers. Working_taxpayers are payers (d near 1.0) — they fund the transfer with constrained exit. Skill_atrophy_poor are payers despite low power — they bear dynamic costs with trapped exit. Policy_administrators are agenda_setters with arbitrage exit — they set rules but bear no personal cost. Labor_market_excluded are excluded — they would pay if included but are kept out. Policy_analysts are observers with analytical exit. The engine derives d from these declarations plus power/exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (insurance gaps for excluded groups) is contested: partly solved by universal healthcare and pensions, partly transformed into precarious work. The constraint persists because the beneficiary coalition (recipients + administrators) blocks reform, while the victim coalition (workers + skill_atrophy_poor) is fragmented. This is mandatrophy: the arrangement's mandate has outlived its original function but persists through coalition inertia. The theater_ratio rise tracks the performative maintenance of 'activation' that neither reduces dependency nor builds skills.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    work_disincentive_magnitude,
    'What is the true labor supply elasticity with respect to unconditional transfers at current withdrawal rates?',
    'Natural experiments from pilot programs (Finland, Canada, Kenya) and regression discontinuity designs at benefit cliffs. Longitudinal tracking of labor market entry/exit around policy changes.',
    'If elasticity is near zero, the extraction is largely incidental and the constraint leans rope. If elasticity is high (>0.3), the work disincentive is the primary extraction mechanism and the constraint leans snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(work_disincentive_magnitude, empirical, 'Whether the measured extraction reflects intentional work disincentive or incidental transfer scale.').

omega_variable(
    skill_atrophy_causality,
    'Does skill atrophy cause persistent dependency, or does persistent dependency cause skill atrophy (reverse causality)?',
    'Sibling studies comparing recipients who exit vs. remain, controlling for pre-existing skill trajectories. Instrumental variable approaches using regional policy variation.',
    'If atrophy causes dependency, the constraint actively creates its own victim class (skill_atrophy_poor) — stronger tangled_rope. If dependency causes atrophy, the extraction is more incidental — weaker coordination-extraction fusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(skill_atrophy_causality, empirical, 'Direction of causality between skill decay and long-term receipt.').

omega_variable(
    committer_frame_ambiguity,
    'Is the income_support_commitment kernel a single stabilised commitment with three readings, or three distinct constraints sharing a label?',
    'Trace the legislative genealogy: does each reading cite the same statutory text as its kernel, or do they invoke different enactments? Map the authority_grounding for each reading.',
    'If single kernel, the readings are in structural tension (forecloses/coexists/influences). If distinct constraints, the kernel_id is a category error and each should stand alone with network.affects_constraints links only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Whether the kernel frame is analytically valid or a category error.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (sanctions, withdrawal rates, eligibility barriers) or internalized (recipients believing they are unemployable, identity fusion with benefit status)?',
    'Post-exit suppression trajectory: track recipients who leave the system — if suppression feelings persist after benefit cessation, reclassify as partially internalized. Psychological surveys measuring self-efficacy and perceived employability.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint carries its suppression mechanism inside the agent. This strengthens the tangled_rope classification by showing extraction persists without active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for recipients.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__dependency_trap_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__dependency_trap_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__dependency_trap_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__dependency_trap_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(inco_tr_t50, income_support_commitment__dependency_trap_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__dependency_trap_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__dependency_trap_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__dependency_trap_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__dependency_trap_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(inco_be_t50, income_support_commitment__dependency_trap_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__dependency_trap_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__dependency_trap_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__dependency_trap_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__dependency_trap_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(inco_su_t50, income_support_commitment__dependency_trap_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__dependency_trap_reading, 0.18).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, labor_market_participation).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, fiscal_sustainability).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, skill_formation_system).

% DUAL FORMULATION NOTE:
% This constraint (dependency_trap_reading) and freedom_floor_reading are dual formulations of the same transfer mechanism: one reads the transfer as extraction+atrophy, the other as coordination+autonomy. targeting_efficiency_reading is a structural critique that would replace the universal transfer with a targeted one — it influences both by changing the legitimacy conditions for universality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__dependency_trap_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
