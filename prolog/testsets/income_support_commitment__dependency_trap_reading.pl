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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_commitment__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Work-Disincentive and Dependency Mechanism
 *   domain: political_economy/welfare_state
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested
 *   'income_support_commitment' kernel. Under the dependency-trap reading,
 *   unconditional income support solves the immediate problem of survival in
 *   unemployment but simultaneously creates persistent non-participation
 *   through work disincentive, skill atrophy, and identity fusion with
 *   non-work status. The constraint is CLAIMED as a tangled rope: genuine
 *   coordination function (income floor preventing destitution), but with
 *   asymmetric extraction (productive workers fund non-productive recipients,
 *   and poor individuals fund their own future wage suppression through
 *   reduced labor-force competition). The three readings of this kernel —
 *   dependency-trap, freedom-floor, and targeting-efficiency — are
 *   structurally distinct constraints that share the same institutional
 *   kernel (the income support program) but instantiate different ε values,
 *   beneficiary/victim structures, and policy implications. This story
 *   authors ONLY the dependency-trap reading; the other two are separate
 *   constraint stories linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - income_support_recipients_exiting_labor_market: Structural beneficiaries, receiving unconditional transfers; power limited but identity-locked exit prevents high mobility.
 *   - working_taxpayers: Primary victims, funding the program through mandatory taxation; constrained exit (cannot easily leave the tax system without relocation).
 *   - poor_individuals_with_atrophying_skills: Secondary victims, harmed by skill loss during extended non-participation; trapped between survival via support and re-entry into lower-wage work.
 *   - program_administrators: Agenda-setters, designing and defending the unconditional structure; institutional power, constrained exit.
 *   - labor_market_employers: Dual-positioned (payers of taxes, beneficiaries of suppressed wage floors); organized power, mobile exit if support is removed.
 *   - political opposition_targeting_efficiency and autonomy_advocates_freedom_floor: Excluded from the dependency-trap reading frame; their presence would restructure the story entirely.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.68).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.52).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support as Work-Disincentive and Dependency Mechanism").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/welfare_state").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, '511303ee-67e6-4648-b31a-7ceda7a84375').
narrative_ontology:cs_kernel_codification('511303ee-67e6-4648-b31a-7ceda7a84375', formalized).
narrative_ontology:cs_authority_grounding('511303ee-67e6-4648-b31a-7ceda7a84375', lineage).
narrative_ontology:cs_interpretation_layer_present('511303ee-67e6-4648-b31a-7ceda7a84375').
narrative_ontology:cs_reading_relation('511303ee-67e6-4648-b31a-7ceda7a84375', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('511303ee-67e6-4648-b31a-7ceda7a84375', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('511303ee-67e6-4648-b31a-7ceda7a84375', foundational, unconditional_structure_creates_work_disincentive).
narrative_ontology:cs_axiom_status(unconditional_structure_creates_work_disincentive, holdable).
narrative_ontology:cs_axiom_grounding('511303ee-67e6-4648-b31a-7ceda7a84375', unconditional_structure_creates_work_disincentive, empirically_contingent).
narrative_ontology:cs_axiom('511303ee-67e6-4648-b31a-7ceda7a84375', foundational, extended_non_participation_atrophies_skill_and_identity).
narrative_ontology:cs_axiom_status(extended_non_participation_atrophies_skill_and_identity, holdable).
narrative_ontology:cs_axiom_grounding('511303ee-67e6-4648-b31a-7ceda7a84375', extended_non_participation_atrophies_skill_and_identity, empirically_contingent).
narrative_ontology:cs_reference_frame('511303ee-67e6-4648-b31a-7ceda7a84375', temporary_income_support_for_cyclical_unemployment).
narrative_ontology:cs_drift_state('511303ee-67e6-4648-b31a-7ceda7a84375', contemporary_long_term_welfare_dependence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('511303ee-67e6-4648-b31a-7ceda7a84375', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, income_support_recipients_exiting_labor_market).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, poor_individuals_with_atrophying_skills).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).

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
 *   Extractiveness (0.68) is measured as the net transfer from productive to non-productive populations, weighted by the skill atrophy and labor-force exit dynamics specific to this reading. The trajectory shows rising extractiveness over the interval (0.48 → 0.68), capturing the empirical pattern where program maturity increases the proportion of long-term recipients with accumulated skill loss and identity lock-in, deepening the extraction from remaining workers. Suppression (0.52) reflects the moderately strong work disincentive built into the structure: the income support itself is not coercive in the traditional sense (no one is forced to stay on it), but the phase-out structure creates implicit penalties for work, and once recipients are identity-locked, the suppression persists internally. Theater ratio (0.41) indicates that while the program operates as intended (distributing income), increasingly large shares of administrative discourse center on defending the program against accusations of promoting idleness — performative justification grows as the constraint's actual coordination function (solving unemployment) becomes less salient for long-term recipients. Accessibility collapse (0.48) is moderate: the constraint does not fully eliminate alternatives (recipients can still attempt re-entry, employers can still hire), but re-entry is increasingly difficult due to skill loss and hiring discrimination based on long gaps. Resistance (0.71) is high because workers and some poor individuals actively resist the structure through political mobilization, taxpayer resentment, and re-entry attempts, even though the resistance often fails to change the program.
 *
 * PERSPECTIVAL GAP:
 *   The program_administrators seat (agenda-setter) should compute a different classification than the working_taxpayers seat (payer). From the administrator's position, the arrangement is genuine coordination it built to solve a real problem (unemployment) and operates as intended (distributing income to those in need); the work disincentive is exaggerated by opponents. From the taxpayer's position, the same structure operates as enforced extraction: they bear the costs without consultation on design, and the program persists despite evidence that it generates long-term dependency rather than transitional support. The engine computes each seat's directionality from the structural data: administrators have low d (beneficiary position, controlling the rules), while taxpayers have high d (target position, paying without control). The classified types should diverge accordingly — the dependency-trap reading's own internal structure instantiates seat divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Income_support_recipients_exiting_labor_market have directionality near 0.0 (full beneficiary): they receive the transfer and their exit is partially enabled by it, even though the reading frames exit as dependency. Working_taxpayers have directionality near 1.0 (full target): they fund the arrangement without participation in its design or benefits. Poor_individuals_with_atrophying_skills have initially moderate d (receive support at first), but as skill atrophy accumulates, their realized extraction increases (they are harmed by the same structure that initially helped them). Program_administrators have low d (they control the arrangement and defend its legitimacy, even if they do not directly collect the extraction). Labor_market_employers have moderate d that drifts toward 0.0 as the program suppresses wage competition — they appear as payers but benefit from wage suppression. The identity-locking of recipients at low power prevents easy exit optimization; constrained taxpayers face real exit barriers (relocation, tax evasion, or political pressure).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was cyclical unemployment and poverty traps created by means-tested benefits (workers losing more in benefits than they gain in wages). The unconditional structure solved the immediate poverty trap by decoupling support from earnings. However, the mandate — temporary income support for cyclical unemployment — has outlived its functional application for many long-term recipients who are no longer primarily experiencing cyclical joblessness but structural non-participation driven by skill loss, identity fusion, and generational transmission of welfare dependence. The program persists not because it solves the founding problem for long-term recipients (the evidence is contested), but because (a) it has created a dependent constituency with political voice, (b) administrators have institutional interest in its continuation, (c) employers benefit from suppressed wages. Mandatrophy is evident in the rising theater ratio: more of the program's active infrastructure is now devoted to defending its legitimacy than to transitioning recipients into work. The program exhibits the classic zombie signature: the founding problem is substantially resolved for most participants, but the arrangement persists and has generated secondary dependency dynamics not present at founding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_atrophy_causality,
    'Is the observed decline in labor-force participation and earnings trajectories caused by the income support structure''s work disincentive, or by confounding factors (recession timing, regional deindustrialization, education levels, family obligations) that existed prior to program enrollment?',
    'Quasi-experimental design comparing similar demographic cohorts in jurisdictions with and without unconditional support, controlling for pre-program labor market conditions and measuring trajectory divergence post-program. Randomized controlled trials of program expansion.',
    'High causal attribution to the program would support the dependency-trap reading and justify policy restructuring toward conditional or time-limited support. Low attribution would undermine the reading''s empirical foundation and suggest support functions as intended (enabling survival while other factors shape re-entry).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(skill_atrophy_causality, empirical, 'Whether observed skill loss is caused by support structure or prior disadvantage.').

omega_variable(
    identity_locking_mechanism,
    'Does the atrophy of labor-market participation and skills arise from the income support structure itself (economic disincentive), or from identity fusion and stigma internalization where recipients view themselves as incompatible with work after extended non-participation, independent of the economic incentive structure?',
    'Qualitative research on re-entry trajectories, stigma narratives, and self-concept changes; measurement of ''why'' non-participation persists (financial calculation vs. identity/capability belief); post-program exit suppression trajectories (does stigma persist after the economic constraint is removed?).',
    'If primarily identity-locked: the constraint''s effective suppression is higher than the measured structural metric (the target carries the lock after exit), and remedies require identity reconstruction support, not just economic incentive restructuring. If primarily economic: targeted transition support and skill-building pathways could enable efficient re-entry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locking_mechanism, empirical, 'Whether suppression is structural (economic disincentive) or internalized (identity fusion).').

omega_variable(
    kernel_reading_contest,
    'Is unconditional income support fundamentally enabling autonomy and dignity (freedom-floor reading), or fundamentally creating work disincentive and dependency (dependency-trap reading)? Or does the same policy instantiate BOTH structures simultaneously for different populations?',
    'Decomposition by recipient subpopulation: identify which cohorts experience genuine autonomy expansion (caring for dependents, pursuing education, exiting abusive work situations) vs. which cohorts experience skill atrophy and identity loss. The policy may be a tangled rope for some seats and a rope for others.',
    'If the readings truly foreclose each other (only one can be structurally true), the policy cannot be simultaneously valid. If they coexist (both describe real dynamics for different populations), the constraint is better modeled as multiple constraint stories, not one. If one reading describes the policy''s intent and the other describes its effects, the mandate-drift analysis becomes central.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the dependency-trap and freedom-floor readings are mutually exclusive or coexist for different populations.').

omega_variable(
    fiscal_sustainability_horizon,
    'As the program matures and the proportion of long-term recipients grows (second- and third-generation dependence on support), does the fiscal cost become unsustainable, forcing either program restructuring, tax increases that deepen the extraction on working taxpayers, or benefit cuts that harm existing recipients?',
    'Long-term fiscal projections modeling recipient cohort aging, earnings trajectories, tax base changes, and program expansion. Historical analysis of similar programs (Scandinavian welfare states, permanent income guarantees in pilot programs) and their fiscal trajectories.',
    'If unsustainability is imminent, the constraint exhibits mandate drift: founded to solve temporary unemployment, it persists as a permanent structure with changing incentive effects. This triggers the mandatrophy analysis: the founding problem (cyclical unemployment) may be solved, but the constraint persists and has generated new dependency structures not present at founding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_horizon, empirical, 'Whether the program''s fiscal structure enables indefinite sustainability or requires eventual restructuring.').

omega_variable(
    counterfactual_labor_supply,
    'What is the actual price elasticity of labor supply with respect to income support levels? For each dollar of unconditional support increase, how many hours of work are forgone across the population?',
    'Econometric analysis of labor-supply responses to support level changes, geographic variation in support generosity, and natural experiments from program design changes (phase-out rate changes, payment frequency changes, eligibility threshold shifts).',
    'High elasticity (large work reduction per dollar of support) validates the dependency-trap reading''s causal mechanism. Low elasticity (work remains despite support) suggests the reading exaggerates the disincentive effect and that other factors (skill maintenance, social connection, identity) are more important than income in shaping participation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_labor_supply, empirical, 'Magnitude of labor-supply response to income support levels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(inco_tr_t5, income_support_commitment__dependency_trap_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__dependency_trap_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(inco_tr_t15, income_support_commitment__dependency_trap_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__dependency_trap_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(inco_tr_t25, income_support_commitment__dependency_trap_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(inco_be_t5, income_support_commitment__dependency_trap_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__dependency_trap_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(inco_be_t15, income_support_commitment__dependency_trap_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__dependency_trap_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(inco_be_t25, income_support_commitment__dependency_trap_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(inco_su_t5, income_support_commitment__dependency_trap_reading, suppression_requirement, 5, 0.43).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__dependency_trap_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(inco_su_t15, income_support_commitment__dependency_trap_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__dependency_trap_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(inco_su_t25, income_support_commitment__dependency_trap_reading, suppression_requirement, 25, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__dependency_trap_reading, 0.18).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__targeting_efficiency_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, labor_market_wage_suppression_through_reserve_army).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, intergenerational_welfare_transmission).

% DUAL FORMULATION NOTE:
% The 'income_support_commitment' kernel instantiates three structurally distinct constraints depending on which reading is adopted. The dependency-trap reading (this story) posits ε ≈ 0.68 (moderate-high extraction from productive to non-productive) with long-term skill loss and identity lock-in as harm mechanisms. The freedom-floor reading posits ε ≈ 0.15 (low extraction, high coordination benefit) with autonomy expansion and capability unfurling. The targeting-efficiency reading posits ε ≈ 0.45 (moderate extraction due to inefficient distribution, but reducible through means-testing). These three readings decompose the single institutional kernel into three ε-distinct constraint stories. The dependency-trap reading is downstream of the freedom-floor reading (critics of the freedom-floor claim invoke the dependency-trap mechanism), but causally independent in observables: both readings account for the same observable institution. Decomposition is necessary because the three readings would produce incoherent joint claims if merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__dependency_trap_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
