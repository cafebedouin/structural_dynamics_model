% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__freedom_floor_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Autonomy-Enabling Floor
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   Unconditional income support—a guaranteed basic income paid to all
 *   citizens regardless of employment status or wealth—presents as a
 *   coordination mechanism that removes structural coercion from labor market
 *   participation. This constraint story instantiates the
 *   FREEDOM_FLOOR_READING of the contested kernel
 *   'unconditional_income_support': the reading that emphasizes autonomy
 *   enablement, stigma elimination, and economic security as preconditions
 *   for free labor market participation. This reading is one of three
 *   competing interpretations in active policy dispute. The empirical basis
 *   (Alaska Permanent Fund, Kenya GiveDirectly trials, Finland 2017–2018
 *   pilot) shows minimal labor supply reduction (2–5%), suggesting the
 *   autonomy reading is structurally sound: the floor enables choice rather
 *   than incentivizing withdrawal. The constraint exhibits low extractiveness
 *   (0.18) and minimal theater (0.25)—the policy does what it claims without
 *   significant performative content. Beneficiaries are identified as those
 *   structurally excluded from dignified labor market participation:
 *   precarious workers, caregivers, artists, and abuse victims. No victims
 *   are claimed in this reading—the structure is reframed as Pareto
 *   improvement where previously-constrained populations gain autonomy
 *   without imposing costs on others. The challenge is empirical: does the
 *   policy actually function as autonomy-enablement in practice, or does
 *   secondary extraction (via predatory lending to newly-liquid populations)
 *   or persistent stigma (despite policy design) undermine the rope
 *   classification?
 *
 * KEY AGENTS:
 *   - Precarious Workers (powerless/mobile): Gig workers, domestic care, underemployed—experience constraint as removal of wage-suppression coercion. Primary beneficiary. Shift from trapped to mobile exit options.
 *   - Caregivers (moderate/mobile): Full-time care providers (childcare, elder care) who are structurally excluded from wage labor or forced into dependence. Benefit from dignification of care work. Secondary beneficiary.
 *   - Labor Movement (organized/mobile): Collective actors; benefit from restored worker bargaining power as floor removes desperation wage-suppression. Coordination function enabled.
 *   - Employers Dependent on Precarious Labor (powerful/constrained): Extraction dimension—lose ability to suppress wages via desperation. Coordination benefit dimension—gain labor supply stability. Mixed experience (tangled_rope).
 *   - Social Reproduction Institutions (institutional/arbitrage): Public health, education, social services; benefit from reduced emergency demand and elimination of means-testing bureaucracy. No extraction experienced.
 *   - Analytical Observer (analytical/analytical): Sees low extractiveness and high coordination function. Constraint enables autonomy as a precondition for voluntary exchange rather than coercive subsistence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.18).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.12).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support as Autonomy-Enabling Floor").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, '14498a3d-ec62-496e-9934-ff16a31baf43').
narrative_ontology:cs_kernel_codification('14498a3d-ec62-496e-9934-ff16a31baf43', formalized).
narrative_ontology:cs_authority_grounding('14498a3d-ec62-496e-9934-ff16a31baf43', expertise).
narrative_ontology:cs_interpretation_layer_present('14498a3d-ec62-496e-9934-ff16a31baf43').
narrative_ontology:cs_reading_relation('14498a3d-ec62-496e-9934-ff16a31baf43', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('14498a3d-ec62-496e-9934-ff16a31baf43', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('14498a3d-ec62-496e-9934-ff16a31baf43', foundational, labor_market_coercion_removable_via_income_floor).
narrative_ontology:cs_axiom_status(labor_market_coercion_removable_via_income_floor, holdable).
narrative_ontology:cs_axiom_grounding('14498a3d-ec62-496e-9934-ff16a31baf43', labor_market_coercion_removable_via_income_floor, empirically_contingent).
narrative_ontology:cs_axiom('14498a3d-ec62-496e-9934-ff16a31baf43', secondary, universality_as_dignity_preservation).
narrative_ontology:cs_axiom_status(universality_as_dignity_preservation, holdable).
narrative_ontology:cs_axiom_grounding('14498a3d-ec62-496e-9934-ff16a31baf43', universality_as_dignity_preservation, deontological).
narrative_ontology:cs_reference_frame('14498a3d-ec62-496e-9934-ff16a31baf43', labor_market_autonomy_commitment).
narrative_ontology:cs_drift_state('14498a3d-ec62-496e-9934-ff16a31baf43', contemporary_welfare_state_maturity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('14498a3d-ec62-496e-9934-ff16a31baf43', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, artists).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, abuse_victims).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, workers_with_constrained_labor_options).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (ROPE) — A gig worker, domestic care provider, or underemployed artist experiences unconditional income support as removing coercion from labor market participation. The baseline income enables rejecting exploitative work, negotiating better terms, or transitioning between sectors. No extraction experienced — the constraint is a coordination mechanism enabling voluntary labor market participation. Exit option shifts from trapped (forced to accept any work) to mobile (can choose). Time horizon: biographical — the autonomy effect operates across a career lifetime.
constraint_indexing:constraint_classification(unconditional_income_support__freedom_floor_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: CAREGIVER (ROPE) — Parent providing full-time childcare or adult caring for aging relative experiences the constraint as dignifying unpaid care work. Without unconditional income support, caregiving forces dependence on a wage-earner or means-tested welfare (stigmatized, conditional, invasive). With unconditional support, caregiving becomes a legitimate autonomous choice rather than an economic trap. Coordination function: the mechanism enables the essential social reproduction work (childcare, elder care) that markets systematically undervalue. No extraction — the beneficiary is a victim of market undervaluation that the constraint corrects.
constraint_indexing:constraint_classification(unconditional_income_support__freedom_floor_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: LABOR MOVEMENT (ROPE) — Organized labor experiences unconditional income support as restoring worker bargaining power eroded over decades of precariat expansion and labor market deregulation. The floor removes the desperation that unions historically fought to escape — accepting any wage to survive. Collective exit capacity is restored. The constraint is a coordination mechanism enabling the labor movement's core function: collective protection against wage-suppression dynamics. Coordination benefit is high; extraction is zero from this perspective.
constraint_indexing:constraint_classification(unconditional_income_support__freedom_floor_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: SOCIAL REPRODUCTION COALITION (ROPE) — Public health, education, and social service institutions experience unconditional income support as coordinating mechanism that reduces their own administrative burden and enables their core functions. Unconditional income reduces demand for emergency services (mental health crisis, substance abuse intervention) and frees resources for preventive work. Replaces means-testing bureaucracy (invasive, expensive to administer, creates perverse incentives) with universal platform. Net benefit to institutional actors — coordination function, not extraction.
constraint_indexing:constraint_classification(unconditional_income_support__freedom_floor_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EMPLOYERS DEPENDENT ON PRECARIOUS LABOR (TANGLED ROPE) — Firms relying on wage suppression through desperate labor supply experience extraction: unconditional income support shifts labor market dynamics, reducing their ability to extract surplus via worker desperation. However, from a production-coordination perspective, the constraint also enables more stable labor supply and reduces turnover costs. Mixed: extraction on dimension of wage suppression, coordination benefit on dimension of labor stability. Medium extraction experienced; coordination function present. Time horizon: generational — the labor supply shift affects sector composition over generations.
constraint_indexing:constraint_classification(unconditional_income_support__freedom_floor_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / AUTONOMY READING (ROPE) — From a civilizational/global perspective, unconditional income support functions as a coordination mechanism that removes the structural coercion inherent to market economies without externally-imposed labor. The constraint enables the social contract where material security does not depend on labor market participation. Extractiveness is minimal; the mechanism is largely functional. Theater ratio is low — the policy does what it claims (provides income unconditionally) without significant performative content.
constraint_indexing:constraint_classification(unconditional_income_support__freedom_floor_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__freedom_floor_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unconditional_income_support__freedom_floor_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unconditional_income_support__freedom_floor_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(unconditional_income_support__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint provides unconditional income—it is not extractive in the classical sense (taking from beneficiaries). The measure reflects that some labor supply reduction is empirically observed (2–5% in high-quality studies), representing foregone economic output; this is counted as minimal extraction cost. The trajectory (0.12 → 0.18 over 10 years) reflects potential secondary extraction via predatory lending targeting newly-liquid populations, which partially offsets the autonomy benefit. This low value is consistent with rope classification. Suppression (0.12): Very low. The constraint explicitly removes suppression mechanisms: it eliminates conditional welfare surveillance, ends means-testing invasiveness, and removes the desperation that forces acceptance of exploitative work. The small residual (0.12) reflects unavoidable administrative overhead and potential cultural stigma persistence despite policy universality. Theater ratio (0.25): Very low. The policy does what it claims: it provides unconditional income. There is minimal performative content—no complex eligibility verification theater, no means-testing ritual, no conditionality enforcement. The small non-zero value reflects that political justification requires framing the transfer as 'investment' or 'dividend' rather than pure redistribution, which adds rhetorical theater to the policy defense.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a significant perspectival gap between the primary beneficiary (precarious worker: rope) and the employer dependent on precarious labor (tangled rope). The worker experiences pure coordination and autonomy enablement; the employer experiences both coordination benefit (more stable labor supply) and extraction (loss of wage-suppression capacity). The analytical observer and social reproduction coalition agree with the beneficiary on rope classification, while the dependency-trap reading (embodied as a sibling constraint) would classify this same structural configuration as snare from the precarious-worker perspective, claiming that 'unconditional income creates dependency.' The gap reveals the kernel contest: can the same policy be both autonomy-enabling (freedom floor) and dependency-creating (incentive distortion)? This reading asserts that the beneficiary's lived experience (autonomy, choice, dignity) is more structurally informative than a presumed behavioral response (labor supply reduction) to the policy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the precarious-worker perspective is derived from the constraint's structure: they are primary beneficiaries (d ≈ 0.1, beneficiary + mobile exit) experiencing low effective extraction. The caregiver perspective has d ≈ 0.15 (beneficiary correcting market undervaluation). The labor movement has d ≈ 0.2 (beneficiary restoring bargaining power). The employer perspective flips: d ≈ 0.65 (partial victim of wage-suppression extraction loss + beneficiary of stability coordination, yielding mixed d). The analytical observer uses canonical d ≈ 0.73 (analytical context, moderate f(d) ≈ 1.15). Each perspective's d is stable across the biographical/generational horizon because the autonomy structure does not depend on time-frame—a precarious worker benefits from autonomy in the short term and long term equally. The constraint's directionality is fundamentally asymmetric: large positive effect on workers with constrained options, small mixed effect on powerful employers, zero or negative effect on structured victims. This asymmetry supports rope rather than mountain—the constraint is not universal law, it is contingent institutional choice that beneficiaries can perceive and powerful agents can resist.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids the mandatrophy (extraction disguised as coordination) by remaining in the low-extraction regime (ε=0.18). The core claim—that unconditional income enables autonomy—is not being used to hide extractive mechanisms. The empirical tests are clear: labor supply reduction < 5% (enabling autonomy reading), secondary extraction via predatory lending (potential hidden cost), and stigma persistence (potential hidden cost). If those empirical tests fail—if labor supply reduction exceeds 15%, or if secondary extraction exceeds 30% of transfer value, or if stigma persists fully despite universality—the classification may shift toward tangled_rope or snare. But under the current empirical record, the constraint functions as claimed. The mandatrophy risk is located in the sibling dependency_trap_reading, which claims the same policy is actually pure extraction disguised as redistribution. The committer frame surfaces this explicitly via omega variables and reading_relations rather than hiding it within the metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_empirical_response,
    'What is the actual labor supply response to unconditional income support at various transfer levels and in different labor market contexts?',
    'Randomized controlled trials (Kenya GiveDirectly, Finland pilot 2017–2018), quasi-experimental variation (Alaska Permanent Fund), and observational studies comparing labor force participation pre/post implementation. Measurement of full-time employment, part-time work, sector switching, and unpaid care work entry.',
    'If labor supply reduction < 3%: rope classification confirmed — minimal extraction, coordination mechanism works as designed. If labor supply reduction > 15%: classification may shift toward tangled_rope or snare from employer perspectives; dependency concerns gain empirical support. If heterogeneous response (some increase hours, some exit): constraint enables labor market choice, confirming autonomy reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_supply_empirical_response, empirical, 'Empirical labor supply response to unconditional income transfers').

omega_variable(
    kernel_framing_contest,
    'Is unconditional income support fundamentally an autonomy-enabling mechanism or fundamentally an incentive-distorting subsidy? Can both framings coexist within a single policy implementation, or do they require incompatible design choices?',
    'Design analysis: identify specific policy parameters (transfer level, funding mechanism, conditionality exclusions, relation to minimum wage) that activate each reading. If different parameters are required, the readings are incompatible framings of a contested kernel. If the same policy instantiates both readings for different agents, the constraint exhibits genuine hybridity (tangled_rope from some perspectives).',
    'If framings are incompatible: kernel decomposition justified — write separate constraint stories for each reading. If framings coexist: the constraint is genuinely multi-perspectival, and the presheaf over readings IS the analytical output. This omega resolves the strategic authoring question: is this one reading of a contested kernel, or is it a constraint that unifies contradictory claims?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_contest, conceptual, 'Whether autonomy-enabling and incentive-distorting readings are compatible framings or incompatible commitments').

omega_variable(
    extraction_via_financialization,
    'Does unconditional income support, by increasing purchasing power in precarious populations, create new extraction opportunities via predatory lending, payday loans, and financial services targeting the newly-liquid segment?',
    'Observational analysis of credit market dynamics in post-implementation regions: consumer debt growth, payday loan uptake, subprime loan origination, and effective interest rates paid by UIS recipients vs. pre-UIS baselines. Comparison across contexts with different financial regulation.',
    'If secondary extraction via financialization > 30% of transfer value: effective extractiveness rises, and the constraint becomes tangled_rope or snare from the precarious-worker perspective (coordination benefit + extraction via debt trap). If negligible (< 5%): autonomy reading confirmed. Medium likelihood — depends on regulatory context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_via_financialization, empirical, 'Secondary extraction via predatory lending targeting newly-liquid populations').

omega_variable(
    welfare_stigma_persistence,
    'Does the elimination of conditionality and means-testing actually eliminate welfare stigma, or does stigma persist due to cultural framing independent of policy design?',
    'Ethnographic and survey-based analysis of benefit uptake patterns, self-identification effects, and reported dignity/shame around receiving unconditional transfers. Comparison of stigma effects between means-tested welfare, conditional transfers, and unconditional income support in same-country contexts (e.g., comparing TANF, EITC, and hypothetical UBI in USA).',
    'If stigma fully eliminated: the constraint removes a genuine coercive mechanism (dignity cost). If stigma partially persistent: the constraint provides autonomy improvement but not full emancipation from social judgment. This affects the characterization of suppression: reduced but not eliminated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_stigma_persistence, empirical, 'Persistence of welfare stigma independent of conditionality status').

omega_variable(
    universality_vs_targeting_trade_off,
    'Does the universality of unconditional income support (providing to all regardless of need) represent a genuine autonomy improvement over targeted assistance, or does it constitute inefficient redistribution that could fund higher targeted benefits with same fiscal cost?',
    'Comparative fiscal analysis: cost of universal floor at $X vs. equivalent-cost targeted negative income tax or expanded earned income tax credit. Measurement of poverty reduction per dollar spent. Analysis of leakage (benefits to non-poor) vs. coverage (reaching all who need assistance). Democratic legitimacy assessment: is universality a feature (eliminates surveillance, supports broad coalition) or inefficiency (resources dispersed thinly)?',
    'If universality proves fiscally efficient and politically stabilizing: supports rope classification and autonomy reading. If universality is inefficient but politically necessary: the constraint exhibits tangled_rope structure (coordination benefit via broad coalition + some inefficient transfer). This omega connects to the sibling universality_paradox_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_vs_targeting_trade_off, empirical, 'Fiscal trade-off between universality and targeted assistance').

omega_variable(
    reading_incompatibility_with_dependency_trap,
    'Can the autonomy-enabling reading and the dependency-trap reading (the sibling) coexist within a single policy framework, or does empirical confirmation of one reading logically foreclose the other?',
    'This is a committer-axis omega. Resolve by analyzing whether the core axioms of each reading are empirically falsifiable together or are alternative interpretations of the same phenomena. If labor supply reduction occurs but is experienced as autonomous choice (workers exit exploitative sectors), does this confirm or refute the dependency-trap reading? If no labor supply reduction occurs, does this refute the dependency reading or the autonomy reading? The resolution mechanism is conceptual: identify the observation that would constitute foreclosure vs. coexistence.',
    'If readings foreclose each other: kernel is actually decomposed into distinct constraints with different ε values. If readings coexist: the constraint genuinely exhibits multiple simultaneous classifications depending on observer position. This determines the authoring answer to the kernel commission.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_incompatibility_with_dependency_trap, conceptual, 'Whether autonomy-enabling and dependency-trap readings foreclose each other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__freedom_floor_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__freedom_floor_reading, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__freedom_floor_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__freedom_floor_reading, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel: unconditional_income_support. Sibling readings with different ε values and classifications are linked via network.affects_constraints. The kernel decomposition follows ε-invariance: if evaluating the same policy produces different ε values depending on which interpretation (autonomy-enabling vs. dependency-trap vs. universality-paradox) you adopt, those are different constraints requiring separate stories. They are not alternate perspectives on a single constraint, but genuinely distinct claims about the same policy object.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
