% ============================================================================
% CONSTRAINT STORY: targeting_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_targeting_efficiency_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: targeting_efficiency_reading
 *   human_readable: Income Support Concentration: Targeting Efficiency Reading
 *   domain: political_economy/welfare_state_design/social_policy
 *
 * SUMMARY:
 *   The targeting efficiency reading of income support policy instantiates a
 *   constraint where concentrated support justified by demonstrated need
 *   creates a snare mechanism for poor households. This reading holds that
 *   means-testing efficiently allocates scarce resources to those with
 *   greatest need, but the structural consequence is a trap: recipients face
 *   benefit cliffs (earning an additional dollar costs them $0.50–$1.00 in
 *   withdrawn benefits), administrative humiliation (documentation and
 *   verification requirements), and intergenerational stigma transmission.
 *   The reading applies demonstrably to major means-tested programs in the
 *   U.S. welfare state: TANF (Temporary Assistance for Needy Families), SNAP
 *   (Supplemental Nutrition Assistance Program), and many Medicaid designs. A
 *   household at $31,100 annual income receives approximately $19,100 in
 *   targeted benefits (housing subsidy, food assistance, healthcare). Under
 *   this reading, they are efficiently 'helped' — resources are concentrated
 *   on those with greatest need. But the constraint itself makes exit from
 *   the poverty trap difficult: earning $1,000 more triggers benefit
 *   clawbacks that net them less than the $1,000 gained in wage income. The
 *   efficiency gain (targeting instead of universal distribution) is
 *   purchased at the cost of intergenerational entrapment and administrative
 *   extraction. This reading is one of three interpretations of the kernel
 *   'income support commitment': the others being a freedom_floor_reading
 *   (everyone deserves a basic income floor regardless of means) and a
 *   dependency_trap_reading (focusing on how targeting creates psychological
 *   dependency). Each reading produces different victim sets, different
 *   measures of extractiveness, and different classification types from a
 *   single underlying policy domain.
 *
 * KEY AGENTS:
 *   - Means-tested beneficiaries (powerless/trapped): Primary victims. Households below targeting threshold experience maximum suppression and extraction via benefit cliffs and administrative burden.
 *   - Welfare stigma bearers (powerless/identity_locked): Secondary victims. Applicants internalize shame and deservingness testing; intergenerational transmission of stigma identity.
 *   - Budgetary efficiency advocates (institutional/arbitrage): Primary beneficiaries. Policy designers, legislators, fiscal hawks who benefit from targeting-justified austerity; experience the constraint as coordination (efficient allocation).
 *   - Means-testing administrators (institutional/arbitrage): Secondary beneficiaries. Caseworkers, verification systems, procedural architects who benefit from jobs and bureaucratic expansion.
 *   - Near-poor workers (moderate/constrained): Mixed agent. Just above eligibility threshold; benefit from existential reassurance of system but face precarity and taxation.
 *   - Administrative system (institutional/arbitrage): Piton perspective. Procedural machinery maintains targeting through institutional inertia despite degraded function.
 *   - Reform coalitions (organized/mobile): Scaffold perspective. Organized advocates for 'smarter targeting' and real-time eligibility verification; see targeting as temporary coordination problem with technological solution.
 *   - Analytical observer (analytical/analytical): Naturalization risk. Can falsely treat means-testing as immutable law of scarcity rather than contingent policy choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(targeting_efficiency_reading, 0.68).
domain_priors:suppression_score(targeting_efficiency_reading, 0.62).
domain_priors:theater_ratio(targeting_efficiency_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(targeting_efficiency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(targeting_efficiency_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(targeting_efficiency_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(targeting_efficiency_reading, snare).
narrative_ontology:human_readable(targeting_efficiency_reading, "Income Support Concentration: Targeting Efficiency Reading").
narrative_ontology:topic_domain(targeting_efficiency_reading, "political_economy/welfare_state_design/social_policy").

domain_priors:requires_active_enforcement(targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(targeting_efficiency_reading, formalized).
narrative_ontology:cs_authority_grounding(targeting_efficiency_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(targeting_efficiency_reading).
narrative_ontology:cs_kernel_id(targeting_efficiency_reading, income_support_commitment).
narrative_ontology:cs_reading_relation(targeting_efficiency_reading, freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation(targeting_efficiency_reading, dependency_trap_reading, influences).
narrative_ontology:cs_axiom(targeting_efficiency_reading, foundational, demonstrated_need_justifies_differential_access).
narrative_ontology:cs_axiom_status(demonstrated_need_justifies_differential_access, holdable).
narrative_ontology:cs_axiom(targeting_efficiency_reading, foundational, efficiency_maximization_under_budget_constraint).
narrative_ontology:cs_axiom_status(efficiency_maximization_under_budget_constraint, holdable).
narrative_ontology:cs_reference_frame(targeting_efficiency_reading, efficient_poverty_relief).
narrative_ontology:cs_drift_state(targeting_efficiency_reading, contemporary_means_testing_era, gap(codification_collapse, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(targeting_efficiency_reading, budgetary_efficiency_advocates).
narrative_ontology:constraint_beneficiary(targeting_efficiency_reading, means_testing_administrators).
narrative_ontology:constraint_victim(targeting_efficiency_reading, poor_households_under_targeting).
narrative_ontology:constraint_victim(targeting_efficiency_reading, welfare_stigma_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED BENEFICIARY (SNARE) — A household at $31,100 annual income receives $19,100 in targeted benefits. Under means-testing logic, they are 'helped.' But exit from the constraint requires either income growth (which triggers cliff effects and benefit loss) or permanent poverty. The trap mechanism: every dollar earned above the threshold costs them in withdrawn benefits. Maximum suppression and maximum extraction — the constraint is coercive and alternative exit routes (income mobility, skill investment) are structurally blocked by clawback design.
constraint_indexing:constraint_classification(targeting_efficiency_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STIGMATIZED APPLICANT (SNARE) — Over generational time, means-testing creates internalized suppression through welfare stigma. Applicants must prove need, disclose finances, submit to investigation. The constraint combines material extraction (benefit cliffs) with social suppression (the documented stigma and administrative humiliation of means-tested systems). Generational perspective shows intergenerational transmission of stigma identity — children of means-tested recipients internalize stigma as identity. The extraction mechanism persists not just through material incentives but through normalized shame.
constraint_indexing:constraint_classification(targeting_efficiency_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: BUDGETARY EFFICIENCY ADVOCATE (ROPE) — From this perspective, targeting is pure coordination: directing scarce resources to those with greatest need solves the collective action problem of poverty relief under budget constraint. The advocate experiences no extraction — they benefit from efficient allocation, and the constraint enables rather than blocks their goals. Net beneficiary position. The classification as Rope reflects genuine coordination function: targeting DOES solve the resource allocation problem IF you accept the constraint's framing (need = deservingness, efficiency = moral value).
constraint_indexing:constraint_classification(targeting_efficiency_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NEAR-POOR WORKER (TANGLED ROPE) — A household at $33,000 annual income, just above the eligibility cliff, receives no targeted benefits. They benefit modestly from the targeting constraint's existence (psychological reassurance that the system helps the worse-off) but bear costs (taxation to fund the system, and precarity — if income drops below threshold, they are trapped). Constrained exit because moving into or out of eligibility creates discontinuous benefit shocks. Mixed extraction: some benefit from the efficient targeting logic, but also constrained by the cliff design.
constraint_indexing:constraint_classification(targeting_efficiency_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized advocates for 'smarter targeting' (dynamic income verification, real-time eligibility, asset limits, behavioral requirements) see means-testing as a temporary coordination problem awaiting better administrative technology. They have agency and exit options: shifting to universal programs, relaxing eligibility verification, or building new platforms. This perspective is scaffold because it assumes the efficiency-targeting trade-off is solvable by better administration — a sunset clause implicit in 'smarter systems.' Moderate extraction because they have alternatives and see a path forward.
constraint_indexing:constraint_classification(targeting_efficiency_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ADMINISTRATIVE SYSTEM (PITON) — The welfare bureaucracy experiences targeting as performative ritual: elaborate means-testing procedures, documentation requirements, and caseworker investigations that consume resources without measurable improvement in allocation efficiency. Theater ratio high (0.35 is conservative for this view — actual theater closer to 0.65) because much administrative effort goes to compliance theater, not outcome improvement. The system persists through institutional inertia: eligibility workers have jobs, the procedures are codified, alternatives are politically fraught. Piton classification: degraded function maintained by institutional momentum.
constraint_indexing:constraint_classification(targeting_efficiency_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / TARGETING NATURALIZATION (SNARE) — From a civilizational/global perspective, this reading can naturalize targeting as inherent to resource scarcity: 'We cannot afford universal benefits; targeting is a natural law of limited budgets.' However, the structural data (specific beneficiaries, measurable stigma, designed cliff mechanisms) reveals this as a false snare — the extraction is not immutable but contingent on specific policy design choices. The analytical observer risks naturalizing a political commitment as an economic limit.
constraint_indexing:constraint_classification(targeting_efficiency_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(targeting_efficiency_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(targeting_efficiency_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(targeting_efficiency_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(targeting_efficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(targeting_efficiency_reading, TR),
    TR >= 0.70.

:- end_tests(targeting_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The targeting reading creates an asymmetric extraction mechanism where beneficiaries (budget advocates) gain efficiency gains and beneficiaries gain political durability, while victims (poor households) lose exit capacity, accumulate stigma, and face cliff-induced poverty traps. The value of 0.68 reflects that while efficiency gains are real (targeting does reduce per-recipient cost relative to universal programs), the extraction from poor households is substantial and intentional — the constraint concentrates resources on demonstrated need precisely by making non-need-demonstration (exit, income growth) costly. The extraction rises over time (0.52 → 0.68) as verification technology advances, allowing tighter targeting and stronger clawback mechanisms. Suppression (0.62): Moderate-high. Multiple suppression mechanisms operate: (1) material — benefit cliffs make income growth uneconomical; (2) administrative — documentation and verification requirements create compliance burdens and investigation anxiety; (3) psychological — welfare stigma internalized as identity. The suppression is not total (some recipients do exit poverty despite cliffs; some resist stigma) but substantial and architectural. Theater ratio (0.35): Moderate. The targeting constraint has genuine coordination function (it does allocate resources efficiently) but increasingly relies on performative administration (verification procedures that consume resources without proportional outcome improvement). The ratio rises over time as administrative sophistication increases without corresponding improvement in actual targeting accuracy — the system becomes more elaborate but not more effective.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the targeting_efficiency_reading's beneficiaries and victims is maximal. Efficiency advocates see coordination (Rope) — the constraint solves the allocation problem under budget constraint. Budgetary/administrative beneficiaries experience the constraint as enabling their goals (enabling efficient governance). But trapped beneficiaries see extraction (Snare) — the same constraint that 'efficiently allocates' resources to them also prevents their exit. The near-poor worker sees mixed extraction and coordination (Tangled Rope) — the system theoretically helps the worse-off (coordination function) but threatens them with precarity (extraction mechanism). The administrative system sees performative ritual (Piton) — verification procedures that persist through inertia without corresponding outcome improvement. The reform coalition sees a temporary problem with a technological solution (Scaffold) — better real-time eligibility systems will solve targeting tradeoffs. The analytical observer risks seeing a natural law (Snare naturalized as immutable scarcity trade-off) but the structural data reveals it as a political choice, not an economic limit.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position: beneficiaries of targeting efficiency gain from the constraint and face low exit costs (arbitrage capacity), while victims face high exit costs (trapped in benefit system, unable to earn without penalty) and bear suppression. The powerless/trapped victim experiences maximum d (≈0.95) → high f(d) ≈1.42 → high experienced χ. The institutional/arbitrage beneficiary experiences low d (≈0.10) → low f(d) ≈0.02 → negative or near-zero experienced χ (they benefit from the extraction mechanism). The moderate/constrained near-poor worker experiences intermediate d (≈0.65) → f(d) ≈1.00 → moderate χ. The analytical observer's d is derived from 'seeing the full structure' position (≈0.72) → f(d) ≈1.15, showing high experienced extraction from the analytical standpoint, but faces the risk of naturalizing the constraint (treating it as immutable law rather than policy choice).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved in this reading by clarifying that the targeting efficiency framing is NOT coordinated with the freedom_floor reading: they are different commitments to different principles. This reading commits to efficiency and deservingness-testing; the sibling reading commits to universalism and unconditional basic security. The snare classification emerges from the targeting_efficiency_reading's own axioms: if you commit to concentrated support justified by demonstrated need, you necessarily create (1) differential access (some get support, others don't) and (2) verification mechanisms to establish need. The extraction mechanism is endogenous to the reading — not a failure of targeting but its structural consequence. The mandatrophy resolves because the snare classification does not depend on claiming 'targeting is bad' — it depends on showing that the efficiency gain is purchased at the cost of entrapment and administrative extraction. Other readings (freedom_floor, dependency_trap) would show different classifications because they commit to different axioms about what income support owes to whom.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_ambiguity,
    'Is ''income support commitment'' grounded in a universalist principle (everyone deserves basic security) or a narrower principle (assistance for the poor)?',
    'Textual/historical analysis of foundational legislation, political rhetoric, and cross-national comparative institutional analysis of similar welfare systems',
    'If universalist: targeting reading forecloses the freedom_floor_reading (cannot hold both in same framework). If narrower: readings coexist — targeting and universalism are different applications of assistance principle. This determines the cs_structure.reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_ambiguity, conceptual, 'Whether the kernel commits to universal or narrower assistance principle').

omega_variable(
    means_testing_implementation_cost,
    'What percentage of targeted program budgets actually go to administrative overhead (casework, verification, enforcement) vs. direct benefits?',
    'Comparative cost accounting across targeted programs (TANF, SNAP, Medicaid) and universal programs (Social Security, Medicare); studies of administrative burden on recipients',
    'If overhead > 15%: targeting''s efficiency claim is undermined; snare extraction mechanism is stronger. If overhead < 5%: targeting is administratively efficient and snare classification weakens. Directly affects extractiveness calibration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(means_testing_implementation_cost, empirical, 'Administrative overhead as percentage of targeted program budgets').

omega_variable(
    benefit_cliff_trap_incidence,
    'What fraction of targeted-program recipients experience marginal tax rates > 50% due to benefit clawbacks?',
    'Longitudinal income data tracking recipient earnings changes and correlated benefit changes; RAND experiments measuring behavioral response to different clawback rates',
    'If high incidence: trap mechanism confirmed; snare classification strong; dependency_trap_reading gains empirical support. If low: benefit design is not inherently trapping; classification softens toward tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(benefit_cliff_trap_incidence, empirical, 'Prevalence of high marginal tax rates in targeted programs').

omega_variable(
    welfare_stigma_internalization_mechanisms,
    'Is welfare stigma primarily external (social judgment) or internalized (recipient self-perception) or architectural (designed into application/verification procedures)?',
    'Qualitative research on applicant experience; comparative analysis of programs with high vs low procedural stigma (e.g., EITC vs TANF); neurobehavioral studies of shame responses in means-testing contexts',
    'If architectural: suppression is intentional design feature; extraction mechanism is active. If internalized only: suppression depends on cultural context; classification may vary across jurisdictions. Affects whether suppression (0.62) is structural or contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_stigma_internalization_mechanisms, empirical, 'Sources of welfare stigma and their role in program suppression').

omega_variable(
    alternative_income_support_reading_coexistence,
    'Can the targeting_efficiency_reading and freedom_floor_reading coexist as live positions within a single welfare state framework, or do they logically foreclose each other?',
    'Jurisdictional analysis: identify welfare systems that claim both efficiency and universalism; analyze how different political coalitions instantiate different readings of the same income support commitment; interview policymakers about trade-off perception',
    'If coexist: reading_relations classify both as coexists_with or influences. If foreclose: reading_relations classify as forecloses. Determines the axiom status (holdable vs foreclosed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_income_support_reading_coexistence, conceptual, 'Logical coexistence vs foreclosure between targeting and universalism readings').

omega_variable(
    snare_vs_tangled_rope_boundary,
    'At what level of suppression and extractiveness does targeting cease to be pure extraction (snare) and become mixed coordination-extraction (tangled_rope)?',
    'Comparative case analysis of targeting systems with different design parameters (clawback rates, verification burden, benefit levels relative to poverty line); measurement of recipient perception of ''fairness'' vs ''trap''',
    'If this reading''s actual metrics show higher extracted value for vulnerable groups but some genuine coordination function exists: reclassification to tangled_rope warranted. If extraction is unambiguously asymmetric: snare confirmed. Affects claimed_type calibration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(snare_vs_tangled_rope_boundary, empirical, 'Boundary between pure extraction (snare) and mixed coordination-extraction (tangled rope)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(targeting_efficiency_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(targ_tr_t0, targeting_efficiency_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(targ_tr_t10, targeting_efficiency_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(targ_tr_t20, targeting_efficiency_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(targ_be_t0, targeting_efficiency_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(targ_be_t10, targeting_efficiency_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(targ_be_t20, targeting_efficiency_reading, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(targeting_efficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(targeting_efficiency_reading, 0.12).
narrative_ontology:affects_constraint(targeting_efficiency_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(targeting_efficiency_reading, dependency_trap_reading).

% DUAL FORMULATION NOTE:
% The targeting_efficiency_reading, freedom_floor_reading, and dependency_trap_reading form a constraint family decomposed from the contested kernel 'income_support_commitment.' Each reading has its own ε, its own beneficiary/victim structure, and its own classification. They are linked by network.affects_constraints because they represent alternative instantiations of the same kernel: different readings produce different downstream policy consequences and different alliance structures. The targeting_efficiency_reading produces snare and rope classifications depending on position; the freedom_floor_reading typically produces rope and scaffold; the dependency_trap_reading typically produces tangled_rope and snare with different victim sets. The decomposition reflects that 'targeting vs universalism' cannot be modeled as a single constraint with multiple observables — it is genuinely two distinct commitments with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
