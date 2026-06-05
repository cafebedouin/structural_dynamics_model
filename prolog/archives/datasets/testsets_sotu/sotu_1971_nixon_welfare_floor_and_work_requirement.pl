% ============================================================================
% CONSTRAINT STORY: sotu_1971_nixon_welfare_floor_and_work_requirement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1971_nixon_welfare_floor_and_work_requirement, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1971_nixon_welfare_floor_and_work_requirement
 *   human_readable: Income Floor with Mandatory Work Requirements (Nixon 1971)
 *   domain: social_policy/welfare_reform
 *
 * SUMMARY:
 *   In 1971, President Nixon proposed the Family Assistance Plan, a
 *   guaranteed minimum income floor for all families with children paired
 *   with mandatory work requirements and incentive structures for able-bodied
 *   recipients. The constraint aimed to eliminate means-testing stigma while
 *   creating a disciplinary mechanism that separated deserving poor (unable
 *   to work, elderly, disabled) from undeserving poor (able but unwilling).
 *   Taxpayers and working families benefit from reduced fraud and behavioral
 *   assurance; welfare recipients gain income stability and
 *   dignity-preserving access without means-testing scrutiny; but they lose
 *   discretionary non-work income and gain mandatory work participation.
 *   Children benefit from increased family resources but potentially lose
 *   parental time availability. The constraint operates as a hybrid
 *   coordination-extraction mechanism: it genuinely solves the problem of how
 *   to provide income security without creating work disincentives, but it
 *   also extracts labor participation and behavioral conformity from welfare
 *   recipients, particularly single parents with childcare barriers. The
 *   measurement trajectory shows extractiveness rising sharply in years 0-2
 *   (as work requirements become operationalized and understood as
 *   non-negotiable), then plateauing as recipients adapt. Theater_ratio rises
 *   steadily as work-requirement bureaucracy develops, suggesting that the
 *   behavioral monitoring and documentation of work-seeking efforts become
 *   increasingly performative over time.
 *
 * KEY AGENTS:
 *   - Single Parents with Childcare Burden: Primary victim (powerless/trapped) — cannot exit work requirements due to childcare costs and labor-market barriers; structurally trapped by requirement to work while lacking childcare support
 *   - Able-Bodied Welfare Recipients: Secondary victim (moderate/constrained) — face work requirements with high costs; can exit through employment but lose income stability and discretionary non-work time
 *   - Children in Low-Income Families: Indirect victim/beneficiary (powerless/trapped) — gain income resources but lose parental time; extraction is hidden in time poverty and developmental stress
 *   - Taxpayers and Working Families: Primary beneficiary (institutional/arbitrage) — gain assurance that welfare system discourages free-riding; experience reduced fraud and means-testing stigma
 *   - Fiscal Conservatives: Secondary beneficiary (powerful/arbitrage) — benefit from narrative of behavioral discipline and work incentives; political capital from 'tough on welfare' positioning
 *   - Welfare Bureaucracy: Institutional maintainer (institutional/arbitrage) — transitions from means-testing verification to work-requirement monitoring; maintains theater through procedural complexity
 *   - Disability Advocates and Social Workers: Organized observers (organized/constrained) — see extraction from misclassified disabled recipients; constrained by institutional power of work-requirement regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1971_nixon_welfare_floor_and_work_requirement, 0.52).
domain_priors:suppression_score(sotu_1971_nixon_welfare_floor_and_work_requirement, 0.65).
domain_priors:theater_ratio(sotu_1971_nixon_welfare_floor_and_work_requirement, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1971_nixon_welfare_floor_and_work_requirement, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1971_nixon_welfare_floor_and_work_requirement, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1971_nixon_welfare_floor_and_work_requirement, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1971_nixon_welfare_floor_and_work_requirement, tangled_rope).
narrative_ontology:human_readable(sotu_1971_nixon_welfare_floor_and_work_requirement, "Income Floor with Mandatory Work Requirements (Nixon 1971)").
narrative_ontology:topic_domain(sotu_1971_nixon_welfare_floor_and_work_requirement, "social_policy/welfare_reform").

domain_priors:requires_active_enforcement(sotu_1971_nixon_welfare_floor_and_work_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1971_nixon_welfare_floor_and_work_requirement, taxpayers_and_working_families).
narrative_ontology:constraint_beneficiary(sotu_1971_nixon_welfare_floor_and_work_requirement, fiscal_conservatives).
narrative_ontology:constraint_victim(sotu_1971_nixon_welfare_floor_and_work_requirement, welfare_recipients_unable_to_work).
narrative_ontology:constraint_victim(sotu_1971_nixon_welfare_floor_and_work_requirement, single_parents_with_childcare_burden).
narrative_ontology:constraint_victim(sotu_1971_nixon_welfare_floor_and_work_requirement, children_in_low_income_families).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SINGLE PARENT WITH CHILDCARE BURDEN (SNARE) — Trapped by the requirement to work while lacking affordable childcare, accessible employment, and transportation. The income floor exists but only if work requirements are met. Discretionary non-work income is eliminated. Suppression is structural: labor market entry is blocked by childcare costs that exceed the minimum income supplement. No exit option exists that preserves family income and child welfare.
constraint_indexing:constraint_classification(sotu_1971_nixon_welfare_floor_and_work_requirement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ABLE-BODIED WELFARE RECIPIENT (TANGLED ROPE) — Experiences genuine coordination benefit (income floor eliminates means-testing stigma, provides stability) alongside asymmetric extraction (mandatory work requirement without childcare support, loss of discretionary income, labor-market discipline). Can exit through employment but at significant cost (foregone non-work income, time poverty). The constraint coordinates family stability while extracting work participation.
constraint_indexing:constraint_classification(sotu_1971_nixon_welfare_floor_and_work_requirement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TAXPAYERS AND WORKING FAMILIES (ROPE) — Benefit from reduced welfare fraud, means-testing elimination, and the disciplinary signal that work is required. The constraint solves a coordination problem: how to ensure welfare supports families without creating perverse incentives. This agent perceives the constraint as pure coordination with minimal cost extraction. Can arbitrage by supporting the program (political capital, tax benefit framing).
constraint_indexing:constraint_classification(sotu_1971_nixon_welfare_floor_and_work_requirement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CHILD ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Organized agents (child welfare advocates, anti-poverty researchers) see coordination benefit (stable income floor improves child development outcomes) but also extraction (work requirements without childcare support, labor-market discipline, dignity extraction via stigma reduction paired with behavioral conditioning). Constrained by need to work within existing policy frameworks; cannot fully exit without losing credibility.
constraint_indexing:constraint_classification(sotu_1971_nixon_welfare_floor_and_work_requirement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WELFARE BUREAUCRACY (PITON) — The administrative system transitions from means-testing verification to work-requirement monitoring, but much of the bureaucratic theater persists under new form. Eligibility verification is replaced by work verification, but the performative aspect (documenting worthiness) remains. Theater_ratio is elevated because the 'work requirement' becomes a ritual of behavioral monitoring rather than functional labor-force integration. The bureaucracy maintains itself through procedural complexity.
constraint_indexing:constraint_classification(sotu_1971_nixon_welfare_floor_and_work_requirement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LABOR SUPPLY NATURALISM (MOUNTAIN) — From a civilizational perspective, the constraint appears to articulate an immutable principle: able-bodied adults must work to receive income; income floors must include work incentives. This perspective naturalizes what is actually a constructed policy choice — treating labor supply responsiveness, deservingness categories, and work ethic as natural laws rather than contingent institutional arrangements. The engine identifies this as a false summit.
constraint_indexing:constraint_classification(sotu_1971_nixon_welfare_floor_and_work_requirement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1971_nixon_welfare_floor_and_work_requirement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1971_nixon_welfare_floor_and_work_requirement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1971_nixon_welfare_floor_and_work_requirement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1971_nixon_welfare_floor_and_work_requirement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1971_nixon_welfare_floor_and_work_requirement, TR),
    TR >= 0.70.

:- end_tests(sotu_1971_nixon_welfare_floor_and_work_requirement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts work participation and behavioral conformity from welfare recipients, particularly those with structural barriers to employment (single parents, disabled workers misclassified as able-bodied). The extraction is not total because the income floor provides genuine material benefit; recipients are not impoverished into compliance but rather conditioned into compliance. The measurement trajectory shows initial rise (0.35 → 0.48) as work requirements become operationalized, then plateau (0.48 → 0.55) as the regime stabilizes. Suppression (0.65): High. Childcare costs, labor-market discrimination, disability misclassification, and transportation barriers create structural obstacles to work participation. The 'voluntary' nature of the income floor is conditional on meeting work requirements that many recipients cannot realistically meet. The constraint does not trap recipients through legal prohibition but through economic necessity: the income floor exists only if work requirements are satisfied, creating de facto compulsion. Theater_ratio (0.58): Moderate-high. Work-requirement verification becomes a bureaucratic ritual over time. Case workers document job-seeking efforts, training participation, and employment interviews in ways that mirror means-testing documentation practices. The theater increases (0.42 → 0.50 → 0.58) as the regime matures because the performative aspects (demonstrating worthiness through work-seeking behavior) become more ritualized. Income floor provision (the genuine coordination benefit) remains constant; the behavioral monitoring (theatrical extraction mechanism) expands. Claimed Type (tangled_rope): The constraint genuinely coordinates family income stability with work participation incentives (rope benefit) but extracts labor compliance and behavioral conformity from recipients unable to meet work requirements (snare extraction). The coexistence of genuine coordination and asymmetric extraction is the signature tangled rope structure.
 *
 * PERSPECTIVAL GAP:
 *   The constraint achieves maximum perspectival divergence because the same institutional structure (income floor + work requirement) produces opposite structural effects for beneficiaries versus victims. Taxpayers perceive pure coordination (Rope): 'This solves the problem of how to help poor families without creating work disincentives.' Single parents perceive a trap (Snare): 'I cannot work because childcare costs exceed the income floor, but I cannot access the income floor without working.' This is not a communication gap — it reflects real structural asymmetry. The beneficiary's experience (coordination) and the victim's experience (extraction) are both accurate descriptions of how the constraint operates. The perspectival gap is a diagnostic signal of tangled rope structure: genuine coordination function coexisting with asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation operates as follows: (1) Taxpayers and working families are declared beneficiaries (constraint reduces perceived free-riding, provides behavioral assurance). With institutional power and arbitrage exit, their derived d ≈ 0.05-0.15 (full beneficiary range). f(d) ≈ -0.12, producing negative effective extraction χ — they benefit. (2) Single parents unable to work due to childcare barriers are primary victims. Powerless, trapped exit, high structural extraction. Derived d ≈ 0.90-0.95. f(d) ≈ 1.40, producing high effective extraction χ. (3) Able-bodied recipients able to work but at significant cost are secondary victims. Moderate power, constrained exit. Derived d ≈ 0.55-0.65. f(d) ≈ 0.75, producing moderate effective extraction χ. (4) Welfare bureaucracy benefits from regime expansion and funding. Institutional power, arbitrage exit. Derived d ≈ 0.10-0.20. f(d) ≈ 0.00, low extraction experienced. The perspectival gap (rope for beneficiaries, snare for victims, tangled rope for moderate actors) is a direct consequence of the beneficiary/victim structure and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that tangled rope is the correct classification precisely because beneficiaries (taxpayers) genuinely perceive the constraint as coordination (rope) while victims (welfare recipients unable to meet work requirements) genuinely perceive it as extraction (snare). The temptation to collapse to a single type ('This is really just a work incentive program' = Rope only, or 'This is really coercive behavioral conditioning' = Snare only) misconstrues the structure. The constraint IS both coordination and extraction — these are not alternative interpretations but simultaneous structural features. Taxpayers benefit from reduced fraud perception (genuine coordination benefit); welfare recipients are extracted from (behavioral compliance requirement). The mandatrophy is resolved by the indexical framework: from the beneficiary index (institutional power, arbitrage), the constraint is Rope; from the victim index (powerless, trapped), the constraint is Snare; from the analytical index (civilizational scope), the constraint is Tangled Rope. No single type is 'correct' — they are all accurate from their respective indices. The false summit detector flags the mountain perspective (analytical observer naturalizing work requirement as law) correctly: the work requirement is a contingent institutional choice, not an immutable principle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    childcare_cost_structural_trap,
    'Are childcare costs an external structural barrier (unmeasured poverty) or a legitimate labor-market cost that welfare recipients should bear?',
    'Empirical comparison: (a) childcare cost as percentage of minimum income across state/regional markets; (b) employment rates of welfare recipients with vs without childcare subsidies; (c) comparison to control group without childcare barriers',
    'If childcare is unmeasured barrier: single parents are trapped by structural factors, not individual work capacity. Snare classification becomes appropriate for all low-income parents. If childcare is legitimate labor cost: constraint is more rope-like for those who can absorb the cost. Classification gap reveals whether work requirement assumes household resource sharing (two-parent model) that single parents cannot access.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(childcare_cost_structural_trap, empirical, 'Whether childcare costs create structural traps').

omega_variable(
    deserving_undeserving_taxonomy_coherence,
    'Does the ''deserving/undeserving'' distinction produce a coherent category separation in practice, or do situational factors (disability onset, job loss, family disruption) make the distinction unstable?',
    'Longitudinal analysis of welfare recipients: (a) tracking of those classified as ''able-bodied'' who subsequently develop work-limiting conditions; (b) employment outcome tracking to identify those who cannot sustain work despite good-faith effort; (c) comparison of ex-ante classifications to realized outcomes',
    'If distinction is stable: work requirements accurately target the right population. If unstable: work requirement creates extraction from misclassified disabled/displaced workers. This determines whether the suppression value (0.65) is justified as behavioral incentive or revealed as coercive misidentification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deserving_undeserving_taxonomy_coherence, empirical, 'Coherence of deserving/undeserving distinction in practice').

omega_variable(
    stigma_elimination_trade_off,
    'Does replacing means-testing with work requirements eliminate stigma or merely substitute one form of degradation (unworthiness of means-testing) for another (unworthiness of joblessness)?',
    'Qualitative research: welfare recipient narratives comparing stigma experience under means-testing vs work-requirement regimes; survey data on public attitudes toward welfare recipients vs unemployed workers; longitudinal identity and self-concept measures',
    'If work requirement reduces stigma: constraint achieves stated goal (dignity-preserving income floor). If stigma merely shifts form: constraint extracts psychological cost alongside income extraction; the dignity benefit is theatrical. This determines whether beneficiaries genuinely experience rope (coordination with dignity) or tangled_rope (extraction disguised as dignity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_elimination_trade_off, empirical, 'Whether work requirements reduce or merely relocate stigma').

omega_variable(
    work_incentive_magnitude_calibration,
    'Are work incentives (income supplements for employment) sufficient to overcome labor-market barriers, or are they merely performative signals of behavioral expectation?',
    'Comparative labor-supply elasticity: (a) employment rate increases under income floor alone vs income floor + work incentive; (b) wage-income correlation for welfare recipients with vs without incentive structures; (c) counterfactual: what employment rate would prevail without any income floor (true labor supply) vs with generous unconditional floor (behavioral response to income floor alone)',
    'If incentives are effective: constraint solves coordination problem (income security + work participation). If performative: work requirement is extraction mechanism (disciplining behavior via financial penalty for non-work, not genuine incentive). Theater_ratio may be underestimated if incentive structure is theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(work_incentive_magnitude_calibration, empirical, 'Whether work incentives are effective or performative').

omega_variable(
    intergenerational_extraction_from_children,
    'Does the constraint extract from children in the form of time poverty (parental work requirements reducing parental availability) even while increasing material resources?',
    'Longitudinal comparison: (a) child development outcomes (cognitive, socio-emotional, attachment) in welfare-recipient families before vs after work requirements; (b) parental time availability and childcare quality as mediators; (c) comparison to control group receiving income floor without work requirements (if such comparison is available)',
    'If time poverty outweighs material benefit: constraint extracts from children''s development, making them indirect victims despite material gain. Single victims-declaration (children as beneficiaries) becomes inadequate — children are simultaneously beneficiaries (income) and victims (time availability, parental stress). This reveals hidden extraction layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_extraction_from_children, empirical, 'Whether work requirements extract time from children').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1971_nixon_welfare_floor_and_work_requirement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_1971_theater_t0, sotu_1971_nixon_welfare_floor_and_work_requirement, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sotu_1971_theater_t2, sotu_1971_nixon_welfare_floor_and_work_requirement, theater_ratio, 2, 0.5).
narrative_ontology:measurement(sotu_1971_theater_t5, sotu_1971_nixon_welfare_floor_and_work_requirement, theater_ratio, 5, 0.58).
narrative_ontology:measurement(sotu_1971_theater_t10, sotu_1971_nixon_welfare_floor_and_work_requirement, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(sotu_1971_extract_t0, sotu_1971_nixon_welfare_floor_and_work_requirement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu_1971_extract_t2, sotu_1971_nixon_welfare_floor_and_work_requirement, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(sotu_1971_extract_t5, sotu_1971_nixon_welfare_floor_and_work_requirement, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(sotu_1971_extract_t10, sotu_1971_nixon_welfare_floor_and_work_requirement, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1971_nixon_welfare_floor_and_work_requirement, resource_allocation).
narrative_ontology:affects_constraint(sotu_1971_nixon_welfare_floor_and_work_requirement, welfare_stigma_means_testing_regime).
narrative_ontology:affects_constraint(sotu_1971_nixon_welfare_floor_and_work_requirement, childcare_affordability_barrier).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the welfare stigma problem (means-testing verification as degrading) and upstream of childcare affordability constraints. The income floor partially solves the stigma problem while creating work-requirement barriers that intersect with childcare availability. Decomposition: (1) welfare_floor_income_coordination (ε ≈ 0.15, Rope) — pure coordination benefit of guaranteed minimum income; (2) work_requirement_extraction (ε ≈ 0.62, Snare) — pure extraction mechanism of mandatory work participation; (3) this story (sotu_1971) combines both at ε = 0.52 (Tangled Rope). The two substories have different ε values because the observable shifts from 'does the income floor improve family stability?' to 'does the work requirement increase labor compliance?' Two distinct constraints linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1971_nixon_welfare_floor_and_work_requirement, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
