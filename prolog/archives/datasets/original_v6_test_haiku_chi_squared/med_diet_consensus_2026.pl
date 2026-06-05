% ============================================================================
% CONSTRAINT STORY: med_diet_consensus_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_med_diet_consensus_2026, []).

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
 *   constraint_id: med_diet_consensus_2026
 *   human_readable: Mediterranean Diet Scientific Hegemony
 *   domain: health/scientific/economic
 *
 * SUMMARY:
 *   The Mediterranean diet scientific consensus represents a constraint
 *   operating across multiple institutional layers: nutritional epidemiology,
 *   agricultural economics, public health governance, and health-tech
 *   marketing. Since the 1950s Seven Countries Study, Mediterranean diet has
 *   consolidated into the globally dominant framework for healthy eating,
 *   endorsed by WHO, major health organizations, and mainstream nutrition
 *   science. The constraint exhibits the full spectrum of DR types depending
 *   on structural position: Mediterranean agricultural exporters and
 *   nutrition research establishments see coordination (Rope); low-income
 *   populations outside the Mediterranean see pure extraction (Snare); health
 *   equity advocates see mixed coordination-extraction (Tangled Rope);
 *   alternative diet researchers see suppression (Snare); health-tech
 *   industries use the consensus as scaffolding for performative
 *   personalization (Piton). The consensus operates through genuine empirical
 *   evidence (Mediterranean diet does correlate with health outcomes)
 *   combined with structural extraction mechanisms (publication bias, journal
 *   gatekeeping, economic accessibility asymmetry, suppression of competing
 *   hypotheses). Theater_ratio (0.64) reflects that while core claims about
 *   diet quality are functional, much surrounding infrastructure is
 *   performative: personalized Mediterranean diet apps, genetic testing for
 *   diet response, supplements claiming to replicate Mediterranean benefits
 *   without dietary change.
 *
 * KEY AGENTS:
 *   - Low-Income Populations Outside Mediterranean: Primary victims (powerless/trapped) — bear full cost of nutrition guidance optimized for economically inaccessible foods; no exit option
 *   - Alternative Diet Research Communities: Primary victims (powerless/trapped) — suppressed by publication bias, grant scarcity, journal gatekeeping; cannot exit mainstream research funding without career cost
 *   - Mediterranean Agricultural Exporters: Primary beneficiaries (institutional/arbitrage) — capture premium pricing and expanded markets through scientific endorsement
 *   - Nutrition Research Establishment: Primary beneficiaries (institutional/arbitrage) — generate continuous research agenda, secure funding, reproducible publication pathway from diet consensus
 *   - Regional Agricultural Producers (Non-Mediterranean): Secondary victims (moderate/constrained) — face market disadvantage when Mediterranean preference reduces demand for local staples
 *   - Health Equity Advocates: Organized actors (organized/mobile) — recognize coordination function (unified standards) but trapped by economic reality (regressive resource requirements)
 *   - Health-Tech and Supplement Industries: Institutional actors (institutional/arbitrage) — leverage consensus to generate demand for measurement and personalization services; mostly performative
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable nutrition science
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(med_diet_consensus_2026, 0.52).
domain_priors:suppression_score(med_diet_consensus_2026, 0.58).
domain_priors:theater_ratio(med_diet_consensus_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(med_diet_consensus_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(med_diet_consensus_2026, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(med_diet_consensus_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(med_diet_consensus_2026, tangled_rope).
narrative_ontology:human_readable(med_diet_consensus_2026, "Mediterranean Diet Scientific Hegemony").
narrative_ontology:topic_domain(med_diet_consensus_2026, "health/scientific/economic").

domain_priors:requires_active_enforcement(med_diet_consensus_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(med_diet_consensus_2026, mediterranean_region_agricultural_exporters).
narrative_ontology:constraint_beneficiary(med_diet_consensus_2026, nutrition_research_establishment).
narrative_ontology:constraint_beneficiary(med_diet_consensus_2026, health_tech_and_supplement_industries).
narrative_ontology:constraint_victim(med_diet_consensus_2026, alternative_diet_research_communities).
narrative_ontology:constraint_victim(med_diet_consensus_2026, low_income_populations_outside_mediterranean).
narrative_ontology:constraint_victim(med_diet_consensus_2026, competing_agricultural_interests).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME POPULATIONS (SNARE) — Trapped by nutritional guidance optimized for Mediterranean-accessible foods (olive oil, fresh fish, diverse produce) that are economically inaccessible in food deserts and developing regions. Bears full cost of diet-health gap without agency to exit. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.81.
constraint_indexing:constraint_classification(med_diet_consensus_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE DIET RESEARCHERS (SNARE) — Trapped by publication bias, grant scarcity, and journal gatekeeping that suppress non-Mediterranean diet hypotheses (low-carb, plant-based, culture-specific). Cannot exit mainstream research funding without career cost. d≈0.88, f(d)≈1.32, σ=1.1 → χ≈0.76.
constraint_indexing:constraint_classification(med_diet_consensus_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPETING AGRICULTURAL PRODUCERS (TANGLED ROPE) — Constrained by market disadvantage when Mediterranean diet hegemony reduces demand for local staples (grains, legumes, regional produce types). Some coordination benefit from nutrition science legitimizing agriculture generally, but asymmetric extraction through preference hierarchies. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(med_diet_consensus_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDITERRANEAN EXPORTERS (ROPE) — Institutional beneficiaries with arbitrage exits. Capture premium pricing and market share through scientific endorsement of Mediterranean diet. Experience constraint as coordination: diet-disease research establishes market demand, enables scale. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(med_diet_consensus_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NUTRITION RESEARCH ESTABLISHMENT (ROPE) — Institutional beneficiary with arbitrage capacity. Mediterranean diet consensus generates continuous research agenda (mechanisms, variants, sub-populations), secure funding, reproducible publication pathway, media visibility. Experiences constraint as coordination: established guidelines enable research. d≈0.12, f(d)≈-0.05, σ=1.1 → χ≈-0.03.
constraint_indexing:constraint_classification(med_diet_consensus_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HEALTH EQUITY ADVOCATES (TANGLED ROPE) — Organized agents (public health bodies, WHO, nutrition equity organizations) with some mobility. Recognize Mediterranean diet as coordinating principle for general nutrition science (legitimate universalism), but trapped by economic reality: guidance based on economically inaccessible ingredients extracts from poorest populations. See both coordination (unified standards) and extraction (regressive resource requirements). d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.47.
constraint_indexing:constraint_classification(med_diet_consensus_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: HEALTH-TECH INDUSTRIES (PITON) — Institutional actor with arbitrage capacity. Mediterranean diet consensus generates demand for measurement and optimization: fitness trackers, genetic testing for diet response, supplements claiming to replicate Mediterranean benefits. Theater_ratio=0.64: Much of the 'personalized Mediterranean diet' market is performative—measurement and tailoring provide psychological reassurance rather than meaningful health gain beyond base diet quality. Maintains hedgehog hypothesis (test if you're 'Mediterranean-type') despite low functional differentiation. d≈0.18, f(d)≈0.08, σ=1.2 → χ≈0.06.
constraint_indexing:constraint_classification(med_diet_consensus_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational view, Mediterranean diet may be a natural optimization reflecting geography, climate, and disease ecology: populations living on Mediterranean coasts evolved and selected foodways that match local ecology and human physiology. The diet may emerge naturally as good from constraint of environment. However, structural data (ε=0.52, suppression=0.58, theater=0.64) contradicts mountain classification. This is a false summit: the consensus is contingent on scientific institutions, publication systems, economic structures—not immutable.
constraint_indexing:constraint_classification(med_diet_consensus_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(med_diet_consensus_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(med_diet_consensus_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(med_diet_consensus_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(med_diet_consensus_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(med_diet_consensus_2026, TR),
    TR >= 0.70.

:- end_tests(med_diet_consensus_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Mediterranean diet consensus has legitimate scientific basis (30+ years of epidemiological evidence, mechanistic plausibility, reproducible outcomes in diverse populations). However, extraction occurs through: (1) economic accessibility asymmetry—guidance assumes affordable access to olive oil, fresh fish, diverse produce that are expensive or unavailable in food deserts and low-income regions globally; (2) suppression of alternative diet research (low-carb, plant-based, culture-specific whole-food patterns) through publication bias and grant scarcity; (3) market concentration that benefits Mediterranean exporters disproportionately. The value has increased from 0.28 (when consensus was emerging) to 0.52 (current state) as institutional lock-in has strengthened. Suppression (0.58): Moderate-high. Significant barriers to alternative diet research include: journal preference for Mediterranean diet studies in high-impact outlets, difficulty obtaining grants for non-Mediterranean diet RCTs, career risk for researchers challenging consensus, publication bias against null or negative results for Mediterranean diet. But suppression is not total—alternative research exists and publishes; the constraint is institutional bias, not complete blockade. Theater ratio (0.64): Moderate-high. The core Mediterranean diet science is functional—diet quality, plant content, healthy fat composition genuinely predict health outcomes. But surrounding infrastructure is substantially performative: personalized Mediterranean diet apps provide psychological reassurance rather than mechanistic differentiation; genetic testing for diet response lacks validated algorithms; supplements claiming to replicate Mediterranean benefits without dietary change are largely marketing. Theater has increased from 0.38 (when science was primary focus) to 0.64 (as commercialization and personalization have expanded).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across five distinct agent classes. Mediterranean exporters and nutrition researchers see coordination (Rope): legitimate advancement of knowledge and sustainable livelihoods. Low-income populations and alternative researchers see extraction (Snare): constraints imposed without agency or choice. Health equity advocates see the paradox (Tangled Rope): genuine coordination function (unified standards enable research, align interventions) coupled with regressive extraction (guidance assumes economic access most poor populations lack). Health-tech industries see performative optimization (Piton): the constraint enables their services without delivering proportionate value. The analytical observer risks false summit (Mountain): naturalizing the consensus as immutable nutrition science rather than contingent institutional arrangement. The perspectival gap widens over time as commercialization and lock-in increase.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income populations: Victims + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Cannot exit guidance, cannot access prescribed foods. Alternative researchers: Victims + trapped → d≈0.88, f(d)≈1.32. High extraction through institutional suppression. Mediterranean exporters: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries. Can capture value and exit if markets shift. Nutrition researchers: Beneficiaries + arbitrage → d≈0.12, f(d)≈-0.05. Net beneficiaries. Secure research agenda and funding. Health equity advocates: Mixed (both beneficiary and victim) + mobile → d≈0.55, f(d)≈0.75. Recognize both coordination and extraction; have some agency to modify constraint. Health-tech: Beneficiary + arbitrage → d≈0.18, f(d)≈0.08. Low-moderate extraction through performative services.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the Mediterranean diet consensus solves a genuine coordination problem (providing unified nutrition guidance, enabling research infrastructure, standardizing health messaging) while simultaneously extracting through accessibility asymmetry and suppression of alternatives. This is not a false tangled rope—the coordination function is real and valuable (low-income populations benefit from any evidence-based nutrition guidance, alternative researchers benefit from mainstream science legitimacy even if suppressed). The extraction is also real and measurable: guidance optimized for Mediterranean-accessible foods imposes regressive costs on poor populations; publication bias and grant scarcity suppress potentially valuable research. The constraint remains tangled_rope because both functions persist structurally. It is not coordination pretending to be extraction (snare masquerading as rope) nor extraction pretending to be coordination (rope masquerading as snare)—it genuinely does both. The rising theater_ratio (0.38→0.64) reflects that commercialization has added performative elements (personalized apps, genetic testing, supplements) that amplify extraction without strengthening coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mediterranean_diet_cardiovascular_exclusivity,
    'Is cardiovascular protection exclusive to Mediterranean diet pattern, or does it emerge from any diet emphasizing whole foods, healthy fats, and plant content?',
    'Meta-analysis controlling for food quality (minimally processed) and macro composition across diet types (low-carb whole-food, plant-based whole-food, traditional non-Mediterranean cultures); randomized trials comparing Mediterranean to other high-quality whole-food diets',
    'If exclusive: Mediterranean hegemony is justified by unique mechanisms, constraint justified. If general property: constraint is arbitrary hegemony—reframes as pure snare, not tangled rope. Opens space for alternative diet research.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mediterranean_diet_cardiovascular_exclusivity, empirical, 'Whether Mediterranean diet uniquely protects cardiovascular health or if benefit generalizes to all whole-food diets').

omega_variable(
    mediterranean_accessibility_economic_substitute,
    'Can Mediterranean diet benefits be achieved by low-income populations using locally affordable, minimally processed foods that differ in composition but match in cost and accessibility?',
    'Longitudinal studies comparing health outcomes in low-income populations using locally sourced whole foods (not Mediterranean pattern) vs purchasing Mediterranean ingredients on limited budgets vs receiving no diet guidance',
    'If yes: constraint is unjust—imposes culturally specific diet on populations for whom it''s unaffordable. Shifts classification toward pure snare (victims) and away from rope (coordination). If no: Mediterranean pattern is genuinely optimal and requires international food access support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mediterranean_accessibility_economic_substitute, empirical, 'Whether health benefits of Mediterranean diet can be achieved with affordable local alternatives').

omega_variable(
    publication_bias_mediterranean_orientation,
    'How much of Mediterranean diet''s apparent superiority reflects genuine mechanistic advantage vs publication bias favoring Mediterranean diet studies in high-impact journals?',
    'Comparison of effect sizes from Mediterranean diet RCTs published in high-impact journals vs low-impact journals; funnel plot analysis; pre-registered replication of high-profile Mediterranean diet studies; analysis of submission and acceptance rates for Mediterranean vs alternative diet studies in leading nutrition journals',
    'If substantial bias: constraint is amplified by scientific institutions. Reframes as institutional snare on alternative researchers, not discovery. If minimal: consensus reflects true evidence; constraint is coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publication_bias_mediterranean_orientation, empirical, 'Degree to which publication bias amplifies Mediterranean diet hegemony').

omega_variable(
    mediterranean_diet_mechanism_universality,
    'Do the proposed mechanisms (polyphenols, monounsaturated fat, fiber, plant diversity) apply universally, or are they mediated by genetic variation, microbiome composition, or cultural food combinations specific to Mediterranean populations?',
    'Genomic studies of Mediterranean vs non-Mediterranean populations; microbiome analysis across populations; mechanistic studies isolating individual Mediterranean diet components in genetically diverse cohorts; cross-cultural diet studies controlling for confounders',
    'If universal: supports generalization; constraint justified. If population-specific: mechanism is contingent; reframes as cultural hegemony rather than discovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mediterranean_diet_mechanism_universality, empirical, 'Whether Mediterranean diet mechanisms apply universally or are population-specific').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(med_diet_consensus_2026, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meddieta_tr_t0, med_diet_consensus_2026, theater_ratio, 0, 0.38).
narrative_ontology:measurement(meddieta_tr_t15, med_diet_consensus_2026, theater_ratio, 15, 0.52).
narrative_ontology:measurement(meddieta_tr_t30, med_diet_consensus_2026, theater_ratio, 30, 0.64).

% Extraction over time
narrative_ontology:measurement(meddieta_be_t0, med_diet_consensus_2026, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(meddieta_be_t15, med_diet_consensus_2026, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(meddieta_be_t30, med_diet_consensus_2026, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(med_diet_consensus_2026, information_standard).
narrative_ontology:affects_constraint(med_diet_consensus_2026, agricultural_subsidy_regimes).
narrative_ontology:affects_constraint(med_diet_consensus_2026, nutrition_research_funding_bias).
narrative_ontology:affects_constraint(med_diet_consensus_2026, food_accessibility_inequality).
narrative_ontology:affects_constraint(med_diet_consensus_2026, health_tech_personalization_theater).

% DUAL FORMULATION NOTE:
% The Mediterranean diet consensus decomposes into at least three structurally distinct constraints: (1) nutritional_efficacy (ε≈0.15, Mountain-class—Mediterranean diet pattern genuinely predicts health outcomes across populations; scientific foundation); (2) accessibility_regressive_design (ε≈0.68, Snare-class—guidance assumes expensive-to-access foods; economic extraction from poor populations); (3) research_suppression_bias (ε≈0.55, Snare-class—publication bias and grant scarcity suppress alternative diet research). The three stories are linked: nutritional_efficacy is the upstream claim that legitimate scientific consensus; the other two represent institutional and economic pathologies in how that consensus is deployed and enforced. Each has distinct ε, distinct primary victims, distinct potential remediation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(med_diet_consensus_2026, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
