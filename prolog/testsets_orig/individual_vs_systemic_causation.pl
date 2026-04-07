% ============================================================================
% CONSTRAINT STORY: individual_vs_systemic_causation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_individual_vs_systemic_causation, []).

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
 *   constraint_id: individual_vs_systemic_causation
 *   human_readable: Individual vs. Systemic Causation Attribution in Public Health Policy
 *   domain: public_health/policy
 *
 * SUMMARY:
 *   The attribution of health outcomes to individual vs. systemic causation
 *   represents a structural constraint on public health discourse and policy.
 *   This constraint creates a perspectival chasm: low-income communities
 *   experience it as extraction (blamed for health outcomes driven by
 *   structural factors), while corporate and ideological interests experience
 *   it as coordination (individual responsibility narrative enables
 *   market-based solutions and limits regulatory intervention). Public health
 *   practitioners experience it as genuine coordination problem requiring
 *   both individual behavior change and systemic reform, but the
 *   institutional measurement systems, funding incentives, and career paths
 *   systematically privilege individual-level research. The constraint has
 *   intensified over the past 50 years as epidemiology has generated
 *   increasingly sophisticated data on social determinants of health while
 *   policy discourse remains anchored to individual causation framing.
 *   Theater ratio has increased because major institutions (CDC, WHO, medical
 *   schools) now formally acknowledge systemic factors in training and
 *   frameworks, yet practice and funding continue to emphasize individual
 *   interventions. The extraction mechanism operates through causal
 *   misattribution: by framing health disparities as consequences of
 *   individual choices rather than structural barriers, the constraint
 *   directs resources away from systemic reform toward individual behavior
 *   change programs that cannot address root causes.
 *
 * KEY AGENTS:
 *   - Low-income and structurally disadvantaged communities: Primary victims (powerless/trapped) — bear health burden while being blamed for personal failure; no exit from either constraint aspect
 *   - Food, pharmaceutical, and healthcare industries: Primary beneficiaries (institutional/arbitrage) — benefit from individual responsibility narrative that enables market-based health initiatives and prevents regulatory constraint
 *   - Public health officials and researchers: Secondary actor (moderate/constrained) — face genuine coordination problem but also career incentives that favor individual-level research and interventions
 *   - Conservative political organizations: Secondary beneficiaries (institutional/arbitrage) — benefit from individual causation framing as ideological cover for limited-government health policy
 *   - Health equity and social justice organizations: Secondary victims (organized/constrained) — advocating for structural change but constrained by political feasibility and resource barriers
 *   - Traditional medical and public health institutions: Institutional actor (powerful/constrained) — maintain performative individual-causation frameworks through measurement systems and training despite acknowledging systemic factors
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing political choice to emphasize individual causation as irreducible causal complexity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(individual_vs_systemic_causation, 0.52).
domain_priors:suppression_score(individual_vs_systemic_causation, 0.65).
domain_priors:theater_ratio(individual_vs_systemic_causation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(individual_vs_systemic_causation, extractiveness, 0.52).
narrative_ontology:constraint_metric(individual_vs_systemic_causation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(individual_vs_systemic_causation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(individual_vs_systemic_causation, tangled_rope).
narrative_ontology:human_readable(individual_vs_systemic_causation, "Individual vs. Systemic Causation Attribution in Public Health Policy").
narrative_ontology:topic_domain(individual_vs_systemic_causation, "public_health/policy").

domain_priors:requires_active_enforcement(individual_vs_systemic_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(individual_vs_systemic_causation, corporate_health_interests).
narrative_ontology:constraint_beneficiary(individual_vs_systemic_causation, individual_responsibility_advocates).
narrative_ontology:constraint_victim(individual_vs_systemic_causation, structural_health_equity).
narrative_ontology:constraint_victim(individual_vs_systemic_causation, population_health_interventions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRUCTURAL VICTIMS (SNARE) — Communities with limited access to healthy food, safe housing, quality healthcare, and employment bear full health burden while being blamed for individual choices. Exit options are constrained by geography, economics, and social mobility barriers. The constraint extracts through misdirection: systemic pathology is reframed as personal failure, preventing resource allocation toward structural change. Maximum experienced extraction — no escape from either the health harm or the causal misattribution.
constraint_indexing:constraint_classification(individual_vs_systemic_causation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH PRACTITIONERS (TANGLED ROPE) — Face genuine coordination problem: individual behavior change and systemic policy reform are both necessary, but limited funding and political feasibility require triage. Also benefit from the current framing through career incentives (behavioral interventions are fundable and measurable; structural change is harder to credit to individual researchers). Experience constraint as mixed: real coordination function (behavior + policy) but also extraction (careerism incentivizes overselling behavior change).
constraint_indexing:constraint_classification(individual_vs_systemic_causation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CORPORATE HEALTH INTERESTS (ROPE) — Benefit from individual causation framing because it directs policy attention toward consumer education, medication adherence, and personal wellness programs rather than regulation, taxation, or structural constraint on product marketing. Experience the constraint as coordination: individual responsibility language enables public-private partnerships, corporate wellness initiatives, and market-based health interventions. Net beneficiary through arbitrage — can participate in 'solutions' while avoiding systemic regulation.
constraint_indexing:constraint_classification(individual_vs_systemic_causation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: IDEOLOGICAL BENEFICIARIES (ROPE) — Benefit from individual causation framing because it aligns with limited-government, personal-responsibility political ideology. Experience the constraint as pure coordination: individual causation language enables market-based health policy, reduces demands for state intervention, and provides intellectual framework for resisting structural regulation. Arbitrage option allows selective engagement with public health discourse while advancing ideological objectives.
constraint_indexing:constraint_classification(individual_vs_systemic_causation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ESTABLISHED MEDICAL INSTITUTIONS (PITON) — Individual causation framing persists through institutional inertia: medical training emphasizes individual patient management, epidemiological methods privilege measurable individual risk factors, and career advancement rewards individual-level research. The institutions see their own framing as increasingly outdated (social determinants of health is now mainstream teaching) but the performative individual-causation framework persists in practice due to measurement systems, funding structures, and educational paths. Theater ratio high because the institutions acknowledge systemic factors while continuing to organize around individual-level interventions.
constraint_indexing:constraint_classification(individual_vs_systemic_causation, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EQUITY ADVOCATES (TANGLED ROPE) — Organized agents (community health centers, civil rights groups, health equity nonprofits) see the constraint as both coordination mechanism (individual behavior change and systemic reform are complementary) and extraction mechanism (individual framing subordinates structural change to individual responsibility). Constrained exit because they must participate in health policy discourse but face structural barriers to shifting the causal narrative. Experience moderate extraction through constant reframing battles where individual causation language dominates despite structural data.
constraint_indexing:constraint_classification(individual_vs_systemic_causation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CAUSAL COMPLEXITY (MOUNTAIN) — From civilizational/universal perspective, all health outcomes result from both individual and systemic factors: genetics, behavior, and environment all contribute causally. No pure individual causation possible; no pure systemic causation possible. The constraint between them appears as a natural epistemic law — the irreducible problem of causal attribution in complex systems. However, this perspective risks false summit: the constraint is not about epistemic complexity but about resource allocation power. The analytical view may naturalize what is actually a political choice to emphasize individual over systemic factors.
constraint_indexing:constraint_classification(individual_vs_systemic_causation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(individual_vs_systemic_causation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(individual_vs_systemic_causation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(individual_vs_systemic_causation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(individual_vs_systemic_causation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(individual_vs_systemic_causation, TR),
    TR >= 0.70.

:- end_tests(individual_vs_systemic_causation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The constraint extracts through causal misattribution. Individual causation framing prevents resource allocation toward structural interventions by creating false explanations for health disparities. The extraction is not total (social determinants research has increased) but systematic and persistent. The 26-year trajectory shows increasing extractiveness as corporate health interests have amplified individual-responsibility messaging despite growing scientific evidence for systemic factors. Suppression (0.65): High. Multiple mechanisms suppress structural causation: institutional inertia in medical education, measurement convenience bias (individual factors easier to quantify than systemic factors), funding structures that reward individual-level interventions, and political barriers to systemic regulation. Suppression takes the form of narrative dominance rather than censorship — systemic factors are discussed but subordinated to individual responsibility framing. Theater ratio (0.68): Moderate-high. Public health institutions now formally include social determinants of health in frameworks and teaching, but resource allocation and intervention design remain overwhelmingly focused on individual behavior change. The performative acknowledgment of systemic factors without corresponding structural policy action indicates theater. The increase over the interval reflects growing gap between acknowledged systemic understanding and actual policy implementation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximal perspectival divergence. Structural victims (powerless/trapped) experience pure extraction (Snare): health disparities are attributed to personal choices, preventing systemic intervention. Beneficiaries (institutional/arbitrage) experience pure coordination (Rope): individual responsibility language enables market solutions and private sector health initiatives. Public health practitioners experience genuine coordination tension (Tangled Rope): both individual and systemic factors matter, but organizational structures privilege individual approaches. Equity advocates (organized/constrained) experience mixed extraction and coordination (Tangled Rope): systemic change is necessary but constantly reframed as secondary to individual responsibility. Traditional institutions (powerful/constrained) experience their own framework as degraded (Piton): they acknowledge systemic factors academically while practicing individual-level medicine. The analytical observer risks false mountain: causal complexity (both individual and systemic factors contribute) is presented as natural law, naturalizing what is actually a political choice to weight individual causation heavily.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position relative to the extraction flow. Structural victims with no exit options (d≈0.95) experience maximum chi through suppression of systemic intervention and causal misattribution. Corporate beneficiaries with arbitrage options (d≈0.10) experience negative chi through the individual-responsibility narrative enabling market participation without regulation. Public health practitioners with constrained exit (d≈0.55) experience moderate chi from mixed incentives: career benefits from individual-level research offset by professional understanding that systemic change is necessary. Equity advocates with organized power but constrained structural position (d≈0.60) experience moderate-high chi through constant reframing battles. Traditional institutions with powerful position but inertial constraints (d≈0.50) experience symmetric costs and benefits: they benefit from institutional legitimacy while paying costs of cognitive dissonance between acknowledged systemic understanding and individual-practice organization. Analytical observer (d≈0.72) experiences high chi through risk of naturalizing political choice as epistemic law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through perspectival decomposition. The mandatrophy question is: 'Is the individual vs. systemic causation distinction a coordination problem (legitimate tension between two necessary approaches) or an extraction mechanism (systematic suppression of structural change)?' The answer is: both, from different perspectives. For structural victims and equity advocates, it is extraction. For corporate interests, it is beneficial coordination. For public health practitioners, it is genuine coordination with extraction side effects. No single type is 'correct' — the presheaf over the observation site reveals the distribution of truth. The constraint's classification as Tangled Rope (rather than pure Snare or pure Rope) at the analytical level reflects that it possesses both genuine coordination function (individual behavior change and systemic reform are complementary) AND asymmetric extraction (systemic reform is systematically subordinated to individual responsibility). The increasing theater ratio indicates Goodhart drift: the formal acknowledgment of systemic factors (meeting the coordination requirement) masks continued individual-level resource allocation (maintaining extraction). Resolution would require either (a) true integration (simultaneous funding and authority for individual and systemic interventions) or (b) structural dominance (systemic factors treated as primary driver with individual factors treated as secondary mechanism).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_responsibility_threshold,
    'What level of individual control over a health outcome constitutes moral or policy-relevant responsibility?',
    'Comparative analysis of health outcomes with and without individual behavior change, controlling for structural factors; philosophical/ethical framework for apportioning responsibility',
    'If threshold is low (individual has any measurable control): individual causation dominates policy. If threshold is high (individual must have near-total control): systemic factors dominate. Current ambiguity enables extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_responsibility_threshold, preference, 'Threshold for assigning responsibility based on individual control').

omega_variable(
    measurement_bias_individual_vs_systemic,
    'Do existing epidemiological methods systematically overweight individual risk factors relative to systemic factors due to measurement convenience?',
    'Meta-analysis of epidemiological studies; comparison of explained variance when systemic factors (food environment, housing quality, employment, discrimination) are measured with same rigor as individual factors (BMI, adherence, exercise)',
    'If yes: current causal estimates are biased toward individual causation; constraint perpetuates false narrative. If no: apparent individual causation reflects true causal weights and is not extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_bias_individual_vs_systemic, empirical, 'Whether measurement bias overweights individual vs systemic causation').

omega_variable(
    policy_substitution_evidence,
    'Does individual causation framing actually crowd out systemic policy interventions, or are they pursued in parallel?',
    'Historical analysis of health policy funding and regulatory intensity: correlation between dominant causation narrative (individual vs systemic emphasis) and actual spending on structural interventions (food regulation, housing policy, workplace health) vs individual interventions (education, incentives)',
    'If substitution is real: constraint is extractive (prevents structural change). If policies pursued in parallel: constraint is neutral coordination problem. If systemic spending increases despite individual narrative: constraint has low actual power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_substitution_evidence, empirical, 'Whether individual causation framing crowds out systemic policy').

omega_variable(
    corporate_influence_causal_narrative,
    'How much of the persistence of individual causation framing reflects genuine scientific belief vs. strategic amplification by interests benefiting from individual-responsibility policy?',
    'Analysis of funding sources for individual vs systemic causation research; content analysis of industry-funded health communications; comparison of causal narratives in regions with varying corporate influence',
    'If corporate influence dominates: constraint is primarily extractive (Snare from population perspective). If genuine scientific disagreement: constraint is coordination problem (Tangled Rope). Degree of influence determines classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_influence_causal_narrative, empirical, 'Degree of corporate influence on causal narrative persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(individual_vs_systemic_causation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ivsc_tr_t0, individual_vs_systemic_causation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ivsc_tr_t25, individual_vs_systemic_causation, theater_ratio, 25, 0.55).
narrative_ontology:measurement(ivsc_tr_t50, individual_vs_systemic_causation, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(ivsc_be_t0, individual_vs_systemic_causation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ivsc_be_t25, individual_vs_systemic_causation, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(ivsc_be_t50, individual_vs_systemic_causation, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(individual_vs_systemic_causation, information_standard).
narrative_ontology:affects_constraint(individual_vs_systemic_causation, health_disparities_measurement).
narrative_ontology:affects_constraint(individual_vs_systemic_causation, behavioral_epidemiology_funding).
narrative_ontology:affects_constraint(individual_vs_systemic_causation, structural_determinants_data_availability).

% DUAL FORMULATION NOTE:
% Individual vs. systemic causation is downstream of specific epidemiological findings about social determinants of health. The upstream constraint concerns measurement bias and causal inference methods; the downstream constraints concern policy implementation and resource allocation. Each has distinct extractiveness reflecting the structural position of different actors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(individual_vs_systemic_causation, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
