% ============================================================================
% CONSTRAINT STORY: nutritional_reductionism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nutritional_reductionism, []).

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
 *   constraint_id: nutritional_reductionism
 *   human_readable: Nutritional Reductionism: The Constraint on Holistic Food Science
 *   domain: nutritional_science/epistemology
 *
 * SUMMARY:
 *   Nutritional reductionism is the epistemic constraint that isolates and
 *   privileges the study of individual nutrients, biomarkers, and mechanistic
 *   pathways while systematically devaluing holistic dietary pattern
 *   research, food synergies, and traditional food knowledge systems. This
 *   constraint operates across funding mechanisms, journal gatekeeping,
 *   academic career incentives, and industrial product development. It
 *   transforms foods (holistic biological systems) into nutrients
 *   (isolatable, patentable compounds), enabling supplement manufacturers and
 *   functional food companies to extract economic value while simultaneously
 *   suppressing research traditions that cannot be reduced to isolated
 *   mechanisms. The constraint exhibits clear tangled_rope structure: genuine
 *   coordination function (enabling mechanistic understanding of
 *   bioavailability and metabolic processes) combined with asymmetric
 *   extraction favoring proprietary interventions over whole-food approaches.
 *   The theater_ratio has risen from 0.48 to 0.68 over the measurement
 *   interval, indicating that reductionist nutrition science is performing
 *   its validity increasingly through isolated biomarker studies while losing
 *   predictive power for actual health outcomes. Simultaneously,
 *   systems-level nutrition approaches are building alternative pathways
 *   (personalized nutrition, nutrigenomics, microbiome science) that suggest
 *   a generational sunset on pure reductionism, though the transition period
 *   remains high-suppression.
 *
 * KEY AGENTS:
 *   - Whole Food Research Communities: Primary victim (powerless/trapped) — excluded from high-status publication venues and funding mechanisms; face systematic career penalties for studying dietary patterns rather than isolated nutrients
 *   - Indigenous and Traditional Food Knowledge Systems: Primary victim (powerless/trapped) — epistemic colonization; traditional food combinations dismissed as non-scientific unless reducible to biomarker mechanisms
 *   - Supplement and Functional Food Industry: Primary beneficiary (institutional/arbitrage) — reductionism enables extraction of whole foods into isolatable bioactive compounds and intellectual property; constraint transforms common foods into proprietary products
 *   - Pharmaceutical Nutrition Companies: Secondary beneficiary (institutional/arbitrage) — benefit from reduction of dietary problems to treatable nutrient deficiencies; market for supplementation
 *   - Clinical Nutrition Researchers: Mixed position (moderate/constrained) — need reductionist tools for bioavailability and dosing but face pressure to reduce complex problems to single-nutrient interventions
 *   - Epidemiological Research Communities: Mixed position (organized/constrained) — generate holistic dietary pattern data but must translate findings into reductionist language for credibility
 *   - Systems Nutrition and Nutrigenomics Movement: Organized counter-actor (organized/constrained) — building alternative paradigms that integrate components into systems; represent sunset clause on pure reductionism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent funding incentives and industry influence as inherent constraints on scientific methodology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nutritional_reductionism, 0.58).
domain_priors:suppression_score(nutritional_reductionism, 0.62).
domain_priors:theater_ratio(nutritional_reductionism, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nutritional_reductionism, extractiveness, 0.58).
narrative_ontology:constraint_metric(nutritional_reductionism, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(nutritional_reductionism, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nutritional_reductionism, tangled_rope).
narrative_ontology:human_readable(nutritional_reductionism, "Nutritional Reductionism: The Constraint on Holistic Food Science").
narrative_ontology:topic_domain(nutritional_reductionism, "nutritional_science/epistemology").

domain_priors:requires_active_enforcement(nutritional_reductionism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nutritional_reductionism, supplement_manufacturers).
narrative_ontology:constraint_beneficiary(nutritional_reductionism, pharmaceutical_nutrition_companies).
narrative_ontology:constraint_beneficiary(nutritional_reductionism, biomarker_research_industry).
narrative_ontology:constraint_victim(nutritional_reductionism, whole_food_research_communities).
narrative_ontology:constraint_victim(nutritional_reductionism, dietary_pattern_epidemiology).
narrative_ontology:constraint_victim(nutritional_reductionism, indigenous_food_knowledge_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WHOLE FOOD RESEARCH (SNARE) — Cannot exit the reductionist paradigm; funding mechanisms, peer review gatekeeping, and journal hierarchy all reward isolated nutrient studies over dietary pattern investigation. Researchers studying Mediterranean diet patterns or traditional food systems face publication bias, underfunding, and career penalties. Trapped by institutional structure despite legitimate scientific value. Maximum extraction from this position.
constraint_indexing:constraint_classification(nutritional_reductionism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIGENOUS FOOD KNOWLEDGE (SNARE) — Trapped by epistemic colonization: reductionist frameworks dismiss traditional food combinations and preparation methods as non-scientific unless they can be reduced to biomarker mechanisms. Centuries of documented health outcomes from traditional diets are classified as 'anecdotal' or 'confounded' because they operate on wholes, not isolated compounds. No structural exit from this subordination; knowledge is extracted and remade through reductionist translation or discarded as invalid.
constraint_indexing:constraint_classification(nutritional_reductionism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SUPPLEMENT MANUFACTURERS (ROPE) — Experience reductionism as pure coordination: the constraint translates whole foods into isolatable bioactive compounds, enabling extraction and patentability. β-carotene isolated from carrots becomes a standalone product; curcumin extracted from turmeric becomes intellectual property. The industry benefits from the reductionist framework because it converts common foods into proprietary interventions. Effective extraction runs toward this agent; they perceive the constraint as solving a legitimate problem (how to deliver bioactives at scale).
constraint_indexing:constraint_classification(nutritional_reductionism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLINICAL NUTRITION (TANGLED ROPE) — Constrained by mixed incentives. Genuinely need reductionist tools for understanding bioavailability, metabolic mechanisms, and therapeutic dosing — the constraint provides real coordination value for clinical applications. But also face pressure to reduce complex nutritional problems to single-nutrient interventions (zinc supplementation, vitamin D dosing, iron fortification) when the actual clinical problem involves multiple interactive nutrients and food-based delivery. Constrained: they could pursue whole-food intervention research but face career risk and funding penalties. Experience moderate extraction alongside legitimate benefit.
constraint_indexing:constraint_classification(nutritional_reductionism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EPIDEMIOLOGY (TANGLED ROPE) — Large cohort studies tracking dietary patterns over decades (Framingham, PREDIMED, NHS) contain holistic data but must translate findings into reductionist language to gain credibility: 'Mediterranean diet' becomes 'olive oil polyphenols plus whole grain fiber plus omega-3 index.' The constraint extracts from epidemiology by forcing translation of their holistic findings into component parts, yet the constraint also enables these studies to gain institutional legitimacy. Organized enough to resist and reframe (recent cohort work emphasizes dietary patterns), but constrained by journal gatekeeping and funding mechanisms that still privilege nutrient-level studies.
constraint_indexing:constraint_classification(nutritional_reductionism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REDUCTIONIST ESTABLISHMENT (PITON) — The constraint is increasingly theatrical: it persists through institutional inertia despite mounting evidence of its limitations. Nutrient biomarkers (vitamin D serum levels, apoB cholesterol, omega-3 index) cannot predict health outcomes as strongly as dietary pattern data. Yet the establishment continues performing reductionist validity — funding isolated nutrient randomized trials with null results, maintaining nutrient-based dietary guidelines, evaluating supplements by isolated biomarker changes rather than health endpoints. Theater ratio high: the activity (studying β-carotene supplementation) persists while the function (predicting or improving health) has degraded. The system maintains itself through grant momentum and career path lock-in.
constraint_indexing:constraint_classification(nutritional_reductionism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: SYSTEMS NUTRITION / FOOD-AS-MEDICINE (SCAFFOLD) — Organized coalitions (personalized nutrition research, nutrigenomics, microbiome science) are building alternative paradigms that incorporate reductionist mechanistic data but organize them around wholes (individual microbiomes, genetic-dietary interactions, food synergies). These represent a sunset clause on pure reductionism: as measurement technologies improve (metabolomics, metaomics), the framework is shifting from 'isolate the active compound' to 'understand the system through its components.' The transition is incomplete (suppression remains high during the shift period), but the exit path is structurally real. Sunset estimated at 15-25 years as systems-level nutrition becomes standard pedagogy.
constraint_indexing:constraint_classification(nutritional_reductionism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL / NATURAL LAW (MOUNTAIN) — From a civilizational analytical view, some reductionism is inherent to science itself: you cannot understand complex systems without analyzing components, and isolating variables is a foundational epistemic practice. This perspective sees nutritional reductionism as reflecting universal constraints on how knowledge is generated. However, the structural data contradicts the mountain classification: the dominance of reductionist framing is not inevitable but reflects funding incentives, journal gatekeeping, and industry influence. The engine's false summit detector will identify this as naturalization of what is contingent.
constraint_indexing:constraint_classification(nutritional_reductionism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nutritional_reductionism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nutritional_reductionism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nutritional_reductionism, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nutritional_reductionism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nutritional_reductionism, TR),
    TR >= 0.70.

:- end_tests(nutritional_reductionism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reductionist constraint extracts significant value from whole-food research communities (suppressing their work, underfunding it, marginalizing it) and from epistemic traditions that cannot be translated into isolated mechanisms (indigenous knowledge, culinary nutrition). However, the extraction is not total — clinical applications genuinely benefit from reductionist mechanistic understanding, and some funding still reaches holistic research. The value reflects that the constraint provides real coordination function (mechanistic clarity) alongside asymmetric extraction (favoring proprietary interventions). Suppression (0.62): High. Multiple reinforcing barriers: (1) funding structures strongly favor isolated nutrient research; (2) high-impact journals publish biomarker studies far more readily than dietary pattern investigations; (3) career advancement in nutrition science rewards reductionist publication records; (4) industry funding overwhelmingly supports isolation of specific compounds; (5) dietary pattern research requires multi-year cohorts with less dramatic results. The suppression has remained stable or increased over the measurement interval. Theater ratio (0.68): High and rising. The reductionist paradigm increasingly performs validity through isolated biomarker studies (vitamin D serum levels, apoB cholesterol, omega-3 index) while the predictive power for actual health outcomes has not increased proportionally. Many funded nutrient supplementation trials return null results but continue because the reductionist framework expects them. The establishment performs mechanistic clarity while losing practical guidance.
 *
 * PERSPECTIVAL GAP:
 *   CRITICAL PERSPECTIVAL DISAGREEMENT ON CLASSIFICATION: The same structural constraint (reductionism's dominance in nutrition science) produces radically different classifications from different positions. The supplement industry sees Rope (pure coordination solving a legitimate distribution problem). Whole-food researchers see Snare (pure extraction with minimal coordination benefit). Clinical researchers see Tangled Rope (mixed coordination for mechanistic understanding plus extraction through pressure toward simplistic interventions). Epidemiologists see Tangled Rope (genuine coordination need for understanding bioavailability plus extraction of their holistic findings through forced translation). Traditional knowledge systems see Snare with epistemic colonization (pure extraction, not coordination). The reductionist establishment sees Piton (increasingly performative, maintained by inertia). Systems nutrition sees Scaffold (temporary constraint with structural sunset). The analytical observer risks seeing Mountain (reductionism as inherent to scientific methodology) but the structural data reveals false summit — the dominance is contingent on funding, journals, and industry, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the reductionist constraint. Supplement manufacturers with arbitrage exit (can always profit from isolated compounds) have low d — the constraint subsidizes them. Whole-food researchers trapped in funding mechanisms and journal hierarchies have high d — the constraint extracts from them. Clinical researchers face mixed d: they benefit from reductionist tools (lowering d) but are constrained by pressure to reduce complex problems to single nutrients (raising d), yielding moderate d. The epidemiological communities have high d because they generate holistic data that the constraint forces them to translate into reductionist language, extracting the epistemic value of their work and reframing it through a different lens. Indigenous knowledge systems have maximum d — the constraint reduces their knowledge to 'anecdotal' or subjects it to colonization through reductionist translation. The engine's derivation chain maps these structural relationships to f(d) values, producing the experienced extraction each perspective reports.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that nutritional reductionism represents GENUINE COORDINATION FUNCTION (mechanistic understanding of bioavailability, dosing, biomarker relationships) PLUS ASYMMETRIC EXTRACTION (suppression of holistic research, epistemic colonization of traditional knowledge, capture of funding and publication mechanisms by supplement industry). The classification is Tangled Rope, not Snare, because the reductionist framework does solve a legitimate scientific problem — understanding how individual nutrients function in human metabolism. But the classification is NOT Rope because the extraction is substantial, systematic, and asymmetric: beneficiaries with arbitrage (supplement industry) prosper while victims trapped in academic systems (whole-food researchers, traditional knowledge holders) are suppressed. The mandatrophy is prevented by the tangled_rope type, which explicitly requires beneficiaries + victims + enforcement. The constraint genuinely coordinates mechanistic research while extracting from alternative epistemic frameworks. No single type fully captures this — and the temptation to call it Rope (just coordination) or Snare (just extraction) is exactly the mandatrophy that tangled_rope exists to resolve. The true diagnostic is the perspectival disagreement: if the beneficiaries (supplement industry) experienced it as Snare, or if the victims (whole-food researchers) experienced it as Rope, the classification would be simpler and likely wrong.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reductionist_utility_threshold,
    'At what level of system complexity does reductionist methodology cease to provide actionable health guidance?',
    'Predictive validity comparison: nutrient-level biomarkers vs dietary pattern indices vs food-quality scores for health outcomes over 5-10 year cohorts',
    'If reductionist approaches remain predictive at high complexity: constraint is legitimate coordination problem (Rope from more perspectives). If predictive validity collapses above moderate complexity: constraint is primarily extractive (Snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reductionist_utility_threshold, empirical, 'Predictive validity threshold for reductionist nutritional models').

omega_variable(
    food_synergy_measurability,
    'Can food synergy effects (nutrient-nutrient interactions within whole foods) be measured and mechanistically understood without reducing to isolated compounds?',
    'High-resolution metabolomics of whole foods vs isolated nutrients; systems pharmacology modeling of food matrices; longitudinal data on food combinations vs isolated supplementation',
    'If synergies are mechanistically tractable at systems level: reductionism is unnecessary bottleneck (Tangled Rope confirmed). If synergies require true holist approaches: reductionism is false consciousness limiting available frameworks (Snare from epistemic perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(food_synergy_measurability, empirical, 'Whether food synergy effects are measurable outside reductionist isolation').

omega_variable(
    industry_capture_mechanism,
    'How much of nutritional reductionism''s dominance is driven by supplement industry funding and patent incentives vs legitimate epistemological preference for mechanistic clarity?',
    'Funding source analysis of published nutrition research; comparison of reductionist vs holistic research funding ratios by source; citation analysis of industry-funded vs independently funded studies',
    'If capture is dominant (>60% of reductionist advantage): constraint is primarily extractive (Snare, Tangled Rope). If legitimate epistemology is dominant: constraint is mixed coordination-extraction (Tangled Rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_capture_mechanism, empirical, 'Degree of supplement industry structural influence on reductionist paradigm').

omega_variable(
    indigenous_knowledge_validation,
    'Can traditional food preparation methods and combinations produce health outcomes through mechanisms other than isolated bioactives?',
    'Mechanistic investigation of traditional food systems through non-reductionist frameworks (food matrix analysis, metabolomic profiles of traditional preparations, microbiome response patterns); longitudinal health data from populations maintaining traditional diets',
    'If mechanisms exist outside reductionist scope: indigenous knowledge is dismissed by epistemic colonization (Snare from traditional knowledge perspective confirmed). If mechanisms reduce to bioactives: reductionism was legitimate integrating framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_knowledge_validation, empirical, 'Whether traditional food health effects require non-reductionist explanation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nutritional_reductionism, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nutred_tr_t0, nutritional_reductionism, theater_ratio, 0, 0.48).
narrative_ontology:measurement(nutred_tr_t10, nutritional_reductionism, theater_ratio, 10, 0.63).
narrative_ontology:measurement(nutred_tr_t20, nutritional_reductionism, theater_ratio, 20, 0.68).
narrative_ontology:measurement(nutred_tr_t30, nutritional_reductionism, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(nutred_be_t0, nutritional_reductionism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nutred_be_t10, nutritional_reductionism, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(nutred_be_t20, nutritional_reductionism, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(nutred_be_t30, nutritional_reductionism, base_extractiveness, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nutritional_reductionism, information_standard).
narrative_ontology:affects_constraint(nutritional_reductionism, supplement_efficacy_claims).
narrative_ontology:affects_constraint(nutritional_reductionism, food_knowledge_colonization).
narrative_ontology:affects_constraint(nutritional_reductionism, precision_nutrition_industry_capture).

% DUAL FORMULATION NOTE:
% Nutritional reductionism decomposes into multiple structurally distinct constraints: (1) the epistemological preference for mechanistic clarity (genuine coordination problem, ε≈0.15, Rope); (2) the funding mechanism bias toward isolated nutrient studies (extraction mechanism, ε≈0.45, Tangled Rope); (3) the journal gatekeeping that devalues dietary pattern research (suppression mechanism, ε≈0.52, Tangled Rope); (4) the epistemic colonization of indigenous food knowledge (extraction-dominating mechanism, ε≈0.68, Snare). This story treats reductionism as an integrated constraint (ε=0.58, Tangled Rope) but the network links show how the constraint depends on downstream extraction mechanisms in supplement marketing and journal prestige systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nutritional_reductionism, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
