% ============================================================================
% CONSTRAINT STORY: med_diet_consensus_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The Mediterranean diet scientific consensus represents a tangled hybrid
 *   of genuine health coordination and economic extraction. Originating from
 *   the Seven Countries Study (1950s-60s), the consensus has become
 *   institutionalized across nutrition research, public health policy,
 *   agricultural trade, and health marketing. The constraint exhibits
 *   extraction through publication suppression of alternative dietary
 *   approaches, career penalties for researchers challenging the paradigm,
 *   and market consolidation favoring Mediterranean produce exporters.
 *   Simultaneously, the constraint performs genuine coordination: providing
 *   clear, actionable dietary guidance simplifies public health messaging and
 *   creates research coherence. The theater ratio has risen from 0.35 (when
 *   Mediterranean diet claims were empirically contestable) to 0.61
 *   (contemporary state where much consensus maintenance operates through
 *   citation networks and institutional inertia rather than new empirical
 *   evidence). The extractiveness has accumulated from 0.28 (early
 *   coordination-dominant phase) to 0.52 (current mixed
 *   extraction-coordination state), driven by publication bias, funding
 *   concentration, and suppression of comparative dietary research.
 *
 * KEY AGENTS:
 *   - Mediterranean Agricultural Exporters: Primary beneficiary (institutional/arbitrage) — capture market share and premium pricing justified by scientific consensus; arbitrage capacity allows exit from constraint
 *   - Nutrition Research Institutions: Secondary beneficiary and enforcer (organized/mobile) — gain research funding, citation prestige, and institutional standing through Mediterranean diet research programs; enforce consensus via journal gate-keeping and funding review
 *   - Alternative Diet Researchers: Primary victim (powerless/trapped) — suppressed from publication venues, penalized in funding competitions, marginalized in professional networks; cannot exit without abandoning research career
 *   - Small-Scale Non-Mediterranean Farmers: Secondary victim (powerless/trapped) — unable to compete in markets where Mediterranean produce commands premiums; trapped in disadvantaged market position
 *   - Public Health Agencies: Mixed actor (moderate/constrained) — benefit from simplified guidance but constrained by consensus commitment; limited policy flexibility despite emerging evidence of dietary diversity benefits
 *   - Dietary Diversity Evidence: Tertiary victim (powerless/trapped) — abstract collective good; evidence for alternative approaches is suppressed or deprioritized; no institutional advocate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(med_diet_consensus_2026, 0.52).
domain_priors:suppression_score(med_diet_consensus_2026, 0.48).
domain_priors:theater_ratio(med_diet_consensus_2026, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(med_diet_consensus_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(med_diet_consensus_2026, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(med_diet_consensus_2026, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(med_diet_consensus_2026, tangled_rope).
narrative_ontology:human_readable(med_diet_consensus_2026, "Mediterranean Diet Scientific Hegemony").
narrative_ontology:topic_domain(med_diet_consensus_2026, "health/scientific/economic").

domain_priors:requires_active_enforcement(med_diet_consensus_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(med_diet_consensus_2026, mediterranean_agricultural_exporters).
narrative_ontology:constraint_beneficiary(med_diet_consensus_2026, nutrition_research_institutions).
narrative_ontology:constraint_beneficiary(med_diet_consensus_2026, mediterranean_health_tourism).
narrative_ontology:constraint_victim(med_diet_consensus_2026, alternative_diet_researchers).
narrative_ontology:constraint_victim(med_diet_consensus_2026, non_mediterranean_agricultural_producers).
narrative_ontology:constraint_victim(med_diet_consensus_2026, dietary_diversity_evidence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE DIET RESEARCHER (SNARE) — Trapped within funding structures and publication venues dominated by Mediterranean diet paradigm. Career advancement, grant funding, and journal acceptance systematically penalize research on non-Mediterranean dietary patterns (ketogenic, carnivore, plant-based alternatives). Maximum extraction experienced: suppressed from professional legitimacy with no exit option.
constraint_indexing:constraint_classification(med_diet_consensus_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL-SCALE NON-MEDITERRANEAN FARMERS (SNARE) — Trapped in agricultural markets where Mediterranean produce commands price premiums justified by scientific consensus. Unable to compete on nutritional grounds or to establish equivalent credibility for their own crops. Suppression operates through market channels reinforced by scientific authority.
constraint_indexing:constraint_classification(med_diet_consensus_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: MEDITERRANEAN AGRICULTURAL EXPORTERS (ROPE) — Institutional beneficiaries with significant arbitrage capacity. Scientific consensus generates demand; trade agreements and export subsidies amplify benefits. Experience constraint as pure coordination: aligning scientific messaging, marketing narratives, and health policy creates market capture without coercive overhead.
constraint_indexing:constraint_classification(med_diet_consensus_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC HEALTH AGENCIES (TANGLED ROPE) — Constrained by limited budgets and political pressure, but also benefit from simplified dietary guidance. Mediterranean diet consensus provides actionable public messaging and reduces decision-making uncertainty. However, agencies bear extraction costs: commitment to consensus limits policy flexibility, suppresses consideration of alternative dietary approaches, and creates path dependency in nutrition policy.
constraint_indexing:constraint_classification(med_diet_consensus_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NUTRITION RESEARCH INSTITUTIONS (TANGLED ROPE) — Organized actors with significant agency. Benefit from consensus (research funding, publication prestige, institutional standing). Extract from the constraint: institutions with Mediterranean diet research programs gain resources and citations; institutions without such programs must establish them or risk marginal status. Active enforcement required: journal gate-keeping, citation networks, funding review panels coordinate consensus maintenance.
constraint_indexing:constraint_classification(med_diet_consensus_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: THE SEVEN COUNTRIES STUDY LEGACY (PITON) — Institutional inertia. The foundational 1950s-60s Seven Countries Study data had methodological limitations (participant selection bias, short follow-up windows, covariate adjustment issues), yet the consensus it generated persists through institutional momentum. Modern meta-analyses show Mediterranean diet benefits are modest and context-dependent, but the original research framework maintains credibility through theatrical citation patterns. Theater ratio elevated: reviewing institutions continue to frame the Seven Countries Study as authoritative despite known limitations.
constraint_indexing:constraint_classification(med_diet_consensus_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Civilizational view reveals mixed coordination and extraction. Genuine coordination function: Mediterranean diet is correlated with longevity and cardiometabolic health in observational settings. Genuine extraction: consensus suppresses alternative hypotheses (genetic ancestry confounding, confounded socioeconomic factors, measurement error in longitudinal studies) and concentrates research resources on confirming rather than falsifying the paradigm. Effective extraction moderate but persistent across generational timescales.
constraint_indexing:constraint_classification(med_diet_consensus_2026, tangled_rope,
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
    constraint_indexing:constraint_classification(med_diet_consensus_2026, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.52): The constraint exhibits meaningful extraction but not total. Mediterranean agricultural exporters capture market benefits; research institutions gain resources and prestige; alternative researchers are suppressed from funding and publication. However, extraction is not monolithic — some alternative diet research is published, some funding reaches non-Mediterranean investigations, and some researchers establish careers despite consensus opposition. The 0.52 value reflects moderate extraction with partial suppression effectiveness. Suppression (0.48): Significant but incomplete. Publication bias and journal gate-keeping suppress alternatives, but do not eliminate them entirely. Career penalties are real but not absolute — some researchers build successful careers challenging consensus. Funding disparities are documented but alternative approaches still receive some resources. Theater ratio (0.61): Elevated and rising. Early consensus was empirically grounded in genuine observational associations. Contemporary consensus maintenance relies increasingly on citation networks, institutional authority, and therapeutic marketing rather than new evidence. The Seven Countries Study framework persists despite methodological limitations; meta-analyses showing modest context-dependent effects are reinterpreted as 'Mediterranean diet proven' rather than 'effects smaller than previously claimed.' Piton detection triggered: theater ratio > 0.70 is not met, but the rising trajectory combined with degraded empirical support suggests approaching piton territory in next observational window.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary and victim perspectives diverge sharply. Mediterranean agricultural exporters and nutrition institutions see the constraint as pure coordination (Rope) — aligning around a scientifically-supported dietary pattern that enables market function and research coherence. Alternative researchers and non-Mediterranean farmers see it as pure extraction (Snare) — systematic suppression of their work with no exit option. Public health agencies occupy the middle ground (Tangled Rope) — they benefit from simplified guidance but are constrained by consensus rigidity. The analytical observer's tangled rope classification resolves the gap: both coordination and extraction are structurally real; they are not observable-dependent. The constraint simultaneously enables (genuine health benefits from Mediterranean patterns) and suppresses (alternative approaches that may be equally or context-specifically superior). The perspectival disagreement is not about facts but about the relative weighting of coordination benefits vs extraction costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position in the extraction flow. Mediterranean agricultural exporters are beneficiaries with arbitrage options (d ≈ 0.15) — they can exit by shifting crops or markets if consensus dissolves, and the constraint actively benefits them. Alternative researchers are victims with trapped exit (d ≈ 0.95) — they cannot exit the research domain without abandoning professional identity, and the constraint systematically penalizes them. Public health agencies are constrained beneficiaries (d ≈ 0.50) — they benefit from simplified guidance but cannot easily exit consensus without political backlash. Nutrition research institutions are organized beneficiaries with mobile exit (d ≈ 0.40) — they could redirect research to alternative approaches but face institutional inertia and career incentives favoring consensus work. The engine's sigmoid f(d) applies these structural positions: low-d beneficiaries experience negative effective extraction (they gain from the constraint); high-d victims experience amplified extraction (the constraint amplifies their costs); mid-range constrained actors experience moderate extraction. Directionality overrides were not needed — structural derivation produces accurate d values from beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating genuine coordination function alongside genuine extraction. The Mediterranean diet is not falsely labeled coordination (it has real health benefits and enables coherent public messaging). The constraint is correctly identified as Tangled Rope, not Snare, because the coordination benefits are structural, not illusory. However, the analytical observer perspective reveals that the coordination function is being exploited to justify suppression of alternatives. The extraction mechanism relies on naturalizing the coordination benefit ('Mediterranean diet is scientifically proven best') into a reason for suppressing comparative research ('why fund studies on inferior approaches?'). The mandatrophy resolution: the constraint is legitimately mixed. The perspectival gap is not between truth and falsehood but between the beneficiary's real coordination benefits and the victim's real suppression costs. Both are operative. The rising theater ratio (0.35→0.61) suggests gradual drift from empirically-grounded coordination toward institutionally-maintained consensus theater — a degradation pathway toward Piton if suppression remains constant while empirical justification weakens further.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    confounding_by_socioeconomic_proxy,
    'How much of the Mediterranean diet health benefit reflects Mediterranean geography/culture rather than specific dietary composition?',
    'Randomized controlled trials with Mediterranean diet vs other nutrient-matched diets in diverse populations; genetic ancestry analysis in cohort studies; geographic migration studies comparing diet-health association by settlement location',
    'If socioeconomic/cultural confounding > 50%: consensus overstates specific dietary mechanism, classification remains Tangled Rope but beneficiary list shifts from diet content to geographic branding. If < 20%: Mediterranean diet mechanism is robust, constraint classification shifts toward Rope (pure coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(confounding_by_socioeconomic_proxy, empirical, 'Whether Mediterranean diet benefits reflect specific composition or socioeconomic/geographic confounding').

omega_variable(
    measurement_error_in_longitudinal_cohorts,
    'Do dietary measurement errors in observational cohorts systematically favor Mediterranean diet associations?',
    'Validation studies comparing food frequency questionnaires to biomarkers; regression calibration analysis of measurement error impact on effect estimates; comparison of diet-health associations in studies using repeated measurements vs single baseline assessment',
    'If measurement error favors Mediterranean associations by > 30%: consensus reflects methodological artifact rather than true effect, may warrant downgrade to Piton (degraded institutional memory). If < 10%: associations are robust, constraint remains Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_error_in_longitudinal_cohorts, empirical, 'Measurement error bias in Mediterranean diet association studies').

omega_variable(
    publication_bias_and_file_drawer,
    'What fraction of completed Mediterranean diet studies remain unpublished due to null or negative findings?',
    'Prospective trial registration database analysis comparing pre-registered hypotheses to published results; funnel plot asymmetry tests; communication with researchers about unpublished work; examination of grant databases for completed studies without corresponding publications',
    'If > 40% of studies unpublished: consensus is substantially inflated by publication bias, classification warrants Snare designation (extraction via suppression of alternatives). If < 15%: publication bias is minor factor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publication_bias_and_file_drawer, empirical, 'Publication bias and file drawer effects in Mediterranean diet research').

omega_variable(
    alternative_dietary_pattern_suppression,
    'Are alternative dietary approaches (plant-based, low-carbohydrate, traditional non-Mediterranean) systematically receiving less research funding and publication venue access?',
    'NIH grant database analysis of funding allocation by dietary approach over time; journal acceptance rate comparison for Mediterranean vs non-Mediterranean diet submissions; citation network analysis of research clusters',
    'If alternative approaches receive < 20% of equivalent funding despite equivalent population health relevance: suppression is structural and intentional, classification remains Tangled Rope with high suppression. If funding is proportionate to evidence: constraint is primarily coordination-based Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_dietary_pattern_suppression, empirical, 'Funding and publication suppression of non-Mediterranean dietary research').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(med_diet_consensus_2026, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meddict_theater_1950s, med_diet_consensus_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(meddict_theater_1980s, med_diet_consensus_2026, theater_ratio, 30, 0.5).
narrative_ontology:measurement(meddict_theater_2020s, med_diet_consensus_2026, theater_ratio, 70, 0.61).

% Extraction over time
narrative_ontology:measurement(meddict_extract_1950s, med_diet_consensus_2026, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(meddict_extract_1980s, med_diet_consensus_2026, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(meddict_extract_2020s, med_diet_consensus_2026, base_extractiveness, 70, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(med_diet_consensus_2026, information_standard).
narrative_ontology:affects_constraint(med_diet_consensus_2026, nutritional_reductionism).
narrative_ontology:affects_constraint(med_diet_consensus_2026, agricultural_trade_protectionism).
narrative_ontology:affects_constraint(med_diet_consensus_2026, alternative_diet_research_funding).

% DUAL FORMULATION NOTE:
% The Mediterranean diet consensus decomposes into two structurally distinct constraint families: (1) the empirical claim about health benefits (observed associations in epidemiological data — genuinely mixed coordination and extraction), and (2) the institutional consensus enforcement (publication suppression, funding allocation, research paradigm dominance — pure extraction dressed as coordination). This story addresses the mixed empirical + institutional phenomenon. Downstream constraints capture specific mechanisms: nutritional reductionism (the conceptual framing that single dietary patterns can be 'optimal'), agricultural trade protectionism (how Mediterranean produce exporters use health consensus to justify trade advantages), and alternative diet research funding gaps (the suppression mechanism itself as a constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
