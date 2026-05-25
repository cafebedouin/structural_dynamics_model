% ============================================================================
% CONSTRAINT STORY: climate_attribution_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_attribution_2026, []).

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
 *   constraint_id: climate_attribution_2026
 *   human_readable: Extreme Weather Attribution Science
 *   domain: scientific/political/economic
 *
 * SUMMARY:
 *   Extreme weather attribution science quantifies how much human-induced
 *   climate change has altered the probability and magnitude of specific
 *   extreme events (hurricanes, heatwaves, floods). The science creates a
 *   structural tension: attribution findings that confirm climate causation
 *   generate legal liability for emitters and insurance risk for
 *   carbon-intensive sectors, while attribution uncertainty preserves the
 *   status quo. This constraint exhibits a tangled coordination-extraction
 *   hybrid with institutional piton aspects and a nascent scaffold sunset.
 *   The same scientific question — 'How much did climate change increase the
 *   likelihood of this hurricane?' — appears as a coordination tool for
 *   insurance pricing (rope), an institutional bottleneck that defers
 *   liability (piton), a source of pure extraction for vulnerable populations
 *   with no exit (snare), a mixed extraction-coordination mechanism
 *   constraining scientists (tangled rope), or an epistemologically necessary
 *   uncertainty (mountain view that naturalizes conservatism). The
 *   theater_ratio increase (0.45 → 0.64 over 10 years) reflects IPCC
 *   assessment cycles that create appearance of definitive closure while
 *   actual attribution science remains contested and politicized. Rapid
 *   attribution networks and climate litigation are beginning to construct
 *   alternative pathways that bypass IPCC delays, with sunset logic: as legal
 *   precedent accumulates and real-time attribution matures, the delay-based
 *   extraction mechanism weakens.
 *
 * KEY AGENTS:
 *   - Climate-Vulnerable Populations: Primary victim (powerless/trapped) — bear weather impacts with no negotiating power; attribution lag defers policy response and liability recognition
 *   - Attribution Science Community: Secondary victim (moderate/constrained) — career structure and institutional pressure constrain research agendas; litigation risk suppresses inconvenient findings; reputational attacks from both fossil fuel industry and climate advocates
 *   - Fossil Fuel Industry & Carbon Sectors: Primary beneficiary (institutional/arbitrage) — benefit from attribution lag which defers legal liability and regulatory triggers; methodological conservatism (high confidence thresholds) maintains ambiguity
 *   - Insurance Industry: Mixed position (organized/constrained) — benefit from risk quantification but lose from attribution lag; constrained by regulatory and litigation pressure
 *   - IPCC Assessment Process: Institutional gatekeeper (institutional/arbitrage) — maintains performative authority while cycle delays ensure science lags behind attribution demands
 *   - Rapid Attribution Networks: Emerging alternative (organized/constrained) — building real-time attribution pathways; sunset logic suggests 10-15 year replacement of IPCC bottleneck
 *   - Climate Litigation Coalitions: Organized advocates (organized/constrained) — drive demand for specific attribution but litigation adversarialism may suppress scientific objectivity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_attribution_2026, 0.52).
domain_priors:suppression_score(climate_attribution_2026, 0.58).
domain_priors:theater_ratio(climate_attribution_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_attribution_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(climate_attribution_2026, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_attribution_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_attribution_2026, tangled_rope).
narrative_ontology:human_readable(climate_attribution_2026, "Extreme Weather Attribution Science").
narrative_ontology:topic_domain(climate_attribution_2026, "scientific/political/economic").

domain_priors:requires_active_enforcement(climate_attribution_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_attribution_2026, fossil_fuel_industry).
narrative_ontology:constraint_beneficiary(climate_attribution_2026, carbon_intensive_sectors).
narrative_ontology:constraint_beneficiary(climate_attribution_2026, litigation_defense_interests).
narrative_ontology:constraint_victim(climate_attribution_2026, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_attribution_2026, insurance_market_transparency).
narrative_ontology:constraint_victim(climate_attribution_2026, attribution_science_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE-VULNERABLE POPULATIONS (SNARE) — Small island states, subsistence farmers, urban poor in flood zones experience extreme weather impacts but cannot exit or negotiate. Bear full cost of attribution lag; no voice in science policy. Maximum extraction from trapped position.
constraint_indexing:constraint_classification(climate_attribution_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ATTRIBUTION SCIENCE COMMUNITY (TANGLED ROPE) — Scientists benefit from research funding and career advancement through attribution work, but also bear extraction via suppression of inconvenient findings, litigation risk, and reputational attacks. Constrained exit: career structure locks researchers in; institutional pressure forces cautious framing.
constraint_indexing:constraint_classification(climate_attribution_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FOSSIL FUEL INDUSTRY & CARBON-INTENSIVE SECTORS (ROPE) — Primary beneficiary. Experiences the constraint as coordination: attribution science's methodological conservatism (requiring 5+ events to establish human link, high confidence thresholds) enables continued operation by deferring legal liability and regulatory action. Net beneficiary position with strong arbitrage options.
constraint_indexing:constraint_classification(climate_attribution_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSURANCE INDUSTRY (TANGLED ROPE) — Mixed position. Benefits from attribution science's quantification of risk (enables pricing models). But extraction: attribution lag delays risk-adjusted premiums, creating cross-subsidization between regions. Organized but constrained by regulatory pressure and tort liability.
constraint_indexing:constraint_classification(climate_attribution_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: IPCC ATTRIBUTION ASSESSMENT (PITON) — Performative institutional ritual. IPCC cycles (every 7 years) ensure assessment lags behind science. Statements like 'it is unequivocal that human influence has warmed the climate' create false sense of resolution while deferring specific-event attribution. Theater_ratio=0.64 reflects gap between apparent authority and actual actionability.
constraint_indexing:constraint_classification(climate_attribution_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Statistical attribution is inherently difficult: extreme events are rare, climate variability is large, causal chains are complex. Some attribution uncertainty is epistemologically necessary. However, this risks naturalizing the conservatism bias built into attribution methodology (high confidence thresholds, rare-event bias, peer review suppression of early signals). The analytical observer's mountain view naturalizes what may be contingent institutional choices.
constraint_indexing:constraint_classification(climate_attribution_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: RAPID ATTRIBUTION NETWORKS & LEGAL MOBILIZATION (SCAFFOLD) — New institutions (World Weather Attribution, climate litigation coalitions) are building real-time attribution pathways that bypass IPCC cycles. Sunset logic: as rapid attribution matures and legal precedents establish causation, the delay-based extraction mechanism weakens. These networks see a sunset window (10-15 years) before climate causation becomes routine in law.
constraint_indexing:constraint_classification(climate_attribution_2026, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_attribution_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_attribution_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_attribution_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_attribution_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_attribution_2026, TR),
    TR >= 0.70.

:- end_tests(climate_attribution_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts through attribution lag that delays policy response and liability recognition. The extraction is structural: attribution methodology requires high confidence thresholds (>95% for human causation attribution), multiple events to establish pattern, and peer review cycles that exclude early signals. These features are partly epistemologically necessary (small samples, natural variability) but partly institutional choices (conservative liability protection). The increase from 0.28 to 0.52 over the interval reflects growing litigation demand, which amplifies the delay mechanism. Suppression (0.58): Moderate-high. Multiple barriers limit attribution scope: (1) statistical — extreme events are rare, climate variability is large; (2) institutional — litigation risk suppresses inconvenient findings; (3) epistemic — counterfactual specificity is difficult for individual events. Suppression is not total — some attribution findings do emerge despite barriers. Theater_ratio (0.64): Moderate-high. IPCC assessment cycles create theatrical closure ('it is unequivocal that...') while actual event-specific attribution remains contested. The ritual of global climate summits and IPCC reports produces appearance of definitive authority without enabling specific legal/policy action. Theater has increased over the interval as gap between general climate consensus and specific-event attribution has widened.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap lies between beneficiaries (fossil fuel industry, IPCC process) who see the constraint as protective coordination or natural epistemological limit, versus victims (vulnerable populations, attribution science integrity) who see pure extraction. The fossil fuel industry's rope perspective emphasizes methodological rigor and the statistical necessity of high confidence thresholds — for them, attribution science is a coordination tool that enables continued operation while satisfying public demand for scientific accountability. The vulnerable populations' snare perspective emphasizes that attribution lag defers life-saving policy responses — they experience the same high confidence threshold as a barrier that protects emitters. The attribution science community's tangled rope perspective captures this: they genuinely want scientific rigor (coordination function) but also experience pressure from litigation adversarialism (extraction). The IPCC's piton perspective reveals that the institutional assessment process is performing authority while remaining substantively constrained by cycles and consensus-building that lag behind actual scientific debate. The rapid attribution networks' scaffold perspective opens a structural exit: real-time attribution with lower latency and higher specificity, with sunset logic suggesting 10-15 year replacement cycle. The analytical observer's mountain view risks naturalizing this entire apparatus as epistemologically necessary, when in fact much of the delay is institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: Fossil fuel industry and carbon sectors experience low effective extraction (d ≈ 0.15) because attribution conservatism protects their interests — they are structural beneficiaries of the delay mechanism. Their exit options (arbitrage) are strong: litigation risk remains manageable, regulatory triggers are deferred. Victim directionality: Climate-vulnerable populations experience high extraction (d ≈ 0.90) due to trapped position (no exit options, cannot negotiate) and victim status (bear full cost of attribution lag). Attribution scientists experience moderate extraction (d ≈ 0.55) from constrained position (career structure locks them in) and mixed beneficiary-victim status (benefit from research funding but lose autonomy to litigation pressure). Insurance industry experiences moderate extraction (d ≈ 0.50) from organized but constrained position (can organize responses but cannot fully exit regulatory environment). Rapid attribution networks derive d ≈ 0.35 (mobile position creating alternative pathways) suggesting scaffold classification with sunset logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in climate attribution centers on whether the constraint is fundamentally a coordination problem (rope) or an extraction mechanism (snare). The fossil fuel industry frames it as pure coordination: attribution science serves the public need to understand climate causation while maintaining scientific rigor. Vulnerable populations frame it as pure extraction: attribution delays defer policy responses that could save lives. The tangled_rope classification resolves this: the constraint BOTH coordinates (enables scientists to pursue rigorous research, enables insurance pricing) AND extracts (suppresses inconvenient findings, defers liability recognition, constrains policy through uncertainty). The mandatrophy is not 'which is it?' but 'the coordination and extraction are structurally intertwined.' Litigation pressure creates feedback loops: scientists who produce findings favoring plaintiffs face adversarial cross-examination (suppression mechanism), so the scientific community becomes cautious (extraction from scientists). The beneficiary (industry) benefits from this caution (coordination for them). The victim (vulnerable populations) bears the cost. Separating the two would require institutional decoupling: independent rapid attribution that is not subject to litigation discovery, or legal systems that accept statistical causation without demanding epistemological certainty. The scaffold perspective suggests this decoupling is emerging (rapid attribution networks, climate litigation establishing precedent), with 10-15 year sunset. Until then, the tangled_rope classification with high mandatrophy risk applies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    confidence_threshold_convergence,
    'Are current attribution confidence thresholds (>95% human causation link) products of statistical necessity or institutional conservatism protecting liability interests?',
    'Bayesian analysis comparing attributed vs unattributed events; historical comparison of confidence thresholds across decades; game-theoretic modeling of how liability pressure shapes methodology',
    'If statistical necessity: threshold reflects true epistemological limit (mountain or rope). If institutional conservatism: threshold reflects extraction mechanism (snare or tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confidence_threshold_convergence, empirical, 'Whether attribution confidence thresholds reflect statistical necessity or institutional conservatism').

omega_variable(
    counterfactual_attribution_uniqueness,
    'Can extreme weather attribution establish unique counterfactuals (i.e., ''this specific hurricane would not have occurred without climate change'') or only risk modification (i.e., ''this type of event is 2x more likely'')?',
    'Theoretical limits analysis on counterfactual specification in climate science; comparison with attribution in other domains (epidemiology, caustics); expert elicitation on whether individual-event counterfactuals are conceptually possible',
    'If unique counterfactuals possible: legal causation can be established with high specificity (reduces snare extraction). If only risk modification: legal claims remain statistical (maintains ambiguity favoring defendants).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counterfactual_attribution_uniqueness, conceptual, 'Whether attribution can establish unique event counterfactuals or only risk modification').

omega_variable(
    litigation_feedback_bias,
    'Does the legal system''s demand for specific attribution (which extreme events were caused by climate change?) distort the scientific process through adversarial filtering, such that inconvenient findings are suppressed in anticipation of cross-examination?',
    'Comparative analysis of attribution findings in litigation vs peer-reviewed literature; interview study of attribution scientists on litigation risk influence; publication bias testing for results disfavoring plaintiffs',
    'If strong feedback bias: litigation shapes science more than science shapes litigation (tangled rope extraction mechanism verified). If weak bias: attribution science maintains independence (rope or scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(litigation_feedback_bias, empirical, 'Whether litigation pressure suppresses inconvenient attribution findings').

omega_variable(
    rapid_attribution_scalability,
    'Can real-time attribution networks (World Weather Attribution, national rapid attribution centers) achieve sustained accuracy and institutional authority comparable to IPCC assessments, or do they sacrifice rigor for speed?',
    'Longitudinal accuracy tracking of rapid vs IPCC attribution; retrospective validation of rapid attribution forecasts; institutional adoption rate by legal and insurance systems',
    'If scalable with maintained rigor: scaffold sunset is real (20-year path to alternative institution). If rigor-speed tradeoff: rapid attribution remains supplementary, maintaining IPCC bottleneck.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rapid_attribution_scalability, empirical, 'Whether rapid attribution networks can scale without sacrificing rigor').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_attribution_2026, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_attr_tr_t0, climate_attribution_2026, theater_ratio, 0, 0.45).
narrative_ontology:measurement(clim_attr_tr_t10, climate_attribution_2026, theater_ratio, 10, 0.64).
narrative_ontology:measurement(clim_attr_tr_t20, climate_attribution_2026, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(clim_attr_be_t0, climate_attribution_2026, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(clim_attr_be_t10, climate_attribution_2026, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(clim_attr_be_t20, climate_attribution_2026, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_attribution_2026, information_standard).
narrative_ontology:affects_constraint(climate_attribution_2026, climate_liability_legal_standard).
narrative_ontology:affects_constraint(climate_attribution_2026, insurance_loss_recognition_timing).
narrative_ontology:affects_constraint(climate_attribution_2026, rapid_attribution_methodology).

% DUAL FORMULATION NOTE:
% Climate attribution science decomposes into three structurally distinct constraints: (1) general climate causation (now well-established, ε ≈ 0.08, mountain); (2) extreme weather event attribution (contested, ε ≈ 0.52, tangled rope); (3) legal liability causation standards (policy/legal rather than scientific, different ε). This story focuses on constraint 2. The three are linked: general climate causation is cited as evidence for extreme event attribution; extreme event attribution drives legal liability standards. Each has different beneficiaries/victims and different extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_attribution_2026, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
