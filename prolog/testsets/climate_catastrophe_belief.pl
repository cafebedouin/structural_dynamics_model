% ============================================================================
% CONSTRAINT STORY: climate_catastrophe_belief
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_catastrophe_belief, []).

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
 *   constraint_id: climate_catastrophe_belief
 *   human_readable: Belief in Inevitable Near-Term Climate Catastrophe
 *   domain: social/political
 *
 * SUMMARY:
 *   The belief in inevitable near-term climate catastrophe represents a
 *   structural constraint that operates simultaneously as coordination
 *   mechanism, extraction apparatus, and institutional theater. Beginning
 *   around 2005 with increasing media emphasis on tipping points and
 *   extinction risks, and intensifying through 2015-2025, the catastrophe
 *   narrative has shaped climate policy, investment flows, public psychology,
 *   and scientific communication. The constraint's extractiveness derives
 *   from the gap between the psychological costs of catastrophe conviction
 *   (anxiety, reduced reproductive intention, existential despair) and the
 *   actual decision-making power of those bearing the belief. Its
 *   coordination function derives from the genuine need to mobilize
 *   decarbonization investment against free-rider incentives. Its theater
 *   derives from the shift in climate science communication from
 *   probabilistic risk assessment to emphatic catastrophe framing, often with
 *   deadlines that passed without predicted events. The constraint exhibits
 *   all seven perspectives: pure extraction for trapped workers and
 *   anxiety-burdened youth, mixed coordination-extraction for regulatory
 *   actors and moderate stakeholders, pure coordination for green industries,
 *   degraded theater for climate institutions, and measured tangled-rope for
 *   analytical observers who see both real climate risk and real
 *   institutional capture.
 *
 * KEY AGENTS:
 *   - Fossil Fuel Dependent Workers: Primary victims (powerless/trapped) — face industry obsolescence, policy discrimination, and social stigma; zero exit options within the constraint timeframe
 *   - Climate-Anxious Youth: Secondary victims (powerless/trapped) — bear psychological harm (clinical anxiety, depression, reduced reproductive intention) without decision-making power over climate policy
 *   - Green Technology Industries: Primary beneficiaries (institutional/arbitrage) — experience the constraint as pure coordination; catastrophe belief drives policy, investment, and demand; abundant exit options (alternative markets, jurisdictions)
 *   - Regulatory Agencies and Compliance Actors: Secondary beneficiaries (moderate/constrained) — benefit from policy urgency and institutional funding; constrained by dual mandate (climate action AND social stability)
 *   - Climate Science Establishment: Institutional actor (institutional/arbitrage) — maintains catastrophe framing through institutional inertia; sees own models as degraded (piton perspective) but persists due to funding and media dependence
 *   - Climate Mitigation Coalition: Organized agents (organized/constrained) — perceive constraint as temporary scaffold with sunset (decarbonization transition sufficiently complete); constrained by need to maintain political coalition
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine climate coordination need and extraction through institutional capture, psychological harm, and policy distortion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_catastrophe_belief, 0.62).
domain_priors:suppression_score(climate_catastrophe_belief, 0.58).
domain_priors:theater_ratio(climate_catastrophe_belief, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_catastrophe_belief, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_catastrophe_belief, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_catastrophe_belief, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_catastrophe_belief, tangled_rope).
narrative_ontology:human_readable(climate_catastrophe_belief, "Belief in Inevitable Near-Term Climate Catastrophe").
narrative_ontology:topic_domain(climate_catastrophe_belief, "social/political").

domain_priors:requires_active_enforcement(climate_catastrophe_belief).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_catastrophe_belief, climate_emergency_institutional_actors).
narrative_ontology:constraint_beneficiary(climate_catastrophe_belief, green_technology_industries).
narrative_ontology:constraint_beneficiary(climate_catastrophe_belief, regulatory_agencies).
narrative_ontology:constraint_victim(climate_catastrophe_belief, fossil_fuel_dependents).
narrative_ontology:constraint_victim(climate_catastrophe_belief, climate_skeptics).
narrative_ontology:constraint_victim(climate_catastrophe_belief, economically_vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOSSIL FUEL DEPENDENT WORKER (SNARE) — Trapped in industries marked for obsolescence by the catastrophe narrative. Career alternatives absent, geographic relocation difficult, social status declining. No exit from the constraint; maximum extraction through job insecurity, policy discrimination, and social stigma. Powerless agent in a snare with no coordination benefit.
constraint_indexing:constraint_classification(climate_catastrophe_belief, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLIMATE-ANXIOUS YOUTH (SNARE) — Trapped by psychological conviction of near-certain catastrophe; no exit option for anxiety itself. The belief mechanism produces measured psychological harm (climate anxiety, depression, reduced reproductive intention). Constraint extraction appears as emotional burden without corresponding agency or control. Trapped both by belief and by lack of decision-making power over climate policy.
constraint_indexing:constraint_classification(climate_catastrophe_belief, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: REGULATORY COMPLIANCE ACTOR (TANGLED ROPE) — Constrained by dual mandate: reduce emissions AND maintain social stability. Experiences both coordination function (climate policy coordination) and extraction (compliance costs, liability risk, political pressure). Can exit partially through litigation or lobbying, but constrained by public mandate and scientific authority claims. Mixed experience: genuine climate coordination need coexists with rent-seeking by green technology sectors.
constraint_indexing:constraint_classification(climate_catastrophe_belief, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GREEN TECHNOLOGY INDUSTRY (ROPE) — High arbitrage exit option; can invest in alternative markets and jurisdictions. Experiences the catastrophe belief as pure coordination mechanism: the shared conviction of climate necessity drives policy, investment, and demand. Net beneficiary from the belief's enforcement. Low experienced extraction because exit options are abundant and the constraint aligns with profit incentives.
constraint_indexing:constraint_classification(climate_catastrophe_belief, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CLIMATE MITIGATION COALITION (SCAFFOLD) — Organized actors (environmental NGOs, climate scientists, progressive governments) perceive the catastrophe belief as a temporary mobilization device with an implicit sunset: once decarbonization transitions are sufficiently advanced and renewable infrastructure matures, the acute psychological pressure of imminent catastrophe can relax into stable long-term management. Constrained by the need to maintain political coalition while executing the transition. Theater ratio moderate: some communication is genuine education, some is crisis-framing for policy urgency.
constraint_indexing:constraint_classification(climate_catastrophe_belief, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CLIMATE SCIENCE ESTABLISHMENT (PITON) — Maintains the catastrophe framing through institutional inertia even as model predictions have been repeatedly revised and specific deadlines passed without predicted tipping points. The constraint serves performative functions: institutional funding preservation, media engagement, policy access. Theater ratio 0.65: scientific communication has shifted from measured uncertainty quantification to catastrophe emphasis. The primary scientific function (accurate climate modeling) coexists with theatrical enforcement of urgency. Scientists see their own models and timelines as increasingly degraded, but the institutional apparatus persists.
constraint_indexing:constraint_classification(climate_catastrophe_belief, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational analytical stance, the catastrophe belief constraint serves genuine coordination function (mobilizing decarbonization investment, preventing complacency) while simultaneously extracting through psychological harm, policy distortions, and suppression of nuanced cost-benefit analysis. The belief has coordination benefits (mobilization for climate action) and extraction costs (anxiety disorders, reduced life satisfaction, policy capture by green rent-seekers). Not a false summit (Mountain would require ε ≤ 0.25); not pure extraction (Snare would require suppression ≥ 0.60 and ε ≥ 0.46 without coordination). Tangled Rope is the precise classification: both coordination and extraction are structurally necessary.
constraint_indexing:constraint_classification(climate_catastrophe_belief, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_catastrophe_belief_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_catastrophe_belief, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_catastrophe_belief, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_catastrophe_belief, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_catastrophe_belief, TR),
    TR >= 0.70.

:- end_tests(climate_catastrophe_belief_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The constraint extracts psychological costs (measured anxiety and depression in climate-anxious populations), opportunity costs (policy-driven industry obsolescence for fossil fuel dependents), and efficiency costs (some climate policies fail standard benefit-cost tests). But extraction is not maximal because decarbonization is a legitimate collective action problem requiring mobilization, and catastrophe framing does produce genuine coordination benefits. The value reflects both real harms and real benefits coexisting. Suppression (0.58): Moderate-high. Suppression mechanisms include social stigma for climate skeptics, media amplification of worst-case scenarios while dampening moderate risk assessments, institutional suppression of cost-benefit analyses that show unfavorable policy ratios, and psychological suppression (anxiety creates compliance without explicit coercion). But suppression is not total — alternative viewpoints exist in published literature, and some jurisdictions maintain pragmatic cost-benefit analysis. Theater ratio (0.65): Moderate-high. Climate science communication has shifted from quantified probability distributions to emphatic catastrophe narratives. Specific deadlines (Arctic ice-free by 2013, ice sheets collapsing by 2025, Amazon dieback imminent) have passed or been repeatedly extended without major media acknowledgment of prediction failure. Communication emphasizes worst-case scenarios and excludes adaptive capacity, technological solutions, and historical precedents of successful environmental remediation. But theater is not absolute — peer-reviewed literature maintains more nuance, and some communicators prioritize measured assessment. The theater ratio increased over the interval as catastrophe framing replaced probabilistic risk communication.
 *
 * PERSPECTIVAL GAP:
 *   The most extreme perspectival gap is between the fossil fuel dependent worker (snare: pure extraction, trapped, powerless) and the green technology industry (rope: pure coordination, arbitrage exit, institutional power). The worker bears extraction through industry obsolescence and social stigma with zero exit options. The industry experiences the same constraint as a coordination mechanism that drives profitable demand. The regulatory actor occupies the middle ground (tangled rope: both coordination and extraction, constrained exit, moderate power). The climate-anxious youth and fossil fuel worker both classify as snare but through different mechanisms: the youth through psychological extraction, the worker through economic extraction. The climate mitigation coalition sees the constraint as temporary (scaffold) with a sunset when decarbonization infrastructure matures. The climate science establishment sees its own process as degraded (piton: theatrical maintenance of urgency despite repeated deadline failures). The analytical observer sees the full tangled-rope structure: genuine coordination need coexists with extraction through policy capture, psychological harm, and institutional theater. No single perspective is false; each reveals a true structural relationship.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation of directionality (d) for each agent follows from beneficiary/victim status and exit options. Fossil fuel workers are victims with trapped exit → d approaches 1.0 (full target). Climate-anxious youth are victims with trapped exit (trapped by anxiety and lack of agency) → d approaches 0.95 (high target, though slightly less than total because some agency exists via personal adaptation). Green technology industries are beneficiaries with arbitrage exit → d approaches 0.05 (full beneficiary, can exit into profitable alternatives). Regulatory actors are both (regulation is a coordination need AND creates compliance extraction) with constrained exit → d approaches 0.50-0.55 (symmetric, but leaning toward target because suppression limits exit). Climate scientists are institutional beneficiaries (institutional funding, media access, policy influence) with arbitrage exit (can shift research domains) → d approaches 0.15 (lean beneficiary). The analytical observer occupies d ≈ 0.72 (neutral analytical position). These directionality values are not overridden; they derive from the structural relationship declarations in beneficiaries/victims and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies how mandatrophy resolution prevents false lumping. The initial temptation is to classify the entire constraint as either 'pure coordination for collective climate action' (Rope) or 'pure extraction by green industries and climate institutions' (Snare). The mandatrophy is: how can both be true simultaneously? The resolution is perspectival: the constraint IS Rope from the analytical observer's civilizational stance (genuine coordination problem, justified mobilization) AND Snare from the trapped worker's perspective (pure extraction, zero exit) AND Tangled Rope from the regulatory perspective (mixed coordination and extraction, constrained exit) AND Scaffold from the climate coalition perspective (temporary, with sunset once decarbonization matures) AND Piton from the science establishment perspective (degraded institutional theater). The mandatrophy is not resolved by choosing one type; it is resolved by recognizing that the constraint's true structure is the presheaf of all six perspectives. The catastrophe belief constraint is neither purely coordination nor purely extraction — it is a hybrid that different structural positions experience differently. The false summit at the analytical mountain level is detected by the fact that ε = 0.62 and suppression = 0.58 violate the mountain threshold (ε ≤ 0.25, suppression ≤ 0.05); the constraint cannot be a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climate_model_accuracy_horizon,
    'At what prediction horizon do climate models transition from scientifically reliable to highly speculative?',
    'Longitudinal validation: compare published climate predictions from 1990-2010 against observed outcomes; quantify forecast skill decay as function of prediction lead time; assess whether catastrophe-level events were predicted or post-hoc narratives',
    'If models are reliable to 50+ years: catastrophe belief is coordinated on scientific reality. If models degrade sharply after 20-30 years: catastrophe belief extends into speculation range, reducing evidence quality (ε increases toward 0.75, snare classification strengthens across more perspectives). If prediction skill was primarily retrospective (post-hoc fitting): belief mechanism is therapeutic rather than epistemic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_model_accuracy_horizon, empirical, 'Climate model reliability as function of prediction horizon').

omega_variable(
    psychological_harm_quantification,
    'Does the catastrophe belief produce measurable psychological harm (clinical anxiety, depression, reduced reproductive intention, suicide ideation) beyond baseline environmental concern?',
    'Longitudinal cohort studies comparing anxiety metrics between high-catastrophe-belief and low-belief populations controlling for socioeconomic status, exposure to extreme weather, and baseline neuroticism; mediation analysis to isolate belief contribution from actual climate events',
    'If harm is substantial and belief-mediated: snare classification for powerless agents is confirmed (high extraction). If harm is minimal or primarily reactive to observed weather: extraction is overstated, rope classification gains weight (coordination benefit outweighs psychological cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psychological_harm_quantification, empirical, 'Psychological harm attributable to catastrophe belief').

omega_variable(
    policy_distortion_magnitude,
    'What share of current climate policy (carbon pricing, renewable mandates, fossil fuel bans) is optimally calibrated to climate risk vs. driven by catastrophe-belief-induced policy capture?',
    'Cost-benefit analysis of major climate policies using consistent damage function; comparison against benefit-cost analyses by mainstream economic models (DICE, PAGE); identification of policies with benefit-cost ratios < 1.0 when risk discounting is applied at standard rates',
    'If most policies are benefit-justified: extraction is low, rope classification dominates (coordination for legitimate risk mitigation). If substantial share of policies fail standard benefit-cost tests: extraction is high, tangled rope and snare gain strength (coordination coexists with rent-seeking by green sectors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_distortion_magnitude, empirical, 'Policy efficiency relative to climate risk assessment').

omega_variable(
    tipping_point_prediction_accuracy,
    'Have published predictions of specific climate tipping points (Amazon dieback, ice sheet collapse, Atlantic meridional overturning circulation shutdown) been validated, delayed, or rendered spectral (never definitively confirmed or refuted)?',
    'Literature review of tipping point predictions from 2005-2025; categorize outcomes as: confirmed, falsified, delayed with extended prediction, or indeterminate; assess whether failed predictions updated scientific consensus or entrenched in catastrophe narrative',
    'If predictions are validated: catastrophe belief is evidence-backed coordination. If predictions are repeatedly delayed or indeterminate: belief persists despite weak evidence, suggesting theater and institutional inertia (piton strengthens). If failed predictions are suppressed in mainstream communication: suppression metric increases, snare classification strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tipping_point_prediction_accuracy, empirical, 'Validation status of specific climate catastrophe predictions').

omega_variable(
    coordination_efficiency_of_catastrophe_framing,
    'Is the catastrophe framing necessary for climate action mobilization, or would equivalent decarbonization investment occur with moderate-risk framing?',
    'Natural experiment analysis: compare investment trajectories and policy adoption across jurisdictions with high vs. moderate catastrophe emphasis (e.g., Scandinavian pragmatism vs. Californian catastrophism); assess whether outcome efficiency differs; survey policy decision-makers on counterfactual: would moderate framing have produced same investment?',
    'If catastrophe framing is necessary: coordination benefit is high, rope classification confirmed (necessary coercion for solving collective action problem). If moderate framing would produce equivalent outcomes: catastrophe framing is theater and rent-seeking, extraction increases (tangled rope and snare gain strength).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_efficiency_of_catastrophe_framing, conceptual, 'Whether catastrophe framing is necessary for climate action').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_catastrophe_belief, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccb_tr_t0, climate_catastrophe_belief, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ccb_tr_t10, climate_catastrophe_belief, theater_ratio, 10, 0.52).
narrative_ontology:measurement(ccb_tr_t20, climate_catastrophe_belief, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(ccb_be_t0, climate_catastrophe_belief, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ccb_be_t10, climate_catastrophe_belief, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(ccb_be_t20, climate_catastrophe_belief, base_extractiveness, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_catastrophe_belief, information_standard).
narrative_ontology:affects_constraint(climate_catastrophe_belief, green_technology_rent_seeking).
narrative_ontology:affects_constraint(climate_catastrophe_belief, fossil_fuel_stranded_assets).
narrative_ontology:affects_constraint(climate_catastrophe_belief, climate_policy_capture).

% DUAL FORMULATION NOTE:
% The catastrophe belief constraint is downstream of climate science claims (which have their own ε and classification) and upstream of specific policy implementations (carbon pricing, renewable mandates, divestment campaigns). The belief itself is a distinct constraint operating at the psychological/institutional level, separate from the underlying physical climate reality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
