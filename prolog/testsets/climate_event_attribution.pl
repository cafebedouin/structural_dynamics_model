% ============================================================================
% CONSTRAINT STORY: climate_event_attribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_event_attribution, []).

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
 *   constraint_id: climate_event_attribution
 *   human_readable: Scientific methodology for attributing extreme weather to climate change
 *   domain: scientific/political
 *
 * SUMMARY:
 *   Event attribution science — the methodology for assigning specific
 *   extreme weather events to anthropogenic climate change — has evolved from
 *   a specialized statistical technique into a powerful tool for climate
 *   policy advocacy. Over the past decade, the field has become entangled
 *   with political and institutional incentives that push attribution
 *   confidence beyond what the underlying methodology can fully justify. The
 *   constraint exhibits characteristics of both genuine coordination
 *   (standardized attribution frameworks enable insurance, policy design, and
 *   risk assessment) and asymmetric extraction (institutional pressures
 *   suppress methodological skepticism, enforce higher confidence claims than
 *   justified, and marginalize alternative frameworks). The theater_ratio has
 *   risen from 0.35 to 0.65, indicating that institutional performance of
 *   certainty has outpaced actual methodological capability. Extractiveness
 *   has increased from 0.28 to 0.52, reflecting growing asymmetry between
 *   those benefiting from strong attribution claims (climate institutions,
 *   policy advocates) and those bearing the cost of suppressed methodological
 *   caution (skeptical researchers, statistical purists). This is
 *   fundamentally a Tangled Rope: it solves a real coordination problem (how
 *   to make climate science policy-relevant) while simultaneously extracting
 *   from skeptical voices and constraining scientific discourse.
 *
 * KEY AGENTS:
 *   - Climate Research Institutions: Primary beneficiary (institutional/arbitrage) — control methodology, funding, and publication standards; benefit from policy relevance and visibility of attribution work
 *   - Skeptical Researchers: Primary victim (powerless/trapped) — face career penalties for expressing methodological doubt; no exit without reputational cost
 *   - Policy Advocates and Climate Coalitions: Secondary beneficiary (organized/constrained) — use attribution as evidence for intervention; exert pressure on science to support stronger claims
 *   - Insurance and Risk Assessment Community: Constrained beneficiary (moderate/constrained) — benefits from standardized attribution methodology but forced to adopt frameworks that may overstate confidence
 *   - Statistical Gatekeepers: Institutional observer (institutional/constrained) — maintain classical statistical frameworks that persist through inertia rather than function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional asymmetries as physical limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_event_attribution, 0.52).
domain_priors:suppression_score(climate_event_attribution, 0.48).
domain_priors:theater_ratio(climate_event_attribution, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_event_attribution, extractiveness, 0.52).
narrative_ontology:constraint_metric(climate_event_attribution, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(climate_event_attribution, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_event_attribution, tangled_rope).
narrative_ontology:human_readable(climate_event_attribution, "Scientific methodology for attributing extreme weather to climate change").
narrative_ontology:topic_domain(climate_event_attribution, "scientific/political").

domain_priors:requires_active_enforcement(climate_event_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_event_attribution, climate_research_institutions).
narrative_ontology:constraint_beneficiary(climate_event_attribution, policy_advocates_for_climate_action).
narrative_ontology:constraint_victim(climate_event_attribution, scientific_caution_norms).
narrative_ontology:constraint_victim(climate_event_attribution, skeptical_research_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SKEPTICAL RESEARCHER (SNARE) — Caught in a system where expressing methodological doubt about attribution confidence levels carries career and reputational penalties. Publishing work that emphasizes uncertainty margins or challenges attribution certainty is systematically suppressed through peer review gatekeeping, editorial bias, and social pressure from dominant institutions. No exit without career cost — trapped in either complicity or marginalization.
constraint_indexing:constraint_classification(climate_event_attribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSURANCE/RISK ASSESSMENT (TANGLED ROPE) — Constrained by regulatory requirements to use climate attribution frameworks for pricing and reserve decisions, but also benefits from the standardized methodology that enables actuarial modeling at scale. Extraction is asymmetric: must adopt attribution models (enforcement) but gains coordination value through standardized risk assessment tools. Constrained exit — cannot ignore attribution without regulatory violation.
constraint_indexing:constraint_classification(climate_event_attribution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CLIMATE RESEARCH INSTITUTIONS (ROPE) — Primary beneficiary. Controls attribution methodology, funding allocation, and publication gateways. Experiences the constraint as coordination that enables large-scale climate science enterprise. Benefits from first-mover advantage in methodology development and funding prioritization. High arbitrage capacity — can shift methodological standards to favor their frameworks.
constraint_indexing:constraint_classification(climate_event_attribution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POLICY ADVOCATES (TANGLED ROPE) — Organized agents using attribution science as evidence for climate policy. Require strong attribution claims to justify intervention. Extract coordination value from science-policy linkage but also enforce tighter certainty standards than pure science demands. Constrained by need to maintain scientific credibility — cannot push attribution claims beyond what institutions will defend.
constraint_indexing:constraint_classification(climate_event_attribution, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: STATISTICAL CAUSALITY GATEKEEPERS (PITON) — Traditional frequentist statistical frameworks (p-values, significance thresholds, confidence intervals) have become institutionalized gatekeepers for attribution claims, yet they perform diminished function. Modern attribution uses Bayesian methods, ensemble models, and process-based reasoning that exceed classical statistical inference. The classical framework persists through institutional inertia in journal review standards and educational curricula despite lower functional relevance. Theater_ratio high: much publication effort devoted to satisfying classical thresholds that no longer capture the real verification mechanism.
constraint_indexing:constraint_classification(climate_event_attribution, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, attribution of single events to climate change faces irreducible limits due to chaos theory and the stochastic nature of weather: individual events are always partly noise, and the signal-to-noise ratio for single-event attribution has a floor set by physics. No observational methodology can violate this limit. However, the structural data suggests this is a false summit — the real constraint is not the physical limit (which is well-understood and symmetric) but the asymmetric incentive structure that pushes attribution claims beyond what epistemically justified.
constraint_indexing:constraint_classification(climate_event_attribution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_event_attribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_event_attribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_event_attribution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_event_attribution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_event_attribution, TR),
    TR >= 0.70.

:- end_tests(climate_event_attribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. Climate attribution science has moved from a cautious statistical endeavor (epsilon ≈ 0.28 in interval start) toward increasingly confident claims about event attribution. The increase is driven by both genuine methodological advances (ensemble methods, Bayesian frameworks, process understanding) and institutional incentives that encourage stronger claims. The 0.52 value reflects the current state where methodological capability supports moderate confidence claims but institutional pressure often generates higher confidence statements. Suppression (0.48): Moderate. Significant barriers to expressing methodological doubt include peer review gatekeeping that favors strong attribution claims, funding concentration in climate institutions with attribution focus, and social pressure from policy advocates who require strong science. However, suppression is not total — skeptical voices persist in journals, workshops, and some institutional contexts. Theater_ratio (0.65): High and rising. Much of the institutional effort in attribution goes to validating methodology (publications demonstrating attribution feasibility) and building confidence statements that satisfy policy audiences. The actual epistemic mechanism — understanding whether observed events would occur in the same form without climate change — is obscured by ensemble performance metrics, statistical thresholds, and replication studies that perform function while consuming theater.
 *
 * PERSPECTIVAL GAP:
 *   Climate research institutions see attribution as enabling and coordination framework that makes climate science policy-relevant (Rope). Policy advocates see it as partially constrained by scientific caution but essential for justifying intervention (Tangled Rope). Skeptical researchers see it as a system suppressing methodological doubt and enforcing claims beyond justified confidence (Snare). Risk assessors see it as both coordination mechanism and forced adoption of potentially overconfident standards (Tangled Rope). Statistical purists see classical frameworks persisting through inertia in review gatekeeping despite their diminished role (Piton). The civilizational analytical observer risks treating the stochastic limits of event attribution as immutable natural law (Mountain), when the real constraint is institutional asymmetry in how those limits are applied. The perspectival gap reveals that the constraint is not the inherent difficulty of attribution (which is symmetric) but the asymmetric incentive to claim success despite the difficulty.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness depends on their power level, exit capacity, and structural relationship to the attribution framework. Climate institutions have institutional power and high exit capacity (arbitrage) — they can shape attribution standards and move between frameworks. They experience low or negative effective extraction because they shape the constraint. Skeptical researchers have powerless position and trapped exit — they cannot publish strong methodological criticism without career cost. They experience high effective extraction. Policy advocates have organized power and constrained exit — they need strong science claims but are constrained by scientific credibility requirements. They experience moderate extraction because they have agency but limited freedom. Skeptical researchers bear maximum chi because their exit options are minimal and their power is low. Climate institutions experience near-zero or negative chi because they benefit from and shape the constraint. The directionality pipeline captures these differences through the d derivation: beneficiaries get low d (benefit side), victims get high d (extraction side), and the sigmoid f(d) converts d to experienced power modifier.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by acknowledging that the genuine coordination function (standardized attribution enabling policy design) and the asymmetric extraction (suppressing skepticism to maintain strong claims) are structurally entangled. The constraint is neither pure coordination nor pure extraction — it is a Tangled Rope where the coordination function is real but subsidizes extraction. The false summit risk occurs in the mountain perspective: treating the stochastic limits of event attribution as natural law masks the institutional choices about how those limits are applied. The piton classification is diagnostic: classical statistical frameworks persist in review standards despite diminished functional role, and the theater_ratio rise from 0.35 to 0.65 indicates that validating methodology has become substituted for actual scientific work. The policy community's stake in strong attribution creates pressure to solve the theater via institutional performance (better ensemble metrics, more publications) rather than fundamental methodological breakthrough. This is the signature of a constraint approaching degradation toward piton status — the functional coordination (attribution enabling policy) persists while institutional theater (publication metrics, confidence statements) becomes the real product.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attribution_confidence_threshold,
    'What confidence threshold (65%, 75%, 90%) for event attribution should trigger policy decision-making versus scientific publishing?',
    'Historical analysis of attribution-based policies and outcomes; comparison of claimed confidence intervals with subsequent verification; post-hoc analysis of decision quality at different thresholds',
    'If threshold too low (65%): policies enacted on weak evidence, reputational damage when updated science revises attributions. If threshold too high (90%): policy delayed beyond optimal intervention window; extracted coordination value lost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_confidence_threshold, preference, 'Confidence threshold for policy-relevant attribution claims').

omega_variable(
    counterfactual_world_epistemology,
    'Can the ''counterfactual no-warming world'' used in attribution studies be sufficiently well-defined to support causal claims about observed events?',
    'Comparison of different counterfactual construction methods (statistical residualization, ensemble member selection, parameter variation); sensitivity analysis of attribution results to counterfactual specification; expert consensus on epistemological status of counterfactuals',
    'If counterfactual is epistemically sound: attribution rests on solid causal inference foundations. If fundamentally underdetermined: attribution claims exceed what methodology can support, and extraction mechanism is clearer (enforcement of claims beyond justification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counterfactual_world_epistemology, conceptual, 'Epistemological status of counterfactual worlds in attribution').

omega_variable(
    observational_system_bias,
    'Do institutional interests in climate attribution generate systematic bias in model selection, tuning parameters, and ensemble composition that inflates attribution confidence?',
    'Blind model selection studies; comparison of attribution results across groups with different funding incentives; analysis of parameter choices with opposite institutional incentives; meta-analysis of published confidence intervals vs uncertainty ranges from sensitivity studies',
    'If bias detected: suppression mechanism is real and structurally entrenched. If no systematic bias: extraction is weaker than structure suggests; field has self-correcting mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observational_system_bias, empirical, 'Whether institutional interests bias attribution methodology').

omega_variable(
    single_event_physical_limits,
    'What is the irreducible lower bound on attribution signal-to-noise ratio for individual weather events given atmospheric chaos and incomplete process knowledge?',
    'Theoretical analysis of information-theoretic limits; ensemble chaos experiments; comparison of attribution methodology against perfect-model physics simulations with known forcing',
    'If physical limit is near current methodology''s claims: constraint is natural law (mountain). If physical limit is well below current claims: institutional choices drive the gap, not physics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(single_event_physical_limits, empirical, 'Physical limits on single-event attribution fidelity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_event_attribution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cea_tr_t0, climate_event_attribution, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cea_tr_t5, climate_event_attribution, theater_ratio, 5, 0.5).
narrative_ontology:measurement(cea_tr_t10, climate_event_attribution, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(cea_be_t0, climate_event_attribution, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cea_be_t5, climate_event_attribution, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(cea_be_t10, climate_event_attribution, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_event_attribution, information_standard).
narrative_ontology:affects_constraint(climate_event_attribution, climate_policy_intervention_threshold).
narrative_ontology:affects_constraint(climate_event_attribution, climate_scenario_ensemble_construction).

% DUAL FORMULATION NOTE:
% Event attribution has two structurally distinct constraint stories: (1) the fundamental challenge of attributing single events to climate change given chaos and stochasticity (epistemological problem with physical floor), and (2) the institutional and incentive structure that pushes attribution confidence beyond what methodology justified (institutional extraction problem). These have different epsilon values and different classifications. This story focuses on the institutional extraction structure. The epistemological floor is a separate constraint downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_event_attribution, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
