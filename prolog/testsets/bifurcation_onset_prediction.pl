% ============================================================================
% CONSTRAINT STORY: bifurcation_onset_prediction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bifurcation_onset_prediction, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bifurcation_onset_prediction
 *   human_readable: Bifurcation Onset Prediction in Complex Systems
 *   domain: complex_systems/dynamical_systems/applied_mathematics
 *
 * SUMMARY:
 *   Bifurcation onset prediction represents a structural constraint on system
 *   operators, policymakers, and populations dependent on forecasting
 *   frameworks for critical infrastructure management. The constraint
 *   operates across multiple domains (climate science, financial regulation,
 *   power grid management, epidemiology) where institutions have adopted
 *   bifurcation-based onset prediction as the authoritative framework for
 *   detecting system state transitions. The constraint exhibits high
 *   extractiveness (0.68) because the framework creates asymmetric
 *   dependencies: operators become trapped in reliance on predictions that
 *   have limited forward-predictive power; beneficiaries (modeling industry,
 *   regulatory agencies) capture authority and legitimacy; the framework
 *   itself resists empirical falsification through post-hoc explanation of
 *   failed predictions. The theater ratio (0.65) reflects that bifurcation
 *   frameworks excel at explaining historical transitions retroactively but
 *   show weak performance on prospective prediction of novel bifurcations.
 *   The gap between explanatory power and predictive power is suppressed
 *   through institutional framing: failures are attributed to 'incomplete
 *   initial conditions' or 'exogenous shocks' rather than framework
 *   limitations. Suppression (0.72) operates through multiple channels:
 *   alternatives are dismissed as 'unscientific' or 'ad-hoc'; stochastic
 *   noise is downplayed despite dominating real-world system dynamics;
 *   regulatory mandates lock operators into bifurcation-based forecasting;
 *   the mathematics of bifurcation theory is canonicalized in textbooks,
 *   creating path dependency for credentialing and institutional legitimacy.
 *
 * KEY AGENTS:
 *   - System Operators (powerless/trapped): Dependent on bifurcation predictions for critical infrastructure management; bear full cost of prediction failures; no structurally viable exit options
 *   - Affected Populations (moderate/constrained): Subject to consequences of bifurcation-driven policy and infrastructure failures; constrained migration/adaptation options; externalized costs of prediction errors
 *   - Computational Modeling Industry (institutional/arbitrage): Primary beneficiary; sells prediction services and modeling platforms; experiences constraint as coordination enabling market capture; maintains exit options via regulatory arbitrage
 *   - Regulatory Agencies (organized/constrained): Mandated to forecast bifurcations; benefit from legitimacy of prediction framework; constrained by public expectation of forecasting capacity; bear reputational cost of prediction failures
 *   - Mathematical Physics Establishment (institutional/arbitrage): Maintains institutional authority through bifurcation theory canonicity; benefits from textbook dominance and credentialing control; sees framework as natural mathematical description
 *   - Early Warning Systems Research (organized/mobile): Organized alternative research community building independent early warning frameworks; has mobile exit options via alternative institutional pathways; represents sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bifurcation_onset_prediction, 0.68).
domain_priors:suppression_score(bifurcation_onset_prediction, 0.72).
domain_priors:theater_ratio(bifurcation_onset_prediction, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bifurcation_onset_prediction, extractiveness, 0.68).
narrative_ontology:constraint_metric(bifurcation_onset_prediction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bifurcation_onset_prediction, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bifurcation_onset_prediction, snare).
narrative_ontology:human_readable(bifurcation_onset_prediction, "Bifurcation Onset Prediction in Complex Systems").
narrative_ontology:topic_domain(bifurcation_onset_prediction, "complex_systems/dynamical_systems/applied_mathematics").

domain_priors:requires_active_enforcement(bifurcation_onset_prediction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bifurcation_onset_prediction, computational_modeling_industry).
narrative_ontology:constraint_beneficiary(bifurcation_onset_prediction, regulatory_agencies_using_predictive_models).
narrative_ontology:constraint_victim(bifurcation_onset_prediction, system_operators_dependent_on_predictions).
narrative_ontology:constraint_victim(bifurcation_onset_prediction, populations_subject_to_system_failure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYSTEM OPERATOR (SNARE) — Operator of critical infrastructure (power grids, climate systems, financial networks) depends on bifurcation predictions but has no alternative framework. When predictions fail, operator bears full cost (system collapse, loss of life). Cannot exit dependence on prediction models; trapped by institutional requirement to forecast. High suppression — alternatives are systematically suppressed as 'unscientific' or 'unmeasurable'.
constraint_indexing:constraint_classification(bifurcation_onset_prediction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AFFECTED POPULATION (SNARE) — Populations affected by bifurcations (financial crisis, climate tipping points, power grid failure) have constrained exit. Can migrate or adapt at high cost, but escape is not available for most. Extraction runs toward this group — their resources, labor, and welfare are expended managing consequences of prediction failures. Theater ratio drives suppression: early warning narratives are performative, creating false confidence in system stability.
constraint_indexing:constraint_classification(bifurcation_onset_prediction, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPUTATIONAL MODELING INDUSTRY (ROPE) — Benefits substantially from bifurcation prediction frameworks. Sells modeling services, maintains institutional role as 'expert forecaster'. Experiences the constraint as pure coordination — the framework legitimizes their authority and enables market capture. Arbitrage exit options: can package model outputs for different regulatory regimes. Net beneficiary.
constraint_indexing:constraint_classification(bifurcation_onset_prediction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AGENCIES (TANGLED ROPE) — Agencies mandated to forecast and prevent bifurcations benefit from having a prediction framework (coordination function), but are also constrained by the framework's limitations. When predictions fail, agencies bear reputational and legal cost. Constrained exit: cannot simply abandon bifurcation framework because public expects regulation; cannot acknowledge prediction limits without losing legitimacy. Mixed experience: genuine coordination of risk-aware policy with embedded extraction of agency credibility.
constraint_indexing:constraint_classification(bifurcation_onset_prediction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MATHEMATICAL PHYSICS ESTABLISHMENT (PITON) — Bifurcation onset framework has been canonical in dynamical systems theory for 40+ years. The theater ratio is high because the framework is largely descriptive after-the-fact rather than predictive before-the-fact. Researchers use bifurcation theory to explain historical transitions (e.g., phase transitions, climate shifts) but prediction performance on novel systems remains weak. The framework persists through institutional inertia and textbook canonicity despite limited forward predictive power. Theater sustains the constraint rather than functional necessity.
constraint_indexing:constraint_classification(bifurcation_onset_prediction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a universal/civilizational perspective, bifurcations themselves (as mathematical phenomena) are natural — all dynamical systems with nonlinearity exhibit qualitative state changes. The onset of bifurcation is a structural feature of complex systems, not a contingent institutional constraint. From this view, bifurcation onset prediction is discovering an intrinsic property of nature. However, the structural data (high extractiveness, high suppression, high theater) indicates this perspective naturalizes a contingent institutional arrangement — the framework's real predictive limits are masked by the frame of 'discovering natural laws'.
constraint_indexing:constraint_classification(bifurcation_onset_prediction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: EARLY WARNING SYSTEMS COMMUNITY (SCAFFOLD) — Distributed research on alternative bifurcation detection methods (critical slowing down indicators, complexity measures, machine learning on microstates) represents sunset logic. These methods are building independent verification pathways that do not rely on the classical bifurcation framework. As alternative methods mature and accumulate predictive success, the classical framework's monopoly on 'legitimate' prediction erodes. Organized agents (university labs, open-source projects) have mobile exit options — can migrate to alternative frameworks. Sunset clause: 15-25 years for alternatives to establish institutional legitimacy.
constraint_indexing:constraint_classification(bifurcation_onset_prediction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bifurcation_onset_prediction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bifurcation_onset_prediction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bifurcation_onset_prediction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bifurcation_onset_prediction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bifurcation_onset_prediction, TR),
    TR >= 0.70.

:- end_tests(bifurcation_onset_prediction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts substantial value from operators and affected populations through mandatory dependence on limited-predictive-power frameworks. The modeling industry captures profit from prediction services; regulatory agencies capture legitimacy from 'scientific forecasting'; the mathematical physics establishment captures institutional authority through theoretical canonicity. The extraction is not maximal (0.95) because alternatives do exist and are accumulating evidence, and some prediction accuracy is genuine rather than purely theater. The interval trajectory (0.52→0.68) shows extraction accumulating over time as frameworks become more institutionalized and alternatives face higher barriers to entry. Suppression (0.72): High. Multiple suppression mechanisms: (1) Alternatives are systematically dismissed ('unscientific,' 'lacks theoretical grounding'). (2) Stochastic noise—the primary control mechanism in real systems—is treated as 'correction term' rather than central to dynamics. (3) Prediction failures are retroactively reframed as 'incomplete initial conditions' rather than framework limitations. (4) Regulatory lock-in creates legal barriers to alternative frameworks. (5) Textbook canonicity creates path dependency in credentialing, forcing new researchers through bifurcation theory gatekeeping. Theater ratio (0.65): Moderate-high. Bifurcation frameworks demonstrate strong explanatory power for historical transitions (phase transitions, climate shifts, financial crises analyzed post-hoc) but weak prospective predictive power (~40% accuracy on novel systems). The institutional narrative elides this gap through rhetoric: 'We discovered the mechanism of X' becomes 'We can predict X.' The performative content increases over the interval (0.40→0.65) as institutions invest more heavily in prediction narratives despite stagnant accuracy improvements.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence on a single structural system. The modeling industry sees pure coordination (Rope)—solving the legitimate problem of forecasting critical transitions. System operators see pure extraction (Snare)—trapped dependence on frameworks with limited predictive power. Regulatory agencies see hybrid extraction-coordination (Tangled Rope)—genuine forecasting benefits mixed with legitimacy capture. The early warning community sees a sunset problem with alternatives emerging (Scaffold). The mathematical establishment sees degraded institutional theater maintaining a framework whose functional predictive role has atrophied (Piton). The civilizational observer risks seeing immutable natural law (Mountain)—bifurcations are inherent to nonlinear systems—but structural metrics reveal false summit: the constraint's extractive power derives from human institutional choices (canonicity, credentialing, regulatory mandate), not from mathematics. The perspectival gap is productive—it reveals that calling bifurcation 'natural law' or 'mathematical discovery' masks institutional extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to extraction flow. System operators are trapped—high d, high chi, snare classification. Modeling industry benefits (d ≈ 0.10, low chi) and sees rope. Regulatory agencies occupy mixed position: they enforce the framework (beneficiary role, low d) but also bear costs when it fails (victim role, high d). The tangled_rope classification reflects this hybridity—constrained exit despite benefits. The analytical observer's mountain perspective risks naturalizing the contingency: bifurcations are real mathematical phenomena, but the predictive framework's limitations are human institutional arrangements, not natural laws. The early warning community has mobile options (d ≈ 0.35) because they can credibly claim alternative frameworks as their institutional home. The mathematical establishment maintains arbitrage (d ≈ 0.05) through textbook monopoly.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION MECHANISM: The mandatrophy is resolved by explicitly decomposing 'bifurcation onset prediction' into two structurally distinct claims: (1) Bifurcations as mathematical phenomena are real and have onset dynamics (this is Mountain-like in mathematical logic). (2) Bifurcation frameworks are empirically predictive of future real-world transitions (this is Snare-like operationally). Claim 1 naturalizes as mathematical truth; Claim 2 extracts institutional authority from operationalizing Claim 1 without demonstrating forward-predictive validity. The framework suppresses awareness of the claim boundary by collapsing them: 'bifurcations are mathematical realities → bifurcation models predict real transitions.' Early warning research creates alternative decompositions: bifurcations-as-mathematical-phenomena are preserved; alternative frameworks (critical slowing down, complexity measures) replace predictive role. This permits Claim 1 (mathematical) to remain canonical while Claim 2 (predictive) migrates to alternative institutions. The constraint's extractiveness derives from Claim 2's institutional monopoly. As alternatives accumulate evidence and institutional legitimacy, Claim 2 loses monopoly status and extractiveness drops. The theater ratio tracks this transition: as alternatives mature, bifurcation frameworks are increasingly recognized as explanatory-retrospective rather than predictive-prospective, and the framework's institutional theater becomes visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prediction_vs_explanation_ambiguity,
    'Is bifurcation onset framework primarily a mathematical explanation of observed transitions or a predictive framework for future bifurcations?',
    'Prospective comparison: prediction accuracy of bifurcation frameworks on novel systems vs retroactive explanation accuracy on historical transitions. Quantify the forward/backward asymmetry.',
    'If primarily explanatory: framework is misclassified as predictive; extractiveness drops to 0.35 (Tangled Rope). If primarily predictive: framework should demonstrate >70% accuracy on novel holdout systems — current literature shows ~40% accuracy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prediction_vs_explanation_ambiguity, empirical, 'Whether framework functions as prediction or post-hoc explanation').

omega_variable(
    early_warning_alternative_viability,
    'Do alternative early warning frameworks (critical slowing down, variance scaling, entropy rate) provide independent predictive signal or are they mathematically isomorphic to bifurcation onset indicators?',
    'Information-theoretic analysis of prediction signal correlation; test on systems where classical bifurcation prediction fails but alternatives succeed (or vice versa). Identify structural independence.',
    'If truly independent: scaffold sunset is real, institutional monopoly will erode. If isomorphic: alternatives are merely reparametrizations, scaffold perspective is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(early_warning_alternative_viability, empirical, 'Whether alternative early warning methods are structurally independent').

omega_variable(
    tipping_point_vs_bifurcation_conflation,
    'Are ''tipping points'' and ''bifurcation onsets'' distinct phenomena or rhetorical substitutes that serve different institutional purposes?',
    'Linguistic and institutional analysis: trace adoption of ''tipping point'' language in policy vs ''bifurcation'' language in mathematical physics. Identify which communities use which frame and for what institutional purpose. Cross-reference with prediction success rates.',
    'If distinct: two separate constraints need separate stories. If rhetorical substitutes: the frame choice drives institutional authority allocation (mathematical physics claims expertise; policy communication employs tipping point narrative). High impact on understanding suppression mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tipping_point_vs_bifurcation_conflation, conceptual, 'Whether tipping points and bifurcations are distinct or rhetorical variants').

omega_variable(
    suppression_of_stochasticity,
    'How much does the deterministic bifurcation framework suppress recognition of stochastic noise as a primary control mechanism in real systems?',
    'Empirical comparison: systems with low noise where bifurcation prediction works vs high-noise systems where it fails. Quantify noise sensitivity. Analyze whether prediction frameworks explicitly incorporate stochastic forcing.',
    'If stochasticity is primary: framework extractiveness should increase to 0.78+ (Snare with aggressive suppression). If secondary: current 0.72 is appropriate. High impact on understanding why operators remain trapped despite prediction framework limitations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_of_stochasticity, empirical, 'Degree to which stochastic noise suppression mechanisms mask prediction limitations').

omega_variable(
    regulatory_mandate_lock_in,
    'Are regulatory agencies structurally locked into bifurcation prediction frameworks by statute/policy, or do they maintain voluntary dependence due to absence of certified alternatives?',
    'Regulatory audit: identify statutes, policies, and agency procedures that mandate bifurcation-based forecasting vs those that permit alternative frameworks. Document transition costs if agencies were to adopt alternatives.',
    'If statutory lock-in: agencies are victims (trapped), not mere beneficiaries. Extractiveness should be weighted toward agency capture rather than mutual benefit. If voluntary: agencies are partially responsible for maintaining the constraint and benefit from its legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_mandate_lock_in, empirical, 'Degree of legal/institutional lock-in to bifurcation prediction frameworks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bifurcation_onset_prediction, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bifp_tr_t0, bifurcation_onset_prediction, theater_ratio, 0, 0.4).
narrative_ontology:measurement(bifp_tr_t15, bifurcation_onset_prediction, theater_ratio, 15, 0.58).
narrative_ontology:measurement(bifp_tr_t30, bifurcation_onset_prediction, theater_ratio, 30, 0.65).
narrative_ontology:measurement(bifp_tr_t10, bifurcation_onset_prediction, theater_ratio, 10, 0.5).

% Extraction over time
narrative_ontology:measurement(bifp_be_t0, bifurcation_onset_prediction, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(bifp_be_t15, bifurcation_onset_prediction, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(bifp_be_t30, bifurcation_onset_prediction, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(bifp_be_t10, bifurcation_onset_prediction, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bifurcation_onset_prediction, information_standard).
narrative_ontology:boltzmann_floor_override(bifurcation_onset_prediction, 0.08).
narrative_ontology:affects_constraint(bifurcation_onset_prediction, climate_tipping_point_prediction).
narrative_ontology:affects_constraint(bifurcation_onset_prediction, financial_systemic_risk_detection).
narrative_ontology:affects_constraint(bifurcation_onset_prediction, power_grid_stability_forecasting).

% DUAL FORMULATION NOTE:
% Bifurcation onset prediction is upstream of domain-specific constraint stories (climate, finance, power systems) that apply the framework. Each downstream constraint inherits the base framework's extractiveness but may adjust based on domain-specific institutional factors. Network decomposition reflects framework application variance: same mathematical framework produces different institutional outcomes depending on operator structure, regulatory environment, and availability of domain-specific alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bifurcation_onset_prediction, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
