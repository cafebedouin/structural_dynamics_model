% ============================================================================
% CONSTRAINT STORY: china_taiwan_reunification_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_taiwan_reunification_mandate, []).

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
 *   constraint_id: china_taiwan_reunification_mandate
 *   human_readable: The Mandate of National Rejuvenation (PRC Perspective)
 *   domain: political
 *
 * SUMMARY:
 *   From the perspective of the People's Republic of China (PRC),
 *   reunification with Taiwan is not a strategic choice but a historical
 *   necessity and a "core interest" integral to the "Great Rejuvenation of
 *   the Chinese Nation" by the 2049 centenary. The commitment to
 *   reunification is deeply embedded in the CCP's ideology and national
 *   narrative, presenting it as a non-negotiable imperative.
 *
 * KEY AGENTS:
 *   - Chinese Communist Party: Primary beneficiary (institutional/constrained). Benefits from the perceived legitimacy and internal cohesion derived from pursuing national rejuvenation.
 *   - Taiwanese Autonomy: Primary victim (powerless/trapped). Faces existential threat due to the PRC's claims of sovereignty and military pressure.
 *   - International Community: Secondary victim (moderate/constrained). Constrained by economic ties with China and the desire to avoid military conflict, but also bound by norms of sovereignty.
 *   - Mainland China National Identity: Beneficiary (institutional/constrained) The concept of national rejuvenation reinforces mainland China's national identity. 
 *   - Regional Stability: Secondary victim (powerful/constrained). Regional players are powerful yet are constrained by military actions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_taiwan_reunification_mandate, 0.75).
domain_priors:suppression_score(china_taiwan_reunification_mandate, 0.85).
domain_priors:theater_ratio(china_taiwan_reunification_mandate, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_taiwan_reunification_mandate, extractiveness, 0.75).
narrative_ontology:constraint_metric(china_taiwan_reunification_mandate, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(china_taiwan_reunification_mandate, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_taiwan_reunification_mandate, snare).
narrative_ontology:human_readable(china_taiwan_reunification_mandate, "The Mandate of National Rejuvenation (PRC Perspective)").
narrative_ontology:topic_domain(china_taiwan_reunification_mandate, "political").

domain_priors:requires_active_enforcement(china_taiwan_reunification_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_taiwan_reunification_mandate, chinese_communist_party).
narrative_ontology:constraint_beneficiary(china_taiwan_reunification_mandate, mainland_china_national_identity).
narrative_ontology:constraint_victim(china_taiwan_reunification_mandate, taiwanese_autonomy).
narrative_ontology:constraint_victim(china_taiwan_reunification_mandate, taiwanese_national_identity).
narrative_ontology:constraint_victim(china_taiwan_reunification_mandate, regional_stability).
narrative_ontology:constraint_victim(china_taiwan_reunification_mandate, international_norms_of_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Taiwanese autonomy is trapped by the PRC's claims of sovereignty and military pressure, facing existential threat. They cannot unilaterally alter the status quo without risking military intervention, thus extraction is maximized.
constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% The international community (particularly Western democracies) finds itself constrained by economic ties with China and a desire to avoid direct military conflict, but also bound by norms of sovereignty. The extraction is reflected in the degradation of international law/norms.
constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The CCP benefits from the mandate of reunification by bolstering its legitimacy and consolidating power, but is simultaneously constrained by the potential economic and social costs of military action. The coordination benefit and asymmetric extraction make it a tangled rope.
constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% Regional stability is trapped, as a potential conflict over Taiwan would have catastrophic consequences for the entire region. This classifies as a snare, as regional actors are powerless to prevent a conflict should the CCP choose to pursue reunification by force.
constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% From an analytical perspective, the mandate of national rejuvenation serves both as a source of internal cohesion for the CCP (coordination) and as justification for external pressure on Taiwan and other regional actors (extraction).
constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_taiwan_reunification_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_taiwan_reunification_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(china_taiwan_reunification_mandate, TR),
    TR >= 0.70.

:- end_tests(china_taiwan_reunification_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.75 - High. The mandate extracts significant autonomy and self-determination from Taiwan, and risks extracting stability from the region. Suppression: 0.85 - Very High. The PRC actively suppresses alternative narratives both within China and internationally, and limits Taiwan's ability to participate in international forums. Theater Ratio: 0.75 - High. There is a significant amount of performative rhetoric around peaceful reunification, used both domestically and internationally.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives reveal a fundamental disagreement on the legitimacy and impact of the mandate. The CCP sees it as a legitimate pursuit of national unity, while Taiwan views it as an existential threat to its autonomy and identity. The international community struggles to balance competing interests and values, while an analytical observer sees the mandate as both a source of internal cohesion and external pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   The CCP benefits from the mandate through increased legitimacy and internal cohesion, but also faces the potential costs of military action. Taiwan bears the cost of the mandate by facing existential threat to its autonomy and national identity. The international community is affected, as the mandate strains international norms and threatens regional stability.
 *
 * MANDATROPHY ANALYSIS:
 *   This scenario illustrates a clear mandatrophy. While the CCP frames the mandate as a legitimate pursuit of national unity (potentially justifying it as a 'rope' or 'scaffold'), the coercive methods employed and the existential threat posed to Taiwan suggest it functions primarily as a 'snare'. The high levels of extraction and suppression make it difficult to reframe as genuine coordination, even from the CCP's perspective. The mandate_resolved flag is set to true to indicate that the high extraction is due to the nature of the constraint and is not a misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taiwan_identity_shift,
    'To what extent will Taiwanese national identity continue to diverge from mainland Chinese identity?',
    'Longitudinal surveys of Taiwanese identity and cross-strait relations.',
    'If Taiwanese identity continues to strengthen, the political cost of forced reunification for the CCP increases. If it weakens, the pressure for reunification from within China might decrease.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(taiwan_identity_shift, empirical, 'The degree to which Taiwanese identity diverges from mainland Chinese identity.').

omega_variable(
    economic_interdependence_threshold,
    'What level of economic interdependence would effectively deter military conflict across the Taiwan Strait?',
    'Economic modeling and game-theoretic analysis of cross-strait economic relations.',
    'If a high degree of interdependence is required, then current ties may be insufficient to prevent conflict. If a low degree is sufficient, then the current ties may create incentives to preserve the status quo.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_interdependence_threshold, empirical, 'The threshold of economic interdependence required to deter military conflict.').

omega_variable(
    international_response_calibration,
    'How would the international community respond to different levels of PRC pressure on Taiwan (e.g., economic coercion, grey-zone tactics, military blockade, invasion)?',
    'Scenario planning and diplomatic signaling analysis.',
    'The credibility of international deterrence influences CCP''s cost-benefit analysis. A weak response emboldens further escalation; a strong response raises the cost of military action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_response_calibration, conceptual, 'The nature and strength of the international response to different levels of PRC pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_taiwan_reunification_mandate, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chin_tr_t0, china_taiwan_reunification_mandate, theater_ratio, 0, 0.2).
narrative_ontology:measurement(chin_tr_t25, china_taiwan_reunification_mandate, theater_ratio, 25, 0.5).
narrative_ontology:measurement(chin_tr_t50, china_taiwan_reunification_mandate, theater_ratio, 50, 0.75).

% Extraction over time
narrative_ontology:measurement(chin_be_t0, china_taiwan_reunification_mandate, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(chin_be_t25, china_taiwan_reunification_mandate, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(chin_be_t50, china_taiwan_reunification_mandate, base_extractiveness, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_taiwan_reunification_mandate, enforcement_mechanism).
narrative_ontology:affects_constraint(china_taiwan_reunification_mandate, south_china_sea_claims).
narrative_ontology:affects_constraint(china_taiwan_reunification_mandate, one_country_two_systems).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
