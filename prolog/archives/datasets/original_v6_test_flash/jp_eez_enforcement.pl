% ============================================================================
% CONSTRAINT STORY: jp_eez_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jp_eez_enforcement, []).

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
 *   constraint_id: jp_eez_enforcement
 *   human_readable: Enforcement of Japan's Claimed Exclusive Economic Zone (EEZ)
 *   domain: geopolitical
 *
 * SUMMARY:
 *   This constraint represents the active enforcement of Japan's claimed
 *   Exclusive Economic Zone (EEZ) around the Senkaku/Diaoyu Islands. This
 *   enforcement is a point of contention with China, which also claims the
 *   islands and the surrounding waters. The enforcement activities, primarily
 *   carried out by the Japan Coast Guard, involve patrolling the area,
 *   intercepting foreign vessels (mainly Chinese fishing boats), and
 *   potentially seizing vessels found to be operating illegally within the
 *   EEZ. The situation is further complicated by the ambiguous legal status
 *   of the islands and the surrounding waters under international law.
 *
 * KEY AGENTS:
 *   - Chinese Fishing Vessels: Primary target (powerless/trapped) - faces potential capture and fines.
 *   - Japan Coast Guard: Primary enforcer (institutional/arbitrage) - benefits from increased mandate and resources.
 *   - Japanese Fishing Industry: Beneficiary (powerful/constrained) - benefits from protection of fishing grounds.
 *   - Regional Stability: Affected party (moderate/constrained) - faces increased tension and risk of escalation.
 *   - Analytical Observer: Neutral observer (analytical/analytical) - assesses long-term implications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jp_eez_enforcement, 0.6).
domain_priors:suppression_score(jp_eez_enforcement, 0.7).
domain_priors:theater_ratio(jp_eez_enforcement, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jp_eez_enforcement, extractiveness, 0.6).
narrative_ontology:constraint_metric(jp_eez_enforcement, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(jp_eez_enforcement, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jp_eez_enforcement, tangled_rope).
narrative_ontology:human_readable(jp_eez_enforcement, "Enforcement of Japan's Claimed Exclusive Economic Zone (EEZ)").
narrative_ontology:topic_domain(jp_eez_enforcement, "geopolitical").

domain_priors:requires_active_enforcement(jp_eez_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jp_eez_enforcement, japanese_fishing_industry).
narrative_ontology:constraint_beneficiary(jp_eez_enforcement, japan_coast_guard).
narrative_ontology:constraint_victim(jp_eez_enforcement, chinese_fishing_vessels).
narrative_ontology:constraint_victim(jp_eez_enforcement, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of Chinese fishing vessels operating in the disputed waters. They face potential capture, fines, and loss of livelihood, with limited ability to exit the situation due to economic pressures and state support.
constraint_indexing:constraint_classification(jp_eez_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% Perspective of regional stability. The enforcement activity contributes to tensions and risks escalation, but also provides a degree of predictability and prevents unchecked exploitation of resources. Constrained by the existing geopolitical landscape.
constraint_indexing:constraint_classification(jp_eez_enforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective of the Japan Coast Guard. Benefits from increased resources and mandate. Sees its role as maintaining order and protecting Japanese interests. Can reallocate resources based on strategic needs.
constraint_indexing:constraint_classification(jp_eez_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective of the Japanese fishing industry. Benefits from the protection of fishing grounds, but also faces constraints due to geopolitical tensions and potential disruptions of trade.
constraint_indexing:constraint_classification(jp_eez_enforcement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Analytical perspective observing the long-term implications of the enforcement activity on international law, regional security, and resource management. This perspective considers the interplay of geopolitical interests, historical claims, and economic factors.
constraint_indexing:constraint_classification(jp_eez_enforcement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jp_eez_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jp_eez_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jp_eez_enforcement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jp_eez_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jp_eez_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The enforcement actions directly extract resources (fines, seized vessels) and restrict access to fishing grounds for Chinese vessels. Suppression (0.70): High. Japan actively suppresses competing claims to the EEZ through patrols and enforcement. Theater Ratio (0.30): Low. The enforcement actions are largely functional, with a clear objective of maintaining control over the EEZ, although the act itself carries performative weight in signaling resolve.
 *
 * PERSPECTIVAL GAP:
 *   The enforcement of Japan's EEZ is viewed differently by various actors. Chinese fishing vessels perceive it as a snare, limiting their access to resources. The Japan Coast Guard sees it as a rope, enabling them to maintain order. The Japanese fishing industry benefits, but also acknowledges the constrained environment created by geopolitical tensions. Regional stability is affected by both the stabilizing and destabilizing aspects of the enforcement actions, leading to a tangled rope classification. The analytical observer recognizes the complex interplay of factors, also resulting in a tangled rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the beneficiaries and victims declared in base_properties. The Japan Coast Guard and the Japanese fishing industry benefit from the enforcement, resulting in a lower directionality value. Chinese fishing vessels and regional stability bear the costs, resulting in a higher directionality value.
 *
 * MANDATROPHY ANALYSIS:
 *   The enforcement of the EEZ could be misconstrued as simply a protection of resources (Rope) or pure aggression (Snare). However, the Tangled Rope classification captures the complexity of the situation. It acknowledges the legitimate need for resource management and maritime security while also recognizing the potential for escalation and the impact on other stakeholders. The enforcement provides a coordination function for Japanese fishing interests, but also extracts from Chinese fishing vessels and contributes to regional instability. Resolving the mandatrophy requires considering the interplay of these factors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    senkaku_island_sovereignty,
    'What is the legally recognized sovereignty status of the Senkaku/Diaoyu Islands?',
    'International legal rulings, historical treaty analysis, diplomatic negotiations.',
    'If Japanese sovereignty is universally recognized, enforcement is a routine act. If Chinese sovereignty is recognized, enforcement is an act of aggression. Contested status leads to the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(senkaku_island_sovereignty, conceptual, 'Legally recognized sovereignty status of the Senkaku/Diaoyu Islands.').

omega_variable(
    resource_depletion_threshold,
    'What is the threshold of resource depletion in the EEZ that would trigger international intervention or condemnation of enforcement activities?',
    'Scientific assessments of fish stocks, monitoring of fishing activities, international agreements on sustainable resource management.',
    'If depletion is high, international pressure may force Japan to reduce enforcement activities, shifting the classification. If depletion is low, enforcement may be seen as justified resource management.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_depletion_threshold, empirical, 'Threshold of resource depletion triggering intervention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jp_eez_enforcement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jp_e_tr_t0, jp_eez_enforcement, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jp_e_tr_t5, jp_eez_enforcement, theater_ratio, 5, 0.2).
narrative_ontology:measurement(jp_e_tr_t10, jp_eez_enforcement, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(jp_e_be_t0, jp_eez_enforcement, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jp_e_be_t5, jp_eez_enforcement, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(jp_e_be_t10, jp_eez_enforcement, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jp_eez_enforcement, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
