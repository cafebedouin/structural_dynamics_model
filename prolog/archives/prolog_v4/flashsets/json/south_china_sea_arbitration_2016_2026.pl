% ============================================================================
% CONSTRAINT STORY: south_china_sea_arbitration_2016_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_south_china_sea_arbitration_2016_2026, []).

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
 *   constraint_id: south_china_sea_arbitration_2016_2026
 *   human_readable: The 2016 South China Sea Arbitral Award (2016-2026)
 *   domain: geopolitical/legal
 *
 * SUMMARY:
 *   On July 12, 2016, an Arbitral Tribunal under UNCLOS ruled in favor of the
 *   Philippines, invalidating China's "nine-dash line" and "historic rights"
 *   claims in the South China Sea. This constraint story analyzes the impact
 *   of the award over the period 2016-2026, focusing on its effectiveness in
 *   altering the regional geopolitical landscape and upholding international
 *   maritime law.
 *
 * KEY AGENTS:
 *   - Philippines: Beneficiary (moderate/constrained) - gained a legal victory but lacks the power to fully enforce it.
 *   - China: Victim (powerful/mobile) - lost the legal case but continues its activities in the region.
 *   - Regional Stability: Victim (powerless/trapped) - suffers from increased tensions and militarization.
 *   - International Maritime Law: Institutional (institutional/constrained) - suffered a blow to its authority due to the award's non-enforcement
 *   - Analytical Observer: Analytical (analytical/analytical) - sees the tangled rope nature of international law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(south_china_sea_arbitration_2016_2026, 0.6).
domain_priors:suppression_score(south_china_sea_arbitration_2016_2026, 0.7).
domain_priors:theater_ratio(south_china_sea_arbitration_2016_2026, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(south_china_sea_arbitration_2016_2026, extractiveness, 0.6).
narrative_ontology:constraint_metric(south_china_sea_arbitration_2016_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(south_china_sea_arbitration_2016_2026, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(south_china_sea_arbitration_2016_2026, tangled_rope).
narrative_ontology:human_readable(south_china_sea_arbitration_2016_2026, "The 2016 South China Sea Arbitral Award (2016-2026)").
narrative_ontology:topic_domain(south_china_sea_arbitration_2016_2026, "geopolitical/legal").

domain_priors:requires_active_enforcement(south_china_sea_arbitration_2016_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(south_china_sea_arbitration_2016_2026, philippines).
narrative_ontology:constraint_beneficiary(south_china_sea_arbitration_2016_2026, international_maritime_law).
narrative_ontology:constraint_victim(south_china_sea_arbitration_2016_2026, china).
narrative_ontology:constraint_victim(south_china_sea_arbitration_2016_2026, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Regional stability is trapped by the unresolved dispute and increased militarization. Has no direct means of escaping the negative consequences of the award's non-enforcement and China's continued assertiveness.
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% The Philippines benefits from the legal victory but is constrained by its limited ability to enforce the award against China. Experiences the situation as a tangled rope -- a mix of benefit and constraint.
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% International maritime law, as embodied by UNCLOS, has been weakened by the lack of enforcement and the challenges to its authority. The award is theatrically upheld, but enforcement remains weak. Former function atrophied due to lack of enforcement mechanism.
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% China bears the cost of the legal loss but remains powerful and mobile in its actions, largely ignoring the award. Enjoys economic benefits from the region. Experiences some constraint but exerts considerable influence.
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical observer sees the arbitration as a tangled rope, a mix of legal coordination and geopolitical extraction. The award represents a legal framework (coordination) but also serves as a tool for projecting power and influencing regional dynamics (extraction).
constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(south_china_sea_arbitration_2016_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(south_china_sea_arbitration_2016_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(south_china_sea_arbitration_2016_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(south_china_sea_arbitration_2016_2026, TR),
    TR >= 0.70.

:- end_tests(south_china_sea_arbitration_2016_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is relatively high (0.6) because the award, while intended to promote legal coordination, in practice has resulted in increased regional instability and militarization. Suppression is also high (0.7) because China continues to disregard the award and suppress dissenting voices. Theater ratio is moderate (0.75) because there is some performative adherence to international law, but little real enforcement. The tangled rope classification reflects the mixed nature of the constraint: a legal instrument intended for coordination that is simultaneously contributing to extraction and conflict.
 *
 * PERSPECTIVAL GAP:
 *   The different perspectives highlight the conflicting impacts of the award. The Philippines views the award as a positive development, but their constrained ability to enforce it limits the benefit. China views the award negatively but maintains mobility in the region through military and economic power. Regional stability is trapped by the ongoing dispute. The International Maritime Law is theatrically upheld, while the Analytical Observer sees both sides of the tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the agent's structural position and their relationship to the extraction flow. The Philippines benefits from the award but is constrained, so their d value is moderate. China is negatively impacted but remains powerful, so their d value is also moderate. Regional stability is trapped, resulting in a higher d value.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism,
    'Will a viable enforcement mechanism for international maritime law emerge?',
    'Development of international consensus, creation of multilateral enforcement bodies, changes in great power politics.',
    'If yes, UNCLOS and international law are strengthened. If no, the system continues to degrade into a theatrical performance with little functional impact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism, preference, 'The future of enforcement mechanisms for international maritime law.').

omega_variable(
    regional_power_balance,
    'How will the regional power balance shift over the next decade?',
    'Analysis of military spending, economic growth, diplomatic alliances, and technological advancements.',
    'Determines the enforceability of international law and the extent to which the ruling will be respected. Affects whether this is a snare for regional stability or a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_power_balance, empirical, 'The shift in regional power balance in the South China Sea.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(south_china_sea_arbitration_2016_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sout_tr_t0, south_china_sea_arbitration_2016_2026, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sout_tr_t5, south_china_sea_arbitration_2016_2026, theater_ratio, 5, 0.6).
narrative_ontology:measurement(sout_tr_t10, south_china_sea_arbitration_2016_2026, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(sout_be_t0, south_china_sea_arbitration_2016_2026, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(sout_be_t5, south_china_sea_arbitration_2016_2026, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(sout_be_t10, south_china_sea_arbitration_2016_2026, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(south_china_sea_arbitration_2016_2026, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
