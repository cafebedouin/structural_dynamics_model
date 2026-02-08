% ============================================================================
% CONSTRAINT STORY: china_taiwan_reunification_mandate
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_china_taiwan_reunification_mandate, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: china_taiwan_reunification_mandate
 * human_readable: The Mandate of National Rejuvenation (PRC Perspective)
 * domain: political
 * * SUMMARY:
 * From the perspective of the People's Republic of China (PRC), reunification with Taiwan is not a
 * strategic choice but a historical necessity and a "core interest" integral to the "Great
 * Rejuvenation of the Chinese Nation" by the 2049 centenary. This mandate frames the issue as the
 * final step in ending the "Century of Humiliation," making any alternative politically untenable.
 * * KEY AGENTS:
 * - Mainland Citizen: The subject (Powerless), socialized to view the "One China" principle as a natural law of history.
 * - PRC Leadership (CCP): The beneficiary (Institutional), for whom the mandate is a cornerstone of regime legitimacy.
 * - PLA Strategist: An enforcer (Organized), managing military deterrence to suppress alternatives.
 * - Analytical Observer: A neutral auditor assessing the structural properties of the mandate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale: 0.8. High. The mandate extracts massive state resources, military focus, and diplomatic
% capital to enforce international adherence to the One China Principle.
domain_priors:base_extractiveness(china_taiwan_reunification_mandate, 0.8).
% Rationale: 0.9. Extreme. The "Taiwan Independence" alternative is legally defined as a "red line"
% triggering the Anti-Secession Law, effectively suppressing it with the threat of force.
domain_priors:suppression_score(china_taiwan_reunification_mandate, 0.9).
% Rationale: 0.1. Low. The mandate is a deeply held ideological position, not a performative policy.
% Activities are functional (military drills, diplomatic pressure) rather than theatrical.
domain_priors:theater_ratio(china_taiwan_reunification_mandate, 0.1).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(china_taiwan_reunification_mandate, extractiveness, 0.8).
narrative_ontology:constraint_metric(china_taiwan_reunification_mandate, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(china_taiwan_reunification_mandate, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% The state presents reunification not as a policy but as an inevitable, natural historical conclusion.
narrative_ontology:constraint_claim(china_taiwan_reunification_mandate, tangled_rope).

% Binary flags
% Requires constant diplomatic pressure, ADIZ incursions, and blockade rehearsals.
domain_priors:requires_active_enforcement(china_taiwan_reunification_mandate).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(china_taiwan_reunification_mandate, ccp_regime_legitimacy).
narrative_ontology:constraint_beneficiary(china_taiwan_reunification_mandate, chinese_national_identity).
narrative_ontology:constraint_victim(china_taiwan_reunification_mandate, taiwanese_sovereignty).
narrative_ontology:constraint_victim(china_taiwan_reunification_mandate, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MAINLAND CITIZEN) -> MOUNTAIN
% For a citizen socialized within the state's educational and media apparatus, Taiwan as part of
% China is an unchangeable fact of history and geography, akin to a natural law.
constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (PRC LEADERSHIP) -> ROPE
% For the architects in Beijing, the mandate is a Rope—a functional coordination mechanism for
% national rejuvenation and consolidating legitimacy. It aligns state policy, military posture,
% and national identity toward a single goal.
constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER -> TANGLED ROPE
% The observer sees both the coordination function (unifying national identity) and the severe,
% asymmetric extraction (loss of regional stability, suppression of Taiwanese self-determination).
% Because it has both, and requires active enforcement, it is a Tangled Rope.
constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ENFORCER (PLA STRATEGIST) -> SNARE
% The military strategist, tasked with implementation, views the mandate's tools—blockade
% rehearsals, gray-zone warfare, diplomatic isolation—as a Snare designed to choke the space for
% "separatist forces" until political surrender is the only option.
constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, snare,
    context(agent_power(organized),
            time_horizon(historical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_taiwan_reunification_mandate_tests).

test(perspectival_gap_mountain_vs_rope) :-
    constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, mountain,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, rope,
        context(agent_power(institutional), _, _, _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(high_extraction_and_suppression_thresholds) :-
    narrative_ontology:constraint_metric(china_taiwan_reunification_mandate, extractiveness, E),
    narrative_ontology:constraint_metric(china_taiwan_reunification_mandate, suppression_requirement, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(china_taiwan_reunification_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a constraint that is ideologically rigid and operationally severe. The base
 * extractiveness (0.8) and suppression (0.9) are among the highest in the corpus, representing a
 * state-level mandate treated as a non-negotiable core interest. The perspectival gap is stark:
 * the CCP leadership experiences it as a necessary Rope for national coordination, while citizens
 * perceive it as an immutable Mountain of historical fact. The analytical view must be Tangled
 * Rope because the mandate serves a genuine coordination function for one group (consolidating
 * national identity) while simultaneously imposing severe asymmetric extraction on others.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This case is a primary example of mandatrophy risk. An institutional actor presents a
 * high-extraction constraint (ε=0.8) as a pure Rope (coordination). A naive classification might
 * label it a Snare. However, the Deferential Realism system, by identifying both a genuine
 * beneficiary (coordination function) and a victim (asymmetric extraction), correctly classifies
 * it as a Tangled Rope from the neutral analytical perspective. This acknowledges the dual nature
 * of the constraint without dismissing the beneficiary's claim or the victim's experience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_reunification_mandate,
    'Is the 2049 deadline a hard trigger for action (Mountain of time), or a flexible political aspiration (Rope for mobilization)?',
    'Analysis of internal CCP documents and shifts in military resource allocation as the date approaches.',
    'If a hard trigger, the risk of conflict is non-linear and increases sharply. If flexible, the constraint remains a coercive but manageable Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_taiwan_reunification_mandate, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The mandate has intensified over the last decade, moving from a long-term goal to a more urgent
% component of national policy under the "Rejuvenation" banner.
% Theater ratio remains low and stable, as the policy is not performative.
narrative_ontology:measurement(ctr_tr_t0, china_taiwan_reunification_mandate, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ctr_tr_t5, china_taiwan_reunification_mandate, theater_ratio, 5, 0.1).
narrative_ontology:measurement(ctr_tr_t10, china_taiwan_reunification_mandate, theater_ratio, 10, 0.1).

% Extraction has increased as more diplomatic, economic, and military capital is expended.
narrative_ontology:measurement(ctr_ex_t0, china_taiwan_reunification_mandate, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(ctr_ex_t5, china_taiwan_reunification_mandate, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(ctr_ex_t10, china_taiwan_reunification_mandate, base_extractiveness, 10, 0.8).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The mandate functions as a top-down mechanism to enforce national policy and identity.
narrative_ontology:coordination_type(china_taiwan_reunification_mandate, enforcement_mechanism).

% The mandate's enforcement has direct, structural effects on global technology supply chains,
% given Taiwan's central role in semiconductor manufacturing.
narrative_ontology:affects_constraint(china_taiwan_reunification_mandate, semiconductor_supply_chains).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */