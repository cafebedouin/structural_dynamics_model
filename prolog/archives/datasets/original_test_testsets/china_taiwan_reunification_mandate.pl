% ============================================================================
% CONSTRAINT STORY: china_taiwan_reunification_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
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
 *   domain: political/geopolitical
 *
 * SUMMARY:
 *   The PRC's mandate for reunification with Taiwan, framed as essential to
 *   the 'Great Rejuvenation of the Chinese Nation,' is a powerful political
 *   constraint. It functions as a core legitimizing narrative for the CCP,
 *   demanding immense state resources and suppressing alternative political
 *   futures. For Taiwan, it represents an existential threat to its
 *   sovereignty. This story serves as a diagnostic exemplar, demonstrating
 *   how a single, high-stakes political constraint can manifest as all six DR
 *   types depending on the observer's structural relationship to it.
 *
 * KEY AGENTS:
 *   - CCP Leadership: Primary beneficiary (institutional/arbitrage) — uses the mandate to legitimize rule and direct national policy.
 *   - Taiwanese Sovereignty & Autonomy: Primary victim (powerless/trapped) — the direct target of extraction and coercion.
 *   - PRC Citizens: Secondary victims (moderate/constrained) — bear the economic and social costs of the mandate.
 *   - Multinational Corporations: Powerful actors (powerful/arbitrage) — navigate the risk as a temporary business condition.
 *   - Analytical Observers: External analysts (analytical/analytical) — risk mischaracterizing a political project as a historical inevitability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_taiwan_reunification_mandate, 0.65).
domain_priors:suppression_score(china_taiwan_reunification_mandate, 0.8).
domain_priors:theater_ratio(china_taiwan_reunification_mandate, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_taiwan_reunification_mandate, extractiveness, 0.65).
narrative_ontology:constraint_metric(china_taiwan_reunification_mandate, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(china_taiwan_reunification_mandate, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_taiwan_reunification_mandate, tangled_rope).
narrative_ontology:human_readable(china_taiwan_reunification_mandate, "The Mandate of National Rejuvenation (PRC Perspective)").
narrative_ontology:topic_domain(china_taiwan_reunification_mandate, "political/geopolitical").

domain_priors:requires_active_enforcement(china_taiwan_reunification_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_taiwan_reunification_mandate, chinese_communist_party_leadership).
narrative_ontology:constraint_beneficiary(china_taiwan_reunification_mandate, prc_nationalist_identity).
narrative_ontology:constraint_victim(china_taiwan_reunification_mandate, taiwanese_sovereignty_and_autonomy).
narrative_ontology:constraint_victim(china_taiwan_reunification_mandate, prc_citizens_bearing_costs).
narrative_ontology:constraint_victim(china_taiwan_reunification_mandate, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAIWANESE CITIZEN (SNARE) — Trapped by geography and geopolitics, the individual citizen faces the direct threat of coercion and the extraction of their political autonomy. The mandate offers no coordination benefit, only existential risk. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.74. This high effective extraction firmly classifies the constraint as a Snare.
constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CCP LEADERSHIP (ROPE) — As the primary beneficiary with arbitrage over the timing and intensity of enforcement, the leadership experiences the mandate as a pure coordination device. It aligns state resources, legitimizes power, and solves the 'problem' of national division. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.08. The negative effective extraction signifies a net subsidy to this agent's power.
constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — The observer sees the full structure: a genuine internal coordination function for the PRC (Rope-like) combined with a highly coercive, extractive function targeting Taiwan (Snare-like). This duality is the definition of a Tangled Rope. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.90. The classification holds at the upper bound of the Tangled Rope range.
constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: OLD GUARD KMT VETERAN (PITON) — For a traditionalist who believes in 'One China' under the ROC, the CCP's mandate is a degraded form of a once-functional goal. The original purpose (a unified, free China) has atrophied, leaving only the performative shell of nationalist rhetoric. The high theater_ratio (0.75) and inertial nature of the claim satisfy the Piton classification.
constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: 'MANDATE OF HEAVEN' HISTORIAN (MOUNTAIN) — This perspective naturalizes the political mandate as an immutable law of Chinese history, a recurring cycle of unification and fragmentation. It frames the constraint as a Mountain. The engine will identify this as a 'false summit,' as the base properties (ε=0.65, suppression=0.80) are antithetical to a natural law.
constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: MULTINATIONAL CORPORATION (SCAFFOLD) — A corporation with operations in both markets sees the tension as a temporary business risk to be managed via redundant supply chains and political hedging. This risk structure (scaffold) has a de-facto sunset clause defined by their own investment horizons (e.g., 10-15 years). They extract value from both sides (coordination) while managing the costs of instability. d≈0.25, f(d)≈0.15, σ=1.2 → χ≈0.12. This low effective extraction classifies as a Scaffold.
constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_taiwan_reunification_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_taiwan_reunification_mandate, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.65): High. Reflects the immense diversion of PRC state resources to military and diplomatic efforts, and the intended extraction of political sovereignty from 23 million people in Taiwan. Suppression (0.80): Very High. Internally, dissent on this 'core interest' is nonexistent in public discourse. Externally, Taiwan's international space is actively and systematically suppressed. Theater Ratio (0.75): High. The narrative is built on historical claims, nationalist sentiment, and performative military displays, which are crucial for maintaining domestic consensus and signaling intent abroad. The functional aspect (actual invasion planning) is real but shrouded by the vast theatrical production.
 *
 * PERSPECTIVAL GAP:
 *   The gap is maximal. The CCP sees a noble coordination project (Rope). A Taiwanese citizen sees a coercive trap (Snare). An old-school nationalist sees a degraded ideal (Piton). A historian sees an eternal cycle (Mountain). A corporation sees a manageable risk (Scaffold). The analytical observer sees the synthesis of coordination and extraction (Tangled Rope). The profound disagreement is not about the facts on the ground, but about the structural interpretation of those facts from different positions of power, exit, and vulnerability.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (CCP) with arbitrage have a low 'd' value, experiencing the constraint as a net benefit (negative chi). Victims (Taiwan) who are trapped have a maximal 'd' value, experiencing it as pure extraction (high chi). Other actors fall in between based on their ability to exit or re-frame the constraint (e.g., the MNC's ability to treat it as a temporary scaffold). The model correctly derives these varied experiences from the declared structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a powerful resolution to mandatrophy. It demonstrates that arguing whether the mandate is 'really' a Rope or 'really' a Snare is a category error. It is structurally both, and more. The full character of the constraint is the collection of all its perspectival classifications. The system avoids mislabeling a coercive Snare as a benign Rope by requiring classification from the victim's perspective, while also acknowledging the genuine (internal) coordination function perceived by the beneficiary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    leadership_intent,
    'Is the ''rejuvenation'' mandate a deeply held ideological belief or a pragmatic instrument for maintaining CCP legitimacy?',
    'Access to internal party documents and un-curated statements from leadership (currently impossible).',
    'If ideological, the constraint is less responsive to off-ramps or economic costs (more Mountain-like). If instrumental, it is more susceptible to cost-benefit analysis (more Tangled Rope-like).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(leadership_intent, conceptual, 'Distinguishing ideological conviction from instrumental legitimacy tool').

omega_variable(
    popular_support_uncoerced,
    'What is the true level of popular support within the PRC for a military invasion of Taiwan, absent state censorship and propaganda?',
    'Large-scale, methodologically sound, uncensored polling within the PRC.',
    'High organic support would make the constraint more rigid, as the CCP would be responding to a real mandate. Low support would imply the constraint is primarily a top-down imposition, making it more brittle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(popular_support_uncoerced, empirical, 'True level of popular support for military action absent propaganda').

omega_variable(
    economic_cost_tolerance,
    'What is the CCP leadership''s tolerance for economic damage (from sanctions, war costs) in pursuit of reunification?',
    'Revealed preferences during crises; internal economic modeling and wargaming results.',
    'A high tolerance for economic pain makes the Snare aspect more severe and likely. A low tolerance suggests the theatrical and diplomatic aspects are primary, and the military threat is a bargaining tool.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_cost_tolerance, preference, 'Leadership''s tolerance for economic self-harm to achieve reunification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_taiwan_reunification_mandate, 2012, 2049).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chin_tr_t2012, china_taiwan_reunification_mandate, theater_ratio, 2012, 0.6).
narrative_ontology:measurement(chin_tr_t2018, china_taiwan_reunification_mandate, theater_ratio, 2018, 0.7).
narrative_ontology:measurement(chin_tr_t2024, china_taiwan_reunification_mandate, theater_ratio, 2024, 0.75).

% Extraction over time
narrative_ontology:measurement(chin_be_t2012, china_taiwan_reunification_mandate, base_extractiveness, 2012, 0.5).
narrative_ontology:measurement(chin_be_t2018, china_taiwan_reunification_mandate, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement(chin_be_t2024, china_taiwan_reunification_mandate, base_extractiveness, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_taiwan_reunification_mandate, enforcement_mechanism).
narrative_ontology:affects_constraint(china_taiwan_reunification_mandate, semiconductor_supply_chain).
narrative_ontology:affects_constraint(china_taiwan_reunification_mandate, us_china_strategic_competition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
