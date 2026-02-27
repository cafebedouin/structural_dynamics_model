% ============================================================================
% CONSTRAINT STORY: un_high_seas_treaty_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_un_high_seas_treaty_2026, []).

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
 *   constraint_id: un_high_seas_treaty_2026
 *   human_readable: UN High Seas Treaty for Marine Biodiversity (BBNJ)
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The UN High Seas Treaty, effective in 2026, establishes a legal framework
 *   for governing biodiversity in areas beyond national jurisdiction (the
 *   "high seas"). This treaty aims to balance conservation with sustainable
 *   use of marine resources, creating inherent tensions among various
 *   stakeholders. The treaty's effectiveness hinges on enforcement capacity,
 *   state cooperation, and the definition of 'significant harm' to marine
 *   biodiversity.
 *
 * KEY AGENTS:
 *   - Marine Ecosystems: Primary victim (powerless/trapped) - Suffer from continued exploitation despite the treaty.
 *   - Developing Coastal States: Moderate actors (moderate/constrained) - Constrained by resources, benefit from regulated access.
 *   - UN Regulatory Bodies: Primary beneficiary (institutional/arbitrage) - Benefit from increased mandate and funding.
 *   - Regulated Fishing Fleets: Organized actors (organized/mobile) - Bear the cost of restrictions, benefit from sustainable practices.
 *   - States Opposed to Regulation: Powerful actors (powerful/constrained) - Find their access and exploitation limited.
 *   - Marine Conservation Organizations: Primary beneficiary (organized/mobile) - benefit from protection of marine biodiversity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(un_high_seas_treaty_2026, 0.55).
domain_priors:suppression_score(un_high_seas_treaty_2026, 0.45).
domain_priors:theater_ratio(un_high_seas_treaty_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(un_high_seas_treaty_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(un_high_seas_treaty_2026, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(un_high_seas_treaty_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(un_high_seas_treaty_2026, tangled_rope).
narrative_ontology:human_readable(un_high_seas_treaty_2026, "UN High Seas Treaty for Marine Biodiversity (BBNJ)").
narrative_ontology:topic_domain(un_high_seas_treaty_2026, "geopolitical").

domain_priors:requires_active_enforcement(un_high_seas_treaty_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(un_high_seas_treaty_2026, marine_conservation_organizations).
narrative_ontology:constraint_beneficiary(un_high_seas_treaty_2026, scientific_community).
narrative_ontology:constraint_victim(un_high_seas_treaty_2026, unregulated_fishing_fleets).
narrative_ontology:constraint_victim(un_high_seas_treaty_2026, states_opposed_to_regulation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Marine Ecosystems - Trapped, suffer from continued exploitation despite the treaty. No exit; high extraction.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Developing Coastal States - Constrained by resources for enforcement but benefit from regulated access and biodiversity protection. Mixed extraction/coordination.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 3: UN Regulatory Bodies - Benefit from increased mandate and funding. Low extraction; high coordination function.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: Regulated Fishing Fleets - Bear the cost of restricted zones but benefit from sustainable practices and reduced competition. Mobile exit (can move to unregulated waters). Mixed extraction/coordination.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Perspective 5: Analytical Observer - Sees the mixed coordination and extraction, the inherent tension between conservation and exploitation.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective 6: States Opposed to Regulation - Powerful states find their access and exploitation limited by the treaty. While they have some influence, their exit is constrained by international norms and pressures.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(un_high_seas_treaty_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(un_high_seas_treaty_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(un_high_seas_treaty_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(un_high_seas_treaty_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(un_high_seas_treaty_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.55 - Reflects the treaty's extraction from unregulated actors and limited restrictions on regulated ones. Suppression: 0.45 - Represents the limited but significant coercion imposed by the treaty through regulations and monitoring. Theater ratio: 0.30 - Relatively low, indicating that the treaty's primary function is regulatory, not performative.
 *
 * PERSPECTIVAL GAP:
 *   The treaty is viewed as a Snare by marine ecosystems and some states, a Tangled Rope by developing coastal states and regulated fishing fleets, and a Rope by UN regulatory bodies. This demonstrates the inherent tensions and trade-offs involved in international environmental governance. Marine ecosystems are unable to exit the high seas, and suffer the effects of those exploiting it, thus viewing it as a Snare. The developed world benefit from the high seas, but cannot completely exploit it, since, to do so, it would no longer be able to be exploited.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations determine the directionality. Marine ecosystems and some states (victims) experience high d values, leading to higher effective extraction (chi). UN regulatory bodies (beneficiaries) have low d values, resulting in lower or negative chi. Regulated fishing fleets and developing coastal states occupy intermediate positions, experiencing mixed effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The UN High Seas Treaty presents as a Tangled Rope, incorporating both coordination and extraction. The treaty's coordination function aims to sustainably manage marine biodiversity, but this inherently requires extracting from certain actors (unregulated fishing, states opposed to regulation) to benefit others (marine ecosystems, responsible users). Mandatrophy is resolved by recognizing the treaty's duality: it serves as a coordinating framework for global governance while simultaneously imposing restrictions and costs on specific stakeholders. The 'tangled' nature reflects the complex interplay of interests and the imperfect balance between conservation and exploitation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity,
    'Will enforcement capacity be sufficient to deter unregulated activity?',
    'Monitoring of enforcement efforts and compliance rates.',
    'If low: treaty becomes ineffective, remains a Snare for marine ecosystems. If high: treaty achieves its goals, becomes more of a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity, empirical, 'Sufficiency of enforcement capacity.').

omega_variable(
    state_cooperation,
    'Will major states cooperate effectively in implementation?',
    'Analysis of voting patterns and participation in treaty mechanisms.',
    'If low: treaty effectiveness is limited, more Snare-like. If high: treaty gains broad legitimacy, more Rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_cooperation, empirical, 'Level of state cooperation.').

omega_variable(
    definition_of_harm,
    'How will ''significant harm'' to marine biodiversity be defined and assessed?',
    'Development of scientific assessment methodologies and legal precedents.',
    'If narrowly defined: allows continued exploitation, treaty remains Snare for marine ecosystems. If broadly defined: provides strong protection, treaty becomes more Rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_harm, conceptual, 'Definition of ''significant harm''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(un_high_seas_treaty_2026, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(un_h_tr_t0, un_high_seas_treaty_2026, theater_ratio, 0, 0.15).
narrative_ontology:measurement(un_h_tr_t10, un_high_seas_treaty_2026, theater_ratio, 10, 0.25).
narrative_ontology:measurement(un_h_tr_t20, un_high_seas_treaty_2026, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(un_h_be_t0, un_high_seas_treaty_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(un_h_be_t10, un_high_seas_treaty_2026, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(un_h_be_t20, un_high_seas_treaty_2026, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(un_high_seas_treaty_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(un_high_seas_treaty_2026, illegal_fishing_practices).
narrative_ontology:affects_constraint(un_high_seas_treaty_2026, marine_protected_areas_effectiveness).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
