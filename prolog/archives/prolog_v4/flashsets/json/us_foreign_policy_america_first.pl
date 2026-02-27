% ============================================================================
% CONSTRAINT STORY: us_foreign_policy_america_first
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_foreign_policy_america_first, []).

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
 *   constraint_id: us_foreign_policy_america_first
 *   human_readable: "America First" Foreign Policy Doctrine
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The "America First" foreign policy doctrine prioritizes bilateral,
 *   transactional relationships over multilateral alliances and international
 *   norms. This approach aims to maximize US benefits in the short term but
 *   can strain alliances and undermine global governance institutions. The
 *   doctrine's impact is viewed differently depending on the observer's
 *   position, ranging from a beneficial strategy for US revitalization to a
 *   detrimental extraction that weakens alliances and international
 *   stability.
 *
 * KEY AGENTS:
 *   - US Executive Branch: Primary beneficiary (institutional/arbitrage) - Gains flexibility and short-term economic advantages.
 *   - International Allies: Primary victim (moderate/constrained) - Face increased demands and weakened security guarantees.
 *   - Global Governance Institutions: Secondary victim (institutional/constrained) - Experience reduced US support and effectiveness.
 *   - Favored Trade Partners: Secondary beneficiary (moderate/mobile) - Benefit from preferential trade agreements with the US.
 *   - Small Allied Nations: Primary Victim (powerless/trapped) - Reliant on US security guarantees and trade, they are the target of increased demands and transactional relationships.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_foreign_policy_america_first, 0.6).
domain_priors:suppression_score(us_foreign_policy_america_first, 0.7).
domain_priors:theater_ratio(us_foreign_policy_america_first, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_foreign_policy_america_first, extractiveness, 0.6).
narrative_ontology:constraint_metric(us_foreign_policy_america_first, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_foreign_policy_america_first, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_foreign_policy_america_first, tangled_rope).
narrative_ontology:human_readable(us_foreign_policy_america_first, "\"America First\" Foreign Policy Doctrine").
narrative_ontology:topic_domain(us_foreign_policy_america_first, "geopolitical").

domain_priors:requires_active_enforcement(us_foreign_policy_america_first).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_foreign_policy_america_first, us_executive_branch).
narrative_ontology:constraint_beneficiary(us_foreign_policy_america_first, favored_trade_partners).
narrative_ontology:constraint_victim(us_foreign_policy_america_first, international_allies).
narrative_ontology:constraint_victim(us_foreign_policy_america_first, global_governance_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Smaller nations heavily reliant on US security guarantees and trade relationships find themselves trapped with limited exit options, bearing the brunt of US transactional demands and reduced commitment to collective security.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Traditional US allies experience both the extraction of the "America First" policy (increased defense spending demands, trade concessions) and the coordination benefits (continued, albeit weakened, security umbrella, access to US markets).
constraint_indexing:constraint_classification(us_foreign_policy_america_first, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% The US Executive Branch, particularly during administrations that champion the doctrine, benefits from increased flexibility in foreign policy decision-making, ability to pursue short-term economic gains, and reduced constraints from international norms and institutions.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Global governance institutions (UN, WTO, WHO) experience a decline in US support, funding, and legitimacy, leading to institutional atrophy and reduced effectiveness in addressing global challenges. The institutions remain in place but are weakened and less functional.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% An analytical observer recognizes the complex interplay of coordination and extraction, as the 'America First' doctrine simultaneously weakens multilateral institutions while potentially strengthening the US position in certain bilateral relationships.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_foreign_policy_america_first_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_foreign_policy_america_first, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_foreign_policy_america_first, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_foreign_policy_america_first, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_foreign_policy_america_first, TR),
    TR >= 0.70.

:- end_tests(us_foreign_policy_america_first_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): Moderate-high. US seeks to extract greater concessions from allies and trade partners. Suppression (0.70): High. The doctrine suppresses alternative foreign policy approaches, prioritizing US interests over collective action. Theater ratio (0.75): High. While the focus is on concrete results and measurable benefits for the US, the decline in multilateral engagement and diplomatic norms creates a theatrical aspect, where the US projects an image of unilateral strength even as its global influence potentially diminishes.
 *
 * PERSPECTIVAL GAP:
 *   The 'America First' doctrine is viewed as a Rope by the US Executive Branch, which benefits from increased flexibility and short-term gains. Traditional US Allies see it as a Tangled Rope, experiencing both extraction (increased demands) and coordination (continued security umbrella). Small Allied Nations see it as a Snare, trapped with limited exit options. Global Governance Institutions perceive it as a Piton, where US support has waned, leading to institutional decline. The analytical observer classifies it as a Tangled Rope, recognizing both its extractive and coordinative aspects.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality value (d) is determined by each agent's structural position. The US Executive Branch, as the primary beneficiary, has a low d value, experiencing coordination. Allies, as victims, have higher d values, experiencing varying degrees of extraction. Global governance institutions, experiencing decline, also have high d values. The analytical observer's perspective reflects the complex interplay of extraction and coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing the different perspectives on the same policy. 'America First' is not simply 'good' or 'bad,' but rather a complex constraint with both positive and negative consequences, depending on the observer's position. The framework captures these differing perspectives, providing a more nuanced understanding of the doctrine's impact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_economic_impact,
    'What is the long-term impact of prioritizing bilateral trade deals over multilateral agreements on US economic competitiveness?',
    'Comparative analysis of economic growth, trade balances, and investment flows under different trade policy regimes.',
    'If beneficial: ''America First'' is a legitimate strategy for US economic revitalization. If detrimental: it is a short-sighted extraction that undermines long-term prosperity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_economic_impact, empirical, 'Assessment of the long-term economic consequences of prioritizing bilateral trade deals.').

omega_variable(
    alliance_erosion_threshold,
    'To what extent does the ''America First'' doctrine erode US alliances and its ability to project power globally?',
    'Analysis of alliance cohesion, military cooperation, and diplomatic influence under different foreign policy approaches.',
    'If minimal: US can maintain its global leadership role while pursuing its own interests. If significant: US power is diminished and its security is compromised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_erosion_threshold, empirical, 'Evaluation of the impact on alliance strength and US global influence.').

omega_variable(
    rise_of_alternative_powers,
    'Does the ''America First'' doctrine create opportunities for alternative global powers to expand their influence and challenge US hegemony?',
    'Monitoring of the geopolitical strategies, economic investments, and military capabilities of rising powers.',
    'If yes: ''America First'' accelerates the shift towards a multipolar world order. If no: US remains the dominant global power despite its unilateralist tendencies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rise_of_alternative_powers, empirical, 'Assessment of how ''America First'' influences the global power distribution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_foreign_policy_america_first, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_f_tr_t0, us_foreign_policy_america_first, theater_ratio, 0, 0.2).
narrative_ontology:measurement(us_f_tr_t5, us_foreign_policy_america_first, theater_ratio, 5, 0.5).
narrative_ontology:measurement(us_f_tr_t10, us_foreign_policy_america_first, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(us_f_be_t0, us_foreign_policy_america_first, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(us_f_be_t5, us_foreign_policy_america_first, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(us_f_be_t10, us_foreign_policy_america_first, base_extractiveness, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_foreign_policy_america_first, enforcement_mechanism).
narrative_ontology:affects_constraint(us_foreign_policy_america_first, trade_protectionism).
narrative_ontology:affects_constraint(us_foreign_policy_america_first, nato_burden_sharing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
