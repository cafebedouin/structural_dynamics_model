% ============================================================================
% CONSTRAINT STORY: cloudflare_dual_class_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cloudflare_dual_class_asymmetry, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cloudflare_dual_class_asymmetry
 *   human_readable: Cloudflare Dual-Class Voting Control
 *   domain: economic
 *
 * SUMMARY:
 *   Cloudflare employs a dual-class stock structure. Class B shares, held by
 *   founders and management, have 10 votes per share, while Class A shares
 *   offered to the public have only one. This arrangement concentrates voting
 *   power, potentially creating a divergence between management's interests
 *   and those of minority shareholders.
 *
 * KEY AGENTS:
 *   - Cloudflare Founders & Management: Primary beneficiary (institutional/arbitrage) – Benefit from control.
 *   - Class A Shareholders: Primary victims (powerless/trapped) – Limited voting influence.
 *   - Potential Challengers: Secondary victims (powerful/constrained) – Face high barriers to influencing company direction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cloudflare_dual_class_asymmetry, 0.6).
domain_priors:suppression_score(cloudflare_dual_class_asymmetry, 0.7).
domain_priors:theater_ratio(cloudflare_dual_class_asymmetry, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cloudflare_dual_class_asymmetry, extractiveness, 0.6).
narrative_ontology:constraint_metric(cloudflare_dual_class_asymmetry, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(cloudflare_dual_class_asymmetry, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cloudflare_dual_class_asymmetry, tangled_rope).
narrative_ontology:human_readable(cloudflare_dual_class_asymmetry, "Cloudflare Dual-Class Voting Control").
narrative_ontology:topic_domain(cloudflare_dual_class_asymmetry, "economic").

domain_priors:requires_active_enforcement(cloudflare_dual_class_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cloudflare_dual_class_asymmetry, cloudflare_founders_management).
narrative_ontology:constraint_victim(cloudflare_dual_class_asymmetry, class_a_shareholders).
narrative_ontology:constraint_victim(cloudflare_dual_class_asymmetry, potential_challengers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Class A shareholders are locked into a system where their voting power is significantly diluted. They are essentially trapped because selling their shares would mean realizing a loss of influence on the company's direction.
constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% The founders and management benefit from this structure by maintaining control over the company's direction, even with reduced equity ownership. They can effectively arbitrage this control to their advantage.
constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% An analytical observer sees the dual-class structure as a tangled rope because it provides a coordination mechanism (clear leadership) but also extracts from other shareholders by limiting their influence.  It requires active enforcement through the legal and corporate governance systems.
constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cloudflare_dual_class_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cloudflare_dual_class_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cloudflare_dual_class_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.60. The dual-class structure allows insiders to extract value by prioritizing their interests over those of Class A shareholders, especially in decisions regarding executive compensation, related-party transactions, and strategic direction. Suppression: 0.70. The structure makes it difficult for outside shareholders to challenge management decisions or advocate for changes, as their voting power is diluted. Theater Ratio: 0.30. While there are some performative aspects of corporate governance (e.g., shareholder meetings), the real control lies with the Class B shareholders, so the theater ratio is relatively low.
 *
 * PERSPECTIVAL GAP:
 *   Class A shareholders perceive the structure as a snare, limiting their influence and accountability. Cloudflare's founders and management see it as a rope, enabling them to maintain strategic control and pursue their vision. An analytical observer sees the dual-class structure as a tangled rope, offering a form of coordination (stable leadership) at the expense of shareholder democracy and accountability.
 *
 * DIRECTIONALITY LOGIC:
 *   Cloudflare Founders & Management: Beneficiary + Arbitrage -> Low 'd' value. Class A Shareholders: Victim + Trapped -> High 'd' value. Potential Challengers: Victim + Constrained -> Moderate 'd' value. The structural relationships dictate the level of benefit or extraction experienced by each agent.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_performance_alignment,
    'Does the concentrated control lead to better long-term strategic decisions and value creation, or does it shield management from accountability and lead to value extraction?',
    'Track Cloudflare''s long-term financial performance and strategic decisions compared to peers with different governance structures.  Analyze shareholder returns and key strategic indicators.',
    'If aligned, then the dual-class structure might be justifiable as a beneficial coordination mechanism. If misaligned, it''s a value extraction tool.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_performance_alignment, empirical, 'The extent to which the dual-class structure aligns management incentives with long-term shareholder value creation.').

omega_variable(
    potential_challenger_suppression,
    'Does the dual-class structure deter potential activist investors or acquisition offers that could unlock shareholder value?',
    'Analyze Cloudflare''s vulnerability to activist campaigns or acquisition offers compared to companies with more democratic governance. Consider the premiums paid in acquisitions of companies with similar characteristics.',
    'If it deters value-enhancing interventions, it strengthens the snare classification. If not, the impact on minority shareholders is lessened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potential_challenger_suppression, empirical, 'The extent to which the dual-class structure suppresses external challenges to management control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cloudflare_dual_class_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clou_tr_t0, cloudflare_dual_class_asymmetry, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clou_tr_t5, cloudflare_dual_class_asymmetry, theater_ratio, 5, 0.3).
narrative_ontology:measurement(clou_tr_t10, cloudflare_dual_class_asymmetry, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(clou_be_t0, cloudflare_dual_class_asymmetry, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(clou_be_t5, cloudflare_dual_class_asymmetry, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(clou_be_t10, cloudflare_dual_class_asymmetry, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
