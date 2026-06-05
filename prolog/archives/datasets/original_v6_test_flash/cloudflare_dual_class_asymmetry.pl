% ============================================================================
% CONSTRAINT STORY: cloudflare_dual_class_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
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
 *   constraint_id: cloudflare_dual_class_asymmetry
 *   human_readable: Cloudflare Dual-Class Voting Control
 *   domain: economic
 *
 * SUMMARY:
 *   Cloudflare's dual-class stock structure, detailed in its S-1 filing,
 *   concentrates voting power in the hands of its founders and management
 *   through Class B shares (10 votes per share), while public shareholders
 *   receive Class A shares (1 vote per share). This arrangement creates an
 *   asymmetry in control, where a small group can dictate company direction
 *   despite owning a minority of the equity. This constraint story examines
 *   the implications of this structure from various perspectives.
 *
 * KEY AGENTS:
 *   - Cloudflare Founders & Management: Primary beneficiaries (institutional/arbitrage) - Maintain control disproportionate to their equity stake
 *   - Public Shareholders: Primary victims (powerless/trapped) - Limited influence over company direction
 *   - Analytical Observer: Assesses the overall impact of the structure
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
narrative_ontology:constraint_victim(cloudflare_dual_class_asymmetry, public_shareholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Public shareholders are effectively trapped within the corporate structure, with limited ability to influence company direction due to the voting power asymmetry. They are the primary victims, bearing the cost of decisions made without their effective consent.
constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Cloudflare's founders and management benefit from the dual-class structure, maintaining control over the company's strategic direction and governance even with reduced equity ownership. They can arbitrage this control into long-term value creation, but also potentially self-serving decisions.
constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% An analytical observer sees the dual-class structure as a tangled rope. It facilitates long-term strategic planning and shields the company from short-term market pressures (coordination), but also allows the founders to extract value and maintain control disproportionate to their economic stake (extraction).
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
 *   Extractiveness (0.60): Reflects the potential for controlling shareholders to make decisions that benefit themselves at the expense of public shareholders. Suppression (0.70): Indicates the limited ability of public shareholders to challenge the controlling shareholders' decisions. Theater Ratio (0.30): Suggests that while there is some performative accountability to public shareholders (e.g., board representation), the dual-class structure ultimately limits their real influence.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the asymmetry in power and exit options. Founders and management see a rope, enabling them to execute their long-term vision without short-term market pressures. Public shareholders, however, experience a snare, with limited recourse if the controlling shareholders act against their interests. The analytical observer sees the tangled rope, acknowledging both the potential benefits and risks of the dual-class structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the structural relationships. Cloudflare founders and management benefit from the control the dual-class structure provides, giving them a low 'd' value. Public shareholders bear the costs of this arrangement, resulting in a high 'd' value. The analytical observer is more neutral, assessing the overall impact of the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This dual-class structure has elements of coordination and extraction. The classification as tangled rope highlights the mixed nature. It avoids mislabeling pure coordination because there are clearly identifiable victims (public shareholders) who lack power. It avoids mislabeling as pure extraction because there's a coordination element in maintaining long-term strategic direction, albeit with concentrated control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_alignment,
    'To what extent does the founders'' vision align with the long-term interests of all stakeholders, including public shareholders?',
    'Track the company''s strategic decisions and financial performance over time, assessing whether they benefit all stakeholders or primarily the controlling shareholders.',
    'If alignment is high, the dual-class structure may be a beneficial rope, facilitating value creation. If alignment is low, it''s a snare, enabling extraction at the expense of public shareholders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_alignment, empirical, 'Assessment of alignment between founders'' vision and stakeholder interests').

omega_variable(
    corporate_governance_checks,
    'Are there adequate corporate governance mechanisms in place to prevent abuse of power by the controlling shareholders?',
    'Examine the composition and independence of the board of directors, the existence of independent committees, and the company''s policies on related-party transactions.',
    'Strong governance checks mitigate the risks of extraction and shift the classification towards a more balanced tangled rope. Weak governance increases the risk of a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_governance_checks, conceptual, 'Evaluation of corporate governance mechanisms').


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
narrative_ontology:measurement(clou_tr_t10, cloudflare_dual_class_asymmetry, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(clou_be_t0, cloudflare_dual_class_asymmetry, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(clou_be_t5, cloudflare_dual_class_asymmetry, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(clou_be_t10, cloudflare_dual_class_asymmetry, base_extractiveness, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cloudflare_dual_class_asymmetry, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
