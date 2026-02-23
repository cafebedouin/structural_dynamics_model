% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u3_exp_r4
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u3_exp_r4, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u3_exp_r4
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing system relies on researchers and institutions
 *   providing free labor (writing, peer review, editing) to for-profit
 *   publishers. These publishers then consolidate this labor into journals,
 *   which they sell back to the same institutions at extremely high
 *   subscription costs. The system is maintained by the 'publish or perish'
 *   culture of academia, where career advancement (especially tenure) is tied
 *   to publishing in high-prestige, publisher-owned journals.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - provide free labor under career duress.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - capture and monetize the value of academic labor.
 *   - University Libraries/Consortia: Victims and Auditors (institutional/constrained) - forced to pay escalating subscription fees.
 *   - University Administrations: Enforcers and Victims (institutional/constrained) - perpetuate the system through tenure requirements while also bearing its costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u3_exp_r4, 0.68).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u3_exp_r4, 0.75).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u3_exp_r4, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_exp_r4, extractiveness, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_exp_r4, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_exp_r4, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u3_exp_r4, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u3_exp_r4, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u3_exp_r4, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u3_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u3_exp_r4, for_profit_journal_publishers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_exp_r4, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_exp_r4, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_exp_r4, public_research_funders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: The 'publish or perish' mandate makes participation non-optional for career survival, creating a coercive labor extraction system.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: From the publisher's view, they provide a valuable coordination service (quality control, prestige signaling, dissemination) that justifies their business model.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r4, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: The analytical view recognizes both the genuine coordination function and the highly extractive, asymmetric value capture.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: Universities are constrained by the need for prestige, forcing them to both enforce the system (via tenure) and pay its extractive costs (via subscriptions).
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r4, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u3_exp_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r4, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u3_exp_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u3_exp_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the immense value captured by publishers from uncompensated labor and institutional subscriptions. The high suppression (0.75) reflects the powerful lock-in effect of journal prestige and the tenure system, which makes it extremely difficult for academics and institutions to opt-out or use alternative platforms without severe career penalties.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the publisher, who views their role as a necessary Rope for coordinating scientific discourse, and the junior academic, who experiences it as a coercive Snare demanding labor for career survival. The analytical perspective of a library consortium or the system as a whole reveals the Tangled Rope: a system with a legitimate coordination function that has been captured and transformed into a mechanism for asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is unambiguous. Value flows from public funders, university budgets, and the uncompensated labor of academics towards the for-profit publishers. The publishers are the sole structural beneficiaries with arbitrage power. All other key agents are victims, either trapped (academics) or constrained (universities).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic Tangled Rope. To misclassify it as a pure Snare would be to ignore the genuine, albeit increasingly inefficient, coordination function it serves in standardizing and disseminating research. To misclassify it as a Rope would be to ignore the massive, coercive extraction of value. The Tangled Rope classification correctly identifies that a coordination mechanism has been coupled with a powerful rent-seeking apparatus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_control_necessity,
    'Is the current publisher-centric model a necessary cost for maintaining rigorous scientific quality control, or is it primarily rent-seeking on a captured market?',
    'Comparative analysis of article quality, retraction rates, and impact between high-cost subscription journals and high-quality, low-cost open-access alternatives (e.g., arXiv overlay journals).',
    'If the model is proven necessary for quality, the classification leans more towards a high-cost Rope. If quality can be maintained without it, it is a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_control_necessity, empirical, 'Distinguishes the necessary cost of quality control from monopolistic rent-seeking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u3_exp_r4, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1975, academic_peer_review_gatekeeping_u3_exp_r4, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(acad_tr_t2000, academic_peer_review_gatekeeping_u3_exp_r4, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(acad_tr_t2025, academic_peer_review_gatekeeping_u3_exp_r4, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(acad_be_t1975, academic_peer_review_gatekeeping_u3_exp_r4, base_extractiveness, 1975, 0.25).
narrative_ontology:measurement(acad_be_t2000, academic_peer_review_gatekeeping_u3_exp_r4, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(acad_be_t2025, academic_peer_review_gatekeeping_u3_exp_r4, base_extractiveness, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u3_exp_r4, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u3_exp_r4, university_tenure_process).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u3_exp_r4, research_funding_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
