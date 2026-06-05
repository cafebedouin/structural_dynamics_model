% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u2_sed_r5
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u2_sed_r5, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u2_sed_r5
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing model is a system where the primary producers and
 *   consumers of research (academics and their institutions) provide free
 *   labor (writing, peer review, editing) to for-profit publishers. These
 *   publishers then package the research and sell it back to the same
 *   institutions through expensive journal subscriptions, effectively
 *   creating a tollbooth on publicly funded knowledge. The system is
 *   maintained by the 'publish or perish' culture of academia, where career
 *   advancement is tied to publication in high-prestige, high-cost journals.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - must provide free labor and publish to gain tenure.
 *   - Journal Publishers: Primary beneficiaries (institutional/arbitrage) - capture the value of academic labor and sell it back to the system.
 *   - University Libraries/Consortia: Institutional victims (analytical/constrained) - must pay escalating subscription fees, often at the expense of other resources.
 *   - Senior Academics/Editors: Secondary beneficiaries (powerful/mobile) - uphold the system that grants them prestige and gatekeeping authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_sed_r5, 0.78).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u2_sed_r5, 0.7).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u2_sed_r5, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_sed_r5, extractiveness, 0.78).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_sed_r5, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_sed_r5, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u2_sed_r5, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u2_sed_r5, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u2_sed_r5, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u2_sed_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_sed_r5, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_sed_r5, tenured_senior_academics).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_sed_r5, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_sed_r5, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_sed_r5, the_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Must publish in high-prestige journals to secure tenure, providing free labor (research, review) into a system that extracts value from them and their institution. The 'publish or perish' mandate makes exit nearly impossible without abandoning their career.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Views the system as a highly efficient coordination mechanism for sourcing, vetting, and distributing content. The free labor of academics is the core of the business model, which they can arbitrage for immense profit. From this perspective, it is pure coordination and value creation.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r5, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Recognizes both the claimed coordination function (quality control via peer review) and the severe, asymmetric extraction (paying exorbitant fees for research their own institution produced). The analysis reveals a hybrid system where a coordination rationale masks a highly extractive financial structure.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% A tenured academic who sits on editorial boards. They benefit from the prestige and wield gatekeeping power. For them, the system is a functional rope for maintaining standards and directing the field, with the costs externalized to others.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r5, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u2_sed_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r5, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_sed_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u2_sed_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.78) is extremely high, reflecting the model of acquiring raw materials (research papers, reviews) for free and selling the finished product at a massive markup. Suppression (0.70) is also high; while alternatives like pre-print servers and open-access journals exist, the career-critical prestige economy is overwhelmingly tied to the legacy publishers, making exit a significant professional risk.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a junior professor, the system is a coercive Snare they cannot escape without jeopardizing their career. For a publisher, it is a perfect Rope—a business model that coordinates the dissemination of science for profit. For an analyst or library, it is a Tangled Rope, acknowledging the (debatable) coordination function of peer review while being acutely aware of the unsustainable extractive costs.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is clear: value flows from researchers and their host institutions (universities) to the shareholders of large publishing houses. Junior faculty are the primary targets of extraction, providing the uncompensated labor. Publishers are the primary beneficiaries. Senior academics act as both beneficiaries of the prestige economy and enforcers of its norms.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic case of potential mandatrophy. It presents itself as a Rope for coordinating scientific quality control. However, the Tangled Rope classification, mandated by the high extraction and active enforcement, correctly identifies that this coordination function is coupled to a severe, asymmetric extractive mechanism. Ignoring the extraction would misclassify it as a pure Rope; ignoring the (claimed) coordination function would misclassify it as a pure Snare. The high theater ratio also suggests the quality-control function may be degrading into a prestige-signaling function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_vs_quality,
    'Is the prestige of top-tier journals a reliable proxy for research quality, or is it a self-perpetuating theatrical signal?',
    'Large-scale longitudinal analysis correlating journal impact factor with long-term study replicability, citation decay rates, and retractions.',
    'If prestige is largely theatrical, the constraint is closer to a pure Snare, as its primary coordination claim is false. If it is a reliable quality signal, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_vs_quality, empirical, 'Whether journal prestige is a functional quality signal or theatrical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u2_sed_r5, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1975, academic_peer_review_gatekeeping_u2_sed_r5, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(acad_tr_t2000, academic_peer_review_gatekeeping_u2_sed_r5, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(acad_tr_t2025, academic_peer_review_gatekeeping_u2_sed_r5, theater_ratio, 2025, 0.6).

% Extraction over time
narrative_ontology:measurement(acad_be_t1975, academic_peer_review_gatekeeping_u2_sed_r5, base_extractiveness, 1975, 0.3).
narrative_ontology:measurement(acad_be_t2000, academic_peer_review_gatekeeping_u2_sed_r5, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(acad_be_t2025, academic_peer_review_gatekeeping_u2_sed_r5, base_extractiveness, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u2_sed_r5, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_sed_r5, university_funding_models).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_sed_r5, scientific_research_integrity).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_sed_r5, tenure_and_promotion_systems).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
