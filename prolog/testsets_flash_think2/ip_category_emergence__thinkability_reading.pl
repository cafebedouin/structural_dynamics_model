% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__thinkability_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ip_category_emergence__thinkability_reading
 *   human_readable: IP Category Emergence: Thinkability Reading (Post-1710)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This constraint story instantiates the 'thinkability_reading' of the
 *   'ip_category_emergence' kernel. It focuses on the conceptual shift around
 *   1710, when 'ownable expression' became a legally coherent category,
 *   distinct from prior guild privileges or patronage. The constraint
 *   describes the emergence of this conceptual space, rather than the
 *   subsequent enforcement or economic effects of intellectual property. The
 *   low extractiveness, suppression, and theater ratio reflect that the
 *   *emergence* of the category itself is a foundational shift, not an
 *   extractive mechanism in its own right. The claimed type is 'mountain'
 *   because the conceptual coherence, once established, acts as a fixed point
 *   in legal thought, though its 'naturalness' is contested by the presence
 *   of beneficiaries, triggering FSM.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.05).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.05).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, mountain).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "IP Category Emergence: Thinkability Reading (Post-1710)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:emerges_naturally(ip_category_emergence__thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, '8d39016b-b9b1-40ab-a665-d1b6810218d8').
narrative_ontology:cs_kernel_codification('8d39016b-b9b1-40ab-a665-d1b6810218d8', formalized).
narrative_ontology:cs_authority_grounding('8d39016b-b9b1-40ab-a665-d1b6810218d8', lineage).
narrative_ontology:cs_interpretation_layer_present('8d39016b-b9b1-40ab-a665-d1b6810218d8').
narrative_ontology:cs_reading_relation('8d39016b-b9b1-40ab-a665-d1b6810218d8', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d39016b-b9b1-40ab-a665-d1b6810218d8', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('8d39016b-b9b1-40ab-a665-d1b6810218d8', foundational, legal_categories_are_constitutive_of_rights).
narrative_ontology:cs_axiom_status(legal_categories_are_constitutive_of_rights, holdable).
narrative_ontology:cs_axiom_grounding('8d39016b-b9b1-40ab-a665-d1b6810218d8', legal_categories_are_constitutive_of_rights, conventional).
narrative_ontology:cs_reference_frame('8d39016b-b9b1-40ab-a665-d1b6810218d8', post_statute_of_anne_coherence).
narrative_ontology:cs_drift_state('8d39016b-b9b1-40ab-a665-d1b6810218d8', contemporary_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8d39016b-b9b1-40ab-a665-d1b6810218d8', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, publishers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The legislative body that passed the Statute of Anne in 1710, formally establishing 'copy right' as a distinct legal category, thereby making the concept of ownable expression legally coherent.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, parliament_of_great_britain, agenda_setter,
    institutional, generational, analytical, national).

% Benefited from the conceptual emergence of 'ownable expression' as it provided a new legal basis for claiming rights over their creative works, distinct from previous patronage or guild systems.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, authors, beneficiary,
    moderate, biographical, constrained, national).

% Benefited from the new legal coherence as it provided a clearer framework for acquiring and enforcing rights to print and distribute works, stabilizing their investments in literary production.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, publishers, beneficiary,
    powerful, generational, mobile, national).

% Represent the conceptual interest in free access to knowledge and culture. While not an organized voice in 1710, their philosophical position is implicitly challenged by the emergence of a category of private ownership over expression.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, public_domain_advocates, excluded,
    powerless, civilizational, identity_locked, universal).

% Analyze the historical development of intellectual property law, tracing the conceptual shifts and their implications for legal theory and practice.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_historians, observer,
    analytical, generational, analytical, universal).

narrative_ontology:fixing_cost_class(ip_category_emergence__thinkability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a shared conceptual and legal framework for understanding and adjudicating claims over creative works, enabling coordination around the new category of 'ownable expression' where none formally existed before.
% TRANSFER_FUNCTION: The emergence itself did not directly transfer value, but it created the legal and conceptual preconditions for future transfers of rights and economic value from the public domain (or previous common use) to authors and publishers.
% ABSENT_VOICES: Future public domain advocates and those who would philosophically resist the commodification of ideas were not an organized voice in 1710, but their interests in free access were conceptually diminished by the new category of ownership.
% DISAPPEARANCE_RATIONALE: If the concept of 'ownable expression' had never become legally coherent in 1710, the entire edifice of modern intellectual property law and the industries built upon it would not exist, fundamentally reorganizing the global economy of information and culture.
% FOUNDING_PROBLEM: The lack of a clear, legally coherent framework for authors to claim rights over their works, leading to disputes over literary property and economic precarity for creators in the absence of formal protection.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and scholars of intellectual property universally acknowledge the pre-1710 ambiguities and the Statute of Anne's role in establishing a new legal category. Economic analyses of the early modern publishing industry corroborate the need for clearer rights to incentivize creation and investment.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__thinkability_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__thinkability_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, ExtMetricName, E),
    domain_priors:suppression_score(ip_category_emergence__thinkability_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ip_category_emergence__thinkability_reading),
    narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ip_category_emergence__thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics are set low because the constraint describes a conceptual emergence, not an active, ongoing extractive or suppressive mechanism. The 'thinkability' of a legal category is a precondition for its operation, not the operation itself. Accessibility collapse is high because the previous state of 'unthinkability' for a distinct 'copy right' largely collapsed once the Statute of Anne provided the legal vocabulary and framework. Resistance is low because resistance would be against the *application* of IP rights, not the conceptual emergence of the category itself.
 *
 * PERSPECTIVAL GAP:
 *   The 'thinkability_reading' emphasizes the conceptual and legal-philosophical shift, which is experienced as a foundational change. Other readings might focus on the economic or social consequences, leading to different classifications for the *effects* of IP, but this story maintains a focus on the *emergence* of the category itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The Parliament of Great Britain is the agenda-setter, formalizing the conceptual shift. Authors and publishers are beneficiaries because the new legal coherence provided a foundation for their claims and business models. Public domain advocates are conceptually excluded, as their position is implicitly undermined by the establishment of private ownership over expression. Legal historians serve as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_coherence,
    'Is the emergence of ''ownable expression'' as a coherent legal category a natural conceptual evolution in legal thought, or a constructed legal framework that primarily benefits identifiable agents (authors, publishers)?',
    'Comparative legal history examining alternative conceptualizations of literary property in other jurisdictions or counterfactual analyses of legal development without the Statute of Anne''s specific framing.',
    'If primarily constructed, the ''mountain'' claim for conceptual coherence is a false summit, and the constraint would be reclassified (e.g., as a ''tangled_rope'' for its foundational role in an extractive system). If genuinely natural, the mountain classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_coherence, conceptual, 'Ambiguity regarding the naturalness of the legal category''s emergence.').

omega_variable(
    conceptual_vs_economic_primacy,
    'Does the conceptual coherence of ''ownable expression'' (this reading) primarily influence the economic and social effects of IP (first_holding_reading), or are these aspects more independent?',
    'Detailed historical analysis of the causal links between the legal-conceptual framework and subsequent economic practices and social norms.',
    'If conceptual coherence strongly influences economic effects, this reading''s classification as a foundational ''mountain'' would underscore its indirect but profound impact on later extractive structures. If independent, the ''mountain'' status would be more isolated from the ''tangled_rope'' aspects of IP enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_economic_primacy, empirical, 'Relationship between conceptual emergence and subsequent economic effects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 1700, 1720).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1700, ip_category_emergence__thinkability_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(ip_c_tr_t1705, ip_category_emergence__thinkability_reading, theater_ratio, 1705, 0.05).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__thinkability_reading, theater_ratio, 1710, 0.05).
narrative_ontology:measurement(ip_c_tr_t1715, ip_category_emergence__thinkability_reading, theater_ratio, 1715, 0.05).
narrative_ontology:measurement(ip_c_tr_t1720, ip_category_emergence__thinkability_reading, theater_ratio, 1720, 0.05).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1700, ip_category_emergence__thinkability_reading, base_extractiveness, 1700, 0.05).
narrative_ontology:measurement(ip_c_be_t1705, ip_category_emergence__thinkability_reading, base_extractiveness, 1705, 0.05).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__thinkability_reading, base_extractiveness, 1710, 0.05).
narrative_ontology:measurement(ip_c_be_t1715, ip_category_emergence__thinkability_reading, base_extractiveness, 1715, 0.05).
narrative_ontology:measurement(ip_c_be_t1720, ip_category_emergence__thinkability_reading, base_extractiveness, 1720, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1700, ip_category_emergence__thinkability_reading, suppression_requirement, 1700, 0.05).
narrative_ontology:measurement(ip_c_su_t1705, ip_category_emergence__thinkability_reading, suppression_requirement, 1705, 0.05).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__thinkability_reading, suppression_requirement, 1710, 0.05).
narrative_ontology:measurement(ip_c_su_t1715, ip_category_emergence__thinkability_reading, suppression_requirement, 1715, 0.05).
narrative_ontology:measurement(ip_c_su_t1720, ip_category_emergence__thinkability_reading, suppression_requirement, 1720, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
