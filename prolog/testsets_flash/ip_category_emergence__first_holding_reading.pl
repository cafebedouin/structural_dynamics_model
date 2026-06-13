% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__first_holding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__first_holding_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ip_category_emergence__first_holding_reading
 *   human_readable: IP Category Emergence: First Holding Reading (1710 Statute of Anne)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This constraint story, 'IP Category Emergence: First Holding Reading
 *   (1710 Statute of Anne)', is one reading of the 'ip_category_emergence'
 *   kernel. It focuses on the Statute of Anne (1710) as the moment when the
 *   author-as-rights-holder entered the legitimate claimant set, marking a
 *   fundamental shift in the occupancy of intellectual property rights. This
 *   reading emphasizes the legal and structural change in who could 'hold'
 *   and enforce these rights, moving away from the Stationers' Company's
 *   perpetual monopoly towards statutory author protection and a limited
 *   term, ultimately benefiting the public domain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.3).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.4).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "IP Category Emergence: First Holding Reading (1710 Statute of Anne)").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, 'e3b3beba-8b05-4ae0-94e1-b30046b4c59d').
narrative_ontology:cs_kernel_codification('e3b3beba-8b05-4ae0-94e1-b30046b4c59d', formalized).
narrative_ontology:cs_authority_grounding('e3b3beba-8b05-4ae0-94e1-b30046b4c59d', lineage).
narrative_ontology:cs_interpretation_layer_present('e3b3beba-8b05-4ae0-94e1-b30046b4c59d').
narrative_ontology:cs_reading_relation('e3b3beba-8b05-4ae0-94e1-b30046b4c59d', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('e3b3beba-8b05-4ae0-94e1-b30046b4c59d', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('e3b3beba-8b05-4ae0-94e1-b30046b4c59d', foundational, author_as_primary_rights_holder).
narrative_ontology:cs_axiom_status(author_as_primary_rights_holder, holdable).
narrative_ontology:cs_axiom_grounding('e3b3beba-8b05-4ae0-94e1-b30046b4c59d', author_as_primary_rights_holder, conventional).
narrative_ontology:cs_axiom('e3b3beba-8b05-4ae0-94e1-b30046b4c59d', secondary, statutory_term_limits_for_public_benefit).
narrative_ontology:cs_axiom_status(statutory_term_limits_for_public_benefit, holdable).
narrative_ontology:cs_axiom_grounding('e3b3beba-8b05-4ae0-94e1-b30046b4c59d', statutory_term_limits_for_public_benefit, instrumental).
narrative_ontology:cs_reference_frame('e3b3beba-8b05-4ae0-94e1-b30046b4c59d', pre_statute_common_law_monopoly).
narrative_ontology:cs_drift_state('e3b3beba-8b05-4ae0-94e1-b30046b4c59d', post_statute_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e3b3beba-8b05-4ae0-94e1-b30046b4c59d', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, authors_as_rights_holders).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, public_domain).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, stationers_monopoly).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ip_category_emergence__first_holding_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__first_holding_reading_tests).
:- end_tests(ip_category_emergence__first_holding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.3) because while it introduced a new form of limited extraction for authors, it simultaneously curtailed the more extensive, perpetual extraction of the Stationers' Company. Suppression is moderate (0.4) as it required active enforcement to break the existing monopoly and establish the new statutory regime. Theater ratio is low (0.1) because the Statute of Anne was a genuinely functional piece of legislation that created a new legal category and system, not merely a performative act. Accessibility collapse is high (0.7) because the Statute fundamentally altered the legal landscape, making the previous common law claims largely inaccessible or irrelevant.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authors, the Statute of Anne was a liberation, establishing their rights. From the Stationers' perspective, it was a significant loss of established privilege and a new constraint on their business model. The engine's classification will reflect this divergence based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Authors became beneficiaries as their rights were legally recognized and protected. The Stationers' Company became a 'payer' in the sense that they lost their perpetual monopoly and had to adapt to a new, more constrained system. The public domain is a beneficiary due to the introduction of term limits. Parliament acted as the agenda-setter, enacting the new legal framework. Readers and consumers also benefited from increased access and competition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_of_holding_vs_thinkability,
    'Did the legal ''holding'' of rights by authors (first_holding_reading) enable the ''thinkability'' of ownable expression, or vice versa?',
    'Detailed historical-legal analysis of pre-1710 intellectual discourse and legislative intent: did the concept of authorial ownership precede or follow the legal mechanism for its enforcement?',
    'If ''holding'' enabled ''thinkability'', this reading''s causal primacy is strengthened. If ''thinkability'' preceded ''holding'', this reading describes an institutionalization of a pre-existing concept, influencing its classification as a response to a prior conceptual shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_of_holding_vs_thinkability, conceptual, 'The causal relationship between the legal mechanism for holding rights and the conceptual coherence of ownable expression.').

omega_variable(
    stationers_resistance_efficacy,
    'To what extent did the Stationers'' Company effectively resist or subvert the intent of the Statute of Anne, and for how long?',
    'Analysis of post-1710 legal cases, lobbying efforts, and publishing practices to determine the actual impact and enforcement challenges of the new statutory regime.',
    'Higher effective resistance would suggest the ''suppression'' metric was initially underestimated, and the transition from monopoly to statutory rights was more contested and prolonged than a single ''first holding'' event implies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stationers_resistance_efficacy, empirical, 'The actual efficacy of the Statute of Anne in immediately curtailing the Stationers'' monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 1710, 1710).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__first_holding_reading, theater_ratio, 1710, 0.1).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__first_holding_reading, base_extractiveness, 1710, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__first_holding_reading, suppression_requirement, 1710, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__first_holding_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ip_category_emergence' kernel. This 'first_holding_reading' emphasizes the shift in who could legally hold IP rights, while the 'thinkability_reading' focuses on the conceptual coherence of ownable expression, and the 'synchronic_diachronic_seam' examines their relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
