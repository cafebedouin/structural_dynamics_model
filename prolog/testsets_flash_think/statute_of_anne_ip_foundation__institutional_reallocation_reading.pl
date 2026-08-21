% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__institutional_reallocation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__institutional_reallocation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: statute_of_anne_ip_foundation__institutional_reallocation_reading
 *   human_readable: Statute of Anne: Institutional Reallocation of IP Rights
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This constraint story represents the 'institutional reallocation' reading
 *   of the Statute of Anne (1710). This reading emphasizes the statute's role
 *   in shifting existing rights and control over publishing from the
 *   Stationers' Company's perpetual common law monopoly to authors for a
 *   limited statutory term. It focuses on the change in who occupied the
 *   institutional space of intellectual property rights, rather than the
 *   creation of an entirely new conceptual space for copyright. The statute
 *   is claimed as a 'rope' reflecting its framing as a progressive
 *   coordination mechanism for authors, but the metrics reflect the
 *   substantial extraction and suppression involved in dismantling the old
 *   order and establishing the new, which primarily benefited publishers
 *   through assignment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.65).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.75).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne: Institutional Reallocation of IP Rights").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, '9237c734-3b5a-4681-8475-3dc3c46786b8').
narrative_ontology:cs_kernel_codification('9237c734-3b5a-4681-8475-3dc3c46786b8', formalized).
narrative_ontology:cs_authority_grounding('9237c734-3b5a-4681-8475-3dc3c46786b8', lineage).
narrative_ontology:cs_interpretation_layer_present('9237c734-3b5a-4681-8475-3dc3c46786b8').
narrative_ontology:cs_reading_relation('9237c734-3b5a-4681-8475-3dc3c46786b8', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('9237c734-3b5a-4681-8475-3dc3c46786b8', statute_of_anne_ip_foundation__entangled_event_reading, forecloses).
narrative_ontology:cs_axiom('9237c734-3b5a-4681-8475-3dc3c46786b8', foundational, rights_are_reallocatable_institutional_assets).
narrative_ontology:cs_axiom_status(rights_are_reallocatable_institutional_assets, holdable).
narrative_ontology:cs_axiom_grounding('9237c734-3b5a-4681-8475-3dc3c46786b8', rights_are_reallocatable_institutional_assets, conventional).
narrative_ontology:cs_axiom('9237c734-3b5a-4681-8475-3dc3c46786b8', foundational, authors_as_primary_rightsholders).
narrative_ontology:cs_axiom_status(authors_as_primary_rightsholders, holdable).
narrative_ontology:cs_axiom_grounding('9237c734-3b5a-4681-8475-3dc3c46786b8', authors_as_primary_rightsholders, conventional).
narrative_ontology:cs_reference_frame('9237c734-3b5a-4681-8475-3dc3c46786b8', pre_statute_monopoly_system).
narrative_ontology:cs_drift_state('9237c734-3b5a-4681-8475-3dc3c46786b8', post_enactment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9237c734-3b5a-4681-8475-3dc3c46786b8', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The legislative body that enacted the Statute of Anne, establishing a new legal framework for copyright and reallocating existing rights.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliament, agenda_setter,
    institutional, civilizational, analytical, national).

% Gained statutory rights to their works for a limited term, providing a legal basis for their intellectual labor, though often assigning these rights to publishers.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors, beneficiary,
    moderate, biographical, constrained, national).

% Became the primary commercial beneficiaries by acquiring authors' rights through assignment, operating within the new limited-term monopoly framework.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers, agenda_setter).

% Lost its perpetual common law monopoly over printing and publishing, being forced to comply with the new statutory term limits, significantly diminishing its institutional power.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company, payer,
    institutional, generational, constrained, national).

% Benefited from eventual access to works after the statutory term expired, but still bore the cost of the limited monopoly during the term, paying for copyrighted works.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, public, payer,
    powerless, generational, trapped, national).

% Analyze the historical impact and legal implications of the Statute, interpreting its role in the evolution of intellectual property law and its institutional effects.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a clear, limited-term legal right for authors, providing a structured framework for the creation, ownership, and commercial exploitation of literary works, replacing a system dominated by a perpetual corporate monopoly.
% TRANSFER_FUNCTION: Transferred the primary right to control copying from the Stationers' Company's perpetual common law monopoly to authors for a limited statutory term, which was then typically assigned to publishers, allowing them to collect revenue for that term.
% ABSENT_VOICES: The broader reading public, who might have advocated for even shorter terms or immediate public domain access, were not directly represented in the legislative process that shaped the statute.
% DISAPPEARANCE_RATIONALE: If the Statute of Anne had never been enacted, the legal and institutional landscape of intellectual property in Britain and its colonies would have developed along fundamentally different lines, likely retaining common law perpetual rights or an entirely different legislative approach, profoundly altering publishing and authorship.
% FOUNDING_PROBLEM: The Stationers' Company's perpetual common law monopoly was perceived as stifling competition, limiting the availability of books, and failing to adequately reward authors, leading to calls for reform and a more equitable and structured system.
% FOUNDING_PROBLEM_CORROBORATION: Historical parliamentary records, contemporary pamphlets, and analyses by legal historians and economic historians corroborate the problems associated with the Stationers' monopoly and the legislative intent to address them, though the effectiveness and ultimate beneficiaries remain debated.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) reflects the ongoing cost to the public for copyrighted works, now under a limited monopoly, which is still substantial though less than the prior perpetual monopoly. Suppression (0.75) is high due to the active legislative and judicial enforcement required to dismantle the Stationers' Company's entrenched power and establish the new statutory regime. The theater ratio is low (0.10) as the statute represented a genuine and impactful legislative change, not mere performance. Accessibility collapse (0.40) indicates that while the old monopoly was broken, a new, albeit limited, form of exclusive control was established, not a complete opening of access. Resistance (0.70) was significant from the Stationers' Company, who fought to retain their privileges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authors and publishers, the Statute of Anne was a beneficial coordination mechanism (a 'rope') that clarified rights and fostered literary production. From the perspective of the Stationers' Company, it was a highly extractive and suppressive act (a 'snare') that dismantled their established institutional power. The engine's classification will highlight this divergence from the claimed 'rope' based on the high extractiveness and suppression metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament acted as the agenda-setter, legislating the change. Authors and publishers are beneficiaries, as they gained new or more clearly defined rights and commercial opportunities, with publishers becoming the primary commercial beneficiaries through assignment. The Stationers' Company is the victim, losing its perpetual monopoly. The public is a payer, bearing the costs of the limited monopoly, even while gaining eventual access to works.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reallocation_vs_creation_of_rights,
    'Did the Statute of Anne primarily reallocate existing rights (as this reading claims), or did it fundamentally create a new conceptual category of ''copyright''?',
    'Detailed legal-historical analysis of pre-statute common law practices and contemporary legal discourse, comparing the nature of Stationers'' privileges with the statutory rights granted to authors.',
    'If it primarily created a new right, the ''conceptual emergence'' reading gains strength, suggesting a more foundational shift than mere reallocation. This would alter the interpretation of the constraint''s ''founding problem'' and its ''transfer_function''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reallocation_vs_creation_of_rights, conceptual, 'Ambiguity regarding whether the statute reallocated existing rights or created new ones.').

omega_variable(
    separability_of_institutional_and_conceptual_change,
    'Can the institutional reallocation of rights be meaningfully disentangled from the conceptual emergence of modern copyright, or are these two dimensions of the Statute of Anne fundamentally inseparable?',
    'Philosophical and legal-historical arguments examining the causal and logical dependencies between the institutional mechanisms and the underlying conceptual frameworks of intellectual property at the time.',
    'If inseparable, the ''entangled event'' reading gains strength, suggesting that analyzing the institutional reallocation in isolation misses a crucial dimension of the statute''s impact, potentially leading to a different classification that captures the dual nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_of_institutional_and_conceptual_change, conceptual, 'Whether institutional and conceptual changes of the Statute of Anne are separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 1700, 1730).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1700, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1710, 0.1).
narrative_ontology:measurement(stat_tr_t1720, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1720, 0.1).
narrative_ontology:measurement(stat_tr_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1730, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1700, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1700, 0.75).
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1710, 0.6).
narrative_ontology:measurement(stat_be_t1720, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1720, 0.63).
narrative_ontology:measurement(stat_be_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1730, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1700, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1700, 0.6).
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1710, 0.75).
narrative_ontology:measurement(stat_su_t1720, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1720, 0.73).
narrative_ontology:measurement(stat_su_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1730, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Statute of Anne kernel. This 'institutional reallocation' reading focuses on the shift of existing rights and control, while the 'conceptual emergence' reading emphasizes the creation of a new legal concept, and the 'entangled event' reading argues for their inseparability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
