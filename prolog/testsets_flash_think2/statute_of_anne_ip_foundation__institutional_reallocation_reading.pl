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
 *   human_readable: Statute of Anne: Institutional Reallocation of Literary Rights
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is often considered the foundational text of
 *   modern copyright law. This reading focuses on its role as an act of
 *   institutional reallocation, shifting existing literary property rights
 *   from the Stationers' Company's perpetual monopoly to authors for a
 *   limited term. While presented as empowering authors, the practical effect
 *   was often a transfer of these new, time-limited rights to publishers via
 *   assignment, who then became the primary beneficiaries and enforcers of
 *   the new system, while the Stationers' Company became the primary victim
 *   of this reallocation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.68).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.75).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne: Institutional Reallocation of Literary Rights").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'f1558950-8527-437a-ae91-3e2cf910f25c').
narrative_ontology:cs_kernel_codification('f1558950-8527-437a-ae91-3e2cf910f25c', formalized).
narrative_ontology:cs_authority_grounding('f1558950-8527-437a-ae91-3e2cf910f25c', lineage).
narrative_ontology:cs_interpretation_layer_present('f1558950-8527-437a-ae91-3e2cf910f25c').
narrative_ontology:cs_reading_relation('f1558950-8527-437a-ae91-3e2cf910f25c', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1558950-8527-437a-ae91-3e2cf910f25c', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('f1558950-8527-437a-ae91-3e2cf910f25c', foundational, property_rights_are_reallocable_by_statute).
narrative_ontology:cs_axiom_status(property_rights_are_reallocable_by_statute, holdable).
narrative_ontology:cs_axiom_grounding('f1558950-8527-437a-ae91-3e2cf910f25c', property_rights_are_reallocable_by_statute, conventional).
narrative_ontology:cs_axiom('f1558950-8527-437a-ae91-3e2cf910f25c', foundational, institutional_control_defines_rights_enforcement).
narrative_ontology:cs_axiom_status(institutional_control_defines_rights_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('f1558950-8527-437a-ae91-3e2cf910f25c', institutional_control_defines_rights_enforcement, conventional).
narrative_ontology:cs_reference_frame('f1558950-8527-437a-ae91-3e2cf910f25c', pre_statute_monopoly_framework).
narrative_ontology:cs_drift_state('f1558950-8527-437a-ae91-3e2cf910f25c', post_statute_implementation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f1558950-8527-437a-ae91-3e2cf910f25c', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, readers_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained statutory rights to their works for a limited term, a significant shift from the previous system. However, they often assigned these rights to publishers for immediate payment, limiting their long-term benefit.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors, beneficiary,
    moderate, biographical, constrained, national).

% Quickly adapted to the new system by acquiring rights from authors, effectively maintaining control over the lucrative aspects of literary production and distribution, albeit for a limited term rather than in perpetuity. They became the primary enforcers of the new rights.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers, agenda_setter).

% Lost their perpetual monopoly over printing and publishing, which had been enforced through royal charters and guild control. They were forced to adapt to a system of limited-term rights, experiencing a significant loss of institutional power and revenue.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company, payer,
    institutional, generational, trapped, national).

% Enacted the statute to address the Stationers' monopoly and promote learning, establishing a new legal framework for literary property. They set the terms of the reallocation and the new enforcement mechanisms.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliament, agenda_setter,
    institutional, civilizational, analytical, national).

% Benefited from the eventual expansion of the public domain and potentially greater access to diverse works as the Stationers' monopoly was broken. They also experienced the new enforcement of limited-term rights.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, readers_public, beneficiary,
    organized, generational, mobile, national).

% Analyze the historical and legal impact of the Statute, interpreting its role in the evolution of intellectual property law and its effects on institutional power dynamics.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a new, formalized legal framework for literary property, replacing an informal, guild-based monopoly with a statutory system that defined rights and their duration.
% TRANSFER_FUNCTION: Reallocated the right to control the printing and sale of books from the Stationers' Company (perpetual monopoly) to authors (for a limited term), with much of the actual economic benefit subsequently flowing to publishers via assignment.
% ABSENT_VOICES: Future generations of creators and the broader public interest in a robust public domain, who might argue for even shorter terms or more expansive fair use, were not directly represented in the initial legislative process.
% DISAPPEARANCE_RATIONALE: If the Statute of Anne and its principles vanished, the entire edifice of modern copyright law would collapse, leading to a chaotic and undefined landscape for literary and creative works. The institutional structures built around it would cease to function.
% FOUNDING_PROBLEM: The Stationers' Company held a perpetual monopoly over printing, leading to high prices, limited access to books, and a lack of clear rights for authors, hindering the advancement of learning.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts and parliamentary records corroborate the problem of the Stationers' monopoly. However, the extent to which the Statute truly empowered authors versus merely shifting the locus of control to publishers (via assignment) remains a subject of ongoing legal and historical debate among scholars and legal practitioners, outside of the direct beneficiaries.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high (0.68) because the statute fundamentally dispossessed the Stationers' Company of their perpetual rights, transferring significant economic value and control. Suppression is also high (0.75) as the new statutory regime actively suppressed the old monopoly and enforced the new, limited-term rights. The theater ratio is low (0.10) because the statute was a highly functional and effective piece of legislation that achieved its immediate institutional goals, even if the long-term outcomes diverged from its stated intent to benefit authors directly. Accessibility collapse is moderate (0.60) as it dismantled one system of control but established another, albeit with different terms and beneficiaries. Resistance from the Stationers' Company was substantial (0.70) as they fought to retain their traditional privileges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Stationers' Company, the Statute was a highly extractive act that dismantled their established institutional power. From the perspective of authors and publishers, it was a necessary coordination mechanism that rationalized the literary market, even if the benefits were unevenly distributed. Parliament viewed it as a public good, promoting learning and breaking a monopoly. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The Stationers' Company is the clear target (victim) of this constraint, losing their long-held monopoly. Authors are initial beneficiaries, gaining statutory rights, but their effective directionality is complex due to frequent assignment of rights to publishers. Publishers emerge as significant beneficiaries, adapting to the new system to secure rights from authors. Parliament is the agenda-setter, defining the new institutional space. The public benefits from the eventual public domain but also bears the costs of enforced limited monopolies.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creation_vs_reallocation_ambiguity,
    'Did the Statute of Anne primarily create a new form of intellectual property (copyright) or reallocate existing, albeit informally recognized, literary rights?',
    'Detailed historical and legal analysis of pre-Statute common law and Stationers'' Company practices to determine the nature of ''literary property'' before 1710.',
    'If primarily creation, the ''conceptual_emergence_reading'' gains strength, potentially shifting the constraint''s classification towards a more ''rope-like'' origin. If primarily reallocation, this ''institutional_reallocation_reading'' is strongly corroborated, emphasizing the extractive aspect from the Stationers'' Company.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(creation_vs_reallocation_ambiguity, conceptual, 'Ambiguity regarding the Statute''s fundamental nature as either creation or reallocation of rights.').

omega_variable(
    true_beneficiary_authors_vs_publishers,
    'To what extent did the Statute truly benefit authors, versus primarily benefiting publishers who acquired authors'' rights through assignment?',
    'Economic analysis of author contracts and publisher profits in the decades following the Statute''s enactment, compared to pre-Statute conditions.',
    'If authors saw minimal long-term benefit, the ''extractiveness'' metric for authors would increase, and the overall classification would lean more towards a ''snare'' for authors, with publishers as the primary beneficiaries of the extraction. If authors retained significant benefit, the ''tangled_rope'' classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_beneficiary_authors_vs_publishers, empirical, 'The actual distribution of benefits between authors and publishers post-Statute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 1710, 1740).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1710, 0.1).
narrative_ontology:measurement(stat_tr_t1715, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1715, 0.09).
narrative_ontology:measurement(stat_tr_t1720, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1720, 0.09).
narrative_ontology:measurement(stat_tr_t1725, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1725, 0.1).
narrative_ontology:measurement(stat_tr_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1730, 0.1).
narrative_ontology:measurement(stat_tr_t1735, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1735, 0.1).
narrative_ontology:measurement(stat_tr_t1740, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1740, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1710, 0.6).
narrative_ontology:measurement(stat_be_t1715, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1715, 0.62).
narrative_ontology:measurement(stat_be_t1720, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1720, 0.64).
narrative_ontology:measurement(stat_be_t1725, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1725, 0.65).
narrative_ontology:measurement(stat_be_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1730, 0.66).
narrative_ontology:measurement(stat_be_t1735, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1735, 0.67).
narrative_ontology:measurement(stat_be_t1740, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1740, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1710, 0.7).
narrative_ontology:measurement(stat_su_t1715, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1715, 0.71).
narrative_ontology:measurement(stat_su_t1720, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1720, 0.72).
narrative_ontology:measurement(stat_su_t1725, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1725, 0.73).
narrative_ontology:measurement(stat_su_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1730, 0.74).
narrative_ontology:measurement(stat_su_t1735, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1735, 0.74).
narrative_ontology:measurement(stat_su_t1740, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1740, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__entangled_event_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, modern_copyright_law_evolution).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Statute of Anne IP Foundation' kernel, focusing on the institutional reallocation of rights. It is linked to its sibling readings and to the broader evolution of copyright law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
