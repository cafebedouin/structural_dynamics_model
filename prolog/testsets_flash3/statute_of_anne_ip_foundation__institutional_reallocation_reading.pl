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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   This constraint story interprets the Statute of Anne (1710) as primarily
 *   an act of institutional reallocation of existing rights, rather than the
 *   creation of an entirely new conceptual category of 'copyright'. It views
 *   the statute as shifting the legal and economic control over printing from
 *   the Stationers' Company's perpetual monopoly to authors (and by
 *   assignment, to publishers) for a fixed term. The core change is who
 *   occupies the institutional space of intellectual property control, with
 *   identifiable beneficiaries (publishers via assignment) and victims (the
 *   Stationers' Company's monopoly).
 *
 * KEY AGENTS:
 *   - authors: Nominal beneficiaries, often assigned rights to publishers.
 *   - publishers_via_assignment: Primary beneficiaries, gained statutory control.
 *   - stationers_company_monopoly: Primary victim, lost perpetual rights.
 *   - readers_public: Indirect beneficiaries of increased access.
 *   - parliament: Agenda-setter, enacted the reallocation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.6).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.7).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne: Institutional Reallocation of IP Rights").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, '8d16e771-2b01-4088-be3a-d064be544a3a').
narrative_ontology:cs_kernel_codification('8d16e771-2b01-4088-be3a-d064be544a3a', formalized).
narrative_ontology:cs_authority_grounding('8d16e771-2b01-4088-be3a-d064be544a3a', lineage).
narrative_ontology:cs_interpretation_layer_present('8d16e771-2b01-4088-be3a-d064be544a3a').
narrative_ontology:cs_reading_relation('8d16e771-2b01-4088-be3a-d064be544a3a', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d16e771-2b01-4088-be3a-d064be544a3a', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('8d16e771-2b01-4088-be3a-d064be544a3a', foundational, ip_rights_are_institutional_allocations).
narrative_ontology:cs_axiom_status(ip_rights_are_institutional_allocations, holdable).
narrative_ontology:cs_axiom_grounding('8d16e771-2b01-4088-be3a-d064be544a3a', ip_rights_are_institutional_allocations, conventional).
narrative_ontology:cs_axiom('8d16e771-2b01-4088-be3a-d064be544a3a', secondary, statute_as_regulatory_intervention).
narrative_ontology:cs_axiom_status(statute_as_regulatory_intervention, holdable).
narrative_ontology:cs_axiom_grounding('8d16e771-2b01-4088-be3a-d064be544a3a', statute_as_regulatory_intervention, instrumental).
narrative_ontology:cs_reference_frame('8d16e771-2b01-4088-be3a-d064be544a3a', pre_statute_common_law_monopoly).
narrative_ontology:cs_drift_state('8d16e771-2b01-4088-be3a-d064be544a3a', post_statute_implementation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8d16e771-2b01-4088-be3a-d064be544a3a', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers_via_assignment).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_monopoly).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, readers_public).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__institutional_reallocation_reading, authorial_right_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nominally granted the 'sole liberty of printing' for a fixed term, but often immediately assigned these rights to publishers for a lump sum, gaining limited direct benefit from the new system.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors, beneficiary,
    moderate, biographical, constrained, national).

% Acquired rights from authors, effectively continuing their control over printing and distribution, but now with a statutory basis and fixed term rather than perpetual common law claims. They benefited from the clarity and enforceability of the new system.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers_via_assignment, agenda_setter,
    powerful, generational, mobile, national).

% Lost its perpetual common law rights and its exclusive control over the book trade, being forced to operate under a statutory, time-limited framework. This was a significant reduction in their institutional power and extractive capacity.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_monopoly, payer,
    institutional, generational, constrained, national).

% Benefited from the eventual increase in the availability of books as the monopoly was broken, and the public domain expanded after the fixed term of copyright expired. However, they bore the costs of initial statutory protection.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, readers_public, beneficiary,
    powerless, generational, mobile, national).

% Enacted the statute to balance the interests of authors, publishers, and the public, aiming to promote learning while regulating the book trade. It established the new legal framework for intellectual property.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliament, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reallocated and clarified the legal basis for printing rights, moving from a guild-based perpetual monopoly to a time-limited statutory right, thereby coordinating the interests of authors, publishers, and the public in the production and dissemination of books.
% TRANSFER_FUNCTION: Transferred the primary legal locus of printing rights from the Stationers' Company to authors, and subsequently, via assignment, to publishers. This transferred economic value and control over literary works.
% ABSENT_VOICES: Independent printers and booksellers who were previously excluded by the Stationers' monopoly, and who would have argued for even greater liberalization of printing rights, were not directly represented in the drafting of the statute.
% DISAPPEARANCE_RATIONALE: If the Statute of Anne vanished, the legal foundation of modern copyright would disappear, leading to a chaotic and uncertain environment for authors and publishers. The entire intellectual property system would need to be re-established, likely reverting to some form of common law or new statutory framework.
% FOUNDING_PROBLEM: The perpetual common law monopoly of the Stationers' Company was seen as stifling learning and creating an unfair system for authors, while also leading to disputes over printing rights.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars widely corroborate that the Stationers' Company's monopoly was a central problem the statute aimed to address. While some aspects of balancing author/publisher/public interests remain live, the specific problem of the Stationers' perpetual monopoly is considered resolved.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is moderate (0.6, decreasing to 0.5) because while the Stationers' Company lost its perpetual monopoly, publishers quickly adapted by acquiring rights from authors, maintaining significant control and profit. Suppression is high (0.7, decreasing to 0.6) as the statute actively suppressed the old common law regime and enforced the new statutory framework, requiring active legal enforcement against infringers and those attempting to circumvent the new system. Theater ratio is low (0.1, increasing to 0.2) as the statute's primary function was direct institutional change, not performative maintenance. The claimed type is 'tangled_rope' because it served a coordination function (clarifying rights) but also involved significant asymmetric extraction (from the Stationers' Company to publishers/authors).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Stationers' Company, the statute was a snare, extracting their long-held perpetual rights. From the perspective of publishers, it was a tangled rope, providing a clear, enforceable framework for their business, albeit with a time limit. Authors, while nominally beneficiaries, often found themselves in a constrained position, quickly assigning their rights.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers (via assignment) are beneficiaries (d near 0.0) as they gained a clear, enforceable, time-limited right that they could exploit. The Stationers' Company is the victim (d near 1.0) as their perpetual monopoly was dismantled. Authors are nominal beneficiaries but often constrained, leading to a directionality closer to symmetric or slightly targeted. Parliament is the agenda-setter, shaping the entire structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the Statute of Anne as a pure 'rope' (simple coordination) by highlighting the significant extraction from the Stationers' Company and the active enforcement required to dismantle their prior system. It also avoids mislabeling it as a pure 'snare' by acknowledging the genuine coordination function of establishing a new, clearer legal framework for intellectual property. The 'tangled_rope' classification captures the hybrid nature of both coordination and asymmetric extraction inherent in this institutional reallocation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_institutional_primacy,
    'Was the primary effect of the Statute of Anne the creation of a new conceptual category of ''copyright'' (conceptual_emergence_reading), or the reallocation of existing institutional rights (institutional_reallocation_reading)?',
    'Analysis of contemporary legal discourse and parliamentary records: did the debates focus on defining a new right or on regulating existing trade practices and monopolies?',
    'If conceptual emergence is primary, the constraint''s extractiveness might be lower (as it created something new rather than taking something old), and its classification might lean more towards a ''rope'' or ''scaffold'' for a new legal concept. If institutional reallocation is primary, the ''tangled_rope'' classification with its associated extraction and suppression is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_vs_institutional_primacy, conceptual, 'Ambiguity regarding the Statute of Anne''s primary impact: conceptual innovation vs. institutional restructuring.').

omega_variable(
    author_benefit_realization,
    'To what extent did authors, the nominal beneficiaries, actually realize the economic benefits of the new statutory rights, given the common practice of assignment to publishers?',
    'Quantitative historical analysis of author contracts and publisher profits in the decades following the statute''s enactment.',
    'If authors consistently received minimal benefit, their directionality would shift further towards ''target'' or ''constrained'', increasing the overall effective extraction of the constraint. If they gained significant leverage, the ''tangled_rope'' classification would be more balanced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(author_benefit_realization, empirical, 'The actual economic benefit realized by authors from the Statute of Anne.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 1710, 1730).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1710, 0.1).
narrative_ontology:measurement(stat_tr_t1715, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1715, 0.12).
narrative_ontology:measurement(stat_tr_t1720, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1720, 0.15).
narrative_ontology:measurement(stat_tr_t1725, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1725, 0.18).
narrative_ontology:measurement(stat_tr_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 1730, 0.2).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1710, 0.6).
narrative_ontology:measurement(stat_be_t1715, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1715, 0.58).
narrative_ontology:measurement(stat_be_t1720, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1720, 0.55).
narrative_ontology:measurement(stat_be_t1725, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1725, 0.53).
narrative_ontology:measurement(stat_be_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 1730, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1710, 0.7).
narrative_ontology:measurement(stat_su_t1715, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1715, 0.68).
narrative_ontology:measurement(stat_su_t1720, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1720, 0.65).
narrative_ontology:measurement(stat_su_t1725, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1725, 0.62).
narrative_ontology:measurement(stat_su_t1730, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 1730, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Statute of Anne's impact, focusing on the reallocation of institutional rights. It is part of a family of constraints that interpret the statute's multifaceted effects. The conceptual_emergence_reading focuses on the creation of a new legal concept, while the entangled_event_reading argues for the inseparability of conceptual and institutional change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
