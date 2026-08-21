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
 *   constraint_id: ip_category_emergence__first_holding_reading
 *   human_readable: First Holding of Authorial Rights (1710 Statute of Anne Reading)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This constraint story represents a 'first holding' reading of the
 *   emergence of intellectual property, specifically focusing on the Statute
 *   of Anne (1710) as the moment authors entered the legitimate claimant set
 *   for rights over their works. It emphasizes the shift from a
 *   publisher-centric monopoly to a statutory, author-centric system, marking
 *   a change in who could occupy the 'rights-holder' position. The constraint
 *   is classified as a Tangled Rope because it provided a genuine
 *   coordination function (clear statutory rights) but also involved
 *   significant extraction from the public domain and required active
 *   enforcement against unauthorized printers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.65).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.7).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "First Holding of Authorial Rights (1710 Statute of Anne Reading)").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, '936ac7e9-721f-4244-88ce-013c04d18ed2').
narrative_ontology:cs_kernel_codification('936ac7e9-721f-4244-88ce-013c04d18ed2', formalized).
narrative_ontology:cs_authority_grounding('936ac7e9-721f-4244-88ce-013c04d18ed2', lineage).
narrative_ontology:cs_interpretation_layer_present('936ac7e9-721f-4244-88ce-013c04d18ed2').
narrative_ontology:cs_reading_relation('936ac7e9-721f-4244-88ce-013c04d18ed2', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('936ac7e9-721f-4244-88ce-013c04d18ed2', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('936ac7e9-721f-4244-88ce-013c04d18ed2', foundational, author_as_original_proprietor).
narrative_ontology:cs_axiom_status(author_as_original_proprietor, holdable).
narrative_ontology:cs_axiom_grounding('936ac7e9-721f-4244-88ce-013c04d18ed2', author_as_original_proprietor, conventional).
narrative_ontology:cs_axiom('936ac7e9-721f-4244-88ce-013c04d18ed2', foundational, statutory_grant_as_basis_of_right).
narrative_ontology:cs_axiom_status(statutory_grant_as_basis_of_right, holdable).
narrative_ontology:cs_axiom_grounding('936ac7e9-721f-4244-88ce-013c04d18ed2', statutory_grant_as_basis_of_right, conventional).
narrative_ontology:cs_reference_frame('936ac7e9-721f-4244-88ce-013c04d18ed2', statutory_authorial_right_framework).
narrative_ontology:cs_drift_state('936ac7e9-721f-4244-88ce-013c04d18ed2', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('936ac7e9-721f-4244-88ce-013c04d18ed2', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, authors_as_rights_holders).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, publishers_under_statute).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, public_domain_users).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, unauthorized_printers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% For the first time, authors gained a statutory right to control the reproduction of their works for a limited term, shifting from a publisher-centric system. This provided a new basis for income and professional identity, though enforcement remained challenging.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, authors_as_rights_holders, beneficiary,
    moderate, biographical, constrained, national).

% While losing their perpetual common-law monopoly, publishers adapted to the statutory term, often acquiring rights from authors. They became key enforcers of the new statutory regime, benefiting from clear, albeit time-limited, exclusive rights.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, publishers_under_statute, agenda_setter,
    institutional, generational, constrained, national).

% Lost the immediate ability to freely copy works that would have otherwise entered the public domain or were previously subject only to common-law claims. Their access to knowledge and creative works became subject to new statutory restrictions.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, public_domain_users, payer,
    powerless, generational, constrained, national).

% Were directly targeted by the enforcement mechanisms of the Statute of Anne, facing legal penalties for infringing the new authorial rights. Their business model of reprinting popular works without permission became illegal.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, unauthorized_printers, payer,
    powerless, immediate, trapped, local).

% The former holder of a perpetual monopoly over printing, the Stationers' Company saw its power diminished by the Statute of Anne, which shifted the basis of copyright from their guild privilege to authorial right. They would have preferred the continuation of their common-law rights.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, stationers_company, excluded,
    institutional, generational, constrained, national).

% Analyze the historical shift in intellectual property law, debating whether the Statute of Anne truly created authorial rights or merely codified existing practices, and its long-term impact on creativity and public access.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a clear, statutory framework for the ownership and transfer of literary property, providing a standardized term of protection and a mechanism for authors to benefit from their creations, thereby incentivizing new works.
% TRANSFER_FUNCTION: Transferred exclusive rights to reproduce and distribute literary works from a perpetual common-law monopoly (often held by publishers) to authors for a limited statutory term, with enforcement mechanisms to protect these new rights.
% ABSENT_VOICES: The broader public, who would advocate for maximal access to knowledge and minimal restrictions on copying, were not directly represented in the legislative process that balanced authorial incentives against public domain access. Unauthorized printers, whose livelihoods were directly threatened, also lacked a voice.
% DISAPPEARANCE_RATIONALE: If the Statute of Anne and its principles vanished, the legal basis for authorial rights would collapse, reverting to a fragmented common-law system or a publisher-dominated landscape. The entire publishing industry and creative economy would need to fundamentally reorganize around new principles of ownership and compensation.
% FOUNDING_PROBLEM: The lack of a clear, time-limited statutory right for authors to control their works, leading to disputes over ownership, piracy, and an imbalance of power between authors and publishers, hindering the incentive to create.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and intellectual property scholars widely corroborate the existence of this problem, citing contemporary legal debates, petitions from authors, and the preamble of the Statute of Anne itself. While the specific context has evolved, the underlying tension between authorial incentive and public access remains a live issue.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ip_category_emergence__first_holding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__first_holding_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__first_holding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__first_holding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the new statutory rights restricted public access to works that might otherwise have been freely copied, creating a new form of scarcity. Suppression is also high due to the active legal enforcement required to prevent unauthorized printing and uphold the new rights. Theater ratio is low as the primary function of the statute was genuinely to establish and enforce these rights, not merely to perform. The metrics reflect the immediate post-enactment period where the new regime was actively being established and defended.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authors, the Statute of Anne was a Rope, providing necessary coordination and incentive. From the perspective of the public or unauthorized printers, it was a Snare, imposing new restrictions and costs. This reading acknowledges the coordination function but emphasizes the extractive and suppressive aspects from the perspective of those who bore the costs of the new regime.
 *
 * DIRECTIONALITY LOGIC:
 *   Authors and publishers (who acquired rights from authors) are beneficiaries, gaining new or clarified exclusive rights. The public domain users and unauthorized printers are victims, losing prior freedoms or facing new legal penalties. The Stationers' Company, though powerful, is excluded from the new statutory framework's primary benefits, having lost their perpetual monopoly.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_causality_of_authorial_rights,
    'Did the Statute of Anne truly ''create'' authorial rights, or did it merely codify and formalize pre-existing common-law or customary practices, thereby shifting the legal basis rather than the fundamental concept?',
    'Detailed historical-legal analysis of pre-1710 court cases, publishing contracts, and author petitions to determine the extent of de facto authorial control and recognition prior to the statute.',
    'If it merely codified, the ''first holding'' aspect of this reading is weakened, suggesting a more gradual evolution rather than a sharp emergence. This would shift the constraint''s ''naturalness'' closer to a Mountain (emerging practice) rather than a constructed Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_causality_of_authorial_rights, empirical, 'Ambiguity regarding the Statute of Anne''s role as creator vs. codifier of authorial rights.').

omega_variable(
    first_holding_vs_thinkability_framing,
    'Is the ''first holding'' (authors entering the claimant set) a distinct structural event from the ''thinkability'' (ownable expression becoming conceptually coherent) of intellectual property, or are they two facets of the same underlying shift?',
    'Conceptual analysis of legal and philosophical texts from the period, examining whether the legal recognition of authors as rights-holders necessarily implies a prior or simultaneous conceptual shift in what ''ownable expression'' means.',
    'If the events are inseparable, this reading''s distinctness from the ''thinkability_reading'' is reduced, suggesting a more unified kernel. If separable, it reinforces the ''first holding'' as a unique structural change in the distribution of rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_holding_vs_thinkability_framing, conceptual, 'Distinction between the historical enactment of rights and the conceptual emergence of ownable expression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 1710, 1730).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__first_holding_reading, theater_ratio, 1710, 0.05).
narrative_ontology:measurement(ip_c_tr_t1715, ip_category_emergence__first_holding_reading, theater_ratio, 1715, 0.08).
narrative_ontology:measurement(ip_c_tr_t1720, ip_category_emergence__first_holding_reading, theater_ratio, 1720, 0.1).
narrative_ontology:measurement(ip_c_tr_t1725, ip_category_emergence__first_holding_reading, theater_ratio, 1725, 0.1).
narrative_ontology:measurement(ip_c_tr_t1730, ip_category_emergence__first_holding_reading, theater_ratio, 1730, 0.1).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__first_holding_reading, base_extractiveness, 1710, 0.55).
narrative_ontology:measurement(ip_c_be_t1715, ip_category_emergence__first_holding_reading, base_extractiveness, 1715, 0.6).
narrative_ontology:measurement(ip_c_be_t1720, ip_category_emergence__first_holding_reading, base_extractiveness, 1720, 0.63).
narrative_ontology:measurement(ip_c_be_t1725, ip_category_emergence__first_holding_reading, base_extractiveness, 1725, 0.65).
narrative_ontology:measurement(ip_c_be_t1730, ip_category_emergence__first_holding_reading, base_extractiveness, 1730, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__first_holding_reading, suppression_requirement, 1710, 0.6).
narrative_ontology:measurement(ip_c_su_t1715, ip_category_emergence__first_holding_reading, suppression_requirement, 1715, 0.65).
narrative_ontology:measurement(ip_c_su_t1720, ip_category_emergence__first_holding_reading, suppression_requirement, 1720, 0.68).
narrative_ontology:measurement(ip_c_su_t1725, ip_category_emergence__first_holding_reading, suppression_requirement, 1725, 0.7).
narrative_ontology:measurement(ip_c_su_t1730, ip_category_emergence__first_holding_reading, suppression_requirement, 1730, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__first_holding_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ip_category_emergence' kernel, focusing on the historical 'first holding' of authorial rights. It is linked to sibling readings that emphasize conceptual thinkability or the synchronic/diachronic seam of the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
