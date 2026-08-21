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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: IP Category Emergence: Author as First Holder (1710 Statute of Anne Reading)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the 'first holding' reading of the IP category
 *   emergence kernel, focusing on the historical shift in legal
 *   rights-holding with the 1710 Statute of Anne. It emphasizes the entry of
 *   authors into the legitimate claimant set for literary property, marking a
 *   transition from a publisher-centric (Stationers' Company) monopoly to a
 *   statutory, time-limited authorial right. The constraint is classified as
 *   a Tangled Rope because it established a coordination mechanism for
 *   literary works while simultaneously creating a system of asymmetric
 *   extraction from the public domain and unauthorized printers for the
 *   benefit of authors and their assignees.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.65).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.75).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "IP Category Emergence: Author as First Holder (1710 Statute of Anne Reading)").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).
narrative_ontology:has_sunset_clause(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, '7c905559-9d2f-4e59-945d-4cb1571f8e6a').
narrative_ontology:cs_kernel_codification('7c905559-9d2f-4e59-945d-4cb1571f8e6a', fixed_text).
narrative_ontology:cs_authority_grounding('7c905559-9d2f-4e59-945d-4cb1571f8e6a', lineage).
narrative_ontology:cs_interpretation_layer_present('7c905559-9d2f-4e59-945d-4cb1571f8e6a').
narrative_ontology:cs_reading_relation('7c905559-9d2f-4e59-945d-4cb1571f8e6a', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c905559-9d2f-4e59-945d-4cb1571f8e6a', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('7c905559-9d2f-4e59-945d-4cb1571f8e6a', foundational, author_as_original_proprietor).
narrative_ontology:cs_axiom_status(author_as_original_proprietor, holdable).
narrative_ontology:cs_axiom_grounding('7c905559-9d2f-4e59-945d-4cb1571f8e6a', author_as_original_proprietor, conventional).
narrative_ontology:cs_axiom('7c905559-9d2f-4e59-945d-4cb1571f8e6a', secondary, time_limited_monopoly_for_public_good).
narrative_ontology:cs_axiom_status(time_limited_monopoly_for_public_good, holdable).
narrative_ontology:cs_axiom_grounding('7c905559-9d2f-4e59-945d-4cb1571f8e6a', time_limited_monopoly_for_public_good, instrumental).
narrative_ontology:cs_reference_frame('7c905559-9d2f-4e59-945d-4cb1571f8e6a', statute_of_anne_legal_framework).
narrative_ontology:cs_drift_state('7c905559-9d2f-4e59-945d-4cb1571f8e6a', post_statute_of_anne_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7c905559-9d2f-4e59-945d-4cb1571f8e6a', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, statutory_publishers).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, public_domain).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, unauthorized_printers).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, public_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, stationers_company).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained legal recognition for their creative works, allowing them to license rights to publishers and earn income, but still dependent on publishers for distribution and enforcement. Their works were now protected for a limited term.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, authors, beneficiary,
    moderate, biographical, constrained, national).

% Acquired exclusive rights from authors, allowing them to invest in printing and distribution without fear of immediate copying, replacing the old Stationers' monopoly. They actively enforced these new statutory rights.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, statutory_publishers, agenda_setter,
    institutional, generational, mobile, national).

% Lost their perpetual monopoly over printing and publishing, now subject to statutory author rights and time limits. Their previous legal basis for control was removed.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, stationers_company, payer,
    institutional, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, stationers_company, excluded).

% Their previous practice of reprinting popular works without permission became illegal, leading to legal penalties and the collapse of their business model.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, unauthorized_printers, payer,
    powerless, immediate, trapped, local).

% Gained access to a more diverse range of works due to author incentives, but now had to pay for works that might previously have been freely copied after initial publication, and access was time-limited.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, public_readers, payer,
    moderate, biographical, constrained, national).

% Enacted the Statute of Anne to address perceived market failures and balance interests, establishing a new legal regime for literary property with a fixed term and authorial rights.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, parliament, agenda_setter,
    institutional, generational, arbitrage, national).

% Analyze the historical and philosophical implications of the Statute of Anne, its role in the development of intellectual property law, and its ongoing legacy.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__first_holding_reading, statutory_publishers).
narrative_ontology:fixing_cost_class(ip_category_emergence__first_holding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a clear legal framework for ownership of literary works, incentivizing creation and investment in publishing by defining exclusive rights and their duration, thereby coordinating the market for books.
% TRANSFER_FUNCTION: Transferred the right to control copying and distribution from a de facto publisher monopoly (Stationers' Company) to authors (and their assignees), enabling them to collect rents from their creations for a limited term.
% ABSENT_VOICES: The broader public, who might argue for a more expansive public domain or shorter protection terms, were not directly represented in the drafting process. Unauthorized printers, whose business model was criminalized, were certainly absent from the legislative debate.
% DISAPPEARANCE_RATIONALE: If the Statute of Anne and its principles vanished overnight, the entire modern IP system for literary works would collapse. Publishers would lose their exclusive rights, authors would lose their primary incentive mechanism, and the market for books would reorganize into a chaotic free-for-all, drastically changing the economics of creation and distribution.
% FOUNDING_PROBLEM: The lack of clear, time-limited rights for authors led to perpetual monopolies for publishers (Stationers' Company) and insufficient incentive for new creation, as well as rampant disputes over unauthorized copying.
% FOUNDING_PROBLEM_CORROBORATION: The preamble to the Statute of Anne itself states the problem. Contemporary legal scholarship and historical economic analyses of intellectual property rights corroborate the founding problem and its ongoing relevance, though the specific solutions and their scope remain contested in modern IP debates.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.65) reflects the creation of exclusive rights that limit public access and generate rents for rights-holders. Suppression (0.75) is high due to the necessity of active legal enforcement (e.g., lawsuits against unauthorized printers) to maintain these exclusive rights. The theater ratio is low (0.10) as the enforcement mechanisms were genuinely functional, not merely performative. Accessibility collapse (0.70) is significant as the legal framework effectively criminalized previously common practices of copying. Resistance (0.50) was present from those who benefited from the prior system or advocated for broader public access.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authors and statutory publishers, the Statute of Anne was a necessary coordination mechanism that corrected market failures and incentivized creation. From the perspective of unauthorized printers and the public, it introduced new forms of extraction and limited access to knowledge. The engine's per-seat classification will reflect these divergent experiences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Authors and statutory publishers are the primary beneficiaries, gaining new legal protections and revenue streams. The Stationers' Company, having lost its perpetual monopoly, became a payer/excluded party. Unauthorized printers and the public (as readers) are victims, facing legal penalties or higher prices for works that might otherwise have been freely available. Parliament acts as the agenda-setter, establishing the new legal framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_historical_shift,
    'Is the ''first holding'' (historical legal shift) truly distinct from the ''thinkability'' (conceptual emergence) of intellectual property, or are they two facets of the same underlying phenomenon?',
    'Detailed historical-conceptual analysis comparing the legal discourse of the time with philosophical treatises on ownership and creativity. If the legal framework was enacted before the conceptual framework was fully articulated, it suggests distinctness.',
    'If distinct, this reading stands as a primary historical account. If inseparable, the `first_holding_reading` and `thinkability_reading` might collapse into a single, more complex constraint, or their relationship would be more tightly coupled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_vs_historical_shift, conceptual, 'Distinction between legal enactment and conceptual coherence of IP.').

omega_variable(
    stationers_monopoly_vs_author_rights,
    'To what extent did the Statute of Anne genuinely shift power to authors versus merely re-entrenching a new form of publisher control under a different legal guise?',
    'Analysis of author contracts and publisher practices in the decades following 1710, examining the actual economic leverage of authors versus publishers.',
    'If publisher control remained dominant, the ''beneficiary'' role for authors might be overstated, and the constraint''s extractiveness might be more concentrated on publishers, making it a more direct Snare for authors as well.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stationers_monopoly_vs_author_rights, empirical, 'Actual power dynamics between authors and publishers post-1710.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 1710, 1731).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__first_holding_reading, theater_ratio, 1710, 0.1).
narrative_ontology:measurement(ip_c_tr_t1715, ip_category_emergence__first_holding_reading, theater_ratio, 1715, 0.1).
narrative_ontology:measurement(ip_c_tr_t1720, ip_category_emergence__first_holding_reading, theater_ratio, 1720, 0.1).
narrative_ontology:measurement(ip_c_tr_t1725, ip_category_emergence__first_holding_reading, theater_ratio, 1725, 0.1).
narrative_ontology:measurement(ip_c_tr_t1731, ip_category_emergence__first_holding_reading, theater_ratio, 1731, 0.1).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__first_holding_reading, base_extractiveness, 1710, 0.55).
narrative_ontology:measurement(ip_c_be_t1715, ip_category_emergence__first_holding_reading, base_extractiveness, 1715, 0.6).
narrative_ontology:measurement(ip_c_be_t1720, ip_category_emergence__first_holding_reading, base_extractiveness, 1720, 0.63).
narrative_ontology:measurement(ip_c_be_t1725, ip_category_emergence__first_holding_reading, base_extractiveness, 1725, 0.65).
narrative_ontology:measurement(ip_c_be_t1731, ip_category_emergence__first_holding_reading, base_extractiveness, 1731, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__first_holding_reading, suppression_requirement, 1710, 0.7).
narrative_ontology:measurement(ip_c_su_t1715, ip_category_emergence__first_holding_reading, suppression_requirement, 1715, 0.72).
narrative_ontology:measurement(ip_c_su_t1720, ip_category_emergence__first_holding_reading, suppression_requirement, 1720, 0.74).
narrative_ontology:measurement(ip_c_su_t1725, ip_category_emergence__first_holding_reading, suppression_requirement, 1725, 0.75).
narrative_ontology:measurement(ip_c_su_t1731, ip_category_emergence__first_holding_reading, suppression_requirement, 1731, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__first_holding_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ip_category_emergence' kernel, focusing on the historical legal shift of rights-holding. It is linked to the 'thinkability' reading (conceptual emergence) and the 'synchronic-diachronic seam' reading (relationship between conceptual and historical shifts).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
