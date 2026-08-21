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
 *   human_readable: Emergence of Author as IP Rights-Holder (1710 Statute of Anne Reading)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This constraint is the `first_holding_reading` of the
 *   `ip_category_emergence` kernel. It focuses on the historical moment
 *   (1710, Statute of Anne) when authors gained statutory rights, shifting
 *   the legitimate claimant set from publishers' perpetual monopolies to
 *   authors for a limited term. Sibling readings include
 *   `thinkability_reading` (focus on conceptual coherence of ownable
 *   expression) and `synchronic_diachronic_seam` (examining the relationship
 *   between conceptual emergence and first holding). This reading emphasizes
 *   the concrete legal and economic shift in who could claim and enforce
 *   intellectual property rights.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.65).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.7).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "Emergence of Author as IP Rights-Holder (1710 Statute of Anne Reading)").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, 'a30b6a47-63e4-474b-950c-ff550fdbc758').
narrative_ontology:cs_kernel_codification('a30b6a47-63e4-474b-950c-ff550fdbc758', formalized).
narrative_ontology:cs_authority_grounding('a30b6a47-63e4-474b-950c-ff550fdbc758', lineage).
narrative_ontology:cs_interpretation_layer_present('a30b6a47-63e4-474b-950c-ff550fdbc758').
narrative_ontology:cs_reading_relation('a30b6a47-63e4-474b-950c-ff550fdbc758', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('a30b6a47-63e4-474b-950c-ff550fdbc758', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('a30b6a47-63e4-474b-950c-ff550fdbc758', foundational, author_as_original_proprietor).
narrative_ontology:cs_axiom_status(author_as_original_proprietor, holdable).
narrative_ontology:cs_axiom_grounding('a30b6a47-63e4-474b-950c-ff550fdbc758', author_as_original_proprietor, deontological).
narrative_ontology:cs_axiom('a30b6a47-63e4-474b-950c-ff550fdbc758', foundational, limited_term_for_public_benefit).
narrative_ontology:cs_axiom_status(limited_term_for_public_benefit, holdable).
narrative_ontology:cs_axiom_grounding('a30b6a47-63e4-474b-950c-ff550fdbc758', limited_term_for_public_benefit, conventional).
narrative_ontology:cs_reference_frame('a30b6a47-63e4-474b-950c-ff550fdbc758', statutory_author_rights_framework).
narrative_ontology:cs_drift_state('a30b6a47-63e4-474b-950c-ff550fdbc758', post_statute_of_anne_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a30b6a47-63e4-474b-950c-ff550fdbc758', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, publishers_under_statute).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, public_domain_users).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, printers_losing_monopoly).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained statutory rights to control copying and profit from their works for a limited term, providing incentive for creation but still reliant on publishers for distribution and enforcement of these new rights.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, authors, beneficiary,
    moderate, biographical, constrained, global).

% Adapted from perpetual monopolies to acquiring rights from authors for a limited term, becoming key intermediaries in the new IP system. They actively enforce the statutory rights they acquire from authors.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, publishers_under_statute, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, publishers_under_statute, beneficiary).

% Lost the ability to freely copy and distribute works once they entered the statutory protection regime, now subject to licensing or waiting for the expiration of copyright terms.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, public_domain_users, payer,
    powerless, immediate, trapped, global).

% Members of the Stationers' Company who previously held perpetual monopolies over many works; their rights were curtailed by the Statute of Anne, forcing them to adapt to a limited-term, author-centric system or face legal challenges.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, printers_losing_monopoly, payer,
    powerful, biographical, constrained, national).

% Interpreted and enforced the provisions of the Statute of Anne, adjudicating disputes over copyright infringement and establishing legal precedent for authorial rights, thereby shaping the constraint's operation.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, courts_of_law, agenda_setter,
    institutional, civilizational, analytical, national).

% Legislated the Statute of Anne, establishing the new legal framework for copyright and balancing various interests. Continues to oversee and potentially revise IP law, acting as the ultimate authority.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, parliament, agenda_setter,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__first_holding_reading, publishers_under_statute).
narrative_ontology:fixing_cost_class(ip_category_emergence__first_holding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a legal framework for authors to claim temporary exclusive rights over their works, coordinating the production and distribution of creative works by incentivizing creation and regulating copying, thereby structuring the literary market.
% TRANSFER_FUNCTION: Transferred the right to control copying and profit from literary works from a perpetual printer's monopoly (or the public domain) to authors (and their assignees) for a limited term, creating a new form of economic value.
% ABSENT_VOICES: The broader public who previously had free access to works after initial publication; future generations who would inherit a more restricted public domain. Their interests were not directly represented in the legislative process.
% DISAPPEARANCE_RATIONALE: If the concept of authorial IP rights (as established in 1710) vanished, the entire publishing industry, creative economy, and legal framework for cultural production would collapse and reorganize around different principles of access and compensation.
% FOUNDING_PROBLEM: To balance the interests of authors (incentive to create), publishers (return on investment), and the public (access to knowledge), while breaking the perpetual monopoly of the Stationers' Company and preventing unauthorized copying.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, historians of intellectual property, and contemporary policy debates corroborate the ongoing tension between these interests. Legislative hearing testimony and independent economic analysis from outside the benefiting parties support the continued relevance of these foundational tensions.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the production and distribution of literary works by incentivizing authors, but it also involves significant asymmetric extraction from the public domain (which lost free access) and from printers who lost their perpetual monopolies. The extractiveness (0.65) reflects this re-allocation of value, while suppression (0.70) indicates the active legal enforcement required to maintain the new statutory rights against unauthorized copying. The theater ratio is low (0.15) as the enforcement mechanisms were genuinely functional, not merely performative. The temporal measurements show a rise in both extractiveness and suppression as the Statute of Anne was enacted and its provisions were increasingly enforced over the subsequent decades.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authors, the Statute of Anne was a liberating act, granting them deserved rights and incentivizing creativity. From the perspective of the public or the former monopolists, it represented a new form of enclosure and restriction. The engine's classification will reflect this divergence, showing a beneficial outcome for authors and an extractive one for others, based on the structural roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Authors and publishers (who acquire rights from authors) are the primary beneficiaries, gaining new legal protections and revenue streams. The public, who previously had freer access to works, and printers who lost their perpetual monopolies, are the primary targets of extraction. Parliament and the courts act as agenda-setters, establishing and enforcing the new legal framework. The shift in legal status fundamentally altered the directionality of resource flows.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_first_holding,
    'Is this constraint primarily about the historical ''first holding'' of IP rights by authors, or is it better understood as a conceptual ''thinkability'' shift?',
    'Analysis of legal and philosophical texts from the period: if the discourse primarily concerns the practicalities of claiming and enforcing rights, it supports ''first holding''; if it concerns the very possibility of owning expression, it supports ''thinkability''.',
    'If reclassified as ''thinkability'', the focus shifts from enforcement mechanisms to conceptual coherence, potentially altering the perceived extractiveness and suppression related to the legal framework itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_first_holding, conceptual, 'Distinguishing between the ''first holding'' and ''thinkability'' aspects of IP emergence.').

omega_variable(
    synchronic_diachronic_seam_ambiguity,
    'Are the ''first holding'' and ''thinkability'' aspects of IP emergence truly distinct historical events, or are they merely different temporal framings of a single, complex socio-legal transformation?',
    'Detailed historical-sociological analysis tracing the co-evolution of legal practice, economic structures, and conceptual frameworks. If distinct causal pathways are identified, they are separate; if they are tightly coupled and co-dependent, they are a single seam.',
    'If they are a single seam, the ''first holding'' constraint might be seen as an instantiation of the ''thinkability'' constraint, leading to a more integrated analysis of the kernel''s emergence. If distinct, they remain separate but linked constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synchronic_diachronic_seam_ambiguity, empirical, 'Assessing the synchronic vs. diachronic relationship between IP''s conceptual emergence and its first legal holding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 1700, 1750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1700, ip_category_emergence__first_holding_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__first_holding_reading, theater_ratio, 1710, 0.12).
narrative_ontology:measurement(ip_c_tr_t1720, ip_category_emergence__first_holding_reading, theater_ratio, 1720, 0.14).
narrative_ontology:measurement(ip_c_tr_t1730, ip_category_emergence__first_holding_reading, theater_ratio, 1730, 0.15).
narrative_ontology:measurement(ip_c_tr_t1740, ip_category_emergence__first_holding_reading, theater_ratio, 1740, 0.15).
narrative_ontology:measurement(ip_c_tr_t1750, ip_category_emergence__first_holding_reading, theater_ratio, 1750, 0.15).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1700, ip_category_emergence__first_holding_reading, base_extractiveness, 1700, 0.45).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__first_holding_reading, base_extractiveness, 1710, 0.55).
narrative_ontology:measurement(ip_c_be_t1720, ip_category_emergence__first_holding_reading, base_extractiveness, 1720, 0.6).
narrative_ontology:measurement(ip_c_be_t1730, ip_category_emergence__first_holding_reading, base_extractiveness, 1730, 0.62).
narrative_ontology:measurement(ip_c_be_t1740, ip_category_emergence__first_holding_reading, base_extractiveness, 1740, 0.64).
narrative_ontology:measurement(ip_c_be_t1750, ip_category_emergence__first_holding_reading, base_extractiveness, 1750, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1700, ip_category_emergence__first_holding_reading, suppression_requirement, 1700, 0.5).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__first_holding_reading, suppression_requirement, 1710, 0.6).
narrative_ontology:measurement(ip_c_su_t1720, ip_category_emergence__first_holding_reading, suppression_requirement, 1720, 0.65).
narrative_ontology:measurement(ip_c_su_t1730, ip_category_emergence__first_holding_reading, suppression_requirement, 1730, 0.67).
narrative_ontology:measurement(ip_c_su_t1740, ip_category_emergence__first_holding_reading, suppression_requirement, 1740, 0.69).
narrative_ontology:measurement(ip_c_su_t1750, ip_category_emergence__first_holding_reading, suppression_requirement, 1750, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__first_holding_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
