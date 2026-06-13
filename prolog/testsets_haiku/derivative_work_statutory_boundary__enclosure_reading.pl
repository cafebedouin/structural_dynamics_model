% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__enclosure_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Derivative Work Statutory Boundary (Enclosure Reading)
 *   domain: intellectual_property/technology/information_economics
 *
 * SUMMARY:
 *   The enclosure reading interprets the statutory derivative-work boundary
 *   to include ANY use of copyrighted expression in creating new work, even
 *   if the new work is transformative, limited, or fair-use in character.
 *   Under this reading, a songwriter who samples a copyrighted note, a
 *   filmmaker who incorporates archival footage, or a scholar who adapts a
 *   historical text all prepare derivative works pre-creation and must obtain
 *   authorization from the incumbent copyright holder before legally
 *   proceeding. This reading maximizes the licensing gatekeeping power of
 *   incumbents and licensing intermediaries, and it minimizes the space for
 *   downstream creative practice without permission. The constraint is
 *   classified as a snare: extraction is high (0.81 by interval end),
 *   suppression is high (0.79 — the rule operates ex-ante to block creation
 *   itself, not merely to extract fees after the fact), and the theater ratio
 *   (0.42) shows that a growing share of enforcement is devoted to defending
 *   licensing gatekeeping rather than protecting the original author's
 *   attribution or integrity interests.
 *
 * KEY AGENTS:
 *   - incumbent_copyright_holders — institutional power, arbitrage exit, benefits from broad derivative-work definition and licensing control
 *   - licensing_intermediaries — institutional power, mobile exit, collect rents as gatekeepers; profit from every licensing transaction the broad definition generates
 *   - downstream_creators — moderate power, constrained exit, face licensing delays and fees before creation can commence
 *   - independent_remix_artists — powerless, trapped exit, cannot afford licensing fees; work is effectively unreleasable under the constraint
 *   - fair_use_doctrine_proponents — excluded from rulemaking, constrained by burden-of-proof requirements in litigation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.81).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.79).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Derivative Work Statutory Boundary (Enclosure Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property/technology/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, '5cd8fd56-cfa1-4a55-8c51-59ac7fe0228c').
narrative_ontology:cs_kernel_codification('5cd8fd56-cfa1-4a55-8c51-59ac7fe0228c', fixed_text).
narrative_ontology:cs_authority_grounding('5cd8fd56-cfa1-4a55-8c51-59ac7fe0228c', extraction).
narrative_ontology:cs_interpretation_layer_present('5cd8fd56-cfa1-4a55-8c51-59ac7fe0228c').
narrative_ontology:cs_reading_relation('5cd8fd56-cfa1-4a55-8c51-59ac7fe0228c', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('5cd8fd56-cfa1-4a55-8c51-59ac7fe0228c', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('5cd8fd56-cfa1-4a55-8c51-59ac7fe0228c', foundational, incorporation_ipso_facto_derivative).
narrative_ontology:cs_axiom_status(incorporation_ipso_facto_derivative, holdable).
narrative_ontology:cs_axiom_grounding('5cd8fd56-cfa1-4a55-8c51-59ac7fe0228c', incorporation_ipso_facto_derivative, conventional).
narrative_ontology:cs_axiom('5cd8fd56-cfa1-4a55-8c51-59ac7fe0228c', secondary, licensing_authorization_requirement_pre_creation).
narrative_ontology:cs_axiom_status(licensing_authorization_requirement_pre_creation, holdable).
narrative_ontology:cs_axiom_grounding('5cd8fd56-cfa1-4a55-8c51-59ac7fe0228c', licensing_authorization_requirement_pre_creation, instrumental).
narrative_ontology:cs_reference_frame('5cd8fd56-cfa1-4a55-8c51-59ac7fe0228c', statutory_derivative_work_authorship_control).
narrative_ontology:cs_drift_state('5cd8fd56-cfa1-4a55-8c51-59ac7fe0228c', contemporary_post_transformative_use_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5cd8fd56-cfa1-4a55-8c51-59ac7fe0228c', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, downstream_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, transformative_practitioners).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, independent_remix_artists).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, educational_adapters).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness measurement (0.81 at t=40) reflects that the enclosure reading creates an ex-ante licensing bottleneck: downstream creators cannot legally begin work without permission. This is higher-extractiveness than a post-hoc licensing regime (where creation can begin and licensing fees are collected afterward) because it blocks entry itself. The suppression measurement (0.79) reflects that the constraint operates through legal prohibition and liability threat, not merely through fee incentives; creators are actively suppressed from attempting unauthorized incorporation. The suppression_requirement series shows steady rise from 0.62 to 0.79 over the interval, indicating that enforcement infrastructure (takedown systems, litigation threats, licensing-negotiation gatekeeping) has been steadily strengthened to maintain the broad reading. Theater rises from 0.25 to 0.42, indicating that a growing share of enforcement is spent defending licensing gatekeeping (performative licensing-rate-setting, token licensing-availability signals) rather than protecting authorship integrity. Accessibility_collapse (0.76) reflects that once the broad enclosure reading is in place and creators understand it, alternatives (non-infringing creation without permission, fair-use-based downstream work) appear to collapse: creators face binary choice of licensing or abandonment. Resistance (0.68) reflects substantial pushback from creators, scholars, and courts, but this resistance has not yet shifted the institutional reading.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent copyright holder's seat and the downstream creator's seat should compute the constraint as fundamentally different types. From the incumbent's position: the constraint is coordination—it clarifies derivative-work ownership, prevents chaos over new-version authorship, and enables efficient licensing markets. From the downstream creator's position: the constraint is snare—it blocks pre-creation and forces payment for permission to incorporate. The engine computes per-seat classifications from the structural data: the incumbent (beneficiary, institutional power, arbitrage exit) will compute toward rope or coordination; the downstream creator (victim, constrained power, trapped exit) will compute toward snare. This divergence is the measurement the corpus is designed to capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (allocation of derivative-work authorship and control) was real and legitimate when the statute was written. However, the founding_problem_status is now DEAD: the original problem is solved by statutory assignment and registration systems, and by fair-use doctrine (which addresses the case where new authorship deserves protection from original-copyright control). The constraint now persists primarily as a licensing-gatekeeping mechanism, not as an authorship-allocation mechanism. This is classic mandatrophy: the founding problem is gone, but the institutional arrangement persists, repurposed as extraction. The theater_ratio rise (0.25 → 0.42) is evidence of this drift—licensing gatekeeping and regulatory theater are replacing the original authorship-clarity function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_use_boundary_ambiguity,
    'Is transformative use (change in purpose, meaning, message, or audience) sufficient to render incorporation non-infringing, or does any incorporation into new work ipso facto constitute derivative-work preparation?',
    'Convergence via case law (Campbell precedent broadens; courts recognize transformative use as carving infringing derivative works out of the broader class of incorporations) or via statutory amendment (narrowing the statutory definition of derivative work to exclude transformative uses).',
    'If transformative use is recognized as non-infringing, the enclosure reading collapses and the constraint reclassifies as rope or hybrid. If the enclosure reading prevails, transformative uses are only permissible as fair-use affirmative defenses, not as non-infringement ex-ante, preserving the constraint''s bottleneck power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformative_use_boundary_ambiguity, empirical, 'Whether transformation alone defeats derivative-work status.').

omega_variable(
    licensing_transaction_cost_equilibrium,
    'At what licensing-fee level does the transaction cost of obtaining permission become prohibitive for non-commercial and small-scale downstream creators?',
    'Empirical study of licensing-negotiation timescales, fee structures, and adoption barriers for independent creators; cross-jurisdictional comparison with narrower derivative-work definitions (EU transformative-use carve-outs, fair-use expansion in digital jurisprudence).',
    'High transaction costs (long negotiation timescales, per-use fees, minimum license payments) are evidence that the enclosure reading operates as extraction rather than coordination; they support the snare classification and could justify regulatory intervention (compulsory licensing, statutory licensing floors, or derivative-work boundary narrowing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_transaction_cost_equilibrium, empirical, 'Whether licensing fees and transaction costs actually prohibit downstream creation.').

omega_variable(
    reading_contest_epistemic_status,
    'Is the contest between the enclosure reading and the coordination reading a genuine interpretive ambiguity in the statute, or has the enclosure reading been established by stable case law as the binding reading?',
    'Jurisprudential analysis of controlling precedent (Circuit-split evidence, Supreme Court rulings, ALI Restatement positions); legislative history review; international comparison of copyright law interpretations.',
    'If the enclosure reading is established law, then the constraint is a stable institutional arrangement, not a contested margin; if the readings are in genuine conflict, then the constraint''s classification depends on which reading legal institutions settle on, and the corpus should track the contest as it evolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_epistemic_status, conceptual, 'Whether the enclosure reading is settled law or a contested interpretation.').

omega_variable(
    institutional_capture_of_derivative_work_doctrine,
    'Has the derivative-work doctrine been captured by incumbent copyright holders and licensing intermediaries such that the broad enclosure reading serves their rent-extraction interests rather than the statute''s authorship-allocation purpose?',
    'Analysis of lobbying expenditure by copyright-holding industries on derivative-work statutory amendments; tracking of licensing-fee escalation relative to service costs; comparison of licensing-rate negotiation power between incumbents and downstream creators; audit of licensing intermediary profit margins.',
    'Evidence of institutional capture would support reclassification of the constraint from rope (genuine coordination) to snare (captured extraction). Such evidence could justify regulatory remedy (antitrust action, compulsory licensing, or statutory narrowing of derivative-work definition).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_of_derivative_work_doctrine, empirical, 'Whether the doctrine has been captured for incumbent benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(deri_tr_t25, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(deri_tr_t30, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(deri_tr_t35, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(deri_tr_t40, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 5, 0.64).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 10, 0.69).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 15, 0.73).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(deri_be_t25, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement(deri_be_t30, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(deri_be_t35, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 35, 0.81).
narrative_ontology:measurement(deri_be_t40, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 5, 0.67).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(deri_su_t25, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(deri_su_t30, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(deri_su_t35, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 35, 0.79).
narrative_ontology:measurement(deri_su_t40, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__enclosure_reading, 0.18).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, fair_use_doctrine_transformative_use_expansion).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, copyright_licensing_market_extraction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the derivative_work_statutory_boundary kernel. The enclosure reading (high-extraction snare) interprets the statute to require authorization for any incorporation of copyrighted expression. The coordination_reading interprets the same statute to permit transformative, limited, and fair-use incorporations without authorization. These are not measurements of the same constraint from different angles—they are genuinely different constraints with different ε values, different beneficiary/victim structures, and different institutional functions. The kernel is the contested statutory text; the readings are institutional interpretations of it. The corpus models them as separate constraint stories linked by network.affects_constraints to track how the interpretive contest evolves.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__enclosure_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
