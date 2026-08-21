% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 Proportionality Balancing Standard
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality balancing' reading of
 *   Common Article 3 of the Geneva Conventions, which requires humane
 *   treatment for persons not taking an active part in hostilities. This
 *   reading interprets CA3 as mandating a case-by-case assessment to balance
 *   detainee dignity against legitimate security needs, rejecting both
 *   absolute prohibitions on all forms of 'enhanced' interrogation and
 *   unlimited state discretion. It positions courts as gatekeepers, requiring
 *   procedural safeguards and justification for treatment that might
 *   otherwise be considered degrading. The constraint is claimed as a Rope by
 *   its proponents, but its operation as a Tangled Rope is evident in the
 *   asymmetric costs borne by detainees and the active enforcement required
 *   to maintain the balancing act.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.45).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.6).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.45).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 Proportionality Balancing Standard").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, 'b489a5d4-fa00-4e52-a486-023fa704550a').
narrative_ontology:cs_kernel_codification('b489a5d4-fa00-4e52-a486-023fa704550a', fixed_text).
narrative_ontology:cs_authority_grounding('b489a5d4-fa00-4e52-a486-023fa704550a', lineage).
narrative_ontology:cs_interpretation_layer_present('b489a5d4-fa00-4e52-a486-023fa704550a').
narrative_ontology:cs_reading_relation('b489a5d4-fa00-4e52-a486-023fa704550a', humane_treatment_standard__absolute_prohibition, coexists_with).
narrative_ontology:cs_reading_relation('b489a5d4-fa00-4e52-a486-023fa704550a', humane_treatment_standard__contextual_necessity, coexists_with).
narrative_ontology:cs_axiom('b489a5d4-fa00-4e52-a486-023fa704550a', foundational, dignity_and_security_are_balanceable).
narrative_ontology:cs_axiom_status(dignity_and_security_are_balanceable, holdable).
narrative_ontology:cs_axiom_grounding('b489a5d4-fa00-4e52-a486-023fa704550a', dignity_and_security_are_balanceable, deontological).
narrative_ontology:cs_axiom('b489a5d4-fa00-4e52-a486-023fa704550a', secondary, judicial_review_ensures_proportionality).
narrative_ontology:cs_axiom_status(judicial_review_ensures_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('b489a5d4-fa00-4e52-a486-023fa704550a', judicial_review_ensures_proportionality, conventional).
narrative_ontology:cs_reference_frame('b489a5d4-fa00-4e52-a486-023fa704550a', post_geneva_conventions_era).
narrative_ontology:cs_drift_state('b489a5d4-fa00-4e52-a486-023fa704550a', contemporary_counterterrorism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b489a5d4-fa00-4e52-a486-023fa704550a', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, detaining_states).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, military_commanders).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that detain individuals in armed conflict must balance their security needs with obligations under international law. This reading allows them to justify certain measures as proportional, avoiding absolute prohibitions but requiring judicial oversight.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detaining_states, agenda_setter,
    institutional, generational, constrained, global).

% Individuals held in detention, whose treatment is subject to the balancing test. While it offers more protection than unlimited discretion, it still permits some forms of treatment that might be considered degrading under an absolute prohibition standard.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainees, payer,
    powerless, immediate, trapped, local).

% Commanders responsible for interrogations and detention operations benefit from the flexibility this standard offers, allowing them to adapt tactics to perceived security threats, provided they can demonstrate proportionality and procedural safeguards.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, military_commanders, beneficiary,
    powerful, biographical, constrained, regional).

% Advocate for stricter interpretations of humane treatment, viewing proportionality balancing as a potential loophole that can erode fundamental protections. They bear the cost of continuously challenging state practices in courts and public opinion.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, human_rights_advocates, payer,
    organized, generational, constrained, global).

% Serve as gatekeepers, adjudicating whether state practices meet the proportionality balancing standard. Their rulings define the practical limits of the constraint, shaping state behavior through precedent.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, international_courts, agenda_setter,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to navigate the tension between security imperatives and humanitarian obligations in armed conflict, offering a middle ground that avoids both absolute prohibitions and unchecked discretion.
% TRANSFER_FUNCTION: Transfers the burden of proof and justification for certain treatment methods to detaining states, while transferring some risk of degrading treatment to detainees, mediated by judicial review.
% ABSENT_VOICES: Victims of past abuses who would argue for an absolute prohibition standard, emphasizing the non-derogable nature of human dignity. Their voices are often heard through human rights organizations rather than directly in the legal proceedings.
% DISAPPEARANCE_RATIONALE: If this standard vanished, states would likely revert to either absolute prohibition (under pressure from human rights bodies) or, more likely, to a contextual necessity framework, leading to increased discretion and potential for abuse. The legal and operational landscape for detention would be fundamentally altered.
% FOUNDING_PROBLEM: The need to reconcile the legitimate security concerns of states in armed conflict with the fundamental human dignity of detainees, preventing both excessive cruelty and impractical restrictions on military operations.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, military ethicists, and some state legal advisors (outside the direct operational chain of command) corroborate the ongoing tension and the need for a framework to manage it. Human rights bodies acknowledge the problem but dispute this reading's efficacy.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__proportionality_balancing, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).
:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while it prevents the worst abuses, it still permits some forms of treatment that human rights advocates would deem degrading. Suppression (0.6) is necessary to enforce the procedural safeguards and judicial oversight against state impulses for greater discretion. The theater ratio (0.2) is low, as the balancing act and judicial review are genuinely performed, though sometimes imperfectly. Accessibility collapse (0.4) is moderate, as alternatives (absolute prohibition or unlimited discretion) are conceptually available but legally constrained. Resistance (0.5) is ongoing from human rights groups pushing for stricter standards.
 *
 * PERSPECTIVAL GAP:
 *   Detaining states and military commanders perceive this as a necessary and reasonable constraint, allowing operational flexibility while upholding humanitarian principles. Detainees and human rights advocates, however, experience it as a structure that permits too much extraction, viewing the 'balancing' as a mechanism to legitimize practices that erode dignity. International courts, as agenda-setters, mediate these divergent perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Detaining states and military commanders are beneficiaries, as this reading provides a legal framework to justify their actions and avoid absolute prohibitions. Detainees and human rights advocates are payers, bearing the costs of potentially degrading treatment and the continuous effort to challenge state interpretations. International courts act as agenda-setters, defining the boundaries of the balancing test.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (as proponents claim) by highlighting the active enforcement and asymmetric extraction. It also avoids mislabeling it as a Snare by acknowledging the genuine coordination function of providing a legal framework for states. The 'live' status of the founding problem, combined with the 'world_rearranges' disappearance verdict, suggests the constraint's mandate is still relevant, though its implementation is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_independence_efficacy,
    'How effective are international and domestic courts in enforcing the proportionality balancing standard against state security interests?',
    'Empirical analysis of judicial review outcomes, compliance rates with court rulings, and the actual impact of judgments on state detention practices.',
    'If judicial enforcement is weak, the constraint''s effective extractiveness for detainees is higher, and its classification shifts closer to a Snare. If strong, it functions more as a Tangled Rope, with genuine (though imperfect) coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_efficacy, empirical, 'The actual power of courts to constrain state action under this standard.').

omega_variable(
    scope_of_degrading_treatment,
    'Where is the precise line between ''humane treatment'' and ''degrading treatment'' under this proportionality balancing standard, and how consistently is it applied across different jurisdictions and contexts?',
    'Comparative legal analysis of case law, expert testimony on interrogation techniques, and detailed reporting on detention conditions across various states and conflicts.',
    'If the line is consistently drawn to permit practices widely considered degrading, the constraint''s extractiveness is higher. If it effectively prevents such practices, extractiveness is lower. This ambiguity is central to the contest between readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_degrading_treatment, conceptual, 'The interpretive flexibility of ''degrading treatment'' within the balancing framework.').

omega_variable(
    balancing_test_legitimacy,
    'Is the proportionality balancing test itself a legitimate method for protecting fundamental human dignity, or does it inherently compromise non-derogable rights?',
    'Philosophical and ethical debate, evolving international human rights jurisprudence, and the long-term impact of its application on human dignity norms.',
    'If the test is deemed inherently compromising, the ''absolute_prohibition'' reading gains normative force, and this constraint''s legitimacy as a coordination mechanism is undermined, pushing it closer to a Snare from a human rights perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_legitimacy, preference, 'The fundamental normative acceptability of a balancing approach to human dignity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__proportionality_balancing, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(huma_be_t5, humane_treatment_standard__proportionality_balancing, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__proportionality_balancing, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(huma_be_t15, humane_treatment_standard__proportionality_balancing, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__proportionality_balancing, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__proportionality_balancing, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(huma_su_t5, humane_treatment_standard__proportionality_balancing, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__proportionality_balancing, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(huma_su_t15, humane_treatment_standard__proportionality_balancing, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__proportionality_balancing, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, detainee_interrogation_guidelines).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, state_secrecy_laws).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
