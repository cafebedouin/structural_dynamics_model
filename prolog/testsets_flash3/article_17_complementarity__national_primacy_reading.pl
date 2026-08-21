% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__national_primacy_reading, []).

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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: ICC Article 17 Complementarity (National Primacy Reading)
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'national primacy' reading of Article 17
 *   complementarity in the Rome Statute, which governs the International
 *   Criminal Court's jurisdiction. Under this reading, national courts are
 *   presumed adequate unless proven to be a 'sham,' placing a high burden on
 *   the ICC to demonstrate a state's 'unwillingness or inability' to
 *   genuinely prosecute. This interpretation prioritizes state sovereignty
 *   and cooperation, often at the expense of victims in states with weak but
 *   not entirely collapsed judicial systems. The constraint is claimed as a
 *   Rope by its proponents (a necessary coordination mechanism), but its
 *   operation, as described by the metrics, is more extractive for victims
 *   and advocates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.65).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.7).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "ICC Article 17 Complementarity (National Primacy Reading)").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, '64e1e1c7-9a72-4de0-ad3e-b951b6016ea0').
narrative_ontology:cs_kernel_codification('64e1e1c7-9a72-4de0-ad3e-b951b6016ea0', fixed_text).
narrative_ontology:cs_authority_grounding('64e1e1c7-9a72-4de0-ad3e-b951b6016ea0', lineage).
narrative_ontology:cs_interpretation_layer_present('64e1e1c7-9a72-4de0-ad3e-b951b6016ea0').
narrative_ontology:cs_reading_relation('64e1e1c7-9a72-4de0-ad3e-b951b6016ea0', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('64e1e1c7-9a72-4de0-ad3e-b951b6016ea0', foundational, state_sovereignty_primacy_in_justice).
narrative_ontology:cs_axiom_status(state_sovereignty_primacy_in_justice, holdable).
narrative_ontology:cs_axiom_grounding('64e1e1c7-9a72-4de0-ad3e-b951b6016ea0', state_sovereignty_primacy_in_justice, deontological).
narrative_ontology:cs_axiom('64e1e1c7-9a72-4de0-ad3e-b951b6016ea0', foundational, icc_as_court_of_last_resort_only).
narrative_ontology:cs_axiom_status(icc_as_court_of_last_resort_only, holdable).
narrative_ontology:cs_axiom_grounding('64e1e1c7-9a72-4de0-ad3e-b951b6016ea0', icc_as_court_of_last_resort_only, conventional).
narrative_ontology:cs_reference_frame('64e1e1c7-9a72-4de0-ad3e-b951b6016ea0', original_rome_statute_intent_sovereignty_focus).
narrative_ontology:cs_drift_state('64e1e1c7-9a72-4de0-ad3e-b951b6016ea0', contemporary_icc_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('64e1e1c7-9a72-4de0-ad3e-b951b6016ea0', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_of_atrocity_crimes_in_weak_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, international_justice_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states benefit from a high threshold for ICC intervention, preserving their jurisdiction over domestic crimes and minimizing external scrutiny. They actively advocate for a narrow interpretation of 'unwilling or unable'.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, beneficiary,
    institutional, generational, mobile, global).

% National courts are presumed competent and primary under this reading, reinforcing their authority and reducing the likelihood of ICC intervention. They are incentivized to conduct at least nominal proceedings to avoid ICC jurisdiction.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, beneficiary,
    institutional, biographical, constrained, national).

% The ICC is bound by this interpretation, requiring it to demonstrate a state's 'unwillingness or inability' to genuinely prosecute. This places a significant burden on the Court, limiting its caseload and scope of intervention, and prioritizing state cooperation.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_criminal_court, agenda_setter,
    institutional, civilizational, constrained, global).

% These victims bear the cost of a high inadmissibility threshold. If their national courts conduct sham or ineffective proceedings that are not demonstrably 'unwilling or unable' by the ICC's high standard, they are denied international justice and remain without redress.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_of_atrocity_crimes_in_weak_states, payer,
    powerless, immediate, trapped, local).

% Advocates for international accountability find their efforts constrained by this reading, as it prioritizes state sovereignty over immediate victim redress. They must work within a framework that makes ICC intervention difficult, even in cases of clear impunity.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_justice_advocates, payer,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the division of labor between national jurisdictions and the International Criminal Court, ensuring that states retain primary responsibility for prosecuting international crimes while providing a backstop for complete state failure.
% TRANSFER_FUNCTION: Transfers the burden of proof for inadmissibility from states to the ICC, and effectively transfers the responsibility for justice from the international to the national level, even when national capacity or will is weak but not entirely absent.
% ABSENT_VOICES: Victims' rights organizations and human rights defenders who prioritize accountability over state sovereignty are often marginalized in the interpretation of complementarity, as their perspective would push for a lower threshold for ICC intervention.
% DISAPPEARANCE_RATIONALE: If this reading of complementarity vanished, the ICC's jurisdiction would expand significantly, potentially leading to more interventions and a shift in the balance of power between international and national criminal justice systems. States would lose a key defense against international scrutiny.
% FOUNDING_PROBLEM: The Rome Statute sought to balance state sovereignty with the need to end impunity for atrocity crimes, creating a system where the ICC would only act when national systems genuinely failed.
% FOUNDING_PROBLEM_CORROBORATION: Sovereignty-maximizing states and some legal scholars attest that the founding problem of balancing sovereignty is still live and this reading correctly upholds it. International justice advocates and victims' groups argue that the problem of impunity persists due to this reading's high threshold, and that the original intent was more focused on effective accountability.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because this reading effectively shields states with weak but functional judiciaries from ICC intervention, denying justice to victims who cannot obtain it domestically. Suppression (0.70) is also high, as the ICC's ability to act is severely constrained by the high burden of proof. Theater ratio (0.20) is moderate, reflecting that while some national proceedings are genuine, others are conducted primarily to avoid ICC jurisdiction, creating a performative aspect to 'justice'. The metrics show a gradual increase in extractiveness and suppression over time, as this interpretation has solidified and been leveraged by states.
 *
 * PERSPECTIVAL GAP:
 *   The ICC, as the agenda-setter, experiences this constraint as a necessary, albeit challenging, framework for international cooperation. However, victims and advocates experience it as a barrier to justice, where state sovereignty is prioritized over accountability. Sovereignty-maximizing states see it as a legitimate defense of national jurisdiction. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereignty-maximizing states and national judiciaries are clear beneficiaries, as this reading protects their jurisdiction and reduces external oversight. The ICC, while an agenda-setter, is constrained by this reading, facing a high bar for intervention. Victims of atrocity crimes in weak states and international justice advocates are the primary payers, as their access to international justice is significantly curtailed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sham_proceedings_detection,
    'How effectively can the ICC distinguish between genuinely unwilling/unable states and those conducting sham proceedings to avoid jurisdiction under this reading?',
    'Empirical analysis of ICC inadmissibility decisions and subsequent national proceedings, assessing whether states genuinely pursue justice or merely perform compliance.',
    'If sham proceedings are frequently undetected, the effective extractiveness for victims is higher, and the constraint functions more as a Snare. If detection is robust, the constraint''s coordination function is more legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sham_proceedings_detection, empirical, 'The ICC''s capacity to identify and challenge performative national prosecutions.').

omega_variable(
    sovereignty_vs_accountability_balance,
    'Is the balance between state sovereignty and international accountability, as interpreted by this reading, optimal for achieving the Rome Statute''s overall goals?',
    'Conceptual analysis and normative debate among international legal scholars and policymakers, weighing the values of state autonomy against the imperative to end impunity.',
    'A shift in the normative consensus towards greater accountability could lead to a re-interpretation of complementarity, lowering the inadmissibility threshold and reclassifying the constraint towards a more Rope-like function for victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_accountability_balance, preference, 'Normative assessment of the trade-off between state sovereignty and international justice.').

omega_variable(
    interpretation_drift_direction,
    'Is the interpretation of ''unwilling or unable'' drifting towards an even higher threshold for ICC intervention, or is there pressure for a more expansive reading?',
    'Longitudinal study of ICC jurisprudence, state practice, and scholarly commentary on complementarity over time.',
    'If the threshold continues to rise, the constraint becomes more extractive for victims. If pressure for a more expansive reading gains traction, the constraint could become less extractive over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_drift_direction, empirical, 'Tracking the evolution of complementarity jurisprudence and state practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__national_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(arti_tr_t5, article_17_complementarity__national_primacy_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(arti_tr_t10, article_17_complementarity__national_primacy_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(arti_tr_t15, article_17_complementarity__national_primacy_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(arti_tr_t20, article_17_complementarity__national_primacy_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(arti_tr_t25, article_17_complementarity__national_primacy_reading, theater_ratio, 25, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__national_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(arti_be_t5, article_17_complementarity__national_primacy_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(arti_be_t10, article_17_complementarity__national_primacy_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(arti_be_t15, article_17_complementarity__national_primacy_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(arti_be_t20, article_17_complementarity__national_primacy_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(arti_be_t25, article_17_complementarity__national_primacy_reading, base_extractiveness, 25, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__national_primacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(arti_su_t5, article_17_complementarity__national_primacy_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(arti_su_t10, article_17_complementarity__national_primacy_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(arti_su_t15, article_17_complementarity__national_primacy_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(arti_su_t20, article_17_complementarity__national_primacy_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(arti_su_t25, article_17_complementarity__national_primacy_reading, suppression_requirement, 25, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 17 complementarity kernel. Its sibling, 'international_oversight_reading', offers a different interpretation of the balance between state sovereignty and ICC jurisdiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
