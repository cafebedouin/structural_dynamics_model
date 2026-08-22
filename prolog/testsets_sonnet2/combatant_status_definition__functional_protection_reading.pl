% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__functional_protection_reading, []).

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
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Common Article 3 Status-Independent Minimum Protections
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This story instantiates the functional-protection reading of the
 *   combatant-status-definition kernel: under Common Article 3 to the Geneva
 *   Conventions, all persons detained in connection with an armed conflict
 *   receive a baseline floor of humane treatment and minimum fair-trial
 *   guarantees, and that floor does not depend on first resolving whether the
 *   detainee qualifies as a lawful combatant. This is deliberately narrow and
 *   ε-invariant: it does not describe the state-centric reading (which makes
 *   POW protections conditional on Article 4 criteria and excludes non-state
 *   actors categorically) or the national-liberation reading (which extends
 *   full combatant status to organized groups fighting
 *   colonial/occupation/racist regimes under AP I Article 1(4)). Those are
 *   different constraints with different beneficiary sets and different ε,
 *   generated separately and linked via network.affects_constraints. Low
 *   extraction here reflects that the floor is close to a genuine
 *   coordination good: it removes an incentive detaining powers would
 *   otherwise have to prolong status disputes as a treatment-withholding
 *   tactic, and virtually no party has a durable structural incentive to
 *   defeat it once accepted, though enforcement still depends on monitoring
 *   and diplomatic pressure rather than automatic compliance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.12).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.28).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Common Article 3 Status-Independent Minimum Protections").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, '9fc3f8a3-4cf7-4893-82fe-e1f62573ba33').
narrative_ontology:cs_kernel_codification('9fc3f8a3-4cf7-4893-82fe-e1f62573ba33', formalized).
narrative_ontology:cs_authority_grounding('9fc3f8a3-4cf7-4893-82fe-e1f62573ba33', lineage).
narrative_ontology:cs_interpretation_layer_present('9fc3f8a3-4cf7-4893-82fe-e1f62573ba33').
narrative_ontology:cs_reading_relation('9fc3f8a3-4cf7-4893-82fe-e1f62573ba33', combatant_status_definition__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('9fc3f8a3-4cf7-4893-82fe-e1f62573ba33', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_axiom('9fc3f8a3-4cf7-4893-82fe-e1f62573ba33', foundational, humane_treatment_is_status_independent).
narrative_ontology:cs_axiom_status(humane_treatment_is_status_independent, holdable).
narrative_ontology:cs_axiom_grounding('9fc3f8a3-4cf7-4893-82fe-e1f62573ba33', humane_treatment_is_status_independent, conventional).
narrative_ontology:cs_axiom('9fc3f8a3-4cf7-4893-82fe-e1f62573ba33', foundational, status_determination_is_not_a_precondition_for_baseline_protection).
narrative_ontology:cs_axiom_status(status_determination_is_not_a_precondition_for_baseline_protection, holdable).
narrative_ontology:cs_axiom_grounding('9fc3f8a3-4cf7-4893-82fe-e1f62573ba33', status_determination_is_not_a_precondition_for_baseline_protection, conventional).
narrative_ontology:cs_reference_frame('9fc3f8a3-4cf7-4893-82fe-e1f62573ba33', common_article_3_customary_floor).
narrative_ontology:cs_drift_state('9fc3f8a3-4cf7-4893-82fe-e1f62573ba33', post_gwot_detention_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9fc3f8a3-4cf7-4893-82fe-e1f62573ba33', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, all_detained_persons).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, captured_irregular_fighters).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, detaining_powers_reputational_standing).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, military_commanders_and_interrogators).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, humane_treatment_is_status_independent).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, fair_trial_minimum_applies_universally).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held by a detaining power in an armed conflict, whether captured as regular soldiers, irregular fighters, or civilians suspected of hostile acts. Under this reading, they receive a humane-treatment and fair-trial floor the moment they are in custody, without any prior adjudication of their combatant status. They cannot compel their own reclassification and depend entirely on the detaining power and outside monitors to apply the floor.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, all_detained_persons, beneficiary,
    powerless, immediate, trapped, global).

% States and, where treaty and custom extend Common Article 3 obligations, non-state parties to a conflict hold and interrogate detainees. They administer detention facilities, decide interrogation practices, and convene any tribunals. They can attempt to argue status ambiguity to delay or dilute treatment, but under this reading that argument does not suspend the baseline floor — they remain bound regardless of how status disputes resolve.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detaining_powers, agenda_setter,
    institutional, generational, constrained, global).

% The International Committee of the Red Cross and similar bodies visit detention sites, document treatment, and press detaining powers to comply with the status-independent floor. They have no coercive power of their own; their leverage is documentation, diplomatic pressure, and reputational cost to noncompliant powers.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, icrc_and_monitoring_bodies, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__functional_protection_reading, icrc_and_monitoring_bodies, agenda_setter).

% Field commanders and interrogators who might otherwise use status ambiguity as interrogation leverage lose that tool under this reading: the floor applies before and regardless of any status determination, foreclosing an argument they could otherwise use to justify coercive or degrading treatment of persons they suspect are unlawful combatants.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, military_commanders_and_interrogators, payer,
    moderate, immediate, constrained, national).

% Legal scholars and state advocates who argue that persons failing Article 4 criteria forfeit combatant-linked protections and fall into a lesser or undefined legal category. This reading treats their status-determination-first framework as irrelevant to the humane-treatment floor; their argument is not adjudicated here, only bypassed.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, unlawful_combatancy_theorists, excluded,
    moderate, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, non-negotiable floor of humane treatment and minimal fair-trial guarantees that applies to every person a party to a conflict detains, so that treatment does not turn on a contested and often unresolvable classification dispute conducted in the middle of hostilities.
% TRANSFER_FUNCTION: Moves a guaranteed minimum of procedural and physical protection to every detainee, funded by a corresponding constraint on detaining powers' interrogation and disposition discretion — detaining powers give up the option of withholding baseline treatment pending status resolution.
% ABSENT_VOICES: Advocates of the state-centric reading, who would object that extending unconditional protection to irregular fighters removes an incentive for combatants to distinguish themselves and comply with the laws of war, are not addressed inside this reading — this reading treats the incentive argument as orthogonal to the humane-treatment floor rather than refuting it.
% DISAPPEARANCE_RATIONALE: If the status-independent floor disappeared, detaining powers would have a live legal argument to withhold humane treatment and fair-trial guarantees pending status determination, converting protection into something contingent on classification proceedings that occur, if at all, well after capture and interrogation — detention practice would visibly shift toward the state-centric reading's precondition model.
% FOUNDING_PROBLEM: Post-1949 conflicts repeatedly produced detainees whose combatant status was disputed or undeterminable in the field, and status disputes were being used to justify withholding any protection at all — the founding problem was to prevent a legal classification gap from becoming a treatment vacuum.
% FOUNDING_PROBLEM_CORROBORATION: ICRC commentary and customary IHL studies, produced by an institution with no custody power and no stake in any detaining power's outcomes, attest the classification-gap problem remains live in irregular and non-international armed conflicts; this corroboration sits outside the detaining powers who administer the floor.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__functional_protection_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__functional_protection_reading_tests).
:- end_tests(combatant_status_definition__functional_protection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness stays low and nearly flat across the interval (0.10 to 0.12) because the floor, once established as customary international law, is not itself a mechanism that extracts value from any party — it is a constraint on discretion, not a transfer of resources. Suppression is moderate and rises slowly (0.20 to 0.28) because enforcement depends on monitoring, diplomatic pressure, and eventually international criminal accountability, and the credibility of that enforcement machinery has hardened somewhat over the measured interval as customary status solidified. Theater ratio is low but drifts upward (0.10 to 0.18), reflecting that some detaining powers perform compliance (visits, paper commissions) while contesting the floor's application to specific detainees in practice — a mild Goodhart drift worth flagging but not yet severe.
 *
 * DIRECTIONALITY LOGIC:
 *   All detained persons are the structural beneficiaries: the floor subsidizes them by removing a precondition (status determination) that could otherwise be used to delay or deny treatment. Detaining powers and their field-level interrogators bear the cost as the payer/agenda-setter split: the detaining power as an institution sets and administers detention policy, but interrogators and commanders in the field bear the immediate loss of a tool (status ambiguity as leverage) that this reading forecloses. The ICRC and monitoring bodies sit as observers whose only leverage is documentation and reputational pressure, not enforcement power in a coercive sense.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (status disputes being weaponized as a treatment-withholding tactic) is assessed as still live rather than resolved and abandoned, so this is not read as mandatrophy: the floor continues to do the job it was built for, in ongoing non-international and irregular conflicts where status disputes remain common. If the founding problem were dead — if status determination reliably preceded treatment in all conflicts without incident — the continued insistence on a status-independent floor would look more like inertial doctrine than active coordination, but current practice does not support that.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    floor_versus_ceiling_ambiguity,
    'Does Common Article 3''s status-independent floor function as a genuine universal minimum (compatible with any resolution of the status question) or does its existence get invoked by some detaining powers to argue that no ADDITIONAL protections are owed once the floor is met, effectively capping treatment at the minimum for contested-status detainees?',
    'Comparative analysis of detention practice across conflicts where status was contested: track whether detainees whose status was later confirmed as lawful-combatant received treatment beyond the Common Article 3 floor promptly, or whether the floor became the practical ceiling pending unresolved status litigation.',
    'If the floor is being used as a ceiling-in-practice, effective extraction is higher than the low ε authored here suggests, and this reading would need re-scoring or a companion story documenting the ceiling-effect as a distinct constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(floor_versus_ceiling_ambiguity, empirical, 'Whether the protective floor is sometimes weaponized as a de facto ceiling for contested-status detainees.').

omega_variable(
    customary_status_versus_treaty_text,
    'Is the status-independent floor genuinely customary international law binding on all parties to a conflict (including non-state armed groups), or does its binding force still depend on treaty ratification and state consent in ways that leave gaps for non-signatory or non-state actors?',
    'ICJ and ICTY/ICTR jurisprudence on the customary status of Common Article 3, and state practice/opinio juris surveys, would resolve whether the floor is genuinely universal or has residual treaty-dependence.',
    'If treaty-dependent gaps exist, this reading''s claim of a truly universal, status-independent floor is partially aspirational rather than fully operative, which would raise the authored extractiveness and suppression figures for detainees held by non-signatory or non-recognized actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(customary_status_versus_treaty_text, empirical, 'Whether the floor''s customary-law status closes all gaps left by treaty consent structures.').

omega_variable(
    kernel_framing_choice,
    'Is the combatant-status-definition kernel better framed as a single contested legal question with three readings (as done here), or does the functional-protection reading actually operate at a different level of abstraction — not a rival classification rule but a meta-rule that constrains how any classification rule may be applied?',
    'Compare how IHL scholarship treats Common Article 3 relative to Article 4/AP I Article 1(4): if treated as operating at a different logical level (procedural floor vs substantive classification), the three-reading kernel structure used here may understate the functional-protection reading''s actual scope.',
    'If the meta-rule framing is more accurate, this reading''s reading_relations to the two classification-focused siblings should lean more heavily toward ''influences'' (constraining how they operate) rather than being read as a peer classification rule alongside them, though the coexists_with/influences choices below already reflect this asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the functional-protection reading is a peer classification rule or a meta-level constraint on classification rules.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t0, combatant_status_definition__functional_protection_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comb_tr_t8, combatant_status_definition__functional_protection_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(comb_tr_t16, combatant_status_definition__functional_protection_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(comb_tr_t24, combatant_status_definition__functional_protection_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(comb_tr_t32, combatant_status_definition__functional_protection_reading, theater_ratio, 32, 0.17).
narrative_ontology:measurement(comb_tr_t40, combatant_status_definition__functional_protection_reading, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(comb_be_t0, combatant_status_definition__functional_protection_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(comb_be_t8, combatant_status_definition__functional_protection_reading, base_extractiveness, 8, 0.1).
narrative_ontology:measurement(comb_be_t16, combatant_status_definition__functional_protection_reading, base_extractiveness, 16, 0.11).
narrative_ontology:measurement(comb_be_t24, combatant_status_definition__functional_protection_reading, base_extractiveness, 24, 0.11).
narrative_ontology:measurement(comb_be_t32, combatant_status_definition__functional_protection_reading, base_extractiveness, 32, 0.12).
narrative_ontology:measurement(comb_be_t40, combatant_status_definition__functional_protection_reading, base_extractiveness, 40, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t0, combatant_status_definition__functional_protection_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(comb_su_t8, combatant_status_definition__functional_protection_reading, suppression_requirement, 8, 0.22).
narrative_ontology:measurement(comb_su_t16, combatant_status_definition__functional_protection_reading, suppression_requirement, 16, 0.24).
narrative_ontology:measurement(comb_su_t24, combatant_status_definition__functional_protection_reading, suppression_requirement, 24, 0.26).
narrative_ontology:measurement(comb_su_t32, combatant_status_definition__functional_protection_reading, suppression_requirement, 32, 0.27).
narrative_ontology:measurement(comb_su_t40, combatant_status_definition__functional_protection_reading, suppression_requirement, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__functional_protection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the combatant_status_definition kernel. The state_centric_reading makes POW protection conditional on Article 4 criteria and categorically excludes non-state actors; this functional_protection_reading holds that a humane-treatment floor applies regardless of that determination, so it structurally influences (without foreclosing) the state-centric reading by removing the treatment stakes from the classification dispute. The national_liberation_reading argues certain non-state groups should be admitted into full combatant status under AP I Article 1(4); this reading coexists with it because both readings can be held simultaneously by the same legal tradition — the floor applies regardless of whether the liberation-movement status question is resolved in favor of inclusion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
