% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__deterrence_reading, []).

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
 *   constraint_id: state_killing_legitimacy__deterrence_reading
 *   human_readable: Capital Punishment Justified as Deterrent Signal
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This story instantiates the deterrence reading of the contested
 *   state_killing_legitimacy kernel: execution is justified not by
 *   backward-looking desert (the retributive reading) nor rejected as
 *   categorically impermissible regardless of outcome (the abolition
 *   reading), but forward-looking as a rational signal that lowers the
 *   expected utility of murder for potential future offenders, thereby
 *   protecting an unidentifiable class of future victims. Structurally this
 *   instrumentalizes the offender as a means to a social end distinct from
 *   what they individually deserve — the offender's death is justified by its
 *   effect on OTHERS' future behavior, not by what the offender did. The
 *   empirical support for the deterrent mechanism is genuinely contested
 *   (decades of econometric literature yield weak or null effects in most
 *   jurisdictions), which keeps epsilon moderate rather than extreme: the
 *   coordination story (protecting future victims) is not obviously false,
 *   but neither is it confirmed, and the mechanism persists institutionally
 *   regardless of the evidentiary weakness.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.52).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.68).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "Capital Punishment Justified as Deterrent Signal").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, '62515332-3b29-4406-ae7b-4e98a0008d06').
narrative_ontology:cs_kernel_codification('62515332-3b29-4406-ae7b-4e98a0008d06', distributed).
narrative_ontology:cs_authority_grounding('62515332-3b29-4406-ae7b-4e98a0008d06', distributed).
narrative_ontology:cs_reading_relation('62515332-3b29-4406-ae7b-4e98a0008d06', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('62515332-3b29-4406-ae7b-4e98a0008d06', state_killing_legitimacy__abolition_reading, influences).
narrative_ontology:cs_axiom('62515332-3b29-4406-ae7b-4e98a0008d06', foundational, offender_instrumentalizable_for_aggregate_welfare).
narrative_ontology:cs_axiom_status(offender_instrumentalizable_for_aggregate_welfare, holdable).
narrative_ontology:cs_axiom_grounding('62515332-3b29-4406-ae7b-4e98a0008d06', offender_instrumentalizable_for_aggregate_welfare, instrumental).
narrative_ontology:cs_axiom('62515332-3b29-4406-ae7b-4e98a0008d06', foundational, deterrent_signal_causally_reduces_future_homicide).
narrative_ontology:cs_axiom_status(deterrent_signal_causally_reduces_future_homicide, holdable).
narrative_ontology:cs_axiom_grounding('62515332-3b29-4406-ae7b-4e98a0008d06', deterrent_signal_causally_reduces_future_homicide, empirically_contingent).
narrative_ontology:cs_reference_frame('62515332-3b29-4406-ae7b-4e98a0008d06', utilitarian_penal_calculus).
narrative_ontology:cs_drift_state('62515332-3b29-4406-ae7b-4e98a0008d06', post_meta_analysis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('62515332-3b29-4406-ae7b-4e98a0008d06', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, deterrence_theory_advocates).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, prosecutorial_offices_seeking_capital_convictions).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, wrongfully_convicted_death_row_inmates).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, offenders_families).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, rational_actor_deterrence_model).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, state_authority_to_instrumentalize_punishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks and secures capital sentences, justifying the practice publicly as a rational deterrent that reduces future homicide rates. Controls charging decisions, statutory framing, and the appellate defense of the sentence. Bears no direct cost from execution and collects legitimacy and political capital from appearing tough on violent crime.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, state_prosecuting_authority, agenda_setter,
    institutional, generational, analytical, national).

% Instrumentalized as the visible example the deterrence argument requires — their death is treated not as backward-looking desert but as a forward-looking signal to others. They have no exit once convicted and sentenced; appeals are the only lever, and the deterrence rationale itself has no mechanism requiring proportionality to their individual culpability.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% Bear the full cost of the deterrence signal's error rate. Because the theory justifies execution by its aggregate signaling effect rather than individual verified guilt, wrongful convictions are treated as an acceptable statistical cost of maintaining the deterrent's credibility rather than as a fatal defect in the justification.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, wrongfully_convicted_death_row_inmates, payer,
    powerless, immediate, trapped, local).

% An unidentifiable, diffuse population who would allegedly not be murdered because the execution deterred a would-be killer. They cannot be named, do not organize as a lobbying bloc directly, and the causal link between any specific execution and their survival is unverifiable — they are the beneficiary class the entire justification depends on but the one least able to confirm receipt of the benefit.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, potential_future_victims, beneficiary,
    organized, generational, analytical, national).

% Criminologists, policymakers, and political actors whose careers, legislative platforms, or intellectual reputations are built on the deterrence model. They benefit from the model's continued institutional acceptance regardless of contested empirical support, and actively shape statute and public discourse to sustain it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, deterrence_theory_advocates, beneficiary,
    organized, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__deterrence_reading, deterrence_theory_advocates, agenda_setter).

% Decades of econometric studies on deterrence effects reach conflicting and largely null or weak results; this contested empirical record rarely enters legislative or prosecutorial decision-making with the weight its uncertainty warrants. Researchers publish dissenting findings but are structurally outside the room where charging and sentencing policy is actually set.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, criminology_research_community, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__deterrence_reading, criminology_research_community, observer).

% Bear collateral loss, stigma, and grief from an execution justified not by what their relative did but by the abstract signal the state wishes to send to others. They have no standing in the deterrence calculus at all — their cost is not even counted as a variable in the justification.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, offenders_families, payer,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Purports to solve a genuine collective-action problem: reducing future homicides by raising the expected cost of murder for rational or quasi-rational potential offenders, coordinating social expectation around a credible maximum penalty.
% TRANSFER_FUNCTION: Moves the offender's life from the offender to the state's signaling apparatus; the intended transfer is protection to potential future victims, but the empirical link connecting the two is contested, so what is verifiably transferred is legitimacy and political capital to the prosecuting authority and to deterrence-theory institutions.
% ABSENT_VOICES: Potential future victims cannot testify to having been spared, so the beneficiary class is structurally unable to corroborate the transaction; the criminology research community documenting weak or null deterrence effects is institutionally sidelined from charging and sentencing policy; wrongfully convicted individuals executed under the deterrence rationale cannot object post-execution.
% DISAPPEARANCE_RATIONALE: Abolishing capital punishment under the deterrence rationale specifically would not measurably change homicide rates according to the bulk of contested econometric evidence, suggesting the world would be largely unchanged; but prosecutorial offices, political platforms, and public safety narratives are organized around the deterrence claim, so those institutional arrangements would visibly rearrange. The parties dispute which effect dominates.
% FOUNDING_PROBLEM: The claimed founding problem is unchecked lethal violence: a mechanism was needed to lower the incidence of murder by raising its cost to rational calculating offenders.
% FOUNDING_PROBLEM_CORROBORATION: Prosecuting authorities and deterrence advocates attest the problem remains live and the mechanism functions. Independent criminological meta-analyses (outside the benefiting institutional set) report the deterrent effect is empirically unconfirmed or statistically indistinguishable from zero across most jurisdictions studied, meaning the founding problem's proposed solution mechanism itself lacks external corroboration even where violent crime remains a live concern.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, contested).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__deterrence_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) reflecting genuine contestation: if the deterrent effect were confirmed, the coordination function would be real and extraction lower; if confirmed absent, the practice would be closer to a pure snare on executed offenders dressed in coordination language. Suppression is substantial (0.68) because the apparatus requires active enforcement — capital statutes, appellate defense of sentences, and public communication sustaining the deterrence narrative against a large contested empirical literature. Theater ratio is moderate-rising (0.44 at endpoint) because an increasing share of the justificatory apparatus is rhetorical maintenance of the deterrence claim in the face of accumulating null results, rather than active updating of policy in response to the evidence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state prosecuting authority and deterrence-theory advocates sit near the beneficiary end: they collect legitimacy, career capital, and political payoff from the practice's continuation regardless of confirmed efficacy. Executed offenders and wrongfully convicted inmates sit at the full-target end: trapped, powerless, bearing the entire cost with no mechanism in the justification requiring the harm be proportionate to their own culpability — the deterrence rationale explicitly justifies their death by its effect on OTHERS. Potential future victims are declared beneficiaries but their directionality is analytically unusual: they cannot confirm receipt, so their benefit is asserted rather than demonstrated, which is precisely the empirical gap the omega below is built to hold.
 *
 * MANDATROPHY ANALYSIS:
 *   The deterrence reading resists both over-claiming (treating any contested empirical claim as settled coordination) and under-claiming (dismissing all capital punishment as pure snare) by keeping epsilon moderate and tied explicitly to the state of the evidence. If the deterrent effect were robustly confirmed across jurisdictions, this reading would sit closer to a genuine (if brutal) rope; if robustly disconfirmed and policy persisted anyway, it would collapse toward a snare or piton. The tangled_rope classification captures the current state: real coordination claim, real victims, active enforcement, unresolved efficacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_reading_sibling_structure,
    'This constraint is one reading (deterrence_reading) of the contested state_killing_legitimacy kernel. What would the retributive_reading and abolition_reading change structurally if adopted instead?',
    'Compare the three sibling constraint files: retributive_reading would remove potential_future_victims from beneficiaries entirely (desert is self-contained, no third-party protection claim) and would not gate legitimacy on empirical deterrence evidence at all; abolition_reading would reclassify the entire arrangement as a pure snare with no coordination function, since it holds execution categorically impermissible regardless of desert or utility.',
    'The disagreement between readings is located specifically at the justificatory ground: retributive grounds legitimacy in backward-looking proportional desert (immune to deterrence evidence), deterrence grounds it in forward-looking contested empirical efficacy (this story), and abolition denies legitimacy can be grounded at all. Adopting a different reading does not change the base facts of who is executed — it changes which structural claim justifies it and therefore who counts as a beneficiary, whether epsilon is empirically contingent or fixed, and whether the arrangement can be a tangled_rope at all.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_reading_sibling_structure, conceptual, 'Committer structure: this story is the deterrence reading of a three-reading kernel; documents what the sibling readings would change.').

omega_variable(
    deterrent_effect_empirical_status,
    'Does capital punishment produce a measurable, causally attributable reduction in future homicide rates beyond what life imprisonment achieves?',
    'Meta-analysis of natural experiments (moratoria, abolition events, cross-jurisdictional panel data controlling for confounds) with pre-registered methodology; the existing literature is contested partly due to identification problems (simultaneity, jurisdiction selection effects).',
    'If robustly confirmed, this reading''s coordination claim strengthens substantially and the classification should move toward rope; if robustly disconfirmed, the coordination story is exposed as cover and the classification should move toward snare — the current tangled_rope classification is a direct function of the evidence remaining genuinely unresolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrent_effect_empirical_status, empirical, 'The central contested empirical claim this reading''s legitimacy depends on.').

omega_variable(
    future_victim_beneficiary_verifiability,
    'Can any specific instance of the deterrent mechanism working — a specific person not murdered because of a specific execution''s signaling effect — ever be verified, even in principle?',
    'This is likely irresolvable in principle (a counterfactual non-event cannot be individually verified), which is itself diagnostic: a beneficiary class that can never confirm receipt of a benefit is structurally different from beneficiaries who can and do report the benefit (e.g. the os_marketplace_operator''s revenue in the worked example).',
    'If irresolvable in principle, potential_future_victims should be understood as an asserted rather than demonstrated beneficiary class permanently, which is grounds for treating the coordination claim with permanent rather than temporary skepticism regardless of aggregate statistical findings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(future_victim_beneficiary_verifiability, conceptual, 'Whether the primary beneficiary class can ever verify the benefit it allegedly receives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__deterrence_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(stat_tr_t8, state_killing_legitimacy__deterrence_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(stat_tr_t16, state_killing_legitimacy__deterrence_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(stat_tr_t24, state_killing_legitimacy__deterrence_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(stat_tr_t32, state_killing_legitimacy__deterrence_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__deterrence_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__deterrence_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(stat_be_t8, state_killing_legitimacy__deterrence_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(stat_be_t16, state_killing_legitimacy__deterrence_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(stat_be_t24, state_killing_legitimacy__deterrence_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(stat_be_t32, state_killing_legitimacy__deterrence_reading, base_extractiveness, 32, 0.51).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__deterrence_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__deterrence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t8, state_killing_legitimacy__deterrence_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(stat_su_t16, state_killing_legitimacy__deterrence_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(stat_su_t24, state_killing_legitimacy__deterrence_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(stat_su_t32, state_killing_legitimacy__deterrence_reading, suppression_requirement, 32, 0.66).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__deterrence_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the state_killing_legitimacy kernel, decomposed per the epsilon-invariance principle: retributive_reading grounds legitimacy in backward-looking proportional desert independent of empirical deterrence claims; abolition_reading holds state killing categorically impermissible regardless of desert or utility; this deterrence_reading grounds legitimacy in a contested forward-looking empirical claim about reducing future murders. The three readings share a kernel (the legitimacy of state killing) but diverge on epsilon, beneficiary structure, and classification, and must not be merged into a single averaged constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
