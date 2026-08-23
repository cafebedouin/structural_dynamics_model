% ============================================================================
% CONSTRAINT STORY: state_killing_authority__categorical_abolition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__categorical_abolition, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: state_killing_authority__categorical_abolition
 *   human_readable: Categorical Abolition of State Killing
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story represents the categorical abolitionist reading of
 *   the state_killing_authority kernel. The reading asserts that state
 *   killing is inherently impermissible because life is an inalienable right.
 *   The standing arrangement under contest is the death penalty system, which
 *   the abolitionist reads as a high-extraction, high-suppression constraint
 *   on the condemned. The metrics describe the death penalty arrangement as
 *   seen from the abolitionist perspective: it extracts life (extractiveness
 *   0.85), suppresses alternatives through state machinery (suppression 0.9),
 *   and meets significant resistance (resistance 0.7). The abolitionist
 *   claims the prohibition is a mountain (natural law), but the engine will
 *   compute the type of the death penalty arrangement from these metrics. The
 *   beneficiary/victim structure reflects the abolitionist's view: the
 *   condemned are primary beneficiaries of the prohibition, while families
 *   seeking execution and state prosecutors bear its costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.85).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.9).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.85).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, mountain).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "Categorical Abolition of State Killing").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:emerges_naturally(state_killing_authority__categorical_abolition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, '152b266d-cd05-4447-ac4c-d9f6d246fe47').
narrative_ontology:cs_kernel_codification('152b266d-cd05-4447-ac4c-d9f6d246fe47', formalized).
narrative_ontology:cs_authority_grounding('152b266d-cd05-4447-ac4c-d9f6d246fe47', extraction).
narrative_ontology:cs_interpretation_layer_present('152b266d-cd05-4447-ac4c-d9f6d246fe47').
narrative_ontology:cs_reading_relation('152b266d-cd05-4447-ac4c-d9f6d246fe47', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('152b266d-cd05-4447-ac4c-d9f6d246fe47', state_killing_authority__deterrence_instrument, forecloses).
narrative_ontology:cs_axiom('152b266d-cd05-4447-ac4c-d9f6d246fe47', foundational, life_is_inalienable).
narrative_ontology:cs_axiom_status(life_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('152b266d-cd05-4447-ac4c-d9f6d246fe47', life_is_inalienable, deontological).
narrative_ontology:cs_axiom('152b266d-cd05-4447-ac4c-d9f6d246fe47', foundational, state_killing_is_inherently_impermissible).
narrative_ontology:cs_axiom_status(state_killing_is_inherently_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('152b266d-cd05-4447-ac4c-d9f6d246fe47', state_killing_is_inherently_impermissible, deontological).
narrative_ontology:cs_reference_frame('152b266d-cd05-4447-ac4c-d9f6d246fe47', natural_right_to_life).
narrative_ontology:cs_drift_state('152b266d-cd05-4447-ac4c-d9f6d246fe47', contemporary_international_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('152b266d-cd05-4447-ac4c-d9f6d246fe47', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, condemned_persons).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, abolitionist_families).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, human_rights_advocates).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, victims_families_seeking_execution).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, state_prosecutors).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, state_institution).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, right_to_life_inalienable).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, state_killing_inherently_impermissible).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face execution under the standing death penalty arrangement. The abolitionist constraint recognizes their life as inalienable, removing the state's power to kill them. They have no exit from the threat of execution except through legal abolition or clemency.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_persons, beneficiary,
    powerless, immediate, trapped, national).

% Families of murder victims who oppose execution on moral grounds. They are often marginalized by prosecutors and excluded from victim-impact proceedings when they advocate against the death penalty. The abolitionist constraint validates their position.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolitionist_families, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__categorical_abolition, abolitionist_families, excluded).

% Activists and organizations campaigning for universal abolition. They benefit from the constraint's normative force in international law and domestic litigation. Their exit options include shifting focus to other human rights issues.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Families of murder victims who view execution as necessary for justice or closure. They bear the cost of the abolitionist constraint by being denied the retributive outcome they seek. Their exit is constrained by the legal system's refusal to impose death sentences.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, victims_families_seeking_execution, payer,
    moderate, biographical, constrained, national).

% Prosecutors who lose a coercive tool (death penalty leverage) for plea bargaining and deterrence. They bear professional costs in high-profile murder cases where the ultimate sanction is unavailable. They can move to private practice or other jurisdictions.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, state_prosecutors, payer,
    powerful, biographical, mobile, national).

% The state loses the ultimate coercive sanction, which some argue weakens its deterrent capacity and symbolic authority. It retains the power to imprison for life. The state can choose to retain or abolish the death penalty (in federal systems, sub-national units can choose).
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, state_institution, payer,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__categorical_abolition, state_institution, agenda_setter).

% Philosophers and legal scholars who argue that justice requires proportional punishment including death for murder. They are excluded from the abolitionist framework's foundational premise but remain influential in policy debates.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, retributive_theorists, excluded,
    moderate, generational, analytical, global).

% Social scientists studying whether capital punishment deters homicide. They provide empirical input to the policy debate but do not directly bear costs or benefits from the constraint.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, deterrence_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The prohibition coordinates society around the norm that human life is inalienable and the state may not take it, replacing the coordination problem of 'who deserves to die' with the bright-line rule 'no one'.
% TRANSFER_FUNCTION: The constraint transfers the power of life-and-death from the state to the individual as a right, removing the state's authority to execute and moving the moral burden of killing off the collective ledger.
% ABSENT_VOICES: Victims' families who want execution are structurally marginalized in abolitionist discourse; their desire for retributive closure is treated as illegitimate. Future potential victims of murders that might have been deterred are also absent — they cannot speak because the counterfactual murders never occur.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, numerous retentionist jurisdictions would immediately resume or expand executions, the legal architecture of 'cruel and unusual punishment' jurisprudence would collapse, and the normative consensus against state killing in international human rights law would fracture.
% FOUNDING_PROBLEM: The historical problem of arbitrary, discriminatory, and politically motivated state killings — from sovereign's prerogative to judicial lynching — that demonstrated the state cannot be trusted with the power of death.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies (UN Human Rights Committee, European Court of Human Rights), historical records of wrongful executions and racial disparities in death sentencing, and the growing global trend toward abolition (over 100 countries abolished in law or practice) corroborate that the founding problem persists.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__categorical_abolition, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__categorical_abolition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, ExtMetricName, E),
    domain_priors:suppression_score(state_killing_authority__categorical_abolition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(state_killing_authority__categorical_abolition),
    narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(state_killing_authority__categorical_abolition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-maximal because the death penalty literally takes the condemned's life — the most fundamental extraction. Suppression is extremely high because the state monopolizes violence and eliminates the alternative of continued life for the condemned. Theater ratio is moderate: the ritual of due process and 'humane' execution methods perform a legimating function but do not change the fatal outcome. Accessibility collapse is high for the condemned (no exit from death row except abolition/clemency) but lower for other actors. Resistance reflects the sustained abolitionist movement, judicial challenges, and international pressure.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist seat sees the death penalty as a snare (pure extraction). The retributive seat sees it as a mountain (just desert). The deterrence seat sees it as a rope (coordination for safety). The engine computes per-seat classifications from the same structural data — the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned are full targets (d≈1.0) of the death penalty arrangement — they bear the full extraction with trapped exit. Victims' families seeking execution are partial targets (d≈0.6) — they are denied a desired outcome but are not directly killed. State prosecutors and the state institution are agenda-setters who also pay costs (d≈0.4) — they lose a tool but retain vast power. Abolitionist families and human rights advocates are beneficiaries (d≈0.1) — the constraint validates their moral position. The engine will derive directionality from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The death penalty's founding problem (arbitrary state killing) remains live, but the arrangement persists in many jurisdictions. The abolitionist constraint claims to resolve the mandatrophy by eliminating the state's killing power entirely. However, the persistence of the death penalty where it exists suggests the mandatrophy is not resolved — the arrangement continues despite its founding problem being widely recognized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_right,
    'Is the inalienable right to life a genuine natural law (mountain) or a constructed normative commitment that benefits identifiable agents (false summit)?',
    'Cross-cultural and historical analysis of whether the prohibition on state killing emerges independently in diverse legal traditions without diffusion, or whether it tracks power shifts toward human rights regimes.',
    'If natural law, the constraint is a genuine mountain with zero extractiveness. If constructed, the FSM signature may reclassify it as tangled_rope, revealing beneficiaries (abolitionist movements, international NGOs) who gain normative authority from the prohibition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_right, conceptual, 'Whether the categorical prohibition is a natural law or a constructed constraint with beneficiaries.').

omega_variable(
    kernel_reading_structure,
    'How do the three readings of state_killing_authority structurally relate — do they foreclose, coexist, or influence each other?',
    'Analyze whether a single legal framework can simultaneously hold categorical abolition, retributive desert, and deterrence instrumentality. Historical test: jurisdictions that abolished for deterrence reasons (empirical) vs. deontological reasons.',
    'If readings foreclose, the kernel is a site of genuine logical conflict. If they coexist, the kernel is a site of pluralistic contestation. If they influence, the kernel''s drift is driven by empirical/normative pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Structural relations among the three declared readings of the state_killing_authority kernel.').

omega_variable(
    victim_family_split,
    'Are victims'' families who oppose execution a distinct structural cohort from those who seek it, or are they the same agents under different framing?',
    'Longitudinal study of victim family positions before and after sentencing, controlling for prosecutor framing and victim-impact statement procedures.',
    'If distinct cohorts, the beneficiary/victim split is structural. If framing-dependent, the split is manipulable and the constraint''s extraction profile changes with prosecutorial practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_family_split, empirical, 'Whether the victim family split is a stable structural feature or a contingent framing effect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ska_ca_tr_t0, state_killing_authority__categorical_abolition, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ska_ca_tr_t25, state_killing_authority__categorical_abolition, theater_ratio, 25, 0.35).
narrative_ontology:measurement(ska_ca_tr_t50, state_killing_authority__categorical_abolition, theater_ratio, 50, 0.32).
narrative_ontology:measurement(ska_ca_tr_t75, state_killing_authority__categorical_abolition, theater_ratio, 75, 0.3).
narrative_ontology:measurement(ska_ca_tr_t100, state_killing_authority__categorical_abolition, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(ska_ca_be_t0, state_killing_authority__categorical_abolition, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(ska_ca_be_t25, state_killing_authority__categorical_abolition, base_extractiveness, 25, 0.88).
narrative_ontology:measurement(ska_ca_be_t50, state_killing_authority__categorical_abolition, base_extractiveness, 50, 0.86).
narrative_ontology:measurement(ska_ca_be_t75, state_killing_authority__categorical_abolition, base_extractiveness, 75, 0.85).
narrative_ontology:measurement(ska_ca_be_t100, state_killing_authority__categorical_abolition, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ska_ca_su_t0, state_killing_authority__categorical_abolition, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(ska_ca_su_t25, state_killing_authority__categorical_abolition, suppression_requirement, 25, 0.92).
narrative_ontology:measurement(ska_ca_su_t50, state_killing_authority__categorical_abolition, suppression_requirement, 50, 0.9).
narrative_ontology:measurement(ska_ca_su_t75, state_killing_authority__categorical_abolition, suppression_requirement, 75, 0.9).
narrative_ontology:measurement(ska_ca_su_t100, state_killing_authority__categorical_abolition, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__categorical_abolition, identity_coordination).
narrative_ontology:boltzmann_floor_override(state_killing_authority__categorical_abolition, 0.08).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__deterrence_instrument).

% DUAL FORMULATION NOTE:
% This constraint (categorical_abolition) and its siblings (retributive_desert, deterrence_instrument) form a constraint family decomposing the state_killing_authority kernel. The abolitionist reading's ε (0.85) assesses the death penalty arrangement as highly extractive; the retributive reading would assign low ε (just desert); the deterrence reading would assign ε conditional on empirical deterrence evidence. They share the same referent (state killing practice) but different ε values per reading — hence separate constraint stories linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_authority__categorical_abolition, institutional, 0.4).
constraint_indexing:directionality_override(state_killing_authority__categorical_abolition, powerful, 0.6).
constraint_indexing:directionality_override(state_killing_authority__categorical_abolition, moderate, 0.6).
constraint_indexing:directionality_override(state_killing_authority__categorical_abolition, powerless, 0.95).
constraint_indexing:directionality_override(state_killing_authority__categorical_abolition, organized, 0.15).
constraint_indexing:directionality_override(state_killing_authority__categorical_abolition, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
