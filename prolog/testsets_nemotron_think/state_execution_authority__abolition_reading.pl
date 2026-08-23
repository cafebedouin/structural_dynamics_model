% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__abolition_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: State Execution Authority (Abolition Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story models the death penalty system (state execution
 *   authority) as assessed by the abolition reading. The kernel
 *   'state_execution_authority' is contested: the abolition reading holds
 *   that state execution is categorically impermissible regardless of crime
 *   severity or procedural safeguards. The standing arrangement under contest
 *   is the death penalty system itself. The abolition reading assesses this
 *   arrangement as a snare: pure extraction (taking life) with no legitimate
 *   coordination function (retribution and deterrence rejected). All executed
 *   persons enter the victim set, including the guilty; wrongful executions
 *   are not errors but proof of systemic illegitimacy. Life imprisonment is
 *   qualitatively different — it preserves the possibility of exoneration and
 *   does not constitute state killing. The claimed_type (snare) and metrics
 *   (very high ε, high suppression, low theater) are authored independently
 *   from the abolition reading's structural assessment; the engine will
 *   compute per-seat classifications from the stakeholder data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.92).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.85).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "State Execution Authority (Abolition Reading)").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, 'a1b93b49-0911-4ec2-b609-dc010b08c441').
narrative_ontology:cs_kernel_codification('a1b93b49-0911-4ec2-b609-dc010b08c441', formalized).
narrative_ontology:cs_authority_grounding('a1b93b49-0911-4ec2-b609-dc010b08c441', lineage).
narrative_ontology:cs_interpretation_layer_present('a1b93b49-0911-4ec2-b609-dc010b08c441').
narrative_ontology:cs_reading_relation('a1b93b49-0911-4ec2-b609-dc010b08c441', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1b93b49-0911-4ec2-b609-dc010b08c441', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('a1b93b49-0911-4ec2-b609-dc010b08c441', foundational, execution_categorically_impermissible).
narrative_ontology:cs_axiom_status(execution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('a1b93b49-0911-4ec2-b609-dc010b08c441', execution_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('a1b93b49-0911-4ec2-b609-dc010b08c441', secondary, wrongful_execution_proves_systemic_illegitimacy).
narrative_ontology:cs_axiom_status(wrongful_execution_proves_systemic_illegitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a1b93b49-0911-4ec2-b609-dc010b08c441', wrongful_execution_proves_systemic_illegitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('a1b93b49-0911-4ec2-b609-dc010b08c441', absolute_prohibition_of_state_killing).
narrative_ontology:cs_drift_state('a1b93b49-0911-4ec2-b609-dc010b08c441', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a1b93b49-0911-4ec2-b609-dc010b08c441', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, death_row_prisoners).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, wrongfully_convicted).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, families_of_executed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_execution_authority__abolition_reading, deterrence_proponents).
narrative_ontology:constraint_vindicates(state_execution_authority__abolition_reading, absolute_right_to_life).
narrative_ontology:constraint_vindicates(state_execution_authority__abolition_reading, state_killing_intrinsically_wrong).
narrative_ontology:constraint_vindicates(state_execution_authority__abolition_reading, wrongful_execution_proves_systemic_illegitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentenced to death and executed by the state; the constraint takes their life as its terminal operation; no exit exists once sentence is carried out; the abolition reading holds that even the guilty are victims of a categorically illegitimate system.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executed_persons, payer,
    powerless, immediate, trapped, local).

% Living under active death sentence; the constraint structures their remaining life around execution preparation; limited exit through clemency, judicial reversal, or commutation; years of psychological torture under threat of state killing.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, death_row_prisoners, payer,
    powerless, immediate, constrained, local).

% Sentenced to death for crimes they did not commit; the constraint nearly executed them; their exoneration proves the system's unreliability; those not exonerated in time are executed by the constraint.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, wrongfully_convicted, payer,
    powerless, biographical, constrained, local).

% Bear the trauma of state killing of their kin; no procedural safeguard prevents the harm; the abolition reading rejects the retributive claim that execution provides closure, viewing it as compounded state violence.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, families_of_executed, payer,
    powerless, biographical, constrained, local).

% Administers the death penalty system: prosecutors seek death sentences, governors grant/deny clemency, corrections departments carry out executions; justifies the system as law enforcement and retributive justice; extracts institutional legitimacy from its operation.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, state_execution_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Positioned by the system as its primary moral beneficiaries (closure, justice); the abolition reading contests this positioning — their claimed benefit is rejected as illegitimate; they would object to abolition but are not the constraint's true beneficiaries.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, victims_families_retributive, excluded,
    moderate, biographical, mobile, local).

% Claim the death penalty deters capital crimes; cite contested studies; influence legislatures to retain statutes; the abolition reading rejects their claimed benefit as empirically unsupported and morally irrelevant to categorical prohibition.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, deterrence_proponents, beneficiary,
    powerful, biographical, mobile, national).

% Litigate, legislate, and organize against the death penalty; do not collect from the system and do not pay into it; seek its categorical abolition; their reading instantiates this constraint story.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, abolitionist_advocates, observer,
    organized, generational, analytical, global).

% Interpret constitutional limits on execution methods, procedural safeguards, and categorical exemptions (juveniles, intellectual disability); their rulings shape the constraint's operation but do not challenge its categorical legitimacy from the abolition view.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, judicial_review_courts, agenda_setter,
    institutional, generational, analytical, national).

% Monitor death penalty compliance with human rights treaties (ICCPR, regional conventions); advocate for universal abolition; no enforcement power over sovereign retentionist states; their consensus corroborates the abolition reading.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function. The death penalty system claims to coordinate retributive justice and deterrence, but the abolition reading rejects both: retribution as morally impermissible state killing, deterrence as empirically unsupported. The constraint does not solve a collective action problem — it creates one (state killing of its own subjects).
% TRANSFER_FUNCTION: Moves the lives of executed persons from the condemned to the state's account of justice; moves moral authority from the condemned to the state; moves public resources (capital trial costs, death row incarceration, execution apparatus) from the public fisc to the execution machinery; moves the risk of irreversible error onto the condemned.
% ABSENT_VOICES: Future executed persons (including those not yet wrongfully convicted); the executed themselves who cannot speak; the global supermajority of abolitionist states whose practice demonstrates the constraint's non-necessity; future generations who will inherit a legal system that once killed its own subjects.
% DISAPPEARANCE_RATIONALE: If the death penalty vanished overnight, retentionist states would restructure sentencing to life without parole; capital trial resources would redirect to investigation and victim services; international human rights compliance would improve; the state would lose its ultimate coercive tool, altering the power relation between state and subject.
% FOUNDING_PROBLEM: The death penalty was instituted to impose ultimate retribution for heinous crimes and to deter capital offenses through exemplary punishment.
% FOUNDING_PROBLEM_CORROBORATION: International human rights law (ICCPR Article 6, Second Optional Protocol, Protocol 13 ECHR), empirical criminology (National Research Council 2012 deterrence review finding no credible evidence of deterrence), and the lived experience of 112 abolitionist states corroborate that the founding problem is dead; no corroborating source outside the retentionist beneficiary set affirms the founding problem as live.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__abolition_reading, 0.92, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because the constraint takes life — the maximum possible extraction — with no substitution possible (life imprisonment preserves life and exoneration possibility). Suppression is high (0.85) because the state actively prevents exit: once sentenced, the condemned has no meaningful alternatives; clemency and appeals are constrained, not free exit. Theater ratio is low (0.15) because the constraint is not performative — it actually kills; the procedural machinery (appeals, stays, clemency) is not theater but the enforcement apparatus itself. Accessibility collapse is near-total (0.90) because once the death sentence is imposed, alternatives (life, freedom, exoneration) collapse structurally. Resistance is substantial (0.70) from abolitionist litigation, legislative campaigns, and international pressure, but has not dismantled the constraint in retentionist jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (state_execution_authority) experiences the constraint as legitimate law enforcement; the payer seats experience it as lethal extraction. The engine computes this divergence from the structural data: the state has arbitrage-grade exit (can abolish by statute) while the condemned are trapped. The excluded seat (victims_families_retributive) is positioned as a beneficiary by the retributive reading but as excluded by the abolition reading — this contested positioning is itself a structural feature of the kernel contest.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_execution_authority (prosecutors, governors, corrections) is the agenda_setter — it administers and benefits from the constraint's legitimacy extraction (d near beneficiary end). Executed_persons, death_row_prisoners, wrongfully_convicted, and families_of_executed are payers — they bear the terminal cost (d = 1.0, full target). Victims_families_retributive are excluded — positioned as beneficiaries by the retributive reading but the abolition reading contests this positioning; they have mobile exit (can advocate for abolition) but are structurally excluded from the abolition reading's framework. Deterrence_proponents claim beneficiary status but the abolition reading rejects their claimed benefit as empirically false; they hold power and mobile exit. Abolitionist_advocates, judicial_review_courts, and international_human_rights_bodies are observers — analytical seats that do not collect or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The death penalty's founding problem (retribution/deterrence) is dead per the abolition reading — retribution is morally impermissible, deterrence empirically unsupported — yet the arrangement persists. This is not a piton (theatrical maintenance of atrophied function) because the constraint actively kills; it is a snare whose mandate has been repudiated by its own operational consequences (wrongful executions) and by the global normative shift. The mandatrophy is unresolved: the constraint persists because the state_execution_authority extracts legitimacy from it, not because the founding problem remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (abolition_reading) of the contested kernel ''state_execution_authority''. What structural elements do sibling readings (retributive_reading, deterrence_reading) change?',
    'Compare the three readings'' victim sets, beneficiary sets, ε values, and claimed types. The abolition reading puts all executed persons in victim set with no legitimate beneficiaries; sibling readings create beneficiaries (victims'' families, society via deterrence) and exclude the guilty from victim set.',
    'If sibling readings produce substantially different ε and type classifications, the kernel is genuinely contested — the label ''death penalty'' covers multiple structurally distinct constraints. This validates the ε-invariance decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing: one kernel, multiple readings with divergent structural assessments.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of exit from the death penalty structural (legal barriers, state monopoly on violence) or partially internalized (moral acceptance of state killing, procedural legitimation)?',
    'Post-abolition trajectories: in states that abolished, did former death row prisoners and personnel internalize the legitimacy of the former system? If suppression persists culturally after legal removal, internalized component is significant.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint''s legitimacy survives its legal form. This affects whether abolition is sufficient or cultural transformation is also required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in state killing systems.').

omega_variable(
    beneficiary_structure_contestation,
    'Are victims'' families genuine beneficiaries of the death penalty (closure, justice) or are they constructed as such by the retributive reading?',
    'Longitudinal studies of victims'' families in abolitionist vs. retentionist jurisdictions; qualitative accounts of whether execution delivers claimed closure; comparison with restorative justice outcomes.',
    'If victims'' families are not genuine beneficiaries, the retributive reading''s coordination claim collapses — the constraint has no coordination function at all, confirming snare classification. If some are, tangled_rope becomes possible for that reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_contestation, empirical, 'Whether the retributive reading''s claimed beneficiaries actually benefit.').

omega_variable(
    deterrence_empirical_status,
    'Is the deterrence claim empirically false, genuinely contested, or irrelevant to the categorical prohibition?',
    'Meta-analysis of deterrence studies (Donohue & Wolfers 2005, NRC 2012, subsequent replications); assessment of whether any deterrence effect could morally justify categorical state killing.',
    'If deterrence is empirically false, the deterrence reading has no coordination function — its claimed_type would be snare from an empirical perspective. If contested, the reading remains live but its coordination claim is unstable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_empirical_status, empirical, 'Empirical status of deterrence justification and its structural consequences.').

omega_variable(
    wrongful_execution_as_proof,
    'Does the occurrence of wrongful executions prove systemic illegitimacy (abolition reading) or merely demand procedural reform (retentionist readings)?',
    'Analyze retentionist responses to exonerations: do they restrict the death penalty (narrowing eligibility, adding safeguards) or move toward abolition? The pattern reveals whether wrongful execution is treated as a bug or a feature.',
    'If retentionist systems treat wrongful executions as bugs to be patched, the constraint''s structure includes error-correction as coordination. If they are treated as proof of illegitimacy, the abolition reading''s victim set (including the guilty) is structurally grounded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wrongful_execution_as_proof, conceptual, 'Structural significance of wrongful execution: bug or feature?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sea_abolition_tr_t1900, state_execution_authority__abolition_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(sea_abolition_tr_t1950, state_execution_authority__abolition_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(sea_abolition_tr_t1972, state_execution_authority__abolition_reading, theater_ratio, 1972, 0.25).
narrative_ontology:measurement(sea_abolition_tr_t1976, state_execution_authority__abolition_reading, theater_ratio, 1976, 0.18).
narrative_ontology:measurement(sea_abolition_tr_t1990, state_execution_authority__abolition_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(sea_abolition_tr_t2000, state_execution_authority__abolition_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(sea_abolition_tr_t2010, state_execution_authority__abolition_reading, theater_ratio, 2010, 0.16).
narrative_ontology:measurement(sea_abolition_tr_t2020, state_execution_authority__abolition_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(sea_abolition_tr_t2024, state_execution_authority__abolition_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(sea_abolition_be_t1900, state_execution_authority__abolition_reading, base_extractiveness, 1900, 0.88).
narrative_ontology:measurement(sea_abolition_be_t1950, state_execution_authority__abolition_reading, base_extractiveness, 1950, 0.85).
narrative_ontology:measurement(sea_abolition_be_t1972, state_execution_authority__abolition_reading, base_extractiveness, 1972, 0.75).
narrative_ontology:measurement(sea_abolition_be_t1976, state_execution_authority__abolition_reading, base_extractiveness, 1976, 0.82).
narrative_ontology:measurement(sea_abolition_be_t1990, state_execution_authority__abolition_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(sea_abolition_be_t2000, state_execution_authority__abolition_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(sea_abolition_be_t2010, state_execution_authority__abolition_reading, base_extractiveness, 2010, 0.85).
narrative_ontology:measurement(sea_abolition_be_t2020, state_execution_authority__abolition_reading, base_extractiveness, 2020, 0.9).
narrative_ontology:measurement(sea_abolition_be_t2024, state_execution_authority__abolition_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(sea_abolition_su_t1900, state_execution_authority__abolition_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(sea_abolition_su_t1950, state_execution_authority__abolition_reading, suppression_requirement, 1950, 0.82).
narrative_ontology:measurement(sea_abolition_su_t1972, state_execution_authority__abolition_reading, suppression_requirement, 1972, 0.6).
narrative_ontology:measurement(sea_abolition_su_t1976, state_execution_authority__abolition_reading, suppression_requirement, 1976, 0.75).
narrative_ontology:measurement(sea_abolition_su_t1990, state_execution_authority__abolition_reading, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(sea_abolition_su_t2000, state_execution_authority__abolition_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(sea_abolition_su_t2010, state_execution_authority__abolition_reading, suppression_requirement, 2010, 0.83).
narrative_ontology:measurement(sea_abolition_su_t2020, state_execution_authority__abolition_reading, suppression_requirement, 2020, 0.85).
narrative_ontology:measurement(sea_abolition_su_t2024, state_execution_authority__abolition_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__deterrence_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes 'state execution authority' into three readings with divergent ε and type assessments. The abolition reading (this story) assesses the standing arrangement as snare (ε=0.92). The retributive and deterrence readings assess the same arrangement as tangled_rope or rope (coordination + extraction). The ε-invariance principle requires separate stories because ε changes with reading — the referent (death penalty system) is fixed but the assessment is reading-indexed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_execution_authority__abolition_reading, institutional, 0.15).
constraint_indexing:directionality_override(state_execution_authority__abolition_reading, powerless, 1.0).
constraint_indexing:directionality_override(state_execution_authority__abolition_reading, moderate, 0.7).
constraint_indexing:directionality_override(state_execution_authority__abolition_reading, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
