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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: state_killing_authority__categorical_abolition
 *   human_readable: Categorical Abolition of State Killing
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story models the categorical abolitionist reading of the
 *   state killing authority kernel: the constitutional and normative
 *   prohibition on capital punishment grounded in the premise that life is
 *   inalienable and state killing is impermissible regardless of crime or
 *   consequence. It is instantiated in jurisdictions with entrenched
 *   constitutional abolition (e.g., South Africa, Colombia, Germany). The
 *   constraint operates as a commitment system that protects condemned
 *   persons from execution while imposing costs on retributive victims'
 *   families and prosecutorial actors who seek death.
 *
 * KEY AGENTS:
 *   - Condemned persons: Primary beneficiary (powerless/trapped) â protected from execution by absolute legal barrier.
 *   - Constitutional court: Agenda-setter (institutional/analytical) â interprets and enforces the prohibition.
 *   - State prosecutors: Primary payer (institutional/constrained) â bear cost of foregone punitive discretion.
 *   - Retributive victims' families: Secondary payer (moderate/constrained) â denied execution as retributive satisfaction.
 *   - Abolitionist victims' families: Secondary beneficiary (moderate/constrained) â aligned with prohibition but marginalized in prosecutorial discourse.
 *   - International human rights bodies: Analytical observer (institutional/analytical) â corroborate founding problem from outside beneficiary set.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.42).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.58).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.42).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "Categorical Abolition of State Killing").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__categorical_abolition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, '9e289a02-f2c5-4bd6-97ec-28657e6e0c65').
narrative_ontology:cs_kernel_codification('9e289a02-f2c5-4bd6-97ec-28657e6e0c65', formalized).
narrative_ontology:cs_authority_grounding('9e289a02-f2c5-4bd6-97ec-28657e6e0c65', lineage).
narrative_ontology:cs_interpretation_layer_present('9e289a02-f2c5-4bd6-97ec-28657e6e0c65').
narrative_ontology:cs_reading_relation('9e289a02-f2c5-4bd6-97ec-28657e6e0c65', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('9e289a02-f2c5-4bd6-97ec-28657e6e0c65', state_killing_authority__deterrence_instrument, forecloses).
narrative_ontology:cs_axiom('9e289a02-f2c5-4bd6-97ec-28657e6e0c65', foundational, life_is_inalienable).
narrative_ontology:cs_axiom_status(life_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('9e289a02-f2c5-4bd6-97ec-28657e6e0c65', life_is_inalienable, deontological).
narrative_ontology:cs_axiom('9e289a02-f2c5-4bd6-97ec-28657e6e0c65', foundational, state_authority_derives_from_respect_for_personhood).
narrative_ontology:cs_axiom_status(state_authority_derives_from_respect_for_personhood, holdable).
narrative_ontology:cs_axiom_grounding('9e289a02-f2c5-4bd6-97ec-28657e6e0c65', state_authority_derives_from_respect_for_personhood, deontological).
narrative_ontology:cs_reference_frame('9e289a02-f2c5-4bd6-97ec-28657e6e0c65', inalienable_personhood_framework).
narrative_ontology:cs_drift_state('9e289a02-f2c5-4bd6-97ec-28657e6e0c65', contemporary_retributive_resurgence, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9e289a02-f2c5-4bd6-97ec-28657e6e0c65', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, condemned_persons).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, abolitionist_victims_families).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, retributive_victims_families).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, state_prosecutors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face the most severe state sanction but remain categorically within the rights-holder set; the constraint removes execution from the menu of possible punishments regardless of conviction or public outrage. Exit is limited to appeal, sentence commutation, or procedural delay.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_persons, beneficiary,
    powerless, immediate, trapped, national).

% Interprets and enforces the constitutional prohibition on state killing, striking down legislative attempts to reintroduce capital punishment and reviewing prosecutorial conduct. Derives authority from constitutional text and international human rights lineage.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, constitutional_court, agenda_setter,
    institutional, generational, analytical, national).

% Bear the cost of constrained punitive discretion; cannot seek execution regardless of crime severity, victim demand, or deterrence claims. May resist through charging manipulation, media campaigns, or appeals to political branches to amend the constitutional framework.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, state_prosecutors, payer,
    institutional, biographical, constrained, national).

% Seek execution as proportional response to homicide; bear the cost of the state's categorical refusal to kill on their behalf. Are told their loss is constitutionally insufficient to trigger state killing; experience the constraint as a denial of justice and closure.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, retributive_victims_families, payer,
    moderate, biographical, constrained, local).

% Oppose execution on moral or religious grounds aligned with the constraint, yet are structurally marginalized in prosecutorial and media narratives that treat execution as the default victim interest. Their presence in the rights-holder framework is protected even as their voice is excluded from charging conversations.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolitionist_victims_families, beneficiary,
    moderate, biographical, constrained, local).

% Monitor and corroborate the abolitionist commitment from outside the national beneficiary set; provide external validation of the founding problem of unchecked state killing and apply transnational pressure against backsliding.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(state_killing_authority__categorical_abolition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes an absolute, non-derogable limit on state power to take life, preventing arbitrary, discriminatory, or consequentialist state killing and securing the rights-holder status of all persons regardless of crime.
% TRANSFER_FUNCTION: Moves the locus of moral status from the state's punitive discretion to the unconditional personhood of the condemned; transfers the burden of non-retribution from the condemned to the state and to victims' families who seek execution.
% ABSENT_VOICES: Abolitionist victims' families are marginalized in prosecutorial discourse, treated as if they do not represent authentic victim interests. Condemned persons speak only through counsel; their direct voice is excluded from constitutional interpretation processes.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, constitutional orders would permit legislative reintroduction of capital punishment; prosecutorial discretion would expand to seek death; the rights-holder boundary would become contingent on crime severity rather than personhood; and the international human rights architecture would lose a foundational pillar.
% FOUNDING_PROBLEM: Unchecked state killing power leads to arbitrary, discriminatory, politically motivated, or error-prone executions, and collapses the distinction between legitimate state authority and lawful violence against subjects.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies (UN Human Rights Committee, regional courts) attest to the problem from outside the benefiting parties. Retributive political factions and some victims' groups contest that the problem is live in their jurisdiction, asserting that democratic majorities can be trusted with death penalty authority.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__categorical_abolition, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__categorical_abolition_tests).
:- end_tests(state_killing_authority__categorical_abolition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the constraint structurally denies retributive satisfaction to victims' families and constrains state punitive power, creating asymmetric cost-bearing despite genuine coordination (human rights protection). Suppression is moderate-high (0.58) because the prohibition actively suppresses capital punishment through constitutional judicial review. Theater ratio is moderate-low (0.30): most activity is substantive rights protection, but a growing share of human rights discourse is performative solidarity that outruns institutional follow-through. Accessibility collapse is high (0.75) because legal alternatives to execution are fully closed once the constitutional framework is operative. Resistance is moderate (0.50) due to persistent prosecutorial resistance, populist pushback, and periodic legislative challenges.
 *
 * PERSPECTIVAL GAP:
 *   The condemned person and abolitionist families experience the constraint as protective (low d, low effective extraction). Retributive victims' families and state prosecutors experience it as an imposed cost that overrides their preferences (high d, high effective extraction). The constitutional court sits at the enforcement seat with analytical exit, experiencing the constraint as an interpretive mandate rather than extraction or subsidy.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (condemned persons, abolitionist families) derive protection and moral alignment from the constraint; structural derivation places them near the beneficiary pole. Victims (retributive families, prosecutors) bear the cost of foregone execution and constrained discretion; structural derivation places them near the target pole. No directionality override is required because beneficiary/victim declarations plus exit options capture the true structural relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resists mandatrophy by embedding a live founding problem (unchecked state killing as arbitrary violence) in constitutional text backed by active judicial enforcement. If the founding problem were universally acknowledged as dead and the constraint persisted only by institutional inertia, it would risk piton status. Currently the problem is contested but live in transnational human rights law, so the constraint retains functional justification rather than theatrical persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed,
    'Is the inalienability of life a discovered moral constant or a constructed legal convention?',
    'Comparative jurisprudence tracking how abolitionist provisions emergedâvia moral discovery narratives versus contingent political settlementâand whether repeal trajectories differ across these origins.',
    'If inalienability is a constructed convention, the constraint is a tangled_rope or snare depending on enforcement asymmetry; if a discovered moral constant, it trends toward mountain or false-summit mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed, conceptual, 'Moral realism vs legal positivism in the constraint''s foundation').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of state killing sustained by structural legal bars alone, or by internalized cultural rejection of execution?',
    'Post-backsliding trajectory analysis: if execution resumes quickly after legal bars weaken, suppression was primarily structural; if legal bars weaken but execution does not resume, internalization is dominant.',
    'If internalized, effective suppression is higher than structural measure suggests and the constraint is more robust; if purely structural, it is vulnerable to political reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of capital punishment').

omega_variable(
    coordination_extraction_boundary,
    'Does the constraint''s protection of condemned persons constitute pure coordination, or does the asymmetric denial of retributive justice to victims'' families render it extractive?',
    'Empirical study of victim-family outcomes in abolitionist versus retentionist jurisdictions, measuring psychological and material well-being across both groups.',
    'If abolitionist regimes produce net harm to victims'' families without compensatory mechanism, the extraction component is higher than modeled; if outcomes are neutral or positive, the coordination function dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the constraint''s coordination function dominates its asymmetric cost imposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__categorical_abolition, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__categorical_abolition, theater_ratio, 10, 0.22).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__categorical_abolition, theater_ratio, 20, 0.25).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__categorical_abolition, theater_ratio, 30, 0.28).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__categorical_abolition, theater_ratio, 40, 0.3).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__categorical_abolition, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__categorical_abolition, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__categorical_abolition, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__categorical_abolition, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__categorical_abolition, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__categorical_abolition, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__categorical_abolition, base_extractiveness, 50, 0.45).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(state_killing_authority__categorical_abolition, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__categorical_abolition, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__deterrence_instrument).

% DUAL FORMULATION NOTE:
% The state_killing_authority kernel decomposes into three structurally distinct constraints per the epsilon-invariance principle: categorical_abolition (this file), retributive_desert, and deterrence_instrument. Each reading has a unique epsilon, beneficiary/victim structure, and logical relationship to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
