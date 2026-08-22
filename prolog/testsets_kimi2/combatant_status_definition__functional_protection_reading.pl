% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Common Article 3 Functional Protection Floor Regardless of Combatant Status
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint instantiates the functional-protection reading of the
 *   contested combatant-status-definition kernel. It holds that Common
 *   Article 3 minimum protections apply to all detained persons regardless of
 *   combatant status, removing status determination as a precondition for
 *   humane treatment. The reading is rival to the state-centric reading
 *   (status requires formal state organization and determines POW
 *   protections) and the national-liberation reading (non-state groups
 *   fighting colonial regimes acquire combatant status under AP I). Key
 *   agents: detained persons (primary beneficiaries), non-state detainees
 *   (beneficiaries under a state-centric exclusion), detaining authorities
 *   (administer the constraint and bear compliance costs), the ICRC
 *   (interpretive guardian), and war-crimes tribunals (external enforcement
 *   reference).
 *
 * KEY AGENTS:
 *   - detained_persons: Primary beneficiary (powerless/trapped) â receives baseline protections unconditionally.
 *   - non_state_detainees: Secondary beneficiary (powerless/trapped) â specifically protected against status-based exclusion.
 *   - detaining_authorities: Agenda-setter (institutional/constrained) â administers detention and bears compliance cost.
 *   - icrc: Observer (institutional/analytical) â monitors and interprets the standard.
 *   - war_crimes_tribunals: Observer (institutional/analytical) â adjudicates violations and reinforces the reading through case law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.18).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.35).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Common Article 3 Functional Protection Floor Regardless of Combatant Status").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "legal/international_humanitarian_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, 'b7cf3828-50ce-46cb-b53a-9c92335fb19e').
narrative_ontology:cs_kernel_codification('b7cf3828-50ce-46cb-b53a-9c92335fb19e', formalized).
narrative_ontology:cs_authority_grounding('b7cf3828-50ce-46cb-b53a-9c92335fb19e', lineage).
narrative_ontology:cs_interpretation_layer_present('b7cf3828-50ce-46cb-b53a-9c92335fb19e').
narrative_ontology:cs_reading_relation('b7cf3828-50ce-46cb-b53a-9c92335fb19e', combatant_status_definition__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('b7cf3828-50ce-46cb-b53a-9c92335fb19e', combatant_status_definition__national_liberation_reading, influences).
narrative_ontology:cs_axiom('b7cf3828-50ce-46cb-b53a-9c92335fb19e', foundational, humane_treatment_status_independent).
narrative_ontology:cs_axiom_status(humane_treatment_status_independent, holdable).
narrative_ontology:cs_axiom_grounding('b7cf3828-50ce-46cb-b53a-9c92335fb19e', humane_treatment_status_independent, conventional).
narrative_ontology:cs_axiom('b7cf3828-50ce-46cb-b53a-9c92335fb19e', foundational, minimum_protections_universal_personhood).
narrative_ontology:cs_axiom_status(minimum_protections_universal_personhood, holdable).
narrative_ontology:cs_axiom_grounding('b7cf3828-50ce-46cb-b53a-9c92335fb19e', minimum_protections_universal_personhood, deontological).
narrative_ontology:cs_reference_frame('b7cf3828-50ce-46cb-b53a-9c92335fb19e', geneva_conventions_protective_floor).
narrative_ontology:cs_drift_state('b7cf3828-50ce-46cb-b53a-9c92335fb19e', contemporary_customary_law_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b7cf3828-50ce-46cb-b53a-9c92335fb19e', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, detained_persons).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, non_state_detainees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons held by parties to armed conflict who receive baseline humane treatment, medical care, and fair-trial guarantees without awaiting a formal combatant-status determination. They cannot exit detention unilaterally and have no alternative protective framework.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detained_persons, beneficiary,
    powerless, immediate, trapped, local).

% Members of non-state armed groups detained in international or non-international conflicts who are often denied POW status under state-centric readings. Under this constraint they receive the same minimum protections as state-combatant detainees without the precondition of formal status recognition.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, non_state_detainees, beneficiary,
    powerless, immediate, trapped, local).

% State armed forces and organized armed groups that operate detention facilities. They bear the operational and resource costs of providing minimum conditions and judicial guarantees to all detainees regardless of status, and they administer the constraint through standard operating procedures and detention orders.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detaining_authorities, agenda_setter,
    institutional, biographical, constrained, national).

% The International Committee of the Red Cross monitors places of detention, engages detaining authorities confidentially, and publishes interpretive guidance including the Customary IHL Study that supports the functional-protection reading. It does not benefit financially nor bear compliance costs.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, icrc, observer,
    institutional, generational, analytical, global).

% International and hybrid criminal tribunals that adjudicate grave breaches and serious violations of Common Article 3. They reinforce the constraint through case law criminalizing status-independent mistreatment, operating as an external analytical and enforcement reference point.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, war_crimes_tribunals, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal baseline of humane treatment and fair-trial guarantees for all persons detained in armed conflict, eliminating the need for a prior legal status determination and thereby preventing parties from denying protections through procedural delay or categorical exclusion.
% TRANSFER_FUNCTION: Transfers obligations of material care, procedural safeguards, and humane conduct from detaining authorities to detained persons. The compliance costâprovision of food, medical attention, quarters, and judicial guaranteesâis borne by the detaining party.
% ABSENT_VOICES: Non-state armed groups that reject international legal frameworks entirely are excluded from the norm-setting conversation. Additionally, military legal advisors from states holding strictly state-centric views are under-represented in human-rights and ICRC fora where the functional reading is elaborated.
% DISAPPEARANCE_RATIONALE: If the status-independent floor vanished, detaining authorities would revert to status-based treatment hierarchies; detainees without recognized combatant status would lose immediate protections during legal limbo, and the ICRC's access and monitoring framework would lose its legal anchor.
% FOUNDING_PROBLEM: Armed conflicts produce detainees whose legal status is uncertain or disputed; without a minimum standard independent of status, parties have an incentive to deny protections by delaying or rejecting status determinations, leading to widespread mistreatment in legal grey zones.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC's Customary International Humanitarian Law Study (2005) and subsequent international criminal jurisprudence attest that status disputes continue to delay or deny protections in contemporary conflicts. State military manuals increasingly incorporate status-independent minimums, corroborating the ongoing need from outside the beneficiary population.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__functional_protection_reading, 0.18, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.18) because the constraint is a baseline protective floor with no rent-seeking intermediary; the compliance cost borne by detaining authorities is coordination cost, not extractive overhead. Suppression is moderate (0.35): the norm is backed by international criminal law and ICRC pressure, but violations remain common and enforcement is geographically uneven. Theater ratio is low (0.20): most compliance activity is functional (material provision, judicial process), though some performative adoption occurs (paper policies without implementation). Accessibility collapse is high (0.70): once the norm is understood, the legal alternativeâdenying protections pending status determinationâis largely collapsed in doctrine, even if practice lags. Resistance is moderate (0.40): some states continue to resist full applicability to non-state actors, producing ongoing doctrinal friction.
 *
 * PERSPECTIVAL GAP:
 *   The detained-person seat experiences the constraint as a protective rope: it delivers tangible goods (safety, food, process) without precondition. The detaining-authority seat experiences it as a regulatory burden: it constrains interrogation methods, requires resource expenditure, and subjects officials to criminal liability. Because there is no third-party beneficiary capturing rents, the engine should compute the detaining-authority directionality as near-symmetric (coordination cost) rather than target (extraction), preserving the rope classification for both seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Detained persons and non-state detainees are beneficiaries (low d, low Ï). Detaining authorities are agenda-setters who administer the constraint; they are not listed as victims because their cost is coordination cost, not extraction. Their derived directionality sits near symmetric (d â 0.5). The ICRC and tribunals are observers (analytical exit, no directionality). No concentrated beneficiary captures rents, and no concentrated payer suffers extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâstatus uncertainty leaving detainees unprotectedâremains live in contemporary conflicts. The constraint has not atrophied into a piton: compliance is not merely theatrical, the beneficiary set is real and vulnerable, and the agenda-setters bear genuine coordination costs rather than inertia. If the problem were solved (all parties routinely provided universal minimums without legal compulsion), the constraint might become a scaffold or dissolve, but that condition does not obtain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_scope,
    'Does the functional-protection reading''s decoupling of minimum protections from combatant status logically resolve the kernel contest, or does it simply shift the locus of dispute to the definition of ''detained person'' and the scope of ''armed conflict''?',
    'Comparative doctrinal analysis of the three readings'' textual grounding in the Geneva Conventions and AP I to see whether the functional reading eliminates the need for a combatant-status definition or merely brackets it.',
    'If the reading only brackets the definition, the kernel remains contested and the constraint family persists; if it resolves the kernel, the sibling readings become historically residual rather than live rivals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Whether the functional reading resolves or merely displaces the kernel contest.').

omega_variable(
    customary_vs_conventional_basis,
    'Is the functional reading''s binding force rooted in the 1949 Geneva Conventions as treaty law, or in parallel customary international law that binds even non-parties?',
    'Systematic review of state practice and opinio juris since 1949, including national military manuals and judicial decisions, to determine whether the status-independent floor has crystallized as custom independent of treaty ratification.',
    'If purely conventional, the constraint''s scope is limited to treaty parties; if customary, it attains universal scope and modifies the legal position of non-state actors and non-party states, altering the classification''s spatial_scope and directionality derivations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_vs_conventional_basis, empirical, 'Whether the reading''s authority derives from treaty text or customary law.').

omega_variable(
    non_state_reciprocity_assumption,
    'Does the constraint''s application to non-state armed groups rest on a reciprocal coordination equilibrium, or does it impose a unilateral obligation on detaining authorities without coordinated counterpart?',
    'Empirical assessment of non-state group compliance with CA3 minimums across multiple conflicts; if reciprocity is absent, the constraint functions as a one-sided imposition.',
    'If unilateral, the detaining-authority seat experiences the constraint as extractive overhead rather than symmetric coordination, raising its effective directionality and potentially shifting the computed type toward tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_state_reciprocity_assumption, empirical, 'Whether non-state actor reciprocity supports the coordination framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(functional_protection_tr_t0, combatant_status_definition__functional_protection_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(functional_protection_tr_t23, combatant_status_definition__functional_protection_reading, theater_ratio, 23, 0.24).
narrative_ontology:measurement(functional_protection_tr_t46, combatant_status_definition__functional_protection_reading, theater_ratio, 46, 0.22).
narrative_ontology:measurement(functional_protection_tr_t70, combatant_status_definition__functional_protection_reading, theater_ratio, 70, 0.2).

% Extraction over time
narrative_ontology:measurement(functional_protection_be_t0, combatant_status_definition__functional_protection_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(functional_protection_be_t23, combatant_status_definition__functional_protection_reading, base_extractiveness, 23, 0.2).
narrative_ontology:measurement(functional_protection_be_t46, combatant_status_definition__functional_protection_reading, base_extractiveness, 46, 0.19).
narrative_ontology:measurement(functional_protection_be_t70, combatant_status_definition__functional_protection_reading, base_extractiveness, 70, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(combatant_status_definition__functional_protection_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).

% DUAL FORMULATION NOTE:
% The kernel 'combatant_status_definition' decomposes into three structurally distinct constraints. This story models the functional-protection reading (universal minimums independent of status). Its sibling readings are the state-centric reading (status-dependent exclusions) and the national-liberation reading (status extension for anti-colonial struggles). Each reading has a distinct epsilon, beneficiary structure, and classification; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
