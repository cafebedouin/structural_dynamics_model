% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__liberal_due_process_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__liberal_due_process_reading, []).

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
 *   constraint_id: magna_carta_clause_39__liberal_due_process_reading
 *   human_readable: Magna Carta Clause 39: Liberal Due Process Reading
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint story analyzes Clause 39 of Magna Carta through a
 *   'liberal due process' reading, which interprets it as establishing
 *   universal individual rights against arbitrary state power. This reading
 *   emphasizes the expansive and evolving nature of due process, extending
 *   beyond the specific feudal context of 1215 to apply to all citizens and
 *   constrain all forms of executive discretion. The metrics reflect this
 *   reading's impact on state power and individual liberty over centuries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.85).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.2).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Magna Carta Clause 39: Liberal Due Process Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, 'ae92a78a-e31a-4611-9ae8-ea5f89d6c4f2').
narrative_ontology:cs_kernel_codification('ae92a78a-e31a-4611-9ae8-ea5f89d6c4f2', fixed_text).
narrative_ontology:cs_authority_grounding('ae92a78a-e31a-4611-9ae8-ea5f89d6c4f2', lineage).
narrative_ontology:cs_interpretation_layer_present('ae92a78a-e31a-4611-9ae8-ea5f89d6c4f2').
narrative_ontology:cs_reading_relation('ae92a78a-e31a-4611-9ae8-ea5f89d6c4f2', magna_carta_clause_39__feudal_prerogative_reading, forecloses).
narrative_ontology:cs_reading_relation('ae92a78a-e31a-4611-9ae8-ea5f89d6c4f2', magna_carta_clause_39__originalist_limitation_reading, forecloses).
narrative_ontology:cs_axiom('ae92a78a-e31a-4611-9ae8-ea5f89d6c4f2', foundational, individual_liberty_is_pre_political).
narrative_ontology:cs_axiom_status(individual_liberty_is_pre_political, holdable).
narrative_ontology:cs_axiom_grounding('ae92a78a-e31a-4611-9ae8-ea5f89d6c4f2', individual_liberty_is_pre_political, deontological).
narrative_ontology:cs_axiom('ae92a78a-e31a-4611-9ae8-ea5f89d6c4f2', foundational, state_power_is_fiduciary).
narrative_ontology:cs_axiom_status(state_power_is_fiduciary, holdable).
narrative_ontology:cs_axiom_grounding('ae92a78a-e31a-4611-9ae8-ea5f89d6c4f2', state_power_is_fiduciary, conventional).
narrative_ontology:cs_reference_frame('ae92a78a-e31a-4611-9ae8-ea5f89d6c4f2', universal_individual_rights_framework).
narrative_ontology:cs_drift_state('ae92a78a-e31a-4611-9ae8-ea5f89d6c4f2', contemporary_human_rights_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ae92a78a-e31a-4611-9ae8-ea5f89d6c4f2', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, citizens).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, judiciary).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, arbitrary_state_power).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, executive_overreach).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from protection against arbitrary state action, enjoying due process and legal safeguards. They bear the indirect costs of maintaining the legal system through taxes but are net beneficiaries of the constraint on power.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, citizens, beneficiary,
    moderate, biographical, constrained, national).

% Interprets and enforces the principles of due process, acting as a check on executive power. Its legitimacy and function are enhanced by upholding these rights, making it a beneficiary of the constraint's operation.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__liberal_due_process_reading, judiciary, beneficiary).

% Is directly constrained by the requirement for due process and legal limits. Its ability to act without justification or legal procedure is curtailed, making it a target of the constraint's 'extraction' of discretion.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, arbitrary_state_power, payer,
    institutional, immediate, trapped, national).

% Represents the tendency of executive branches to exceed their lawful authority. This constraint directly limits such overreach, forcing adherence to legal norms and processes, thus bearing the 'cost' of reduced arbitrary power.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, executive_overreach, payer,
    institutional, immediate, trapped, national).

% Analyze the historical development, interpretation, and application of Clause 39, contributing to its understanding and evolution. They are not directly subject to its enforcement but critically evaluate its impact.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% While historically central to Magna Carta, this liberal reading transcends their specific feudal grievances, making their original, narrow interpretation largely irrelevant to the contemporary understanding of universal rights. They are excluded from the modern interpretive conversation.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, feudal_barons, excluded,
    powerful, generational, identity_locked, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a foundational framework for legitimate governance by coordinating state power with individual liberty, ensuring that state actions affecting individuals adhere to legal processes and standards, rather than arbitrary will.
% TRANSFER_FUNCTION: Transfers authority from unchecked state discretion to legally defined procedures and individual rights; it transfers security and predictability from the state to the individual, at the 'cost' of limiting state flexibility.
% ABSENT_VOICES: Those who advocate for absolute state sovereignty or unchecked executive power, arguing that individual rights unduly impede effective governance or national security. Their arguments are often marginalized in liberal constitutional discourse.
% DISAPPEARANCE_RATIONALE: If the principle of universal individual rights against arbitrary state power vanished, the legal and political systems of many nations would fundamentally reorganize. Protections for due process, fair trial, and personal liberty would erode, potentially leading to authoritarianism and a collapse of trust between citizens and the state.
% FOUNDING_PROBLEM: Arbitrary and unchecked royal power, leading to abuses against individuals and property without legal recourse, and a lack of predictable justice for subjects.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, constitutional scholars, international legal bodies, and historical records of abuses consistently attest to the perennial threat of arbitrary power and the ongoing relevance of due process principles. Contemporary challenges to democratic norms also corroborate its live status.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__liberal_due_process_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__liberal_due_process_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__liberal_due_process_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(magna_carta_clause_39__liberal_due_process_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__liberal_due_process_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is high (0.85) because this reading significantly 'extracts' arbitrary power and discretion from the state, forcing adherence to legal norms. `Suppression` is low (0.20) because the constraint's purpose is to reduce state suppression of individuals. `Theater_ratio` is low (0.10) as the principle is genuinely functional, though its application requires constant vigilance. `Accessibility_collapse` is moderate (0.40) because while the right exists, access to justice and effective enforcement can still be challenging. `Resistance` is high (0.70) reflecting the historical and ongoing struggle to uphold these rights against state interests.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of citizens and the judiciary, this constraint is a vital safeguard of liberty and the rule of law. From the perspective of state actors seeking to exercise unchecked power, it is an impediment to efficient governance. The engine's per-seat classification will highlight this divergence, showing a beneficial 'rope' for citizens and an extractive 'snare' for arbitrary state power.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizens and the judiciary are beneficiaries, as the constraint protects individual liberty and empowers the courts to enforce legal limits. Arbitrary state power and executive overreach are the targets/victims, as their capacity for unconstrained action is curtailed. The directionality reflects the constraint's function as a check on power, rather than a mechanism for extracting from citizens.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_scope_ambiguity,
    'Is Clause 39''s protection against arbitrary power truly universal, or is its application still implicitly limited by historical context or specific legal traditions?',
    'Comparative legal analysis across jurisdictions and historical periods, examining the scope of ''due process'' application to non-citizens, corporations, or in times of emergency.',
    'If its universality is found to be consistently limited, the effective scope of the constraint is narrower, potentially reducing its extractiveness against state power in certain contexts and shifting its classification towards a more ''tangled rope'' for some groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_scope_ambiguity, conceptual, 'Ambiguity regarding the universal vs. context-dependent scope of Clause 39''s protections.').

omega_variable(
    enforcement_effectiveness_gap,
    'How effective is the judiciary in consistently enforcing the liberal due process reading against powerful state actors, especially in areas like national security or emergency powers?',
    'Empirical studies of judicial review outcomes, analysis of legislative overrides, and examination of executive compliance with court orders in high-stakes cases.',
    'If enforcement is found to be consistently weak or subject to political pressure, the effective suppression of arbitrary state power is lower than measured, potentially shifting the constraint''s classification towards a ''piton'' or ''tangled rope'' where the principle is more theatrical than functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness_gap, empirical, 'The gap between the declared right and its practical enforcement against powerful state interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1215, 0.15).
narrative_ontology:measurement(magn_tr_t1688, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1688, 0.12).
narrative_ontology:measurement(magn_tr_t1789, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1789, 0.1).
narrative_ontology:measurement(magn_tr_t1948, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1215, 0.3).
narrative_ontology:measurement(magn_be_t1688, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1688, 0.5).
narrative_ontology:measurement(magn_be_t1789, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1789, 0.65).
narrative_ontology:measurement(magn_be_t1948, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(magn_be_t2024, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1215, 0.4).
narrative_ontology:measurement(magn_su_t1688, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1688, 0.3).
narrative_ontology:measurement(magn_su_t1789, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1789, 0.25).
narrative_ontology:measurement(magn_su_t1948, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1948, 0.2).
narrative_ontology:measurement(magn_su_t2024, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__liberal_due_process_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, rule_of_law_doctrine).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, habeas_corpus_principle).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, human_rights_charters).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of Magna Carta Clause 39, each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
