% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: War Powers Allocation: Functional Accommodation Reading
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This constraint describes the 'functional accommodation' reading of US
 *   war powers, where the executive's authority to use military force is
 *   understood to vary by operational context: unilateral action is permitted
 *   for imminent threats, but prolonged campaigns require congressional
 *   authorization. This reading seeks to balance executive agility with
 *   legislative oversight, but often results in an ambiguous 'gray area'
 *   where both branches claim authority, leading to a gradual expansion of
 *   executive power. It is a tangled rope because it genuinely coordinates
 *   rapid response while extracting oversight from Congress through ambiguity
 *   and political pressure.
 *
 * KEY AGENTS:
 *   - executive_branch: Primary agenda-setter (institutional/constrained) — benefits from flexibility
 *   - congressional_oversight: Primary payer (institutional/constrained) — bears cost of ceded authority
 *   - military_command: Beneficiary (institutional/constrained) — benefits from rapid decision-making
 *   - public_accountability: Payer (powerless/trapped) — bears cost of reduced transparency
 *   - judicial_branch: Analytical observer (institutional/analytical) — generally defers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.65).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.7).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "War Powers Allocation: Functional Accommodation Reading").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, 'efd55ea0-64f7-44c2-9d64-4ce8fa80c771').
narrative_ontology:cs_kernel_codification('efd55ea0-64f7-44c2-9d64-4ce8fa80c771', fixed_text).
narrative_ontology:cs_authority_grounding('efd55ea0-64f7-44c2-9d64-4ce8fa80c771', lineage).
narrative_ontology:cs_interpretation_layer_present('efd55ea0-64f7-44c2-9d64-4ce8fa80c771').
narrative_ontology:cs_reading_relation('efd55ea0-64f7-44c2-9d64-4ce8fa80c771', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('efd55ea0-64f7-44c2-9d64-4ce8fa80c771', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_axiom('efd55ea0-64f7-44c2-9d64-4ce8fa80c771', foundational, executive_agility_in_crisis_is_paramount).
narrative_ontology:cs_axiom_status(executive_agility_in_crisis_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('efd55ea0-64f7-44c2-9d64-4ce8fa80c771', executive_agility_in_crisis_is_paramount, instrumental).
narrative_ontology:cs_axiom('efd55ea0-64f7-44c2-9d64-4ce8fa80c771', foundational, congressional_oversight_for_sustained_conflict).
narrative_ontology:cs_axiom_status(congressional_oversight_for_sustained_conflict, holdable).
narrative_ontology:cs_axiom_grounding('efd55ea0-64f7-44c2-9d64-4ce8fa80c771', congressional_oversight_for_sustained_conflict, deontological).
narrative_ontology:cs_reference_frame('efd55ea0-64f7-44c2-9d64-4ce8fa80c771', post_wwii_bipartisan_consensus).
narrative_ontology:cs_drift_state('efd55ea0-64f7-44c2-9d64-4ce8fa80c771', post_911_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('efd55ea0-64f7-44c2-9d64-4ce8fa80c771', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, military_command).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congressional_oversight).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, public_accountability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims inherent authority for unilateral military action in response to imminent threats, and seeks to define 'imminent' broadly. Benefits from flexibility and speed in foreign policy, but faces political and legal challenges for prolonged engagements without congressional backing.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, executive_branch, agenda_setter,
    institutional, biographical, constrained, national).

% Bears the cost of ceding authority to the executive in crisis, leading to a diminished role in war authorization. Seeks to reassert its constitutional role for prolonged conflicts, but often faces political pressure to support executive action in the name of national unity.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congressional_oversight, payer,
    institutional, generational, constrained, national).

% Benefits from clear, rapid decision-making in operational contexts, especially for immediate threats. Prefers a flexible interpretation of war powers that allows for swift deployment and adaptation, avoiding bureaucratic delays.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, military_command, beneficiary,
    institutional, immediate, constrained, global).

% Bears the cost of reduced transparency and democratic input into decisions of war and peace. Suffers from the erosion of checks and balances when executive action bypasses legislative debate and public scrutiny.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, public_accountability, payer,
    powerless, generational, trapped, national).

% Generally defers to the political branches on war powers, viewing it as a 'political question.' Rarely intervenes to define the boundaries of executive or legislative authority in this domain, acting primarily as an analytical observer.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, judicial_branch, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows for rapid, decisive action by the executive in genuine emergencies while theoretically requiring broader authorization for sustained military campaigns, balancing speed with democratic legitimacy.
% TRANSFER_FUNCTION: Transfers decision-making authority for military force from Congress to the Executive in situations deemed 'imminent threats,' and transfers the burden of justifying prolonged engagements back to the Executive.
% ABSENT_VOICES: A more robustly empowered Congress, particularly its oversight committees, would demand clearer definitions of 'imminent threat' and 'prolonged campaign,' and assert its constitutional prerogative for all but the most immediate defensive actions. Anti-war movements and civil liberties advocates would also demand greater transparency and accountability.
% DISAPPEARANCE_RATIONALE: If this functional accommodation vanished, the US constitutional system would face a severe crisis. Either the executive would be paralyzed in emergencies, or Congress would be entirely sidelined, leading to a fundamental reordering of governmental power and potentially international instability.
% FOUNDING_PROBLEM: The US Constitution divided war powers between the executive (Commander-in-Chief) and legislative (declare war, raise armies) branches, creating an inherent tension that needed resolution for effective governance and national security.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, former government officials from both branches, and military strategists consistently attest to the ongoing challenge of balancing executive agility with legislative deliberation in modern warfare. The problem is widely acknowledged as a persistent feature of US governance, corroborated by historical precedent and contemporary debates.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because the executive consistently pushes the boundaries of 'imminent threat' to justify unilateral action, effectively extracting legislative authority. Suppression (0.70) is high due to the political difficulty for Congress to challenge executive military action once initiated, and the suppression of categorical constitutional rules in favor of flexible interpretation. Theater ratio (0.40) reflects that while some executive actions are genuine responses to threats, a significant portion of the 'accommodation' involves performative deference to Congress that does not translate into actual legislative control. The metrics show a peak in extractiveness and suppression post-9/11, followed by a slight decline as Congress and the public push back, but remaining high.
 *
 * PERSPECTIVAL GAP:
 *   The executive branch perceives this as a necessary and legitimate flexibility for national security, a 'rope' that enables effective governance. Congressional oversight, however, experiences it as a 'snare' where its constitutional powers are gradually eroded through executive overreach and political pressure. The engine's per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch and military command are beneficiaries (d near 0.0-0.2) as they gain flexibility and speed. Congressional oversight and public accountability are targets (d near 0.7-0.9) as they bear the costs of diminished checks and balances and reduced democratic input. The judicial branch is an analytical observer (d=0.5) as it typically avoids adjudicating these disputes.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as a pure 'snare' by acknowledging the genuine coordination function of rapid executive response in emergencies. However, it also highlights how the ambiguity inherent in 'functional accommodation' allows for the accumulation of executive power beyond its original mandate, indicating a drift towards extraction. The 'contested' status of the founding problem reflects this ongoing tension between original intent and evolving practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminent_threat_definition_ambiguity,
    'How is ''imminent threat'' defined, and is that definition consistently applied or strategically expanded by the executive branch?',
    'Analysis of executive branch legal opinions and justifications for military action over time, compared against objective threat assessments and congressional definitions.',
    'If the definition is consistently expanded without clear external justification, it indicates a higher degree of executive extraction and a stronger ''snare'' component. If it tracks genuine changes in threat landscape, it supports the ''rope'' aspect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imminent_threat_definition_ambiguity, empirical, 'Ambiguity in defining ''imminent threat'' allows for executive discretion.').

omega_variable(
    congressional_will_vs_political_expediency,
    'To what extent does congressional ''authorization'' for prolonged campaigns reflect genuine legislative will versus political expediency or deference to executive action?',
    'Analysis of voting patterns, legislative debates, and post-hoc critiques by members of Congress, particularly from the opposition party or those not seeking re-election.',
    'If authorization is primarily driven by political expediency, it suggests a higher ''theater_ratio'' and ''suppression'' of genuine legislative oversight, pushing the constraint closer to a ''snare.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_will_vs_political_expediency, empirical, 'Distinguishing genuine congressional authorization from political deference.').

omega_variable(
    kernel_reading_divergence,
    'Is this ''functional accommodation'' reading a legitimate interpretation of the constitutional war powers, or a strategic rationalization that masks executive power grabs?',
    'Comparative analysis with the ''congressional_primacy_reading'' and ''inherent_executive_reading'' to identify the specific constitutional principles and historical precedents each reading prioritizes, and the structural consequences of each interpretation.',
    'If this reading is found to systematically favor executive power beyond a defensible constitutional balance, it would be reclassified as a more extractive ''tangled_rope'' or ''snare'' from a ''congressional_primacy'' perspective. If it is seen as a pragmatic necessity, it would retain its ''tangled_rope'' classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'This constraint is one reading of the ''war_powers_allocation'' kernel. This reading emphasizes a flexible, context-dependent allocation of war powers, allowing for executive agility in crises while theoretically requiring congressional authorization for prolonged engagements. Sibling readings include ''congressional_primacy_reading'' (emphasizing strict legislative control) and ''inherent_executive_reading'' (emphasizing broad presidential authority). The disagreement is located in the interpretation of constitutional text and historical practice regarding the balance of power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(war__tr_t1965, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1965, 0.28).
narrative_ontology:measurement(war__tr_t1985, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(war__tr_t2001, war_powers_allocation__functional_accommodation_reading, theater_ratio, 2001, 0.45).
narrative_ontology:measurement(war__tr_t2010, war_powers_allocation__functional_accommodation_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(war__tr_t2024, war_powers_allocation__functional_accommodation_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1945, 0.4).
narrative_ontology:measurement(war__be_t1965, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(war__be_t1985, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(war__be_t2001, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 2001, 0.7).
narrative_ontology:measurement(war__be_t2010, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(war__be_t2024, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement(war__su_t1965, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(war__su_t1985, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement(war__su_t2001, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 2001, 0.75).
narrative_ontology:measurement(war__su_t2010, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(war__su_t2024, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, national_security_classification_regime).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, executive_privilege_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'war_powers_allocation' kernel. It focuses on the functional accommodation between executive and legislative branches, where authority shifts based on the operational context (imminent threat vs. prolonged campaign). Its extractiveness varies by context, creating an ambiguity zone that both branches exploit. It is linked to its sibling readings, 'congressional_primacy_reading' and 'inherent_executive_reading', as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
