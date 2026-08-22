% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy: Courts as Final Constitutional Arbiter
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint instantiates the judicial supremacy reading of the
 *   constitutional authority boundary kernel. It treats the constitutional
 *   text as establishing federal courts â particularly the Supreme Court
 *   â as the final and unchallengeable arbiters of all constitutional
 *   questions, with power to invalidate legislative and executive acts and
 *   foreclose legislative override. This reading is structurally distinct
 *   from its siblings: coordinate construction (distributed branch authority)
 *   and parliamentary primacy (legislative sovereignty). The arrangement
 *   coordinates governance by providing finality, while asymmetrically
 *   extracting policy control from elected branches and concentrating
 *   interpretive authority in the judiciary.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Primary agenda-setter and beneficiary (institutional/generational) â accumulates interpretive authority and docket control
 *   - federal_legislature: Primary payer (institutional/generational) â policy space constrained by judicial veto and anticipatory compliance
 *   - executive_branch: Secondary payer (institutional/biographical) â administrative action subordinate to judicial review
 *   - state_governments: Tertiary payer (institutional/generational) â state autonomy bounded by federal judicial interpretation
 *   - constitutional_rights_claimants: Beneficiary (organized/biographical) â gains a channel for rights enforcement against majoritarian action
 *   - constitutional_scholars: Analytical observer (analytical/civilizational) â external critical perspective on the doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.55).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy: Courts as Final Constitutional Arbiter").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, 'c0f7c653-aa9f-4fa4-8d7a-a9860abceceb').
narrative_ontology:cs_kernel_codification('c0f7c653-aa9f-4fa4-8d7a-a9860abceceb', fixed_text).
narrative_ontology:cs_authority_grounding('c0f7c653-aa9f-4fa4-8d7a-a9860abceceb', lineage).
narrative_ontology:cs_interpretation_layer_present('c0f7c653-aa9f-4fa4-8d7a-a9860abceceb').
narrative_ontology:cs_reading_relation('c0f7c653-aa9f-4fa4-8d7a-a9860abceceb', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('c0f7c653-aa9f-4fa4-8d7a-a9860abceceb', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('c0f7c653-aa9f-4fa4-8d7a-a9860abceceb', foundational, judicial_finality_inherent_in_text).
narrative_ontology:cs_axiom_status(judicial_finality_inherent_in_text, holdable).
narrative_ontology:cs_axiom_grounding('c0f7c653-aa9f-4fa4-8d7a-a9860abceceb', judicial_finality_inherent_in_text, conventional).
narrative_ontology:cs_axiom('c0f7c653-aa9f-4fa4-8d7a-a9860abceceb', foundational, counter_majoritarian_veto_legitimate).
narrative_ontology:cs_axiom_status(counter_majoritarian_veto_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('c0f7c653-aa9f-4fa4-8d7a-a9860abceceb', counter_majoritarian_veto_legitimate, deontological).
narrative_ontology:cs_reference_frame('c0f7c653-aa9f-4fa4-8d7a-a9860abceceb', constitutional_finality_framework).
narrative_ontology:cs_drift_state('c0f7c653-aa9f-4fa4-8d7a-a9860abceceb', contemporary_polarized_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c0f7c653-aa9f-4fa4-8d7a-a9860abceceb', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_rights_claimants).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, federal_legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, state_governments).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, judicial_review_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possesses final authority to interpret the constitutional text and invalidate legislative and executive acts. Derives institutional prestige, docket control, and interpretive monopoly from this position. Exit is constrained by lifetime appointment norms and the identity-bound nature of juridical role, though individual judges may retire.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, federal_judiciary, beneficiary).

% Enacts legislation subject to judicial override on constitutional grounds. Policy space is constrained by anticipatory compliance with perceived judicial preferences. Lacks reliable legislative override mechanism for constitutional decisions. Exit options are limited to cumbersome amendment processes or jurisdiction-stripping maneuvers that face their own constitutional challenges.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, federal_legislature, payer,
    institutional, generational, constrained, national).

% Executes laws and promulgates regulations subject to judicial review and invalidation. Administrative policy preferences can be overturned by courts. Executive constitutional interpretation is treated as persuasive at best, subordinate to judicial say-so. Exit constrained by oath to uphold judicial interpretations and political costs of defiance.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% State legislation and administration are subject to federal judicial supremacy via supremacy clause and incorporation. State policy autonomy is bounded by federal judicial interpretation of federal constitutional limits. Exit options historically included interposition and nullification, but these are legally disfavored and politically costly.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, state_governments, payer,
    institutional, generational, constrained, national).

% Litigants and advocacy groups seeking to enforce constitutional rights against legislative or executive action. Benefit from a final arbiter that can invalidate offending laws. Depend on judicial docket and doctrinal mood; their exit is constrained by the cost and uncertainty of litigation, though they are structurally advantaged compared to majoritarian interests.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_rights_claimants, beneficiary,
    organized, biographical, constrained, national).

% Analyze and critique the doctrine of judicial supremacy from external disciplinary perspectives (law, political science, philosophy). Neither collect rents nor bear direct costs from the arrangement. Produce competing readings of the constitutional authority boundary, including coordinate construction and parliamentary primacy frameworks.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__judicial_supremacy_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides final, uniform resolution of constitutional disputes, preventing inter-branch stalemate and establishing stable ground rules for governance. Offers a dedicated channel for minority rights enforcement against majoritarian legislation.
% TRANSFER_FUNCTION: Moves interpretive authority and policy veto power from elected legislative and executive branches to the federal judiciary; shifts policy outcomes toward positions favored by rights claimants and judicial doctrinal preferences.
% ABSENT_VOICES: Proponents of legislative supremacy and coordinate construction (departmentalist executive lawyers, parliamentary sovereignty theorists, populist constitutional movements) are structurally excluded from the interpretive framework; their objections are treated as legally irrelevant within this reading's doctrine.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, legislative and executive acts previously invalidated would stand, inter-branch conflicts over constitutional meaning would proliferate without a final arbiter, and the judiciary would lose its coordinate-dominant position; constitutional law would fragment into competing branch interpretations.
% FOUNDING_PROBLEM: Constitutional ambiguity and inter-branch conflict over the meaning of the fundamental law, risking instability and conflicting enforcement if each branch interpreted the constitution independently.
% FOUNDING_PROBLEM_CORROBORATION: The federal judiciary attests the problem is still live, citing the need for uniform constitutional meaning. Legislative historians, departmentalist scholars, and comparative constitutionalists outside the beneficiary set attest the founding problem was manageable through political processes and that judicial supremacy created a counter-majoritarian democratic deficit rather than solving an intractable coordination failure.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_authority_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the judiciary wields a counter-majoritarian veto with no legislative override, creating interpretive monopoly rents. Suppression is moderate-high (0.55): not violent coercion, but the structural foreclosure of legislative override and the doctrinal exclusion of coordinate construction alternatives. Theater ratio is low-moderate (0.25): much judicial activity is functionally real (deciding cases), though some doctrinal reasoning is performative maintenance of supremacy. Accessibility collapse (0.60) reflects that alternatives like departmentalism or parliamentary override are intellectually known but structurally closed. Resistance (0.45) captures persistent academic and intermittent political challenges. The measurement series share a single time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, the constraint is necessary coordination preventing constitutional chaos and protecting rights; from the legislature's seat, it is democratically illegitimate extraction of policy control by unelected judges. The engine computes this divergence from the structural data without adjudicating the normative dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary sits at low directionality as both agenda-setter and beneficiary: it administers the constraint and captures interpretive authority. Constitutional rights claimants sit at low directionality as beneficiaries of the coordination function. Federal legislature, executive branch, and state governments sit at high directionality as targets: they bear the costs of constrained policy space and invalidated acts. Constitutional scholars are analytical with no directionality effect.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling because it carries a live coordination function (final dispute resolution, rights enforcement channel) alongside its extraction function (interpretive monopoly, legislative disempowerment). Without the coordination narrative, it would read as a pure snare; without the victim set and high extractiveness, it would read as a rope. The hybrid structure is necessary to capture judicial supremacy accurately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_monopoly_rent_vs_cost,
    'Does the federal judiciary accrue extractive rents from its interpretive monopoly, or does the coordination benefit of finality fully absorb the measured extraction?',
    'Comparative institutional analysis of regimes with and without judicial supremacy; measurement of policy divergence between judicial preferences and legislative median.',
    'If rents exceed coordination costs, the classification tilts toward snare; if coordination absorbs extraction, toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_rent_vs_cost, conceptual, 'Whether judicial extraction is rent or necessary coordination cost').

omega_variable(
    coordinate_construction_sub_rosa,
    'Do executive and legislative branches persist in independent constitutional interpretation in practice despite judicial supremacy doctrine?',
    'Empirical study of agency constitutional interpretation, signing statements, and legislative responses to judicial rulings.',
    'If coordinate construction persists sub rosa, effective extraction is lower than doctrinal supremacy suggests, and the constraint operates more as contested tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinate_construction_sub_rosa, empirical, 'Whether coordinate construction survives beneath judicial supremacy doctrine').

omega_variable(
    democratic_override_feasibility,
    'Can a mobilized political majority realistically override judicial constitutional interpretation through amendment, court-packing, or jurisdiction-stripping?',
    'Historical case studies of successful and failed overrides; political science models of amendment feasibility.',
    'If override is structurally feasible, exit is less constrained than modeled and directionality for legislative victims is damped; if prohibitively costly, extraction is amplified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_override_feasibility, empirical, 'Feasibility of democratic override of judicial supremacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 0, 220).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(cons_tr_t80, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(cons_tr_t120, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 120, 0.22).
narrative_ontology:measurement(cons_tr_t160, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 160, 0.24).
narrative_ontology:measurement(cons_tr_t220, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 220, 0.25).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(cons_be_t80, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(cons_be_t120, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 120, 0.62).
narrative_ontology:measurement(cons_be_t160, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 160, 0.65).
narrative_ontology:measurement(cons_be_t220, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 220, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(cons_su_t80, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 80, 0.45).
narrative_ontology:measurement(cons_su_t120, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 120, 0.5).
narrative_ontology:measurement(cons_su_t160, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 160, 0.52).
narrative_ontology:measurement(cons_su_t220, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 220, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_authority_boundary kernel, decomposed from the colloquial label 'constitutional review' into structurally distinct claims. Judicial supremacy treats the kernel as establishing courts as final arbiters; coordinate construction treats it as distributing authority; parliamentary primacy treats it as subordinate to legislative sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
