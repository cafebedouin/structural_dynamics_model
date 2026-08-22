% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__parliamentary_constraint_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Fifth Republic Parliamentary Constraint on Executive
 *   domain: constitutional/law/political
 *
 * SUMMARY:
 *   This constraint story captures the parliamentary constraint reading of
 *   the French Fifth Republic constitution (1958), under which the President
 *   and Government are coordinate executives requiring legislative
 *   authorization for most policy implementation. In this reading, the
 *   National Assembly majority exercises democratic constraint over the
 *   executive through confidence procedures and legislative gatekeeping,
 *   making the executive a structural payer while the legislative majority is
 *   the primary beneficiary. The constraint is instantiated as a formalized
 *   commitment system with active enforcement by the Constitutional Council,
 *   but its operation is contested by hyper-presidential and
 *   cohabitation-equilibrium sibling readings of the same kernel.
 *
 * KEY AGENTS:
 *   - executive_branch (President and Government): Primary target (powerful/constrained) â bears the loss of unilateral policy implementation capacity when the Assembly withholds confidence or blocks legislation.
 *   - legislative_majority (National Assembly majority): Primary beneficiary (institutional/constrained) â captures policy initiative and government formation control.
 *   - constitutional_council: Agenda-setter/enforcer (institutional/analytical) â adjudicates the boundary between executive and legislative authority.
 *   - constitutional_jurists: Analytical observer (analytical/analytical) â evaluates the competing readings of the constitutional kernel.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.32).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.45).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Fifth Republic Parliamentary Constraint on Executive").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional/law/political").

domain_priors:requires_active_enforcement(fifth_republic_constitution__parliamentary_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, '1131243f-0ac3-4d68-a7df-a54ba5fe7786').
narrative_ontology:cs_kernel_codification('1131243f-0ac3-4d68-a7df-a54ba5fe7786', formalized).
narrative_ontology:cs_authority_grounding('1131243f-0ac3-4d68-a7df-a54ba5fe7786', lineage).
narrative_ontology:cs_interpretation_layer_present('1131243f-0ac3-4d68-a7df-a54ba5fe7786').
narrative_ontology:cs_reading_relation('1131243f-0ac3-4d68-a7df-a54ba5fe7786', fifth_republic_constitution__hyper_presidential_reading, forecloses).
narrative_ontology:cs_reading_relation('1131243f-0ac3-4d68-a7df-a54ba5fe7786', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('1131243f-0ac3-4d68-a7df-a54ba5fe7786', foundational, parliamentary_majority_confidence_supremacy).
narrative_ontology:cs_axiom_status(parliamentary_majority_confidence_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('1131243f-0ac3-4d68-a7df-a54ba5fe7786', parliamentary_majority_confidence_supremacy, conventional).
narrative_ontology:cs_axiom('1131243f-0ac3-4d68-a7df-a54ba5fe7786', foundational, executive_coordination_not_command).
narrative_ontology:cs_axiom_status(executive_coordination_not_command, holdable).
narrative_ontology:cs_axiom_grounding('1131243f-0ac3-4d68-a7df-a54ba5fe7786', executive_coordination_not_command, conventional).
narrative_ontology:cs_reference_frame('1131243f-0ac3-4d68-a7df-a54ba5fe7786', coordinate_executive_under_parliament).
narrative_ontology:cs_drift_state('1131243f-0ac3-4d68-a7df-a54ba5fe7786', contemporary_fifth_republic_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1131243f-0ac3-4d68-a7df-a54ba5fe7786', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, executive_branch).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, legislative_confidence_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The President and Government must obtain legislative authorization for most policy implementation and face confidence votes that can force resignation. They retain limited constitutional bypasses such as Article 49-3, ordinances, and dissolution, but these carry significant political costs and do not restore full unilateral autonomy.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, executive_branch, payer,
    powerful, biographical, constrained, national).

% Controls the National Assembly agenda and the confidence procedure, thereby determining whether the executive can govern effectively. Derives institutional power and democratic mandate from the constitutional requirement that the executive secure legislative authorization.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority, beneficiary,
    institutional, biographical, constrained, national).

% Adjudicates disputes over the domain of law versus regulatory power, enforcing the constitutional boundary that requires legislative authorization for specified policy areas. Its rulings directly determine whether executive action must pass through the legislative channel.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_council, agenda_setter,
    institutional, generational, analytical, national).

% Analyze and debate the competing readings of the 1958 constitutional kernel, assessing whether the text structurally mandates legislative constraint, presidential supremacy, or a negotiated dual-executive equilibrium.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_jurists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__parliamentary_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the separation of powers by requiring the executive to obtain legislative authorization for policy, ensuring that government action reflects the will of the parliamentary majority and preventing unilateral executive rule.
% TRANSFER_FUNCTION: Transfers policy initiative and control over government formation from the executive to the legislative majority; when the assembly blocks legislation or withholds confidence, executive policy autonomy is transferred to the parliamentary chamber.
% ABSENT_VOICES: Parliamentary minority parties and dissenting groups whose policy preferences are overridden by the majority beneficiary; also presidentialist legal scholars who would argue for a direct national mandate interpretation but are marginalized in this reading's framework.
% DISAPPEARANCE_RATIONALE: If the requirement for legislative authorization vanished, the executive would govern by decree or direct decree, the National Assembly would lose its primary lever over government formation and policy, and the Fifth Republic would collapse into a hyper-presidential or fully parliamentary system depending on the replacement.
% FOUNDING_PROBLEM: The Fourth Republic's parliamentary instability and executive weakness, where governments fell frequently and lacked coherent authority; the 1958 design sought a strong but accountable executive balanced by a stable parliamentary majority.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative jurists outside the immediate beneficiary parties attest to the Fourth Republic's instability as the historical trigger. However, the claim that the current parliamentary constraint is the necessary and live solution is contested by hyper-presidential scholars, while parliamentary-majority jurists and opposition public-law scholars corroborate the continued relevance of legislative constraint.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 0.32, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).
:- end_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.32, reflecting low but non-zero extraction of executive autonomy: the executive cannot implement policy without legislative authorization, which transfers agenda control to the parliamentary majority. Suppression is 0.45, representing the moderate coercive force of constitutional enforcement (Constitutional Council review, parliamentary procedure) that prevents executive bypass. Theater ratio is low (0.28) because the legislative authorization requirement is largely functional, though some rise reflects increased use of constitutional bypasses that simulate legislative authorization. Accessibility collapse is moderate (0.50): alternative constitutional arrangements are imaginable and observed in other jurisdictions. Resistance is 0.55: the executive persistently seeks to expand autonomous authority through decree powers, referenda, and institutional workarounds.
 *
 * PERSPECTIVAL GAP:
 *   The executive branch experiences this constraint as a democratic check that extracts policy autonomy, while the legislative majority experiences it as the legitimate exercise of representative sovereignty. The Constitutional Council experiences it as a neutral enforcement mechanism, but its rulings on the domain of law versus regulatory power directly determine the intensity of extraction. The engine will compute different seat classifications: the executive seat will show higher effective extraction (directionality near target), while the legislative majority seat will show subsidy or low extraction (directionality near beneficiary).
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative majority is declared beneficiary because the constraint's primary transfer flows to it: control over government formation, confidence votes, and legislative authorization. The executive branch is declared victim (payer) because it bears the cost of lost unilateral capacity. These declarations structurally derive directionality: executive_branch gets d near the target end, legislative_majority gets d near the beneficiary end. No override is needed because the power and exit profiles match the structural relationship: the executive is powerful but constrained by constitutional text, while the majority is institutional and similarly constrained but benefits from the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mandatrophy mislabeling because it preserves a genuine coordination function (democratic accountability, prevention of executive tyranny) while acknowledging asymmetric extraction (executive autonomy is the cost). Without the coordination function, the constraint would be a snare enabling legislative majoritarian domination; without the victim declaration, it would be misread as a pure rope ignoring the executive's structural cost. The R5 genealogy confirms the founding problem (Fourth Republic instability) is contested in status, and the current arrangement's persistence is tied to its democratic justification rather than raw extraction, distinguishing it from a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    executive_victim_or_democratic_cost,
    'Is the executive''s loss of unilateral policy capacity under this reading a genuine extraction of autonomy, or merely the legitimate institutional cost of democratic coordination?',
    'Comparative analysis of semi-presidential systems: if other coordinate systems show similar executive constraint without functional impairment, the cost is coordination overhead; if the French executive uniquely suffers structural disadvantage, it is extraction.',
    'If legitimate cost, the constraint edges toward Rope; if extraction, it remains Tangled Rope with the executive as victim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_victim_or_democratic_cost, conceptual, 'Whether executive constraint is extraction or coordination cost').

omega_variable(
    kernel_text_ambiguity_1958,
    'Does the 1958 constitutional text unambiguously support the parliamentary constraint reading, or is it sufficiently open to sustain the hyper-presidential sibling reading within the same textual framework?',
    'Historical-textual analysis of the travaux prÃ©paratoires and subsequent constitutional amendments; doctrinal survey of French public law scholarship outside the beneficiary camps.',
    'If the text is ambiguous, this reading and its siblings are irreducibly competing framings; if the text clearly supports parliamentary constraint, the hyper-presidential reading is a false summit or deceptive naturalization of presidential power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_text_ambiguity_1958, empirical, 'Constitutional text ambiguity and competing reading validity').

omega_variable(
    practice_drift_presidentialism,
    'Has the Fifth Republic''s constitutional practice drifted so far toward presidential dominance that the parliamentary constraint reading no longer describes the operating system, despite remaining a valid textual interpretation?',
    'Quantitative tracking of Article 49-3 usage, ordinance frequency, and dissolution threats versus actual legislative blockades and confidence withdrawals over the interval.',
    'If practice has substantially drifted, the constraint is becoming a piton (theatrical maintenance of a dead coordination function) or the reading is descriptively obsolete; if practice remains mixed, the reading retains structural validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_drift_presidentialism, empirical, 'Drift between constitutional text and presidential practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 0, 66).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fift_tr_t10, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(fift_tr_t20, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(fift_tr_t30, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(fift_tr_t40, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(fift_tr_t50, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(fift_tr_t60, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(fift_tr_t66, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 66, 0.28).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(fift_be_t10, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(fift_be_t20, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(fift_be_t30, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(fift_be_t40, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(fift_be_t50, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(fift_be_t60, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 60, 0.28).
narrative_ontology:measurement(fift_be_t66, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 66, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fift_su_t10, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(fift_su_t20, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(fift_su_t30, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(fift_su_t40, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(fift_su_t50, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement(fift_su_t60, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement(fift_su_t66, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 66, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the fifth_republic_constitution kernel. It is decompositionally paired with hyper_presidential_reading and cohabitation_equilibrium_reading, which instantiate structurally distinct constraints from the same constitutional text. The epsilon values differ because the referent (the standing arrangement under each reading) assigns different directionalities and beneficiary/victim structures to the same institutional actors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
