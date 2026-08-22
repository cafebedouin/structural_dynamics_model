% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__slippery_slope_mechanism, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: end_of_life_authority__slippery_slope_mechanism
 *   human_readable: Eligibility Drift in Autonomy-Grounded End-of-Life Frameworks
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   Jurisdictions that legalize medically assisted death typically begin with
 *   narrow eligibility: competent adults, terminal diagnosis, imminent death,
 *   multiple independent confirmations. Empirically, several such frameworks
 *   have subsequently broadened eligibility — to non-terminal chronic
 *   suffering, to psychiatric conditions, and in some cases to
 *   advance-directive-based access for patients who have since become
 *   incompetent. This story treats that expansion pathway itself as the
 *   constraint under analysis: a structural mechanism, observed across
 *   jurisdictions, by which an autonomy-grounded coordination arrangement
 *   built for one population comes to govern populations who did not
 *   originally fall within its justification and who often cannot exercise
 *   the same contemporaneous, competent consent the framework was built
 *   around.
 *
 * KEY AGENTS:
 *   - assisted_dying_program_administrators: agenda_setter (institutional/arbitrage) — set and revise eligibility criteria over time
 *   - competent_terminal_patients_seeking_access: beneficiary (moderate/constrained) — the framework's originally intended population
 *   - incompetent_patients_under_substituted_judgment: payer (powerless/trapped) — bear outcomes of decisions made on their behalf
 *   - chronic_non_terminal_disability_patients: payer (powerless/constrained) — become eligible as criteria drift from terminal to chronic suffering
 *   - psychiatric_suffering_patients: payer (powerless/trapped) — capacity assessment entangled with the condition being assessed
 *   - healthcare_cost_containment_systems: beneficiary (institutional/arbitrage) — structurally benefit from reduced long-term care expenditure
 *   - disability_rights_advocates: excluded (organized/analytical) — raise structural concerns but are outweighed in policy design
 *   - courts_and_legislatures: observer (institutional/analytical) — adjudicate and periodically ratify or check the drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.68).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.58).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "Eligibility Drift in Autonomy-Grounded End-of-Life Frameworks").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, '4857573b-55a4-47ce-adc9-3f2d09dd55d1').
narrative_ontology:cs_kernel_codification('4857573b-55a4-47ce-adc9-3f2d09dd55d1', formalized).
narrative_ontology:cs_authority_grounding('4857573b-55a4-47ce-adc9-3f2d09dd55d1', practice).
narrative_ontology:cs_interpretation_layer_present('4857573b-55a4-47ce-adc9-3f2d09dd55d1').
narrative_ontology:cs_reading_relation('4857573b-55a4-47ce-adc9-3f2d09dd55d1', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('4857573b-55a4-47ce-adc9-3f2d09dd55d1', end_of_life_authority__sanctity_reading, influences).
narrative_ontology:cs_axiom('4857573b-55a4-47ce-adc9-3f2d09dd55d1', foundational, eligibility_criteria_are_structurally_unstable_once_enacted).
narrative_ontology:cs_axiom_status(eligibility_criteria_are_structurally_unstable_once_enacted, holdable).
narrative_ontology:cs_axiom_grounding('4857573b-55a4-47ce-adc9-3f2d09dd55d1', eligibility_criteria_are_structurally_unstable_once_enacted, empirically_contingent).
narrative_ontology:cs_axiom('4857573b-55a4-47ce-adc9-3f2d09dd55d1', foundational, administrative_review_bodies_cannot_substitute_for_contemporaneous_competent_consent).
narrative_ontology:cs_axiom_status(administrative_review_bodies_cannot_substitute_for_contemporaneous_competent_consent, holdable).
narrative_ontology:cs_axiom_grounding('4857573b-55a4-47ce-adc9-3f2d09dd55d1', administrative_review_bodies_cannot_substitute_for_contemporaneous_competent_consent, empirically_contingent).
narrative_ontology:cs_reference_frame('4857573b-55a4-47ce-adc9-3f2d09dd55d1', competent_terminal_consent_boundary).
narrative_ontology:cs_drift_state('4857573b-55a4-47ce-adc9-3f2d09dd55d1', post_multi_jurisdiction_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4857573b-55a4-47ce-adc9-3f2d09dd55d1', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, healthcare_cost_containment_systems).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, assisted_dying_program_administrators).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients_seeking_access).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients_under_substituted_judgment).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, chronic_non_terminal_disability_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, psychiatric_suffering_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, elderly_patients_under_family_or_institutional_pressure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and revise eligibility criteria, review boards, and reporting requirements for legal assisted dying. Set the pace and direction of criteria expansion (terminal illness, then unbearable suffering, then non-terminal chronic conditions, then advance directives for incompetent patients in some jurisdictions). Collect institutional legitimacy and reduced caseload/cost pressure from an orderly, rule-bound process; face little personal cost when eligibility broadens.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, assisted_dying_program_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% The originally intended population: individuals with decision-making capacity and a terminal diagnosis who want control over timing and manner of death. Genuinely benefit from the framework operating as designed. Their situation does not depend on eligibility criteria expanding beyond their own case, but the framework's political durability is increasingly built on the expanded population.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients_seeking_access, beneficiary,
    moderate, immediate, constrained, national).

% Cannot personally consent; a guardian, family member, or prior advance directive stands in for their present wishes. Once frameworks extend death authorization via substituted judgment or advance directives written years before incapacity, these patients bear the outcome of a decision they cannot confirm, revise, or refuse in their current state. Structurally cannot exit the determination once it is triggered.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_patients_under_substituted_judgment, payer,
    powerless, immediate, trapped, national).

% Live with disabling, non-fatal conditions causing significant suffering but no terminal prognosis. Once eligibility criteria drift from 'terminal' to 'unbearable and hopeless suffering,' they become eligible for assisted death in a system originally justified by imminent, unavoidable dying. Disability advocacy groups report patients citing inadequate social support and disability-related poverty as reasons for seeking death — exit from poverty/support gaps is harder than exit into the program.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, chronic_non_terminal_disability_patients, payer,
    powerless, biographical, constrained, national).

% Suffer from treatment-resistant psychiatric conditions. Where eligibility expands to include psychiatric suffering, capacity assessment becomes entangled with the illness being assessed (hopelessness is both a symptom and a qualifying criterion). Their exit options are structurally compromised by the condition the framework is evaluating them for.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, psychiatric_suffering_patients, payer,
    powerless, biographical, trapped, national).

% Face implicit or explicit pressure from family caregiving burden, institutional bed-cost pressure, or internalized sense of being a burden. As eligibility broadens and normalization increases, the option to end life competes with under-resourced palliative and long-term care alternatives, making 'choice' partly a function of what alternatives are funded.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, elderly_patients_under_family_or_institutional_pressure, payer,
    powerless, immediate, constrained, national).

% Payers and health systems bear substantial costs from prolonged terminal and chronic care. As eligibility for assisted death broadens, aggregate end-of-life and long-term care expenditures fall. No system publicly frames cost savings as a goal, but the fiscal incentive is structurally present and unaddressed by consent-based safeguards.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, healthcare_cost_containment_systems, beneficiary,
    institutional, generational, arbitrage, national).

% Consistently raise concerns in public comment periods and litigation that eligibility drift disproportionately reaches disabled and chronically ill populations who lack adequate social support, but are treated as a minority objection rather than as central stakeholders in eligibility-setting bodies dominated by clinicians and ethicists focused on the autonomy case.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, disability_rights_advocates, excluded,
    organized, generational, analytical, national).

% Adjudicate challenges to eligibility criteria and periodically revise statutes. Receive competing testimony from disability advocates, medical associations, and patient autonomy groups; their rulings and legislative amendments are the primary mechanism by which drift is either ratified or checked.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__slippery_slope_mechanism, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__slippery_slope_mechanism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally regulated, medically supervised process for ending life that substitutes for unregulated, unsafe, or coercive de facto practices (covert overdosing, violent suicide, unregulated assistance) — a genuine coordination improvement over the unregulated status quo for the population it was designed for.
% TRANSFER_FUNCTION: Moves decisional authority over life-ending from informal, unregulated arrangements into a formal administrative and clinical process; as eligibility criteria broaden, it also moves the practical burden of proving continued eligibility for life-sustaining care onto populations (disabled, chronically ill, incompetent) who were not the framework's original target and who bear the downstream cost of underfunded care alternatives.
% ABSENT_VOICES: Incompetent patients cannot speak for themselves by definition; disability rights organizations are typically consulted but structurally outvoted in policy design bodies weighted toward clinical and autonomy-rights framing; psychiatric patients whose hopelessness is symptomatic are rarely treated as authoritative about their own prognosis.
% DISAPPEARANCE_RATIONALE: If eligibility-expansion mechanisms were rolled back to strict competent-terminal-only criteria, competent terminal patients would lose no access; disability advocates and many bioethicists argue the incompetent and non-terminal populations currently at risk would be protected and the world would meaningfully rearrange for them. Program administrators and some autonomy advocates dispute that rollback is warranted, arguing expansion reflects legitimate moral progress rather than drift — hence contested rather than settled in either direction.
% FOUNDING_PROBLEM: Competent, terminally ill adults facing unavoidable, often painful deaths lacked any legal, medically supervised way to control the timing and manner of their death, forcing recourse to unregulated, unsafe, or covert methods.
% FOUNDING_PROBLEM_CORROBORATION: Competent terminal patients and their advocates attest the founding problem remains live and the framework serves it well. Disability rights organizations, some psychiatric ethics researchers, and dissenting members of government review commissions in multiple jurisdictions with broadened eligibility attest, from outside the group that benefits from expanded eligibility, that the mechanism has drifted to address a different and contested problem (chronic suffering, cost, caregiving burden) than the one it was built to solve.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, contested).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__slippery_slope_mechanism, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__slippery_slope_mechanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__slippery_slope_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.68 — substantial but not maximal — because the underlying coordination function (a regulated alternative to unregulated life-ending) remains genuinely present for the originally intended population even as the mechanism extracts from newly incorporated populations. Suppression (0.58) reflects the structural difficulty incompetent and psychiatrically ill patients face in contesting a determination made about or on behalf of them, without treating this as a fully coercive Snare — courts, review boards, and advocacy litigation provide partial, imperfect check. Theater ratio (0.42) captures that procedural safeguards (waiting periods, independent confirmations, review boards) are increasingly cited publicly as proof of continued narrowness even as the underlying population served has broadened — a Goodhart-style substitution of procedural compliance for the original substantive limitation. Accessibility collapse (0.5) and resistance (0.6) are set at a mid-to-elevated point because alternatives (strict statutory limitation, sunset-and-review mechanisms, categorical exclusion of incompetent/non-terminal populations) are contested and actively fought over in courts and legislatures, not foreclosed the way a Mountain's alternatives are.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent terminal patients and program administrators sit near the beneficiary end: the former receive the coordination benefit the framework was designed for, the latter capture institutional legitimacy and reduced downstream caseload pressure regardless of which population is served. Incompetent patients, non-terminal chronic patients, and psychiatric patients sit near the target end: their exit options are trapped or constrained specifically because the qualifying condition (incapacity, chronic suffering, psychiatric hopelessness) is also what prevents them from contesting or revising the determination. Healthcare cost systems are declared a structural beneficiary via directionality derivation from the beneficiary declaration, not because any actor states cost containment as a goal — the incentive is present regardless of stated intent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unregulated death for competent terminal patients) remains genuinely live for the population it was built for — this is not a pure zombie-mandate case. But founding_problem_status is authored as contested rather than dead, because the mechanism under analysis here is specifically the machinery that has moved on to address a different, contested problem (chronic non-terminal suffering, caregiving burden, cost) for populations who did not consent to that extension. Classifying this reading as tangled_rope rather than snare preserves the fact that real coordination value persists for competent terminal patients while naming the asymmetric extraction now falling on incompetent and non-terminal populations through the same administrative structure — collapsing this into a pure Snare would erase the genuine original coordination function; calling it a pure Rope would erase the documented eligibility drift and its victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_drift_vs_moral_progress,
    'Is the observed eligibility expansion evidence of an inherent slippery-slope mechanism in autonomy-based frameworks, or is it legitimate moral learning — society correctly recognizing that unbearable suffering, not proximity to death, was always the relevant moral criterion?',
    'Comparative jurisdictional analysis: track whether expansion correlates with documented cases of eligibility being applied against patients'' contemporaneous wishes (evidence for drift-as-harm) versus expansion correlating with increased patient-reported satisfaction and reduced suffering among newly eligible populations (evidence for moral progress). Longitudinal outcome studies from multiple jurisdictions with different expansion timelines would help resolve this.',
    'If resolved toward moral progress, this reading''s high extractiveness score would be substantially overstated and the constraint would more closely resemble the autonomy_reading''s low-ε profile extended to a broader population. If resolved toward drift-as-harm, the tangled_rope classification undersells the case for snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_drift_vs_moral_progress, conceptual, 'Whether documented eligibility expansion is a harm mechanism or legitimate ethical evolution.').

omega_variable(
    substituted_judgment_reliability,
    'How reliably does substituted judgment (advance directives, guardian/family determination) for incompetent patients track what the patient would actually have wanted at the time of the decision, versus reflecting the interests or burden-fatigue of the deciding party?',
    'Empirical study comparing advance directive content against contemporaneous behavior/statements of patients who later became incompetent, and audit of guardian decision patterns against documented patient preferences where available.',
    'High reliability would weaken the case that incompetent patients are a genuine victim class under this mechanism; low reliability would strengthen it substantially and support reclassifying elements of this constraint toward snare for that subpopulation specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substituted_judgment_reliability, empirical, 'Whether substituted judgment mechanisms track patient preference or proxy/institutional interest.').

omega_variable(
    cost_containment_causal_role,
    'Does healthcare cost pressure play any measurable causal role in eligibility expansion decisions, or is the cost-benefit correlation coincidental to independently motivated ethical arguments for broadening access?',
    'Analysis of legislative and administrative deliberation records, budget impact statements associated with eligibility-expansion proposals, and comparison of expansion timing against fiscal pressure indicators in each jurisdiction''s healthcare system.',
    'Evidence of causal role would substantially strengthen the beneficiary designation for healthcare_cost_containment_systems and support a harder extraction verdict; absence of evidence would suggest this beneficiary declaration should be weighted as structural-incentive-only rather than causally operative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_containment_causal_role, empirical, 'Whether fiscal incentives causally drive eligibility expansion or are merely structurally present.').

omega_variable(
    reading_boundary_ambiguity,
    'Where exactly does the slippery_slope_mechanism reading''s referent end and the sanctity_reading''s referent begin — is expansion to psychiatric and non-terminal populations better modeled as continued operation of THIS mechanism, or as evidence that the sanctity_reading''s predicted failure mode has simply arrived?',
    'Track whether jurisdictions that have expanded eligibility subsequently narrow it again in response to documented harms (supporting a mechanism-reading with a feedback/correction loop) versus continuing to expand monotonically (supporting a sanctity-reading style claim that no principled stopping point exists once intentional life-ending is permitted at all).',
    'If jurisdictions show correction, the slippery_slope_mechanism reading is the more accurate structural account and sanctity_reading''s stronger claim (no principled stopping point exists) is weakened. If expansion is monotonic and uncorrected across jurisdictions, the sanctity_reading''s structural claim gains support and this reading''s tangled_rope classification may understate persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Where the boundary lies between this reading and the sanctity_reading''s predicted failure mode.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0, 0.15).
narrative_ontology:measurement(end__tr_t5, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 5, 0.2).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 10, 0.28).
narrative_ontology:measurement(end__tr_t15, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 15, 0.34).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 20, 0.39).
narrative_ontology:measurement(end__tr_t25, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(end__be_t5, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(end__be_t15, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(end__be_t25, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(end__su_t5, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(end__su_t15, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(end__su_t25, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__slippery_slope_mechanism, 0.1).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__sanctity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the end_of_life_authority kernel. autonomy_reading authors the original competent-terminal arrangement as low-extraction coordination for the population it was designed for. sanctity_reading authors any intentional life-ending as intrinsically extractive on grounds of life's intrinsic value, independent of consent mechanism or population. This story (slippery_slope_mechanism) authors the empirical drift pathway connecting the two: it takes the autonomy_reading's legitimate starting point and traces the documented mechanism by which eligibility criteria expand to populations (incompetent, non-terminal, psychiatric) whose inclusion increasingly resembles what the sanctity_reading warned against. Each reading has its own ε, beneficiaries, and victims; they should not be averaged or reconciled into a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
