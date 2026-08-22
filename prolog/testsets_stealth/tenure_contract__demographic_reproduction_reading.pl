% ============================================================================
% CONSTRAINT STORY: tenure_contract__demographic_reproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__demographic_reproduction_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: tenure_contract__demographic_reproduction_reading
 *   human_readable: Tenure Peer Review as Demographic Gatekeeping (Demographic Reproduction Reading)
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   Tenure review in research universities requires a positive vote by
 *   incumbent faculty committees applying written criteria — research
 *   productivity, teaching, service — plus unwritten weight on 'fit' and
 *   'collegiality'. This story instantiates ONE reading of the
 *   tenure_contract kernel: that the operative effect of the review system is
 *   demographic reproduction — committees calibrate similarity, and
 *   composition persists across generations of hiring despite changing
 *   applicant pools. The epsilon referent is the standing peer-review
 *   arrangement itself, assessed by this reading's own lights; the sibling
 *   readings (academic_freedom_reading, institutional_extraction_reading) are
 *   separate constraint stories with their own epsilon values and victim
 *   sets, linked only through network edges. Claim and metrics are
 *   independent authored facts: the claimed type states what this reading
 *   holds structurally true; the metrics state what it holds descriptively
 *   true of the arrangement's operation. KEY AGENTS (by structural
 *   relationship): - dominant_group_tenured_faculty: agenda-setting incumbent
 *   seat (institutional/arbitrage) — writes and applies the criteria,
 *   collects compositional continuity -
 *   demographically_homogeneous_departments: beneficiary seat
 *   (institutional/constrained) — inherits each cycle's precedent,
 *   slow-composition equilibrium - university_administrations: policy-layer
 *   agenda setter (institutional/mobile) — mandates rubrics, absorbs
 *   litigation, rarely overrides votes - underrepresented_faculty_candidates:
 *   primary target seat (moderate/constrained) — receives verdicts, bears
 *   career-ending downside - minority_early_career_scholars: prospective
 *   target seat (powerless/identity_locked) — highest exposure; standards
 *   internalized during training - teaching_centered_scholars: excluded seat
 *   (organized/constrained) — holds an alternative account of academic work
 *   with no place in the criteria - civil_rights_enforcement_agencies:
 *   analytical observer (institutional/analytical) — after-the-fact
 *   statistical and legal scrutiny
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.72).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.68).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Gatekeeping (Demographic Reproduction Reading)").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, 'c4bf704d-2c23-4125-b262-bb4a1a9d47f2').
narrative_ontology:cs_kernel_codification('c4bf704d-2c23-4125-b262-bb4a1a9d47f2', fixed_text).
narrative_ontology:cs_authority_grounding('c4bf704d-2c23-4125-b262-bb4a1a9d47f2', extraction).
narrative_ontology:cs_interpretation_layer_present('c4bf704d-2c23-4125-b262-bb4a1a9d47f2').
narrative_ontology:cs_reading_relation('c4bf704d-2c23-4125-b262-bb4a1a9d47f2', tenure_contract__academic_freedom_reading, influences).
narrative_ontology:cs_reading_relation('c4bf704d-2c23-4125-b262-bb4a1a9d47f2', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('c4bf704d-2c23-4125-b262-bb4a1a9d47f2', foundational, fit_collegiality_encode_similarity).
narrative_ontology:cs_axiom_status(fit_collegiality_encode_similarity, holdable).
narrative_ontology:cs_axiom_grounding('c4bf704d-2c23-4125-b262-bb4a1a9d47f2', fit_collegiality_encode_similarity, empirically_contingent).
narrative_ontology:cs_axiom('c4bf704d-2c23-4125-b262-bb4a1a9d47f2', secondary, composition_reproduction_is_operative_function).
narrative_ontology:cs_axiom_status(composition_reproduction_is_operative_function, holdable).
narrative_ontology:cs_axiom_grounding('c4bf704d-2c23-4125-b262-bb4a1a9d47f2', composition_reproduction_is_operative_function, empirically_contingent).
narrative_ontology:cs_reference_frame('c4bf704d-2c23-4125-b262-bb4a1a9d47f2', incumbent_demographic_baseline).
narrative_ontology:cs_drift_state('c4bf704d-2c23-4125-b262-bb4a1a9d47f2', post_sffa_contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c4bf704d-2c23-4125-b262-bb4a1a9d47f2', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, dominant_group_tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, demographically_homogeneous_departments).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, minority_early_career_scholars).
narrative_ontology:constraint_vindicates(tenure_contract__demographic_reproduction_reading, merit_neutrality_doctrine).
narrative_ontology:constraint_vindicates(tenure_contract__demographic_reproduction_reading, collegiality_as_quality_proxy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent appointments and staff the evaluation committees, external-letter networks, and floor votes that decide who joins them. They drafted and maintain the criteria language ('fit', 'collegiality') and apply it to candidates whose backgrounds and career paths resemble or differ from their own. Committee service is declinable without career consequence, and departure from an institution typically means arriving elsewhere with standing intact.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, dominant_group_tenured_faculty, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__demographic_reproduction_reading, dominant_group_tenured_faculty, beneficiary).

% Departments whose composition has changed slowly across decades of hiring cycles. Each round of evaluation is conducted by people formed inside the department's existing culture; admitting a candidate who would shift its demographic or intellectual profile requires a level of collective comfort the current process does not compel. Accreditation and doctoral-program obligations bind the department to keep producing graduates and filling lines regardless of individual outcomes.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, demographically_homogeneous_departments, beneficiary,
    institutional, generational, constrained, national).

% Set evaluation policy, respond to discrimination complaints, accreditation reviews, and legislative pressure, and periodically mandate rubric revisions or reporting requirements. Day-to-day judgment of candidates remains with faculty committees; administrations rarely override a negative vote and absorb the litigation and reputational costs when decisions are challenged. Senior administrators move between institutions on short cycles.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, university_administrations, agenda_setter,
    institutional, biographical, mobile, national).

% Scholars approaching the tenure decision whose records are read through criteria weighing fit and collegiality alongside research output. A negative vote ends the appointment and, in most disciplines, effectively the professorial career; the surrounding labor market offers few positions at equivalent rank, and relocation is bounded by partner employment and geography. They receive the verdict but do not sit on the committee that renders it.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates, payer,
    moderate, biographical, constrained, national).

% Graduate students, postdocs, and assistant professors observing senior colleagues' outcomes while assembling their own cases. Their training, mentorship relationships, and professional identity are invested in the professorial track; the visible alternatives carry status and research-resource costs they experience as loss of vocation. Their standards of what a good colleague and a strong record look like were formed inside the same system that will judge them.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, minority_early_career_scholars, payer,
    powerless, biographical, identity_locked, national).

% Faculty at teaching-intensive institutions and community colleges whose students and pedagogy fall outside the research-productivity definitions used in tenure review. Professional associations represent them, but they have no seat in the review conversations that define 'productivity' and 'collegiality'; their account of academic work is invisible to the criteria and to the deliberations that apply them.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, teaching_centered_scholars, excluded,
    organized, biographical, constrained, national).

% Federal and state civil-rights bodies, accreditors, and courts that receive discrimination complaints about tenure decisions, commission statistical analyses of pass rates, and occasionally impose settlements or monitoring. They act after the fact and case by case, and do not participate in designing the evaluation criteria they scrutinize.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, civil_rights_enforcement_agencies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__demographic_reproduction_reading, dominant_group_tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__demographic_reproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools dispersed expert judgment into a single credible, hard-to-reverse decision about which scholars receive permanent positions and institutional resources, sustaining mentorship pipelines and departmental intellectual continuity across decades.
% TRANSFER_FUNCTION: Moves permanent career security, laboratory and travel resources, curricular authority, and disciplinary voice toward candidates whom incumbent committees judge similar to themselves in background, demeanor, and scholarly taste; moves uncompensated evaluation and service labor toward junior faculty.
% ABSENT_VOICES: Rejected candidates see only their own file and never observe the deliberation that ended their career; teaching-centered scholars and the students educated by homogeneous faculties have no seat; scholars who left after negative votes cannot testify without jeopardizing references. Confidence in the criteria's fairness is produced in rooms from which all of these voices are absent.
% DISAPPEARANCE_RATIONALE: Hiring lines, promotion clocks, sabbatical structures, doctoral training, and the professorial career ladder are all organized around the tenure decision. Overnight removal would force universities onto contract-based review within a budget cycle, redistribute committee labor, reopen recent denials, and unravel retirement timing and departmental staffing plans.
% FOUNDING_PROBLEM: The 1940 settlement addressed summary dismissal: professors could be fired for unpopular findings, donor displeasure, or administrative convenience, and institutions needed a credible way to make decades-long staffing commitments to both sides.
% FOUNDING_PROBLEM_CORROBORATION: AAUP archival records and contemporaneous legal commentary attest the original dismissal problem. That the operative function has shifted is attested from outside the benefiting parties by discrimination-litigation findings, published audits of pass-rate disparities, and replication studies of evaluation bias; no attestation of the shifted-function account comes from within the incumbent beneficiary set, which uniformly denies it.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__demographic_reproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__demographic_reproduction_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__demographic_reproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__demographic_reproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72 at interval end) because the criteria that decide permanent careers are unmoored from the productivity measure the system advertises; the gap between stated and operative standards is where the demographic transfer occurs. Suppression (0.68) is a raw structural property, unscaled by power or scope: it reflects the thinness of the professorial labor market, the irreversibility of a negative vote, and the identity investment of the pipeline. Theater ratio (0.58) crosses the proxy-substitution threshold late in the interval — rubric language of excellence and objectivity increasingly describes activity that functions as similarity assessment. Accessibility collapse is moderate (0.45): alternatives to the tenure track exist (teaching-intensive institutions, industry, government) but at status and resource costs that make them poor substitutes, so understanding the gate does not dissolve it. Resistance (0.62) is real and growing: unions, caucuses, litigation, and accreditor pressure meet the criteria directly. The temporal series share one grid (1940-2024, six points) so every metric is authored at every examined time point; extraction accumulates monotonically, theater crosses 0.5 around 2000, and the suppression_requirement series is authored because this story specifically tracks enforcement-machinery change — informal social enforcement formalizing into handbooks, external-letter regimes, and documentation requirements as litigation risk arrived.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting incumbent seat computes a different constraint than the payer seats do. From inside the committee room, review is self-governance: colleagues judging colleagues under criteria they wrote, with the freedom-protective pedigree of the 1940 settlement behind them. From the candidate's chair, the same procedure is a verdict rendered in a vocabulary ('fit', 'collegiality') whose referents are the committee's own composition, carrying a downside that terminates a vocation. The engine computes per-seat classifications from the structural data — power, exit, and directional position — and the divergence between those computations is the finding this story exists to record; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: dominant_group_tenured_faculty and demographically_homogeneous_departments sit near the subsidized end (d near 0) — the arrangement returns compositional continuity and discretionary comfort to them, and the incumbents' arbitrage-grade exit (portable standing, declinable service) pushes them further toward the beneficiary pole. Victim declarations drive the opposite end: underrepresented_faculty_candidates (constrained exit) and especially minority_early_career_scholars (identity-locked exit) sit near the full-target pole, where effective extraction is amplified. University_administrations occupy a middle band: they set policy and absorb challenge costs without collecting the gate's direct product. Scope is national throughout, which the engine treats as harder to verify and scales effective extraction accordingly for targets. Suppression enters the computation unscaled; only extractiveness is scaled by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — insulation of unpopular inquiry from summary dismissal — is plausibly still live for some occupants of the system and plausibly dead as the operative function, which is why founding_problem_status is authored 'contested' rather than 'dead': the mismatch consumer reads status x disappearance_verdict, and a contested status declines to assert the zombie flag while the genealogy remains genuinely disputed. Classifying the arrangement as a tangled rope rather than a snare preserves the real coordination half — pooled expert judgment making hard-to-reverse staffing commitments is a genuine collective problem, and abolishing review would not solve it — while the victim declarations and the enforcement requirement carry the asymmetric half that a pure-coordination claim would launder away. The metrics prevent the reverse error: a rope-only reading would erase the documented pass-rate asymmetries. Mandatrophy here is not resolved; the arrangement's mandate (protect inquiry) and its operative function (maintain composition) have diverged, and that divergence is the live question the sibling readings dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the demographic_reproduction_reading of the tenure_contract kernel; the sibling readings (academic_freedom_reading, institutional_extraction_reading) locate tenure''s persistence in inquiry protection and in early-winner rent capture respectively — which structural element does the shared referent actually turn on?',
    'Compile all three readings and compare computed per-seat classifications over the identical standing arrangement; the reading whose beneficiary/victim structure predicts observed pass-rate and outcome disparities with the fewest auxiliary assumptions carries.',
    'If the freedom reading dominates, effective extraction falls toward coordination cost and the type shifts toward rope; if the extraction reading dominates, the victim set widens to contingent labor and the type shifts toward snare; this reading''s tangled_rope profile holds only while both a real quality-judgment function and demographic asymmetry are present.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-level contest among three readings of tenure; disagreement located in what peer-review discretion primarily protects.').

omega_variable(
    fit_construct_validity,
    'Do ''fit'' and ''collegiality'' ratings predict subsequent scholarly contribution, or do they primarily track demographic and biographical similarity between raters and ratees?',
    'Multi-institution audits correlating committee fit scores with later productivity, controlling for rater-ratee similarity; natural experiments from institutions adopting structured or blinded criteria.',
    'If fit scores carry predictive validity, part of the measured extraction is mispriced coordination cost and epsilon falls; if they track similarity, the criteria are the gatekeeping mechanism itself and epsilon rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fit_construct_validity, empirical, 'Construct validity of the collegiality/fit criteria.').

omega_variable(
    counterfactual_composition_trajectory,
    'What would departmental composition trajectories have been under productivity-only evaluation with identical applicant pools?',
    'Difference-in-differences across institutions that adopted structured rubrics or external blinded review at known dates, against matched institutions that did not.',
    'Determines how much of the measured extraction is attributable to the evaluation criteria versus upstream pipeline effects; a null result would relocate this reading''s target from peer review to the feeder system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_composition_trajectory, empirical, 'Counterfactual attribution of compositional outcomes to review criteria.').

omega_variable(
    suppression_mechanism_split,
    'Is the persistence of underrepresented scholars inside the pipeline despite adverse odds maintained by structural barriers (thin markets, geography, partner and visa constraints) or by internalized standards of fit formed during training?',
    'Post-exit trajectory study: scholars who leave after negative votes and later thrive elsewhere reveal how much of the binding force traveled with them.',
    'If internalized, effective suppression exceeds the structural measure — targets carry the standard into new settings and the arrangement survives individual exits; if structural, removing the barrier releases the population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized components of the gate''s holding power.').

omega_variable(
    coalition_alteration_capacity,
    'Can underrepresented faculty coalitions — caucuses, unions, cluster-hire leverage, accreditor pressure — raise incumbents'' cost of maintaining unmoored fit criteria above the benefit they collect from them?',
    'Track criterion-revision episodes: which survived committee turnover and budget cycles, and which were absorbed back into discretionary language.',
    'Sustained coalition success would push the arrangement toward a transitional profile with declining extraction; repeated absorption of reforms marks entrenchment and supports drift toward a purely extractive profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_alteration_capacity, empirical, 'Whether organized counter-pressure can alter the criteria durably.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 1940, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_demo_read_tr_t1940, tenure_contract__demographic_reproduction_reading, theater_ratio, 1940, 0.2).
narrative_ontology:measurement_basis(tenure_demo_read_tr_t1940, observed).
narrative_ontology:measurement(tenure_demo_read_tr_t1960, tenure_contract__demographic_reproduction_reading, theater_ratio, 1960, 0.28).
narrative_ontology:measurement_basis(tenure_demo_read_tr_t1960, observed).
narrative_ontology:measurement(tenure_demo_read_tr_t1980, tenure_contract__demographic_reproduction_reading, theater_ratio, 1980, 0.42).
narrative_ontology:measurement_basis(tenure_demo_read_tr_t1980, observed).
narrative_ontology:measurement(tenure_demo_read_tr_t2000, tenure_contract__demographic_reproduction_reading, theater_ratio, 2000, 0.52).
narrative_ontology:measurement_basis(tenure_demo_read_tr_t2000, observed).
narrative_ontology:measurement(tenure_demo_read_tr_t2012, tenure_contract__demographic_reproduction_reading, theater_ratio, 2012, 0.56).
narrative_ontology:measurement_basis(tenure_demo_read_tr_t2012, observed).
narrative_ontology:measurement(tenure_demo_read_tr_t2024, tenure_contract__demographic_reproduction_reading, theater_ratio, 2024, 0.58).
narrative_ontology:measurement_basis(tenure_demo_read_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(tenure_demo_read_be_t1940, tenure_contract__demographic_reproduction_reading, base_extractiveness, 1940, 0.4).
narrative_ontology:measurement_basis(tenure_demo_read_be_t1940, observed).
narrative_ontology:measurement(tenure_demo_read_be_t1960, tenure_contract__demographic_reproduction_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement_basis(tenure_demo_read_be_t1960, observed).
narrative_ontology:measurement(tenure_demo_read_be_t1980, tenure_contract__demographic_reproduction_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement_basis(tenure_demo_read_be_t1980, observed).
narrative_ontology:measurement(tenure_demo_read_be_t2000, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement_basis(tenure_demo_read_be_t2000, observed).
narrative_ontology:measurement(tenure_demo_read_be_t2012, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2012, 0.7).
narrative_ontology:measurement_basis(tenure_demo_read_be_t2012, observed).
narrative_ontology:measurement(tenure_demo_read_be_t2024, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2024, 0.72).
narrative_ontology:measurement_basis(tenure_demo_read_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(tenure_demo_read_su_t1940, tenure_contract__demographic_reproduction_reading, suppression_requirement, 1940, 0.5).
narrative_ontology:measurement_basis(tenure_demo_read_su_t1940, observed).
narrative_ontology:measurement(tenure_demo_read_su_t1960, tenure_contract__demographic_reproduction_reading, suppression_requirement, 1960, 0.48).
narrative_ontology:measurement_basis(tenure_demo_read_su_t1960, observed).
narrative_ontology:measurement(tenure_demo_read_su_t1980, tenure_contract__demographic_reproduction_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement_basis(tenure_demo_read_su_t1980, observed).
narrative_ontology:measurement(tenure_demo_read_su_t2000, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement_basis(tenure_demo_read_su_t2000, observed).
narrative_ontology:measurement(tenure_demo_read_su_t2012, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2012, 0.65).
narrative_ontology:measurement_basis(tenure_demo_read_su_t2012, observed).
narrative_ontology:measurement(tenure_demo_read_su_t2024, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2024, 0.68).
narrative_ontology:measurement_basis(tenure_demo_read_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, resource_allocation).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__institutional_extraction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'tenure' conflates at least three structurally distinct claims with different epsilon values, beneficiary/victim sets, and failure modes: inquiry protection (academic_freedom_reading), demographic reproduction (this file), and early-winner rent capture (institutional_extraction_reading). Per the epsilon-invariance principle these are authored as three linked stories over one shared referent rather than one story with a measurement parameter. This reading sits downstream of the freedom reading historically (the freedom settlement built the discretion this reading says is repurposed) and laterally coupled to the extraction reading (the gate selects the winner class whose rents the extraction reading describes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
