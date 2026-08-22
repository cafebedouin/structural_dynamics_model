% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__hybrid_occupation, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Hybrid Competence Occupation (Simulation + Line Audit + Procedural Reinforcement)
 *   domain: safety/organizational/epistemological
 *
 * SUMMARY:
 *   High-reliability organizations (aviation, nuclear, emergency medicine,
 *   maritime) operate under regulatory mandates that competence occupation
 *   requires continuous multi-mechanism exercise: simulation drills,
 *   classroom refreshers, procedural reinforcement, and line audits. No
 *   consensus exists on the optimal configuration of these mechanisms—what
 *   frequency, what combination, what success criteria. This constraint
 *   instantiates the HYBRID-OCCUPATION READING of the competence-occupation
 *   kernel: the claim that multiple, overlapping observables are necessary to
 *   test whether personnel actually occupy the competence domain. This
 *   reading coexists with two siblings: the SIMULATION-SUFFICIENCY reading
 *   (simulation alone is sufficient for competence maintenance) and the
 *   REAL-INCIDENT-NECESSITY reading (only authentic catastrophic incidents
 *   provide the conditions that validate competence occupation). The hybrid
 *   reading is claimed as tangled_rope: it coordinates a genuine problem
 *   (competence maintenance is necessary in high-reliability contexts) while
 *   extracting substantial costs from operational personnel and
 *   organizations. The extraction arises because the lack of consensus on
 *   optimal configuration drives regime expansion—each new research finding,
 *   each new incident, each regulatory update can justify adding another
 *   mechanism, another refresher cycle, another audit layer. Training
 *   administrators and regulators benefit from the appearance of
 *   comprehensive competence assurance; operational personnel and
 *   cost-bearing organizations bear the growing time and resource burden.
 *
 * KEY AGENTS:
 *   - Training regime administrators: design and enforce the multi-mechanism requirement; benefit from administrative authority and budget allocation
 *   - Operational personnel: must comply with all mechanisms; face identity-locked exit (career and credential loss if they exit)
 *   - Regulatory compliance bodies: mandate and legitimize the requirement; benefit from reduced liability exposure
 *   - Cost-bearing organizations: operationalize the mandate; constrained exit (regulatory violation risk)
 *   - Mechanism-validation researchers: study which mechanisms work; excluded from regulatory decision-making
 *   - Incident-analysis specialists: learn from failures; excluded from binding mandate-setting
 *   - High-reliability organizations: operate under the mandate; situated to observe outcome correlation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.62).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.71).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Hybrid Competence Occupation (Simulation + Line Audit + Procedural Reinforcement)").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "safety/organizational/epistemological").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, 'dc4de1d9-051c-4bfe-962e-ba5bb87ef930').
narrative_ontology:cs_kernel_codification('dc4de1d9-051c-4bfe-962e-ba5bb87ef930', distributed).
narrative_ontology:cs_authority_grounding('dc4de1d9-051c-4bfe-962e-ba5bb87ef930', extraction).
narrative_ontology:cs_interpretation_layer_present('dc4de1d9-051c-4bfe-962e-ba5bb87ef930').
narrative_ontology:cs_reading_relation('dc4de1d9-051c-4bfe-962e-ba5bb87ef930', competence_occupation__simulation_sufficiency, coexists_with).
narrative_ontology:cs_reading_relation('dc4de1d9-051c-4bfe-962e-ba5bb87ef930', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_axiom('dc4de1d9-051c-4bfe-962e-ba5bb87ef930', foundational, competence_multidimensional_occupation).
narrative_ontology:cs_axiom_status(competence_multidimensional_occupation, holdable).
narrative_ontology:cs_axiom_grounding('dc4de1d9-051c-4bfe-962e-ba5bb87ef930', competence_multidimensional_occupation, empirically_contingent).
narrative_ontology:cs_axiom('dc4de1d9-051c-4bfe-962e-ba5bb87ef930', foundational, multiple_observables_necessary).
narrative_ontology:cs_axiom_status(multiple_observables_necessary, holdable).
narrative_ontology:cs_axiom_grounding('dc4de1d9-051c-4bfe-962e-ba5bb87ef930', multiple_observables_necessary, deontological).
narrative_ontology:cs_reference_frame('dc4de1d9-051c-4bfe-962e-ba5bb87ef930', multi_mechanism_competence_assurance).
narrative_ontology:cs_drift_state('dc4de1d9-051c-4bfe-962e-ba5bb87ef930', contemporary_regulatory_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dc4de1d9-051c-4bfe-962e-ba5bb87ef930', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_regime_administrators).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, regulatory_compliance_bodies).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, operational_personnel).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, cost_bearing_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, deliver, and validate competence-maintenance programs that mandate simulation exercises, procedural refreshers, line audits, and periodic assessments. Justify the multi-mechanism requirement as necessary for comprehensive skill retention in high-reliability contexts. Control the curriculum, measurement standards, and pass/fail criteria. Collect budget allocations and administrative authority from the regulatory commitment to continuous competence.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_regime_administrators, agenda_setter,
    institutional, generational, arbitrage, continental).

% Must participate in and pass all required training modules: simulation drills, classroom refreshers, procedural reinforcement, and line audits. Time cost is substantial (40-100 hours annually depending on role). Career advancement, licensure, and continued employment are contingent on compliance. Cannot exit competence-maintenance regimes without abandoning professional identity and credentials.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, operational_personnel, payer,
    moderate, biographical, identity_locked, continental).

% Mandate that competence occupation requires continuous multi-mechanism exercise. Issue guidance and standards that legitimize the training regime and enforce organizational adoption. Benefit from the appearance of systematic competence assurance (reduces liability exposure, satisfies public oversight). Do not themselves deliver training; they set requirements that administrators fulfill.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, regulatory_compliance_bodies, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, regulatory_compliance_bodies, agenda_setter).

% Budget and operationalize the training mandate: facilities, instructors, simulation infrastructure, scheduling around operational demands. Training time reduces productive capacity. The constraint's multi-mechanism character inflates training infrastructure costs relative to single-mechanism alternatives. Cannot exit the regime without regulatory violation and organizational sanctions.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, cost_bearing_organizations, payer,
    organized, biographical, constrained, continental).

% Study which training mechanisms most effectively sustain competence in high-reliability contexts. Identify that simulation performance, line audit behavior, and actual incident response each reveal different aspects of skill retention; no single observable fully captures competence occupation. Would argue for evidence-based mechanism selection and proportionality rather than mandated multi-mechanism stacking, but their findings are not binding on regulatory or administrative bodies.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, mechanism_validation_researchers, excluded,
    moderate, generational, mobile, global).

% Examine actual incidents and near-misses to infer what competence states failed and succeeded. Often find that simulation-trained personnel and audit-compliant personnel both failed under authentic conditions, suggesting the multi-mechanism regime does not guarantee competence occupation. Their evidence challenges the regime's legitimacy but does not formally override it; they operate outside the regulatory approval chain.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, incident_analysis_specialists, excluded,
    moderate, biographical, mobile, global).

% Operate under the competence-occupation mandate and bear the governance burden of sustaining multi-mechanism training. Develop organizational practices, scheduling systems, and local validation criteria to satisfy regulatory requirements while managing operational tempo. Situated to see whether the mandated mechanisms correlate with actual safety outcomes or whether the regime has become decoupled from real competence.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, high_reliability_organizations, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__hybrid_occupation, training_regime_administrators).
narrative_ontology:fixing_cost_class(competence_occupation__hybrid_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes shared standards for what it means to maintain competence in high-reliability roles—substituting for consensus about which observables (simulation performance, audit scores, procedural recall, incident response) actually predict safe performance. Creates common framework across organizations and professions so competence claims are mutually intelligible.
% TRANSFER_FUNCTION: Moves time, financial resources, and organizational capacity from operational production to training administration. Training personnel, simulation equipment, facility costs, and personnel time cost flow to training-regime administrators and regulatory bodies; operational personnel and organizations bear the cost without receiving direct compensation.
% ABSENT_VOICES: Mechanism-validation researchers and incident-analysis specialists would argue that the multi-mechanism requirement is not evidence-based and that the regime confuses process compliance with actual competence occupation. They are excluded because their findings enter the regulatory process too slowly to reshape binding mandates; administrators and regulators set the competence definition, not researchers.
% DISAPPEARANCE_RATIONALE: If the multi-mechanism requirement disappeared, organizations would shift to cheaper, simpler training models (likely simulation-heavy or line-audit-only). Competence-maintenance would become decentered—different organizations would use different mechanisms, and no shared standard would exist. Regulatory bodies would face pressure to establish new competence definitions or abandon systematic oversight. The entire infrastructure of training administration would contract.
% FOUNDING_PROBLEM: Early high-reliability operations had no systematic competence-maintenance framework; personnel skill degraded between critical incidents, and organizational responses to competence gaps were ad hoc and inconsistent. The founding mandate was to establish continuous, systematized competence occupation so critical roles were reliably staffed by current-capable personnel.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and training administrators attest the founding problem is still live—skill decay is continuous and unstructured training leaves competence gaps. Mechanism-validation researchers and incident-analysis specialists attest the problem was substantially solved (organizational learning and incident reporting now drive targeted refresher training; the multi-mechanism stacking is unnecessary overhead) and the regime persists as administrative convenience and regulatory theater. Legislative hearings and published incident investigations from outside the benefiting parties support the 'overhead' reading.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__hybrid_occupation, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__hybrid_occupation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__hybrid_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-to-high because the multi-mechanism requirement imposes substantial time and cost without consensus that the full stack is necessary. The measurement series shows extractiveness rising from 0.48 to 0.62 over the interval—this reflects the accumulation of mechanisms and refresh-cycle inflation that occurs when no determinate optimization problem exists (new research findings and regulatory updates justify adding mechanisms rather than optimizing the existing set). Theater ratio (0.44) is substantial and rising: simulation drills serve genuine competence maintenance, but line audits increasingly serve compliance documentation rather than skill detection; procedural reinforcement becomes pro-forma classroom time; the administrative overhead of coordinating multiple mechanisms inflates relative to actual skill-detection function. Suppression (0.71) is high because operational personnel cannot exit the regime without abandoning professional credentials (identity-locked exit), and organizations cannot exit without regulatory sanctions (constrained exit). The constraint persists through active enforcement: regulatory bodies issue mandates, training administrators police compliance, organizations enforce personnel participation. The measurement trajectory plateaus near the end of the interval (extractiveness, theater, suppression all flatten between t=20 and t=25), suggesting the regime has reached an equilibrium where further addition is administratively difficult despite the lack of optimization consensus.
 *
 * PERSPECTIVAL GAP:
 *   Training administrators and regulators experience this constraint as genuine coordination (necessary competence assurance, comprehensive coverage). Operational personnel and cost-bearing organizations experience it as enforced extraction (perpetual training burden with no clear endpoint, no consensus on whether the full stack is necessary). The engine should compute different types from each seat: administrators see a rope-like coordination function with modest overhead; payers see a snare-like extraction that persists through identity-lock and regulatory constraint. The authored metrics (extractiveness 0.62, suppression 0.71, theater 0.44) describe the constraint's actual operation across all seats; the claimed type (tangled_rope) sits between these two poles—the structure has real coordination function (competence maintenance matters) AND asymmetric extraction (the configuration is not consensus-optimized, costs exceed benefit).
 *
 * DIRECTIONALITY LOGIC:
 *   Training administrators (agenda_setter, institutional power, arbitrage exit) receive budget authority, administrative control, and the appearance of comprehensive competence assurance—low directionality (d ≈ 0.2-0.3), strong beneficiary position. Regulators (secondary agenda_setter, institutional power, arbitrage exit) benefit from liability reduction and public legitimacy—low directionality (d ≈ 0.2-0.3). Operational personnel (payer, moderate power, identity-locked exit) bear the full time cost and cannot exit without credential loss—high directionality (d ≈ 0.75-0.85). Cost-bearing organizations (payer, organized power, constrained exit) bear infrastructure and scheduling costs with no benefit—high directionality (d ≈ 0.7-0.8). Mechanism researchers (excluded, moderate power, mobile exit) would benefit from a more evidence-based regime but are structurally outside the enforcement chain—analytical directionality. The asymmetry between beneficiary and payer seats should trigger the engine's per-seat type computation: administrators may compute as rope, payers as snare or tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competence gaps in high-reliability contexts) is contested: administrators and regulators claim it is still live; mechanism researchers and incident analysts claim it was substantially solved and the regime now persists as overhead. The disappearance verdict is world_rearranges: if the multi-mechanism mandate disappeared, organizations would optimize to simpler training and regulatory frameworks would need redesign. This mismatch (live problem status claimed by beneficiaries; contested/dead status claimed by excluded researchers) is classic mandatrophy setup. The theater ratio rising from 0.28 to 0.44 supports the mandatrophy signal: increasing share of training activity is compliance documentation rather than skill detection. The regime has not resolved its founding problem—competence occupation remains contested—and has accumulated mechanisms without consensus on optimization, suggesting institutional inertia. However, the regime is not a piton: administrators and regulators actively defend it (suppression is high), making it tangled_rope rather than theater-only piton. The mandatrophy is PARTIAL: genuine coordination function persists, but it is increasingly obscured by extraction and theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_observability_multiplicity,
    'Which observable(s) constitute valid evidence of competence occupation? Simulation performance, line audit results, procedural knowledge, actual incident response, or some combination?',
    'Controlled prospective studies correlating each observable with subsequent incident outcomes; incident investigations that establish which trained competencies failed or succeeded in authentic conditions.',
    'If a single observable (e.g., simulation performance) correlates strongly with incident outcomes, the multi-mechanism requirement becomes unjustifiable overhead. If different incidents reveal different failure modes (some demand simulation-trained reflexes, others demand procedural knowledge, others demand line judgment), then multi-mechanism occupation is structurally sound. If no mechanism correlates reliably, the entire regime loses justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_observability_multiplicity, empirical, 'Multiple observables of competence occupation—no consensus on which matters.').

omega_variable(
    skill_decay_curve_heterogeneity,
    'Do different competence domains decay at different rates, requiring mechanisms tuned to each domain''s dynamics?',
    'Longitudinal measurement of skill decay across domains (procedural knowledge, situational awareness, reflexive response, judgment) with and without each training mechanism. Fit decay curves per domain and compute optimal refresh intervals.',
    'If decay curves are homogeneous, a single mechanism (e.g., annual simulation) might suffice for all domains. If heterogeneous, different mechanisms become necessary for different domains, validating the multi-mechanism requirement. If some domains decay slowly while the regime mandates frequent refresher cycles, the regime includes unnecessary theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_decay_curve_heterogeneity, empirical, 'Whether skill decay is uniform or domain-specific, driving mechanism selection.').

omega_variable(
    incident_as_authentic_competence_test,
    'Do actual catastrophic incidents reveal competence gaps that simulation and audit fail to detect, such that real incidents are necessary complements to multi-mechanism training?',
    'Incident investigations isolating which competence states failed, cross-referenced with the personnel''s recent training records (simulation scores, audit results, procedural test results). Track whether undetected gaps correlate with particular mechanism failures.',
    'If incidents consistently reveal gaps not predicted by any single mechanism, the multi-mechanism approach is validated. If incidents show gaps that all mechanisms missed equally, the regime lacks predictive validity. If incidents show personnel with high simulation scores and audit compliance failing, the regime is theater; if they show personnel with low scores succeeding, the regime is unnecessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incident_as_authentic_competence_test, empirical, 'Whether real incidents validate or undermine multi-mechanism competence occupation.').

omega_variable(
    mechanism_configuration_closure,
    'Is there a closed optimization problem with a determinate answer (the right set of mechanisms and refresh frequencies), or is training-regime design a perpetually open research frontier?',
    'Meta-analysis of published mechanism-validation studies; structured expert elicitation on mechanism interdependencies; organizational case studies where training configuration changed and outcomes were measured.',
    'If the optimization problem is closed, the multi-mechanism requirement can be finalized and compliance becomes routine governance. If it is perpetually open, the mandate to use ''continuous multi-mechanism exercise'' without consensus on configuration becomes a recipe for perpetual training inflation—each new research finding triggers regime expansion rather than optimization. Theater ratio stays high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mechanism_configuration_closure, conceptual, 'Whether training-regime optimization is a solvable problem or structurally open-ended.').

omega_variable(
    reading_contest_boundary,
    'Does this reading (hybrid multi-mechanism occupation) foreclose the simulation-sufficiency reading or merely offer a competing claim?',
    'Kernel-level analysis: if hybrid occupation''s core premise is ''multiple observables are necessary to test competence,'' does that logically rule out the simulation-sufficiency claim ''simulation alone is sufficient''? Or does it merely dispute the empirical sufficiency claim while leaving the logical space open for either reading to be true given different evidence?',
    'If the readings logically foreclose each other, they cannot coexist in a single framework (one kernel, two readings, strict incompatibility). If they coexist as different parties'' competing empirical claims about the same mechanism, the contest remains open and the constraint family remains under contention. The classification (tangled_rope vs. snare) may shift if the foreclosure relation is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_boundary, conceptual, 'Whether the hybrid-occupation reading forecloses or coexists with the simulation-sufficiency reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.28).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__hybrid_occupation, theater_ratio, 5, 0.32).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__hybrid_occupation, theater_ratio, 10, 0.37).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__hybrid_occupation, theater_ratio, 15, 0.41).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__hybrid_occupation, theater_ratio, 20, 0.44).
narrative_ontology:measurement(comp_tr_t25, competence_occupation__hybrid_occupation, theater_ratio, 25, 0.44).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(comp_be_t5, competence_occupation__hybrid_occupation, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(comp_be_t10, competence_occupation__hybrid_occupation, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(comp_be_t15, competence_occupation__hybrid_occupation, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(comp_be_t20, competence_occupation__hybrid_occupation, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(comp_be_t25, competence_occupation__hybrid_occupation, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(comp_su_t5, competence_occupation__hybrid_occupation, suppression_requirement, 5, 0.67).
narrative_ontology:measurement(comp_su_t10, competence_occupation__hybrid_occupation, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(comp_su_t15, competence_occupation__hybrid_occupation, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(comp_su_t20, competence_occupation__hybrid_occupation, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(comp_su_t25, competence_occupation__hybrid_occupation, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__hybrid_occupation, 0.12).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__real_incident_necessity).

% DUAL FORMULATION NOTE:
% The competence-occupation kernel admits three structurally distinct readings. The hybrid-occupation reading (this story) claims multiple mechanisms are necessary. The simulation-sufficiency reading would argue that simulation alone suffices and other mechanisms are overhead. The real-incident-necessity reading would argue that only authentic incidents validate competence occupation. Each reading has a different ε (extractiveness of the arrangement it instantiates), different beneficiaries/victims, and a different classification. All three are linked via network.affects_constraints to model the kernel family. The disagreement is located in what constitutes valid evidence of competence occupation—a conceptual question about observables, not a factual dispute about incident outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__hybrid_occupation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
