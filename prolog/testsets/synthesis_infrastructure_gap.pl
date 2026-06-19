% ============================================================================
% CONSTRAINT STORY: synthesis_infrastructure_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_synthesis_infrastructure_gap, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: synthesis_infrastructure_gap
 *   human_readable: Biomedical Knowledge Synthesis Infrastructure Gap
 *   domain: epistemology/institutional/health
 *
 * SUMMARY:
 *   Biomedical knowledge production has validated infrastructure at two
 *   layers: institutional science produces single-domain experimental
 *   findings through peer review and replication; clinical translation
 *   converts those findings into single-variable interventions. The synthesis
 *   layer—connecting validated components across domains into systems-level
 *   frameworks—lacks equivalent infrastructure. No synthesis-focused
 *   journals, funding lines, or career pathways exist. Independent
 *   synthesizers with domain literacy and LLM tools can produce mechanistic
 *   hypotheses connecting validated components, but these lack institutional
 *   validation pathways and are dismissed as speculation. The constraint is
 *   claimed as tangled_rope: genuine coordination function (filtering false
 *   positives through institutional validation) coupled with asymmetric
 *   extraction (systematic exclusion of synthesis work, opportunity cost
 *   borne by patients needing systems interventions).
 *
 * KEY AGENTS:
 *   - institutional_specialists: Primary beneficiaries (institutional/mobile) — domain authority protected by absence of synthesis infrastructure
 *   - academic_journals: Agenda setters (institutional/arbitrage) — enforce methodological orthodoxy, no synthesis validation pathway
 *   - funding_agencies: Agenda setters (institutional/mobile) — allocate resources through domain-specialist peer review
 *   - patients_needing_systems_interventions: Primary victims (powerless/trapped) — bear opportunity cost of missing synthesis layer
 *   - independent_synthesizers: Victims (moderate/constrained) — produce synthesis work without validation pathway or career support
 *   - epistemology_researchers: Observers (analytical/analytical) — document the structural gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(synthesis_infrastructure_gap, 0.68).
domain_priors:suppression_score(synthesis_infrastructure_gap, 0.72).
domain_priors:theater_ratio(synthesis_infrastructure_gap, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(synthesis_infrastructure_gap, extractiveness, 0.68).
narrative_ontology:constraint_metric(synthesis_infrastructure_gap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(synthesis_infrastructure_gap, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(synthesis_infrastructure_gap, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(synthesis_infrastructure_gap, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(synthesis_infrastructure_gap, tangled_rope).
narrative_ontology:human_readable(synthesis_infrastructure_gap, "Biomedical Knowledge Synthesis Infrastructure Gap").
narrative_ontology:topic_domain(synthesis_infrastructure_gap, "epistemology/institutional/health").

domain_priors:requires_active_enforcement(synthesis_infrastructure_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(synthesis_infrastructure_gap, '9a4f887c-1eaf-4f07-93e6-5eb017ffdebf').
narrative_ontology:cs_kernel_codification('9a4f887c-1eaf-4f07-93e6-5eb017ffdebf', distributed).
narrative_ontology:cs_authority_grounding('9a4f887c-1eaf-4f07-93e6-5eb017ffdebf', expertise).
narrative_ontology:cs_interpretation_layer_present('9a4f887c-1eaf-4f07-93e6-5eb017ffdebf').
narrative_ontology:cs_reading_relation('9a4f887c-1eaf-4f07-93e6-5eb017ffdebf', synthesis_infrastructure_gap__synthesis_hypothesis_reading, forecloses).
narrative_ontology:cs_reading_relation('9a4f887c-1eaf-4f07-93e6-5eb017ffdebf', synthesis_infrastructure_gap__pragmatic_action_reading, coexists_with).
narrative_ontology:cs_axiom('9a4f887c-1eaf-4f07-93e6-5eb017ffdebf', foundational, institutional_validation_required).
narrative_ontology:cs_axiom_status(institutional_validation_required, holdable).
narrative_ontology:cs_axiom_grounding('9a4f887c-1eaf-4f07-93e6-5eb017ffdebf', institutional_validation_required, conventional).
narrative_ontology:cs_axiom('9a4f887c-1eaf-4f07-93e6-5eb017ffdebf', foundational, reductionist_methodology_primacy).
narrative_ontology:cs_axiom_status(reductionist_methodology_primacy, holdable).
narrative_ontology:cs_axiom_grounding('9a4f887c-1eaf-4f07-93e6-5eb017ffdebf', reductionist_methodology_primacy, empirically_contingent).
narrative_ontology:cs_axiom('9a4f887c-1eaf-4f07-93e6-5eb017ffdebf', secondary, synthesis_equals_speculation).
narrative_ontology:cs_axiom_status(synthesis_equals_speculation, holdable).
narrative_ontology:cs_axiom_grounding('9a4f887c-1eaf-4f07-93e6-5eb017ffdebf', synthesis_equals_speculation, conventional).
narrative_ontology:cs_reference_frame('9a4f887c-1eaf-4f07-93e6-5eb017ffdebf', methodological_conservatism_framework).
narrative_ontology:cs_drift_state('9a4f887c-1eaf-4f07-93e6-5eb017ffdebf', post_llm_synthesis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a4f887c-1eaf-4f07-93e6-5eb017ffdebf', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(synthesis_infrastructure_gap, institutional_specialists).
narrative_ontology:constraint_beneficiary(synthesis_infrastructure_gap, academic_journals).
narrative_ontology:constraint_beneficiary(synthesis_infrastructure_gap, funding_agencies).
narrative_ontology:constraint_victim(synthesis_infrastructure_gap, patients_needing_systems_interventions).
narrative_ontology:constraint_victim(synthesis_infrastructure_gap, independent_synthesizers).
narrative_ontology:constraint_victim(synthesis_infrastructure_gap, cross_domain_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(synthesis_infrastructure_gap, clinical_translators).
narrative_ontology:constraint_victim(synthesis_infrastructure_gap, clinical_translators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate within established single-domain research programs with stable funding, peer review networks, and career advancement pathways. Their expertise is validated through institutional mechanisms: grants, publications in high-impact journals, academic appointments. The absence of synthesis infrastructure protects their domain authority from cross-domain challenges and maintains the primacy of reductionist single-variable studies as the gold standard.
narrative_ontology:constraint_stakeholder(synthesis_infrastructure_gap, institutional_specialists, beneficiary,
    institutional, generational, mobile, global).

% Set publication standards that privilege single-domain experimental work over cross-domain synthesis. They enforce methodological orthodoxy through peer review, requiring RCT-level evidence for knowledge claims while providing no equivalent validation pathway for mechanistic synthesis. Revenue models depend on institutional subscriptions funded by the same research infrastructure that produces single-domain work.
narrative_ontology:constraint_stakeholder(synthesis_infrastructure_gap, academic_journals, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(synthesis_infrastructure_gap, academic_journals, beneficiary).

% Allocate research funding through peer review panels composed of domain specialists. Grant structures reward hypothesis-driven single-variable experiments with clear methodological protocols. No funding lines exist for professional synthesis work; synthesis appears only as unfunded literature review or as justification for experimental proposals. They coordinate legitimate research activity but the coordination mechanism systematically excludes synthesis infrastructure.
narrative_ontology:constraint_stakeholder(synthesis_infrastructure_gap, funding_agencies, agenda_setter,
    institutional, generational, mobile, national).

% Face complex multi-system health conditions requiring integrated interventions across domains—metabolic, immunological, neurological, environmental. Institutional medicine offers single-variable treatments validated in isolation but lacks frameworks for systems-level intervention design. They bear the opportunity cost of the missing synthesis layer: delayed or absent treatment for conditions that fall between disciplinary boundaries. Exit options are constrained by information asymmetry and the absence of alternative validation pathways they can trust.
narrative_ontology:constraint_stakeholder(synthesis_infrastructure_gap, patients_needing_systems_interventions, payer,
    powerless, immediate, trapped, local).

% Possess domain literacy across multiple fields and access to LLM tools enabling rapid literature synthesis and mechanistic hypothesis generation. They produce cross-domain frameworks connecting validated components but lack institutional validation pathways: no synthesis journals, no synthesis-focused grants, no academic positions for professional synthesizers. Their work is dismissed as speculation regardless of mechanistic rigor because it lacks the institutional imprimatur. Career costs are high—synthesis work does not count toward tenure, grant applications, or professional advancement.
narrative_ontology:constraint_stakeholder(synthesis_infrastructure_gap, independent_synthesizers, payer,
    moderate, biographical, constrained, global).

% Academic researchers attempting to bridge domains face systematic barriers: grant panels lack expertise to evaluate cross-domain proposals, journals reject synthesis work as insufficiently novel or methodologically hybrid, tenure committees discount interdisciplinary publications. They pay the coordination cost of operating across institutional boundaries without the coordination benefit of shared infrastructure. Many retreat to single-domain work to preserve careers.
narrative_ontology:constraint_stakeholder(synthesis_infrastructure_gap, cross_domain_researchers, payer,
    moderate, biographical, constrained, global).

% Physician-scientists who translate single-domain research into clinical interventions. They benefit from clear validation pathways for single-variable treatments but encounter the synthesis gap when patients present with multi-system conditions. The infrastructure supports their primary function while leaving systems-level integration as an unfunded, unvalidated burden they must shoulder individually.
narrative_ontology:constraint_stakeholder(synthesis_infrastructure_gap, clinical_translators, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(synthesis_infrastructure_gap, clinical_translators, payer).

% Study knowledge production systems and institutional epistemology. They document the synthesis gap as a structural feature of contemporary science: validated components at the base layer, clinical translation at the application layer, but no institutional infrastructure for the synthesis layer between. They analyze how methodological conservatism in validation creates systematic blind spots for emergent multi-system phenomena.
narrative_ontology:constraint_stakeholder(synthesis_infrastructure_gap, epistemology_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(synthesis_infrastructure_gap, institutional_specialists).
narrative_ontology:fixing_cost_class(synthesis_infrastructure_gap, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Institutional science coordinates collective knowledge production through peer review, replication requirements, and methodological standards. This prevents individual error, filters false positives, and builds cumulative validated knowledge within domains.
% TRANSFER_FUNCTION: Moves research funding, publication opportunities, and career advancement from synthesis-oriented work to single-domain experimental programs. Institutional specialists and journals collect the resources; patients needing systems interventions and independent synthesizers bear the opportunity cost of the missing synthesis layer.
% ABSENT_VOICES: Patients with complex multi-system conditions who need integrated interventions are structurally excluded from research priority-setting. Independent synthesizers with domain literacy but no institutional credentials are excluded from validation pathways. Both groups would argue for synthesis infrastructure if present in the conversation.
% DISAPPEARANCE_RATIONALE: If the infrastructure gap vanished overnight—synthesis journals launched, synthesis-focused funding lines opened, academic positions for professional synthesizers created—research priorities would shift toward cross-domain integration, independent synthesizers would gain validation pathways, and systems-level intervention frameworks would proliferate. The biomedical knowledge production system would reorganize around a three-layer stack instead of the current two-layer structure.
% FOUNDING_PROBLEM: Early 20th century biomedicine faced rampant speculation, irreproducible findings, and individual bias masquerading as knowledge. Institutional validation mechanisms—peer review, experimental replication, statistical rigor—were built to filter false claims and establish reliable knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Institutional scientists attest the founding problem remains live and the validation mechanisms must be preserved. Epistemology researchers and independent synthesizers attest the founding problem is substantially solved for single-domain claims but the solution has ossified into a barrier against synthesis work—the infrastructure now excludes a category of legitimate knowledge (mechanistic synthesis of validated components) that did not exist when the validation mechanisms were designed. Historical analysis from philosophy of science and multiple legislative hearings on research funding priorities support the shifted-function reading.
narrative_ontology:disappearance_verdict(synthesis_infrastructure_gap, world_rearranges).
narrative_ontology:founding_problem_status(synthesis_infrastructure_gap, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(synthesis_infrastructure_gap, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-18',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(synthesis_infrastructure_gap, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(synthesis_infrastructure_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(synthesis_infrastructure_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(synthesis_infrastructure_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68) because the infrastructure gap is not a passive absence—it is actively maintained through peer review standards, funding criteria, and publication requirements that systematically exclude synthesis work. The coordination function is real (institutional validation prevents false positives) but the same mechanisms extract from synthesis-oriented researchers and patients by denying validation pathways to a legitimate category of knowledge. Suppression is high (0.72) because alternatives are actively blocked: synthesis journals face credibility barriers, synthesis-focused grants are rejected as methodologically unsound, academic positions for synthesizers do not exist. Theater ratio is moderate (0.42) and rising: increasing proportion of peer review and methodological debate serves to defend disciplinary boundaries rather than improve knowledge quality. Accessibility collapse is moderate (0.48) because the synthesis gap is not inevitable—other knowledge production systems have synthesis infrastructure—but resistance is substantial (0.61) as independent synthesizers and cross-domain researchers push against the barriers.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional specialist seat, the constraint operates as legitimate methodological conservatism protecting against false positives—the absence of synthesis infrastructure is a feature, not a bug. From the patient seat, the same structure operates as enforced deprivation: the knowledge they need (systems-level intervention frameworks) is systematically excluded from production. From the independent synthesizer seat, it operates as career suppression: their work is dismissed regardless of mechanistic rigor because it lacks institutional validation. The engine computes these divergent classifications from the structural data; the claimed type does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional specialists are structural beneficiaries: the absence of synthesis infrastructure protects their domain authority and maintains single-domain work as the gold standard. Their directionality is near the beneficiary end. Patients needing systems interventions are full targets: they bear the opportunity cost with no ability to exit (information asymmetry, lack of alternative validation pathways). Independent synthesizers and cross-domain researchers are targets with constrained exit: they can retreat to single-domain work to preserve careers, but this exit option requires abandoning their synthesis function. Academic journals and funding agencies are agenda setters who benefit from the coordination function while administering the extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy characteristics: the founding problem (filtering false positives in early biomedicine) is substantially solved for single-domain claims, but the solution has ossified into a barrier against synthesis work. The infrastructure now excludes a category of legitimate knowledge that did not exist when validation mechanisms were designed. However, this is not pure mandatrophy—the coordination function remains live for single-domain work. The tangled_rope classification captures this: genuine coordination coupled with asymmetric extraction, where the extraction component represents the mandatrophy element (outdated barrier) and the coordination component represents the still-functional validation mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthesis_validation_pathway,
    'Is institutional validation of cross-domain synthesis structurally impossible, or merely absent due to historical path dependence?',
    'Natural experiment from jurisdictions or institutions that create synthesis-focused infrastructure: if synthesis work can be validated through peer review by cross-domain panels, mechanistic coherence standards, and replication of derived predictions, then validation pathways are feasible and the current gap is contingent rather than necessary.',
    'If synthesis validation is feasible, the infrastructure gap is pure extraction (opportunity cost imposed on patients and synthesizers to protect domain authority). If structurally impossible, part of the measured extraction represents irreducible coordination cost (the price of preventing false positives in a domain where synthesis cannot be validated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthesis_validation_pathway, empirical, 'Whether synthesis validation pathways are structurally feasible or necessarily absent').

omega_variable(
    llm_synthesis_reliability,
    'Do LLM-enabled synthesis tools produce mechanistic hypotheses at sufficient reliability to warrant institutional validation pathways, or do they generate coherent but false narratives at rates that justify exclusion?',
    'Systematic evaluation of LLM-generated synthesis hypotheses against subsequent experimental validation: track prediction accuracy, false positive rates, and mechanistic coherence compared to human-only synthesis and to institutional single-domain predictions.',
    'If LLM synthesis reliability is high, the infrastructure gap represents suppression of a legitimate knowledge production method. If reliability is low, the gap represents appropriate methodological conservatism. The answer determines whether independent synthesizers are victims of extraction or appropriately excluded non-experts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(llm_synthesis_reliability, empirical, 'Whether LLM-enabled synthesis meets reliability thresholds for institutional validation').

omega_variable(
    coordination_extraction_separability,
    'Can the coordination function (filtering false positives through institutional validation) be separated from the extraction function (excluding synthesis work), or are they structurally coupled?',
    'Design institutional validation mechanisms specifically for synthesis work: cross-domain peer review panels, mechanistic coherence standards, prediction-tracking systems. If these can filter false positives in synthesis while admitting legitimate synthesis work, the functions are separable.',
    'If separable, the extraction component is pure rent-seeking by domain specialists protecting authority. If inseparable, the measured extraction includes irreducible coordination cost—the price of maintaining validation standards in a domain where synthesis and speculation are hard to distinguish.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction functions are structurally separable').

omega_variable(
    patient_opportunity_cost,
    'What is the actual health burden borne by patients due to the absence of systems-level intervention frameworks—how many conditions fall between disciplinary boundaries and remain untreated or undertreated?',
    'Epidemiological analysis of multi-system conditions, treatment gaps, and outcomes. Compare health trajectories in populations with access to integrative medicine frameworks (which attempt synthesis outside institutional validation) versus populations relying solely on single-domain institutional medicine.',
    'High opportunity cost would establish patients as clear victims and strengthen the case for synthesis infrastructure as a public health priority. Low opportunity cost would suggest the synthesis gap is primarily an academic concern with limited real-world harm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(patient_opportunity_cost, empirical, 'Magnitude of health burden from missing synthesis layer').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(synthesis_infrastructure_gap, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(synt_tr_t0, synthesis_infrastructure_gap, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(synt_tr_t0, observed).
narrative_ontology:measurement(synt_tr_t8, synthesis_infrastructure_gap, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(synt_tr_t8, observed).
narrative_ontology:measurement(synt_tr_t16, synthesis_infrastructure_gap, theater_ratio, 16, 0.35).
narrative_ontology:measurement_basis(synt_tr_t16, observed).
narrative_ontology:measurement(synt_tr_t24, synthesis_infrastructure_gap, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(synt_tr_t24, observed).
narrative_ontology:measurement(synt_tr_t32, synthesis_infrastructure_gap, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(synt_tr_t32, observed).
narrative_ontology:measurement(synt_tr_t40, synthesis_infrastructure_gap, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(synt_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(synt_be_t0, synthesis_infrastructure_gap, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(synt_be_t0, observed).
narrative_ontology:measurement(synt_be_t8, synthesis_infrastructure_gap, base_extractiveness, 8, 0.56).
narrative_ontology:measurement_basis(synt_be_t8, observed).
narrative_ontology:measurement(synt_be_t16, synthesis_infrastructure_gap, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(synt_be_t16, observed).
narrative_ontology:measurement(synt_be_t24, synthesis_infrastructure_gap, base_extractiveness, 24, 0.64).
narrative_ontology:measurement_basis(synt_be_t24, observed).
narrative_ontology:measurement(synt_be_t32, synthesis_infrastructure_gap, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(synt_be_t32, observed).
narrative_ontology:measurement(synt_be_t40, synthesis_infrastructure_gap, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(synt_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(synt_su_t0, synthesis_infrastructure_gap, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(synt_su_t0, observed).
narrative_ontology:measurement(synt_su_t8, synthesis_infrastructure_gap, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(synt_su_t8, observed).
narrative_ontology:measurement(synt_su_t16, synthesis_infrastructure_gap, suppression_requirement, 16, 0.66).
narrative_ontology:measurement_basis(synt_su_t16, observed).
narrative_ontology:measurement(synt_su_t24, synthesis_infrastructure_gap, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(synt_su_t24, observed).
narrative_ontology:measurement(synt_su_t32, synthesis_infrastructure_gap, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(synt_su_t32, observed).
narrative_ontology:measurement(synt_su_t40, synthesis_infrastructure_gap, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(synt_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(synthesis_infrastructure_gap, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(synthesis_infrastructure_gap, 0.12).

% DUAL FORMULATION NOTE:
% This constraint is one reading (institutional_validation_reading) of the knowledge_legitimacy_biomedicine kernel. Sibling readings (synthesis_hypothesis_reading, pragmatic_action_reading) will be authored as separate constraint stories with different beneficiary/victim structures and different ε values, linked via network.affects_constraints. The institutional reading forecloses the synthesis reading within academic biomedicine but coexists with it in independent research communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(synthesis_infrastructure_gap, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
