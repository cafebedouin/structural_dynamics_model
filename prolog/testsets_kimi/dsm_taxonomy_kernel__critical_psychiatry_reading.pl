% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__critical_psychiatry_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__critical_psychiatry_reading, []).

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
 *   constraint_id: dsm_taxonomy_kernel__critical_psychiatry_reading
 *   human_readable: DSM Taxonomy as Pharmaceutical Market Construction (Critical Psychiatry Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy
 *
 * SUMMARY:
 *   This constraint story captures the critical psychiatry reading of the DSM
 *   taxonomy kernel: the Diagnostic and Statistical Manual of Mental
 *   Disorders functions not as a neutral map of pre-existing disease
 *   entities, but as a reverse-engineered classification system whose
 *   categories expand and shift to accommodate available pharmaceutical
 *   treatments and protect market share. Key agents include the
 *   pharmaceutical industry and industry-funded psychiatrists
 *   (beneficiaries), patients and public health systems (victims/payers), the
 *   APA task force (agenda-setter), and excluded neurodiversity advocates.
 *
 * KEY AGENTS:
 *   - pharmaceutical_industry: Primary beneficiary (institutional/global) â collects revenue from expanded drug markets.
 *   - psychiatrists_industry_funded: Secondary beneficiary (moderate/constrained) â receive funding and professional advancement within the DSM-pharma paradigm.
 *   - apa_dsm_task_force: Agenda-setter (institutional/constrained) â administers the taxonomy that structures reimbursement and regulation.
 *   - patients_overprescribed: Primary target (powerless/identity_locked) â bear adverse effects and diagnostic capture.
 *   - public_health_systems: Secondary target (institutional/constrained) â budgets captured by pharmaceutical costs tied to DSM codes.
 *   - critical_psychiatry_movement: Analytical observer (moderate/mobile) â documents capture and category inflation.
 *   - neurodiversity_advocates: Excluded voice (moderate/constrained) â reject pathologization but are absent from revision rooms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.72).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Taxonomy as Pharmaceutical Market Construction (Critical Psychiatry Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical_epistemology/psychiatric_taxonomy").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, '19d8d1de-1d43-4df4-a50e-b933682b8a1b').
narrative_ontology:cs_kernel_codification('19d8d1de-1d43-4df4-a50e-b933682b8a1b', fixed_text).
narrative_ontology:cs_authority_grounding('19d8d1de-1d43-4df4-a50e-b933682b8a1b', extraction).
narrative_ontology:cs_interpretation_layer_present('19d8d1de-1d43-4df4-a50e-b933682b8a1b').
narrative_ontology:cs_reading_relation('19d8d1de-1d43-4df4-a50e-b933682b8a1b', dsm_taxonomy_kernel__biomedical_reading, influences).
narrative_ontology:cs_reading_relation('19d8d1de-1d43-4df4-a50e-b933682b8a1b', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('19d8d1de-1d43-4df4-a50e-b933682b8a1b', foundational, diagnostic_categories_are_pharma_market_instruments).
narrative_ontology:cs_axiom_status(diagnostic_categories_are_pharma_market_instruments, holdable).
narrative_ontology:cs_axiom_grounding('19d8d1de-1d43-4df4-a50e-b933682b8a1b', diagnostic_categories_are_pharma_market_instruments, empirically_contingent).
narrative_ontology:cs_axiom('19d8d1de-1d43-4df4-a50e-b933682b8a1b', foundational, psychiatric_knowledge_is_structurally_captured_by_commercial_interests).
narrative_ontology:cs_axiom_status(psychiatric_knowledge_is_structurally_captured_by_commercial_interests, holdable).
narrative_ontology:cs_axiom_grounding('19d8d1de-1d43-4df4-a50e-b933682b8a1b', psychiatric_knowledge_is_structurally_captured_by_commercial_interests, empirically_contingent).
narrative_ontology:cs_reference_frame('19d8d1de-1d43-4df4-a50e-b933682b8a1b', descriptive_agnostic_taxonomy).
narrative_ontology:cs_drift_state('19d8d1de-1d43-4df4-a50e-b933682b8a1b', post_dsm5_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('19d8d1de-1d43-4df4-a50e-b933682b8a1b', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_industry_funded).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_overprescribed).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, public_health_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Profits from expanded diagnostic categories that create new addressable markets for psychotropic compounds; funds clinical trials, guideline authorship, and continuing medical education that normalize pharmaceutical intervention as the primary response to DSM-defined conditions.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Receive research grants, consulting fees, and speaking honoraria tied to pharmaceutical pipelines; professional advancement depends on producing DSM-aligned, pharma-complementary research and prescribing practices.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_industry_funded, beneficiary,
    moderate, biographical, constrained, national).

% Authors and revises the DSM manual; inclusion or exclusion of categories directly determines insurance reimbursement, regulatory approval endpoints, and prescriptive legitimacy; operates under industry-influenced expert selection and evidence bases.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, apa_dsm_task_force, agenda_setter,
    institutional, generational, constrained, national).

% Receive diagnoses that medicalize distress, difference, or social suffering; subjected to long-term pharmacological regimens with adverse effects; non-pharmacological alternatives are structurally unavailable due to reimbursement rules keyed to DSM codes; diagnostic labels fuse with self-concept, making exit psychologically costly.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_overprescribed, payer,
    powerless, biographical, identity_locked, local).

% Reimburse psychotropic prescriptions and DSM-coded interventions through formulary and insurance design; budgets captured by pharmaceutical costs; regulatory and accreditation requirements lock them into the categorical taxonomy even when cost-effectiveness or patient outcomes favor psychosocial models.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, public_health_systems, payer,
    institutional, generational, constrained, national).

% Documents diagnostic inflation correlating with patent cliffs and market expansion; publishes analyses of ghostwriting, guideline capture, and category proliferation; operates outside mainstream funding streams and faces professional marginalization.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, critical_psychiatry_movement, observer,
    moderate, generational, mobile, national).

% Would object to the pathologization of autism, ADHD, and other neurological differences as DSM disorders requiring normalization; excluded from DSM revision deliberations and from reimbursement pathways that privilege behavioral modification and pharmacological suppression over accommodation.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, neurodiversity_advocates, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_industry).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__critical_psychiatry_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized nosology enabling communication among clinicians, insurers, researchers, and regulators about mental distress and behavioral difference; creates a shared language for treatment authorization and epidemiological tracking.
% TRANSFER_FUNCTION: Moves capital from public health systems and patients to pharmaceutical manufacturers and industry-affiliated prescriber-investigators, via diagnostic categories that expand the reimbursable market for psychotropic drugs.
% ABSENT_VOICES: Patients who experienced severe adverse effects or non-response to psychotropes, neurodiversity advocates who reject the disorder framing of cognitive difference, and psychosocial intervention researchers lacking pharmaceutical sponsorship are underrepresented in DSM revision processes, post-market surveillance, and guideline authorship.
% DISAPPEARANCE_RATIONALE: If DSM categories vanished as the mandatory framework for reimbursement, pharmaceutical markets for many psychotropes would contract, insurance coding would require alternative systems, psychiatric training would reorient around non-categorical or dimensional models, and prescriptive authority would decouple from the manual.
% FOUNDING_PROBLEM: Mid-20th century psychiatry lacked reliable diagnostic criteria, producing arbitrary institutional commitments, inconsistent treatment selection, and inability to coordinate multi-site research or third-party reimbursement.
% FOUNDING_PROBLEM_CORROBORATION: The APA and NIMH attest the reliability problem persists and requires categorical refinement. Independent historians and sociologists of psychiatry (Kirk, Kutchins, Horwitz, Whitaker) attest the reliability problem was substantially addressed by DSM-III and the system has since been captured by commercial interests; their analyses originate outside pharmaceutical funding and corroborate a shifted-function reading.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__critical_psychiatry_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__critical_psychiatry_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__critical_psychiatry_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is moderate-high because the taxonomy decouples from independent biological validation and tracks pharmaceutical market cycles; suppression (0.68) reflects the active exclusion of psychosocial and neurodiversity framings from reimbursement and guideline authority. Theater ratio (0.45) captures the performative scientism of the manualâstatistical reliability rhetoric obscuring construct validity problems. Accessibility collapse (0.75) is high because once a clinician or system adopts the DSM framework, non-categorical alternatives (psychoanalytic, social, trauma-informed) become practically unreachable. Resistance (0.55) reflects sustained but institutionally marginalized critique.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (pharma, funded psychiatrists, APA task force) experience the constraint as necessary professional infrastructure, revenue, and epistemic order. The payer seats (patients, public health systems) experience the same structure as overprescription, budget capture, and diagnostic identity lock. The observer seat sees the structural capture that aligns category boundaries with market imperatives. The engine computes this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical industry and industry-funded psychiatrists are structural beneficiaries (low d): the constraint subsidizes their revenue and career pipelines. Patients overprescribed and public health systems are structural targets (high d): the constraint extracts from their bodies and budgets. The APA task force sits near the middleâits authority is amplified by the constraint, but its autonomy is captured by the industry ecosystem that funds the research underlying its decisions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâdiagnostic unreliabilityâwas substantially addressed by DSM-III (1980). Since then, the taxonomy has accumulated new functions: market expansion, patent protection, and professional boundary maintenance. The biomedical reading insists the mandate is still live (research domain criteria have not replaced DSM). The critical reading treats the persistence as mandatrophy: the coordination function (reliability) is real but has been subordinated to extraction. The T17 and mismatch consumers flag this as contested zombie status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diagnostic_inflation_market_correlation,
    'Do DSM category expansions (e.g., bipolar spectrum, disruptive mood dysregulation, pre-menstrual dysphoric disorder) correlate with pharmaceutical patent cliffs and market-expansion imperatives, or with independent biological discovery?',
    'Historical-sociological case studies of category invention timing relative to drug patent expirations and pipeline composition; analysis of trial funding sources for category-validating studies.',
    'A strong correlation would confirm the high extractiveness score and support reclassification toward snare; weak correlation would lower extraction and support the biomedical reading''s coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diagnostic_inflation_market_correlation, empirical, 'Whether diagnostic inflation is driven by market construction or biological discovery.').

omega_variable(
    alternative_model_suppression_mechanism,
    'Is the suppression of non-DSM psychiatric models structural (reimbursement capture, regulatory approval pathways, licensure requirements) or epistemic (paradigm inertia, funding concentration, disciplinary socialization)?',
    'Track availability and reimbursement of psychosocial interventions across jurisdictions with varying regulatory coupling to DSM; measure career penalties for researchers operating outside the categorical paradigm.',
    'If suppression is primarily structural, reform can be achieved through policy decoupling; if primarily epistemic, reform requires generational change in training and will register as higher inertial resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_model_suppression_mechanism, conceptual, 'Structural versus epistemic suppression of alternative psychiatric models.').

omega_variable(
    kernel_reading_position,
    'How does the classification of the DSM taxonomy change when read through the biomedical or neurodiversity framings instead of the critical psychiatry framing?',
    'Compare the compiled constraint stories across the three kernel readings; variance in epsilon, beneficiary/victim sets, and computed per-seat types measures framing sensitivity.',
    'High variance indicates the colloquial label ''DSM taxonomy'' conflates multiple structurally distinct constraints and should be treated as a constraint family; low variance suggests the readings are observer-relative disagreements about a single constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Framing sensitivity of the DSM taxonomy kernel across its three readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_crit_tr_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(dsm_crit_tr_t15, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(dsm_crit_tr_t30, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(dsm_crit_tr_t45, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 45, 0.42).
narrative_ontology:measurement(dsm_crit_tr_t60, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(dsm_crit_tr_t70, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 70, 0.45).

% Extraction over time
narrative_ontology:measurement(dsm_crit_be_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(dsm_crit_be_t15, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(dsm_crit_be_t30, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(dsm_crit_be_t45, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 45, 0.65).
narrative_ontology:measurement(dsm_crit_be_t60, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(dsm_crit_be_t70, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 70, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(dsm_crit_su_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(dsm_crit_su_t15, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(dsm_crit_su_t30, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(dsm_crit_su_t45, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 45, 0.68).
narrative_ontology:measurement(dsm_crit_su_t60, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(dsm_crit_su_t70, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 70, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, resource_allocation).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__neurodiversity_reading).

% DUAL FORMULATION NOTE:
% This critical psychiatry reading and its siblings (biomedical, neurodiversity) decompose the colloquial label 'DSM taxonomy' into three structurally distinct constraints with different epsilon values, victim/beneficiary structures, and directionality profiles. They form a constraint family linked by shared institutional domain and mutual contamination potential.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
