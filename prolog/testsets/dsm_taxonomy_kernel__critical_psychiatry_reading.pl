% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__critical_psychiatry_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: dsm_taxonomy_kernel__critical_psychiatry_reading
 *   human_readable: DSM Taxonomy as Pharmaceutical Market Construction (Critical Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy
 *
 * SUMMARY:
 *   The Diagnostic and Statistical Manual (DSM) of Mental Disorders is
 *   presented as an objective nosology mapping psychiatric symptoms to
 *   underlying disease categories. Under the critical psychiatry reading, the
 *   DSM is instead reverse-engineered from available pharmaceutical
 *   treatments: diagnostic categories are designed and refined to create
 *   markets for drugs that manufacturers have already developed or anticipate
 *   developing. Psychiatrists with financial ties to pharmaceutical companies
 *   exercise disproportionate influence over DSM revision, advocating for
 *   broad diagnostic criteria that expand the eligible patient population.
 *   The result is systematic overprescription, patient exposure to adverse
 *   effects, and pharmaceutical profit extraction disguised as medical
 *   necessity. This is one reading of the contested DSM taxonomy kernel; the
 *   biomedical reading treats DSM categories as discoveries of objective
 *   disease entities, and the neurodiversity reading treats them as
 *   pathologization of natural neurological variation. This story
 *   instantiates the critical reading's epistemological stance: DSM
 *   categories are constructed through market logic, not empirical discovery.
 *
 * KEY AGENTS:
 *   - pharmaceutical_manufacturers: institutional beneficiary, arbitrage exit — can shift between therapeutic categories; directly profits from DSM-expanded diagnostic categories
 *   - psychiatrists_with_industry_ties: institutional beneficiary and agenda-setter, constrained exit — careers built on pharmaceutical relationships and DSM authority; vote on DSM task forces and influence diagnostic criteria
 *   - patients_subjected_to_overprescription: powerless payers, identity-locked exit — receive diagnoses that conflate variation with disease; prescribed medications they may not need; cannot reject diagnosis without losing mental health system access
 *   - patients_experiencing_adverse_drug_effects: powerless payers, identity-locked exit — bear medical and social costs of psychotropic medication harms; remain on drugs that harm them because stopping requires psychiatric authorization and risks hospitalization
 *   - independent_psychiatrists: moderate power, constrained exit — practice within DSM framework not by choice but because insurance and regulatory systems require DSM coding; face reimbursement denial if they deviate
 *   - critical_psychiatry_scholars: moderate power, mobile exit — document pharmaceutical influence on DSM through research; authority rests on independent scholarship, not institutional position
 *   - insurance_systems: institutional agenda-setter, trapped exit — use DSM as administrative and billing infrastructure; cannot deviate without regulatory and market consequences; become complicit in maintaining the system
 *   - regulatory_authorities: institutional observer, analytical exit — approve psychiatric medications for DSM-defined indications; capacity to question but limited willingness due to institutional inertia and capture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.72).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Taxonomy as Pharmaceutical Market Construction (Critical Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical_epistemology/psychiatric_taxonomy").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, '6b512faf-f8ab-49e6-9db1-c6af45638787').
narrative_ontology:cs_kernel_codification('6b512faf-f8ab-49e6-9db1-c6af45638787', fixed_text).
narrative_ontology:cs_authority_grounding('6b512faf-f8ab-49e6-9db1-c6af45638787', extraction).
narrative_ontology:cs_interpretation_layer_present('6b512faf-f8ab-49e6-9db1-c6af45638787').
narrative_ontology:cs_reading_relation('6b512faf-f8ab-49e6-9db1-c6af45638787', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('6b512faf-f8ab-49e6-9db1-c6af45638787', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('6b512faf-f8ab-49e6-9db1-c6af45638787', foundational, dsm_categories_reverse_engineered_from_drug_availability).
narrative_ontology:cs_axiom_status(dsm_categories_reverse_engineered_from_drug_availability, holdable).
narrative_ontology:cs_axiom_grounding('6b512faf-f8ab-49e6-9db1-c6af45638787', dsm_categories_reverse_engineered_from_drug_availability, empirically_contingent).
narrative_ontology:cs_axiom('6b512faf-f8ab-49e6-9db1-c6af45638787', foundational, pharmaceutical_profit_primary_dsm_maintenance_driver).
narrative_ontology:cs_axiom_status(pharmaceutical_profit_primary_dsm_maintenance_driver, holdable).
narrative_ontology:cs_axiom_grounding('6b512faf-f8ab-49e6-9db1-c6af45638787', pharmaceutical_profit_primary_dsm_maintenance_driver, empirically_contingent).
narrative_ontology:cs_reference_frame('6b512faf-f8ab-49e6-9db1-c6af45638787', dsm_as_objective_disease_taxonomy).
narrative_ontology:cs_drift_state('6b512faf-f8ab-49e6-9db1-c6af45638787', contemporary_post_critical_psychiatry_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6b512faf-f8ab-49e6-9db1-c6af45638787', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_with_industry_ties).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_subjected_to_overprescription).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_experiencing_adverse_drug_effects).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__critical_psychiatry_reading, 'none', 1).

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
 *   Extractiveness climbs from 0.45 to 0.68 over the interval, reflecting the increasing sophistication and scope of pharmaceutical influence on DSM revision. The jump from DSM-IV (1994) to DSM-5 (2013) saw expanded diagnostic categories for attention, mood, and anxiety disorders — categories with large eligible populations and profitable drug markets. Theater ratio rises from 0.35 to 0.58, indicating that an increasing share of the constraint's maintenance activity is performative: the American Psychiatric Association publishes research purporting to validate diagnostic categories, but critical analysis reveals the research is overwhelmingly industry-funded and designed to reach predetermined conclusions. Suppression rises from 0.55 to 0.72 as active enforcement machinery develops: confidentiality enforcement around pharmaceutical payment data, exclusion of critical voices from DSM task forces, reputational attacks on scholars who document industry influence, and institutional pressure on psychiatrists to remain silent about conflicts of interest. Accessibility collapse is moderate (0.48) because alternatives remain available: some clinicians use dimensional or descriptive approaches, peer support communities operate outside the DSM framework, and critical scholarship is published in peer-reviewed outlets. Resistance is substantial (0.62) because the constraint faces organized opposition: critical psychiatry movements, patient advocacy against overprescription, physician organizations promoting careful psychotropic use, and regulatory investigations into pharmaceutical marketing practices. The constraint persists despite this resistance because the beneficiary institutional power is very high (pharmaceutical manufacturers control $100+ billion in annual psychiatric medication revenue globally).
 *
 * PERSPECTIVAL GAP:
 *   The pharmaceutical manufacturers and industry-funded psychiatrists experience the DSM as a legitimate scientific achievement that they steward and improve. They see broader diagnostic criteria as progress — more people identified with treatable conditions. They experience their involvement as scientific leadership and financial success — incentives aligned. From the patients' perspective, the same system is experienced as a mechanism that medicalizes normal variation, extracts wealth, and distributes harm in the form of adverse drug effects, dependency, and lost autonomy. Independent psychiatrists experience the constraint as structural compulsion: they must use DSM coding because insurance systems require it, even if they doubt the diagnostic validity. The critical scholars experience the constraint as a captured system requiring systematic documentation and institutional pressure to reform. The engine computes these different perceived types from the structural data: the high-power institutional beneficiary sees rope (coordination with profit alignment), the powerless victims see snare (extraction with suppression), the moderate-power independent practitioners see tangled rope (coordination function with asymmetric extraction), and the observers see the structure as contestable. The authored claim (tangled_rope) reflects the critical reading's position: the DSM does coordinate psychiatric communication, but the coordination is weaponized to extract profit and medicalize human variation.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical manufacturers are the clearest beneficiaries: they directly profit from expanded diagnostic categories, and their exit is arbitrage (they can shift capital between drug classes if one becomes politically untenable). Their directionality d is near the beneficiary end (~0.1-0.2). Psychiatrists with industry ties are partially beneficiary (financial incentives), partially agenda-setter (DSM authority). Their d is moderate beneficiary (~0.3-0.4) because they do have constrained exit — abandoning the pharmaceutical relationship costs them career capital. Independent psychiatrists have no direct beneficiary incentive but are payers of a sort: they carry the burden of maintaining a system they may not believe in, and their exit is constrained by insurance/regulatory requirements. Their d is near target (~0.7-0.8). Patients are clear targets: they bear the costs of overprescription and adverse effects, and their exit is severely constrained or identity-locked (the diagnosis becomes part of their self-concept; leaving psychiatry means losing access to the entire mental health system). Their d is near full target (~0.85-0.95). Insurance systems are partially beneficiary (they off-load mental health decision-making onto the DSM standard) and partially payer (they finance the overtreatment). Their d is symmetric (~0.5). Critical scholars have no direct stake but can see the structure clearly; they are observers with mobile exit and analytical power. The directionality overrides are not needed because the structural derivation from beneficiary/victim + exit captures the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (lack of diagnostic standardization in mid-20th-century psychiatry) was real and has been solved: modern psychiatry does have a shared DSM, research can identify populations with consistent criteria, and communication across jurisdictions is enabled. However, the constraint's current function has drifted radically from solving that problem. The DSM revision process is now dominated by pharmaceutical influence (documented through funding flows, task force member financial disclosures, and industry-guided research agendas). The constraint persists not because it solves the original coordination problem but because the institutional beneficiaries (pharmaceutical manufacturers, captured psychiatrists) benefit from its maintenance. This is mandatrophy: the founding problem is dead, but the constraint persists through institutional inertia and active defense. The theater ratio (0.58) is the diagnostic signal: more than half of the activity maintaining the DSM is theatrical (performative research, reputation management, institutional theater) rather than genuine coordination. The divergence between founding_problem_status=dead and disappearance_verdict=world_rearranges indicates a zombie constraint: removing it would cause significant institutional disruption because the system has reorganized around it, but the original justification is no longer operative. Classification: tangled_rope where the coordination function has atrophied, the extraction function is primary, and active enforcement (suppression of critical voices, exclusion of non-industry perspectives from DSM revision) is required to maintain the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pharmaceutical_influence_quantification,
    'What is the precise extent of pharmaceutical financial influence on DSM-5 revision task force members compared to independent psychiatrists, and how does this influence translate into diagnostic category decisions?',
    'Full disclosure and independent audit of all financial relationships between pharmaceutical manufacturers and DSM task force members; analysis of voting patterns on specific diagnostic criteria by conflict-of-interest status; comparison of task force members'' published research findings before and after receiving industry funding.',
    'If the financial influence is as extensive as critical scholars document (widespread consulting fees, research funding, speaking contracts), the constraint''s classification shifts firmly toward snare with pharmaceutical-captured beneficiaries; if influence is limited and other factors dominate criterion-setting, the classification moves toward rope or tangled rope with genuine coordination. The difference determines whether the system is best understood as corruption of a legitimate process or as a system designed to serve profit from inception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_influence_quantification, empirical, 'Quantifying the extent and impact of pharmaceutical industry financial influence on DSM revision decisions.').

omega_variable(
    diagnostic_validity_vs_clinical_utility,
    'Are DSM diagnostic categories valid as disease entities (do they map to neurobiological reality), or are they best understood as administratively useful categories for organizing treatment regardless of validity?',
    'Neuroscientific research seeking biomarkers for DSM diagnoses; systematic review of the empirical evidence for neurobiological boundaries between DSM categories; comparison of DSM categories with neuroscience-driven alternative taxonomies (RDoC, dimensional approaches); analysis of treatment response patterns to test whether DSM categories predict medication response better than alternative frameworks.',
    'If DSM categories are valid disease entities (biomarkers discovered, clear neurobiological boundaries), the constraint is better classified as mountain or rope — the categories reflect objective reality and benefit all users. If DSM categories lack validity and function primarily as administrative conveniences, the constraint is snare or tangled rope — they persist because institutions benefit from them, not because they are true. This is the fundamental empirical question underlying the reading contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diagnostic_validity_vs_clinical_utility, empirical, 'Whether DSM categories represent valid disease entities or administratively useful but empirically unfounded groupings.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) primarily structural (external barriers: insurance systems requiring DSM coding, regulatory frameworks enforcing diagnostic standards, institutional pressure to conform) or internalized (patients and psychiatrists believe the DSM is scientifically valid and internalize the discipline even without external enforcement)?',
    'Post-DSM-reform trajectory analysis: if DSM authority were explicitly challenged and stripped of mandatory application, would resistance persist (internalized belief) or would practice rapidly diversify (structural suppression)? Ethnographic analysis of how psychiatrists and patients describe their relationship to the DSM — do they experience it as optional knowledge or as binding constraint? Comparison of psychiatric practice in jurisdictions that officially recognize alternative diagnostic frameworks (ICD-11) — does practice diverge from DSM?',
    'If suppression is primarily structural (external enforcement required to maintain DSM dominance), the constraint is vulnerable to institutional reform: changing insurance systems, weakening regulatory enforcement of DSM coding, or promoting alternative frameworks would loosen it. If internalized (practitioners and patients believe the DSM is valid), the constraint is more resilient — reform requires first changing belief systems, a slower process. Classification does not change based on this, but it informs strategy for resistance and reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether DSM suppression operates through external institutional enforcement or through internalized belief in the system''s validity.').

omega_variable(
    alternative_nosology_viability,
    'Are there documented alternatives to the DSM that organize psychiatric symptomatology with greater empirical validity and less pharmaceutical market alignment (e.g., dimensional approaches, peer-developed frameworks, ICD-11''s less drug-aligned categories)?',
    'Systematic comparison of DSM, ICD-11, dimensional models (RDoC, HiTOP), and peer-led diagnostic systems across validity criteria, treatment-prediction accuracy, and pharmaceutical profit alignment. Implementation studies in jurisdictions adopting alternative frameworks.',
    'If viable alternatives exist, the constraint''s persistence is due to institutional lock-in and beneficiary power, not functional superiority — this strengthens the critical reading''s claim that the constraint is maintained for profit rather than for coordination. If the DSM is functionally superior despite its flaws, the constraint''s persistence becomes more ambiguous — it may persist because it is genuinely better, not purely because it serves profit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_nosology_viability, empirical, 'Whether documented alternatives to the DSM exist with superior validity and less pharmaceutical market alignment.').

omega_variable(
    reading_contest_under_determination,
    'Is the contest between critical, biomedical, and neurodiversity readings a genuine empirical dispute that data could resolve, or a conceptual/normative disagreement where different value premises lead to incommensurable interpretations of the same facts?',
    'Careful mapping of what evidence would vindicate each reading: biomedical reading requires discovery of neurobiological disease entities; critical reading requires documentation of pharmaceutical market engineering; neurodiversity reading requires showing that DSM pathologizes natural variation. Test each reading against available evidence. If all three readings remain plausible after full evidence review, the dispute is conceptual/normative, not empirical.',
    'If empirical (data could settle it), the dispute is tractable through research and evidence review; the corpus can aggregate evidence toward a best reading. If conceptual (value premises diverge), no amount of data will convince all parties; the dispute will persist as a permanent fixture of psychiatric practice. This affects how we frame the mandate of the DSM and what ''reform'' means.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_under_determination, conceptual, 'Whether the reading contest is empirically resolvable or reflects incommensurable value premises.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dsm__tr_t5, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(dsm__tr_t10, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(dsm__tr_t15, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(dsm__tr_t20, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 20, 0.54).
narrative_ontology:measurement(dsm__tr_t25, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 25, 0.57).
narrative_ontology:measurement(dsm__tr_t30, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(dsm__tr_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dsm__be_t5, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(dsm__be_t10, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(dsm__be_t15, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(dsm__be_t20, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(dsm__be_t25, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(dsm__be_t30, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(dsm__be_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dsm__su_t5, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(dsm__su_t10, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(dsm__su_t15, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(dsm__su_t20, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(dsm__su_t25, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(dsm__su_t30, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(dsm__su_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.18).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_marketing_targeting_physicians).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatric_medication_adverse_effects_suppression).

% DUAL FORMULATION NOTE:
% The DSM taxonomy kernel is contested across three structural readings: biomedical (categories map to objective disease), critical (categories reverse-engineered from drug treatments), and neurodiversity (categories pathologize natural variation). These are not competing measurements of the same constraint — they are three distinct constraints instantiated by the same kernel text, each with its own ε, victim/beneficiary structure, and classification. The critical reading (this story) treats DSM categories as extracted value — profit from pharmaceutical sales, medicalization of human variation, suppression of alternative frameworks. The biomedical reading treats them as discovery. The neurodiversity reading treats them as coercive normalization. Each instantiates a different power structure and ethical frame. The three stories are linked through network.affects_constraints and share a kernel_id but have distinct constraint_ids, ε values, and victim/beneficiary sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
