% ============================================================================
% CONSTRAINT STORY: mitochondrial_demand_signal_deficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mitochondrial_demand_signal_deficiency, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mitochondrial_demand_signal_deficiency
 *   human_readable: Mitochondrial Decline as Demand Signal Deficiency
 *   domain: biomedical/aging_biology/nutritional_biochemistry
 *
 * SUMMARY:
 *   Age-related mitochondrial decline is observable and well-documented: ATP
 *   synthesis capacity, electron transport chain activity, and membrane
 *   potential all decrease with age. The constraint at issue is the causal
 *   attribution: is this decline due to absence of maintenance demand signals
 *   (exercise, caloric stress, metabolic challenge) that are correctable, or
 *   due to accumulated structural damage (somatic mutations, membrane
 *   oxidation, proteostatic failure) that is largely irreversible? The
 *   demand-signal framework coordinates research and intervention development
 *   while also channeling resources to supplement markets and away from
 *   acceptance-based clinical approaches. The claim is tangled_rope (genuine
 *   coordination function plus asymmetric extraction); the metrics describe
 *   substantially extractive operation with rising theater ratio as the gap
 *   between intervention promise and clinical reality widens.
 *
 * KEY AGENTS:
 *   - supplement_industry: Primary beneficiary (organized/mobile) — collects revenue from mitochondrial support products marketed on reversibility premise
 *   - longevity_intervention_researchers: Agenda setter (institutional/constrained) — frames the research questions, secures funding, publishes intervention studies
 *   - patients_attributing_decline_to_inevitability: Primary victim (powerless/identity_locked) — internalize intervention failure as personal inadequacy rather than framework limitation
 *   - elderly_populations_purchasing_interventions: Secondary victim (moderate/constrained) — bear financial cost of interventions with uncertain efficacy
 *   - gerontology_structural_aging_researchers: Excluded (institutional/mobile) — study irreversible mechanisms, marginalized in intervention-focused venues
 *   - clinical_geriatricians: Observer (institutional/analytical) — see patient outcomes across both framings
 *   - health_insurance_systems: Institutional payer (institutional/constrained) — face pressure to cover interventions as preventive care
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mitochondrial_demand_signal_deficiency, 0.68).
domain_priors:suppression_score(mitochondrial_demand_signal_deficiency, 0.71).
domain_priors:theater_ratio(mitochondrial_demand_signal_deficiency, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mitochondrial_demand_signal_deficiency, extractiveness, 0.68).
narrative_ontology:constraint_metric(mitochondrial_demand_signal_deficiency, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(mitochondrial_demand_signal_deficiency, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mitochondrial_demand_signal_deficiency, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(mitochondrial_demand_signal_deficiency, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mitochondrial_demand_signal_deficiency, tangled_rope).
narrative_ontology:human_readable(mitochondrial_demand_signal_deficiency, "Mitochondrial Decline as Demand Signal Deficiency").
narrative_ontology:topic_domain(mitochondrial_demand_signal_deficiency, "biomedical/aging_biology/nutritional_biochemistry").

domain_priors:requires_active_enforcement(mitochondrial_demand_signal_deficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mitochondrial_demand_signal_deficiency, supplement_industry).
narrative_ontology:constraint_beneficiary(mitochondrial_demand_signal_deficiency, longevity_intervention_researchers).
narrative_ontology:constraint_victim(mitochondrial_demand_signal_deficiency, patients_attributing_decline_to_inevitability).
narrative_ontology:constraint_victim(mitochondrial_demand_signal_deficiency, elderly_populations_purchasing_interventions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(mitochondrial_demand_signal_deficiency, health_insurance_systems).
narrative_ontology:constraint_vindicates(mitochondrial_demand_signal_deficiency, use_it_or_lose_it_mitochondrial_doctrine).
narrative_ontology:constraint_vindicates(mitochondrial_demand_signal_deficiency, nutritional_intervention_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Markets mitochondrial support compounds (CoQ10, NAD+ precursors, PQQ, carnitine) on the premise that age-related mitochondrial decline is reversible through supplementation restoring demand signals. Revenue depends on framing decline as correctable deficiency rather than structural aging. Can pivot to other supplement categories if this framing loses credibility.
narrative_ontology:constraint_stakeholder(mitochondrial_demand_signal_deficiency, supplement_industry, beneficiary,
    organized, biographical, mobile, global).

% Conduct studies on mitochondrial interventions, publish on demand signal restoration mechanisms, secure grants premised on modifiable decline. Career advancement tied to demonstrating intervention efficacy. The demand-signal framing opens research funding streams that inevitable-degeneration framing would close. Exit to other aging mechanisms possible but costly in accumulated expertise and grant continuity.
narrative_ontology:constraint_stakeholder(mitochondrial_demand_signal_deficiency, longevity_intervention_researchers, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(mitochondrial_demand_signal_deficiency, longevity_intervention_researchers, beneficiary).

% Experience age-related fatigue and functional decline. Told by clinicians and media that mitochondrial decline is reversible via lifestyle and supplementation if they generate sufficient demand signals. Purchase interventions and modify behavior; when outcomes disappoint, internalize failure as insufficient adherence rather than questioning the reversibility premise. Identity as responsible aging individual fused with the intervention framework.
narrative_ontology:constraint_stakeholder(mitochondrial_demand_signal_deficiency, patients_attributing_decline_to_inevitability, payer,
    powerless, biographical, identity_locked, local).

% Spend substantial income on mitochondrial supplements and testing. Motivated by hope that decline is preventable. Face information asymmetry: cannot independently verify whether their mitochondrial parameters are demand-limited or structurally aged. Exit requires accepting decline as inevitable, which conflicts with cultural narratives of active aging.
narrative_ontology:constraint_stakeholder(mitochondrial_demand_signal_deficiency, elderly_populations_purchasing_interventions, payer,
    moderate, immediate, constrained, regional).

% Study mitochondrial aging as accumulation of somatic mutations, membrane damage, and proteostatic decline—processes not reversible by demand signals. Their framing competes for the same grant funding and clinical attention. Structurally excluded from intervention-focused conferences and journals that organize around modifiability.
narrative_ontology:constraint_stakeholder(mitochondrial_demand_signal_deficiency, gerontology_structural_aging_researchers, excluded,
    institutional, generational, mobile, global).

% Treat elderly patients presenting with fatigue and functional decline. Must navigate between offering hope via interventions and managing expectations about irreversible aging. Observe patient outcomes across both intervention and acceptance approaches. Can assess which framing better predicts clinical trajectory.
narrative_ontology:constraint_stakeholder(mitochondrial_demand_signal_deficiency, clinical_geriatricians, observer,
    institutional, biographical, analytical, national).

% Face pressure to cover mitochondrial testing and interventions as preventive care. The demand-signal framing medicalizes normal aging, expanding the treatment-eligible population. Must evaluate whether interventions produce cost-effective health gains or represent expenditure on unproven modifiability claims.
narrative_ontology:constraint_stakeholder(mitochondrial_demand_signal_deficiency, health_insurance_systems, payer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mitochondrial_demand_signal_deficiency, supplement_industry).
narrative_ontology:fixing_cost_class(mitochondrial_demand_signal_deficiency, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes research effort and clinical attention around a testable, intervention-accessible mechanism for age-related decline, creating a shared framework for studying mitochondrial aging that enables comparison across studies and compounds.
% TRANSFER_FUNCTION: Moves money from elderly individuals and health systems to supplement manufacturers and intervention researchers, and moves research focus from structural aging mechanisms to demand-signal restoration pathways.
% ABSENT_VOICES: Structural aging researchers who study irreversible mitochondrial damage are marginalized in intervention-focused funding and publishing venues. Patients who tried interventions without benefit and concluded decline was inevitable are absent from testimonial marketing and study recruitment.
% DISAPPEARANCE_RATIONALE: If the demand-signal framing disappeared, supplement marketing would shift to other mechanisms, research funding would redistribute toward structural aging studies, and clinical messaging would reframe age-related mitochondrial decline as largely inevitable, reducing intervention purchases and altering patient expectations.
% FOUNDING_PROBLEM: Early mitochondrial aging research found that exercise and caloric restriction could improve mitochondrial function in aged animals, suggesting decline was not purely structural. The demand-signal framework emerged to explain these findings and guide intervention development.
% FOUNDING_PROBLEM_CORROBORATION: The founding observations (exercise/CR effects) are corroborated by independent labs and remain robust. What is contested is the generalizability: structural aging researchers argue these interventions work in healthy-aged subjects but not in those with accumulated mitochondrial damage, and that the demand-signal framework overstates reversibility. Clinical geriatricians report mixed patient outcomes, with benefits concentrated in early decline. No party outside the intervention research community attests that demand-signal deficiency is the primary driver of mitochondrial aging across the elderly population.
narrative_ontology:disappearance_verdict(mitochondrial_demand_signal_deficiency, world_rearranges).
narrative_ontology:founding_problem_status(mitochondrial_demand_signal_deficiency, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mitochondrial_demand_signal_deficiency, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-18',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(mitochondrial_demand_signal_deficiency, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mitochondrial_demand_signal_deficiency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mitochondrial_demand_signal_deficiency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mitochondrial_demand_signal_deficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint channels substantial resources to interventions whose efficacy for advanced mitochondrial aging is contested, while suppressing alternative framings that would reduce intervention purchases. Suppression is higher (0.71) because the framework's persistence depends on marginalizing structural aging research and maintaining patient belief that decline is correctable—not on genuine participant preference once full information is available. Theater ratio is moderate (0.42): the demand-signal mechanism is real in healthy-aged subjects and animal models, but a growing share of intervention marketing and clinical application extends the framework beyond its validated domain. Accessibility collapse is moderate (0.48): alternative framings (structural aging, acceptance-based approaches) remain accessible to those who seek them. Resistance is substantial (0.58): structural aging researchers actively contest the framework, and patients who experience intervention failure increasingly question the reversibility premise. The temporal measurements show all three metrics rising over the 25-year interval as the supplement market matured and the gap between intervention promise and population-level outcomes became more apparent.
 *
 * PERSPECTIVAL GAP:
 *   From the supplement industry and intervention researcher seats, the constraint operates as genuine coordination: organizing research around a testable mechanism, enabling intervention development, offering hope to aging populations. From the powerless patient seat with identity-locked exit, the same structure operates as enforced extraction: internalized responsibility for decline, financial cost of interventions, suppression of the information that would enable exit (that their specific decline may not be demand-signal reversible). The engine computes this divergence from the structural data; the authored claim (tangled_rope) and metrics (substantially extractive, actively enforced) are independent facts.
 *
 * DIRECTIONALITY LOGIC:
 *   The supplement industry is the structural beneficiary: collects revenue directly from the constraint's operation, mobile exit options mean low effective extraction (can pivot to other product categories). Longevity intervention researchers are agenda setters and secondary beneficiaries: career advancement tied to the framework but constrained exit (accumulated expertise and grant continuity). Patients attributing decline to inevitability are primary victims: identity-locked (self-concept as responsible aging individual fused with intervention adherence), powerless (cannot independently verify mechanism), high effective extraction. Elderly populations purchasing interventions are secondary victims: moderate power (some resources, some information access), constrained exit (accepting inevitability conflicts with active aging narratives), substantial but not maximal effective extraction. Structural aging researchers are excluded rather than coordinated: their framing competes for the same resources. Clinical geriatricians are observers: analytical exit options, can assess outcomes across framings. Health insurance systems are institutional payers: constrained exit (political pressure to cover preventive interventions), moderate effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (organize research and intervention around correctable demand-signal deficiency) has partially outlived its function: the founding observations (exercise/CR effects in healthy-aged subjects) remain valid, but extension to all mitochondrial aging and all elderly populations overstates reversibility. The framework now serves more to sustain intervention markets and research careers than to accurately guide clinical practice. However, it is not pure mandatrophy because genuine coordination function persists: the demand-signal mechanism is real in its validated domain, and the framework does organize useful research. The tangled_rope classification captures this: real coordination plus asymmetric extraction, requiring active enforcement to suppress the structural aging alternative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversibility_boundary,
    'What is the boundary between demand-signal-reversible mitochondrial decline and structurally irreversible decline, and what fraction of the elderly population falls on each side?',
    'Longitudinal studies tracking mitochondrial parameters and intervention response across the aging spectrum, with genetic and proteomic markers distinguishing demand-limited from damage-limited decline. Requires following cohorts through intervention trials with rigorous responder analysis.',
    'If most elderly decline is demand-signal reversible, the framework is genuine coordination with modest extraction overhead. If most is structurally irreversible, the framework is primarily extractive, channeling resources to interventions that cannot work for the majority. The classification would shift from tangled_rope toward snare for the structurally-aged population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_boundary, empirical, 'What fraction of mitochondrial aging is reversible via demand signals versus structural damage.').

omega_variable(
    intervention_efficacy_vs_marketing,
    'Do mitochondrial interventions produce clinically meaningful functional improvements in real-world elderly populations, or do measured biomarker changes fail to translate to health outcomes?',
    'Meta-analysis of intervention trials using functional endpoints (mobility, independence, hospitalization) rather than surrogate biomarkers, with subgroup analysis by baseline mitochondrial status and age. Requires industry-independent funding to avoid publication bias.',
    'If interventions produce robust functional gains, the extraction component is payment for real benefit. If biomarker changes do not translate to functional improvement, the constraint is extracting payment for theater. This omega directly determines the theater ratio''s interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_efficacy_vs_marketing, empirical, 'Whether mitochondrial interventions deliver functional health benefits or only biomarker changes.').

omega_variable(
    identity_lock_mechanism,
    'Is patient identity fusion with the intervention framework (internalizing failure as personal inadequacy) a side effect of the framing, or is it actively cultivated by marketing and clinical messaging?',
    'Content analysis of supplement marketing and clinical communication materials, comparing identity-reinforcing messaging (you can control your aging, failure means you didn''t try hard enough) versus outcome-neutral messaging. Patient interviews about attribution of intervention failure.',
    'If identity lock is cultivated, suppression is higher than the structural measure suggests—the constraint actively traps patients rather than passively benefiting from their beliefs. If it is a side effect, the measured suppression is accurate. This omega determines whether the constraint is closer to tangled_rope or snare for the powerless patient seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether patient identity fusion is cultivated or incidental.').

omega_variable(
    research_funding_counterfactual,
    'If the demand-signal framework lost credibility, would mitochondrial aging research funding collapse, or would it redistribute to structural aging mechanisms?',
    'Analysis of funding patterns in fields where similar intervention frameworks were abandoned (e.g., antioxidant supplementation after null trials). Interviews with program officers about funding criteria.',
    'If funding would redistribute, the coordination function is real but the specific framing is extractive. If funding would collapse, the framework is sustaining research that would not survive on structural aging evidence alone, suggesting the coordination story is cover for extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(research_funding_counterfactual, conceptual, 'Whether the coordination function depends on the demand-signal framing or would persist under alternative framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mitochondrial_demand_signal_deficiency, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mito_tr_t0, mitochondrial_demand_signal_deficiency, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(mito_tr_t0, observed).
narrative_ontology:measurement(mito_tr_t5, mitochondrial_demand_signal_deficiency, theater_ratio, 5, 0.27).
narrative_ontology:measurement_basis(mito_tr_t5, observed).
narrative_ontology:measurement(mito_tr_t10, mitochondrial_demand_signal_deficiency, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(mito_tr_t10, observed).
narrative_ontology:measurement(mito_tr_t15, mitochondrial_demand_signal_deficiency, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(mito_tr_t15, observed).
narrative_ontology:measurement(mito_tr_t20, mitochondrial_demand_signal_deficiency, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(mito_tr_t20, observed).
narrative_ontology:measurement(mito_tr_t25, mitochondrial_demand_signal_deficiency, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(mito_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(mito_be_t0, mitochondrial_demand_signal_deficiency, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(mito_be_t0, observed).
narrative_ontology:measurement(mito_be_t5, mitochondrial_demand_signal_deficiency, base_extractiveness, 5, 0.53).
narrative_ontology:measurement_basis(mito_be_t5, observed).
narrative_ontology:measurement(mito_be_t10, mitochondrial_demand_signal_deficiency, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(mito_be_t10, observed).
narrative_ontology:measurement(mito_be_t15, mitochondrial_demand_signal_deficiency, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(mito_be_t15, observed).
narrative_ontology:measurement(mito_be_t20, mitochondrial_demand_signal_deficiency, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(mito_be_t20, observed).
narrative_ontology:measurement(mito_be_t25, mitochondrial_demand_signal_deficiency, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(mito_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(mito_su_t0, mitochondrial_demand_signal_deficiency, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(mito_su_t0, observed).
narrative_ontology:measurement(mito_su_t5, mitochondrial_demand_signal_deficiency, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(mito_su_t5, observed).
narrative_ontology:measurement(mito_su_t10, mitochondrial_demand_signal_deficiency, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(mito_su_t10, observed).
narrative_ontology:measurement(mito_su_t15, mitochondrial_demand_signal_deficiency, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(mito_su_t15, observed).
narrative_ontology:measurement(mito_su_t20, mitochondrial_demand_signal_deficiency, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(mito_su_t20, observed).
narrative_ontology:measurement(mito_su_t25, mitochondrial_demand_signal_deficiency, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(mito_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mitochondrial_demand_signal_deficiency, resource_allocation).
narrative_ontology:boltzmann_floor_override(mitochondrial_demand_signal_deficiency, 0.18).
narrative_ontology:affects_constraint(mitochondrial_demand_signal_deficiency, nad_precursor_supplementation_efficacy).
narrative_ontology:affects_constraint(mitochondrial_demand_signal_deficiency, caloric_restriction_mimetic_compounds).
narrative_ontology:affects_constraint(mitochondrial_demand_signal_deficiency, exercise_as_mitochondrial_medicine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the broader 'mitochondrial aging' kernel. A sibling constraint 'mitochondrial_somatic_mutation_accumulation' would frame the same decline as primarily structural and irreversible, with different beneficiary/victim structure (structural aging researchers as agenda setters, acceptance-based clinical approaches as coordination function, supplement industry as excluded). The two constraints share observables (ATP synthesis, ETC activity) but have substantially different ε values because they attribute the decline to different mechanisms with different intervention implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mitochondrial_demand_signal_deficiency, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
