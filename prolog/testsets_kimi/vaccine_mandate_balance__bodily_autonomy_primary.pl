% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__bodily_autonomy_primary, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy Absolute: State Cannot Compel Medical Intervention
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the bodily_autonomy_primary reading of the
 *   vaccine_mandate_balance kernel: a constitutional-legal principle that
 *   individual consent to medical intervention is inviolable and cannot be
 *   overridden by collective benefit claims. During public health
 *   emergencies, this constraint actively suppresses mandate authority and
 *   shifts the burden of epidemic response to voluntary measures and
 *   non-pharmaceutical interventions. Under this reading, unvaccinated
 *   individuals are protected beneficiaries, while public health agencies and
 *   healthcare workers bear the costs of constrained policy capacity;
 *   immunocompromised populations are explicitly excluded from victim status
 *   because elevated risk is treated as inherent to liberty.
 *
 * KEY AGENTS:
 *   - Constitutional judiciary: agenda-setter (institutional/analytical) â enforces the autonomy boundary
 *   - Unvaccinated individuals: primary beneficiary (moderate/constrained) â shielded from state compulsion
 *   - Public health agencies: primary payer (institutional/constrained) â lose mandate tool during emergencies
 *   - Healthcare workers: secondary payer (organized/constrained) â absorb occupational and workload costs
 *   - Immunocompromised populations: excluded (powerless/trapped) â structurally excluded from override consideration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.8).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.68).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.8).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "Bodily Autonomy Absolute: State Cannot Compel Medical Intervention").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, 'e9745a8a-c3a2-417f-9411-fa49dc92e89b').
narrative_ontology:cs_kernel_codification('e9745a8a-c3a2-417f-9411-fa49dc92e89b', formalized).
narrative_ontology:cs_authority_grounding('e9745a8a-c3a2-417f-9411-fa49dc92e89b', lineage).
narrative_ontology:cs_interpretation_layer_present('e9745a8a-c3a2-417f-9411-fa49dc92e89b').
narrative_ontology:cs_reading_relation('e9745a8a-c3a2-417f-9411-fa49dc92e89b', vaccine_mandate_balance__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('e9745a8a-c3a2-417f-9411-fa49dc92e89b', vaccine_mandate_balance__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('e9745a8a-c3a2-417f-9411-fa49dc92e89b', foundational, bodily_autonomy_absolute).
narrative_ontology:cs_axiom_status(bodily_autonomy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('e9745a8a-c3a2-417f-9411-fa49dc92e89b', bodily_autonomy_absolute, deontological).
narrative_ontology:cs_axiom('e9745a8a-c3a2-417f-9411-fa49dc92e89b', foundational, state_medical_power_absolute_prohibition).
narrative_ontology:cs_axiom_status(state_medical_power_absolute_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('e9745a8a-c3a2-417f-9411-fa49dc92e89b', state_medical_power_absolute_prohibition, deontological).
narrative_ontology:cs_reference_frame('e9745a8a-c3a2-417f-9411-fa49dc92e89b', constitutional_autonomy_absolutism).
narrative_ontology:cs_drift_state('e9745a8a-c3a2-417f-9411-fa49dc92e89b', post_covid_mandate_debate, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e9745a8a-c3a2-417f-9411-fa49dc92e89b', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, healthcare_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces constitutional and human rights provisions that bar the state from compelling medical intervention. Issues injunctions against mandate programs, reviews emergency orders, and establishes precedent that treats individual consent as categorically inviolable regardless of asserted collective benefit.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, constitutional_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Are shielded from state-imposed vaccination by legal and constitutional barriers. Cannot be compelled to undergo medical intervention even during declared public health emergencies. Their refusal is protected as a right, though they remain subject to social pressure and cannot easily exit the jurisdiction without significant cost.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_individuals, beneficiary,
    moderate, biographical, constrained, national).

% Bear the operational cost of epidemic containment without access to compulsory vaccination as a policy tool. Must rely on persuasion, voluntary uptake, and less effective non-pharmaceutical interventions when coverage rates are insufficient to protect vulnerable populations.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, public_health_agencies, payer,
    institutional, generational, constrained, national).

% Face elevated occupational exposure and workload when vaccine coverage is suboptimal due to legal barriers on compulsion. Must treat preventable severe illness in unvaccinated populations while operating under staffing and resource constraints.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, healthcare_workers, payer,
    organized, biographical, constrained, national).

% Bear heightened infection risk from community transmission but are structurally excluded from override consideration under this reading; their need for collective protection is treated as subordinate to individual liberty, with risk acceptance framed as inherent to a free society.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_populations, excluded,
    powerless, biographical, trapped, national).

narrative_ontology:fixing_cost_class(vaccine_mandate_balance__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes an absolute legal boundary on state power over the body, preventing government overreach into medical decision-making and solving the coordination problem of how to protect individuals from forced medical intervention during emergencies.
% TRANSFER_FUNCTION: Moves the burden of infectious disease containment from the individual (who cannot be compelled) to public health agencies and healthcare workers (who must absorb the costs of alternative interventions and elevated caseloads), and transfers legal protection to individuals against state medical compulsion.
% ABSENT_VOICES: Public health agencies seeking mandate authority and immunocompromised individuals seeking collective barriers to transmission are structurally excluded; their claims are treated as legally irrelevant because individual consent is held inviolable regardless of collective benefit.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, states could implement compulsory vaccination during public health emergencies; public health agencies would gain a major policy tool, unvaccinated individuals would face direct state coercion, and the legal-ethical architecture would reorganize around proportionality or collective benefit rather than categorical individual inviolability.
% FOUNDING_PROBLEM: Historical state medical abuse, including nonconsensual experimentation, forced sterilization, and coercive public health programs, created the need for an absolute constitutional limit on government power over bodily integrity.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations and medical historians outside the public health policy community attest to ongoing risks of state medical overreach; public health advocates argue the founding problem is being instrumentalized to block legitimate emergency measures, and no neutral party attests unanimity.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 0.8, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.80) because the constraint severely limits public health capacity during emergencies, imposing substantial operational and human costs on agencies and workers. Suppression is high (0.68) because the constraint actively suppresses mandate alternatives through judicial enforcement. Theater is low-moderate (0.28): courts are genuinely enforcing a rights framework rooted in historical abuse, though some of the absolutist rhetoric during political debate is performative. Resistance (0.58) reflects sustained pushback from public health authorities and segments of the medical community. Accessibility collapse (0.72) is high because once the inviolability principle is constitutionalized, proportionality alternatives become legally inaccessible.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and public health agencies should compute to different constraint types from their respective seats: the judiciary experiences the constraint as rights-protection coordination (rope-like), while public health agencies experience it as enforced capacity deprivation (snare-like). The engine should detect this divergence from the structural data â low d for beneficiaries, high d for institutional payers with constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated individuals sit near the beneficiary pole (low d): the constraint subsidizes their liberty by blocking state compulsion. Public health agencies and healthcare workers sit near the target pole (high d): they bear the extraction in the form of constrained policy tools and elevated occupational risk. The constitutional judiciary has analytical exit; its directionality is agenda-setting rather than beneficiary or target. Immunocompromised populations are excluded from the beneficiary-victim derivation entirely under this reading, consistent with the source instruction that their risk exposure is not treated as victim-generating harm.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by preserving the coordination story: the constraint genuinely solves the coordination problem of limiting state medical power, grounded in documented historical abuse. Without the genuine coordination function (rights protection against state overreach), the high extraction from public health capacity would read as a pure snare. The presence of active enforcement, a real coordination function, and identifiable victims in the public health system places it in tangled_rope rather than rope or snare. The low theater ratio distinguishes it from piton: the enforcement is functional, not performative maintenance of an atrophied structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophic_threshold_ambiguity,
    'Does the inviolability constraint remain stable if disease severity crosses a catastrophic or existential threshold, or does the framework implicitly contain an unstated emergency override?',
    'Historical stress-test analysis: examine whether jurisdictions holding this reading have ever suspended it for smallpox, Ebola, or analogous high-lethality pathogens, and whether such suspension is framed as exception or contradiction.',
    'If the constraint is silently suspended at catastrophic thresholds, its absolutism is rhetorical rather than structural, lowering base_extractiveness and potentially shifting classification toward proportionality. If it holds even at catastrophic thresholds, the extraction from public health capacity is more severe than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophic_threshold_ambiguity, empirical, 'Whether absolute bodily autonomy survives catastrophic risk thresholds.').

omega_variable(
    coercion_definition_ambiguity,
    'Is judicial enforcement of bodily autonomy against state mandates itself a coercive mechanism that should count toward suppression, or is blocking state action definitionally non-coercive?',
    'Comparative legal analysis measuring whether the constraint''s operation involves contempt penalties, injunctions, or sanctions against officials â if so, it deploys state coercion to block state coercion, and suppression should reflect both layers.',
    'If judicial enforcement counts as coercion, suppression and extraction are higher than a state-action-only framing suggests; this tightens the tangled_rope classification. If blocking state action is treated as non-coercive by definition, suppression may be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_definition_ambiguity, conceptual, 'Whether judicial enforcement of autonomy constitutes coercion.').

omega_variable(
    kernel_reading_separation,
    'Is the vaccine_mandate_balance kernel properly decomposed into three independent constraints, or does proportionality represent a meta-rule that mediates between the other two rather than a sibling reading?',
    'Corpus analysis: if proportionality_reading constraints consistently network-mediate between bodily_autonomy_primary and public_health_primary constraints rather than operating as independent nodes, the kernel decomposition should be revised to a dual reading plus meta-mediation structure.',
    'If proportionality is a meta-rule, the current three-way decomposition overgenerates constraint objects and misattributes coupling edges; the entire constraint family would need re-authoring.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_separation, conceptual, 'Whether the kernel decomposition into three sibling readings is structurally correct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 6, 0.18).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 12, 0.25).
narrative_ontology:measurement(vacc_tr_t18, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 18, 0.3).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 12, 0.75).
narrative_ontology:measurement(vacc_be_t18, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 18, 0.82).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 24, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(vacc_su_t18, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 18, 0.76).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
