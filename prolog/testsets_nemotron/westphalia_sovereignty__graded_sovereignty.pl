% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__graded_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__graded_sovereignty, []).

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
 *   constraint_id: westphalia_sovereignty__graded_sovereignty
 *   human_readable: Graded Sovereignty — Intervention Legitimacy Calibrated to State Capacity
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   This constraint instantiates the graded_sovereignty reading of the
 *   westphalia_sovereignty kernel. It holds that territorial authority is not
 *   binary (sovereign/non-sovereign) but scalar — states exist on a continuum
 *   from full sovereignty (stable Western democracies) to nominal sovereignty
 *   (failed/near-failed states). Intervention legitimacy is calibrated to
 *   measured capacity deficits: the greater the capacity shortfall, the more
 *   legitimate external intervention becomes. This creates a de facto tiered
 *   state system where capacity-evaluation authorities (IFIs, UN agencies,
 *   great power patrons, NGOs) determine which states retain full territorial
 *   inviolability and which are subject to paternalistic oversight. The
 *   constraint's coordination function is the provision of a legitimate
 *   framework for international assistance to failing states; its extraction
 *   function is the transfer of decision-making authority and resource
 *   control from weak states to evaluation authorities and their great power
 *   patrons. The claim/metric gap is deliberate: the constraint is CLAIMED as
 *   tangled_rope (coordination with asymmetric extraction) while the authored
 *   metrics describe substantial and rising extraction with moderate
 *   coordination theater.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.62).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty — Intervention Legitimacy Calibrated to State Capacity").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, 'aa7907ff-564b-4163-b25d-984ff6ded2d1').
narrative_ontology:cs_kernel_codification('aa7907ff-564b-4163-b25d-984ff6ded2d1', formalized).
narrative_ontology:cs_authority_grounding('aa7907ff-564b-4163-b25d-984ff6ded2d1', extraction).
narrative_ontology:cs_interpretation_layer_present('aa7907ff-564b-4163-b25d-984ff6ded2d1').
narrative_ontology:cs_reading_relation('aa7907ff-564b-4163-b25d-984ff6ded2d1', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('aa7907ff-564b-4163-b25d-984ff6ded2d1', westphalia_sovereignty__conditional_responsibility, influences).
narrative_ontology:cs_axiom('aa7907ff-564b-4163-b25d-984ff6ded2d1', foundational, sovereignty_is_scalar_not_categorical).
narrative_ontology:cs_axiom_status(sovereignty_is_scalar_not_categorical, holdable).
narrative_ontology:cs_axiom_grounding('aa7907ff-564b-4163-b25d-984ff6ded2d1', sovereignty_is_scalar_not_categorical, conventional).
narrative_ontology:cs_axiom('aa7907ff-564b-4163-b25d-984ff6ded2d1', foundational, capacity_metrics_legitimate_intervention_authority).
narrative_ontology:cs_axiom_status(capacity_metrics_legitimate_intervention_authority, holdable).
narrative_ontology:cs_axiom_grounding('aa7907ff-564b-4163-b25d-984ff6ded2d1', capacity_metrics_legitimate_intervention_authority, instrumental).
narrative_ontology:cs_reference_frame('aa7907ff-564b-4163-b25d-984ff6ded2d1', post_cold_war_state_failure_crisis).
narrative_ontology:cs_drift_state('aa7907ff-564b-4163-b25d-984ff6ded2d1', contemporary_geopolitical_competition_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('aa7907ff-564b-4163-b25d-984ff6ded2d1', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, great_power_patrons).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, intervention_mandate_institutions).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, nominal_sovereignty_populations).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, non_aligned_middle_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, intervention_mandate_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% International financial institutions (World Bank, IMF), UN specialized agencies, major bilateral aid agencies, and prominent governance indices (World Governance Indicators, Fragile States Index, Freedom House) that produce, certify, and operationalize state capacity metrics. They set the evaluation frameworks, determine which states fall below intervention thresholds, and design the conditionality and capacity-building programs that follow. Their institutional budgets, staffing, and policy relevance depend on the regime's continuation. They can arbitrage across evaluation mandates — if one framework loses legitimacy, they shift to another.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities, beneficiary).

% Permanent Security Council members and other major powers (US, China, Russia, EU members) that use capacity evaluations to legitimize influence over weak states' domestic affairs. They shape evaluation criteria through board representation and funding, direct intervention mandates through Security Council veto power, and capture post-intervention resource flows. They do not directly administer evaluations but control the political authorization layer. They can arbitrage across intervention frameworks — shifting between R2P, counterterrorism, stabilization, and great power competition rationales as needed.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, great_power_patrons, beneficiary,
    powerful, biographical, arbitrage, global).

% UN Security Council, regional organizations (AU, EU, OAS, ECOWAS), and ad hoc coalitions that authorize and oversee interventions. They gain operational legitimacy and institutional purpose from the graded sovereignty framework but bear substantial enforcement costs (peacekeeping budgets, political capital, troop contributions). Their exit is constrained: they cannot abandon the mandate without losing the legitimacy the framework provides, but they resist bearing costs without burden-sharing.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, intervention_mandate_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, intervention_mandate_institutions, payer).

% States with low governance capacity, limited territorial control, aid dependence, and weak institutional legitimacy (e.g., Somalia, South Sudan, Haiti, CAR, Yemen, Afghanistan post-2001). They are subject to continuous evaluation, conditionality, and graduated intervention (technical assistance → budget support conditionality → peacekeeping → trusteeship-like administration). They cannot exit the evaluation regime without losing recognition, aid, market access, and security guarantees — the regime makes their nominal sovereignty the very mechanism that legitimates the authority transfer. Their populations bear the costs of disrupted services, displaced governance, and lost self-determination.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, weak_states, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, weak_states, payer).

% Populations in weak states who experience the graded sovereignty framework as external control over their daily governance — humanitarian corridors replacing public services, parallel aid bureaucracies bypassing state institutions, security sector reform imposing foreign doctrinal priorities. They have no voice in capacity evaluations, no exit from the intervention framework, and no effective resistance capacity. Their nominal citizenship in a sovereign state becomes the legal basis for the authority transfers they experience.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, nominal_sovereignty_populations, payer,
    powerless, biographical, trapped, local).

% States with sufficient capacity to avoid direct intervention (India, Brazil, Indonesia, South Africa, Turkey, Gulf monarchies) but insufficient to shape evaluation criteria. They must participate in IFI governance, UN reporting, and peer review mechanisms to maintain standing, paying compliance costs (policy conditionality, transparency requirements, aid coordination) without capturing the evaluation agenda. Their exit is constrained: they can resist specific evaluations but cannot reject the framework without diplomatic isolation and market penalties.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, non_aligned_middle_powers, payer,
    moderate, biographical, constrained, regional).

% Academic and practitioner communities that analyze, critique, and legitimize the graded sovereignty framework through treaty interpretation, customary law analysis, and institutional design proposals. They provide the doctrinal infrastructure that makes capacity-based intervention legible as law rather than politics. They are not directly subject to extraction but their intellectual labor sustains the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, international_legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__graded_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legitimate, rule-governed framework for the international community to assist states that cannot fulfill core sovereign functions (security, welfare, territorial control) without each intervention requiring ad hoc justification that violates sovereignty norms. Solves the coordination problem of how to help failing states without either abandoning them or destroying the sovereignty system.
% TRANSFER_FUNCTION: Moves decision-making authority over domestic policy, resource allocation, security sector organization, and institutional design from weak states to capacity-evaluation authorities and their great power patrons. Moves financial resources from great power treasuries and IFI balance sheets to weak states — but with conditionality that transfers effective control over allocation. Moves legitimacy from the sovereign equality principle to the capacity-evaluation hierarchy.
% ABSENT_VOICES: Populations in weak states who would object to the paternalistic framing of their governance deficits but are represented only by the very elites whose capacity failures trigger the interventions. Non-aligned civil society organizations in the Global South that reject both the Western capacity metrics and the intervention outcomes but are excluded from the evaluation design process. States that have exited the IFI system (North Korea, Eritrea historically) — their experience of sovereignty without evaluation is not part of the evidence base.
% DISAPPEARANCE_RATIONALE: If the graded sovereignty framework vanished overnight, the post-1990 architecture of conditional aid, Security Council mandates calibrated to state capacity, IFI conditionality, peacebuilding missions, and R2P operationalization would lose their legitimating logic. Weak states would regain formal sovereign equality but lose the structured assistance framework. Great powers would revert to ad hoc intervention rationales (strategic interest, humanitarian emergency, regional stability). The evaluation industry (governance indices, capacity assessments, fragility rankings) would collapse. The tiered state system would flatten formally but likely persist informally through bilateral power.
% FOUNDING_PROBLEM: The post-Cold War surge in state failure (Somalia, Rwanda, Balkans, Haiti, Liberia, Sierra Leone) created a legitimacy vacuum: the UN Charter's sovereign equality principle provided no framework for addressing states that could not govern, while ad hoc interventions (Somalia 1992, Haiti 1994, Kosovo 1999) lacked consistent legal authorization. The graded sovereignty framework was built to solve: how can the international community legitimately respond to state failure without either ignoring it or destroying the sovereignty system?
% FOUNDING_PROBLEM_CORROBORATION: The evaluation authorities (World Bank, UN Development Programme, OECD-DAC) attest the problem is live — citing climate fragility, pandemic preparedness, and persistent governance deficits as new frontiers. Great power patrons attest the problem is live but contest the framework's application to their spheres of influence. Weak state governments and Global South diplomats (G77, Non-Aligned Movement) attest the founding problem was substantially solved by 2005 (the peak of the state-building consensus) and the framework now persists as extraction — documented in UNGA debates, NAM summit declarations, and independent Global South policy institutes (South Centre, Third World Network).
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__graded_sovereignty, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__graded_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__graded_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68) because capacity evaluation systematically transfers authority over domestic policy, resource allocation, and security from weak states to external evaluators and interveners. Suppression is substantial (0.62) because the constraint's persistence depends on actively constraining exit options for weak states — they cannot opt out of evaluation frameworks, conditionality, or intervention without losing recognition, aid, and market access. Theater ratio is moderate-high (0.45) because the 'capacity building' and 'partnership' framings perform genuine coordination work while masking the authority transfer. The measurement series shows steady extraction accumulation and theater growth across the post-Cold War interval, with suppression requirement tracking the institutionalization of evaluation mechanisms. All metrics share one time grid (1990-2025, 5-year intervals) as required.
 *
 * PERSPECTIVAL GAP:
 *   From the evaluation authority seat, the constraint is genuine coordination: it solves the problem of how to legitimately assist failing states without violating sovereignty norms. From the weak state seat, the same structure operates as enforced extraction: their nominal sovereignty is the mechanism that legitimates the authority transfer. The engine computes this divergence from the structural data — the declared beneficiaries and victims, their power atoms, and exit options. The middle power seat should compute differently from both: they have enough capacity to avoid direct intervention but must pay compliance costs to the evaluation regime.
 *
 * DIRECTIONALITY LOGIC:
 *   Capacity evaluation authorities (IFIs, UN specialized agencies, great power foreign ministries) are structural beneficiaries: they gain decision-making authority, institutional relevance, and resource control through the evaluation regime — d near 0.0 (beneficiary end). Great power patrons benefit indirectly through influence over evaluation outcomes and intervention mandates — d ~0.15. Intervention mandate institutions (Security Council, regional organizations) sit near symmetric (d~0.5): they gain operational legitimacy but bear enforcement costs. Weak states and their populations are structural targets: they bear the authority transfers, conditionality, and interventions — d near 1.0. Non-aligned middle powers occupy a constrained position: they avoid the deepest extraction but must participate in evaluation regimes to maintain standing — d~0.65. The beneficiary/victim declarations drive the engine's directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimate international response to state failure) remains live but has been substantially solved in its original form — the coordination mechanisms exist. The constraint persists because the evaluation regime has become self-justifying: its own outputs (capacity metrics, intervention mandates) are now the primary evidence for its necessity. This is mandatrophy: the mandate (legitimate assistance framework) has outlived its function (solving the coordination problem of how to help without violating sovereignty) and now primarily serves the evaluators' institutional interests. The founding problem status is 'contested' — the original problem is largely solved, but new problems (climate fragility, pandemic preparedness) are invoked to extend the regime.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the graded_sovereignty reading structurally distinct from conditional_responsibility, or do they collapse into a single extractive mechanism under empirical scrutiny?',
    'Compare intervention cases 1990-2025: if the same weak states are targeted under both ''capacity deficit'' and ''atrocity prevention'' framings with identical interveners and outcomes, the readings share a single extraction structure; if targeting patterns diverge (different states, different interveners, different post-intervention arrangements), they are structurally distinct.',
    'If they collapse, the kernel contains only two genuinely distinct readings (absolute_non_intervention vs. extractive_intervention_framework) rather than three. The graded reading''s ε would be authored for the merged structure, not a distinct one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether graded_sovereignty and conditional_responsibility are separate constraints or one constraint with two framings').

omega_variable(
    capacity_metric_capture,
    'Do the capacity metrics (governance indices, territorial control, institutional quality) used to calibrate intervention legitimacy function as objective measures or as instruments calibrated to produce desired intervention outcomes?',
    'Historical audit of index methodology changes vs. intervention decisions: if metrics are revised or selectively applied to justify predetermined interventions, they are captured instruments; if they predict interventions independently of political preferences, they are genuine coordination signals.',
    'If captured, the constraint''s coordination function is substantially theatrical — the metrics serve the extraction, not the reverse. Theater ratio would be higher; effective extraction would approach snare territory for the evaluation authority seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capacity_metric_capture, empirical, 'Whether state capacity metrics are genuine coordination standards or extracted instruments').

omega_variable(
    intervention_legitimacy_cascade,
    'Does the graded sovereignty framework create a self-reinforcing legitimacy cascade where each intervention weakens the target state''s capacity further, justifying deeper subsequent intervention?',
    'Longitudinal tracking of post-intervention state capacity trajectories: if intervened states systematically decline on capacity metrics while non-intervened peers with similar baselines stabilize or improve, the cascade is real.',
    'A real cascade means the constraint''s extraction is self-amplifying — each round of extraction degrades the victim''s capacity to resist the next round. This would push the constraint toward snare classification over time, not merely tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_legitimacy_cascade, empirical, 'Whether graded sovereignty interventions produce capacity degradation that justifies further intervention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalia_sovereignty__graded_sovereignty, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(west_tr_t1995, westphalia_sovereignty__graded_sovereignty, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(west_tr_t2000, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(west_tr_t2005, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(west_tr_t2010, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(west_tr_t2015, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2015, 0.44).
narrative_ontology:measurement(west_tr_t2020, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2020, 0.45).
narrative_ontology:measurement(west_tr_t2025, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(west_be_t1995, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(west_be_t2000, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(west_be_t2005, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(west_be_t2010, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(west_be_t2015, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(west_be_t2020, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(west_be_t2025, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(west_su_t1995, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement(west_su_t2000, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(west_su_t2005, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(west_su_t2010, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(west_su_t2015, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(west_su_t2020, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2020, 0.61).
narrative_ontology:measurement(west_su_t2025, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__graded_sovereignty, 0.12).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, responsibility_to_protect_operationalization).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, international_financial_institution_conditionality).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, state_building_intervention_framework).

% DUAL FORMULATION NOTE:
% This constraint is one member of the westphalia_sovereignty kernel family. The three readings (absolute_non_intervention, conditional_responsibility, graded_sovereignty) share the same kernel but instantiate different constraints with different ε values, different beneficiary/victim structures, and different classifications. absolute_non_intervention is a mountain (ε≈0.05, emerges_naturally=true from the perspective of its adherents). conditional_responsibility is a rope or tangled_rope depending on operationalization (ε≈0.3-0.5). graded_sovereignty is a tangled_rope (ε=0.68) because its continuous calibration creates a permanent evaluation regime with structural extraction. The ε-invariance principle requires separate stories: the label 'sovereignty' covers three structurally distinct claims with different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__graded_sovereignty, institutional, 0.1).
constraint_indexing:directionality_override(westphalia_sovereignty__graded_sovereignty, powerful, 0.15).
constraint_indexing:directionality_override(westphalia_sovereignty__graded_sovereignty, moderate, 0.65).
constraint_indexing:directionality_override(westphalia_sovereignty__graded_sovereignty, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
