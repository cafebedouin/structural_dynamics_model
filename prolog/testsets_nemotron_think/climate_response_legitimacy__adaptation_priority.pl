% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__adaptation_priority, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Legitimacy Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint story captures the adaptation-priority reading of the
 *   contested kernel 'climate_response_legitimacy'. The reading asserts that
 *   legitimate climate response must accept the warming trajectory as given
 *   (per IPCC assessments of committed warming) and prioritize protecting
 *   vulnerable populations through resilience infrastructure and adaptive
 *   capacity. Structurally, this reading functions as a tangled rope: it
 *   coordinates genuine adaptation finance and technology transfer
 *   (coordination function) while extracting the cost of wealthy nations'
 *   continued fossil-fuel-dependent development onto low-income regions and
 *   future generations (asymmetric extraction). The $350B annual adaptation
 *   finance gap represents the extraction magnitude. The reading requires
 *   active enforcement through UNFCCC finance mechanisms, Green Climate Fund
 *   operations, and national adaptation plan requirements.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.75).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.55).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.75).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Adaptation-Priority Climate Legitimacy Reading").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, '852f91db-a2d5-489e-a02a-3251e1118c19').
narrative_ontology:cs_kernel_codification('852f91db-a2d5-489e-a02a-3251e1118c19', distributed).
narrative_ontology:cs_authority_grounding('852f91db-a2d5-489e-a02a-3251e1118c19', extraction).
narrative_ontology:cs_interpretation_layer_present('852f91db-a2d5-489e-a02a-3251e1118c19').
narrative_ontology:cs_reading_relation('852f91db-a2d5-489e-a02a-3251e1118c19', climate_response_legitimacy__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('852f91db-a2d5-489e-a02a-3251e1118c19', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('852f91db-a2d5-489e-a02a-3251e1118c19', foundational, warming_trajectory_accepted_as_given).
narrative_ontology:cs_axiom_status(warming_trajectory_accepted_as_given, holdable).
narrative_ontology:cs_axiom_grounding('852f91db-a2d5-489e-a02a-3251e1118c19', warming_trajectory_accepted_as_given, empirically_contingent).
narrative_ontology:cs_axiom('852f91db-a2d5-489e-a02a-3251e1118c19', foundational, adaptation_finance_obligation_on_wealthy_nations).
narrative_ontology:cs_axiom_status(adaptation_finance_obligation_on_wealthy_nations, holdable).
narrative_ontology:cs_axiom_grounding('852f91db-a2d5-489e-a02a-3251e1118c19', adaptation_finance_obligation_on_wealthy_nations, conventional).
narrative_ontology:cs_reference_frame('852f91db-a2d5-489e-a02a-3251e1118c19', adaptation_centric_legitimacy).
narrative_ontology:cs_drift_state('852f91db-a2d5-489e-a02a-3251e1118c19', contemporary_paris_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('852f91db-a2d5-489e-a02a-3251e1118c19', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_nations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, fossil_fuel_interests).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, development_model_preservationists).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, small_island_states).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, adaptation_finance_principle).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, common_but_differentiated_responsibilities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the adaptation-priority framing in UNFCCC negotiations and control the majority of climate finance flows. Preserve their development model by accepting warming trajectory while committing to adaptation finance that consistently falls short of pledged amounts. Can shift between adaptation/mitigation framing as political winds change.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_nations, agenda_setter,
    powerful, generational, arbitrage, global).

% Face immediate $350B annual adaptation deficit while having contributed least to historical emissions. Depend on unpredictable international finance for resilience infrastructure. Have no exit from climate impacts — geographic and economic lock-in makes adaptation a survival requirement, not a policy choice.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, low_income_regions, payer,
    powerless, biographical, trapped, regional).

% Benefit from legitimacy framework that accepts continued warming, which extends the economic viability of fossil fuel assets. Fund adaptation-focused narratives that displace mitigation urgency. Capture policy influence through lobbying and revolving-door appointments in wealthy nation governments.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, fossil_fuel_interests, beneficiary,
    institutional, generational, arbitrage, global).

% Bear compounded costs of higher warming trajectory locked in by deferred mitigation. No voice in current legitimacy contests; cannot exit the climate system they inherit. Adaptation costs escalate non-linearly with each increment of warming they did not choose.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Face existential risk from sea-level rise even under adaptation-priority scenarios. Negotiate within UNFCCC as a bloc but lack enforcement leverage. Adaptation finance reaches them last and least; their disappearance would be the visible failure of this legitimacy reading.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, small_island_states, payer,
    moderate, biographical, constrained, regional).

% Contest the adaptation-priority reading from outside formal negotiations. Document the adaptation finance gap and trace how wealthy nations use adaptation rhetoric to avoid mitigation. Provide the evidentiary basis for alternative legitimacy readings but are structurally excluded from agenda-setting.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_justice_movements, observer,
    organized, generational, analytical, global).

% Administer the institutional framework that translates legitimacy readings into finance mechanisms and reporting requirements. IPCC assessments legitimize the warming trajectory acceptance; UNFCCC processes negotiate the adaptation finance architecture. Absorb drift between readings without forcing revision of the kernel.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, unfccc_ipcc_bodies, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, unfccc_ipcc_bodies, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinating international adaptation finance and resilience infrastructure to protect vulnerable populations from locked-in warming impacts that mitigation alone can no longer prevent.
% TRANSFER_FUNCTION: Transfers adaptation costs from wealthy nations (who avoid mitigation/transformation costs) to low-income regions (who face $350B adaptation deficit) and future generations (who bear compounded warming costs), while preserving the fossil-fuel-intensive development model of wealthy nations.
% ABSENT_VOICES: Future generations who bear compounded costs of higher warming; indigenous and local communities whose adaptation knowledge systems are marginalized in favor of engineered infrastructure; non-human species and ecosystems with no representational standing in climate governance.
% DISAPPEARANCE_RATIONALE: The adaptation-priority legitimacy reading structures the Green Climate Fund, loss and damage negotiations, national adaptation plan requirements, and the $100B/year finance pledge. Its disappearance would collapse the current climate finance architecture and force a legitimacy shift toward either mitigation-priority (emissions reduction first) or degrowth-transformation (structural economic change) readings.
% FOUNDING_PROBLEM: The failure of mitigation-only frameworks to prevent dangerous warming within the committed carbon budget, leaving vulnerable populations exposed to impacts requiring immediate protection regardless of future emissions reductions.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGII corroborates locked-in warming and quantifies the adaptation gap; however, mitigation advocates (IEA net-zero scenarios, EU/US climate envoys) contest the framing as premature surrender that reduces political pressure for emissions cuts. No corroborating source outside the beneficiary set (wealthy nations, fossil fuel interests) endorses the status as 'live' without qualification.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the adaptation finance gap directly measures the transfer from vulnerable to wealthy nations. Suppression is moderate (0.55) — not direct coercion but structural suppression via finance architecture that makes alternatives (mitigation-priority, degrowth) politically and economically difficult for low-income regions to pursue. Theater ratio (0.45) reflects the gap between adaptation rhetoric ($100B pledge, loss and damage fund) and delivery (consistent shortfall, loan-heavy instruments). Accessibility collapse (0.5) is moderate because mitigation and degrowth readings remain intellectually and politically available but are marginalized in institutional channels. Resistance (0.7) is high because climate justice movements, small island states, and mitigation advocates actively contest this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the wealthy nation seat, this is genuine coordination: they built the adaptation finance architecture and deliver real (if insufficient) resources. From the low-income region seat, the same structure operates as enforced extraction: the finance gap is the price of admission to a system they did not create. The engine computes this divergence from the structural data — the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy nations and fossil fuel interests sit at the beneficiary end (d near 0): they preserve their development model while controlling the finance architecture. Low-income regions, small island states, and future generations sit at the target end (d near 1): they bear the adaptation deficit and compounded warming costs with trapped or constrained exit. UNFCCC/IPCC bodies are analytically positioned but structurally function as agenda-setters who absorb drift between readings. Climate justice movements are observers with analytical exit but organized power to document the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mitigation failure leaving populations exposed) is contested — mitigation advocates argue the problem is solvable with political will, while adaptation advocates argue locked-in warming makes it insoluble. If the founding problem is dead (mitigation succeeds), the adaptation-priority reading becomes a piton: an arrangement that persists by inertia after its justification evaporates. If the founding problem is live (warming trajectory locked in), the reading remains a tangled rope with genuine coordination function. The mandate has not been resolved; the contestation is the structural motor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_deficit_structural_vs_contingent,
    'Is the $350B adaptation finance gap a structural feature of the power asymmetry between wealthy and low-income nations, or a contingent mobilization failure that could be closed with political will?',
    'Counterfactual analysis: if wealthy nations mobilized COVID-scale fiscal response for adaptation finance, would the gap close? Track finance instrument composition (grants vs loans) over time — structural extraction predicts persistent loan dominance.',
    'If structural, the adaptation-priority reading is inherently extractive (tangled_rope or snare); if contingent, it could be a genuine rope with a delivery failure. Determines whether the constraint''s extraction is necessary or removable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_deficit_structural_vs_contingent, empirical, 'Whether the adaptation finance gap reflects power asymmetry or political choice.').

omega_variable(
    warming_trajectory_acceptance_empirical_vs_constructed,
    'Is the ''accepted warming trajectory'' an empirically grounded assessment of committed warming, or a politically constructed premise that legitimizes deferred mitigation?',
    'Compare IPCC scenario ensembles with actual emissions trajectories and policy commitments. If the ''accepted'' trajectory systematically exceeds what current policies deliver, the acceptance is constructed; if it tracks committed warming from historical emissions, it is empirical.',
    'If constructed, the reading''s coordination function is compromised — it coordinates around a politically convenient premise rather than a physical reality. Shifts classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(warming_trajectory_acceptance_empirical_vs_constructed, conceptual, 'Epistemic status of the warming trajectory acceptance premise.').

omega_variable(
    committer_structure_kernel_reading,
    'How does the adaptation_priority reading structurally relate to its sibling readings (mitigation_priority, degrowth_transformation) within the climate_response_legitimacy kernel?',
    'Track institutional uptake: which reading controls finance mechanisms (Green Climate Fund), which controls mitigation architecture (Article 6, carbon markets), which controls transformation discourse (just transition frameworks). Map resource flows to reading allegiance.',
    'If adaptation_priority structurally influences mitigation_priority (normalizes higher warming, reduces mitigation urgency), the kernel''s drift is toward extraction. If all three coexist as live contestation, the kernel remains distributed. If any pair forecloses, the kernel is fragmenting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Structural relationships among sibling readings of the climate legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crlap_tr_t0, climate_response_legitimacy__adaptation_priority, theater_ratio, 0, 0.25).
narrative_ontology:measurement(crlap_tr_t5, climate_response_legitimacy__adaptation_priority, theater_ratio, 5, 0.3).
narrative_ontology:measurement(crlap_tr_t10, climate_response_legitimacy__adaptation_priority, theater_ratio, 10, 0.35).
narrative_ontology:measurement(crlap_tr_t15, climate_response_legitimacy__adaptation_priority, theater_ratio, 15, 0.4).
narrative_ontology:measurement(crlap_tr_t20, climate_response_legitimacy__adaptation_priority, theater_ratio, 20, 0.42).
narrative_ontology:measurement(crlap_tr_t25, climate_response_legitimacy__adaptation_priority, theater_ratio, 25, 0.44).
narrative_ontology:measurement(crlap_tr_t30, climate_response_legitimacy__adaptation_priority, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(crlap_be_t0, climate_response_legitimacy__adaptation_priority, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(crlap_be_t5, climate_response_legitimacy__adaptation_priority, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(crlap_be_t10, climate_response_legitimacy__adaptation_priority, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(crlap_be_t15, climate_response_legitimacy__adaptation_priority, base_extractiveness, 15, 0.7).
narrative_ontology:measurement(crlap_be_t20, climate_response_legitimacy__adaptation_priority, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(crlap_be_t25, climate_response_legitimacy__adaptation_priority, base_extractiveness, 25, 0.74).
narrative_ontology:measurement(crlap_be_t30, climate_response_legitimacy__adaptation_priority, base_extractiveness, 30, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(crlap_su_t0, climate_response_legitimacy__adaptation_priority, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(crlap_su_t5, climate_response_legitimacy__adaptation_priority, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(crlap_su_t10, climate_response_legitimacy__adaptation_priority, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(crlap_su_t15, climate_response_legitimacy__adaptation_priority, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(crlap_su_t20, climate_response_legitimacy__adaptation_priority, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(crlap_su_t25, climate_response_legitimacy__adaptation_priority, suppression_requirement, 25, 0.54).
narrative_ontology:measurement(crlap_su_t30, climate_response_legitimacy__adaptation_priority, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__adaptation_priority, 0.15).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial 'legitimate climate response' into three structurally distinct claims with different ε values, beneficiary/victim structures, and institutional homes. The adaptation_priority reading (this story) has high extractiveness (0.75) because it transfers adaptation costs to the vulnerable. The mitigation_priority reading likely has lower extractiveness but higher suppression (enforcement of carbon pricing). The degrowth_transformation reading has different victim/beneficiary sets entirely. They are linked here because they compete for the same kernel's legitimacy capital.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__adaptation_priority, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
