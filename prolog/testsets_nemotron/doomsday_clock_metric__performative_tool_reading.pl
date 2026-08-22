% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__performative_tool_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__performative_tool_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: doomsday_clock_metric__performative_tool_reading
 *   human_readable: Doomsday Clock Setting as Performative Policy Mobilization Tool
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Doomsday Clock, maintained by the Bulletin of the Atomic Scientists
 *   since 1947, is a metaphorical metric representing proximity to global
 *   catastrophe. This reading holds that the Clock's annual setting is
 *   strategically chosen to maximize policy impact and mobilize collective
 *   action rather than to track measurable existential risk levels through
 *   expert synthesis. The constraint is the standing arrangement: the
 *   Bulletin's exclusive authority to set a globally recognized 'minutes to
 *   midnight' metric, presented as expert judgment but operating as a
 *   performative tool for policy mobilization. Beneficiaries are policy
 *   activists and advocacy organizations who gain rhetorical urgency; victims
 *   are epistemic credibility, public trust in science, and future risk
 *   communication efforts that inherit the credibility deficit. The claimed
 *   type is snare — the coordination story (shared urgency symbol) is cover
 *   for extraction of epistemic authority without accountability.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.72).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.65).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, snare).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock Setting as Performative Policy Mobilization Tool").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, 'ebf4f2b9-25ea-4a94-aa95-c7c983086f5a').
narrative_ontology:cs_kernel_codification('ebf4f2b9-25ea-4a94-aa95-c7c983086f5a', implicit).
narrative_ontology:cs_authority_grounding('ebf4f2b9-25ea-4a94-aa95-c7c983086f5a', lineage).
narrative_ontology:cs_interpretation_layer_present('ebf4f2b9-25ea-4a94-aa95-c7c983086f5a').
narrative_ontology:cs_reading_relation('ebf4f2b9-25ea-4a94-aa95-c7c983086f5a', doomsday_clock_metric__objective_index_reading, forecloses).
narrative_ontology:cs_reading_relation('ebf4f2b9-25ea-4a94-aa95-c7c983086f5a', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('ebf4f2b9-25ea-4a94-aa95-c7c983086f5a', foundational, urgency_mobilization_justifies_metric_flexibility).
narrative_ontology:cs_axiom_status(urgency_mobilization_justifies_metric_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('ebf4f2b9-25ea-4a94-aa95-c7c983086f5a', urgency_mobilization_justifies_metric_flexibility, instrumental).
narrative_ontology:cs_axiom('ebf4f2b9-25ea-4a94-aa95-c7c983086f5a', secondary, symbolic_metrics_operate_by_different_epistemic_rules).
narrative_ontology:cs_axiom_status(symbolic_metrics_operate_by_different_epistemic_rules, holdable).
narrative_ontology:cs_axiom_grounding('ebf4f2b9-25ea-4a94-aa95-c7c983086f5a', symbolic_metrics_operate_by_different_epistemic_rules, conventional).
narrative_ontology:cs_reference_frame('ebf4f2b9-25ea-4a94-aa95-c7c983086f5a', founding_communicative_urgency).
narrative_ontology:cs_drift_state('ebf4f2b9-25ea-4a94-aa95-c7c983086f5a', contemporary_multi_risk_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ebf4f2b9-25ea-4a94-aa95-c7c983086f5a', '2026-08-04T12:00:00Z').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, policy_activists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, bulletin_editors).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, advocacy_organizations).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, epistemic_credibility).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, public_trust_in_science).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, future_risk_communication_efforts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the Clock time annually through a deliberative process they control. Their professional identity and the Bulletin's relevance are fused with the Clock's news-making function. They justify settings as expert synthesis but acknowledge strategic communication aims. Exit would mean abandoning the institutional identity built around the Clock.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, bulletin_editors, agenda_setter,
    institutional, biographical, identity_locked, global).

% Use the Clock setting as a lever in policy debates, funding requests, and media campaigns. The closer-to-midnight setting provides rhetorical urgency that opens doors and unlocks resources. They can switch to other urgency symbols if the Clock loses credibility.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_activists, beneficiary,
    organized, biographical, mobile, global).

% Incorporate the Clock into fundraising, lobbying, and public mobilization. The annual setting cycle provides a reliable news hook. They benefit from the Clock's brand recognition but are not institutionally bound to it.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, advocacy_organizations, beneficiary,
    organized, biographical, mobile, global).

% The standing of expert judgment as a distinct epistemic category erodes when a metric presented as scientific synthesis is strategically manipulated for policy impact. Each strategic setting that diverges from measurable indicators compounds the credibility deficit for future expert communications.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, epistemic_credibility, payer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(doomsday_clock_metric__performative_tool_reading, epistemic_credibility).

% When the public perceives scientific metrics as advocacy tools, trust in the broader scientific enterprise declines. The Clock's high visibility makes it a focal point for this dynamic — its strategic use becomes evidence for 'science is political' narratives.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, public_trust_in_science, payer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(doomsday_clock_metric__performative_tool_reading, public_trust_in_science).

% Legitimate risk communication efforts inherit the credibility deficit created by the Clock's performative use. New metrics, indices, and warning systems must overcome prior skepticism seeded by the perception that the field's most famous metric is strategically set.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, future_risk_communication_efforts, payer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(doomsday_clock_metric__performative_tool_reading, future_risk_communication_efforts).

% Study the Clock as a case of science communication, normative epistemology, and risk governance. They analyze the tension between epistemic legitimacy and political efficacy but do not set the Clock or directly benefit from its settings.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, risk_governance_scholars, observer,
    analytical, biographical, analytical, global).

% Other scientific bodies (IPCC, WHO, nuclear safety agencies) produce risk assessments with different methodologies and timetables. They are not consulted in the Clock setting and would likely object to a single metaphorical metric overriding their domain-specific judgments, but they have no formal role in the process.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, competing_expert_bodies, excluded,
    institutional, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, globally recognized symbol that focuses attention on existential risks and creates a common reference point for urgency across disparate policy domains and publics.
% TRANSFER_FUNCTION: Moves epistemic authority and public attention from domain-specific expert assessments to a single metaphorical metric controlled by the Bulletin, converting scientific credibility into policy mobilization capacity for activists and advocacy organizations.
% ABSENT_VOICES: Domain-specific expert bodies (IPCC, IAEA, WHO, climate science consortia) whose assessments are superseded by the Clock's singular narrative. Affected populations in the Global South who bear existential risks but have no voice in the Clock-setting process. Future generations whose trust in expert warning systems is undermined by current strategic manipulation.
% DISAPPEARANCE_RATIONALE: If the Clock vanished overnight, policy activists would lose their most potent shared urgency symbol; advocacy organizations would lose a reliable annual news hook; the Bulletin would lose its primary relevance mechanism; but domain-specific risk assessments would continue unchanged. The rearrangement would be in the policy mobilization layer, not the underlying risk landscape.
% FOUNDING_PROBLEM: Post-1947 need to communicate nuclear danger to policymakers and publics in a visceral, immediately graspable form when technical risk assessments failed to mobilize action.
% FOUNDING_PROBLEM_CORROBORATION: Bulletin founders' own writings attest the founding problem was communicative urgency, not metric precision. Contemporary nuclear historians (e.g., Alex Wellerstein, Hugh Gusterson) corroborate the Clock was designed as a 'wake-up call' not a measurement instrument. Current Bulletin editors attest the founding problem persists (nuclear threats remain, new risks added). Critics (e.g., Michael Shellenberger, risk communication scholars) attest the founding problem is obsolete — domain-specific metrics now exist and the Clock's imprecision actively harms credibility.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(doomsday_clock_metric__performative_tool_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__performative_tool_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__performative_tool_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__performative_tool_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.72) is high because the constraint extracts epistemic authority from the scientific community and public trust, converting it into mobilization capital for activists, while the metric's connection to measurable risk indicators has attenuated over time. Suppression (0.65) is substantial because the Clock's persistence depends on actively maintaining the presentation as 'expert synthesis' while excluding domain-specific expert bodies from the setting process — alternative metrics and assessments are suppressed by the Clock's media dominance. Theater ratio (0.58) exceeds 0.5 because more than half the Constraint's observable operation (annual announcement cycle, media strategy, symbolic setting) serves the performative mobilization function rather than any epistemic synthesis function. Accessibility collapse (0.35) is moderate because alternative risk metrics (IPCC reports, nuclear threat assessments, pandemic preparedness indices) exist and are accessible, but the Clock's brand recognition creates a strong attentional gravity. Resistance (0.42) is moderate — critics challenge the Clock's methodology and strategic use, but the constraint persists because the beneficiaries (activists, Bulletin) have institutional inertia and media access that resisters lack.
 *
 * PERSPECTIVAL GAP:
 *   From the Bulletin editors' seat (agenda_setter, identity_locked), the Clock is a necessary coordination tool that adapts to new risks — the strategic communication is the point. From policy activists' seat (beneficiary, mobile), the Clock is a useful lever they would defend but could replace. From the analytical victim seats (epistemic credibility, public trust), the Clock is a credibility parasite that undermines the epistemic infrastructure it feeds on. The engine computes this divergence from the structural data — the authored claim (snare) reflects the analytical seat's reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Bulletin editors are the agenda setters — they control the setting process and their institutional identity is fused with the Clock (identity_locked exit). Policy activists and advocacy organizations are beneficiaries — they collect mobilization value without running the Clock (mobile exit, can switch symbols). Epistemic credibility, public trust, and future risk communication are victims — they bear the diffuse, long-horizon costs of credibility erosion (analytical seats, no exit). Competing expert bodies are excluded — they would object to a single metaphor overriding domain-specific judgments but have no formal role (constrained exit, can publish alternatives but cannot displace the Clock's media position). Risk governance scholars are observers — they analyze but do not participate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (communicating nuclear urgency when technical assessments failed) is contested — Bulletin editors say it persists; critics say domain-specific metrics have solved it. The constraint persists regardless because the Bulletin's institutional relevance now depends on the Clock's news-making function (mandatrophy: the mandate has outlived its function but the arrangement persists due to institutional identity fusion). The snare classification captures this: the coordination story is cover; persistence depends on suppressing the fact that the epistemic function has atrophied while the performative function expands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_vs_sincere_setting,
    'To what extent do Bulletin editors consciously set the Clock for policy impact versus believing their own expert-synthesis framing?',
    'Internal deliberation records, editorial correspondence, or retrospective interviews with past editors. Natural experiment: compare Clock movements to measurable indicator movements — systematic divergence suggests strategic setting.',
    'If conscious strategy, the snare classification is confirmed (coordination story is deliberate cover). If sincere belief with unconscious bias, the constraint may be a degraded rope (piton candidate) where the coordination function atrophied without deliberate extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_vs_sincere_setting, empirical, 'Whether the extraction is intentional or emergent from identity-locked belief.').

omega_variable(
    credibility_deficit_measurement,
    'Can the epistemic credibility cost of the Clock''s strategic use be quantified, or is it inherently unmeasurable?',
    'Longitudinal surveys of public trust in scientific institutions correlated with Clock media cycles; citation analysis of Clock vs. domain-specific metrics in policy documents; experimental studies of warning credibility after exposure to ''strategic'' vs. ''tracking'' metrics.',
    'If quantifiable and large, strengthens snare classification and supports mandated reform. If unmeasurable or small, weakens the victim claims and may support rope or piton reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_deficit_measurement, empirical, 'Measurability of the diffuse, long-horizon victim costs.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the doomsday_clock_metric kernel admit a single coherent framing, or are the three readings (performative_tool, objective_index, hybrid_legitimacy) structurally incommensurable framings of different constraints?',
    'Formal analysis of each reading''s epsilon, beneficiary/victim structure, and coordination function. If the readings decompose into different constraints with different structural properties (per epsilon-invariance), the kernel is a label conflation, not a single commitment with multiple readings.',
    'If incommensurable, each reading should be a separate constraint story (already done) and the kernel construct should be retired as analytically misleading. If commensurable, the kernel structure is valid and the readings are genuine interpretive variants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel_id represents one commitment with multiple readings or a colloquial label covering distinct constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dcm_pt_tr_t1947, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1947, 0.15).
narrative_ontology:measurement(dcm_pt_tr_t1960, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(dcm_pt_tr_t1984, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1984, 0.3).
narrative_ontology:measurement(dcm_pt_tr_t1991, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1991, 0.35).
narrative_ontology:measurement(dcm_pt_tr_t2007, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2007, 0.45).
narrative_ontology:measurement(dcm_pt_tr_t2015, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2015, 0.5).
narrative_ontology:measurement(dcm_pt_tr_t2020, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2020, 0.55).
narrative_ontology:measurement(dcm_pt_tr_t2023, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2023, 0.57).
narrative_ontology:measurement(dcm_pt_tr_t2024, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(dcm_pt_be_t1947, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1947, 0.25).
narrative_ontology:measurement(dcm_pt_be_t1960, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement(dcm_pt_be_t1984, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1984, 0.45).
narrative_ontology:measurement(dcm_pt_be_t1991, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1991, 0.5).
narrative_ontology:measurement(dcm_pt_be_t2007, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2007, 0.6).
narrative_ontology:measurement(dcm_pt_be_t2015, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(dcm_pt_be_t2020, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(dcm_pt_be_t2023, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2023, 0.71).
narrative_ontology:measurement(dcm_pt_be_t2024, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(dcm_pt_su_t1947, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1947, 0.2).
narrative_ontology:measurement(dcm_pt_su_t1960, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1960, 0.3).
narrative_ontology:measurement(dcm_pt_su_t1984, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1984, 0.45).
narrative_ontology:measurement(dcm_pt_su_t1991, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1991, 0.4).
narrative_ontology:measurement(dcm_pt_su_t2007, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2007, 0.5).
narrative_ontology:measurement(dcm_pt_su_t2015, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(dcm_pt_su_t2020, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(dcm_pt_su_t2023, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2023, 0.63).
narrative_ontology:measurement(dcm_pt_su_t2024, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__performative_tool_reading, 0.08).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__hybrid_legitimacy_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, nuclear_risk_communication_metrics).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, climate_change_urgency_framing).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, ai_existential_risk_metrics).

% DUAL FORMULATION NOTE:
% The doomsday_clock_metric kernel decomposes into three constraint stories: performative_tool_reading (this story, snare), objective_index_reading (mountain candidate — claims negligible extraction), and hybrid_legitimacy_reading (tangled_rope candidate — claims genuine but entangled coordination/extraction). The performative reading influences the hybrid reading by creating downstream pressure on its 'entanglement' claim. The objective reading is foreclosed by this reading's core premise (strategic setting vs. pure tracking). All three share the same referent (the Bulletin's annual Clock-setting practice) but author different epsilon values and beneficiary/victim structures per epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_metric__performative_tool_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
