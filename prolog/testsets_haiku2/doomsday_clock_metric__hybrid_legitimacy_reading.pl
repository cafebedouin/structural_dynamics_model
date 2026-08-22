% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__hybrid_legitimacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__hybrid_legitimacy_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: doomsday_clock_metric__hybrid_legitimacy_reading
 *   human_readable: Doomsday Clock Metric (Hybrid Legitimacy Reading)
 *   domain: science_communication/risk_governance/epistemology
 *
 * SUMMARY:
 *   The Doomsday Clock, maintained by the Bulletin of the Atomic Scientists,
 *   is a communications tool that translates complex assessment of
 *   existential risk (nuclear, biological, climate, AI, synthetic biology)
 *   into a single position on a clock face, where midnight represents
 *   civilization-ending catastrophe. The constraint story here is the
 *   institutional practice of setting the Clock—the decision process, the
 *   framing narrative, the public communication strategy, and the implicit
 *   normative choices embedded in the measurement. This hybrid legitimacy
 *   reading interprets the Clock as irreducibly entangling scientific
 *   judgment (what are the observable indicators of existential risk? how
 *   confident are we?) with normative judgment (what level of risk is
 *   unacceptable? how much do we want to provoke action?) and institutional
 *   legitimacy (who has the authority to speak for humanity about existential
 *   stakes? what narrative authority does the Bulletin wield?). The reading
 *   holds that this entanglement is NOT a flaw to be corrected—it is the
 *   structure through which the Clock acquires its salience and its power.
 *   The cost is that the Clock cannot clearly separate measurement from
 *   valuation, making it neither purely scientific nor purely political, and
 *   leaving policy actors and the scientific community in a state of
 *   productive but unstable ambiguity about what they are deferring to.
 *
 * KEY AGENTS:
 *   - Bulletin of the Atomic Scientists (institutional agenda-setter): sets the Clock annually; frames the narrative around the current threat landscape; exercises editorial control over which indicators count as existential-risk signals; collects institutional authority and media salience.
 *   - Scientific experts (contributors, internal working groups): provide empirical synthesis on nuclear, biological, climate, AI threat trajectories; navigate the tension between 'what the data says' and 'what narrative the institution needs'; experience the constraint as requiring translation of uncertainty into categorical time judgments.
 *   - Policy makers and state actors (users, dependent parties): depend on the Clock as a focal point for existential risk discussion; face pressure to respond to Clock movements as though they were unambiguous signals; lack independent assessment capacity for the composite threat model.
 *   - Scientific credibility system (distributed, vulnerable): carries costs when measurement and normative judgment are entangled; erosion happens across the existential-risk domain when Clock updates appear to settle what is actually unsettled.
 *   - Competing risk-assessment institutions (excluded): institutional alternatives (IPCC for climate, national security establishments for military threats) exist but lack the Bulletin's unified institutional voice; their exclusion is maintained by the Clock's singular narrative authority.
 *   - Analytical observers (e.g., philosophers of science, epistemologists, risk scholars): see the constraint as a case study in how institutions manage irreducible value/fact entanglement; position not yet captured by the Clock's authority structure but attentive to its legitimacy claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, 0.58).
domain_priors:suppression_score(doomsday_clock_metric__hybrid_legitimacy_reading, 0.42).
domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__hybrid_legitimacy_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock Metric (Hybrid Legitimacy Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "science_communication/risk_governance/epistemology").

domain_priors:requires_active_enforcement(doomsday_clock_metric__hybrid_legitimacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, '1dbea203-3147-4617-a397-4c954b0ffbe8').
narrative_ontology:cs_kernel_codification('1dbea203-3147-4617-a397-4c954b0ffbe8', distributed).
narrative_ontology:cs_authority_grounding('1dbea203-3147-4617-a397-4c954b0ffbe8', extraction).
narrative_ontology:cs_reading_relation('1dbea203-3147-4617-a397-4c954b0ffbe8', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('1dbea203-3147-4617-a397-4c954b0ffbe8', doomsday_clock_metric__performative_tool_reading, influences).
narrative_ontology:cs_axiom('1dbea203-3147-4617-a397-4c954b0ffbe8', foundational, irreducible_measurement_value_entanglement).
narrative_ontology:cs_axiom_status(irreducible_measurement_value_entanglement, holdable).
narrative_ontology:cs_axiom_grounding('1dbea203-3147-4617-a397-4c954b0ffbe8', irreducible_measurement_value_entanglement, deontological).
narrative_ontology:cs_axiom('1dbea203-3147-4617-a397-4c954b0ffbe8', secondary, institutional_epistemic_stewardship).
narrative_ontology:cs_axiom_status(institutional_epistemic_stewardship, holdable).
narrative_ontology:cs_axiom_grounding('1dbea203-3147-4617-a397-4c954b0ffbe8', institutional_epistemic_stewardship, conventional).
narrative_ontology:cs_reference_frame('1dbea203-3147-4617-a397-4c954b0ffbe8', deliberate_ambiguity_as_legitimacy_source).
narrative_ontology:cs_drift_state('1dbea203-3147-4617-a397-4c954b0ffbe8', contemporary_existential_risk_governance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1dbea203-3147-4617-a397-4c954b0ffbe8', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_atomic_scientists_institution).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, policy_makers_dependent_on_clarity).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, scientific_credibility_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, scientific_experts_working_groups).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, scientific_experts_working_groups).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, policy_makers_and_state_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the Clock annually by convening scientific experts, synthesizing existential risk assessment across nuclear, biological, climate, AI, synthetic biology domains, and announcing a new position and narrative. Controls which indicators count, how they are weighted, what threshold triggers movement, and what the public-facing interpretation is. Collects institutional authority, media salience, policy deference. Can exit by reformatting the Clock (more explicit probabilistic model), separating measurement from normative framing, or dissolving the institution entirely; instead, chose to expand the Clock's mandate to include additional existential risks.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_atomic_scientists, agenda_setter,
    institutional, generational, mobile, global).

% Provide empirical synthesis on threat trajectories in their domains (nuclear, biological, climate, AI research). Navigate the tension between 'what the data shows' (which is uncertain and domain-specific) and 'what the institution needs' (which is a composite judgment about existential stakes). Bear credibility risk when the Clock conflates their careful uncertainty quantification with normative judgments about acceptable risk. Cannot easily exit because the Bulletin's institutional platform amplifies existential risk concerns that might otherwise be ignored by policy makers; but relying on the Clock compromises their individual field's epistemic integrity.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, scientific_experts_working_groups, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__hybrid_legitimacy_reading, scientific_experts_working_groups, beneficiary).

% Depend on the Clock as a focal point for existential risk discourse and policy response. Face pressure to act on Clock movements as though they were unambiguous risk signals, even though the actual meaning of the movement (did the threat change, or did the Bulletin's normative judgment shift, or did the framing narrative shift?) is unclear. Lack independent assessment capacity for composite existential risk and rely on institutional deference. Can theoretically ignore the Clock, but doing so means abandoning a major coordinating signal and appearing to ignore existential risk to their publics.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, policy_makers_and_state_actors, payer,
    organized, biographical, constrained, global).

% Carries distributed costs when scientific institutions entangle measurement with normative judgment. Erosion happens when the Clock appears to settle what is actually unsettled, and other institutions must spend credibility defending the distinction between 'what we measured' and 'what we judge acceptable.' The system cannot directly exit the constraint; its credibility is spent defending itself against the Clock's framing.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, scientific_credibility_system, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_non_agent(doomsday_clock_metric__hybrid_legitimacy_reading, scientific_credibility_system).

% Alternative institutional voices (IPCC for climate, national security establishments for military/biosecurity threats, academic existential risk research communities) exist but lack the Bulletin's unified narrative platform and public salience. Their exclusion from the Clock's authority structure is maintained by the Clock's singular institutional voice and media attention. They would argue for domain-specific assessment and contestation of the Clock's aggregation, but the Clock's communication power overshadows their alternatives.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, competing_risk_assessment_institutions, excluded,
    institutional, generational, trapped, global).

% Philosophers of science, risk scholars, epistemologists, and science communication researchers attend to the Clock as a case study in how institutions manage irreducible entanglement of measurement and value. They are not yet captured by the Clock's authority structure, but they are attentive to its legitimacy claims and the structural instability of maintaining deliberate ambiguity.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates complex, uncertain, domain-specific existential risk assessments (nuclear, biological, climate, AI) into a single temporal narrative (time to midnight) that allows policy makers, scientific institutions, and publics to coordinate collective attention and discourse around composite catastrophic risks. Solves the coordination problem of 'how do we talk about existential risk when our individual fields cannot speak with unified voice?'
% TRANSFER_FUNCTION: Transfers institutional authority and policy salience FROM dispersed scientific communities and independent risk assessment institutions TO the Bulletin of the Atomic Scientists. Moves policy makers' capacity to set independent existential risk assessment toward deference to the Bulletin's composite judgment. Moves credibility of individual scientific fields INTO the Clock's aggregated narrative, where normative judgments about acceptable risk are embedded in what appears to be pure measurement.
% ABSENT_VOICES: Domain-specific scientific communities (e.g., IPCC climate scientists, biosecurity researchers, AI safety researchers) would argue that the Clock's aggregation obscures domain-specific uncertainty and conflates risks that operate on different timescales and with different mitigation pathways. Philosophers of science would argue that the Clock's epistemic ambiguity is maintained because making it explicit would expose the normative choices embedded in the measurement frame. Skeptics of existential risk as a legitimate policy category would argue that the Clock amplifies panic about low-probability, high-impact scenarios without adequate grounding in evidence. These voices are structurally excluded because the Clock's institutional authority and media salience allow it to frame the entire existential risk conversation.
% DISAPPEARANCE_RATIONALE: If the Doomsday Clock disappeared, existential risk governance would reorganize around multiple institutional voices and domain-specific assessments. Policy makers would lose a singular focal point and would need to construct coherent existential risk strategy from IPCC reports, national security assessments, academic existential risk research, and scattered media narratives—more fragmented, less actionable, but more epistemically transparent about which judgments are measurement and which are normative. The scientific credibility system would recover some capacity to defend measurement against normative judgment. The Bulletin would lose its institutional centrality in existential risk discourse. Alternative institutional voices would gain traction.
% FOUNDING_PROBLEM: How do you communicate existential risk urgently to policy makers and publics when individual scientific domains (nuclear weapons, biological threats, climate) each have their own assessment communities and cannot speak with unified voice? How do you avoid the risk that fragmented warnings are ignored because there is no clear institutional authority commanding attention?
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin attests the founding problem is live and ongoing: existential risks remain dispersed across domains, institutions lack unified authority, policy makers need a focal point. Climate scientists and biosecurity researchers attest that the Clock's expansion to include their domains CREATES a coordination problem rather than solving it: their fields have better-developed institutional assessment mechanisms (IPCC, professional biosecurity organizations) that are obscured by being aggregated into the Clock's single narrative. Policy scholars attest that the Clock functions as a discourse focal point but question whether policy is materially improved by using the Clock's movements as decision anchors. Academic existential risk researchers attest the founding problem is partly addressed (there is now unified discourse around existential risk) but at the cost of conflating measurement with normative judgment in ways that complicate their own field's epistemic work. No outside corroborating institution attests that the founding problem remains live in its original form—the attest is mostly about whether the Clock is the right solution.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__hybrid_legitimacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__hybrid_legitimacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(doomsday_clock_metric__hybrid_legitimacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.58 at interval end) because the Clock extraction mechanism is not primarily coercive—it is epistemic authority capture. The Bulletin extracts institutional power by conflating its composite normative judgment about acceptable risk with scientific expertise. The rising trajectory (0.38 → 0.58) reflects growing tension: as existential risks become more salient in policy discourse, the Clock's authority expands, but so does pressure to justify how the setting was chosen, which exposes the normative judgment embedded in the 'scientific' measurement. Suppression is moderate (0.42) and stable: the constraint does not rely on preventing exit or alternatives (policy makers can ignore the Clock, scientists can publish independent assessments), but it does suppress debate about the legitimacy of the entanglement itself—anyone who says 'this is partly politics, partly science' is framed as undermining existential risk governance or playing ideological games. Theater is high and rising (0.32 → 0.61): increasingly, the Clock-setting process is performative—media events, narrative framing about 'how close to midnight we are,' symbolic communication about collective stakes—while the scientific content is real but secondary. Accessibility collapse is low (0.38): alternatives persist (IPCC, national security assessments, academic existential risk research), and the Bulletin's judgment is contestable; the Clock does not collapse alternatives so much as it overshadows them through institutional salience. Resistance is high (0.71): significant pushback from scientific institutions, policy scholars, and climate scientists who object to the Clock's treatment of climate as one component among many ('it's not the same as nuclear weapons'), philosophers questioning whether composite existential risk can be meaningfully aggregated, and policy makers who worry the Clock creates false confidence in their understanding of existential threat.
 *
 * PERSPECTIVAL GAP:
 *   From the Bulletin's seat, the Clock is a coordination mechanism solving the hard problem of communicating urgency about composite existential risks to distracted policy makers and publics. The institution is stewarding a collective knowledge project and deserves trust and latitude because the alternative (fragmented, incoherent, privately-captured existential risk narratives) is worse. From the policy maker's seat, the Clock provides a focal point that dramatically simplifies decision-making under uncertainty—they can point to an authoritative assessment and act on it without mastering the underlying science. From the scientific community's seat, the Clock is epistemically ambiguous: it appears to settle questions (how much risk is there really?) that the actual research shows are unsettled, and it attributes authority to the Bulletin to make these judgments in a way that complicates the work of individual research teams trying to communicate the actual state of their fields. From the excluded institutional competitors' seat (IPCC, national security establishments), the Clock is a monopoly on existential risk discourse that prevents alternative institutional voices from gaining traction. From the analytical observer's seat, the Clock is fascinating precisely because it cannot be made to choose between 'is this science or values?'—it works (maintains its salience and power) by refusing to choose, by maintaining deliberate ambiguity about the ground of its legitimacy. The engine should compute these divergences from the structural data: the Bulletin's d and the policy maker's d and the scientific community's d should all differ, and the computed types should split—some seats may experience it as coordination, others as extraction, others as a mix.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin sits at d near the beneficiary end (d ≈ 0.15–0.25): it collects institutional authority, media attention, and policy salience; it sets and administers the constraint; it exercises editorial judgment about which risks count; it has exit options (could publish separate assessments, could fold the Clock, could shift to a less visible format). Policy makers and scientific credibility systems sit near the target end (d ≈ 0.75–0.85): they depend on the Clock's judgment for discourse framing; they bear the cost of ambiguity (policy must respond to movements that may not warrant response; scientists must defend their field's credibility when the Clock conflates judgment with measurement); they have constrained exit (ignoring the Clock means ignoring a major coordinating signal, which is costly). The constraint is asymmetric extraction dressed in coordination language: the Bulletin genuinely coordinates existential risk discourse (solves the problem of how to aggregate diverse threats into a tractable policy signal), but it does this by capturing epistemic authority (making its composite judgment appear more settled and more scientific than it is). This is the structure of tangled_rope: real coordination function + asymmetric extraction + active enforcement (the Bulletin must continuously re-justify the Clock's methodology to suppress questions about legitimacy).
 *
 * MANDATROPHY ANALYSIS:
 *   The Clock's founding problem was to provide a clear, memorable, authoritative signal about nuclear weapons risk to drive policy action during the Cold War. That founding problem is substantially dead: nuclear weapons policy no longer moves primarily on public concern about existential risk (it is driven by strategic doctrine, arms control treaties, deterrence theory). The Clock persists by expanding to include biological, climate, and AI risks—it has found a new mandate in the existential-risk governance domain. This is mandatrophy in motion: the original function (nuclear weapons policy mobilization) is atrophied, replaced by a new function (existential risk discourse coordination). The tangled_rope classification holds because the new function is real and valuable (existential risk governance IS harder and more collective-action-prone than nuclear weapons policy), but the institutional persistence mechanism is partly theatrical—the Clock's salience now depends on media narrative about 'how close to midnight' rather than on direct policy impact on weapons decisions. The constraint does not resolve into pure piton (theaterized inertia) because the coordination function is live and valued; but the rising theater_ratio (0.32 → 0.61) indicates the institution is doing more narrative work to sustain the Clock's authority than it did when the founding problem was directly connected to policy outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_legitimacy_ambiguity,
    'Does the Clock''s legitimacy rest fundamentally on scientific measurement (objective index reading), strategic communication impact (performative tool reading), or irreducible entanglement of both with normative judgment (this hybrid reading)?',
    'Discourse analysis of BAS internal justification across decades; interviews with clock-setting participants about how they weigh empirical data vs. communication intent; comparison of stated scientific criteria with actual setting decisions.',
    'Different answers imply different trust bases: if objective, defer to expert synthesis; if performative, demand explicit accountability for persuasion intent; if hybrid, requires acknowledging irreducible legitimacy ambiguity and building institutional transparency around it. Classification could shift from tangled_rope (if hybrid) to rope (if genuinely coordinated integration) or snare (if performative intent is concealed as objectivity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_legitimacy_ambiguity, conceptual, 'Whether Clock legitimacy grounds in measurement, communication strategy, or inseparable entanglement of both.').

omega_variable(
    beneficiary_identification_opacity,
    'Who structurally benefits from maintaining the Clock''s ambiguous epistemic status? The Bulletin as institutional guardian? The scientific community via amplified policy salience? Policy actors who want a focal point for existential risk governance?',
    'Comparative analysis of how each reading''s adoption would redistribute institutional authority and resource flows; historical trace of institutional interests shaping Clock statements; interviews with policy-maker dependence on the Clock as decision anchor.',
    'If the Bulletin benefits from ambiguity itself (preserves institutional autonomy and centrality), the constraint is snare-adjacent (extraction through epistemic control). If beneficiary is genuinely diffuse (collective existential risk coordination), the tangled_rope frame holds. If no clear beneficiary exists and the constraint persists through inertia alone, it becomes piton-shaped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_opacity, empirical, 'Who structurally profits from the Clock''s ambiguous legitimacy status.').

omega_variable(
    scientific_credibility_cost,
    'Does the hybrid entanglement (normative judgment + scientific judgment) systematically erode scientific credibility by muddying the distinction between what is measured and what is valued?',
    'Longitudinal study of scientific community trust in existential risk expertise; comparison of credibility trajectories for prediction instruments that explicitly separate measurement from normative framing vs. those that entangle them; post-Clock-update discourse among policymakers about uncertainty vs. recommendation clarity.',
    'If erosion is substantial and systematic, the constraint extracts credibility from the scientific system itself (shifting snare indicators). If erosion is localized to existential risk domain and accepted as the price of policy salience, it remains tangled_rope. If the Bulletin''s institutional standing actively compensates for the erosion, the rope interpretation strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scientific_credibility_cost, empirical, 'Cost to scientific credibility from entanglement of measurement and normative judgment in the Clock.').

omega_variable(
    policy_clarity_capture,
    'Does the Clock function as coordination (providing a focal point for otherwise-fragmented existential risk governance) or extraction (capturing policy discourse by appearing to settle what is actually unsettled)?',
    'Case studies of policy decisions materially changed by Clock movements; interviews with policy makers about whether they treat Clock updates as evidence or as aggregated institutional judgment; reconstruction of counterfactual policy paths if Clock had not existed or had moved differently.',
    'If the Clock systematically over-indexes policy response relative to its informational content, it is extractive capture. If policy decisions reflect the Clock''s aggregation rather than depending on it, it is coordination. The measurement series'' rising theater_ratio (0.45 → 0.61) suggests growing performative component, which would push toward capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_clarity_capture, empirical, 'Whether Clock functions as coordination focal point or as policy-discourse capture mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__hybrid_legitimacy_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(doom_tr_t8, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(doom_tr_t16, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(doom_tr_t24, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 24, 0.54).
narrative_ontology:measurement(doom_tr_t32, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 32, 0.61).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(doom_be_t8, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(doom_be_t16, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(doom_be_t24, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(doom_be_t32, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 32, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(doom_su_t8, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(doom_su_t16, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(doom_su_t24, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(doom_su_t32, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 32, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__hybrid_legitimacy_reading, information_standard).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__hybrid_legitimacy_reading, 0.12).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_policy_discourse).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, scientific_credibility_system_in_risk_domain).

% DUAL FORMULATION NOTE:
% The doomsday_clock_metric kernel decomposes into three structurally distinct constraints, each grounded in a different reading of the Clock's legitimacy. The objective_index_reading treats the Clock as measurement; the performative_tool_reading treats it as strategic communication; the hybrid_legitimacy_reading (this constraint) treats the entanglement itself as the source of authority. These are not three observations of the same constraint—they have different epsilon values, different beneficiary structures, different types, and different classifications across stakeholder seats. They are linked by network.affects_constraints to trace how shifts in any one reading's institutional standing would propagate to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
