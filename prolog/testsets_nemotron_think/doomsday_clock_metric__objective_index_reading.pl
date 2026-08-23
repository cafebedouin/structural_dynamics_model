% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__objective_index_reading, []).

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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock as Objective Risk Index (Expert Synthesis Reading)
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Doomsday Clock, maintained by the Bulletin of the Atomic Scientists
 *   since 1947, is read in this constraint story as an objective index
 *   tracking measurable existential risk levels through expert synthesis of
 *   empirical indicators. This reading treats the Clock's minute-hand
 *   settings as the output of a scientific assessment process — aggregating
 *   nuclear, climate, biological, and AI risk indicators into a single
 *   authoritative metric. The reading claims the Clock solves a genuine
 *   coordination problem: providing a unified, credible risk signal that
 *   coordinates global policy attention across disparate threat domains.
 *   Structurally, however, the expert monopoly on interpretation extracts
 *   authority from democratic accountability: the Bulletin's Science and
 *   Security Board sets the Clock without formal accountability to affected
 *   publics or elected representatives, and the 'objective index' framing
 *   suppresses contestation over the normative choices embedded in risk
 *   synthesis (weighting, threshold selection, horizon definition). The
 *   constraint has intensified over time: as the Clock expanded from
 *   nuclear-only to multi-threat, the expert synthesis grew more complex and
 *   less transparent, while the performative maintenance of objectivity
 *   (theater) increased to defend the authority claim against rising
 *   contestation from the performative_tool_reading and
 *   hybrid_legitimacy_reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.42).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.75).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock as Objective Risk Index (Expert Synthesis Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, 'f7ebc855-49e1-4c73-9036-02121a7cc3f0').
narrative_ontology:cs_kernel_codification('f7ebc855-49e1-4c73-9036-02121a7cc3f0', formalized).
narrative_ontology:cs_authority_grounding('f7ebc855-49e1-4c73-9036-02121a7cc3f0', expertise).
narrative_ontology:cs_interpretation_layer_present('f7ebc855-49e1-4c73-9036-02121a7cc3f0').
narrative_ontology:cs_reading_relation('f7ebc855-49e1-4c73-9036-02121a7cc3f0', doomsday_clock_metric__performative_tool_reading, forecloses).
narrative_ontology:cs_reading_relation('f7ebc855-49e1-4c73-9036-02121a7cc3f0', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('f7ebc855-49e1-4c73-9036-02121a7cc3f0', foundational, existential_risk_empirically_quantifiable).
narrative_ontology:cs_axiom_status(existential_risk_empirically_quantifiable, holdable).
narrative_ontology:cs_axiom_grounding('f7ebc855-49e1-4c73-9036-02121a7cc3f0', existential_risk_empirically_quantifiable, empirically_contingent).
narrative_ontology:cs_axiom('f7ebc855-49e1-4c73-9036-02121a7cc3f0', foundational, expert_synthesis_sufficient_for_legitimate_risk_governance).
narrative_ontology:cs_axiom_status(expert_synthesis_sufficient_for_legitimate_risk_governance, holdable).
narrative_ontology:cs_axiom_grounding('f7ebc855-49e1-4c73-9036-02121a7cc3f0', expert_synthesis_sufficient_for_legitimate_risk_governance, conventional).
narrative_ontology:cs_reference_frame('f7ebc855-49e1-4c73-9036-02121a7cc3f0', objective_risk_index_ideal).
narrative_ontology:cs_drift_state('f7ebc855-49e1-4c73-9036-02121a7cc3f0', contemporary_multi_threat_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f7ebc855-49e1-4c73-9036-02121a7cc3f0', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, scientific_expert_community).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, bulletin_atomic_scientists).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_public).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, policy_makers_excluded_from_interpretation).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__objective_index_reading, expert_synthesis_authority).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__objective_index_reading, empirical_risk_quantification_possible).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__objective_index_reading, unified_existential_risk_metric_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Bulletin's Science and Security Board (SASB) sets the Clock's minute-hand position through a closed expert deliberation process. They control the methodology, select the indicators, and authorize the annual statement. They justify the Clock as a scientific synthesis, collect institutional prestige and funding from its authority, and face minimal exit pressure — the Bulletin's brand is the Clock. Their situation is maintaining the Clock's relevance across expanding threat domains while defending the expert-monopoly framing against democratic and performative critiques.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, bulletin_atomic_scientists, agenda_setter,
    institutional, generational, arbitrage, global).

% The broader community of existential risk researchers (nuclear physicists, climate scientists, AI safety researchers, biosecurity experts) gains epistemic authority and policy access through the Clock's expert-synthesis framing. Their work is legitimated by inclusion in the Clock's indicator base; they can exit by publishing alternative assessments, but the Clock's focality makes their alternatives less visible. They benefit from the constraint's suppression of normative contestation — it validates their technical authority as sufficient for risk governance.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, scientific_expert_community, beneficiary,
    organized, biographical, mobile, global).

% Global publics bear the consequences of existential risk policy shaped by the Clock's authoritative signal, but have no formal role in the synthesis process. The 'objective index' framing treats risk valuation as a technical matter, excluding normative disagreements about acceptable risk levels, intergenerational equity, and distributional justice. Exit is trapped: the Clock's discourse shapes the Overton window for existential risk policy; opting out means accepting policy made on terms set by the expert monopoly. The extraction is the transfer of normative authority from democratic deliberation to expert synthesis.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, democratic_public, payer,
    powerless, generational, trapped, global).

% National governments and intergovernmental bodies (UN, IAEA, IPCC) consume the Clock as an authoritative input but have no seat at the setting table. They bear the cost of policy distortion when the Clock's expert synthesis misaligns with national risk assessments or democratic mandates. Their exit is constrained: they can commission parallel assessments (and do), but the Clock's media focality gives it disproportionate agenda-setting power. Some policy makers privately endorse the expert monopoly (reducing their own accountability), creating a secondary beneficiary dynamic.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, policy_makers_excluded_from_interpretation, payer,
    powerful, biographical, constrained, national).

% Alternative expert bodies (e.g., IPCC for climate, WHO for bio, national academies) produce competing risk assessments but are structurally excluded from the Clock's synthesis. They would challenge the Bulletin's methodology, indicator selection, and weighting if admitted. Their exclusion is actively maintained: the SASB controls participation, and the 'objective index' framing delegitimizes alternative syntheses as 'politicized' or 'less rigorous.' They are the absent voices the constraint's suppression targets.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, rival_expert_groups, excluded,
    organized, biographical, constrained, global).

% Academic researchers in science communication, science and technology studies, and risk governance analyze the Clock as a case study in expert authority, boundary work, and the politics of quantification. They neither collect nor pay; they document the structural dynamics this constraint story formalizes. Their analytical exit is absolute — they can study any framing.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, science_communication_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a synthesized, authoritative indicator of existential risk level that coordinates global attention and policy response across diverse threat domains (nuclear, climate, biological, AI) — solving the problem of fragmented, domain-specific risk signals that fail to capture systemic interactions.
% TRANSFER_FUNCTION: Moves interpretive authority over existential risk assessment from democratic deliberation and multi-institutional processes to a closed expert synthesis conducted by the Bulletin's Science and Security Board; the Clock's setting transfers legitimacy to the Bulletin's judgment, concentrating agenda-setting power in a non-accountable body.
% ABSENT_VOICES: Affected populations in the Global South (disproportionately exposed to climate and biological risks), future generations (bear long-horizon risks without representation), dissenting scientific voices who challenge the synthesis methodology (e.g., critics of nuclear risk quantification, AI timeline estimates), and civil society organizations advocating for participatory risk governance — all are structurally excluded from the SASB's closed process.
% DISAPPEARANCE_RATIONALE: If the Clock vanished overnight, existential risk governance would reorganize around competing authorities: the IPCC for climate, IAEA for nuclear, emerging AI governance bodies, and national security establishments. The focal point for multi-threat risk communication would fragment; media attention would diffuse; the Bulletin would lose its primary platform. The rearrangement would be contested — some actors would push for a replacement focal point, others would welcome the fragmentation as democratic opening.
% FOUNDING_PROBLEM: Post-WWII nuclear scientists (Rabinovich, Langsdorf, Szilard) needed a visible, authoritative signal to communicate nuclear danger to policymakers and the public when technical details were inaccessible and governmental secrecy blocked democratic debate. The Clock was built as a 'wake-up call' — a simple metaphor translating expert judgment into public urgency.
% FOUNDING_PROBLEM_CORROBORATION: Original Bulletin founders attested to the signaling purpose in historical records (Rabinovich 1984, Langsdorf 1969). Contemporary critics: Seth Baum (Global Catastrophic Risk Institute) argues the nuclear signaling problem is substantially solved and the Clock now serves expert legitimacy; Sharon Squassoni (former SASB chair) attests the problem has evolved to multi-threat complexity requiring the current form; science studies scholars (Jasanoff, Wynne) document the Clock's role in constructing expert authority rather than merely signaling danger.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__objective_index_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__objective_index_reading_tests).
:- end_tests(doomsday_clock_metric__objective_index_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.42) reflects the expert community's capture of interpretive authority over existential risk — a valuable resource in risk governance — while the coordination function (unified risk signal) remains real but partial. Suppression (0.75) is high because the 'objective index' framing actively marginalizes normative disagreement about risk valuation; alternatives like deliberative democratic risk assessment are excluded from the Clock's authoritative channel. Theater ratio (0.25) captures the growing gap between the claimed scientific objectivity and the actual normative judgments in multi-threat synthesis. Accessibility collapse (0.6) is moderate: alternative risk frameworks (IPCC, Global Risks Report, national assessments) exist but lack the Clock's symbolic focality. Resistance (0.4) reflects sustained contestation from the other two readings and from policy actors who reject expert monopoly. The temporal series shows extraction and suppression rising together as the Clock's scope expanded — a classic tangled_rope trajectory where coordination function atrophies relative to extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the scientific_expert_community seat (beneficiary), the constraint appears as a rope: a necessary coordination mechanism that solves the problem of fragmented risk signals. From the democratic_public seat (payer), it appears as a snare: an expert monopoly that extracts interpretive authority while suppressing normative contestation. The bulletin_atomic_scientists seat (agenda_setter) experiences it as a scaffold under strain — the founding nuclear-signaling problem is largely solved, but the multi-threat expansion lacks a clear sunset. The engine will compute these per-seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin's Science and Security Board (agenda_setter) and the broader scientific_expert_community (beneficiary) sit at the low-d end: they control the synthesis methodology, collect the authority rents, and face arbitrage-grade exit (they could leave the Bulletin but the institution persists). The democratic_public and policy_makers_excluded_from_interpretation (payers) sit at the high-d end: they bear the costs of excluded normative input and policy distortion, with trapped to constrained exit (they cannot easily exit the risk governance regime the Clock shapes). Rival_expert_groups (excluded) are structurally blocked from the synthesis process — their exclusion is the enforcement mechanism. Science_communication_scholars (observer) sit at analytical d=0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (nuclear danger signaling to policymakers/public) is contested: the original signaling function has partially succeeded (nuclear taboo established) but the Clock expanded to climate, bio, AI without a new mandate. The expert community benefits from the expanded scope (authority over broader risk domain), while the democratic public bears the cost of an unaccountable multi-threat index. The classification as tangled_rope (not snare) acknowledges the residual coordination value — the Clock still focuses attention — but the extraction/suppression profile shows the coordination function is no longer the primary driver of persistence. The mandate has atrophied; the constraint persists because the Bulletin extracts authority from maintaining it, and no actor has sufficient incentive to dismantle the focal point.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Doomsday Clock a single constraint with measurement-dependent classification, or three structurally distinct constraints (readings) sharing a label?',
    'Decompose the kernel into separate constraint stories per reading; if each yields stable ε and distinct beneficiary/victim structures across readings, the kernel is a label conflating multiple constraints.',
    'If three constraints, each gets independent classification; the objective_index_reading would be assessed on its own extraction/suppression profile without averaging over performative or hybrid readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel label ''Doomsday Clock'' covers one constraint or a constraint family.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of normative framing structural (institutional gatekeeping by the Bulletin''s Science and Security Board) or internalized (public and policymakers accept expert monopoly as appropriate)?',
    'Post-exclusion discourse analysis: if challenged normative framings reappear in alternative forums (UN policy processes, civil society reports), suppression is structural; if absent even in open forums, internalized component is significant.',
    'If internalized, effective suppression exceeds the structural measure — the constraint travels with agents after formal exit from the Clock''s discourse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in expert monopoly on risk interpretation.').

omega_variable(
    coordination_extraction_boundary,
    'Is the expert synthesis function genuinely necessary for existential risk coordination (irreducible complexity), or does it serve as a cover for expert authority capture?',
    'Counterfactual comparison: if a democratic deliberation process with expert input produced comparable risk rankings, the synthesis is not structurally necessary; if expert-only synthesis consistently outperforms, coordination function is genuine.',
    'If coordination is genuine, the constraint is tangled_rope; if synthesis is unnecessary cover, it trends toward snare (extraction riding a vestigial coordination story).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the expert monopoly on Clock-setting is functionally necessary or extractive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dcm_oir_tr_t0, doomsday_clock_metric__objective_index_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(dcm_oir_tr_t15, doomsday_clock_metric__objective_index_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(dcm_oir_tr_t30, doomsday_clock_metric__objective_index_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(dcm_oir_tr_t45, doomsday_clock_metric__objective_index_reading, theater_ratio, 45, 0.18).
narrative_ontology:measurement(dcm_oir_tr_t60, doomsday_clock_metric__objective_index_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(dcm_oir_tr_t77, doomsday_clock_metric__objective_index_reading, theater_ratio, 77, 0.25).

% Extraction over time
narrative_ontology:measurement(dcm_oir_be_t0, doomsday_clock_metric__objective_index_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(dcm_oir_be_t15, doomsday_clock_metric__objective_index_reading, base_extractiveness, 15, 0.2).
narrative_ontology:measurement(dcm_oir_be_t30, doomsday_clock_metric__objective_index_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(dcm_oir_be_t45, doomsday_clock_metric__objective_index_reading, base_extractiveness, 45, 0.35).
narrative_ontology:measurement(dcm_oir_be_t60, doomsday_clock_metric__objective_index_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(dcm_oir_be_t77, doomsday_clock_metric__objective_index_reading, base_extractiveness, 77, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(dcm_oir_su_t0, doomsday_clock_metric__objective_index_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(dcm_oir_su_t15, doomsday_clock_metric__objective_index_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(dcm_oir_su_t30, doomsday_clock_metric__objective_index_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(dcm_oir_su_t45, doomsday_clock_metric__objective_index_reading, suppression_requirement, 45, 0.65).
narrative_ontology:measurement(dcm_oir_su_t60, doomsday_clock_metric__objective_index_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(dcm_oir_su_t77, doomsday_clock_metric__objective_index_reading, suppression_requirement, 77, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, information_standard).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__objective_index_reading, 0.03).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, nuclear_risk_governance).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, climate_policy_framework).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, ai_governance).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, global_catastrophic_risk_prioritization).

% DUAL FORMULATION NOTE:
% This constraint (objective_index_reading) and its siblings (performative_tool_reading, hybrid_legitimacy_reading) form a constraint family decomposing the 'Doomsday Clock' label. Each has distinct ε, beneficiary/victim structure, and claimed_type. The objective_index_reading claims rope/tangled_rope with ε≈0.42; performative_tool_reading would claim snare with higher ε; hybrid_legitimacy_reading would claim tangled_rope with different beneficiary structure (entangled authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_metric__objective_index_reading, institutional, 0.1).
constraint_indexing:directionality_override(doomsday_clock_metric__objective_index_reading, organized, 0.2).
constraint_indexing:directionality_override(doomsday_clock_metric__objective_index_reading, powerless, 0.9).
constraint_indexing:directionality_override(doomsday_clock_metric__objective_index_reading, powerful, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
