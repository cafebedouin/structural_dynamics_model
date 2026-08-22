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
 *   constraint_id: doomsday_clock_metric__hybrid_legitimacy_reading
 *   human_readable: Doomsday Clock Metric (Hybrid Legitimacy Reading)
 *   domain: science_communication/existential_risk/normative_epistemology
 *
 * SUMMARY:
 *   The Doomsday Clock, maintained by the Bulletin of Atomic Scientists since
 *   1947, presents itself as an objective index of existential risk
 *   proximity, with the minute hand positioned by expert consensus. This
 *   hybrid-legitimacy reading rejects both the claim that the Clock is purely
 *   technical (objective_index_reading) and the claim that it is purely
 *   strategic (performative_tool_reading). Instead, it treats the Clock as a
 *   *deliberately ambiguous* coordination frame that fuses scientific
 *   judgment with normative stakes in a way that cannot be cleanly
 *   decomposed. The reading accepts the entanglement as legitimate—the
 *   Clock's power derives precisely from its refusal to draw a line between
 *   'what the risk is' and 'what we ought to do about it.' But this refusal
 *   creates a new extraction surface: the Bulletin collects authority by
 *   maintaining the ambiguity; policy communities and publics are locked into
 *   a frame they did not author and cannot modify without destroying the
 *   coordination benefit. The constraint is CLAIMED as tangled_rope
 *   (coordination + asymmetric extraction both real) because the hybrid
 *   framing genuinely coordinates action on x-risk while simultaneously
 *   foreclosing scrutiny of its normative foundations.
 *
 * KEY AGENTS:
 *   - Bulletin of Atomic Scientists: agenda-setter; maintains the annual setting process and defends the metaphor against epistemological challenge
 *   - Existential risk research community: beneficiary; gains sustained visibility and research funding through Clock salience
 *   - Policy stakeholders (governments, international organizations): payer; must decide on urgency under a framing they did not co-author
 *   - Mass publics: payer + beneficiary (dual); absorb affective labor but gain collective legibility of existential danger
 *   - Competing risk framers: excluded; alternative risk ontologies barred from the public discourse frame
 *   - Science epistemology observers: analytical seat; measure the entanglement of scientific judgment and normative stakes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, 0.58).
domain_priors:suppression_score(doomsday_clock_metric__hybrid_legitimacy_reading, 0.47).
domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__hybrid_legitimacy_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock Metric (Hybrid Legitimacy Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "science_communication/existential_risk/normative_epistemology").

domain_priors:requires_active_enforcement(doomsday_clock_metric__hybrid_legitimacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, '3d440ed5-317b-4e5a-b657-57eb248e0a81').
narrative_ontology:cs_kernel_codification('3d440ed5-317b-4e5a-b657-57eb248e0a81', fixed_text).
narrative_ontology:cs_authority_grounding('3d440ed5-317b-4e5a-b657-57eb248e0a81', expertise).
narrative_ontology:cs_interpretation_layer_present('3d440ed5-317b-4e5a-b657-57eb248e0a81').
narrative_ontology:cs_reading_relation('3d440ed5-317b-4e5a-b657-57eb248e0a81', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d440ed5-317b-4e5a-b657-57eb248e0a81', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_axiom('3d440ed5-317b-4e5a-b657-57eb248e0a81', foundational, science_and_norms_irreducibly_entangled).
narrative_ontology:cs_axiom_status(science_and_norms_irreducibly_entangled, holdable).
narrative_ontology:cs_axiom_grounding('3d440ed5-317b-4e5a-b657-57eb248e0a81', science_and_norms_irreducibly_entangled, deontological).
narrative_ontology:cs_axiom('3d440ed5-317b-4e5a-b657-57eb248e0a81', foundational, ambiguity_constitutive_not_defective).
narrative_ontology:cs_axiom_status(ambiguity_constitutive_not_defective, holdable).
narrative_ontology:cs_axiom_grounding('3d440ed5-317b-4e5a-b657-57eb248e0a81', ambiguity_constitutive_not_defective, instrumental).
narrative_ontology:cs_reference_frame('3d440ed5-317b-4e5a-b657-57eb248e0a81', unified_existential_risk_symbolism).
narrative_ontology:cs_drift_state('3d440ed5-317b-4e5a-b657-57eb248e0a81', contemporary_portfolio_risk_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3d440ed5-317b-4e5a-b657-57eb248e0a81', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_atomic_scientists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_research_community).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, policy_stakeholders_under_uncertainty).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, publics_absorbing_affective_labor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, publics_absorbing_affective_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the Clock's minute-to-midnight position annually; maintains the metaphor as the primary frame for existential risk discourse; defends the setting against both skeptical scientists and policy pressure; collects institutional legitimacy from being the canonical voice on doomsday proximity.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_atomic_scientists, agenda_setter,
    organized, generational, mobile, global).

% Gains sustained institutional attention, funding pipelines, and public discourse visibility through the Clock's cultural salience. The Clock transforms scattered risk estimates into a unified, memorable narrative that elevates existential risk as a domain deserving research and policy engagement. Constrained because leaving the Clock ecosystem would mean losing the coordination frame that makes existential risk legible.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_research_community, beneficiary,
    organized, generational, constrained, global).

% Must make decisions (nuclear policy, biosecurity, AI governance) under the Clock's framing, which collapses irreducible uncertainties into a single number. They carry the normative weight of the setting without transparent access to how it was derived or what trade-offs it embeds. Their exit is constrained because ignoring the Clock risks appearing reckless; engaging with it means accepting its framing.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, policy_stakeholders_under_uncertainty, payer,
    powerful, biographical, constrained, national).

% Encounter the Clock as an ambient cultural artifact signaling existential danger. They absorb affective load (anxiety, urgency, despair) from the framing without agency to challenge or revise it. The Clock coordinates global attention on existential risk—a genuine coordination benefit—while simultaneously locking them into interpretive frames they did not author and cannot modify.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, publics_absorbing_affective_labor, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__hybrid_legitimacy_reading, publics_absorbing_affective_labor, beneficiary).

% Represent alternative risk ontologies (probabilistic forecasting, scenario modeling, Bayesian frameworks) that are structurally excluded from the Clock's metaphorical apparatus. They could argue for decomposing the single number into risk classes, time horizons, and epistemic confidence bands, but the Clock's cultural dominance forecloses these alternatives in public discourse.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, competing_risk_framers, excluded,
    organized, generational, constrained, global).

% Analyzes the Clock as a case study in how scientific judgment becomes entangled with normative stakes; how objectivity claims mask value commitments; how a coordination frame can simultaneously enable action and foreclose scrutiny of its foundations.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, science_epistemology_community, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_atomic_scientists).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__hybrid_legitimacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates distributed, heterogeneous expert judgments about existential risk into a single unified symbol that makes planetary-scale danger collectively legible and actionable; enables policy communities to coordinate on the urgency of existential risk without requiring consensus on specific threat profiles.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual scientists and policy actors to the Bulletin's annual consensus process; transfers affective labor (anxiety, urgency management) from policy elites to mass publics; transfers credibility from narrow technical judgment to broad cultural authority.
% ABSENT_VOICES: Scientists who reject the Clock's numerical synthesis in favor of risk decomposition; publics who would prefer transparency about normative choices embedded in the setting; risk framers using non-metaphorical models (probability distributions, scenario hierarchies); communities whose risk profiles differ from the Clock's implicit global baseline.
% DISAPPEARANCE_RATIONALE: If the Clock vanished, existential risk discourse would fragment into competing technical vocabularies; policy coordination on the urgency of x-risk would lose its primary common symbol; the Bulletin would lose institutional prominence; publics would lose their primary ambient cue for existential danger. The arrangement would require rapid reconstruction in a different form.
% FOUNDING_PROBLEM: Existential risk (nuclear annihilation, engineered pathogens, AI misalignment, climate tipping points) is scientifically contested, geographically distributed across policy domains, and affectively overwhelming for ordinary publics. A single, memorable, annually-updatable symbol was needed to keep the threat visible and to coordinate action across disciplines and polities without requiring technical expertise in each threat domain.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin attests the founding problem is live and the Clock solves it. Policy communities (UN, IPCC-adjacent actors, US National Security Council) attest the Clock provides valuable focal point for risk prioritization. Scientists outside the Bulletin attest that the founding problem (lack of unified existential risk framing) persists but dispute whether the Clock's solution is scientifically defensible or merely culturally effective. Independent epistemologists attest the real founding problem is deeper: how to communicate irreducible uncertainty in a form that drives policy without falsely implying precision.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__hybrid_legitimacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__hybrid_legitimacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.58 at end) because the Clock genuinely coordinates action on x-risk (a real coordination benefit) but distributes costs asymmetrically: the Bulletin retains interpretive authority; policy communities must absorb the framing's normative weight without transparency; publics absorb affective labor. Theater is elevated (0.62) because the Clock's annual ritual—the deliberation, the announcement, the media cycle—performs scientific rationality while embedding normative choices that are treated as technical outputs. Suppression is moderate (0.47) because alternatives can exist at the margins, but the Clock's cultural dominance forecloses serious consideration of competing risk frameworks in mainstream discourse. The measurement series shows extractiveness and theater rising then stabilizing, suggesting the arrangement is consolidating its legitimacy through repetition rather than through deepening justification. The one-grid alignment across all metrics ensures temporal coherence.
 *
 * PERSPECTIVAL GAP:
 *   From the Bulletin's seat, the Clock is a genuine epistemic achievement: it translates impossible-to-compare risk domains into a single legible symbol and coordinates global attention on x-risk. From policy stakeholders' seats, the Clock is a constraint that forces them to treat irreducible uncertainty as if it were settled consensus. From publics' seats, the Clock is simultaneously a liberation (finally, a name for the ambient danger) and a trap (the only name available for it). The engine computes each seat's experienced type from the structural data: the Bulletin perceives rope (coordination benefit, symmetrical, self-directed); policy stakeholders perceive snare (constrained by a frame they cannot exit or modify); publics perceive a hybrid—the coordination is real but experienced as extraction because they cannot author it. This divergence is the measurement the corpus exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin seats as beneficiary (d near 0.2): they collect institutional authority, agenda-setting power, and sustained prominence by maintaining the Clock. The existential-risk research community also benefits (d near 0.3) but is constrained by dependence on the Clock's framing. Policy stakeholders are the primary targets (d near 0.75): they inherit the Clock's normative commitments—its implicit risk hierarchy, its temporal compression, its fusion of ought-with-is—without having authored them. Publics are the most deeply captured (d near 0.85): they are identity-locked to the Clock's framing because it has become the primary cultural artifact for collectively grasping existential danger; rejecting it means rejecting the very frame that makes the danger legible. The asymmetry is structural: the Bulletin and research communities benefit from the ambiguity; policy and publics pay the cost of living inside it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading rejects the mandatrophy hypothesis for the Clock. The founding problem is NOT dead: existential risk discourse genuinely needs a unified symbol, and the Clock provides one. What has changed is the *normative stakes* surrounding the founding problem. Early in the Clock's history (1947–1960s), the founding problem was real (how to keep nuclear risk visible after Hiroshima faded from immediate memory) and the Clock solved it cleanly. Now the founding problem has evolved: it is no longer 'how to communicate a single, well-understood threat' but 'how to communicate a portfolio of irreducible uncertainties (nuclear, biological, climate, AI) that do not map to a single probability distribution.' The Clock's response to this evolved problem is to maintain the metaphor even as its referent has become ambiguous. This is not mandatrophy (the function is not dead); it is *functional drift*—the arrangement persists by extending its scope in ways that increase extractiveness while retaining the original coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_ambiguity_as_legitimacy,
    'Is the Clock''s fusion of scientific judgment and normative stakes a legitimate epistemic stance, or a category error that should be decomposed?',
    'Extended engagement between the Bulletin''s epistemic defenders and independent philosophy-of-science critics; commissioned studies of whether policy stakeholders'' decisions differ depending on whether they encounter the Clock as ''objective measurement'' vs. ''strategic communication.''',
    'If ambiguity is legitimate, the constraint classification remains tangled_rope (genuine coordination + genuine extraction). If ambiguity is a defect, the constraint reclassifies to snare (the coordination benefit is illusory, the extraction is real). If the readings diverge in policy effects, the constraint family splits into separate constraints per epistemic frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_ambiguity_as_legitimacy, conceptual, 'Whether the Clock''s deliberate ambiguity is a feature or a defect of its epistemic legitimacy.').

omega_variable(
    normative_lock_mechanism,
    'Is the public''s identity-lock to the Clock primarily epistemic (the Clock is the only available frame for collective risk sense-making) or social (rejecting the Clock means social exclusion from x-risk discourse)?',
    'Ethnographic study of alternative risk framers'' actual access to policy channels; measurement of how thoroughly the Clock''s language dominates media, academic, and policy discourse on x-risk; post-Clock scenarios where an alternative frame (e.g., probabilistic risk distribution) becomes culturally dominant.',
    'If primarily epistemic, the lock is structural and may be functional—the Clock provides a genuine service by offering a unified frame. If primarily social, the lock is extractive—the Bulletin enforces its frame through institutional gatekeeping rather than epistemic necessity. These suggest different remedies (decompose the Clock''s components vs. open competing framers to policy access).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(normative_lock_mechanism, empirical, 'Whether the public is locked by lack of alternatives or by social enforcement of the Clock frame.').

omega_variable(
    theater_consolidation_driver,
    'Is the rising theater_ratio (0.54 → 0.62 → 0.62) a sign that the Clock''s annual ritual is becoming increasingly performative (the ceremony matters more than the number), or a sign that the coordination function is deepening (more stakeholders now participate in the ritual, not just observe it)?',
    'Detailed institutional analysis of the Bulletin''s decision process: track whether the annual setting is driven by new empirical evidence or by stakeholder pressure to move the clock; track whether policy actors'' decisions correlate with the Clock''s position or with the Bulletin''s public framing of the decision.',
    'If performative consolidation, theater_ratio rising suggests the arrangement is shifting from coordination to staged legitimacy (snare drift). If ritual deepening, theater_ratio rising is consistent with genuine coordination becoming more participatory. These suggest opposite remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_consolidation_driver, empirical, 'Whether the Clock''s annual ritual is becoming more performative or more participatory.').

omega_variable(
    sibling_reading_foreclosure,
    'Do the three readings—hybrid_legitimacy, objective_index, performative_tool—coexist as live options in the discourse, or does the hybrid reading''s dominance foreclose serious consideration of the siblings?',
    'Systematic analysis of publications, policy documents, and media coverage mentioning the Clock; count how often each reading explicitly appears; measure whether policy stakeholders encounter all three framings or only the hybrid framing.',
    'If siblings are foreclosed, the constraint on Clock interpretation is a snare (monopoly over the framing, suppression of alternatives). If siblings coexist at the margins, the constraint is a genuine tangled_rope (hybrid framing dominant but not hegemonic). This measures whether the Bulletin''s authority is active (enforced) or passive (default).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, empirical, 'Whether the hybrid reading monopolizes Clock interpretation or coexists with siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__hybrid_legitimacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doomsday_hybrid_tr_t0, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0, 0.54).
narrative_ontology:measurement(doomsday_hybrid_tr_t3, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 3, 0.57).
narrative_ontology:measurement(doomsday_hybrid_tr_t6, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 6, 0.6).
narrative_ontology:measurement(doomsday_hybrid_tr_t10, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 10, 0.63).
narrative_ontology:measurement(doomsday_hybrid_tr_t15, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 15, 0.65).
narrative_ontology:measurement(doomsday_hybrid_tr_t20, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 20, 0.64).
narrative_ontology:measurement(doomsday_hybrid_tr_t25, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 25, 0.62).

% Extraction over time
narrative_ontology:measurement(doomsday_hybrid_be_t0, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(doomsday_hybrid_be_t3, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(doomsday_hybrid_be_t6, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(doomsday_hybrid_be_t10, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(doomsday_hybrid_be_t15, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(doomsday_hybrid_be_t20, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(doomsday_hybrid_be_t25, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(doomsday_hybrid_su_t0, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0, 0.41).
narrative_ontology:measurement(doomsday_hybrid_su_t3, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 3, 0.43).
narrative_ontology:measurement(doomsday_hybrid_su_t6, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(doomsday_hybrid_su_t10, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(doomsday_hybrid_su_t15, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement(doomsday_hybrid_su_t20, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(doomsday_hybrid_su_t25, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 25, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__hybrid_legitimacy_reading, information_standard).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__hybrid_legitimacy_reading, 0.12).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_policy_coordination).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, x_risk_research_funding_allocation).

% DUAL FORMULATION NOTE:
% This constraint family (doomsday_clock_metric kernel + three readings) represents a decomposition per the ε-invariance principle. The three readings are structurally distinct constraints with different ε values, beneficiary/victim structures, and classifications. The hybrid_legitimacy_reading accepts the entanglement of science and norms as constitutive (moderate extraction, genuine coordination). The objective_index_reading treats the entanglement as a defect requiring epistemological decomposition (lower extraction, higher theater as defect). The performative_tool_reading treats the entanglement as instrumental for policy impact (higher extraction, explicit acceptance of performance). Each reading should be authored separately; they are linked here to enable cross-reading comparison and to document the kernel contest structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_metric__hybrid_legitimacy_reading, organized, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
