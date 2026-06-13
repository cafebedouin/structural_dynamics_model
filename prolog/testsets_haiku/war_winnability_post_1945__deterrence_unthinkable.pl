% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__deterrence_unthinkable, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Nuclear Deterrence: War Winnability Eliminated
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel
 *   'war_winnability_post_1945'. The deterrence_unthinkable reading asserts
 *   that nuclear weapons made great-power total war CATEGORICALLY UNWINNABLE
 *   — winnability exits the reachable strategic space entirely. Under this
 *   reading, planning for victory becomes logically incoherent, not merely
 *   difficult. Military establishments face a permanent mission inversion:
 *   from conquest-and-defense to prevention-and-stabilization. This creates a
 *   structural asymmetry: civilian populations benefit from the impossibility
 *   of war; military institutions suffer from the incoherence of their core
 *   function. Two sibling readings contest this framing:
 *   countervailing_thinkable argues limited victory remains possible through
 *   selective targeting; rhetorical_contraction argues winnability remained
 *   operationally planned but became unsayable in public discourse. This
 *   story models only the deterrence_unthinkable reading.
 *
 * KEY AGENTS:
 *   - Civilian populations (great powers): protected from existential war by the constraint; trapped in the protection; beneficiary
 *   - Military establishments: identity-locked to the incoherence (institutional purpose inverted from victory to prevention); payer
 *   - Strategic planners/analysts: navigate the paradox of planning unwinnable wars; constrained observer seats
 *   - Arms control regimes: derive legitimacy from the constraint; institutional beneficiaries
 *   - Political leaders (great powers): operate under the hard ceiling the constraint imposes; agenda-setters constrained by logic
 *   - Nuclear-armed non-state actors: excluded from the constraint's mutual-vulnerability logic; would contest the winnability closure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.22).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.18).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.22).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, mountain).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Nuclear Deterrence: War Winnability Eliminated").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic_studies/international_relations").

domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, '4429759a-339c-499b-816e-7e09c9415f36').
narrative_ontology:cs_kernel_codification('4429759a-339c-499b-816e-7e09c9415f36', fixed_text).
narrative_ontology:cs_authority_grounding('4429759a-339c-499b-816e-7e09c9415f36', expertise).
narrative_ontology:cs_interpretation_layer_present('4429759a-339c-499b-816e-7e09c9415f36').
narrative_ontology:cs_reading_relation('4429759a-339c-499b-816e-7e09c9415f36', war_winnability_post_1945__countervailing_thinkable, coexists_with).
narrative_ontology:cs_reading_relation('4429759a-339c-499b-816e-7e09c9415f36', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('4429759a-339c-499b-816e-7e09c9415f36', foundational, mutual_vulnerability_forecloses_victory).
narrative_ontology:cs_axiom_status(mutual_vulnerability_forecloses_victory, holdable).
narrative_ontology:cs_axiom_grounding('4429759a-339c-499b-816e-7e09c9415f36', mutual_vulnerability_forecloses_victory, empirically_contingent).
narrative_ontology:cs_axiom('4429759a-339c-499b-816e-7e09c9415f36', foundational, war_planning_must_assume_unwinnable_premise).
narrative_ontology:cs_axiom_status(war_planning_must_assume_unwinnable_premise, holdable).
narrative_ontology:cs_axiom_grounding('4429759a-339c-499b-816e-7e09c9415f36', war_planning_must_assume_unwinnable_premise, deontological).
narrative_ontology:cs_reference_frame('4429759a-339c-499b-816e-7e09c9415f36', victory_through_military_strength).
narrative_ontology:cs_drift_state('4429759a-339c-499b-816e-7e09c9415f36', post_nuclear_capability_stabilization, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('4429759a-339c-499b-816e-7e09c9415f36', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, species_survival).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations_great_powers).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, arms_control_verification_regimes).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments_great_powers).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__deterrence_unthinkable, mutually_assured_destruction_logic).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__deterrence_unthinkable, rational_deterrence_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are protected from great-power total war by the constraint that makes victory incoherent. They cannot opt out of the protection (trapped in the affected geography), and they cannot renegotiate the terms. The constraint provides a real benefit: absence of the nuclear taboo would make great-power war thinkable again and existential risk would return to the planning table.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations_great_powers, beneficiary,
    powerless, generational, trapped, global).

% Experience incoherence in their core function: military institutions exist to win wars and project power; the constraint makes this mission logically impossible at the strategic level. Doctrine pivots to deterrence (preventing war rather than winning it), which inverts the professional identity. Exit would require abandoning the institutional identity itself or acknowledging that victory is no longer the metric that matters.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments_great_powers, payer,
    institutional, civilizational, identity_locked, global).

% Navigate the paradox of planning for a war they must simultaneously argue cannot be won. They develop counterfactual scenarios (limited nuclear war, counterforce strategies) to preserve analytical coherence, but the underlying constraint — mutual vulnerability — hollows the victory premise. Their work is simultaneously essential (deterrence credibility requires planning) and performative (the plans cannot actually succeed).
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, strategic_planners_military_analysts, observer,
    organized, biographical, constrained, global).

% Are excluded from the constraint's logic because the constraint depends on great-power mutual vulnerability and rationality. Non-state actors with nuclear weapons (or potential future state actors outside the great-power club) do not fully internalize the constraint; they would object to the framing that winnability is eliminated, but they are not seated in the high-level strategic conversation.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, nuclear_armed_non_state_actors, excluded,
    moderate, biographical, constrained, regional).

% The constraint vindicates norms against total war and civilian targeting by making those norms strategically necessary rather than merely moral. The laws of war survive not because they are right but because they are the only frame that preserves any coherence in military planning under mutual annihilation.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, international_humanitarian_law_tradition, beneficiary,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(war_winnability_post_1945__deterrence_unthinkable, international_humanitarian_law_tradition).

% Derive legitimacy and resource allocation from the constraint's logic. Non-Proliferation Treaty, START frameworks, verification agencies — all rest on the premise that nuclear weapons are too dangerous to expand or use, which the constraint makes true in a way pre-1945 weapons never were. Their mission (preventing escalation and ensuring mutual stability) is justified by and dependent on the constraint.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, arms_control_verification_regimes, beneficiary,
    institutional, generational, mobile, global).

% Set policy within the frame the constraint establishes: war with nuclear-armed peers is removed from the option set. They are constrained by the impossibility of victory, which forces political and diplomatic solutions instead. Their power is real, but it operates under a hard ceiling — they cannot wage great-power total war even if they wanted to win it, because winning has become logically impossible.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, political_leaders_great_powers, agenda_setter,
    powerful, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared recognition that mutual vulnerability makes war between nuclear-armed great powers unwinnable, thereby aligning all great-power strategic planning toward war prevention rather than victory. Creates a coordination problem: both sides must credibly commit to non-initiation, which requires stable deterrence postures and mutual vulnerability.
% TRANSFER_FUNCTION: Transfers the cost of great-power conflict from potential war (with unknown casualties) to sustained deterrence operations (armed vigilance, verification regimes, strategic instability management). The constraint routes resources away from offensive capability development and toward defensive/verification infrastructure, and away from war-fighting doctrine toward deterrence stability.
% ABSENT_VOICES: Nuclear-armed non-state actors and threshold nuclear states would contest the premise that winnability is eliminated — they retain hope for decisive action or limited nuclear war. Rising powers outside the original great-power club have historically questioned whether mutual vulnerability truly removes their options. These voices are excluded from the highest-level strategic conversation but would argue for reopening the winnability question if given influence.
% DISAPPEARANCE_RATIONALE: If the constraint vanished (if nuclear weapons became winnable again or ceased to exist), great-power military planning would revert to victory-seeking. Wars would become thinkable as achievable objectives. The entire architecture of non-proliferation, arms control, and deterrence stability would collapse; nations would race to develop decisive capabilities. Great-power conflict would shift from the prevention logic (where we live) back to the conquest-and-defense logic of the pre-1945 era.
% FOUNDING_PROBLEM: After Hiroshima and Nagasaki, military planners faced a new reality: nuclear weapons were too destructive to use and too powerful to ignore. The founding problem was: how can military institutions maintain a deterrent posture when the weapons that deter are the same weapons that, if used, would eliminate victory conditions entirely?
% FOUNDING_PROBLEM_CORROBORATION: Strategic theorists (Brodie, Schelling, Jervis) from outside military establishments attested to the logical structure: mutual vulnerability makes war irrational. Military planners themselves acknowledged the problem (SAC's Curtis LeMay worked under the constraint even while developing massive retaliation doctrine). Physicists who built the weapons (Einstein, Szilard) documented the moment of recognition. The problem remains live because every nuclear-armed state must solve it: how to maintain credible deterrence while accepting that victory is impossible.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__deterrence_unthinkable, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, ExtMetricName, E),
    domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading claims to be MOUNTAIN (natural law emerging from physics and logic, not from human construction or beneficiary capture). The authored extractiveness is LOW (0.22 at interval end) because the constraint is not collecting rents or suppressing alternatives — it is stating a structural fact about the world: mutual vulnerability eliminates victory. Suppression is MINIMAL (0.18) because the constraint does not require coercion to maintain; the logic is self-enforcing. Theater is MODERATE (0.41), reflecting two dynamics: (1) military doctrine contains a performative element — plans for limited nuclear war are authored to preserve institutional coherence, even though the underlying premise (winnability) is incoherent; (2) deterrence itself is performative — it works through credible signal and counter-signal, not through execution. Accessibility collapse is VERY HIGH (0.89) because once the nuclear physics is understood, alternatives (winning great-power war) genuinely disappear from the strategic space. Resistance is MODERATELY HIGH (0.76) because military establishments and some strategic thinkers persistently contest the reading, offering countervailing narratives (limited war, counterforce, damage limitation) that preserve a vestigial winnability. The measurement series show extraction rising from 1945 to 1980 (as nuclear arsenals grew and the incoherence deepened) then stabilizing, with theater rising sharply during the Cold War (high performative content in strategic doctrine) then plateauing. This is consistent with a constraint that is NATURAL (the physics does not change) but whose HUMAN OPERATION becomes increasingly theatrical as institutions work to preserve functional meaning in an incoherent situation.
 *
 * PERSPECTIVAL GAP:
 *   From the civilian population seat: the constraint is a MOUNTAIN — natural, protective, emergent from physics. From the military-institutional seat: the constraint is extractive because it imposes an incoherent mission without consent (they did not choose mutual vulnerability; it was imposed by the invention of weapons). From the strategic analyst seat: the constraint is a TANGLED ROPE because it offers real benefits (no great-power war) but at the cost of perpetual doctrinal incoherence (planning wars that cannot be won). The engine should compute these differently because the power and exit positions differ, even though the underlying logical constraint is identical.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is identity_locked for military establishments because the constraint is fused with institutional identity — they CANNOT exit without ceasing to be military institutions oriented toward power projection (they could pivot to purely defensive roles, but that is an identity transformation). Directionality is trapped for civilian populations because they are geographically bound and cannot renegotiate the terms of their protection. Directionality for strategic planning communities is CONSTRAINED because they can adopt countervailing framings (limited war, counterforce) but cannot fully escape the underlying incoherence. Political leaders are CONSTRAINED because they operate under the logic even while exercising real power within those constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to maintain military power under mutual annihilation) is LIVE and unsolved. Witness: every nuclear-armed great power maintains strategic command structures, war plans, and deterrence doctrine that all assume mutual vulnerability. The constraint has not become mandatrophy (a zombie rule maintained theatrically without function) because the threat is real and the need for stabilizing practices (verification, command-and-control, signaling) persists. However, the theater_ratio's rise and plateau (1962–2024) suggests institutional inertia: the doctrinal infrastructure preserves functional FORM (war-fighting plans exist) while FUNCTION (winning wars) has atrophied. This is not yet full piton (the constraint still prevents great-power war) but it shows the conditions that could lead to mandatrophy if military establishments increasingly treat the plans as purely symbolic. The measurement of theater at 0.41 reflects this: the constraint's operation is 41% performative because military doctrine and command structure must preserve the appearance of victory-seeking to maintain institutional legitimacy, even though victory is logically impossible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_deterrence,
    'Is the constraint that war is unwinnable a natural law emerging from nuclear physics and mutual vulnerability, or a constructed deterrence narrative maintained by great-power elites to justify existing power structures and avoid admitting uncertainty?',
    'Examine whether the logical impossibility of victory persists even if political will, technology, or strategic doctrine were to change. Investigate whether non-nuclear powers (or future nuclear powers outside the great-power club) can escape the constraint through different assumptions or capabilities.',
    'If natural law: the constraint is a MOUNTAIN; beneficiaries (civilian populations) are protected by logic, not by contingent policy. If constructed: the constraint may be a FALSE SUMMIT — a natural-law framing that actually benefits institutional elites (military establishments that benefit from permanent deterrence funding without combat risk). FSM trigger: the constraint declares beneficiaries (civilian populations) while claiming emerges_naturally=true.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_deterrence, empirical, 'Whether constraint emergence is physical/logical or socially constructed.').

omega_variable(
    limited_war_counterfactual,
    'Could a great-power nuclear war remain limited to theater nuclear weapons or counterforce targeting, thereby preserving some notion of victory (e.g., territorial gain, political concession) even if global annihilation is avoided?',
    'Historical analysis of strategic doctrine evolution (SAC plans, Soviet general staff doctrine, NATO escalation frameworks) to determine whether ''limited victory'' remained operationally thinkable across the interval. Game-theoretic analysis of whether a first-strike counterforce strategy could achieve victory before retaliation.',
    'If limited war is truly possible: the constraint is NOT deterministic; countervailing_thinkable reading becomes structurally valid, and this reading''s claim of categorical unwinability is overclaimed. Extraction would rise (the constraint would become TANGLED ROPE: coordination + extraction via suppression of the countervailing option). If limited war is proven impossible: the mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(limited_war_counterfactual, empirical, 'Whether winnability could persist in limited nuclear war scenarios.').

omega_variable(
    theater_ratio_interpretation,
    'Does the measured theater_ratio (0.41) reflect performative institutional maintenance (military doctrine preserving form when function is impossible) or does it reflect genuine strategic uncertainty about whether limited war is possible?',
    'Classify the doctrinal content: statements of operational planning can be analyzed for whether they assume victory is possible (genuine strategy) or assume victory is impossible but maintain the plans for deterrence credibility (theater). Examine whether strategic planners themselves treat the plans as real or performative.',
    'If theater reflects genuine operational incoherence (performing for institutional legitimacy), the theater_ratio diagnosis holds. If theater reflects strategic uncertainty (planners truly believe limited victory might be possible), then the extraction and theater metrics should be reinterpreted as cost of living with genuine uncertainty, not as performative maintenance of a closed question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_interpretation, empirical, 'Whether theater_ratio indicates performative maintenance or genuine strategic uncertainty.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the deterrence_unthinkable reading (this constraint) logically foreclose the countervailing_thinkable reading, or do both remain live positions that different institutional actors can coherently hold?',
    'Test whether a strategic planner can simultaneously hold: (a) mutual vulnerability makes great-power war unwinnable (deterrence_unthinkable premise) and (b) limited nuclear war could achieve limited victory (countervailing premise). If these are logically incompatible in a single framework, the relation is forecloses; if they are compatible (because ''unwinnable'' could mean ''total war unwinnable but limited war possible''), the relation is coexists_with.',
    'If foreclosure: countervailing_thinkable is incoherent and should not be instantiated as a separate constraint. If coexistence: both readings are live and the corpus should include both as distinct stories with different ε values, linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether this reading''s core premise rules out countervailing reading.').

omega_variable(
    identity_lock_escape_velocity,
    'Can military establishments genuinely escape identity_locked status by reframing their mission from conquest-and-power-projection to pure deterrence-and-defense, or does that reframing constitute exit-death (the institution survives but ceases to be itself)?',
    'Examine documented instances of military-institutional transformation (Japan''s Self-Defense Force, post-Cold War German Bundeswehr, demilitarization contexts). Assess whether reorientation to defensive-only posture is experienced as institutional survival or institutional death by participants.',
    'If exit-death (institutional survival requires victory-seeking): identity_lock is structurally permanent and exit_options should remain identity_locked. If genuine escape: exit_options could shift to constrained or mobile, reducing directionality toward the target end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_escape_velocity, empirical, 'Whether military identity-lock to the constraint is permanent or reversible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1945, 0.1).
narrative_ontology:measurement_basis(war__tr_t1945, observed).
narrative_ontology:measurement(war__tr_t1962, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1962, 0.28).
narrative_ontology:measurement_basis(war__tr_t1962, observed).
narrative_ontology:measurement(war__tr_t1980, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1980, 0.42).
narrative_ontology:measurement_basis(war__tr_t1980, observed).
narrative_ontology:measurement(war__tr_t2000, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2000, 0.38).
narrative_ontology:measurement_basis(war__tr_t2000, observed).
narrative_ontology:measurement(war__tr_t2013, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2013, 0.4).
narrative_ontology:measurement_basis(war__tr_t2013, observed).
narrative_ontology:measurement(war__tr_t2024, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(war__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1945, 0.08).
narrative_ontology:measurement_basis(war__be_t1945, observed).
narrative_ontology:measurement(war__be_t1962, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1962, 0.18).
narrative_ontology:measurement_basis(war__be_t1962, observed).
narrative_ontology:measurement(war__be_t1980, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1980, 0.24).
narrative_ontology:measurement_basis(war__be_t1980, observed).
narrative_ontology:measurement(war__be_t2000, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2000, 0.19).
narrative_ontology:measurement_basis(war__be_t2000, observed).
narrative_ontology:measurement(war__be_t2013, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2013, 0.22).
narrative_ontology:measurement_basis(war__be_t2013, observed).
narrative_ontology:measurement(war__be_t2024, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2024, 0.22).
narrative_ontology:measurement_basis(war__be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1945, 0.05).
narrative_ontology:measurement_basis(war__su_t1945, observed).
narrative_ontology:measurement(war__su_t1962, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1962, 0.15).
narrative_ontology:measurement_basis(war__su_t1962, observed).
narrative_ontology:measurement(war__su_t1980, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1980, 0.22).
narrative_ontology:measurement_basis(war__su_t1980, observed).
narrative_ontology:measurement(war__su_t2000, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2000, 0.17).
narrative_ontology:measurement_basis(war__su_t2000, observed).
narrative_ontology:measurement(war__su_t2013, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2013, 0.19).
narrative_ontology:measurement_basis(war__su_t2013, observed).
narrative_ontology:measurement(war__su_t2024, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2024, 0.18).
narrative_ontology:measurement_basis(war__su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__deterrence_unthinkable, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__deterrence_unthinkable, 0.08).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__rhetorical_contraction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel: 'war_winnability_post_1945'. The kernel is a stabilized commitment that nuclear weapons fundamentally changed great-power war. Three structurally distinct constraints instantiate three readings: (1) deterrence_unthinkable (this story) — nuclear weapons made victory IMPOSSIBLE; (2) countervailing_thinkable — victory remains possible through limited war and counterforce; (3) rhetorical_contraction — victory became unsayable but remained operationally planned. Each reading has different ε, different beneficiary/victim structure, and different strategic implications. They are linked via network.affects_constraints because they compete for the same institutional and analytical space — adopting one reading shapes how the others are perceived. This story claims deterrence_unthinkable is a MOUNTAIN (emerges from physics and logic); countervailing_thinkable will claim limited victory is empirically possible (shifting ε upward); rhetorical_contraction will claim winnability remains operationally real despite public taboo (a different empirical claim about institutional practice). All three stories must be in the corpus for the kernel contest to be analyzable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__deterrence_unthinkable, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
