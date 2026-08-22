% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__inherent_executive_reading, []).

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
 *   constraint_id: war_powers_allocation__inherent_executive_reading
 *   human_readable: Inherent Executive War Powers Authority
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The inherent-executive-authority reading of war powers grants the
 *   president constitutional power to deploy military force in defense of
 *   national interests without prior Congressional authorization. Under this
 *   reading, the Commander-in-Chief clause vests the president with
 *   independent war-making authority, and Congressional authorization becomes
 *   courteous consultation rather than constitutional requirement. Congress
 *   retains post-hoc appropriations power but cannot prevent deployment. This
 *   reading has been the dominant executive-branch legal position since the
 *   Cold War and underpins numerous unilateral deployments. It is contested
 *   by the competing congressional-primacy reading (which requires explicit
 *   prior authorization) and the functional-accommodation reading (which
 *   permits unilateral action only for imminent threats). The
 *   inherent-authority reading is one of three constraint stories
 *   instantiating the contested war_powers_allocation kernel; this JSON
 *   generates ONLY this reading as a self-contained constraint with its own
 *   ε, beneficiary/victim structure, and metrics. The sibling readings are
 *   OTHER FILES, not integrated here.
 *
 * KEY AGENTS:
 *   - president_as_commander: Institutional actor holding executive authority; defines national interests, initiates deployment, supplies interpretation
 *   - congress: Institutional actor holding nominal war-declaration authority; reduced to post-hoc funding lever
 *   - military_command_structure: Institutional beneficiary of unified authority; operates without legislative veto
 *   - american_public: Powerless agent bearing cost of conscription/war without direct authorization voice
 *   - courts: Powerful observer declining to arbitrate via political-question doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.68).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.42).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive War Powers Authority").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional/political").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, 'be1fc71b-0844-4b10-9339-e44009962657').
narrative_ontology:cs_kernel_codification('be1fc71b-0844-4b10-9339-e44009962657', formalized).
narrative_ontology:cs_authority_grounding('be1fc71b-0844-4b10-9339-e44009962657', lineage).
narrative_ontology:cs_interpretation_layer_present('be1fc71b-0844-4b10-9339-e44009962657').
narrative_ontology:cs_reading_relation('be1fc71b-0844-4b10-9339-e44009962657', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('be1fc71b-0844-4b10-9339-e44009962657', war_powers_allocation__functional_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('be1fc71b-0844-4b10-9339-e44009962657', foundational, commander_in_chief_clause_grants_inherent_authority).
narrative_ontology:cs_axiom_status(commander_in_chief_clause_grants_inherent_authority, holdable).
narrative_ontology:cs_axiom_grounding('be1fc71b-0844-4b10-9339-e44009962657', commander_in_chief_clause_grants_inherent_authority, deontological).
narrative_ontology:cs_axiom('be1fc71b-0844-4b10-9339-e44009962657', foundational, executive_authority_not_dependent_on_prior_congressional_authorization).
narrative_ontology:cs_axiom_status(executive_authority_not_dependent_on_prior_congressional_authorization, holdable).
narrative_ontology:cs_axiom_grounding('be1fc71b-0844-4b10-9339-e44009962657', executive_authority_not_dependent_on_prior_congressional_authorization, empirically_contingent).
narrative_ontology:cs_reference_frame('be1fc71b-0844-4b10-9339-e44009962657', constitutional_executive_autonomy_in_war_powers).
narrative_ontology:cs_drift_state('be1fc71b-0844-4b10-9339-e44009962657', contemporary_post_cold_war, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('be1fc71b-0844-4b10-9339-e44009962657', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, presidential_unilateral_prerogative).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, congress).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, war_powers_consultation_requirement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, military_command_structure).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, american_public).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, american_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As commander-in-chief, interprets the constitutional grant of military authority as inherent power to deploy force for national defense without prior legislative authorization. Sets the definition of what constitutes 'defense of national interests' and determines urgency thresholds unilaterally. Can initiate military operations immediately and present Congress with fait accompli; Congress's post-hoc appropriations become ratification mechanism rather than authorization gate.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, president_as_commander, agenda_setter,
    institutional, generational, arbitrage, global).

% Holds formal constitutional power to declare war and authorize military force, but under this reading that power becomes advisory or retrospective. Congress funds military operations after executive initiation (appropriations are treated as ratification) and can theoretically defund, but political cost of cutting funds during active deployment is prohibitive. Cannot prevent deployment but must validate it through budgeting. Excluded from the initial authorization gate that determines whether force is deployed.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, congress, payer,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__inherent_executive_reading, congress, excluded).

% Operates under clear chains of command from the president without the friction of prior legislative debate or constraint. Benefits from unambiguous authority and rapid decision-making capacity. Receives orders to deploy with constitutional imprimatur of inherent executive power; the reading legitimizes unilateral executive military action and removes the institutional uncertainty of legislative veto power.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, military_command_structure, beneficiary,
    institutional, biographical, trapped, global).

% Receives the coordinate benefit of rapid executive response to imminent threats without the delay of legislative process. Also bears the cost of potentially being conscripted into wars authorized only by executive decree, with limited direct voice in the authorization decision. Cannot prevent executive deployment but can attempt to influence Congress to cut appropriations after the fact — a constrained and indirect veto power.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, american_public, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__inherent_executive_reading, american_public, beneficiary).

% Maintain formal power to review constitutional questions but under political-question doctrine traditionally decline to adjudicate war powers allocation disputes as between the branches. Their abstention leaves the executive/legislative allocation subject only to inter-branch negotiation and congressional appropriations control, not judicial arbitration.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, courts, observer,
    powerful, generational, analytical, national).

% Face the constraint that U.S. military response to their actions can be deployed unilaterally by the president without the delay of legislative authorization. This removes what would otherwise be a constraint on executive speed — they cannot count on congressional friction to slow response or raise its political cost.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, foreign_adversaries, excluded,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__inherent_executive_reading, president_as_commander).
narrative_ontology:fixing_cost_class(war_powers_allocation__inherent_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides decisive executive authority to respond rapidly to military threats and emergencies without the friction of legislative process; coordinates military action under unified command from the commander-in-chief rather than through divided civilian-military-legislative authority.
% TRANSFER_FUNCTION: Transfers war authorization authority from shared legislative-executive gate (per the competing reading) to unilateral executive authority, with Congress relegated to post-hoc funding validation. Moves the timing and framing of military action from negotiated to executive-determined.
% ABSENT_VOICES: Legislative doves and war-skeptical constituencies are excluded from the authorization gate; they retain only post-deployment appropriations leverage, which is politically costly to exercise. International law and treaty-bound allies are not seated in the U.S. constitutional allocation — the reading does not constrain executive action by reference to treaty obligations or UN authorization.
% DISAPPEARANCE_RATIONALE: If this reading were replaced by congressional-primacy reading, military deployment would require explicit prior authorization; executive speed would decrease; Congress would gain a veto gate; the balance of war powers would shift visibly. Strategic calculations by allies and adversaries would shift in response to slower U.S. decisional process.
% FOUNDING_PROBLEM: Eighteenth-century concern: executive authority needed to respond to sudden invasion or emergency without waiting for legislative assembly; need for unified command of military forces under civilian president rather than coordination failures from divided authority.
% FOUNDING_PROBLEM_CORROBORATION: Executive branch and originalist scholars attest the founding problem is still live (emergencies remain possible). Congressional and international-law scholars attest the problem is substantially solved by modern communication and that the reading has metastasized into permanent unilateral authority divorced from emergency conditions. Independent historical scholarship on 18th-century military emergencies and the Framers' intent is divided; however, the shift from emergency-only deployments to sustained political-objective deployments is documented in congressional records and defense department historical analyses.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__inherent_executive_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__inherent_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__inherent_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because executive authority to deploy force is decoupled from prior authorization and Congress is systematically excluded from the gate. The constraint's persistence depends on Congress accepting post-hoc appropriations as ratification — which they do, politically, because defunding deployed troops carries electoral cost. This is the enforcement mechanism: not coercion but political cost structure. Theater is low-moderate (0.28) because the reading has genuine constitutional foundation in the text (Commander-in-Chief clause) and real coordination benefit (unified command, speed), but the theatrical component grows over the interval as executive deployments extend beyond emergency contexts (t=0 to t=60 shows rising theater as the 'emergency' framing becomes less plausible for prolonged operations; t=60 to t=80 flattens as the reading stabilizes at its current interpretation). Suppression is low (0.42) because Congress is not violently prevented from asserting authority — they simply face political cost. Accessibility of alternatives is moderate (0.65 in base_properties) because congressional action to reassert authorization authority is always formally available but politically costly. Resistance is high (0.72) because Congress, courts, and international-law scholars persistently argue for the competing reading and pressure against the inherent-authority framing. The measurement series show extractiveness rising from t=0 to t=60 as the reading crystallizes into permanent post-hoc-appropriations norm, then stabilizing. Theater similarly rises then stabilizes as the reading becomes institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   The perspective gap is severe and deliberate. From the executive seat, the constraint is pure coordination: unified command, rapid response to threats, the constitutional text clearly grants this power. From Congress's seat, the constraint is enforced extraction: authorization authority is formally held but politically unusable, Congress is relegated to post-hoc validation of executive choice. From the public seat, it is hybrid: genuine security benefit when threats are imminent, but also experience of being conscripted into wars authorized without their voice. From the courts' seat, it is a pure institutional boundary-keeping measure: defer to the political branches (which happens to favor the executive). The engine computes these different per-seat types from the structural data — the authored claim (tangled_rope) represents the true structural classification (has coordination function + asymmetric extraction + active enforcement), but the seats experience different local types. The perspectives are structural consequences of the asymmetric power and exit positions, not observer bias.
 *
 * DIRECTIONALITY LOGIC:
 *   The president's seat experiences this constraint as a beneficiary with arbitrage exit: the president can reinterpret what counts as national interest and can define urgency unilaterally, operating with maximum structural flexibility. Congress experiences it as a constrained payer: formally holding authorization power but politically unable to exercise it (defunding deployed troops is electorally toxic), so the constraint extracts authorization authority from Congress and transfers it to the executive. The military command structure benefits straightforwardly. The American public is dual-positioned: genuinely benefits from unified command and rapid response to imminent threats (coordination benefit), but also bears conscription and war costs without direct authorization voice (extraction). Courts are pure observers: they have power but decline to exercise it (political question). The reading structures inherent asymmetry: the executive's exit options are arbitrage (redefine the constraint's scope), Congress's are identity-locked (cannot defund deployed troops without appearing to endanger troops), the public's are trapped (cannot exit the polity or conscription). Effective extraction χ is highest for Congress because they are high-power institutional targets with highly constrained exit; moderately high for the public because they are powerless; low for the executive because they are beneficiaries with arbitrage exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (emergency response authority, unified command) was live in 1789 when communication delays and assembly friction made emergency executive authority structurally necessary. The founding problem is now contested: modern communication means Congress can assemble in hours; the reading has metastasized from emergency-only authority into permanent, context-independent power to deploy for political objectives (undeclared wars, regime-change operations, drone strikes). The constraint shows mandatrophy markers: (1) The founding problem is dead for modern emergency response but the reading persists. (2) Congress could reassert authorization authority (has the formal power), but political cost of defunding troops creates structural suppression of that power. (3) The constraint is genuinely coordinated (unified command works; executive speed is real), which is why it is tangled_rope not pure snare — the coordination function is real but so is the extraction. The mandatrophy is partial: the reading is not purely theatrical (coordination works), but its persistence beyond the founding problem represents the constraint being held in place by extraction benefit (executive power accumulation) more than by coordination necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergency_vs_permanent_authority,
    'Is the inherent executive authority inherently limited to emergency/imminent-threat scenarios, or is it a permanent, context-independent power to deploy force for ''national interests''?',
    'Historical analysis of original Framing intent, contemporary executive practice when imminent threats are absent (e.g., undeclared wars of political choice), and whether courts or Congress constrain scope.',
    'If limited to emergency, the reading is narrower and the extraction is justified by coordination cost; if permanent and discretionary, the reading absorbs congressional authorization authority entirely, raising extraction classification and mandatrophy questions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_vs_permanent_authority, empirical, 'Whether inherent authority is bounded by emergency or applies always.').

omega_variable(
    appropriations_as_ratification,
    'Does post-hoc congressional appropriation for deployed military operations constitute valid authorization (ratification) or merely funding of a fait accompli the president imposed?',
    'Congressional intent analysis: when Congress votes appropriations for an ongoing war, does it vote on the war itself or only on logistics? Legislative history, floor debates, and testimony from members about their understanding of the vote.',
    'If appropriation counts as authorization, Congress retains a gate (albeit weak and politically costly); if appropriation is purely logistical, Congress has no authorization gate and the constraint is pure extraction. This determines whether the reading is tangled_rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appropriations_as_ratification, conceptual, 'Whether post-deployment appropriations vest Congress with authorization authority.').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the inherent-authority reading logically foreclose the congressional-primacy reading, or do they coexist as live positions within the constitutional framework?',
    'Test whether a single institutional actor (a president, a congress, a court) could coherently hold both readings simultaneously, or whether holding inherent-authority forecloses holding that Congress has primary authorization power. Examine whether the readings differ only on empirical facts (e.g., what the Framers intended) or on foundational constitutional premises.',
    'Foreclosure would mean the readings represent a genuine constitutional contradiction with a definite answer; coexistence would mean the readings are compatible competing interpretations held by different parties, and the constraint captures one coalition''s reading of a contested kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Whether this reading and congressional-primacy reading are logically incompatible or coexistable.').

omega_variable(
    suppression_mechanism_identity_lock,
    'Is the suppression of congressional authorization authority structural (political cost of defunding deployed troops) or internalized (institutional actors have internalized norms of executive military authority such that suppression persists even when structural barriers weaken)?',
    'Comparative institutional analysis: examine periods when structural cost to Congress of defunding was lower (e.g., end of deployments, legislative majorities hostile to executive war) and whether Congress asserts authorization authority more forcefully. Post-deployment congressional assertion of authority vs. deference patterns.',
    'If suppression is largely internalized norm, the constraint''s persistence does not depend on active enforcement and resembles a piton (theatrical maintenance of norms) more than a tangled_rope; if structural, it is truly enforced by political cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_lock, empirical, 'Whether suppression of Congress''s authorization role is structural or internalized as institutional norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__inherent_executive_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(war__tr_t10, war_powers_allocation__inherent_executive_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(war__tr_t20, war_powers_allocation__inherent_executive_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__inherent_executive_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(war__tr_t40, war_powers_allocation__inherent_executive_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(war__tr_t50, war_powers_allocation__inherent_executive_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(war__tr_t60, war_powers_allocation__inherent_executive_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement(war__tr_t80, war_powers_allocation__inherent_executive_reading, theater_ratio, 80, 0.28).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__inherent_executive_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(war__be_t10, war_powers_allocation__inherent_executive_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(war__be_t20, war_powers_allocation__inherent_executive_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__inherent_executive_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(war__be_t40, war_powers_allocation__inherent_executive_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(war__be_t50, war_powers_allocation__inherent_executive_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(war__be_t60, war_powers_allocation__inherent_executive_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(war__be_t80, war_powers_allocation__inherent_executive_reading, base_extractiveness, 80, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__inherent_executive_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(war__su_t10, war_powers_allocation__inherent_executive_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(war__su_t20, war_powers_allocation__inherent_executive_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__inherent_executive_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(war__su_t40, war_powers_allocation__inherent_executive_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(war__su_t50, war_powers_allocation__inherent_executive_reading, suppression_requirement, 50, 0.41).
narrative_ontology:measurement(war__su_t60, war_powers_allocation__inherent_executive_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(war__su_t80, war_powers_allocation__inherent_executive_reading, suppression_requirement, 80, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__inherent_executive_reading, 0.12).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__functional_accommodation_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, presidential_emergency_powers_scope).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, congressional_appropriations_as_veto).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the war_powers_allocation kernel. The kernel is contested: three readings (inherent_executive, congressional_primacy, functional_accommodation) represent different institutional actors' constitutional interpretations. All three are constraints on the same constitutional text; each has different ε (executive reading is more extractive than congressional reading), different victim sets, different persistence mechanisms. The sibling constraints are linked via network.affects_constraints. Do not merge the readings into one constraint — that would conflate ε values and obscure the structural asymmetry the corpus is designed to detect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
