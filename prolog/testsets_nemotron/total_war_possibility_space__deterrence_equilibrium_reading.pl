% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__deterrence_equilibrium_reading, []).

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
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Deterrence Equilibrium Reading — Mutual Vulnerability as Constraint on Total War
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This constraint story instantiates the deterrence_equilibrium_reading of
 *   the total_war_possibility_space kernel. The reading holds that total war
 *   remains in the strategically thinkable and planable space, but is
 *   rendered non-preferable by a cost-benefit calculation whose extreme costs
 *   derive from mutual vulnerability. The constraint is not a normative
 *   prohibition or a cognitive exclusion — it is a continuous, actively
 *   maintained equilibrium that requires credible war-fighting capabilities
 *   (counterforce, damage limitation, escalation management) to signal
 *   resolve. The coordination function (preventing great power war) is real
 *   and substantial; the extraction function (rent-seeking by defense
 *   establishments, risk externalization to the powerless, identity-locked
 *   institutional reproduction) is co-present and growing. The engine
 *   computes per-seat classifications from the structural data; this story
 *   provides the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.62).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.41).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.37).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Deterrence Equilibrium Reading — Mutual Vulnerability as Constraint on Total War").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, '877129c8-1d96-4bc2-a1a9-17e178e785d2').
narrative_ontology:cs_kernel_codification('877129c8-1d96-4bc2-a1a9-17e178e785d2', distributed).
narrative_ontology:cs_authority_grounding('877129c8-1d96-4bc2-a1a9-17e178e785d2', practice).
narrative_ontology:cs_interpretation_layer_present('877129c8-1d96-4bc2-a1a9-17e178e785d2').
narrative_ontology:cs_reading_relation('877129c8-1d96-4bc2-a1a9-17e178e785d2', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_reading_relation('877129c8-1d96-4bc2-a1a9-17e178e785d2', total_war_possibility_space__space_contraction_reading, influences).
narrative_ontology:cs_axiom('877129c8-1d96-4bc2-a1a9-17e178e785d2', foundational, mutual_vulnerability_as_stabilizer).
narrative_ontology:cs_axiom_status(mutual_vulnerability_as_stabilizer, holdable).
narrative_ontology:cs_axiom_grounding('877129c8-1d96-4bc2-a1a9-17e178e785d2', mutual_vulnerability_as_stabilizer, empirically_contingent).
narrative_ontology:cs_axiom('877129c8-1d96-4bc2-a1a9-17e178e785d2', foundational, counterforce_credibility_requirement).
narrative_ontology:cs_axiom_status(counterforce_credibility_requirement, holdable).
narrative_ontology:cs_axiom_grounding('877129c8-1d96-4bc2-a1a9-17e178e785d2', counterforce_credibility_requirement, instrumental).
narrative_ontology:cs_reference_frame('877129c8-1d96-4bc2-a1a9-17e178e785d2', schelling_wohlstetter_equilibrium).
narrative_ontology:cs_drift_state('877129c8-1d96-4bc2-a1a9-17e178e785d2', contemporary_modernization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('877129c8-1d96-4bc2-a1a9-17e178e785d2', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_armed_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_complexes).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, strategic_planning_establishments).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, disarmament_advocacy_networks).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, rational_deterrence_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, mutual_assured_destruction_stability).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear arsenals and the command infrastructure to employ them. Set declaratory doctrines, force postures, and escalation thresholds. Bear the direct costs of arsenal maintenance and modernization but extract security guarantees and strategic autonomy from the deterrence equilibrium. Exit from the deterrence framework would require unilateral disarmament or a verified multilateral regime — both treated as existential risks.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_armed_states, agenda_setter,
    institutional, generational, constrained, global).

% Receive sustained funding for warhead modernization, delivery systems, command/control, and supporting infrastructure. The deterrence equilibrium's requirement for credible, survivable, and penetrative forces generates continuous procurement cycles. Influence doctrine and requirements through revolving-door personnel, funded think tanks, and congressional liaison. Exit is mobile — they can pivot to conventional or dual-use portfolios — but the deterrence rent is the premium segment.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_complexes, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_complexes, agenda_setter).

% Produce the doctrinal architecture (counterforce targeting, escalation ladders, damage-limitation criteria) that makes deterrence 'credible.' Their professional identity, career progression, and institutional mandate are constituted by the deterrence mission. Exit would mean abandoning the epistemic framework that defines their expertise — not a career change but an identity dissolution. They are the custodians of the planning space the reading describes.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, strategic_planning_establishments, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, strategic_planning_establishments, agenda_setter).

% Live under the deterrence equilibrium's shadow without possessing its central currency. Bear risks of nuclear escalation in conflicts involving nuclear-armed patrons or adversaries; excluded from the decision calculus that sets escalation thresholds. Their security is hostage to calculations they cannot influence. Exit options are constrained: bandwagoning, balancing via conventional means, or pursuing latent capability — each carries the equilibrium's penalty structure.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states, excluded).

% Bear the immediate physical consequences when deterrence fails or when conventional conflict occurs under the nuclear shadow. Have no voice in the doctrines that calculate acceptable damage or escalation risks. Exit is physically impossible — they are the terrain on which the equilibrium's credibility is demonstrated. Their victimhood is structural: the equilibrium's stability condition is that they remain targetable.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Challenge the deterrence equilibrium's legitimacy and push for treaty regimes (NPT, TPNW, test bans). Their exclusion is institutional: they are consulted performatively but their core demand — elimination — is treated as destabilizing by the agenda-setters. Exit is mobile as organizations but their political project is trapped by the equilibrium's logic: any progress is framed as undermining the very stability that prevents use.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, disarmament_advocacy_networks, excluded,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, disarmament_advocacy_networks, observer).

% Produce the strategic assessments that feed force planning and arms control negotiations. They see the full structure — the coordination function (preventing great power war) and the extraction function (rent-seeking, risk externalization, identity-locked institutions). Their analytical exit is unconstrained; they do not bear the equilibrium's costs nor collect its rents.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, great_power_competition_analysts, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents direct great power war by making the cost of total war exceed any conceivable gain — a genuine collective action problem solved by mutual vulnerability rather than central authority. The equilibrium coordinates restraint without a sovereign enforcer.
% TRANSFER_FUNCTION: Transfers security resources and strategic autonomy from non-nuclear states and civilian populations to nuclear-armed states and their defense establishments. The coordination benefit (no great power war) is captured by the nuclear-armed; the residual risk (accidental use, escalation, proliferation cascades) is externalized to the excluded and powerless.
% ABSENT_VOICES: Future generations who inherit the accumulated waste, environmental contamination, and persistent existential risk; populations in the Global South who host testing legacies and bear proliferation pressures without deterrence benefits; the planetary biosphere as a stakeholder with no institutional representation.
% DISAPPEARANCE_RATIONALE: If the deterrence equilibrium vanished overnight — mutual vulnerability no longer credited as a restraint — nuclear-armed states would immediately shift to warfighting doctrines optimized for damage limitation and first-strike advantage. Crisis stability would collapse; arms racing would accelerate from modernization to numerical and qualitative breakout. The world would rearrange around explicit war-fighting postures rather than deterrence signaling.
% FOUNDING_PROBLEM: The founding problem was the strategic instability of the early nuclear age: monopoly gave way to bilateral arsenal growth without a conceptual framework for coexistence. Mutual vulnerability was articulated (Brother, Wohlstetter, Schelling) as the only stable equilibrium — the problem was how to make the bomb unusable without abolishing it.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as live by nuclear-armed states' declaratory policies (deterrence remains the mission) and by the continued production of counterforce capabilities. It is attested as dead by disarmament advocates and TPNW parties who argue the problem was misdiagnosed — the bomb was never made unusable, only its use was deferred while the machinery grew. Independent historians of the early cold war (e.g., Kaplan, Rosenberg) corroborate that the 'solution' was always contested within the establishing institutions themselves — SAC, the JCS, and the NSC never accepted mutual vulnerability as final.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the growing gap between the marginal cost of deterrence credibility and the resources extracted to maintain it — modernization programs (US Sentinel, Russian Sarmat, Chinese silo fields) far exceed what a minimal deterrent posture would require. Suppression (0.41) is moderate: the constraint does not physically prevent disarmament advocacy or treaty negotiation, but it structures the legitimacy space so that elimination proposals are treated as destabilizing. Theater ratio (0.28) captures the increasing share of doctrinal and procurement activity that serves bureaucratic/institutional reproduction rather than marginal deterrence value. Accessibility collapse (0.58) and resistance (0.37) reflect that alternatives (disarmament, no-first-use, de-alerting) remain thinkable and advocated but face high structural barriers.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear-armed state seat, the equilibrium is a successful coordination mechanism (rope-like) that solved the great power war problem. From the non-nuclear state and civilian population seats, the same structure operates as extraction (snare-like) — they pay the risk without collecting the security. From the strategic planning establishment seat, the equilibrium is identity-constitutive (piton-adjacent) — its atrophy would dissolve their professional world. The engine computes this seat divergence; the claim (tangled_rope) states the structural truth that both coordination and extraction are simultaneously operative.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-armed states and their defense establishments are structural beneficiaries (low d) — they collect security rents, institutional budgets, and strategic autonomy. Strategic planning establishments are identity-locked beneficiaries: their professional self-concept is constituted by the deterrence mission, making exit epistemically impossible. Non-nuclear states are payers with constrained exit — they bear systemic risk without deterrence currency. Civilian populations are trapped payers — the equilibrium's stability condition is their targetability. Disarmament networks are excluded: they occupy the analytical seat but are structurally barred from the decision calculus. The engine derives d from these declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (making the bomb unusable without abolishing it) is contested: nuclear-armed states and planning establishments treat it as live (modernization continues, doctrines evolve); disarmament advocates and TPNW parties treat it as dead (the equilibrium never solved the problem, only deferred it while the machinery grew). The mandate has not atrophied into pure inertia — active investment and doctrinal innovation continue — but the coordination function's marginal return is declining while the extraction function's return is rising. This is a tangled_rope approaching a snare transition if the coordination benefit (no great power war) ceases to be credited to the equilibrium rather than to other factors (economic interdependence, institutional density, normative change).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'What fraction of current nuclear modernization spending is necessary for credible deterrence versus what fraction is institutional rent-seeking?',
    'Independent cost-effectiveness analysis comparing current force postures against minimal deterrence benchmarks (e.g., 300 survivable warheads, no counterforce capability). Track procurement justifications against operational requirements documents.',
    'If most modernization is rent-seeking, the constraint is predominantly extractive (snare-tending). If modernization tracks credible deterrence requirements, the coordination function remains substantial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Separability of coordination and extraction in current force postures').

omega_variable(
    deterrence_credit_assignment,
    'Is the absence of great power war since 1945 causally attributable to mutual vulnerability, or to other factors (economic integration, institutional density, normative change, luck)?',
    'Counterfactual analysis using historical near-miss data (Cuban Missile Crisis, Able Archer, etc.) and structural equation modeling of great power war incidence with/without nuclear deterrence as a variable.',
    'If deterrence is not the primary cause, the coordination function is overclaimed and the constraint''s extraction is less justified. If deterrence is necessary but not sufficient, the equilibrium is a necessary coordination substrate with extractive superstructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_credit_assignment, conceptual, 'Causal attribution of the long peace to the deterrence equilibrium').

omega_variable(
    identity_lock_reversibility,
    'Could the strategic planning establishment''s identity-lock be broken by a sustained political decision to pursue disarmament, or is it structurally irreversible without institutional collapse?',
    'Historical analysis of past identity shifts in military establishments (e.g., US post-Vietnam, Soviet post-Afghanistan, post-Cold War drawdowns). Track whether doctrinal communities adapt or resist when political mandates change.',
    'If reversible, the identity-locked exit_option is contingent, not structural — the constraint could shift toward scaffold or rope. If irreversible, the equilibrium has a self-reproducing institutional core that resists mandated change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Reversibility of the planning establishment''s identity commitment to deterrence').

omega_variable(
    kernel_reading_foreclosure,
    'Does the deterrence_equilibrium_reading logically foreclose the nuclear_taboo_reading within a single state''s framework, or do they coexist as complementary layers (material deterrence + normative reinforcement)?',
    'Analyze declaratory policy and doctrinal documents of nuclear-armed states: do they treat the taboo as an independent causal factor or as epiphenomenal to deterrence? Track whether taboo-language appears in operational planning or only in public rhetoric.',
    'If forecloses, the readings are mutually exclusive frameworks — a state cannot simultaneously hold both. If coexists_with, they are layered: deterrence does the material work, taboo provides normative reinforcement. The reading_relations declaration assumes coexists_with; this omega tests that assumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between deterrence and taboo readings within a single framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1949, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1949, 0.08).
narrative_ontology:measurement(tota_tr_t1960, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(tota_tr_t1972, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1972, 0.18).
narrative_ontology:measurement(tota_tr_t1983, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1983, 0.25).
narrative_ontology:measurement(tota_tr_t1991, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1991, 0.22).
narrative_ontology:measurement(tota_tr_t2001, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2001, 0.24).
narrative_ontology:measurement(tota_tr_t2014, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2014, 0.26).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(tota_be_t1949, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1949, 0.18).
narrative_ontology:measurement(tota_be_t1960, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1960, 0.32).
narrative_ontology:measurement(tota_be_t1972, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1972, 0.41).
narrative_ontology:measurement(tota_be_t1983, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1983, 0.55).
narrative_ontology:measurement(tota_be_t1991, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1991, 0.48).
narrative_ontology:measurement(tota_be_t2001, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2001, 0.52).
narrative_ontology:measurement(tota_be_t2014, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2014, 0.58).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1949, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1949, 0.25).
narrative_ontology:measurement(tota_su_t1960, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(tota_su_t1972, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1972, 0.38).
narrative_ontology:measurement(tota_su_t1983, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1983, 0.45).
narrative_ontology:measurement(tota_su_t1991, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1991, 0.35).
narrative_ontology:measurement(tota_su_t2001, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2001, 0.38).
narrative_ontology:measurement(tota_su_t2014, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2014, 0.4).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2025, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__deterrence_equilibrium_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__nuclear_taboo_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_modernization_imperative).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, arms_control_regime_viability).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_credibility).

% DUAL FORMULATION NOTE:
% This story is one member of the total_war_possibility_space constraint family. The three readings (deterrence_equilibrium, nuclear_taboo, space_contraction) share the same empirical referent (the absence of total war since 1945) but author different ε values and different beneficiary/victim structures because they locate the constraint's causal mechanism differently. This reading (deterrence_equilibrium) authors ε=0.62 with beneficiaries in the nuclear-armed establishment and victims in the excluded populations. The nuclear_taboo_reading would author lower ε (normative constraint, less extraction) with beneficiaries in the norm entrepreneurs. The space_contraction_reading would author near-zero ε (cognitive exclusion, not active maintenance) with no clear beneficiaries. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__deterrence_equilibrium_reading, institutional, 0.15).
constraint_indexing:directionality_override(total_war_possibility_space__deterrence_equilibrium_reading, powerless, 0.95).
constraint_indexing:directionality_override(total_war_possibility_space__deterrence_equilibrium_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
