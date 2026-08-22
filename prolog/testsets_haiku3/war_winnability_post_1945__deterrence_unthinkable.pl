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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Nuclear Deterrence: War Winnability Foreclosed (Unthinkable Reading)
 *   domain: strategic_studies/international_relations/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint instantiates the 'deterrence-unthinkable' reading of the
 *   nuclear war winnability kernel: the reading holds that nuclear weapons
 *   made great-power total war categorically unwinnable and rendered planning
 *   for victory incoherent as an operational matter. This is a KERNEL
 *   READING, not a free-standing constraint. The kernel
 *   (war_winnability_post_1945) is contested across three readings:
 *   countervailing_thinkable (winnability persists through selective
 *   targeting), deterrence_unthinkable (winnability is categorically
 *   foreclosed), and rhetorical_contraction (winnability became unsayable
 *   while remaining operationally planned). This story generates ONLY the
 *   deterrence-unthinkable reading. The sibling readings are other
 *   constraints (other JSON files) linked via network.affects_constraints.
 *   The structural delta for this reading is operational contraction:
 *   winnability exits the reachable space. Beneficiaries are civilian
 *   populations (protected by the impossibility of great-power war). Victims
 *   are military establishments (mission incoherence—they maintain arsenals
 *   but cannot plan for victory). The claim/metric gap is intentional and
 *   structural: the constraint is CLAIMED as tangled_rope (coordination via
 *   deterrence + extraction via mission denial) while the metrics describe
 *   high extractiveness and suppression. The engine will compute per-seat
 *   classifications; from military establishments the constraint appears
 *   highly extractive (denied winnability), while from civilians' perspective
 *   it is protective coordination. This divergence is the point.
 *
 * KEY AGENTS:
 *   - Civilian populations (global, powerless): protected by the constraint, dependent on its credibility
 *   - Military establishments (great powers, institutional): pay the extraction cost of incoherent missions
 *   - Strategic planners (powerful, identity-locked): bear the cognitive burden of planning under categorical impossibility
 *   - Nuclear weaponeers (powerful, technical): administer the constraint's technical impossibility
 *   - Political leadership (institutional, constrained): enforce the suppression of winnability discourse
 *   - Non-nuclear states (moderate, mobile): benefit from great-power constraint on total war
 *   - Disarmament advocates (moderate, excluded): would restructure the constraint entirely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.72).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Nuclear Deterrence: War Winnability Foreclosed (Unthinkable Reading)").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic_studies/international_relations/nuclear_deterrence").

domain_priors:requires_active_enforcement(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, '85df0e0a-fefe-414a-9086-c80a2513ef00').
narrative_ontology:cs_kernel_codification('85df0e0a-fefe-414a-9086-c80a2513ef00', formalized).
narrative_ontology:cs_authority_grounding('85df0e0a-fefe-414a-9086-c80a2513ef00', extraction).
narrative_ontology:cs_interpretation_layer_present('85df0e0a-fefe-414a-9086-c80a2513ef00').
narrative_ontology:cs_reading_relation('85df0e0a-fefe-414a-9086-c80a2513ef00', war_winnability_post_1945__countervailing_thinkable, forecloses).
narrative_ontology:cs_reading_relation('85df0e0a-fefe-414a-9086-c80a2513ef00', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('85df0e0a-fefe-414a-9086-c80a2513ef00', foundational, nuclear_war_mutual_destruction_certain).
narrative_ontology:cs_axiom_status(nuclear_war_mutual_destruction_certain, holdable).
narrative_ontology:cs_axiom_grounding('85df0e0a-fefe-414a-9086-c80a2513ef00', nuclear_war_mutual_destruction_certain, empirically_contingent).
narrative_ontology:cs_axiom('85df0e0a-fefe-414a-9086-c80a2513ef00', foundational, winnability_logically_impossible_under_mud).
narrative_ontology:cs_axiom_status(winnability_logically_impossible_under_mud, holdable).
narrative_ontology:cs_axiom_grounding('85df0e0a-fefe-414a-9086-c80a2513ef00', winnability_logically_impossible_under_mud, deontological).
narrative_ontology:cs_reference_frame('85df0e0a-fefe-414a-9086-c80a2513ef00', mutual_assured_destruction_constraint).
narrative_ontology:cs_drift_state('85df0e0a-fefe-414a-9086-c80a2513ef00', contemporary_great_power_competition, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('85df0e0a-fefe-414a-9086-c80a2513ef00', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, non_nuclear_states).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, strategic_planners).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, great_power_militaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, nuclear_weaponeers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected by the categorical impossibility of great-power total war, which nuclear weapons enforce. They cannot choose deterrence; they are shielded by it as a fact of physics and strategy. Their survival depends on the constraint holding—on the belief that nuclear war is unwinnable remaining credible.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations, beneficiary,
    powerless, biographical, trapped, universal).

% Undergo a categorical mission transformation: victory is no longer a coherent operational goal, only prevention of war. They maintain enormous arsenals and operational readiness, but planning for victory—the historic purpose of military establishments—becomes incoherent. This is enforced by the doctrine itself: any plan that assumes winnability is invalidated by mutual assured destruction. They pay the cost of maintaining this constraint: absence of a meaningful victory condition.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__deterrence_unthinkable, military_establishments, agenda_setter).

% Operate under a categorical contradiction: they are hired to plan for military victory, but the constraint makes victory categorically unachievable. Their professional identity fuses with the constraint's operation—they become defenders of deterrence stability, not pursuers of victory. The identity lock is profound: a planner who publicly argues for winnability undermines the deterrence consensus that protects civilians and sustains their institutional role.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, strategic_planners, payer,
    powerful, biographical, identity_locked, global).

% Maintain massive nuclear arsenals not to win wars but to prevent them. This inversion of military purpose is sustained by the constraint: they cannot credibly plan for victory, so they plan for escalation deterrence. The alternative—disarmament—would require politically impossible coordination among rivals. They are trapped between maintaining the constraint (which denies them victory) and abandoning it (which would make nuclear war thinkable and thus more likely).
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, great_power_militaries, payer,
    institutional, civilizational, constrained, global).

% Benefit from the great-power constraint on total war: they are largely protected from great-power conflict absorption through nuclear interdiction of escalation. Their exit option is limited nuclear-power status (technological and political path), but the constraint makes this option costly and unstable. The constraint protects them; entering it as nuclear powers would expose them to the same mission incoherence.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, non_nuclear_states, beneficiary,
    moderate, generational, mobile, global).

% Administer the technical maintenance of arsenals and the credibility of deterrence doctrine. They benefit from institutional funding and technical autonomy; they set the technical parameters that make winnability literally impossible (assured second-strike capability, redundant delivery systems). They are the structural experts whose knowledge is essential to the constraint's operation.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, nuclear_weaponeers, agenda_setter,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__deterrence_unthinkable, nuclear_weaponeers, beneficiary).

% Maintains the public and operational commitment to deterrence as unsayable and unthinkable. They suppress rhetorical and operational alternatives (first-strike planning, limited nuclear war scenarios, damage limitation) by framing them as irresponsible. This enforcement maintains the constraint's credibility—once winnability becomes sayable at the leadership level, deterrence belief weakens.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, political_leadership, agenda_setter,
    institutional, biographical, constrained, global).

% Argue that the constraint is unstable and that abolition is the only safe path. They would restructure the strategic logic entirely by removing nuclear weapons rather than accepting their deterrence logic. They are structurally excluded from strategic planning decision-making at the great-power level, kept out by the framing that deterrence stability requires continuity and credibility.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, disarmament_advocates, excluded,
    moderate, generational, constrained, global).

% Analyze and debate the constraint's coherence and implications. From this reading's perspective (deterrence-unthinkable), they model winnability as structurally foreclosed. They produce the intellectual rationale for the constraint and monitor deviations from it in doctrine and planning. Their analysis sustains the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, strategic_theorists, observer,
    powerful, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__deterrence_unthinkable, diffuse).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__deterrence_unthinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents great-power total war by making its conduct and planning categorically incoherent: mutual assured destruction means no outcome of a nuclear exchange constitutes victory for any party. The coordination problem solved is: 'How do rivals accept coexistence without military hierarchy or conquest?' The answer is: 'Make conquest impossible.' This is a negative coordination—agreement that a space (winnability) is closed.
% TRANSFER_FUNCTION: Transfers the operative goal of military establishments from victory to prevention. Military power is redirected from conquest and battlefield dominance to deterrence and stability maintenance. The extraction operates on military establishments themselves: they surrender the historic purpose of warfare (winning) and reorganize around an opposite goal (preventing war). This is enforced by the doctrinal fact that any military plan assuming winnability is invalidated by mutual retaliation.
% ABSENT_VOICES: Disarmament advocates and non-aligned countries that argue for alternative arrangements (abolition, conventional deterrence balancing) are excluded from great-power strategic planning. Military theorists who explore limited nuclear war or counterforce strategies are present but systematically delegitimized within the constraint's operational space. Populations of great powers who might prefer disarmament over deterrence are not consulted; their consent is assumed but unvoiced.
% DISAPPEARANCE_RATIONALE: If nuclear weapons were removed or rendered non-functional, great-power total war would become operationally thinkable again, and military establishments would reorganize around winning. The absence of the constraint would restructure strategic planning, alliance formation, and military doctrine. The entire post-1945 international order—great-power peace through mutual nuclear terror—depends on winnability remaining categorically foreclosed.
% FOUNDING_PROBLEM: After 1945, the technical fact of nuclear weapons created an asymmetry: total war between great powers with nuclear arsenals would destroy both, making victory incoherent. The founding problem was: 'How do we think about military strategy when winning is structurally impossible?' The reading instantiated here answers: 'We stop thinking about winning; we think about preventing the war entirely.'
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by strategic theorists (Schelling, Waltz, Brodie), declassified military doctrine (Assured Destruction, Mutual Assured Destruction), the absence of great-power total war since 1945, and structural analysis of nuclear arsenals (both superpowers maintained second-strike capability that made disarming first strikes impossible). Corroborated from OUTSIDE the military establishment by international relations scholars, historians, and policy analysts from non-benefiting seats (countries that would prefer alternative arrangements). The founding problem is contested by the sibling reading (countervailing_thinkable), which argues winnability remains structurally possible through selective targeting, but even that reading concedes the problem existed and shaped doctrine.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__deterrence_unthinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__deterrence_unthinkable, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured as the cost imposed on military establishments: the denial of a coherent victory condition is a real extraction—they maintain forces but cannot plan for success in the classical sense. This cost rises from 1945 (when nuclear weapons were novel and winnability was still being debated) through the Cold War (when doctrine hardened around assured destruction, locking in the extraction), and remains high (0.68) at present because military establishments continue to operate under this constraint despite possessing the technology to wage nuclear war. Suppression rises in parallel (1945: 0.35 → 2025: 0.72) because maintaining winnability-as-unthinkable requires active enforcement: declassification restrictions on first-strike plans, delegitimization of 'winnable nuclear war' scenarios, institutional suppression of strategic theorists who explore limited nuclear options. Theater rises from low (0.15 in 1945, when nuclear strategy was still being developed) to moderate (0.41 in 2025) because an increasing share of military activity is theatrical maintenance of deterrence credibility: weapons displays, doctrine announcements, simulated retaliation—activities that sustain belief in the constraint rather than materially change warfighting capability. The measurement grid is shared across all three metrics at each time point, with observational basis declared; the rises track historical events (Cuban Missile Crisis 1962, strategic arms buildup 1979, Cold War end 1991, contemporary great-power competition 2008–2025).
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces radical seat divergence. From the civilian seat (beneficiary): the constraint is protective, unambiguously good, survival-sustaining. From the military-establishment seat (payer): the constraint is extractive, a denial of professional purpose, coherence-destroying. From the strategic-planner seat (identity-locked payer): the constraint is both—protective of their status as guardians of deterrence, but corrosive to their professional identity as planners of military victory. Political leadership experiences it as a maintenance burden (suppression must be actively enforced to keep winnability unsayable). The engine computes each seat's classification from power, exit options, and the beneficiary/victim structure; seats with different power atoms, exit_options, and directional relationships to the constraint will compute to different types. Military establishments will compute as snare-adjacent or tangled_rope-victims; civilians will compute as beneficiaries of a coordination mechanism; strategic planners will compute as high-d payers with identity_locked exit, producing maximal extraction from their seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are identified as civilian populations and non-nuclear states: they receive protection from the impossibility of great-power total war, with zero cost (they cannot opt out; they do not pay). Their directionality is near 0.0 (full beneficiary). Victims are military establishments, strategic planners, and great-power militaries: they pay via the denial of winnability and the incoherence of traditional military planning. Their directionality is high (0.75–1.0, full target). The asymmetry is structural: the constraint redistributes not money but operational coherence—it takes coherence from military establishments and gives security to civilians. Strategic planners face the harshest extraction because their exit is identity-locked: a planner who publicly argues for winnability undermines the very constraint that sustains their institutional role. Their directionality sits at ~0.85 (high target, constrained exit tightening the screws). No directionality overrides are declared because the derivation chain produces accurate relationships: beneficiary (civilian) = low d; victim (military) = high d; identity_locked (planner) = d amplified toward full target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to think about military strategy when nuclear war is unwinnable) was solved by the constraint's establishment in doctrine and practice from roughly 1950–1962. At present (2025), the problem remains LIVE: great-power nuclear arsenals continue to exist, the structural asymmetry persists, and deterrence stability remains contested in strategic theory. The constraint has NOT undergone mandatrophy (death of the function). However, the theater_ratio rise (from 0.15 to 0.41) signals Goodhart drift: as winnability became unsayable at the political level, an increasing share of military activity became performative maintenance of credibility rather than functional adaptation to the strategic environment. The measurement series catches this drift: suppression rises faster than extractiveness, and theater rises across the interval. This pattern is consistent with a constraint that remains functionally needed (preventing great-power war) but increasingly dependent on theatrical maintenance (public reaffirmation of deterrence commitment, strategic exercises, doctrine announcements) rather than material deterrent improvements. The mandatrophy check passes: the constraint's classification as tangled_rope (coordination + extraction) is stable, not degraded into pure piton. Civilians still need the protection; military establishments still bear the extraction; the theaters sustaining it are growing but have not yet consumed the function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    winnability_structure_vs_doctrine,
    'Is winnability genuinely structurally impossible (physics + arsenals make it incoherent), or is winnability incoherent only within the doctrinal constraint (military establishments could operationally plan for victory if political constraints were removed)?',
    'Comparative analysis of declassified military plans (counterforce strategies, damage-limitation scenarios) across reading frameworks: do these plans represent genuine operational thinking about winnability, or are they performative compliance with deterrence doctrine while accepting incoherence?',
    'If winnability is doctrinal-only incoherent, the constraint is maintained by suppression of alternative planning; it is a snare or piton, not a tangled rope. If structurally incoherent, the constraint is sustained by physics; it is closer to a mountain with beneficiaries (false-summit candidate). The difference determines whether the constraint requires active enforcement or sustains itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(winnability_structure_vs_doctrine, empirical, 'Whether winnability incoherence is physical or doctrinal.').

omega_variable(
    mission_incoherence_extraction_mechanism,
    'Is the denial of winnability experienced as extraction by military establishments because they genuinely value victory (and are denied it), or because the institutional identity of militaries is fused with the pursuit of victory regardless of its achievability?',
    'Ethnographic and interview study of military strategic planning communities: do they experience winnability-denial as a loss of genuine capability, or as an identity shock (the work they were trained for is now incoherent)?',
    'If genuine capability loss, the extraction is economic or strategic (real reduction in operative space). If identity shock, the extraction is internalized; removal of the constraint would not restore coherence to the mission because the mission''s incoherence is baked into the institutional identity, not the technical possibility space. This affects whether exit from the constraint would resolve the cost or simply shift it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mission_incoherence_extraction_mechanism, empirical, 'Whether mission incoherence is technical or internalized identity fusion.').

omega_variable(
    deterrence_belief_fragility,
    'How credible is the constraint''s enforcement mechanism (the belief that nuclear war is unwinnable)? If deterrence belief collapses, does the constraint collapse with it, or is there a physical/technical foundation that would sustain winnability-impossibility even if political/doctrinal commitment wavered?',
    'Strategic analysis of feedback loops: if a great power announces a winnability strategy, does the second-strike capability of the other power remain sufficient to make that announcement incoherent? Or does the constraint depend on mutual agreement to treat winnability as unsayable?',
    'If the constraint is belief-dependent, it is fragile and highly sensitive to suppression failure or rhetorical drift. If it has technical foundation independent of belief, it is more robust. This affects classification at the piton margin: constraints dependent on theater without technical foundation slide toward piton; constraints with physical backing resist pitonization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_belief_fragility, empirical, 'Whether deterrence constraint is belief-dependent or physically rooted.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the deterrence-unthinkable reading logically foreclose the countervailing-thinkable reading, or can both coexist as live positions held by different military establishments?',
    'Doctrinal history: the Soviet Union and China have both held countervailing strategies while the U.S. held deterrence-unthinkable. Are these genuine coexisting alternatives, or is countervailing formally refuted by the physics of mutual arsenals?',
    'If deterrence-unthinkable forecloses countervailing, the readings are in formal logical contradiction and only one can be true. If they coexist, they are different parties'' operational commitments to the same kernel. The reading_relations block codes this as forecloses vs. coexists_with; the impact determines which is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between deterrence-unthinkable and countervailing-thinkable readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1945, 0.15).
narrative_ontology:measurement_basis(war__tr_t1945, observed).
narrative_ontology:measurement(war__tr_t1962, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1962, 0.28).
narrative_ontology:measurement_basis(war__tr_t1962, observed).
narrative_ontology:measurement(war__tr_t1979, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1979, 0.36).
narrative_ontology:measurement_basis(war__tr_t1979, observed).
narrative_ontology:measurement(war__tr_t1991, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1991, 0.33).
narrative_ontology:measurement_basis(war__tr_t1991, observed).
narrative_ontology:measurement(war__tr_t2008, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2008, 0.39).
narrative_ontology:measurement_basis(war__tr_t2008, observed).
narrative_ontology:measurement(war__tr_t2025, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2025, 0.41).
narrative_ontology:measurement_basis(war__tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1945, 0.42).
narrative_ontology:measurement_basis(war__be_t1945, observed).
narrative_ontology:measurement(war__be_t1962, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1962, 0.58).
narrative_ontology:measurement_basis(war__be_t1962, observed).
narrative_ontology:measurement(war__be_t1979, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1979, 0.65).
narrative_ontology:measurement_basis(war__be_t1979, observed).
narrative_ontology:measurement(war__be_t1991, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1991, 0.62).
narrative_ontology:measurement_basis(war__be_t1991, observed).
narrative_ontology:measurement(war__be_t2008, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2008, 0.66).
narrative_ontology:measurement_basis(war__be_t2008, observed).
narrative_ontology:measurement(war__be_t2025, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(war__be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement_basis(war__su_t1945, observed).
narrative_ontology:measurement(war__su_t1962, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1962, 0.58).
narrative_ontology:measurement_basis(war__su_t1962, observed).
narrative_ontology:measurement(war__su_t1979, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1979, 0.68).
narrative_ontology:measurement_basis(war__su_t1979, observed).
narrative_ontology:measurement(war__su_t1991, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1991, 0.65).
narrative_ontology:measurement_basis(war__su_t1991, observed).
narrative_ontology:measurement(war__su_t2008, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement_basis(war__su_t2008, observed).
narrative_ontology:measurement(war__su_t2025, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(war__su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__deterrence_unthinkable, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__deterrence_unthinkable, 0.12).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__rhetorical_contraction).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, strategic_stability_doctrine).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, cold_war_deterrence_balance).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, nuclear_command_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel war_winnability_post_1945, which is contested across three structural interpretations: deterrence_unthinkable (this story: winnability is categorically foreclosed), countervailing_thinkable (winnability remains possible through selective targeting), and rhetorical_contraction (winnability became unsayable rhetorically while operationally planned). Each reading instantiates a different constraint with different ε, beneficiary/victim structures, and types. They share a kernel (the post-1945 strategic reality of mutual nuclear arsenals) but diverge on the interpretation: what does that reality make possible or impossible? The three stories are linked via network.affects_constraints because each reading's credibility influences the others' operational space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__deterrence_unthinkable, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
