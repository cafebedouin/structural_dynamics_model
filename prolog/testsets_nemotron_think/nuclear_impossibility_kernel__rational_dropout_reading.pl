% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__rational_dropout_reading, []).

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
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Nuclear Deterrence as Rational-Choice Constraint (Rational Dropout Reading)
 *   domain: strategic_studies/international_relations/nuclear_deterrence
 *
 * SUMMARY:
 *   The rational dropout reading of nuclear impossibility holds that nuclear
 *   weapons created a game-theoretic constraint: victory remains structurally
 *   possible (the weapons work, delivery systems reach targets, command
 *   chains function) but the cost-benefit calculus renders initiation
 *   irrational for any actor with something to lose. War is not physically
 *   impossible (contra structural_contraction_reading) nor is the threat
 *   inherently incredible (contra credibility_paradox_reading) — rather, war
 *   is dropped from the active consideration set of rational decision-makers.
 *   This reading powers the 'deterrence works' consensus among nuclear weapon
 *   states and their allies. It extracts compliance from non-nuclear states
 *   through the NPT bargain (foreswear weapons, receive security guarantees
 *   and peaceful use) while the disarmament obligation (Article VI) remains
 *   unfulfilled. The constraint requires active enforcement:
 *   non-proliferation sanctions, extended deterrence commitments,
 *   modernization programs, and the epistemic gatekeeping that defines
 *   'rational' strategic thought.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.72).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.78).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Deterrence as Rational-Choice Constraint (Rational Dropout Reading)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic_studies/international_relations/nuclear_deterrence").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, '4c65cdcb-e183-43d3-848d-a9f387fe846d').
narrative_ontology:cs_kernel_codification('4c65cdcb-e183-43d3-848d-a9f387fe846d', distributed).
narrative_ontology:cs_authority_grounding('4c65cdcb-e183-43d3-848d-a9f387fe846d', extraction).
narrative_ontology:cs_interpretation_layer_present('4c65cdcb-e183-43d3-848d-a9f387fe846d').
narrative_ontology:cs_reading_relation('4c65cdcb-e183-43d3-848d-a9f387fe846d', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c65cdcb-e183-43d3-848d-a9f387fe846d', nuclear_impossibility_kernel__credibility_paradox_reading, influences).
narrative_ontology:cs_axiom('4c65cdcb-e183-43d3-848d-a9f387fe846d', foundational, nuclear_war_costs_exceed_all_benefits).
narrative_ontology:cs_axiom_status(nuclear_war_costs_exceed_all_benefits, holdable).
narrative_ontology:cs_axiom_grounding('4c65cdcb-e183-43d3-848d-a9f387fe846d', nuclear_war_costs_exceed_all_benefits, empirically_contingent).
narrative_ontology:cs_axiom('4c65cdcb-e183-43d3-848d-a9f387fe846d', foundational, rational_actors_do_not_initiate_nuclear_war).
narrative_ontology:cs_axiom_status(rational_actors_do_not_initiate_nuclear_war, holdable).
narrative_ontology:cs_axiom_grounding('4c65cdcb-e183-43d3-848d-a9f387fe846d', rational_actors_do_not_initiate_nuclear_war, conventional).
narrative_ontology:cs_axiom('4c65cdcb-e183-43d3-848d-a9f387fe846d', secondary, mutual_vulnerability_is_stable).
narrative_ontology:cs_axiom_status(mutual_vulnerability_is_stable, holdable).
narrative_ontology:cs_axiom_grounding('4c65cdcb-e183-43d3-848d-a9f387fe846d', mutual_vulnerability_is_stable, conventional).
narrative_ontology:cs_reference_frame('4c65cdcb-e183-43d3-848d-a9f387fe846d', mutual_vulnerability_stability).
narrative_ontology:cs_drift_state('4c65cdcb-e183-43d3-848d-a9f387fe846d', post_cold_war_modernization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4c65cdcb-e183-43d3-848d-a9f387fe846d', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, deterrence_establishment).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, military_industrial_complexes).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_allies).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, civilian_populations).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, future_generations).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, disarmament_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_allies).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, mutual_vulnerability_stability).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, rational_actor_deterrence_theory).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_taboo_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear arsenals and set the terms of the non-proliferation regime. Benefit from the constraint freezing great power competition at a level they dominate. Bear modernization costs but extract security privilege and status. Exit would require disarmament — structurally constrained by prestige, alliance commitments, and domestic bureaucracies.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).

% Strategic intellectuals, think tanks, military planners, and doctrinal communities whose careers and epistemic authority depend on the rationality of deterrence. Their professional identity is fused with the constraint's logic — leaving the field means abandoning the framework that constitutes their expertise. Collect status, funding, and policy access.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, deterrence_establishment, beneficiary,
    organized, biographical, identity_locked, global).

% Defense contractors, nuclear labs, and procurement bureaucracies that receive sustained funding for modernization, life-extension, and new warhead programs. The rational dropout framing justifies continuous investment: if war is unthinkable but possible, the arsenal must be credible, modern, and survivable. Exit is mobile — they could pivot to conventional or other domains — but the nuclear mission is their most protected revenue stream.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, military_industrial_complexes, beneficiary,
    powerful, biographical, mobile, national).

% Non-nuclear allies under nuclear umbrellas (NATO, Japan, South Korea, Australia). Benefit from security guarantee without own arsenal costs. Pay through hosting obligations, political subordination to patron's deterrence posture, and foreclosed independent deterrent options. Exit is constrained — developing own weapons breaks alliance and triggers proliferation sanctions; relying on patron means accepting their risk calculus.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_allies, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_allies, payer).

% NPT non-nuclear parties that foreswore weapons in exchange for disarmament progress and peaceful use cooperation. Bear the constraint's costs: perpetual vulnerability to nuclear coercion, exclusion from great power decision-making, intrusive verification regimes, and opportunity costs of forgone deterrent. Exit is constrained — withdrawal triggers sanctions and isolation; compliance yields no visible disarmament progress.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, global).

% Populations in both nuclear and non-nuclear states who live under the risk of accidental, unauthorized, or deliberate nuclear use. Bear existential risk without consent or meaningful exit. The rational dropout framing treats their survival as a variable in a cost-benefit calculation they did not author. No individual exit exists; collective exit requires political mobilization that the constraint's logic delegitimizes as 'irrational.'
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, civilian_populations, payer,
    powerless, biographical, trapped, global).

% Inherit the constraint's long-term risks: environmental contamination from testing and production, erosion of arms control architecture, automation of launch systems, and the structural possibility that rational choice fails once. No voice in the original bargain; no exit from the inherited risk structure. The constraint's time horizon exceeds any democratic accountability mechanism.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Civil society, TPNW states, and normative entrepreneurs who argue the constraint is a trap, not a solution. Structurally excluded from the deterrence consensus — their proposals (de-alerting, no-first-use, elimination) are treated as category errors by the establishment. Their exclusion is maintained by the identity_locked epistemic community that defines 'serious' strategic thought.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, disarmament_advocates, excluded,
    moderate, generational, constrained, global).

% Historians, game theorists, philosophers, and independent analysts who evaluate the constraint from outside the operational logic. See the full structure: the genuine coordination (war prevention) and the asymmetric extraction (privilege maintenance). Their exit is analytical — they can change frameworks without material cost — but their influence on the constraint's operation is near zero.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents great power war by making the cost of nuclear conflict exceed any conceivable gain for rational actors. Solves the security dilemma among nuclear-armed states through mutual vulnerability rather than defense or offense.
% TRANSFER_FUNCTION: Transfers security autonomy and existential risk from nuclear weapon states to non-nuclear states and civilian populations; transfers resources from public treasuries to nuclear modernization programs; transfers political agency from disarmament advocates to deterrence establishment.
% ABSENT_VOICES: Future generations who inherit the risk structure without consent; civilian populations in the global south who bear testing fallout and environmental contamination but are absent from deterrence calculus; TPNW states whose legal elimination framework is treated as irrelevant by nuclear-armed states.
% DISAPPEARANCE_RATIONALE: If the rational dropout constraint vanished overnight — meaning actors no longer accepted that costs exceed benefits — great power war would return to the active policy menu. Nuclear weapon states would face pressure to use or lose arsenals; allies would proliferate; the taboo would collapse. The post-1945 order is organized around this constraint's operation.
% FOUNDING_PROBLEM: How to prevent great power war in the nuclear age without world government. The 1945-1949 monopoly period showed US atomic coercion was unstable; the Soviet test created mutual vulnerability. The rational dropout framing emerged (Brodie, Kahn, Schelling) as the intellectual solution: make war 'unthinkable' by proving it irrational.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing great power war) is attested as still live by nuclear weapon states and deterrence establishment — they cite 75 years without direct conflict. It is attested as substantially solved but the arrangement persisting as privilege maintenance by TPNW states, disarmament NGOs, and critical security scholars — they cite arms racing, new nuclear states, and the constraint's use to block disarmament. No neutral corroboration exists; the dispute is the constraint's operating condition.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint transfers existential risk and political autonomy to those who did not choose it, while concentrating security privilege in nine states. Suppression is higher (0.78) because alternatives — disarmament, common security, nuclear-weapon-free zones — are actively marginalized through institutional, epistemic, and material means. Theater ratio is moderate (0.48) because arms control rituals (START, NPT RevCons, P5 process) perform coordination while modernization proceeds. Accessibility collapse (0.65) reflects how the deterrence framework makes alternatives appear 'unrealistic' once internalized. Resistance (0.42) is real but structurally contained: TPNW exists but nuclear-armed states treat it as legally irrelevant; disarmament movements cycle but never reach decision-making centers.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear weapon state seat, the constraint is a Rope: genuine coordination preventing the war that would destroy them. From the non-nuclear weapon state seat, it is a Snare: a bargain they were coerced into (NPT) whose other party (disarmament) defaults perpetually. From the civilian population seat, it is a Mountain they cannot affect — but one that is human-made and maintained. The engine computes these divergences from the structural data; the claimed_type (tangled_rope) captures the system-level hybridity.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states and deterrence establishment are structural beneficiaries (d near 0.0-0.2): they collect security privilege, status, and resources. Extended deterrence allies are mixed (d ~0.4): genuine security benefit but constrained autonomy. Non-nuclear weapon states and civilian populations are targets (d ~0.8-1.0): bear risks and constraints without offsetting benefits. Future generations are maximally targeted (d=1.0): inherit all risk, zero voice. Disarmament advocates are excluded from the coordination game entirely — their exclusion is the enforcement mechanism. Analytical observers sit at d=0.5 (symmetric) but with zero causal leverage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing great power war) is contested: NWS say it's live; disarmament advocates say it's solved but the arrangement persists as privilege maintenance. The mandate (NPT Article VI) has atrophied — disarmament negotiations are performative while modernization accelerates. This is not a Piton because active enforcement continues and beneficiaries actively profit; it is a Tangled Rope whose coordination function is real but whose extraction has grown beyond the coordination floor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_rationality,
    'Is the cost-exceeds-benefit calculation a structural feature of nuclear physics/game theory (natural law) or a contingent framing that serves identifiable beneficiaries?',
    'Counterfactual analysis: if a state valued regime survival above all else (e.g., facing existential conventional defeat), would the calculus change? Historical near-use episodes (1962, 1983, 1999) suggest the ''rational'' boundary is contested in practice.',
    'If natural law, the constraint is a Mountain (with FSM risk from beneficiaries). If constructed, it is a Tangled Rope whose extraction is maintainable. The FSM signature would trigger if Mountain is claimed with beneficiaries declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_rationality, conceptual, 'Whether rational dropout reflects physics or power.').

omega_variable(
    coordination_extraction_boundary,
    'How much of the measured extraction is necessary coordination overhead (the price of mutual vulnerability stability) versus discretionary privilege maintenance?',
    'Comparative analysis: what minimum institutional structure would sustain mutual vulnerability without the current extraction profile (modernization beyond survivability, extended deterrence as political control, NPT as non-proliferation only)?',
    'If most extraction is discretionary, the constraint is closer to Snare. If most is coordination cost, it is a genuine Tangled Rope. The Boltzmann floor for enforcement_mechanism coordination type (0.10) provides a benchmark.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Separating necessary coordination cost from extractive surplus.').

omega_variable(
    rationality_universality,
    'Does the ''rational actor'' premise hold across all nuclear-armed states and future proliferators, or is it a culturally specific strategic culture projected as universal?',
    'Examine divergent nuclear doctrines (e.g., Russian ''escalate to de-escalate,'' Pakistani tactical first-use, Israeli opacity, North Korean regime survival calculus) for evidence that the cost-benefit threshold varies systematically.',
    'If rationality is not universal, the constraint''s coordination function is fragile — it works only among a specific epistemic community. This would increase effective extraction for states outside that community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_universality, empirical, 'Whether the rational actor model generalizes across nuclear decision-makers.').

omega_variable(
    committer_frame_ambiguity,
    'This constraint is one reading (rational_dropout_reading) of the nuclear_impossibility_kernel. The sibling readings (structural_contraction_reading, credibility_paradox_reading) make different structural claims about the same kernel. Where exactly is the disagreement located?',
    'Map the structural delta: structural_contraction says war is physically unreachable (M-set exclusion); rational_dropout says war is reachable but dropped from consideration (M-set inclusion, active-set exclusion); credibility_paradox says the threat to use is structurally incredible. The disagreement is on the constraint''s *modal status*: impossible vs. irrational vs. incredible.',
    'If the kernel admits multiple stable readings, no single reading can claim Mountain status — the contest itself proves the constraint is not a natural law. This routes to the FSM omega above.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Kernel reading decomposition: where the three readings structurally diverge.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the suppression of disarmament alternatives structural (institutional barriers, treaty obligations) or internalized (strategic elites genuinely believe alternatives are impossible)?',
    'Post-Cold War natural experiment: when structural barriers briefly lowered (1989-1995), did alternatives advance? The rapid return to modernization suggests internalized suppression — the deterrence establishment''s identity_locked exit means they cannot conceive alternatives even when politically possible.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint reproduces itself through the cognitive architecture of its administrators.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Structural vs. internalized suppression in the deterrence epistemic community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nuc_imp_rat_drop_tr_t1945, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(nuc_imp_rat_drop_tr_t1955, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1955, 0.15).
narrative_ontology:measurement(nuc_imp_rat_drop_tr_t1965, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1965, 0.25).
narrative_ontology:measurement(nuc_imp_rat_drop_tr_t1975, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1975, 0.35).
narrative_ontology:measurement(nuc_imp_rat_drop_tr_t1985, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1985, 0.4).
narrative_ontology:measurement(nuc_imp_rat_drop_tr_t1995, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(nuc_imp_rat_drop_tr_t2005, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(nuc_imp_rat_drop_tr_t2015, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2015, 0.44).
narrative_ontology:measurement(nuc_imp_rat_drop_tr_t2025, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(nuc_imp_rat_drop_be_t1945, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(nuc_imp_rat_drop_be_t1955, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1955, 0.45).
narrative_ontology:measurement(nuc_imp_rat_drop_be_t1965, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(nuc_imp_rat_drop_be_t1975, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(nuc_imp_rat_drop_be_t1985, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(nuc_imp_rat_drop_be_t1995, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(nuc_imp_rat_drop_be_t2005, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(nuc_imp_rat_drop_be_t2015, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(nuc_imp_rat_drop_be_t2025, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(nuc_imp_rat_drop_su_t1945, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(nuc_imp_rat_drop_su_t1955, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1955, 0.55).
narrative_ontology:measurement(nuc_imp_rat_drop_su_t1965, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1965, 0.65).
narrative_ontology:measurement(nuc_imp_rat_drop_su_t1975, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1975, 0.7).
narrative_ontology:measurement(nuc_imp_rat_drop_su_t1985, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1985, 0.75).
narrative_ontology:measurement(nuc_imp_rat_drop_su_t1995, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(nuc_imp_rat_drop_su_t2005, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(nuc_imp_rat_drop_su_t2015, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(nuc_imp_rat_drop_su_t2025, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__rational_dropout_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__rational_dropout_reading, 0.12).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__credibility_paradox_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, npt_bargain).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_architecture).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_modernization_programs).

% DUAL FORMULATION NOTE:
% This constraint (rational_dropout_reading) and its siblings (structural_contraction_reading, credibility_paradox_reading) form a constraint family decomposing the nuclear_impossibility_kernel. Each reading has a distinct ε: structural_contraction (low ε, Mountain), credibility_paradox (high ε, Snare/Tangled Rope), rational_dropout (moderate-high ε, Tangled Rope). The kernel's colloquial label 'nuclear deterrence makes war impossible' conflates three structurally distinct claims with different beneficiary/victim structures and enforcement requirements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_impossibility_kernel__rational_dropout_reading, institutional, 0.15).
constraint_indexing:directionality_override(nuclear_impossibility_kernel__rational_dropout_reading, organized, 0.35).
constraint_indexing:directionality_override(nuclear_impossibility_kernel__rational_dropout_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
