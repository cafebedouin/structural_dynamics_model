% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contingent_reachability_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Total War Reachability — Technology-Contingent Reading
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This reading holds that total war reachability is technology-dependent.
 *   The post-Cold War contraction in strategic reachability is a piton — an
 *   atrophied capability maintained largely through declaratory theater and
 *   institutional inertia, not functional necessity. Technological change
 *   (hypersonics, AI targeting, cyber-nuclear entanglement, directed energy)
 *   could reverse the contraction and reopen the strategic space for winnable
 *   total war among nuclear-armed powers. The constraint currently operates
 *   as a degraded Snare/Rope hybrid: states invest in destabilizing
 *   technologies (beneficiaries) while populations bear existential risk if
 *   deterrence fails (victims). The high theater ratio reflects that current
 *   posturing (arsenal sizes, declaratory policy, exercise patterns) exceeds
 *   functional deterrence requirements — the constraint persists because no
 *   state wants to be seen dismantling the taboo, not because the taboo is
 *   structurally necessary.
 *
 * KEY AGENTS:
 *   - destabilizing_technology_states: Primary beneficiaries (institutional/arbitrage) — invest in capabilities that could reopen total war space
 *   - defense_industrial_complexes: Secondary beneficiaries (organized/arbitrage) — profit from modernization programs justified by deterrence maintenance
 *   - civilian_populations_under_deterrence_failure: Primary victims (powerless/trapped) — bear existential risk if the constraint collapses
 *   - non_nuclear_weapon_states: Secondary victims (moderate/constrained) — excluded from strategic decisions but bear collateral consequences
 *   - nuclear_armed_great_powers: Agenda setters (institutional/constrained) — administer the constraint, could change it but face identity-locked institutional inertia
 *   - strategic_analysts: Observers (analytical/analytical) — track the technological trajectory and doctrinal shifts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.32).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.15).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, piton).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Total War Reachability — Technology-Contingent Reading").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, 'deef46d8-02db-4311-9c7d-44f0af21e64f').
narrative_ontology:cs_kernel_codification('deef46d8-02db-4311-9c7d-44f0af21e64f', distributed).
narrative_ontology:cs_authority_grounding('deef46d8-02db-4311-9c7d-44f0af21e64f', practice).
narrative_ontology:cs_interpretation_layer_present('deef46d8-02db-4311-9c7d-44f0af21e64f').
narrative_ontology:cs_reading_relation('deef46d8-02db-4311-9c7d-44f0af21e64f', total_war_reachability_boundary__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('deef46d8-02db-4311-9c7d-44f0af21e64f', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_axiom('deef46d8-02db-4311-9c7d-44f0af21e64f', foundational, reachability_is_technology_contingent).
narrative_ontology:cs_axiom_status(reachability_is_technology_contingent, holdable).
narrative_ontology:cs_axiom_grounding('deef46d8-02db-4311-9c7d-44f0af21e64f', reachability_is_technology_contingent, empirically_contingent).
narrative_ontology:cs_axiom('deef46d8-02db-4311-9c7d-44f0af21e64f', foundational, current_contraction_is_atrophied_not_permanent).
narrative_ontology:cs_axiom_status(current_contraction_is_atrophied_not_permanent, holdable).
narrative_ontology:cs_axiom_grounding('deef46d8-02db-4311-9c7d-44f0af21e64f', current_contraction_is_atrophied_not_permanent, empirically_contingent).
narrative_ontology:cs_reference_frame('deef46d8-02db-4311-9c7d-44f0af21e64f', mutual_vulnerability_equilibrium).
narrative_ontology:cs_drift_state('deef46d8-02db-4311-9c7d-44f0af21e64f', post_cold_war_technological_trajectory, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('deef46d8-02db-4311-9c7d-44f0af21e64f', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, destabilizing_technology_states).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, defense_industrial_complexes).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations_under_deterrence_failure).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contingent_reachability_reading, technological_determinism_in_strategic_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the nuclear deterrence regime through declaratory policy, arsenal posture, and crisis management. They could change the constraint (reduce arsenals, adopt no-first-use, negotiate arms control) but face identity-locked institutional inertia: nuclear status is constitutive of great power identity, and the strategic bureaucracy is organized around deterrence maintenance. Exit would require redefining national security identity.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, nuclear_armed_great_powers, agenda_setter,
    institutional, generational, constrained, global).

% Invest in hypersonic delivery, AI-enabled targeting, cyber-nuclear entanglement, and directed energy weapons. These capabilities could reopen the total war strategic space by enabling disarming first strikes or undermining second-strike assurance. They benefit either way: if the space reopens, they gain strategic advantage; if not, they extract modernization funding and bargaining leverage. Exit is arbitrage-grade — they can pivot investments across domains.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, destabilizing_technology_states, beneficiary,
    institutional, biographical, arbitrage, global).

% Design, build, and sustain nuclear modernization programs (new warheads, delivery systems, command/control). They capture the extraction from state budgets justified by deterrence maintenance. They shape the constraint through lobbying, revolving-door personnel, and threat-inflation narratives. Exit is arbitrage-grade — they can diversify into conventional or commercial markets.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, defense_industrial_complexes, beneficiary,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, defense_industrial_complexes, agenda_setter).

% Bear the existential risk of deterrence failure (nuclear war casualties, nuclear winter, civilizational collapse). They have zero strategic agency, no exit from the geographic scope of nuclear effects, and no voice in the decisions that set arsenal postures or crisis thresholds. Their situation is structural hostage-taking: survival depends on a coordination equilibrium they cannot influence.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations_under_deterrence_failure, payer,
    powerless, biographical, trapped, global).

% Excluded from nuclear decision-making but bear collateral consequences: nuclear winter effects, economic disruption, refugee flows, and normative pressure (NPT obligations without disarmament reciprocity). Some pursue latent capability (Japan, Germany) as constrained exit; others lead disarmament advocacy (TPNW). Their constraint is the asymmetry of risk without agency.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, global).

% Track technological trajectories, doctrinal shifts, and crisis stability. They produce the knowledge that informs (or fails to inform) policy. Their situation is epistemic: they see the full structure but occupy no seat in the game. Some become agenda_setters via government advisory roles (secondary_role not declared — that transition is temporal, not simultaneous).
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, strategic_analysts, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Deterrence stability among nuclear-armed powers: preventing total war by making the cost of escalation unacceptable to all parties. The constraint coordinates mutual vulnerability as a substitute for trust.
% TRANSFER_FUNCTION: Moves state resources (modernization budgets, industrial capacity, scientific talent) into nuclear maintenance and modernization, while moving existential risk onto civilian populations. The transfer is probabilistic: states pay continuously; populations pay catastrophically if the equilibrium fails.
% ABSENT_VOICES: Future generations (who inherit the risk without having participated in the bargain), populations in the global south (who bear disproportionate nuclear winter effects), and the disarmed (states that gave up nuclear weapons or never acquired them but still bear the systemic risk). They are absent because they hold no nuclear veto and no strategic bureaucracy represents them.
% DISAPPEARANCE_RATIONALE: If the total war taboo and its deterrence architecture vanished overnight, nuclear-armed powers would rapidly reconfigure arsenals for warfighting (counterforce targeting, lower yields, integration with conventional forces), crisis stability would collapse, and the probability of nuclear use would rise sharply. The world would rearrange into a multipolar security dilemma with usable nuclear weapons.
% FOUNDING_PROBLEM: The 1945-1962 period revealed that nuclear-armed great powers could not safely coexist without a mutual vulnerability constraint: the Berlin and Cuban crises demonstrated that escalation dominance incentives made total war reachable unless both sides accepted assured destruction.
% FOUNDING_PROBLEM_CORROBORATION: The nuclear-armed powers attest the problem is still live (citing new threats, technological uncertainty). Disarmament advocates and TPNW states attest the problem is dead (mutual vulnerability is a choice, not a necessity; the constraint persists as identity maintenance). Independent strategic analysts (e.g., Perkovich, Acton, Narang) attest it is contested: mutual vulnerability remains necessary for current arsenals/postures but technological change could alter the calculation — corroboration from outside the beneficiary set exists but is divided.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).
:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32) is moderate because the constraint's current operation costs states heavily (modernization programs) while the extraction from populations is probabilistic (deterrence failure risk). The declining trajectory from 1985 reflects the post-Cold War drawdown and reduced alert postures. Theater ratio (0.68) is high and rising: declaratory policy, arsenal postures, and exercise patterns increasingly exceed what is needed for credible minimum deterrence — the gap is performative maintenance of the total war taboo. Suppression (0.15) is low: the constraint does not actively coerce compliance; states comply because the alternative (uncontrolled escalation) is worse, not because enforcement machinery suppresses exit. Accessibility collapse (0.25) is low: alternative arrangements (treaty regimes, confidence-building measures, technological arms control) remain conceptually accessible. Resistance (0.45) is moderate: disarmament advocacy, treaty movements (TPNW), and public opposition exist but have not shifted state behavior.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear-armed powers' seat (agenda_setter, institutional, constrained exit), the constraint appears as a necessary evil — the taboo must be maintained because the technology of reversal exists in principle. From the civilian population seat (powerless, trapped), the constraint is a permanent hostage situation — their survival depends on a deterrence equilibrium they cannot influence. From the destabilizing technology states' seat (institutional, arbitrage), the constraint is an opportunity — investing in hypersonics, AI, and cyber capabilities positions them to benefit if the strategic space reopens. The engine computes these divergences from the declared power/exit structures.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-armed great powers (agenda_setters, institutional power, constrained exit) are structurally symmetric — they both bear modernization costs and benefit from deterrence stability. Their directionality is near 0.5 (costs ≈ benefits). Destabilizing technology states (beneficiaries, institutional power, arbitrage exit) gain from capability investment regardless of whether the space reopens — d near 0.2 (net beneficiary). Defense industrial complexes (beneficiaries, organized power, arbitrage exit) capture procurement revenue — d near 0.1. Civilian populations (victims, powerless, trapped) bear catastrophic risk with zero exit — d near 1.0. Non-nuclear weapon states (victims, moderate power, constrained exit) bear collateral risk without strategic agency — d near 0.8.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate (preventing total war among nuclear powers) has partially succeeded — total war has not occurred. But the mandate has atrophied into a ritual: the taboo is maintained theatrically while the underlying technological conditions that made it necessary are changing. The constraint is not resolved (mandatrophy_resolved = false) because the risk of reversal is real; it is a scaffold that does not know it is a scaffold, maintained as a piton. The classification prevents mislabeling this as pure coordination (rope) because the extraction from populations is real and asymmetric, and prevents mislabeling as pure extraction (snare) because the coordination function (deterrence stability) is genuine, if increasingly performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingent_reachability,
    'Is total war reachability a technology-contingent boundary (this reading) or a permanently contracted space (contraction_reading) or a deterrence-maintained equilibrium (dropping_reading)?',
    'Track technological trajectory in hypersonic delivery, AI-enabled targeting, cyber-nuclear entanglement, and directed energy weapons; assess whether any trajectory reopens the strategic space for winnable total war among nuclear-armed powers.',
    'If technology reopens the space, this reading''s scaffold classification is vindicated; if space remains closed regardless of tech, contraction_reading is correct; if deterrence equilibrium holds without tech reversal, dropping_reading prevails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contingent_reachability, empirical, 'Which kernel reading correctly captures the structural nature of total war reachability.').

omega_variable(
    piton_vs_scaffold_boundary,
    'Is the current constraint a piton (atrophied capability maintained theatrically) or a scaffold (genuinely temporary, with an implicit sunset via technological change)?',
    'Measure whether states actively maintain total war capabilities (countervalue targeting, large arsenals, warfighting doctrine) or have allowed them to atrophy while retaining declaratory posture; track doctrinal documents, procurement, and exercise patterns.',
    'Piton: extraction persists via institutional inertia despite capability atrophy; Scaffold: constraint genuinely awaits technological transition. Distinction determines whether current posture is performative maintenance or adaptive positioning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_vs_scaffold_boundary, conceptual, 'Whether the constraint''s current form is inertial performance or transitional staging.').

omega_variable(
    extraction_referent_ambiguity,
    'Does the constraint extract from populations (risk of deterrence failure) or from states (maintaining obsolete arsenals)?',
    'Cost-accounting of nuclear modernization programs vs. risk-weighted casualty estimates under deterrence failure scenarios; compare state budget burdens to population risk profiles.',
    'If state budgets bear the extraction, beneficiaries include defense industries; if populations bear the risk, extraction is diffuse and catastrophic. Changes the victim/beneficiary structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_referent_ambiguity, empirical, 'Where the constraint''s extraction actually lands — state treasuries or civilian lives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 1985, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twrb_crr_tr_t1985, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(twrb_crr_tr_t1995, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1995, 0.45).
narrative_ontology:measurement(twrb_crr_tr_t2005, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2005, 0.55).
narrative_ontology:measurement(twrb_crr_tr_t2015, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2015, 0.62).
narrative_ontology:measurement(twrb_crr_tr_t2025, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2025, 0.68).

% Extraction over time
narrative_ontology:measurement(twrb_crr_be_t1985, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(twrb_crr_be_t1995, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(twrb_crr_be_t2005, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(twrb_crr_be_t2015, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(twrb_crr_be_t2025, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2025, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(twrb_crr_su_t1985, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(twrb_crr_su_t1995, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1995, 0.22).
narrative_ontology:measurement(twrb_crr_su_t2005, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2005, 0.18).
narrative_ontology:measurement(twrb_crr_su_t2015, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2015, 0.16).
narrative_ontology:measurement(twrb_crr_su_t2025, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__contingent_reachability_reading, 0.12).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, nuclear_deterrence_stability).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, arms_control_treaty_regime).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, strategic_modernization_programs).

% DUAL FORMULATION NOTE:
% This reading decomposes the kernel total_war_reachability_boundary by fixing the technological contingency axis. contraction_reading fixes the axis at 'permanently contracted'; dropping_reading fixes it at 'probabilistically dropped but structurally stable.' Each reading has its own ε, beneficiaries, and victims. They are linked because the kernel's natural-language label ('total war reachability') conflates them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__contingent_reachability_reading, institutional, 0.55).
constraint_indexing:directionality_override(total_war_reachability_boundary__contingent_reachability_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
