% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Technology-Contingent Total War Reachability (Reversible Contraction Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This story instantiates the 'contingent reachability' reading of the
 *   total_war_reachability_boundary kernel: total war's current infeasibility
 *   is not a permanent structural fact (the contraction reading) nor merely a
 *   low-probability but stable equilibrium (the dropping reading), but a
 *   scaffold resting on a specific, actively-eroding technological
 *   equilibrium — principally mutual second-strike survivability. That
 *   equilibrium is being invested against by the very states whose security
 *   establishments claim to be maintaining it: counterforce precision strike,
 *   missile defense, AI-compressed command-and-control, and hypersonic glide
 *   vehicles each independently threaten to restore first-strike viability,
 *   which is the mechanism by which total war would re-enter the feasible
 *   strategic menu. The theater_ratio rising over the interval reflects that
 *   an increasing share of 'deterrence maintenance' activity (modernization
 *   programs, doctrine reviews, arms-control theater) performs stability
 *   rather than producing it, even as the underlying capability balance
 *   shifts.
 *
 * KEY AGENTS:
 *   - states_investing_in_counterforce_and_missile_defense: institutional beneficiary/agenda_setter — funds the technology shift that could reverse the current contraction
 *   - states_developing_ai_enabled_c2_and_hypersonic_delivery: institutional beneficiary — compresses decision timelines, eroding warning-time stability
 *   - arms_control_epistemic_community: organized beneficiary/observer — professionally invested in the boundary's contested, managed status
 *   - global_civilian_populations: powerless, trapped payer — bears catastrophic downside with zero decision input
 *   - non_nuclear_states_in_contested_regions: moderate, constrained payer — lives inside the crisis-instability zones first affected by reversal
 *   - nuclear_weapon_states_command_authorities: institutional agenda_setter — administers the operational posture that currently holds the equilibrium
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.42).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.35).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, scaffold).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Technology-Contingent Total War Reachability (Reversible Contraction Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:has_sunset_clause(total_war_reachability_boundary__contingent_reachability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, '602fb715-4c48-4190-b66c-db77ec3a1695').
narrative_ontology:cs_kernel_codification('602fb715-4c48-4190-b66c-db77ec3a1695', distributed).
narrative_ontology:cs_authority_grounding('602fb715-4c48-4190-b66c-db77ec3a1695', distributed).
narrative_ontology:cs_reading_relation('602fb715-4c48-4190-b66c-db77ec3a1695', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('602fb715-4c48-4190-b66c-db77ec3a1695', total_war_reachability_boundary__dropping_reading, influences).
narrative_ontology:cs_axiom('602fb715-4c48-4190-b66c-db77ec3a1695', foundational, strategic_feasibility_is_technology_indexed).
narrative_ontology:cs_axiom_status(strategic_feasibility_is_technology_indexed, holdable).
narrative_ontology:cs_axiom_grounding('602fb715-4c48-4190-b66c-db77ec3a1695', strategic_feasibility_is_technology_indexed, empirically_contingent).
narrative_ontology:cs_axiom('602fb715-4c48-4190-b66c-db77ec3a1695', secondary, capability_investment_can_reverse_boundary_state).
narrative_ontology:cs_axiom_status(capability_investment_can_reverse_boundary_state, holdable).
narrative_ontology:cs_axiom_grounding('602fb715-4c48-4190-b66c-db77ec3a1695', capability_investment_can_reverse_boundary_state, empirically_contingent).
narrative_ontology:cs_reference_frame('602fb715-4c48-4190-b66c-db77ec3a1695', mutual_second_strike_survivability_equilibrium).
narrative_ontology:cs_drift_state('602fb715-4c48-4190-b66c-db77ec3a1695', post_2010_great_power_competition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('602fb715-4c48-4190-b66c-db77ec3a1695', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_counterforce_and_missile_defense).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, states_developing_ai_enabled_c2_and_hypersonic_delivery).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, arms_control_epistemic_community).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, global_civilian_populations).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_states_in_contested_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fund and field counterforce, missile-defense, and precision-strike programs whose declared purpose is deterrence maintenance, but whose structural effect is eroding the mutual-vulnerability floor that currently makes total war infeasible. They set the pace of the underlying technology equilibrium and can reverse the present contraction by fielding capabilities that restore first-strike viability.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_counterforce_and_missile_defense, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_counterforce_and_missile_defense, agenda_setter).

% Pursue compressed decision-timeline systems (AI-assisted command and control, hypersonic glide vehicles) that shrink warning time and blur the line between conventional and strategic escalation. They gain relative advantage from being early movers even though their programs are the specific mechanism by which the reachability boundary could shift back toward feasibility.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, states_developing_ai_enabled_c2_and_hypersonic_delivery, beneficiary,
    institutional, generational, arbitrage, global).

% Academics, think tanks, and treaty negotiators whose professional standing depends on total war remaining an object of active management rather than settled history. They benefit from the boundary's contingency being taken seriously (it justifies continued funding and relevance) while also being the analytical voice most likely to document the reversal risk honestly.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, arms_control_epistemic_community, beneficiary,
    organized, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, arms_control_epistemic_community, observer).

% Bear the entire downside if the technology equilibrium shifts and deterrence fails; have no voice in the technology investment decisions that determine whether the current contraction holds or reverses, and no exit from the consequences of a strategic miscalculation made by others.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, global_civilian_populations, payer,
    powerless, civilizational, trapped, universal).

% Live inside the theaters (contested straits, disputed borders, forward-deployed zones) where a technology-driven reachability shift would first manifest as crisis instability. They can hedge diplomatically or seek alliance guarantees but cannot alter the great-power technology trajectories that set the boundary they must live inside.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_states_in_contested_regions, payer,
    moderate, generational, constrained, regional).

% Hold launch authority and manage the operational posture that currently keeps mutual destruction credible and total war outside the feasible set. Their day-to-day choices about alert levels, doctrine, and modernization directly administer whether the current technological equilibrium holds or erodes, but they operate under domestic political and alliance pressures that constrain unilateral restraint.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, nuclear_weapon_states_command_authorities, agenda_setter,
    institutional, immediate, constrained, global).

% Would bear the consequences of any technology-driven reversal decades hence but have no seat in current investment or doctrine decisions; their interests are represented, if at all, only through the arms control community's advocacy.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, future_populations, excluded,
    powerless, civilizational, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__contingent_reachability_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__contingent_reachability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The current reachability boundary coordinates major powers around a shared technological equilibrium (mutual second-strike survivability) that makes total war infeasible without requiring anyone to renounce the underlying weapons or capability — a stability achieved through technology parity rather than treaty alone.
% TRANSFER_FUNCTION: Moves strategic risk from the present (where investing states gain relative military and political advantage from advanced capabilities) to the future (where the population bears the risk of the equilibrium shifting toward reachability), while research funding and prestige flow to the epistemic community that manages the boundary's contingent status.
% ABSENT_VOICES: Future populations who would live under a reversed equilibrium have no representation in current procurement, doctrine, or arms-control negotiation; non-nuclear regional states affected by crisis instability are consulted only marginally in great-power technology decisions.
% DISAPPEARANCE_RATIONALE: If the technological factors currently making total war infeasible were removed overnight (i.e., if a reversal occurred), nuclear command authorities and defense establishments would immediately rearrange doctrine, alert postures, and alliance commitments — a dramatic rearrangement. But whether the CONSTRAINT itself (the current low-reachability state) is fragile or robust is exactly the contested question this reading takes a position on: it holds the state is scaffold-like and reversible, not naturally fixed.
% FOUNDING_PROBLEM: The mutual-assured-destruction equilibrium was built to solve the problem that first-strike-capable major powers could otherwise fight and potentially win a total war; guaranteeing unacceptable retaliatory damage was intended to remove total war from the feasible strategic menu.
% FOUNDING_PROBLEM_CORROBORATION: Independent strategic studies scholars and some retired military planners attest the founding problem (first-strike viability) is being actively reopened by counterforce and hypersonic programs — a view corroborated outside the investing states' own defense establishments by nonproliferation researchers and, historically, by scientists' organizations warning about doomsday-clock movements. The investing states themselves characterize their programs as strengthening rather than eroding the equilibrium, which is the disputed self-assessment this reading treats skeptically.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, contested).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored as moderate and rising (0.22 to 0.50 projected) because the primary transfer is not direct rent extraction but a slow transfer of catastrophic risk from present investing states (who gain relative strategic advantage now) to future/global populations (who would bear the cost of a reversal). Suppression is moderate and initially declining post-Cold-War (0.50 to 0.34) reflecting reduced Cold War-era alert rigidity, then ticking back up (0.35 to 0.40 projected) as renewed great-power competition reintroduces doctrinal rigidity and escalation-management theater. Theater ratio rises steadily (0.30 to 0.62) because an increasing share of 'deterrence stability' activity — modernization justified as maintaining rather than eroding stability, arms control talks that produce declaratory rather than verifiable restraint — is performative relative to its stated stabilizing function. Accessibility collapse is moderate (0.40): alternatives to the current arms competition (verified restraint regimes, technology-sharing agreements) are not foreclosed but are increasingly costly to pursue given sunk investment. Resistance is moderate-high (0.55): the arms control community, some allied governments, and segments of domestic publics actively contest the trajectory, but lack veto power over the investing states' procurement decisions.
 *
 * PERSPECTIVAL GAP:
 *   From the investing states' seat, each individual capability (missile defense, precision strike, AI-C2) is locally rational deterrence maintenance or alliance reassurance — the engine would likely compute their seat close to a rope or scaffold they administer responsibly. From the payer seats (global populations, regional non-nuclear states), the same aggregate trajectory reads as an unmanaged drift toward reachability, structurally indistinguishable from a slow-motion tangled rope where coordination language covers an asymmetric risk transfer. This divergence is the point of the contingent-reachability reading: it holds that the boundary is a piton-with-reversal-potential, not a settled mountain, precisely because different seats disagree about whether the technology trajectory is stabilizing or destabilizing the equilibrium.
 *
 * DIRECTIONALITY LOGIC:
 *   States investing in destabilizing technologies are coded as beneficiaries (low d) because they capture relative strategic advantage, prestige, and domestic political capital from modernization now, while the catastrophic tail risk is deferred and diffused. Global civilian populations and non-nuclear regional states are coded as victims/payers (high d) because they have no influence over the investment decisions and would bear concentrated, immediate costs if the equilibrium reverses and deterrence fails. Command authorities are agenda_setters with constrained exit rather than pure beneficiaries — they administer the equilibrium but operate under domestic and alliance pressures that limit unilateral restraint, distinguishing them from the investing states that more freely choose to fund destabilizing programs.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification (with a declared but not yet legislated 'sunset' — the boundary is meant to last only as long as the current technology equilibrium, not indefinitely) prevents two mislabeling errors: (1) treating the current low-reachability state as a permanent mountain (the contraction reading's error, which would make investment in destabilizing technology invisible as a structural threat), and (2) treating the entire deterrence architecture as pure extraction with no coordination value (which would ignore that mutual survivability genuinely has prevented total war for eight decades). By naming the beneficiaries of the underlying technology race explicitly, this reading keeps the reversal risk visible as an authored structural fact rather than an unstated background assumption.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversal_threshold_uncertainty,
    'At what point does cumulative counterforce, missile defense, and AI-C2 capability actually cross the threshold at which a first strike becomes rationally viable, restoring total war to the feasible strategic set?',
    'Classified and unclassified net assessment of survivable second-strike force levels against fielded and projected counterforce/defense capability; historical calibration against Cold War-era stability-instability threshold estimates.',
    'If the threshold is far off, the current contraction functions closer to a durable piton with low near-term reversal risk; if the threshold is near or already crossed in some dyads (e.g., regional nuclear rivals), the scaffold framing understates urgency and the constraint may already be functioning as a tangled_rope or snare in those specific relationships.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversal_threshold_uncertainty, empirical, 'Uncertainty about the technical threshold at which capability accumulation restores first-strike viability.').

omega_variable(
    kernel_framing_choice_reachability_vs_probability,
    'Is the correct unit of analysis the FEASIBLE SET (can total war happen at all — this reading''s and the contraction reading''s shared frame) or the PROBABILITY DISTRIBUTION over outcomes (has the likelihood dropped even though the event remains reachable — the dropping reading''s frame)?',
    'This is a conceptual framing choice, not an empirical fact resolvable by data; it depends on whether one models strategic stability as a boundary condition (feasible/infeasible) or a continuous risk parameter. The choice was guided here by the source material''s explicit language of ''reachability'' and ''piton (atrophied capability),'' which presupposes a boundary/capability frame rather than a probability frame.',
    'Adopting the probability frame (dropping_reading) would reclassify this constraint as a rope with a risk parameter rather than a scaffold with a reversible boundary condition, changing which metrics (probability of use vs. technical feasibility) are treated as primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_reachability_vs_probability, conceptual, 'Whether reachability (boundary/feasible-set) or probability (risk parameter) is the correct analytical frame for this kernel.').

omega_variable(
    investing_states_self_perception_gap,
    'Do the states investing in counterforce, missile defense, and hypersonic/AI-C2 capabilities genuinely believe they are stabilizing deterrence, or do they knowingly accept destabilization risk in exchange for relative advantage?',
    'Declassified doctrine documents, statements by defense officials under legislative oversight, and comparison of stated doctrine against revealed procurement priorities over multi-decade windows.',
    'If genuine belief in stabilization, the beneficiary designation is structurally accurate but the agents'' own perspective would compute closer to rope/scaffold-administered-responsibly; if knowing risk acceptance, the arrangement more closely resembles a tangled_rope with the coordination story functioning partly as cover for extraction of relative strategic advantage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investing_states_self_perception_gap, conceptual, 'Whether investing states'' stabilization claims are sincere or a cover narrative for relative-advantage seeking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 1991, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1991, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1991, 0.3).
narrative_ontology:measurement(tota_tr_t2000, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(tota_tr_t2010, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2010, 0.45).
narrative_ontology:measurement(tota_tr_t2018, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2018, 0.5).
narrative_ontology:measurement(tota_tr_t2026, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2026, 0.55).
narrative_ontology:measurement(tota_tr_t2035, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2035, 0.62).

% Extraction over time
narrative_ontology:measurement(tota_be_t1991, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1991, 0.22).
narrative_ontology:measurement(tota_be_t2000, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(tota_be_t2010, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2010, 0.3).
narrative_ontology:measurement(tota_be_t2018, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2018, 0.36).
narrative_ontology:measurement(tota_be_t2026, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2026, 0.42).
narrative_ontology:measurement(tota_be_t2035, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2035, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1991, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1991, 0.5).
narrative_ontology:measurement(tota_su_t2000, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(tota_su_t2010, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2010, 0.36).
narrative_ontology:measurement(tota_su_t2018, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2018, 0.34).
narrative_ontology:measurement(tota_su_t2026, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2026, 0.35).
narrative_ontology:measurement(tota_su_t2035, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2035, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__contingent_reachability_reading, 0.12).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, nuclear_non_proliferation_regime).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, missile_defense_arms_race_dynamics).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'has nuclear deterrence made total war impossible.' The contraction_reading treats the post-1945 change as a permanent structural fact (mountain-flavored: the feasible set of strategic outcomes was irreversibly reduced). The dropping_reading treats it as a stable coordination equilibrium managing a persistently reachable outcome (rope-flavored: probability dropped, feasibility unchanged). This reading (contingent_reachability_reading) treats the current low-reachability state as technologically contingent and reversible (scaffold-flavored: a piton whose atrophied capability could be reactivated). Each carries a distinct epsilon appropriate to its own claim: near-zero for the settled-fact reading, low-moderate for the stable-equilibrium reading, moderate-and-rising for this reversible-scaffold reading. They are linked via affects_constraints rather than merged because forcing one epsilon to cover all three claims would violate epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__contingent_reachability_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
