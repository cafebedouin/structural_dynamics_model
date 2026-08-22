% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__space_contraction_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Nuclear-Induced Total War Impossibility (Space Contraction Reading)
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This constraint story instantiates the space_contraction_reading of the
 *   total_war_possibility_space kernel: nuclear weapons removed total war
 *   from the strategically thinkable, not merely from the preferable. The
 *   constraint operates as a categorical impossibility — the planning space
 *   for great-power total war has contracted to zero. This generates
 *   institutional atrophy: mobilization doctrines disappear, general staff
 *   war-gaming for great-power conflict ceases, and strategic studies shifts
 *   to sub-nuclear domains. The constraint is claimed as Mountain
 *   (emerges_naturally: true) because it presents as physical law (mutual
 *   assured destruction makes total war suicide). However, declared
 *   beneficiaries (nuclear_great_powers,
 *   conventional_military_industrial_complex,
 *   strategic_studies_establishment) and victims (non_nuclear_states,
 *   total_war_mobilization_bureaucracies, civil_defense_infrastructure)
 *   create FSM ambiguity: the Mountain claim may be a false summit that
 *   freezes the strategic hierarchy in nuclear powers' favor. The
 *   constraint's ε (0.78) is assessed from the abolitionist reading's lights
 *   on the standing arrangement — the existing nuclear order — not from the
 *   reading's endorsed alternative.
 *
 * KEY AGENTS:
 *   - nuclear_great_powers: Primary beneficiaries (institutional/global/arbitrage) — freeze strategic hierarchy, extract security rent
 *   - conventional_military_industrial_complex: Beneficiary (organized/global/mobile) — shifts procurement to high-tech conventional, avoids mobilization costs
 *   - strategic_studies_establishment: Beneficiary (organized/global/mobile) — field reorients to deterrence theory, sub-nuclear domains, career capital preserved
 *   - non_nuclear_states: Primary victims (powerless/regional/trapped) — denied total-war deterrent, subject to nuclear coercion without reciprocal capability
 *   - total_war_mobilization_bureaucracies: Victims (moderate/national/trapped) — institutional mission evaporates, expertise atrophies, budget authority collapses
 *   - civil_defense_infrastructure: Victims (powerless/national/trapped) — rendered obsolete by physics, maintained only as theater, funding diverted
 *   - analytical_observer: Observer (analytical/civilizational/analytical) — sees full structure, measures divergence between claimed Mountain and computed extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.78).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.85).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Nuclear-Induced Total War Impossibility (Space Contraction Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '1665f054-b69c-4f8d-a7cb-a7d4b07315ea').
narrative_ontology:cs_kernel_codification('1665f054-b69c-4f8d-a7cb-a7d4b07315ea', implicit).
narrative_ontology:cs_authority_grounding('1665f054-b69c-4f8d-a7cb-a7d4b07315ea', extraction).
narrative_ontology:cs_interpretation_layer_present('1665f054-b69c-4f8d-a7cb-a7d4b07315ea').
narrative_ontology:cs_reading_relation('1665f054-b69c-4f8d-a7cb-a7d4b07315ea', total_war_possibility_space__deterrence_equilibrium_reading, forecloses).
narrative_ontology:cs_reading_relation('1665f054-b69c-4f8d-a7cb-a7d4b07315ea', total_war_possibility_space__nuclear_taboo_reading, influences).
narrative_ontology:cs_axiom('1665f054-b69c-4f8d-a7cb-a7d4b07315ea', foundational, total_war_categorically_impossible).
narrative_ontology:cs_axiom_status(total_war_categorically_impossible, holdable).
narrative_ontology:cs_axiom_grounding('1665f054-b69c-4f8d-a7cb-a7d4b07315ea', total_war_categorically_impossible, empirically_contingent).
narrative_ontology:cs_axiom('1665f054-b69c-4f8d-a7cb-a7d4b07315ea', foundational, strategic_planning_space_contracted_to_zero).
narrative_ontology:cs_axiom_status(strategic_planning_space_contracted_to_zero, holdable).
narrative_ontology:cs_axiom_grounding('1665f054-b69c-4f8d-a7cb-a7d4b07315ea', strategic_planning_space_contracted_to_zero, empirically_contingent).
narrative_ontology:cs_reference_frame('1665f054-b69c-4f8d-a7cb-a7d4b07315ea', pre_nuclear_total_war_planning_space).
narrative_ontology:cs_drift_state('1665f054-b69c-4f8d-a7cb-a7d4b07315ea', contemporary_post_cold_war, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1665f054-b69c-4f8d-a7cb-a7d4b07315ea', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, nuclear_great_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, conventional_military_industrial_complex).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, strategic_studies_establishment).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, total_war_mobilization_bureaucracies).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, civil_defense_infrastructure).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, nuclear_revolution_in_military_affairs).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, absolute_weapon_doctrine).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, strategic_paradox_of_great_power_peace).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear arsenals that make great-power total war physically suicidal. They benefit from a frozen strategic hierarchy where their security is guaranteed by physics rather than competition. They extract security rent from non-nuclear states and avoid mobilization costs. Their exit is arbitrage-grade: they could disarm but would lose hierarchy position; the constraint subsidizes their position.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_great_powers, beneficiary,
    institutional, civilizational, arbitrage, global).

% The total-war contraction shifted procurement from mass mobilization systems to high-tech conventional platforms (precision strike, ISR, cyber). This avoids the overhead of maintaining mobilization industrial base while creating new profit centers. They are mobile: could pivot back to mobilization production if constraint relaxed, but current portfolio is optimized for sub-nuclear competition.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, conventional_military_industrial_complex, beneficiary,
    organized, generational, mobile, global).

% The field reoriented from total-war planning to deterrence theory, escalation management, and sub-nuclear domains. Career capital (deterrence frameworks, nuclear strategy curricula) was preserved and expanded. They are mobile: could revive total-war studies if demanded, but institutional incentives (funding, prestige, policy access) align with current constraint.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_studies_establishment, beneficiary,
    organized, biographical, mobile, global).

% Denied the total-war deterrent option that nuclear states possess. Subject to nuclear coercion (extended deterrence, negative security assurances) without reciprocal capability. The constraint extracts their strategic autonomy: they cannot credibly threaten total war in defense, must rely on patron nuclear umbrellas. Exit is trapped: acquiring nuclear weapons invites sanctions/preventive war; remaining non-nuclear accepts structural vulnerability.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, non_nuclear_states, payer,
    powerless, generational, trapped, regional).

% Institutions built for mass mobilization (conscription systems, industrial conversion plans, strategic stockpiles, civil-military coordination staffs) lost their mission when total war became unthinkable. Expertise atrophied, budget authority collapsed, personnel reassigned or retired. They are trapped: the constraint eliminated their raison d'être; reconstituting would require political will and decades of institution-building that no current actor supports.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, total_war_mobilization_bureaucracies, payer,
    moderate, biographical, trapped, national).

% Shelter systems, evacuation plans, fallout monitoring, and public preparedness programs were rendered obsolete by the physics of thermonuclear weapons (no meaningful protection against multi-megaton strikes). Maintained only as theater (theater_ratio 0.12) to signal societal resilience. Funding diverted to other priorities. They are trapped: the constraint made their function physically impossible; no exit to a meaningful civil defense against nuclear total war.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, civil_defense_infrastructure, payer,
    powerless, generational, trapped, national).

% Sees the full structure: the constraint operates as genuine physics (Mountain) for great powers but as extractive imposition for non-nuclear states and atrophied bureaucracies. Measures the divergence between claimed Mountain and computed per-seat χ. Has analytical exit: can change frameworks, compare readings, detect FSM signatures. Neither collects nor pays.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__space_contraction_reading, nuclear_great_powers).
narrative_ontology:fixing_cost_class(total_war_possibility_space__space_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents great-power total war by making it physically suicidal (mutual assured destruction). Solves the security dilemma at the nuclear threshold: no great power can rationally initiate total war because retaliation is guaranteed and catastrophic.
% TRANSFER_FUNCTION: Moves strategic autonomy and deterrent capability from non-nuclear states to nuclear great powers; moves institutional purpose and budget authority from total-war mobilization bureaucracies to conventional military-industrial complex; moves strategic studies field capital from total-war planning to deterrence/sub-nuclear domains.
% ABSENT_VOICES: Non-nuclear states (especially Global South) would object to being structurally denied a total-war deterrent option while nuclear states retain it. Total-war mobilization bureaucracies (now dissolved or repurposed) would object to mission elimination without democratic deliberation. Civil defense professionals would object to theater maintenance of obsolete systems. These voices are excluded by the physics of the constraint itself — the constraint makes their objections strategically irrelevant.
% DISAPPEARANCE_RATIONALE: If the categorical impossibility of total war vanished overnight (e.g., through perfect missile defense, disarmament, or physics breakthrough), nuclear great powers would lose their hierarchy freeze and face renewed great-power security competition; non-nuclear states would regain theoretical total-war deterrent option; mobilization bureaucracies would need reconstruction; strategic studies would reorient. The world would rearrange profoundly — the constraint is load-bearing for the current strategic order.
% FOUNDING_PROBLEM: The founding problem was the recurrent catastrophe of great-power total war (1914-1945): industrialized warfare that could mobilize entire societies and kill tens of millions, with no stable stopping mechanism. Nuclear weapons were developed and deployed to end this cycle by making the cost of great-power war exceed any conceivable gain.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (great-power total war recurrence) is attested as live by nuclear great powers (who cite it to justify arsenals), arms control communities (who cite it to justify restraint), and historians of the 1914-1945 period. However, non-nuclear states and disarmament advocates attest the problem is contested: they argue the nuclear solution created new catastrophes (proliferation risks, accidents, coercion of non-nuclear states) and that the founding problem could have been solved by other means (collective security, disarmament). No single corroboration exists outside the beneficiary set — the constraint's own beneficiaries are the primary attestors of the problem's liveness.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_possibility_space__space_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.78: the constraint extracts strategic autonomy from non-nuclear states (denied total-war option) and institutional purpose from mobilization bureaucracies (mission collapse), while subsidizing nuclear great powers (hierarchy freeze) and conventional MIC (procurement shift). Suppression 0.85: the constraint's persistence depends on physics-enforced exit closure — no alternative strategic framework permits great-power total war. Theater 0.12: minimal performative maintenance; the constraint is genuinely enforced by physics, not theater. Accessibility_collapse 0.91: alternatives (total war planning) have collapsed almost completely once the nuclear constraint is understood — genuine natural-law signature. Resistance 0.08: near-zero active resistance because the constraint is experienced as physical impossibility, not political choice. The claimed_type Mountain is author's structural judgment; metrics describe operational reality. Divergence between claim and metrics is the measurement.
 *
 * PERSPECTIVAL GAP:
 *   From nuclear_great_powers seat (institutional, global, arbitrage): the constraint is genuine coordination — physics solves the great-power war problem, they are net beneficiaries (d ~ 0.1). From non_nuclear_states seat (powerless, regional, trapped): the constraint is pure extraction — denied deterrent option, subject to coercion, no exit (d ~ 0.95). From total_war_mobilization_bureaucracies (moderate, national, trapped): the constraint is institutional death — mission evaporates, no alternative role (d ~ 0.9). From strategic_studies_establishment (organized, global, mobile): the constraint is career-subsidizing coordination — field reorients profitably (d ~ 0.2). Engine computes per-seat χ from these structural positions; author does not reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: nuclear_great_powers (collect security rent, hierarchy freeze), conventional_military_industrial_complex (procurement shift to high-tech, avoids mobilization overhead), strategic_studies_establishment (field reorientation preserves career capital). Victims declared: non_nuclear_states (denied total-war deterrent, coercion target), total_war_mobilization_bureaucracies (mission collapse, expertise atrophy), civil_defense_infrastructure (obsolescence, theater maintenance). Vindicated propositions (not beneficiaries): nuclear_revolution_in_military_affairs, absolute_weapon_doctrine, strategic_paradox_of_great_power_peace — these are doctrines the constraint's operation validates, not actors collecting rents. Directionality derives: beneficiaries get low d, victims get high d, analytical_observer gets 0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (preventing great-power total war through mutual vulnerability) remains live — the physics has not changed. However, the constraint has outlived its coordination function for non-nuclear states and mobilization bureaucracies: it no longer coordinates their security, it extracts it. Mandatrophy is resolved for victims (constraint persists without serving them) but not for nuclear great powers (constraint still serves them). This asymmetry — resolved for some seats, live for others — is the mandatrophy signature. The Mountain claim masks this: by presenting as natural law, the constraint avoids the 'outlived function' critique for victim seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a reading of the total_war_possibility_space kernel (space_contraction_reading), and do the sibling readings (deterrence_equilibrium_reading, nuclear_taboo_reading) instantiate structurally distinct constraints with different ε?',
    'Decompose the kernel into three separate constraint stories per the ε-invariance principle; verify each has stable ε, distinct beneficiary/victim structure, and different computed type. Cross-check network.affects_constraints links.',
    'If readings share ε and structural data, they are one constraint mislabeled as three. If they decompose cleanly, the kernel is a family and each reading is a valid constraint story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel/reading decomposition validity per ε-invariance').

omega_variable(
    natural_law_vs_constructed_impossibility,
    'Is the categorical impossibility of total war a genuine natural-law Mountain (physics of mutual destruction) or a constructed constraint that benefits nuclear great powers by freezing the strategic hierarchy?',
    'Test FSM signature: if Mountain with declared beneficiaries computes as extractive for victim seats, the constraint is a false summit. Track institutional atrophy of total-war apparatus — if atrophy serves great-power stability rather than physical necessity, the Mountain claim is cover.',
    'If Mountain with beneficiaries, FSM triggers reclassification to tangled_rope. If genuine Mountain, beneficiaries are incidental to physics, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_impossibility, conceptual, 'FSM ambiguity: natural law vs. great-power-favoring construction').

omega_variable(
    deterrence_vs_space_contraction_foreclosure,
    'Does the space-contraction reading (war exits planning space entirely) logically foreclose the deterrence-equilibrium reading (war remains reachable but deterred), or do they coexist as different parties'' live commitments?',
    'Analyze whether any single strategic framework can hold both: if planning space has contracted categorically, deterrence equilibrium cannot be ''reachable'' — the premises contradict. If both are held by different actors simultaneously, they coexist.',
    'Forecloses relation means the readings cannot inhabit the same framework; coexists_with means they are competing but live positions. Determines cs_structure.reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_vs_space_contraction_foreclosure, conceptual, 'Logical relationship between space-contraction and deterrence-equilibrium readings').

omega_variable(
    taboo_vs_space_contraction_mechanism,
    'Does the nuclear_taboo_reading (normative prohibition) describe the same structural constraint as space_contraction_reading (categorical impossibility), or a different one?',
    'Test whether taboo violation scenarios (limited nuclear use, tactical strikes) would reinstate total war in planning space. If taboo breach restores thinkability, the constraint is normative (taboo). If thinkability remains contracted regardless of taboo status, the constraint is material (space-contraction).',
    'If same constraint, taboo is the enforcement mechanism of space-contraction. If different, they are distinct constraints with different ε and different victim structures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_vs_space_contraction_mechanism, conceptual, 'Taboo vs. space-contraction: same constraint or different?').

omega_variable(
    institutional_atrophy_reversibility,
    'Is the atrophy of total-war planning apparatus (mobilization doctrine, general staff war-gaming, civil defense) reversible if the constraint were relaxed, or is the knowledge permanently lost?',
    'Historical analysis of post-Cold War strategic studies: did sub-nuclear domain shift represent permanent capability loss or strategic reallocation? Track whether any great power maintains latent total-war planning capacity.',
    'If irreversible, the constraint has path-dependent extraction (permanent disarmament of strategic imagination). If reversible, atrophy is contingent and the constraint''s extraction is less severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_atrophy_reversibility, empirical, 'Reversibility of strategic atrophy under space contraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twps_space_contr_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(twps_space_contr_tr_t1955, total_war_possibility_space__space_contraction_reading, theater_ratio, 1955, 0.08).
narrative_ontology:measurement(twps_space_contr_tr_t1965, total_war_possibility_space__space_contraction_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(twps_space_contr_tr_t1975, total_war_possibility_space__space_contraction_reading, theater_ratio, 1975, 0.11).
narrative_ontology:measurement(twps_space_contr_tr_t1985, total_war_possibility_space__space_contraction_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(twps_space_contr_tr_t1995, total_war_possibility_space__space_contraction_reading, theater_ratio, 1995, 0.11).
narrative_ontology:measurement(twps_space_contr_tr_t2005, total_war_possibility_space__space_contraction_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(twps_space_contr_tr_t2015, total_war_possibility_space__space_contraction_reading, theater_ratio, 2015, 0.11).
narrative_ontology:measurement(twps_space_contr_tr_t2025, total_war_possibility_space__space_contraction_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(twps_space_contr_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(twps_space_contr_be_t1955, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1955, 0.55).
narrative_ontology:measurement(twps_space_contr_be_t1965, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1965, 0.68).
narrative_ontology:measurement(twps_space_contr_be_t1975, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1975, 0.73).
narrative_ontology:measurement(twps_space_contr_be_t1985, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1985, 0.76).
narrative_ontology:measurement(twps_space_contr_be_t1995, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1995, 0.79).
narrative_ontology:measurement(twps_space_contr_be_t2005, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(twps_space_contr_be_t2015, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2015, 0.79).
narrative_ontology:measurement(twps_space_contr_be_t2025, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(twps_space_contr_su_t1945, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(twps_space_contr_su_t1955, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1955, 0.75).
narrative_ontology:measurement(twps_space_contr_su_t1965, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1965, 0.82).
narrative_ontology:measurement(twps_space_contr_su_t1975, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1975, 0.85).
narrative_ontology:measurement(twps_space_contr_su_t1985, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1985, 0.86).
narrative_ontology:measurement(twps_space_contr_su_t1995, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1995, 0.84).
narrative_ontology:measurement(twps_space_contr_su_t2005, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2005, 0.83).
narrative_ontology:measurement(twps_space_contr_su_t2015, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2015, 0.84).
narrative_ontology:measurement(twps_space_contr_su_t2025, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__space_contraction_reading, 0.1).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__nuclear_taboo_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, nuclear_nonproliferation_regime).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, great_power_competition_subnuclear).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, strategic_studies_field_configuration).

% DUAL FORMULATION NOTE:
% Kernel family decomposition: total_war_possibility_space decomposes into three readings with distinct ε. Space-contraction: ε=0.78, Mountain claim, victims=non_nuclear_states+mobilization_bureaucracies. Deterrence-equilibrium: ε≈0.45, Tangled Rope claim, victims=states_in_security_dilemma. Nuclear-taboo: ε≈0.62, Snare claim, victims=taboo_violators. Each story declares the others in affects_constraints. The upstream constraint (space-contraction, higher empirical confidence as physical limit) influences downstream (taboo, deterrence) because the physical limit is cited as evidence for the normative/strategic claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__space_contraction_reading, institutional, 0.15).
constraint_indexing:directionality_override(total_war_possibility_space__space_contraction_reading, powerless, 0.95).
constraint_indexing:directionality_override(total_war_possibility_space__space_contraction_reading, moderate, 0.88).
constraint_indexing:directionality_override(total_war_possibility_space__space_contraction_reading, organized, 0.2).
constraint_indexing:directionality_override(total_war_possibility_space__space_contraction_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
