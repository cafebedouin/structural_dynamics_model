% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__credibility_paradox_reading, []).

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
 *   constraint_id: nuclear_impossibility_kernel__credibility_paradox_reading
 *   human_readable: Nuclear Deterrence Credibility Paradox
 *   domain: strategic/international_relations/nuclear
 *
 * SUMMARY:
 *   This reading of the nuclear impossibility kernel centers the credibility
 *   paradox: deterrence requires a credible threat of nuclear use, but any
 *   use guarantees mutual destruction, rendering the threat inherently
 *   incredible. The structural consequence is that great powers cannot accept
 *   mutual vulnerability as stable — they must seek usable nuclear options
 *   (counterforce, limited war, escalation control) to make the threat
 *   credible. 'Unthinkability' is rhetorical cover; war remains reachable via
 *   escalation ladders. This reading claims the constraint is a tangled rope:
 *   genuine coordination (crisis stability, prevention of great power war)
 *   coexists with asymmetric extraction (nuclear establishments and elites
 *   benefit, populations and non-nuclear states pay existential risk and
 *   material costs). Active enforcement is continuous: modernization
 *   programs, posture reviews, arms control that manages but never eliminates
 *   the paradox.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.78).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.85).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "Nuclear Deterrence Credibility Paradox").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "strategic/international_relations/nuclear").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, '078b9e22-a56e-4f7f-8ea5-cc34aa67ee27').
narrative_ontology:cs_kernel_codification('078b9e22-a56e-4f7f-8ea5-cc34aa67ee27', formalized).
narrative_ontology:cs_authority_grounding('078b9e22-a56e-4f7f-8ea5-cc34aa67ee27', extraction).
narrative_ontology:cs_interpretation_layer_present('078b9e22-a56e-4f7f-8ea5-cc34aa67ee27').
narrative_ontology:cs_reading_relation('078b9e22-a56e-4f7f-8ea5-cc34aa67ee27', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('078b9e22-a56e-4f7f-8ea5-cc34aa67ee27', nuclear_impossibility_kernel__rational_dropout_reading, coexists_with).
narrative_ontology:cs_axiom('078b9e22-a56e-4f7f-8ea5-cc34aa67ee27', foundational, credible_threat_requires_usable_options).
narrative_ontology:cs_axiom_status(credible_threat_requires_usable_options, holdable).
narrative_ontology:cs_axiom_grounding('078b9e22-a56e-4f7f-8ea5-cc34aa67ee27', credible_threat_requires_usable_options, empirically_contingent).
narrative_ontology:cs_axiom('078b9e22-a56e-4f7f-8ea5-cc34aa67ee27', secondary, escalation_ladders_make_war_reachable).
narrative_ontology:cs_axiom_status(escalation_ladders_make_war_reachable, holdable).
narrative_ontology:cs_axiom_grounding('078b9e22-a56e-4f7f-8ea5-cc34aa67ee27', escalation_ladders_make_war_reachable, empirically_contingent).
narrative_ontology:cs_reference_frame('078b9e22-a56e-4f7f-8ea5-cc34aa67ee27', classical_deterrence_stability).
narrative_ontology:cs_drift_state('078b9e22-a56e-4f7f-8ea5-cc34aa67ee27', post_cold_war_modernization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('078b9e22-a56e-4f7f-8ea5-cc34aa67ee27', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_establishments).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, defense_contractors).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, great_power_political_elites).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, populations_under_nuclear_threat).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__credibility_paradox_reading, deterrence_requires_credible_threat).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__credibility_paradox_reading, escalation_ladders_enable_limited_nuclear_war).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, maintain, and modernize nuclear arsenals and supporting infrastructure. They define the operational requirements that make the credibility paradox actionable (counterforce targets, limited options, modernization programs). They benefit from sustained funding, institutional prestige, and career structures built around the paradox. Exit means abandoning the institutional identity and mission.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_establishments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_establishments, beneficiary).

% Authorize nuclear posture reviews, modernization budgets, and declaratory policy. They use the credibility paradox to justify continued investment and to signal resolve. Their exit is constrained by alliance commitments, domestic political pressures, and the perceived necessity of nuclear deterrence for great power status.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, great_power_political_elites, agenda_setter,
    institutional, biographical, constrained, global).

% Receive contracts for warhead modernization, delivery systems, command-and-control, and infrastructure. They lobby for programs justified by the credibility paradox. They have high exit mobility — they can shift to conventional programs — but the nuclear enterprise provides stable, cost-plus revenue streams.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, defense_contractors, beneficiary,
    powerful, biographical, mobile, global).

% Bear the existential risk of deterrence failure and the opportunity costs of nuclear spending. They have no meaningful exit — they cannot leave the targeting crosshairs, cannot opt out of the risk, and have minimal influence on nuclear policy. The credibility paradox makes their insecurity structural: the more 'credible' the threat, the higher their risk.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, populations_under_nuclear_threat, payer,
    powerless, generational, trapped, global).

% Bound by NPT non-proliferation commitments while facing nuclear threats from possessors. They are excluded from nuclear decision-making but pay the price of extended deterrence dependency and proliferation pressure. Their exit options are constrained: acquire nuclear weapons (pariah status, sanctions), rely on extended deterrence (credibility paradox applies to them too), or pursue disarmament diplomacy (blocked by possessors).
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_weapon_states, excluded).

% Negotiate treaties, verify compliance, and analyze strategic stability. They operate within the paradox — their work assumes the paradox is manageable through limits, transparency, and risk reduction. They neither collect the extraction nor bear its full cost; they attempt to bound it. Their analytical exit is real but their institutional role depends on the constraint's persistence.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, arms_control_community, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages great power competition by creating a shared vulnerability that makes major war catastrophic for all parties, providing a floor of stability in an anarchic system.
% TRANSFER_FUNCTION: Moves existential risk from political elites and nuclear establishments onto entire populations (including non-combatants and non-nuclear states), while moving material resources (trillions in modernization) from public treasuries to nuclear establishments and contractors.
% ABSENT_VOICES: Future generations who inherit the risk without consent; populations in the Global South who face nuclear winter effects from Northern hemisphere exchanges but have no voice in nuclear doctrine; hibakusha and downwinders whose testimony is marginalized in strategic discourse.
% DISAPPEARANCE_RATIONALE: If the credibility paradox vanished overnight — i.e., if mutual vulnerability were accepted as making nuclear use threats inherently incredible — nuclear establishments would lose their primary justification for counterforce capabilities and modernization; great powers would face pressure to adopt no-first-use or minimum deterrence; the NPT bargain would collapse or transform; the entire architecture of extended deterrence would become incoherent.
% FOUNDING_PROBLEM: How to prevent great power war in an anarchic system after the invention of weapons that make total war unwinnable. The credibility paradox emerged as the operational solution: make the threat of use credible enough to deter, while hoping it never gets tested.
% FOUNDING_PROBLEM_CORROBORATION: Classical deterrence theorists (Schelling, Jervis) attest the paradox is the core theoretical problem. Nuclear abolition advocates (TPNW supporters, ICAN) attest the founding problem was misdiagnosed — the paradox is not a solution but a trap. Great power political elites attest the problem remains live (hence modernization). No consensus exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__credibility_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__credibility_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__credibility_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint transfers existential risk and trillions in resources to populations and non-nuclear states while benefiting a narrow institutional complex. Suppression is higher (0.85) because alternatives (no-first-use, minimum deterrence, disarmament, TPNW) are actively suppressed through alliance pressure, security guarantees, and institutional inertia. Theater ratio is moderate (0.42): crisis stability and arms control provide genuine coordination, but a growing share of activity (counterforce modernization, low-yield warheads, 'escalate to de-escalate' doctrines) serves the credibility performance rather than stability. The measurement series shows extraction and suppression rising across the nuclear age, with a post-Cold War dip and sharp recent rise.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear establishment seat, the paradox is a management problem: credibility requires usable options, modernization solves it. From the trapped population seat, the same structure is an existential extraction machine: the more 'credible' the threat, the higher their risk. The engine computes this divergence from the structural data — the claimed type (tangled_rope) acknowledges both the coordination and extraction functions without resolving the seat divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear establishments and political elites are structural beneficiaries (d near 0.0) — they control the constraint, collect its resources, and define its terms. Defense contractors are mobile beneficiaries (d ~0.2) — they benefit but have exit. Populations under threat are trapped payers (d near 1.0) — they bear the risk with zero exit. Non-nuclear states are constrained payers (d ~0.8) — they bear extended deterrence risks and proliferation pressure with limited voice. Arms controllers are analytical observers (d ~0.5) — they see the structure but their role depends on it.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate (prevent great power war via mutual vulnerability) has atrophied into a self-justifying complex: the credibility paradox now *requires* the very capabilities (counterforce, limited options) that make the paradox worse. The mandate is not resolved — great power war prevention remains live — but the means have become the end. The constraint persists not because it solves the founding problem but because the institutions it created cannot imagine their own obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_paradox_natural_vs_constructed,
    'Is the credibility paradox a genuine structural feature of nuclear physics and game theory, or a constructed constraint maintained by nuclear establishments to justify their existence?',
    'Historical counterfactual: if a great power had adopted minimum deterrence with no-first-use in 1950 and maintained it, would deterrence have failed? Compare crisis outcomes under mutual vulnerability vs. counterforce postures.',
    'If natural, the paradox is a mountain/tangled_rope hybrid that cannot be escaped — only managed. If constructed, it is a snare maintained by identifiable beneficiaries, and alternative postures (minimum deterrence, no-first-use, abolition) are structurally viable but politically suppressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_paradox_natural_vs_constructed, conceptual, 'Whether the paradox is an irreducible feature of nuclear deterrence or an institutional artifact.').

omega_variable(
    escalation_control_viability,
    'Are escalation ladders and limited nuclear war options genuinely controllable, or is the belief in their controllability the extraction mechanism itself?',
    'Wargaming and historical analysis of near-use incidents (Cuban Missile Crisis, Able Archer, 1983 Soviet nuclear false alarm): did escalation control hold, or did chance dominate?',
    'If controllable, the tangled_rope coordination function is real (limited options provide crisis management). If uncontrollable, the coordination story is cover — the constraint is a snare where ''usable options'' are the extraction mechanism selling false control.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_control_viability, empirical, 'Whether limited nuclear use can be controlled or inevitably escalates to mutual destruction.').

omega_variable(
    nuclear_establishment_identity_lock,
    'To what extent is the nuclear weapons establishment''s commitment to the credibility paradox driven by identity fusion (the institution has ''become'' its function) versus material interest?',
    'Institutional history: trace whether establishments that lost their nuclear mission (e.g., post-Cold War drawdowns, South African disarmament) adapted or resisted. Measure correlation between career-path dependence and doctrinal rigidity.',
    'If identity-locked, the constraint is a piton-in-tangled-rope: the extraction persists because the administrators cannot conceive of themselves without it, not because it serves any coordination function. If material, standard interest-group politics applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_establishment_identity_lock, conceptual, 'Whether institutional identity fusion drives the paradox''s persistence beyond material interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(nucl_tr_t1960, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(nucl_tr_t1975, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1975, 0.35).
narrative_ontology:measurement(nucl_tr_t1990, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(nucl_tr_t2005, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(nucl_be_t1960, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(nucl_be_t1975, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1975, 0.65).
narrative_ontology:measurement(nucl_be_t1990, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(nucl_be_t2005, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(nucl_su_t1960, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(nucl_su_t1975, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1975, 0.8).
narrative_ontology:measurement(nucl_su_t1990, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(nucl_su_t2005, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2005, 0.78).
narrative_ontology:measurement(nucl_su_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__credibility_paradox_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__credibility_paradox_reading, 0.12).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, extended_deterrence_credibility).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, npt_bargain).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_modernization_programs).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the nuclear impossibility kernel into three readings with different ε values and classifications. The credibility paradox reading has high ε (extraction from populations via existential risk and modernization costs) and classifies as tangled_rope. The structural contraction reading has near-zero ε (mutual annihilation is a physical fact, not an extraction) and classifies as mountain. The rational dropout reading has moderate ε (opportunity costs of nuclear spending) and classifies as rope or scaffold depending on era.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_impossibility_kernel__credibility_paradox_reading, institutional, 0.15).
constraint_indexing:directionality_override(nuclear_impossibility_kernel__credibility_paradox_reading, powerless, 0.95).
constraint_indexing:directionality_override(nuclear_impossibility_kernel__credibility_paradox_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
