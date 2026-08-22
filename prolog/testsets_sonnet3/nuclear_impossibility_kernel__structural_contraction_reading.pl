% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__structural_contraction_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: nuclear_impossibility_kernel__structural_contraction_reading
 *   human_readable: Nuclear Impossibility: Structural Contraction of the War-Option Reachable Set
 *   domain: strategic_studies/international_relations/nuclear_deterrence
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the nuclear-impossibility kernel:
 *   the structural-contraction reading, which claims that mutual assured
 *   destruction did not merely raise the cost of great-power war
 *   (rational_dropout_reading) or produce an incredible-threat paradox
 *   (credibility_paradox_reading), but physically removed war between
 *   nuclear-armed peers from the reachable option set entirely. Under this
 *   reading, proxy wars, arms racing, and crisis brinksmanship are not
 *   degraded continuations of great-power war — they are substitutions
 *   occupying the space war used to occupy, because the war option itself no
 *   longer exists in the set any rational or physical process can reach. The
 *   claimed type is mountain (a structural/physical fact about the payoff
 *   landscape once secure second-strike exists), but beneficiaries are
 *   declared because nuclear-weapon states, arms-control institutions, and
 *   defense industry all derive real, ongoing benefit from the impossibility
 *   being read as a permanent physical fact rather than a contingent,
 *   revisable arrangement — this triggers FSM evaluation deliberately.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.42).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 0.71).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear Impossibility: Structural Contraction of the War-Option Reachable Set").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic_studies/international_relations/nuclear_deterrence").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, 'c180aada-dc9c-4fa0-8095-67b1d11019ed').
narrative_ontology:cs_kernel_codification('c180aada-dc9c-4fa0-8095-67b1d11019ed', distributed).
narrative_ontology:cs_authority_grounding('c180aada-dc9c-4fa0-8095-67b1d11019ed', distributed).
narrative_ontology:cs_reading_relation('c180aada-dc9c-4fa0-8095-67b1d11019ed', nuclear_impossibility_kernel__rational_dropout_reading, forecloses).
narrative_ontology:cs_reading_relation('c180aada-dc9c-4fa0-8095-67b1d11019ed', nuclear_impossibility_kernel__credibility_paradox_reading, influences).
narrative_ontology:cs_axiom('c180aada-dc9c-4fa0-8095-67b1d11019ed', foundational, war_option_physically_absent_from_reachable_set).
narrative_ontology:cs_axiom_status(war_option_physically_absent_from_reachable_set, holdable).
narrative_ontology:cs_axiom_grounding('c180aada-dc9c-4fa0-8095-67b1d11019ed', war_option_physically_absent_from_reachable_set, empirically_contingent).
narrative_ontology:cs_axiom('c180aada-dc9c-4fa0-8095-67b1d11019ed', secondary, proxy_conflict_is_substitution_not_continuation).
narrative_ontology:cs_axiom_status(proxy_conflict_is_substitution_not_continuation, holdable).
narrative_ontology:cs_axiom_grounding('c180aada-dc9c-4fa0-8095-67b1d11019ed', proxy_conflict_is_substitution_not_continuation, empirically_contingent).
narrative_ontology:cs_reference_frame('c180aada-dc9c-4fa0-8095-67b1d11019ed', secure_second_strike_payoff_landscape).
narrative_ontology:cs_drift_state('c180aada-dc9c-4fa0-8095-67b1d11019ed', post_cold_war_proliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c180aada-dc9c-4fa0-8095-67b1d11019ed', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, arms_control_bureaucracies).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, defense_industrial_establishment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, populations_under_extended_deterrence).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, populations_under_extended_deterrence).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, mutual_assured_destruction_doctrine).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, great_power_war_obsolescence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold arsenals that physically remove great-power war from the set of reachable strategic options between other nuclear-armed peers. They administer the doctrine (declaratory policy, arsenal sizing, alliance guarantees) that operationalizes the contraction, and they derive real security benefit and diplomatic standing from being read as the guarantors of a structural fact rather than as parties to a bargain.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_weapon_states, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_weapon_states, agenda_setter).

% Verification regimes, treaty secretariats, and strategic-studies institutions exist because the contraction is treated as a fact requiring monitoring and stewardship rather than a policy choice requiring justification. Their institutional continuity depends on the impossibility being read as physical rather than contingent.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, arms_control_bureaucracies, beneficiary,
    institutional, generational, mobile, global).

% Modernization programs, delivery-system contracts, and force-structure budgets are justified by the permanence of the impossibility — the arsenal must be maintained forever because the physical fact it encodes cannot lapse. Benefits from the framing regardless of whether the underlying claim is natural law or maintained artifact.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, defense_industrial_establishment, beneficiary,
    organized, generational, arbitrage, global).

% Operate inside a strategic order where great-power war is foreclosed among the armed peers but conventional and proxy conflict is displaced onto their territories. They did not choose the contraction and cannot exit the system it produces; the substitution of proxy war for direct war lands disproportionately on them.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states, payer,
    moderate, generational, constrained, global).

% Live under the umbrella of the contraction — they benefit from the absence of great-power war between guarantors, but they also bear the standing risk of the failure mode the impossibility claims to make unreachable, and have no voice in arsenal sizing, alert postures, or crisis decision-making that determines whether the impossibility actually holds under pressure.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, populations_under_extended_deterrence, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__structural_contraction_reading, populations_under_extended_deterrence, beneficiary).

% Analyze whether the contraction is a genuine structural feature of the strategic landscape (mutual annihilation removes war from the option set as a matter of physics and payoff structure) or a contingent political arrangement dressed in the language of physical law. Produce the competing readings this kernel decomposes into.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, strategic_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__structural_contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__structural_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the ordinary sense — this reading claims no coordination is required because the option (great-power war between nuclear peers) is not merely undesirable but physically absent from the reachable set. Whatever apparent coordination exists (arms control, crisis hotlines, verification) manages a fact, it does not produce it.
% TRANSFER_FUNCTION: Under this reading nothing is transferred by the impossibility itself; what moves is risk and cost displaced from the direct-war channel (foreclosed) onto proxy conflicts, arms-race expenditure, and populations under extended deterrence who bear tail risk without decision rights.
% ABSENT_VOICES: Populations in proxy-war theaters and non-nuclear states bear the substitution effect (war displaced rather than eliminated) but have no seat in doctrine-setting; strategic theorists who hold the rational_dropout or credibility_paradox readings are structurally present in this same discourse but are treated by this reading's proponents as describing a lesser, revisable constraint rather than the physical floor this reading claims.
% DISAPPEARANCE_RATIONALE: If the claimed physical impossibility were shown false (a rational path to victory were found, e.g. through disarming first strike, missile defense breakthroughs, or decapitation doctrine), nuclear-armed states dispute what would happen: this reading holds the reachable set would not merely shift probabilities but would re-admit great-power war as an option, which the doctrine, arsenal, and alliance structure would have to reorganize around. Beneficiary institutions insist the impossibility is permanent and disappearance is not coherently imaginable; that insistence is itself part of what the omega below interrogates.
% FOUNDING_PROBLEM: The emergence of thermonuclear weapons and survivable second-strike capability in the mid-20th century appeared to remove any combination of first strike, damage limitation, or war termination that avoided mutual societal destruction — the founding claim is that this removed war-as-option entirely, not merely that it changed war's cost-benefit calculus.
% FOUNDING_PROBLEM_CORROBORATION: Independent strategic analysts and historians of the nuclear age (e.g., in post-Cold War archival work on near-miss incidents, launch-on-warning postures, and command-and-control accidents) attest that the physical-impossibility claim has never been tested to failure and that several episodes came close to falsifying the guaranteed-mutual-annihilation premise, which would undercut this reading's structural-contraction claim rather than confirm it; this corroboration comes from historians and arms-control researchers outside the nuclear-weapon-state defense establishments that benefit from the claim's stability.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__structural_contraction_reading, contested).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__structural_contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rising slowly: the contraction itself extracts little directly, but the doctrine and arsenal maintained to enforce/verify it accumulate rent-like costs (permanent modernization budgets, alliance-guarantee costs, opportunity costs of proxy-war displacement) that grow over the interval even though the core claim (mutual annihilation makes victory unreachable) is treated as constant. Accessibility collapse is very high (0.88) because once secure second-strike is understood, no rational actor can construct a path to victory in direct great-power exchange — this is the mountain signature. Resistance is comparatively low (0.35) because there is little active contest of the underlying physics; what contest exists is about whether the physics fully generalizes (crisis instability, accidental war, non-state actors) rather than whether mutual annihilation follows from full exchange.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear-weapon-state seat, the impossibility is a discovered fact about payoff structures under secure second-strike, requiring only stewardship. From the non-nuclear-state and extended-deterrence-population seats, the same structure is an imposed arrangement that displaces risk and cost onto them without giving them any say in whether the physical claim holds under real crisis conditions — the engine's per-seat computation should reflect this divergence even though the authored claimed_type is uniform across all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states and the institutions built around doctrine maintenance sit near the beneficiary end: they collect security, standing, and budget from the impossibility being read as permanent and physical. Non-nuclear states and populations under extended deterrence sit nearer the target end: they bear the substitution effects (proxy war, arms race externalities, tail catastrophic risk) without holding the decision rights the beneficiary seats hold. This is exactly the FSM signature — a claimed natural law with identifiable, structurally-advantaged beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding thermonuclear war between peer nuclear states) remains partially live — no direct great-power nuclear exchange has occurred — but the arrangement built around it (permanent arsenals, permanent verification bureaucracies, permanent modernization budgets) has expanded well past what maintaining the original physical fact would require, and persists in part because the beneficiary institutions have strong incentives to read the claim as unrevisable physics rather than as a maintained, contestable arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_fact_vs_maintained_doctrine,
    'Is the structural contraction of the war-option reachable set a genuine, mind-independent feature of the strategic payoff landscape once secure second-strike exists, or is it a maintained political-doctrinal artifact that requires continuous arsenal investment, alliance signaling, and crisis-management infrastructure to remain true — and that would decay toward reachability if that maintenance stopped?',
    'Historical and technical analysis of near-miss incidents (1962, 1983, 1995 and others), missile-defense and counterforce doctrine trajectories, and whether any state has genuinely pursued or approached a first-strike capability that would restore war to the reachable set; convergent testimony from historians and technologists outside the nuclear-weapon-state policy establishments.',
    'If the contraction is a genuine physical fact independent of maintenance, the mountain classification with FSM-flagged incidental beneficiaries is correct — beneficiaries collect from a real structural feature they did not create. If it is a maintained artifact, the constraint is better read as a tangled_rope or snare: a coordination function (crisis stability) wrapped around asymmetric extraction (permanent budgets, proxy-war displacement) requiring active doctrinal enforcement to appear as physical law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_fact_vs_maintained_doctrine, conceptual, 'Whether the war-option contraction is discovered physical fact or actively maintained doctrinal artifact — the central FSM question for this mountain-claimed constraint.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly do the three kernel readings (structural_contraction, rational_dropout, credibility_paradox) diverge — is it a factual disagreement about whether M-set contraction versus cost-dominance versus threat-incredibility best describes the same underlying strategic reality, or do they pick out genuinely different mechanisms with different falsification conditions?',
    'Formal decision-theoretic and game-theoretic modeling comparing the option-set-contraction claim (this reading) against the cost-dominance claim (rational_dropout) and the credible-commitment claim (credibility_paradox), tested against crisis-bargaining data and declassified deterrence-decision archives.',
    'If the readings are genuinely distinct mechanisms, they remain properly decomposed as three separate constraints (as authored) with different beneficiary/victim structures and different persistence conditions. If they collapse into restatements of one mechanism, the network of sibling constraints should be merged or one designated as the canonical formulation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Locates the structural disagreement between the three kernel readings — the committer content for this reading''s position in the kernel contest.').

omega_variable(
    proxy_war_substitution_completeness,
    'Does the structural contraction of great-power direct war fully displace conflict onto proxy theaters (pure substitution, as this reading claims), or does some residual probability of direct escalation persist that the substitution framing obscures?',
    'Quantitative conflict-data analysis of proxy-war frequency and intensity correlated with nuclear-armed-state involvement, alongside crisis-escalation modeling of how proxy conflicts have historically approached (and been pulled back from) direct confrontation thresholds.',
    'If substitution is complete, the reachable-set contraction claim is strongly supported. If residual direct-escalation risk persists at non-trivial levels, the physical-impossibility claim is overstated and the constraint drifts toward the rational_dropout or credibility_paradox reading''s territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_war_substitution_completeness, empirical, 'Tests whether proxy war is genuine substitution (supporting this reading) or imperfect displacement leaving residual direct-war risk (undermining it).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(nucl_tr_t1960, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(nucl_tr_t1975, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(nucl_tr_t1990, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(nucl_tr_t2005, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2005, 0.34).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(nucl_be_t1960, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement(nucl_be_t1975, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1975, 0.35).
narrative_ontology:measurement(nucl_be_t1990, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1990, 0.33).
narrative_ontology:measurement(nucl_be_t2005, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(nucl_su_t1960, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(nucl_su_t1975, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1975, 0.65).
narrative_ontology:measurement(nucl_su_t1990, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(nucl_su_t2005, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement(nucl_su_t2025, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__structural_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__structural_contraction_reading, 0.12).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposing the colloquial 'nuclear deterrence made war impossible/irrational/paradoxical' claim per the ε-invariance principle. structural_contraction_reading claims physical/ontological removal of the war option (mountain, FSM-flagged for institutional beneficiaries); rational_dropout_reading claims a rational-choice cost-dominance constraint (structurally weaker ontological claim, likely rope or tangled_rope); credibility_paradox_reading claims an incredible-commitment paradox about the deterrent threat itself, not about war's reachability (likely tangled_rope given the live literature on extended-deterrence credibility gaps). Each carries its own ε and its own stakeholder structure; they are linked here rather than merged because their failure modes and falsification conditions differ.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
