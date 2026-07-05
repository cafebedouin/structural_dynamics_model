% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Nuclear Impossibility Kernel — Structural Contraction Reading
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This story instantiates the structural-contraction reading of the nuclear
 *   impossibility kernel: nuclear weapons did not merely make great-power war
 *   incredible to threaten (the credibility-paradox reading) or costly
 *   relative to any conceivable benefit (the rational-dropout reading) — they
 *   removed direct great-power war from the reachable strategic option set
 *   entirely, as a matter of physical fact grounded in guaranteed mutual
 *   retaliatory destruction. Under this reading, war does not survive as a
 *   degraded, costly, or incredible option; it exits the set of things that
 *   can happen at all between peer nuclear arsenals. The measurable
 *   phenomenon this reading tracks is substitution, not
 *   deterrence-as-persuasion: since 1945, great-power contest has been
 *   displaced wholesale into proxy conflict, because the direct channel no
 *   longer exists as a strategic possibility, not merely because it became a
 *   bad bet.
 *
 * KEY AGENTS:
 *   - nuclear_weapons_states: primary beneficiaries of the physical shelter the impossibility confers
 *   - defense_industrial_establishments: institutional beneficiaries maintaining the arsenals that constitute the fact
 *   - non_nuclear_states: bear the substituted contest displaced from the foreclosed direct channel
 *   - proxy_conflict_populations: bear the actual violence of the substitution
 *   - arms_control_negotiators: administer the boundary conditions without power to reopen the foreclosed option
 *   - strategic_theorists: analytical observers documenting the contraction of the reachable set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.42).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 0.58).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear Impossibility Kernel — Structural Contraction Reading").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic_studies/international_relations").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, '74430f68-fd52-43fd-845b-cdb11212aa3d').
narrative_ontology:cs_kernel_codification('74430f68-fd52-43fd-845b-cdb11212aa3d', distributed).
narrative_ontology:cs_authority_grounding('74430f68-fd52-43fd-845b-cdb11212aa3d', distributed).
narrative_ontology:cs_reading_relation('74430f68-fd52-43fd-845b-cdb11212aa3d', nuclear_impossibility_kernel__rational_dropout_reading, coexists_with).
narrative_ontology:cs_reading_relation('74430f68-fd52-43fd-845b-cdb11212aa3d', nuclear_impossibility_kernel__credibility_paradox_reading, influences).
narrative_ontology:cs_axiom('74430f68-fd52-43fd-845b-cdb11212aa3d', foundational, war_exits_reachable_set_as_physical_fact).
narrative_ontology:cs_axiom_status(war_exits_reachable_set_as_physical_fact, holdable).
narrative_ontology:cs_axiom_grounding('74430f68-fd52-43fd-845b-cdb11212aa3d', war_exits_reachable_set_as_physical_fact, empirically_contingent).
narrative_ontology:cs_axiom('74430f68-fd52-43fd-845b-cdb11212aa3d', secondary, proxy_substitution_is_categorically_distinct_from_continuation).
narrative_ontology:cs_axiom_status(proxy_substitution_is_categorically_distinct_from_continuation, holdable).
narrative_ontology:cs_axiom_grounding('74430f68-fd52-43fd-845b-cdb11212aa3d', proxy_substitution_is_categorically_distinct_from_continuation, empirically_contingent).
narrative_ontology:cs_reference_frame('74430f68-fd52-43fd-845b-cdb11212aa3d', mutual_assured_destruction_baseline).
narrative_ontology:cs_drift_state('74430f68-fd52-43fd-845b-cdb11212aa3d', post_cold_war_multipolar_proliferation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('74430f68-fd52-43fd-845b-cdb11212aa3d', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, defense_industrial_establishments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, proxy_conflict_populations).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, mutually_assured_destruction_doctrine).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, great_power_war_obsolescence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold arsenals that make direct great-power war physically self-annihilating for any side that starts it. This removes conventional great-power war from their own reachable strategic option set, but also insulates them from invasion, coercive regime change, and existential conquest by peers. They present the impossibility as a physical fact of the world, and it functions as one at the level of direct homeland-to-homeland war, while also conferring an unearned strategic shelter that lets them absorb no accountability for smaller wars fought elsewhere.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_weapons_states, beneficiary,
    institutional, civilizational, arbitrage, global).

% Maintain, modernize, and justify budgets around the arsenals that constitute the impossibility. They collect procurement revenue and institutional permanence from the maintenance of a physical fact they did not create but whose continued credibility they are paid to sustain (delivery systems, warhead refurbishment, command-and-control infrastructure).
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, defense_industrial_establishments, beneficiary,
    organized, generational, arbitrage, global).

% Live inside a strategic environment where direct great-power confrontation has been physically foreclosed, but the contraction of the reachable set pushes great-power contest downward and outward into their territories as proxy conflict, sanctions regimes, and client-state arrangements. They bear the substituted violence that the impossibility displaces rather than eliminates.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states, payer,
    moderate, generational, constrained, regional).

% Experience the actual violence that results when great-power contest cannot be settled by direct war and is instead exported into their states as materiel, mercenaries, and destabilization campaigns. From their position the impossibility of great-power war is real, but it does not mean an absence of war for them — it means a specific structural substitution of who fights and where.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, proxy_conflict_populations, payer,
    powerless, immediate, trapped, regional).

% Attempt to manage, verify, and occasionally reduce the arsenals whose existence constitutes the impossibility, without ever being positioned to make direct great-power war reachable again. They administer the boundary conditions of the impossibility (numbers, delivery systems, escalation protocols) but cannot alter the underlying physical fact that any full exchange between peer nuclear arsenals destroys both sides.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, arms_control_negotiators, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__structural_contraction_reading, arms_control_negotiators, agenda_setter).

% Study and formally model the reachable strategic set. From this reading's structural-contraction analysis, war as a rational path to victory has been removed from the option space entirely — not merely made incredible as a threat, and not merely made cost-ineffective, but rendered structurally impossible given weapons physics and retaliatory second-strike capability. They document what disappeared from the set, not what merely became irrational or incredible to threaten.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, strategic_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_weapons_states).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__structural_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a physical guarantee — grounded in the retaliatory capacity and destructive yield of nuclear arsenals — that any full-scale war between peer nuclear powers destroys both belligerents, removing direct great-power war from the set of strategically reachable options for any rational actor.
% TRANSFER_FUNCTION: Does not move resources between parties in the ordinary sense; it removes an entire category of action (direct great-power war) from the reachable set and displaces the violence that would have occupied that category into substitute channels — proxy conflict, client-state warfare, and destabilization campaigns borne by non-nuclear states and their populations.
% ABSENT_VOICES: Populations in states where proxy conflicts are actually fought have no voice in the strategic calculus that produced the contraction; the great powers whose direct war became impossible are not the ones who absorb the substituted violence, and the substitution is rarely narrated as a cost of the impossibility rather than as separate regional conflict.
% DISAPPEARANCE_RATIONALE: If nuclear arsenals and their guaranteed retaliatory capacity vanished, direct great-power war would re-enter the reachable strategic set immediately — the entire architecture of deterrence postures, alliance guarantees, and proxy-substitution patterns built around the physical impossibility would need to reorganize around a world where great-power war is once again a live option.
% FOUNDING_PROBLEM: The problem of how to prevent direct war between great powers once both possess weapons capable of destroying the other's ability to make war (and much of its population) in a single retaliatory exchange.
% FOUNDING_PROBLEM_CORROBORATION: Independent nuclear strategists, arms-control scholars, and historians of the Cold War outside any single nuclear-weapons state's defense establishment attest that the physical retaliatory capacity remains intact and that no technological development (missile defense, hypersonics) has yet restored a survivable first-strike option between peer arsenals; this corroboration comes from analysts with no institutional stake in maintaining any specific state's arsenal budget.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored moderate (0.42 at 2025) rather than low, because although the physical fact is genuinely a mountain at the level of direct great-power war, nuclear-weapons states and defense establishments derive an ongoing strategic and budgetary benefit from the fact's persistence and are among the interpreters of what it means — this is the FSM candidate signal: a mountain claim with declared beneficiaries. Suppression is authored at 0.58 because the impossibility is enforced in practice through continuous arsenal maintenance, alliance guarantees, and extended-deterrence commitments that actively foreclose exploration of alternatives (disarmament, no-first-use regimes) rather than resting purely on physics. Accessibility collapse is high (0.88): once the retaliatory-capacity fact is understood, there is no rational path back to treating direct great-power war as survivable. Resistance is low (0.22): almost no serious strategic actor argues direct great-power nuclear war is survivable or winnable; what resistance exists is about the interpretation of the fact (credibility, cost-benefit) rather than the fact itself, which belongs to the sibling readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-weapons states and defense establishments sit near the beneficiary end: the impossibility shelters them from existential invasion while their institutional position is buttressed by maintaining the arsenals that constitute the fact. Non-nuclear states and proxy-conflict populations sit near the target end: they absorb the substituted violence that the contraction displaces, without receiving the shelter the impossibility confers on the arsenal-holders. Arms control negotiators and strategic theorists are analytical/administrative seats — they operate on the boundary conditions of the fact but do not benefit from or bear the substitution the way the state-level or population-level seats do.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in a specific direction: because it treats the war-removal as a physical fact rather than a policy choice, it would be a mistake to treat the founding problem (preventing direct war between arsenal-holders capable of mutual annihilation) as obsolete or as a cover story — the corroboration record from independent strategists confirms the retaliatory-capacity fact remains live. What the classification must NOT do is let the genuine physical-mountain core of the claim launder the separate, contestable claim that the resulting substitution into proxy war is costless or that the arsenal-holders' institutional benefit from the arrangement is itself natural law — that benefit is a constructed feature riding on top of the physical fact, which is exactly why beneficiaries are declared on a mountain here and an omega is required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_fact_vs_constructed_shelter,
    'Is the nuclear impossibility a pure physical/logical mountain (retaliatory capacity guarantees mutual destruction, full stop), or does the identified beneficiary structure (nuclear-weapons states'' strategic shelter, defense-industrial budgetary permanence) indicate a constructed layer riding on top of the physical fact?',
    'Trace whether the strategic shelter and budgetary permanence would persist under a counterfactual arsenal configuration that preserved deterrence at far lower force levels (minimal deterrence doctrine) — if shelter and budget persist unchanged despite radically reduced arsenals, the beneficiary structure is substantially constructed rather than physically necessitated.',
    'If the beneficiary structure is substantially constructed, the FSM signature is warranted and this reading''s classification should be scrutinized for false-summit reclassification toward tangled_rope at the institutional-maintenance layer, while the core physical-impossibility claim at the direct-war level remains mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_fact_vs_constructed_shelter, conceptual, 'Whether nuclear-weapons-state benefit is physically necessitated by deterrence or an excess constructed on top of it.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the structural-contraction reading''s premise (war exits the reachable set entirely) diverge from the rational-dropout reading''s premise (war remains reachable but is cost-dominated) — is this a difference in modal claim (impossible vs. merely irrational) or a difference in empirical confidence about retaliatory survivability?',
    'Compare the two readings'' treatment of edge cases: limited nuclear exchange scenarios, missile defense breakthroughs, and first-strike disarming-strike feasibility studies. If structural_contraction treats these edge cases as still-impossible (physics forecloses) while rational_dropout treats them as still-reachable-but-irrational (cost-benefit disfavors), the disagreement is modal, not empirical.',
    'If the disagreement is genuinely modal (impossible vs. irrational-but-possible), the two readings are not merely differently confident about the same fact but are making structurally distinct claims about the shape of the reachable set — supporting the decomposition into separate constraint stories rather than treating this as one contested empirical question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Locating whether the sibling readings diverge on modality (possible/impossible) or on empirical cost-benefit confidence.').

omega_variable(
    proxy_substitution_completeness,
    'Does proxy war fully absorb the strategic contest that direct great-power war would have resolved, or does incomplete substitution leave residual unresolved great-power tension that could eventually force renegotiation of the impossibility itself (e.g., through emerging technologies that threaten second-strike survivability)?',
    'Longitudinal study of whether proxy conflict outcomes have historically settled the strategic questions (spheres of influence, relative power rankings) that direct war would have settled, or whether they leave those questions permanently unresolved and recurring.',
    'If substitution is incomplete, the M-set contraction claimed by this reading may be less than total — some residual pressure toward direct confrontation could persist beneath the proxy layer, which would qualify the ''entirely'' in the reading''s core premise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proxy_substitution_completeness, empirical, 'Whether proxy war fully substitutes for the foreclosed direct-war option or leaves unresolved pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1962, 0.18).
narrative_ontology:measurement(nucl_tr_t1979, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1979, 0.24).
narrative_ontology:measurement(nucl_tr_t1991, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1991, 0.28).
narrative_ontology:measurement(nucl_tr_t2008, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2008, 0.27).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1945, 0.2).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1962, 0.3).
narrative_ontology:measurement(nucl_be_t1979, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1979, 0.38).
narrative_ontology:measurement(nucl_be_t1991, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1991, 0.35).
narrative_ontology:measurement(nucl_be_t2008, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2008, 0.4).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2025, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nuclear_impossibility_kernel__structural_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__structural_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__structural_contraction_reading, 0.12).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, credibility_paradox_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, rational_dropout_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the nuclear_impossibility_kernel. structural_contraction_reading claims war exits the reachable set entirely (physical impossibility, this file); rational_dropout_reading claims war remains structurally possible but is cost-dominated (rational-choice constraint); credibility_paradox_reading claims deterrence requires an inherently incredible use-threat (paradox of credibility). Each reading is authored as its own ε-invariant constraint per DP-001; the committer structure distinguishing them is routed to omegas in each file rather than folded into a shared classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
