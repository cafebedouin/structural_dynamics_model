% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Total-War Reachability Boundary - Contraction Reading (Winnable Total War Outside the Feasible Set)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   Between 1945 and roughly the mid-1960s, thermonuclear arsenals with
 *   survivable second-strike capability contracted the strategic space of the
 *   great powers: every credible wargame, every feasibility study, and every
 *   crisis converged on the conclusion that total war between nuclear-armed
 *   states has no winning branch. This story instantiates the CONTRACTION
 *   READING of the total_war_reachability_boundary kernel: the removal of
 *   winnable total war from the feasible set is treated as a fixed,
 *   physics-grade limit - a mountain. No actor collects from its operation;
 *   the declared victim set is universal, because the limit's existence
 *   entails permanent species-level tail risk borne by every population,
 *   disproportionately by those with no voice in posture decisions. Per the
 *   epsilon-invariance decomposition rule, the colloquial label 'the nuclear
 *   revolution' covers three structurally distinct claims about this boundary
 *   - permanent contraction (this file), probabilistic reduction with
 *   retained reachability (dropping_reading), and contingent,
 *   technology-reversible contraction (contingent_reachability_reading) -
 *   each authored as its own constraint with its own epsilon and linked
 *   through network.affects_constraints. The claim/metric split is
 *   deliberate: claimed_type asserts mountain; the authored metrics describe
 *   a limit that is nearly absolute in feasibility terms
 *   (accessibility_collapse 0.92) yet surrounded by real intellectual
 *   resistance (0.38) and heavy signaling theater (0.48). The engine
 *   adjudicates; the claim is not tuned to the predicted output. KEY AGENTS
 *   (by structural relationship): - nuclear_strategic_commands: Administrator
 *   seat (institutional/identity_locked) - operates the arsenals constituting
 *   the boundary; collects nothing - nuclear_power_political_leadership: Dual
 *   seat (institutional/constrained) - holds launch authority, bears crisis
 *   exposure - global_civilian_population: Primary universal victim
 *   (powerless/trapped) - bears the entire tail risk - non_nuclear_states:
 *   Sheltered-hostage seat (moderate/constrained) - protected by a limit it
 *   does not own - future_generations: Excluded victim (powerless/trapped) -
 *   inherits embedded risk, cannot object - disarmament_movements: Excluded
 *   opposition (organized/constrained) - outside all posture decisions -
 *   strategic_studies_community: Analytical observer (analytical/analytical)
 *   - maps the feasible set
 *
 * KEY AGENTS:
 *   - nuclear_strategic_commands: Administrator seat (institutional/identity_locked) - operates the arsenals whose second-strike survivability constitutes the boundary; collects nothing; cannot exit without ceasing to be what it is
 *   - nuclear_power_political_leadership: Dual seat (institutional/constrained) - holds launch authority, sets doctrine, bears crisis exposure; payer with agenda-setting powers
 *   - global_civilian_population: Primary universal victim (powerless/trapped) - bears the entire tail risk; no vote, no exit
 *   - non_nuclear_states: Sheltered-hostage seat (moderate/constrained) - protected by a limit they do not own, exposed to consequences they do not control
 *   - future_generations: Excluded victim (powerless/trapped) - inherits embedded risk; structurally unable to object
 *   - disarmament_movements: Excluded opposition (organized/constrained) - argues for abolishing the boundary's basis; holds no seat in any posture decision
 *   - strategic_studies_community: Analytical observer (analytical/analytical) - maps the feasible set; adjudicates reachability claims by model and simulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.16).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.08).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Total-War Reachability Boundary - Contraction Reading (Winnable Total War Outside the Feasible Set)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, 'c5044ba3-7abd-4abb-a5a4-853b282d1dab').
narrative_ontology:cs_kernel_codification('c5044ba3-7abd-4abb-a5a4-853b282d1dab', formalized).
narrative_ontology:cs_authority_grounding('c5044ba3-7abd-4abb-a5a4-853b282d1dab', expertise).
narrative_ontology:cs_interpretation_layer_present('c5044ba3-7abd-4abb-a5a4-853b282d1dab').
narrative_ontology:cs_reading_relation('c5044ba3-7abd-4abb-a5a4-853b282d1dab', total_war_reachability_boundary__dropping_reading, forecloses).
narrative_ontology:cs_reading_relation('c5044ba3-7abd-4abb-a5a4-853b282d1dab', total_war_reachability_boundary__contingent_reachability_reading, forecloses).
narrative_ontology:cs_axiom('c5044ba3-7abd-4abb-a5a4-853b282d1dab', foundational, winnable_total_war_physically_unreachable).
narrative_ontology:cs_axiom_status(winnable_total_war_physically_unreachable, holdable).
narrative_ontology:cs_axiom_grounding('c5044ba3-7abd-4abb-a5a4-853b282d1dab', winnable_total_war_physically_unreachable, empirically_contingent).
narrative_ontology:cs_axiom('c5044ba3-7abd-4abb-a5a4-853b282d1dab', secondary, escalation_converges_to_catastrophe).
narrative_ontology:cs_axiom_status(escalation_converges_to_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('c5044ba3-7abd-4abb-a5a4-853b282d1dab', escalation_converges_to_catastrophe, empirically_contingent).
narrative_ontology:cs_reference_frame('c5044ba3-7abd-4abb-a5a4-853b282d1dab', thermonuclear_contracted_feasible_set).
narrative_ontology:cs_drift_state('c5044ba3-7abd-4abb-a5a4-853b282d1dab', contemporary_multipolar_era, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('c5044ba3-7abd-4abb-a5a4-853b282d1dab', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, global_civilian_population).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contraction_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, nuclear_power_political_leadership).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the arsenals whose survivable second-strike capability constitutes the boundary: maintain alert postures, run the wargames that repeatedly confirm no war-winning pathway exists, and execute retaliation if attacked. They administer the machinery that instantiates the limit but collect no revenue from it; their budgets are costs, their mission is the limit's guarantee. Exit would mean dismantling the force they are - the institution has become its function.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, nuclear_strategic_commands, agenda_setter,
    institutional, generational, identity_locked, global).

% Hold launch authority and set doctrine; bear the decision burden of a limit they cannot repeal and the personal risk that any crisis escalates past recall. They can reshape posture at the margins but cannot restore a winnable total war short of disarmament, which their security establishments resist. They pay in constrained option sets and crisis exposure.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, nuclear_power_political_leadership, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contraction_reading, nuclear_power_political_leadership, agenda_setter).

% Bear the entire tail risk the boundary's existence entails: if the limit ever fails - by accident, miscalculation, or unauthorized use - they die first and in the largest numbers, and no alert posture, doctrine, or treaty is ever put to their vote. Exit is physically unavailable; the blast radius is the species' habitat.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, global_civilian_population, payer,
    powerless, biographical, trapped, universal).

% Live under a limit owned by others: sheltered from great-power total war they could not have survived, yet hostage to arsenals they neither control nor voted for, and exposed to fallout and nuclear winter produced elsewhere. Renouncing or acquiring weapons does not move the boundary; their option set is bounded by decisions made in other capitals.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, non_nuclear_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contraction_reading, non_nuclear_states, beneficiary).

% Inherit whatever risk the present generation's posture decisions embed - waste storage, arsenal modernization, treaty collapse - with no seat in any room where those decisions are made. They would object to the risk being renewed on their behalf; their objection is structurally uncastable.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, future_generations, excluded,
    powerless, civilizational, trapped, universal).

% Organized campaigns - treaty-ban coalitions, humanitarian initiatives - arguing the limit's basis should be abolished outright; they hold moral authority and occasional legal victories but no seat in alert-posture or doctrine decisions in any nuclear-armed capital.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, disarmament_movements, excluded,
    organized, generational, constrained, global).

% The wargamers, physicists, and theorists who mapped the contraction: they adjudicate reachability claims by model and simulation, publish the feasibility analyses all parties cite, and see the full structure - including that the limit binds its own administrators.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, strategic_studies_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes total war from every nuclear power's feasible set simultaneously, terminating the otherwise-unbounded competition in war-winning capability. The removal is not negotiated into place and requires no agreement to sustain: the same physical fact binds all parties at once, so restraint is not coordinated among actors but imposed on them by the shared limit.
% TRANSFER_FUNCTION: Transfers no goods to any seat. What moves is option-space and risk: the total-war option is subtracted from every state's strategy set, and species-level tail risk is distributed across all populations - concentrated on those with no voice in alert postures - while the arsenals that constitute the boundary absorb a standing fraction of great-power budgets.
% ABSENT_VOICES: Future generations would object to inheriting a renewed risk they cannot cast a vote against; the publics of nuclear-armed states would object that alert postures and doctrines are set over their heads; non-nuclear majorities would object to hostage-status they never accepted; disarmament campaigns hold the objection continuously but hold no seat in any capital's posture decisions. All four objections are structurally uncastable inside the rooms where the boundary's terms are administered.
% DISAPPEARANCE_RATIONALE: If winnable total war returned to the feasible set overnight, great-power war planning would revive within a planning cycle: total-war preparation would again be rational for every major power, crisis bargaining would lose its floor, and every alliance, trade, and diplomatic arrangement premised on the impossibility of great-power total war would be repriced against a new possibility of civilizational termination. The rearrangement would be total, which is the operational content of calling the boundary a mountain.
% FOUNDING_PROBLEM: The demonstrated recurrence of industrial-scale total war among great powers (1914-18, 1939-45) and the resulting strategic problem: how any state secures itself against another's total-war ambition when total war is survivable enough to attempt.
% FOUNDING_PROBLEM_CORROBORATION: No beneficiary set exists to self-attest - this reading declares none - so corroboration is necessarily external: the eighty-year empirical absence of great-power total war in the diplomatic-historical record; the contemporaneous internal conclusions of both Cold-War adversaries (superpower wargaming archives and senior decision-makers' documented judgments that total war was unwinnable); and the repeated failure of every attempted refutation (warfighting doctrine, missile defense, disarming-strike concepts) to demonstrate a winning branch. The problem is dead as effectuated by the arrangement - the distinction that keeps this from being a zombie finding is carried in the disappearance_verdict and the theater series, not in this narrative.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_reachability_boundary__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.16 at interval end) because the boundary transfers nothing to anyone: its cost side is universal risk-bearing and the resource sink of the arsenals that instantiate it, with no receiving seat - hence gain_flow 'diffuse'. Suppression is near-floor (0.08) because the limit coerces no one; alternatives collapse on comprehension rather than by enforcement, which is why accessibility_collapse is high (0.92) while suppression stays low - the mountain signature separates these two channels, and suppression is authored as the raw unscaled structural property it is. Resistance (0.38) is real but perpetually unsuccessful: countervailing warfighting doctrine, missile-defense schemes, and disarming-strike concepts are repeated attempts to restore reachability that wargaming keeps refuting. Theater_ratio (0.48) tracks the growth of performative signaling - tests as messages, red lines, posture reviews that change nothing - atop a limit whose core function never degraded; it sits just under the Goodhart line. The series show the boundary's history as a cycle (rise to the 1962 crisis-density peak, Cold-War plateau, post-1991 trough, current rebound) driven by exogenous geopolitical tension and arsenal size rather than by any extraction dynamic - the oscillation is not an intermittent-reinforcement mechanism, and the base_properties scalars reflect the interval-end state. All series share one eight-point grid (T=0..80) so no metric's end-state leaks backward into earlier rows.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical facts. From the strategic commands' position the boundary is the operating environment they administer and guarantee - a mission, not an imposition; from the civilian seats it is pure exposure without agency; from non-nuclear capitals it is shelter purchased with hostage-status; from the analytical seat it is a settled result. The identity-lock on the commands is institutional fusion: the organization has become its function, so exit (dismantling the force) is unthinkable from inside the professional identity that the mission constitutes - if that frame broke, the administrator seat would recompute from steward to potential fixer. The divergence between the administrator seat's experience (stewardship) and the powerless seats' experience (hostagehood) is the story's central perspectival fact, and it is computed by the engine from the structural data, not asserted by the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure: victims are global_civilian_population and future_generations (universal, powerless, trapped - derivation places both near the full-target end, amplified further by the constraint's universal scope in the engine's arithmetic, which it owns); no beneficiaries are declared, because no seat collects from the boundary's operation. Two overrides correct derivations the structural data alone would get wrong: (1) institutional -> 0.58 - both institutional seats administer and bear without capturing, but a generic institutional/agenda-setter derivation assumes administration implies capture-side benefit, which is false here; (2) moderate -> 0.45 - non_nuclear_states carry the victim declaration, which alone would drive them toward the target end, but their net position is slightly sheltered (secondary beneficiary role): protected by the limit, taxed by hostage exposure. The excluded organizer seat (disarmament_movements) is deliberately left to fallback per the R3 rule that authored absences must not drive classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The receipt surface makes this constraint look piton-shaped - diffuse gains, prohibitive fixing - and the R5 interview sharpens the trap: the founding problem (recurrent great-power total war) is dead, and the arrangement persists. But the piton test is cost-asymmetry plus atrophied function, and this limit's function is intact (theater 0.48, accessibility_collapse 0.92): the problem is dead BECAUSE the arrangement holds, which is load-bearing persistence, not vestigial drift. Conversely, the mandatrophy lens prevents the opposite error: reading the dead founding problem as license to treat the boundary as obsolete (the contingent_reachability move) would mistake the extinguisher for the fire. The classification that survives both errors is the one this reading asserts: a fixed limit whose founding problem it consumed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status_reachability_modality,
    'This story instantiates the contraction_reading of the total_war_reachability_boundary kernel: is winnable total war permanently outside the feasible set (this reading), merely improbable while remaining reachable (dropping_reading), or contingently unreachable and technologically reversible (contingent_reachability_reading)?',
    'A demonstrated war-winning pathway in credible wargaming or practice would refute this reading; verified multilateral disarmament that leaves the boundary intact would strengthen it; the sibling stories carry the competing classifications as separate constraints.',
    'If dropping_reading is right, this constraint recomputes as a rope (coordination equilibrium) with real beneficiaries; if contingent_reachability is right, it recomputes as a piton awaiting technological reversal; the mountain classification holds only under permanence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_status_reachability_modality, empirical, 'Which modality of the reachability boundary holds: permanent exclusion, probabilistic reduction, or reversible contraction.').

omega_variable(
    natural_law_vs_maintained_construct,
    'Is the boundary a genuine natural-law limit (a mountain), or a constructed condition contingent on human-maintained arsenals - an artifact whose naturality is inherited from physics but whose continued existence is a standing choice?',
    'Counterfactual analysis of verified multilateral disarmament: if the boundary would survive substrate removal (via reconstitution knowledge and delivery-system latency), it approaches natural-law status; if it dissolves with the arsenals, it is a maintained construct.',
    'If constructed-and-maintained, the mountain claim weakens toward rope or scaffold, and the administering commands acquire a maintenance-beneficiary structure this reading denies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_maintained_construct, conceptual, 'Whether the contraction is a fact of the world or a fact of the arsenal.').

omega_variable(
    universal_risk_extraction_vs_hazard_profile,
    'Does the universal extinction-risk bearing declared here constitute extraction (victims without a capturer) or merely the hazard profile of a protective limit - cost without transfer?',
    'Decompose epsilon into a transfer component (resources moved to any seat) and a risk-bearing component (exposure with no recipient); if transfer dominates and a seat captures it, the structure is extractive despite the mountain profile.',
    'If risk-bearing dominates with no capture, the mountain classification stands with a universal victim set; if a capture seat emerges (for example the weapons-industrial complex), the constraint recomputes toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_risk_extraction_vs_hazard_profile, conceptual, 'Whether universal cost-bearing under a capturer-less limit counts as extraction.').

omega_variable(
    signaling_theater_function_erosion,
    'Is the rising signaling theater around the boundary (measurement series) eroding the boundary''s function, or parasitic on its fixity - performance layered on a limit that performs regardless?',
    'Crisis-behavior analysis: if theater-driven misreading (Able Archer-class incidents) increasingly produces boundary-threatening behavior, theater is eroding function; if crises resolve with the limit intact, theater is parasitic.',
    'Theater sustained above 0.5 with function erosion would push the computed type toward piton; parasitic theater leaves the mountain classification untouched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signaling_theater_function_erosion, empirical, 'Whether growing performative signaling threatens the boundary''s operative core.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_reachability_boundary__contraction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(tota_tr_t0, observed).
narrative_ontology:measurement(tota_tr_t8, total_war_reachability_boundary__contraction_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement_basis(tota_tr_t8, observed).
narrative_ontology:measurement(tota_tr_t17, total_war_reachability_boundary__contraction_reading, theater_ratio, 17, 0.15).
narrative_ontology:measurement_basis(tota_tr_t17, observed).
narrative_ontology:measurement(tota_tr_t25, total_war_reachability_boundary__contraction_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(tota_tr_t25, observed).
narrative_ontology:measurement(tota_tr_t40, total_war_reachability_boundary__contraction_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(tota_tr_t40, observed).
narrative_ontology:measurement(tota_tr_t52, total_war_reachability_boundary__contraction_reading, theater_ratio, 52, 0.38).
narrative_ontology:measurement_basis(tota_tr_t52, observed).
narrative_ontology:measurement(tota_tr_t65, total_war_reachability_boundary__contraction_reading, theater_ratio, 65, 0.42).
narrative_ontology:measurement_basis(tota_tr_t65, observed).
narrative_ontology:measurement(tota_tr_t80, total_war_reachability_boundary__contraction_reading, theater_ratio, 80, 0.48).
narrative_ontology:measurement_basis(tota_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_reachability_boundary__contraction_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(tota_be_t0, observed).
narrative_ontology:measurement(tota_be_t8, total_war_reachability_boundary__contraction_reading, base_extractiveness, 8, 0.13).
narrative_ontology:measurement_basis(tota_be_t8, observed).
narrative_ontology:measurement(tota_be_t17, total_war_reachability_boundary__contraction_reading, base_extractiveness, 17, 0.21).
narrative_ontology:measurement_basis(tota_be_t17, observed).
narrative_ontology:measurement(tota_be_t25, total_war_reachability_boundary__contraction_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement_basis(tota_be_t25, observed).
narrative_ontology:measurement(tota_be_t40, total_war_reachability_boundary__contraction_reading, base_extractiveness, 40, 0.2).
narrative_ontology:measurement_basis(tota_be_t40, observed).
narrative_ontology:measurement(tota_be_t52, total_war_reachability_boundary__contraction_reading, base_extractiveness, 52, 0.1).
narrative_ontology:measurement_basis(tota_be_t52, observed).
narrative_ontology:measurement(tota_be_t65, total_war_reachability_boundary__contraction_reading, base_extractiveness, 65, 0.09).
narrative_ontology:measurement_basis(tota_be_t65, observed).
narrative_ontology:measurement(tota_be_t80, total_war_reachability_boundary__contraction_reading, base_extractiveness, 80, 0.16).
narrative_ontology:measurement_basis(tota_be_t80, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_reachability_boundary__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, nuclear_taboo_norm).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, npt_bargain).

% DUAL FORMULATION NOTE:
% The colloquial label 'the nuclear revolution' decomposes, per the epsilon-invariance principle, into three structurally distinct claims about the same kernel: this file (contraction_reading) authors the strong claim - permanent exclusion of winnable total war, mountain, epsilon indexed to universal risk-bearing with no capture; dropping_reading authors the probabilistic claim (rope: deterrence as coordination equilibrium, reachability retained); contingent_reachability_reading authors the reversibility claim (piton: atrophied capability awaiting technological reversal). Each sibling carries its own epsilon, beneficiary/victim structure, and claimed type. The contraction claim is upstream in the sense that both weaker readings concede current unreachability and dispute only its modality. Edges run from this file to both siblings and to the downstream structures the boundary underwrites (nuclear taboo norm, NPT bargain).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__contraction_reading, institutional, 0.58).
constraint_indexing:directionality_override(total_war_reachability_boundary__contraction_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
