% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__lapsed_alternative_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: market_naturalization__lapsed_alternative_reading
 *   human_readable: Market Dominance as Lapsed Alternative Closure
 *   domain: political_economy/economic_history
 *
 * SUMMARY:
 *   This reading instantiates market dominance as a lapsed closure—a
 *   constraint that once required active maintenance (incumbent firms
 *   defended their positions through exclusive contracts, litigation,
 *   predatory pricing) but has degraded into pure inertia. Under this
 *   reading, no identifiable beneficiary class actively maintains dominance
 *   today. Instead, alternatives have atrophied through decades of non-use;
 *   switching costs are structural rather than imposed. The constraint
 *   persists because the cost to entrants exceeds any visible benefit, not
 *   because dominant firms deploy enforcement machinery. The theater ratio
 *   rises as extractiveness falls: dominant firms spend increasing effort on
 *   justificatory rhetoric ('we earned this through innovation,' 'the market
 *   chose us') rather than on active defense. The performance of
 *   market-excellence narratives replaces the active suppression of
 *   alternatives. This reading directly contests the beneficiary-maintained
 *   reading, which posits active incumbent defense, and coexists with the
 *   hybrid reading, which acknowledges both historical atrophy and residual
 *   enforcement.
 *
 * KEY AGENTS:
 *   - incumbent_market_leaders: Powerful, globally-scoped actors who benefit incidentally from dominance but do not actively defend it
 *   - potential_market_entrants: Moderate-power actors facing structural barriers (switching costs, network effects, scale economics) rather than active exclusion
 *   - regulatory_authorities: Institutional observers searching for enforcement mechanisms and finding little systematic exclusionary conduct
 *   - consumers_and_users: Powerless, structurally excluded from governance of market alternatives
 *   - institutional_memory_carriers: Analytical observers documenting the historical shift from active to lapsed maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.28).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.15).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Alternative Closure").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political_economy/economic_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, '5ae62ef8-a13f-48fc-ba0c-f7ac16961637').
narrative_ontology:cs_kernel_codification('5ae62ef8-a13f-48fc-ba0c-f7ac16961637', distributed).
narrative_ontology:cs_authority_grounding('5ae62ef8-a13f-48fc-ba0c-f7ac16961637', practice).
narrative_ontology:cs_interpretation_layer_present('5ae62ef8-a13f-48fc-ba0c-f7ac16961637').
narrative_ontology:cs_reading_relation('5ae62ef8-a13f-48fc-ba0c-f7ac16961637', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('5ae62ef8-a13f-48fc-ba0c-f7ac16961637', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_axiom('5ae62ef8-a13f-48fc-ba0c-f7ac16961637', foundational, market_dominance_structurally_self_perpetuating).
narrative_ontology:cs_axiom_status(market_dominance_structurally_self_perpetuating, holdable).
narrative_ontology:cs_axiom_grounding('5ae62ef8-a13f-48fc-ba0c-f7ac16961637', market_dominance_structurally_self_perpetuating, empirically_contingent).
narrative_ontology:cs_axiom('5ae62ef8-a13f-48fc-ba0c-f7ac16961637', foundational, incumbent_enforcement_effort_has_substantially_declined).
narrative_ontology:cs_axiom_status(incumbent_enforcement_effort_has_substantially_declined, holdable).
narrative_ontology:cs_axiom_grounding('5ae62ef8-a13f-48fc-ba0c-f7ac16961637', incumbent_enforcement_effort_has_substantially_declined, empirically_contingent).
narrative_ontology:cs_reference_frame('5ae62ef8-a13f-48fc-ba0c-f7ac16961637', lapsed_closure_equilibrium).
narrative_ontology:cs_drift_state('5ae62ef8-a13f-48fc-ba0c-f7ac16961637', contemporary_high_theater_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5ae62ef8-a13f-48fc-ba0c-f7ac16961637', '').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__lapsed_alternative_reading, incumbent_market_leaders).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, potential_market_entrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupy dominant market positions that persist through inertia rather than active defense. They benefit incidentally from the closure of alternatives—their market share remains substantial without requiring constant enforcement machinery. They articulate market dominance as natural outcome of efficiency and consumer choice, not as a defended position.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, incumbent_market_leaders, beneficiary,
    powerful, generational, arbitrage, global).

% Face high barriers to entry that persist through path-dependency and scale economics rather than active exclusion. The barrier is structural—networks, switching costs, installed base—not enforced through litigation or regulatory capture. They encounter a closed market because alternatives have atrophied through non-use over decades, not because incumbents actively suppress them.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, potential_market_entrants, payer,
    moderate, biographical, constrained, global).

% Observe market concentration and attempt to identify the enforcement mechanism sustaining it. Under the lapsed-closure reading, they find no identifiable beneficiary class actively defending dominance, no systematic exclusionary conduct, no organized cartel. The concentration appears as sedimented historical outcome rather than as a maintained extraction.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% Operate within dominant platforms and ecosystem choices that present as natural or inevitable. They would benefit from competitive alternatives but are not parties to any conversation about market structure. Their exclusion from the constraint's governance is structural—they have no seat in determining what alternatives remain viable.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, consumers_and_users, excluded,
    powerless, biographical, constrained, global).

% Document and interpret the historical transition from actively maintained market closure (earlier competitive stages) to lapsed closure (contemporary dominance requiring minimal enforcement). Their role is retrospective: to distinguish the mechanism sustaining dominance today from the mechanisms that fought off challengers decades earlier.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, institutional_memory_carriers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, legible market structure: dominant firms maintain technological standards, backward compatibility, network effects. Users and smaller firms coordinate around these de facto standards without requiring active enforcement of the standard itself.
% TRANSFER_FUNCTION: Transfers value from potential entrants and users to incumbent firms through price-setting power and margin capture, sustained by the structural impossibility of migration rather than by active exclusionary conduct. No party collects from maintenance; the constraint persists by inertia.
% ABSENT_VOICES: Alternate technological architectures and competing platform designs that would have been viable if chosen decades earlier are not represented because they never crystallized into market-ready alternatives. Path-dependent losers are absent not because excluded but because their absence made them losers.
% DISAPPEARANCE_RATIONALE: The lapsed-closure reading asserts that if dominant firms ceased all enforcement activity (ceased litigating, stopped pushing exclusive contracts, stopped lobbying), the market structure would persist through inertia for years—consumers would not spontaneously switch, alternatives would not spontaneously emerge. However, over a long enough horizon (decades), alternative architectures might develop in parallel, eventually offering entry points. The contestation centers on the time scale: is dominance truly self-perpetuating (world_unchanged in the timescale that matters), or do long-term forces eventually rearrange (world_rearranges at a century scale)?
% FOUNDING_PROBLEM: Early market formation required coordination on a single dominant platform to achieve network effects and interoperability; fragmented alternatives were inferior. The founding problem was solved by the emergence of a winner.
% FOUNDING_PROBLEM_CORROBORATION: Technology historians document that the early coordination problem was genuinely difficult and the dominant solution was an improvement. However, contemporary economic analysis argues the problem is now solved—alternatives could be viable today without sacrificing coordination value. The dead-status assessment comes from sources outside the incumbent beneficiary set: academic economists, antitrust analysts, and technology critics who argue that the founding-problem justification no longer applies.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, contested).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__lapsed_alternative_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__lapsed_alternative_reading_tests).
:- end_tests(market_naturalization__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) under this reading because the constraint produces coordination benefits (stable standards, backward compatibility, predictable market) that genuinely reduce transaction costs—the measured extraction approaches the coordination cost floor. Suppression is low (0.15) because alternatives are closed by structural factors (switching costs, network lock-in, scale economics) rather than by active coercion or litigation. Theater ratio is high (0.62) because incumbent firms spend substantial effort justifying their dominance through efficiency claims, innovation narratives, and consumer-benefit rhetoric, which mask the fact that they perform little active defense. The time-series measurements show extractiveness declining and theater rising, consistent with a constraint shifting from active enforcement to performative maintenance. Accessibility collapse is high (0.71) because once-viable alternatives have disappeared from the feasible choice set—not through suppression but through path-dependency. Resistance is low (0.22) because entrants lack the coordination capacity to mount effective challenges; they resist individually but cannot organize.
 *
 * PERSPECTIVAL GAP:
 *   Incumbent firms perceive dominance as earned through innovation and superior service (pure coordination, no extraction). Regulatory observers perceive a concentration of market power that should be contested. Under the lapsed reading, neither party is entirely wrong: incumbents genuinely provide coordination benefits AND the market is genuinely closed, but through structural factors rather than incumbent defense. This is precisely the reading that allows the engine to classify the constraint as piton (performance of superiority, minimal enforcement).
 *
 * DIRECTIONALITY LOGIC:
 *   Under the lapsed-closure reading, no beneficiary class is named because the reading asserts no identifiable beneficiary actively maintains dominance. Potential entrants are named as payers (they bear the cost of high barriers) but the constraint extracts to no specific seat—the value dissipates into incumbent margin capture, which is a diffuse outcome of market structure rather than a consolidated rent collected by a defending agent. This directionality structure produces a piton: the constraint persists by inertia, the cost is borne diffusely by entrants and users, no party profits enough to maintain it and no party bears costs concentrated enough to mobilize fixing it. The gap between the articulated justification (market efficiency, consumer choice) and the measured operation (alternatives atrophied, entry barriers structural) is the engine's measure of theater.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early coordination through dominant platform) is now dead: alternatives could be viable today. The constraint persists not because the founding problem justifies it but because the cost to rearrange the installed base exceeds the perceived benefit. The mandatrophy is resolved by the lapsed-closure reading: the constraint once solved a real problem (coordination in early-stage markets) and now persists through inertia, the archetypal mandatrophic shape. The high theater ratio (performance of efficiency) and low suppression (no active defense) mark the transition from rope (genuine coordination, low theater) to piton (coordination function atrophied, theater high).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_maintenance_vs_structural_inertia,
    'Does market dominance persist because incumbent firms actively defend it (through litigation, exclusionary contracts, regulatory capture), or because alternatives have atrophied and entrants lack the coordination capacity to challenge it?',
    'Comparative analysis: (1) measure enforcement effort (litigation, exclusivity clauses, regulatory lobbying) by incumbent firms over time; (2) assess whether entrant failure is attributable to incumbent action or to structural barriers (switching costs, network effects) that would persist even without incumbent defense; (3) natural experiments from jurisdictions with intervention: did removing enforcement mechanisms lead to rapid entry, or did structural barriers persist?',
    'If active maintenance dominates, the constraint is snare (beneficiary_maintained_reading). If structural inertia dominates, the constraint is piton (lapsed_alternative_reading). The empirical answer determines both the reading and the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_maintenance_vs_structural_inertia, empirical, 'Whether market dominance is actively defended or structurally self-perpetuating').

omega_variable(
    founding_problem_permanence,
    'Would the founding problem (coordination failure in fragmented markets) re-emerge if alternatives suddenly became available, or is the coordination function now provided by ecosystems and standards that exist independent of dominance?',
    'Thought experiment with historical precedent: examine cases where dominant platforms lost market share (MySpace, Yahoo, Kodak)—did fragmentation recur or did new dominant platforms emerge? Did the ecosystem coordinate more poorly during transitions?',
    'If fragmentation recurs, the constraint still solves a live problem (rope reading, coordination justified). If new dominance merely replaces old, the problem was never about fragmentation but about market concentration (snare or piton reading, extraction justified).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_permanence, conceptual, 'Whether the founding problem persists or has been structurally solved').

omega_variable(
    natural_law_vs_manufactured_closure,
    'Is high accessibility_collapse (0.71) evidence of natural market forces (efficient firms naturally win) or manufactured closure (alternatives were foreclosed through choices made decades earlier)?',
    'Historical counterfactual analysis: identify decision points where alternate architectures were rejected (standards wars, acquisition of competitors, exclusive deals). If such points exist and were chosen by incumbents, the closure is manufactured; if the collapsed alternatives were never viable (network effects made them uncompetitive), the closure is natural.',
    'Natural collapse supports the lapsed-closure reading (piton). Manufactured collapse suggests the beneficiary_maintained reading (snare). The distinction determines whether the constraint should be reclassified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_manufactured_closure, conceptual, 'Whether market concentration is emergent or constructed').

omega_variable(
    reading_kernel_contest,
    'Is ''market naturalization'' fundamentally contested between readings that assign different maintenance mechanisms, or is the contest really about whether markets are good things (an observer-axis question)?',
    'Structural analysis: if the readings agree on what incumbents DO but disagree about whether what they do is good (efficiency vs. extraction), the contest is observer-axis, not kernel. If the readings disagree on what incumbents DO (active defense vs. passive benefit), the contest is kernel.',
    'If kernel: this constraint and its sibling readings are genuinely different constraints with different ε values. If observer-axis: the readings are the same constraint viewed from opposed perspectives. The kernel reading frames it as different-ε constraint family; observer framing treats it as one constraint with contested evaluation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Whether market naturalization is a kernel reading or an observer-axis disagreement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__lapsed_alternative_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mark_tr_t8, market_naturalization__lapsed_alternative_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(mark_tr_t16, market_naturalization__lapsed_alternative_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement(mark_tr_t24, market_naturalization__lapsed_alternative_reading, theater_ratio, 24, 0.56).
narrative_ontology:measurement(mark_tr_t32, market_naturalization__lapsed_alternative_reading, theater_ratio, 32, 0.61).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__lapsed_alternative_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__lapsed_alternative_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(mark_be_t8, market_naturalization__lapsed_alternative_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(mark_be_t16, market_naturalization__lapsed_alternative_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(mark_be_t24, market_naturalization__lapsed_alternative_reading, base_extractiveness, 24, 0.32).
narrative_ontology:measurement(mark_be_t32, market_naturalization__lapsed_alternative_reading, base_extractiveness, 32, 0.28).
narrative_ontology:measurement(mark_be_t40, market_naturalization__lapsed_alternative_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__lapsed_alternative_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(mark_su_t8, market_naturalization__lapsed_alternative_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(mark_su_t16, market_naturalization__lapsed_alternative_reading, suppression_requirement, 16, 0.22).
narrative_ontology:measurement(mark_su_t24, market_naturalization__lapsed_alternative_reading, suppression_requirement, 24, 0.18).
narrative_ontology:measurement(mark_su_t32, market_naturalization__lapsed_alternative_reading, suppression_requirement, 32, 0.15).
narrative_ontology:measurement(mark_su_t40, market_naturalization__lapsed_alternative_reading, suppression_requirement, 40, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% Market naturalization is one contested kernel with three distinct readings: lapsed_alternative_reading (this constraint, claiming dominance requires no active maintenance), beneficiary_maintained_reading (claiming dominance is actively defended), and hybrid_reading (claiming both lapsed and active elements). Each reading instantiates a different ε, different beneficiary/victim structure, and different classification. The readings are linked through network.affects_constraints to indicate kernel family relationship. The contest is located in the empirical question of enforcement effort over time and the structural question of whether alternatives have atrophied through non-use or been actively suppressed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
