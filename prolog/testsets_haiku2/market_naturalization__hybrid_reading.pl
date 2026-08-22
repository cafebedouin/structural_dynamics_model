% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__hybrid_reading, []).

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
 *   constraint_id: market_naturalization__hybrid_reading
 *   human_readable: Market Dominance via Mixed Maintenance (Hybrid Reading)
 *   domain: economic/political
 *
 * SUMMARY:
 *   This is the HYBRID READING of the market_naturalization kernel. Market
 *   dominance is instantiated here as a mixed constraint combining lapsed
 *   elements (where alternatives have genuinely become non-functional and
 *   require no active maintenance to remain inert) with actively maintained
 *   barriers (gatekeeping, exclusionary contracts, strategic integration).
 *   The constraint is CLAIMED as tangled_rope (real coordination function +
 *   asymmetric extraction with enforcement) reflecting the hybrid structure:
 *   coordination benefits (scale, integration, network effects) are genuine
 *   and persist passively; extraction (margin inflation, competitor
 *   exclusion, consumer lock-in) is actively defended. This reading coexists
 *   with two siblings: the beneficiary_maintained reading claims incumbents
 *   actively defend even the lapsed barriers, and the lapsed_alternative
 *   reading claims most barriers are purely structural artifacts requiring no
 *   active maintenance. The metrics authored here (moderate-rising
 *   extractiveness, growing theater ratio, rising suppression requirement)
 *   describe a constraint where maintenance is increasing — suggesting the
 *   barrier is actively hardening, not drifting toward pure lapse.
 *
 * KEY AGENTS:
 *   - incumbent_market_holders: Benefit from both passive lock-in and active gatekeeping; institutional power
 *   - distribution_infrastructure_operators: Set and enforce the rules; institutional power; highest d directionality
 *   - potential_market_entrants: Face mixed barriers; moderate power; constrained exit
 *   - displaced_competitors: Powerless; trapped; bore sunk costs of earlier competition
 *   - consumers: Symmetric position; benefit from integration/scale but pay through reduced choice
 *   - antitrust_regulators: Excluded from rule-setting; observer position; doctrinal uncertainty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.58).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.51).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Dominance via Mixed Maintenance (Hybrid Reading)").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "economic/political").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, '4649b693-3188-4e64-ab83-7a0092873b41').
narrative_ontology:cs_kernel_codification('4649b693-3188-4e64-ab83-7a0092873b41', distributed).
narrative_ontology:cs_authority_grounding('4649b693-3188-4e64-ab83-7a0092873b41', extraction).
narrative_ontology:cs_interpretation_layer_present('4649b693-3188-4e64-ab83-7a0092873b41').
narrative_ontology:cs_reading_relation('4649b693-3188-4e64-ab83-7a0092873b41', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('4649b693-3188-4e64-ab83-7a0092873b41', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_axiom('4649b693-3188-4e64-ab83-7a0092873b41', foundational, dominance_combines_passive_and_active_barriers).
narrative_ontology:cs_axiom_status(dominance_combines_passive_and_active_barriers, holdable).
narrative_ontology:cs_axiom_grounding('4649b693-3188-4e64-ab83-7a0092873b41', dominance_combines_passive_and_active_barriers, empirically_contingent).
narrative_ontology:cs_axiom('4649b693-3188-4e64-ab83-7a0092873b41', secondary, coordination_value_erodes_over_time).
narrative_ontology:cs_axiom_status(coordination_value_erodes_over_time, holdable).
narrative_ontology:cs_axiom_grounding('4649b693-3188-4e64-ab83-7a0092873b41', coordination_value_erodes_over_time, empirically_contingent).
narrative_ontology:cs_reference_frame('4649b693-3188-4e64-ab83-7a0092873b41', early_dominance_founded_on_genuine_coordination).
narrative_ontology:cs_drift_state('4649b693-3188-4e64-ab83-7a0092873b41', contemporary_hyperscale_infrastructure_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4649b693-3188-4e64-ab83-7a0092873b41', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_market_holders).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, distribution_infrastructure_operators).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, potential_market_entrants).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, displaced_competitors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, consumers_and_end_users).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, consumers_and_end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominant firms that retain market position through a combination of structural lock-in (where alternatives have naturally lapsed and network effects persist) and active maintenance (lobbying, strategic exclusion, proprietary integration). They benefit from both passive entrenchment and the enforcement machinery they fund.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_market_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Operators of critical infrastructure (platforms, supply chains, logistics) that have evolved to favor incumbent firms. They set and enforce access rules that combine inherited technical standards (no longer justifiable by original function) with active gatekeeping policies that exclude rivals. They collect rents through licensing, access fees, or contractual terms.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, distribution_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Firms attempting to enter or compete in established markets. They face barriers that are partly passive (technical standards that emerged decades ago, cultural expectations, installed-base effects) and partly active (exclusionary contracts, access denial, strategic pricing). The mixed nature of the barrier makes it harder to contest any single component.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, potential_market_entrants, payer,
    moderate, biographical, constrained, global).

% Firms that once competed successfully but were progressively frozen out as dominance hardened. They bear the sunk costs of earlier competitive positioning and lack the scale or coordination to mount legal or technological challenges to the evolving barriers.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, displaced_competitors, payer,
    powerless, biographical, trapped, regional).

% Benefit from incumbent firms' scale, reliability, and integrated ecosystems (genuine coordination benefits); also pay through reduced choice, higher prices where competition is suppressed, and vendor lock-in. Their exit options vary by substitutability of the good.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, consumers_and_end_users, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, consumers_and_end_users, payer).

% Maintain formal standards that began as coordination mechanisms but have calcified into barriers. They observe (but rarely overrule) how incumbents cite backward-compatibility requirements and installed-base effects to resist standards evolution that would lower entry barriers.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, technology_standards_bodies, observer,
    powerful, generational, analytical, global).

% Tasked with enforcing competitive markets but face uncertainty about which barriers are structural artifacts (and thus acceptable) versus active suppression (and thus actionable). The hybrid nature of the constraint makes intervention doctrine contested.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, antitrust_regulators, excluded,
    institutional, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__hybrid_reading, incumbent_market_holders).
narrative_ontology:fixing_cost_class(market_naturalization__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Incumbent dominance combines real coordination benefits (scale economies, network effects, integrated ecosystems that lower transaction costs) with structural lock-in where alternatives have genuinely become non-functional (older technical standards, extinct competitor ecosystems, path-dependent user expectations).
% TRANSFER_FUNCTION: Market position and the rents attached to it flow from potential entrants and displaced competitors to incumbent holders and infrastructure operators. The transfer is enabled partly by passive lapse (alternatives are no longer viable on their own) and partly by active maintenance (exclusionary contracts, proprietary integration, gatekeeping).
% ABSENT_VOICES: Potential market entrants and displaced competitors would object that barriers are unjustifiable and could be lowered; they are structurally excluded from rule-setting because the gatekeepers control access to the market itself. Antitrust regulators would contest whether the mixed nature of the barrier justifies intervention; they are excluded from unambiguous doctrine.
% DISAPPEARANCE_RATIONALE: If the active maintenance machinery vanished (exclusionary contracts lifted, gatekeeping relaxed, standards evolved) while the passive structural barriers remained, new entrants would emerge within months in some domains (software, digital services) but struggle for years in others (physical infrastructure, logistics networks). The constraint's disappearance would be partial, domain-dependent, and staged — not a clean reorganization.
% FOUNDING_PROBLEM: Early dominance reflected genuine competitive superiority and technical coordination value. Incumbents built integrated systems when fragmentation was costly and switching was expensive. The constraint emerged to protect that investment and coordination benefit.
% FOUNDING_PROBLEM_CORROBORATION: Incumbents and infrastructure operators attest the founding coordination problem is still live, citing ongoing complexity and switching costs. Potential entrants and independent technologists attest the problem is largely solved in many domains (modular interfaces, cloud-based services, containerization reduce lock-in); their testimony is excluded from rule-setting but visible in industry journals and technical conferences. Antitrust economists split on whether residual coordination benefits justify maintained barriers.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-rising (0.38→0.58 over the interval) because the constraint initially combines real coordination benefits (inherited from genuine earlier value) with modest extraction. Over time, extraction rises as the active maintenance machinery hardens — incumbents invest in gatekeeping, standards bodies resist evolution, contracts become more exclusionary. Theater ratio rises similarly (0.25→0.44), suggesting performative defense of 'coordination' framing increases as the founding coordination rationale weakens. Suppression requirement rises (0.32→0.51), reflecting that more active enforcement is needed to sustain the barrier as passive alternatives could otherwise revive if gatekeeping relaxed. The plateau after t=25 suggests stabilization: the constraint reaches an equilibrium where active maintenance fully replaces passive lapse, and further extraction becomes politically costly. Accessibility collapse is moderate (0.62) because some alternatives remain theoretically possible (regulatory intervention, standards evolution, coordinated entry) but are politically blocked. Resistance is below suppression (0.47 < 0.51), indicating displaced competitors and entrants resist but lack the power to overcome actively-maintained barriers.
 *
 * PERSPECTIVAL GAP:
 *   Incumbent_market_holders and infrastructure_operators should compute as beneficiaries with low directionality (near 0.0); they benefit from coordination and control the enforcement machinery. Potential_entrants should compute as targets with high directionality (near 1.0); they face both structural barriers and active suppression. Displaced_competitors should compute as high-d targets (trapped exit makes them most vulnerable). Consumers should compute near symmetric (d~0.5): they benefit from integration/scale (pushing d toward 0.0) but pay through reduced choice and lock-in (pushing d toward 1.0). The engine derives these from the structural data; the hybrid reading's claim is that this mixed directionality structure (some genuine beneficiaries, some trapped targets, some symmetric actors) is what distinguishes it from pure beneficiary_maintained (where incumbents actively defend even lapsed barriers) and pure lapsed_alternative (where all barriers are passive artifacts).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are incumbent_market_holders and infrastructure_operators because they collect rents from both passive entrenchment (inherited market position) and active gatekeeping (they control access rules and enforcement). They have arbitrage-level exit (can shift to adjacent markets or enforce globally). Their d values should derive low (~0.1-0.2). Victims are potential_entrants and displaced_competitors because they face both structural barriers (non-functional alternatives, network effects, switching costs) and active suppression (contract terms, standards resistance, access denial). Entrants have constrained exit (can only try different markets or acquire scale elsewhere); displaced_competitors are trapped. Their d values should derive high (~0.75-0.9 for trapped, ~0.6-0.75 for constrained). Consumers have moderate exit (can switch between dominant firms in some domains, trapped in others), making them symmetric on average. The mixed beneficiary structure (real coordination benefits coexist with extraction) is the signature of the hybrid reading and distinguishes it from siblings that claim either active maintenance across the board (beneficiary_maintained) or pure passive lapse (lapsed_alternative).
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading avoids mandatrophy collapse by preserving genuine coordination function as inseparable from the extraction mechanism. The founding problem (coordination in early dominance) is contested as to whether it remains live: incumbents attest yes (ongoing complexity, switching costs); entrants attest no (modularity and containerization have solved integration). The measurement series shows rising theater ratio (performative defense is increasing), which under pure Rope classification would indicate falsification, but under Tangled_Rope it is diagnostic of the hybrid mechanism: as passive barriers erode and alternatives become technically viable, more active maintenance (legislative engagement, standards resistance, contract tightening) is needed to sustain the constraint. The rising extractiveness combined with rising theater ratio is exactly the pattern Tangled_Rope should show when coordination value is declining but extraction value is being defended. The constraint does NOT collapse into pure Snare because genuine coordination benefits remain measurable (network effects, scale economies, integrated user experience); it does NOT collapse into pure Rope because extraction is actively enforced against would-be entrants who could otherwise compete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    passive_vs_active_barrier_decomposition,
    'For a specific market (software, telecommunications, finance, etc.), which barriers are genuinely passive artifacts (network effects, installed base, technical standards) that would persist even without incumbent gatekeeping, and which are actively maintained (exclusionary contracts, standards resistance, strategic pricing)?',
    'Counterfactual observation: temporarily relax active gatekeeping (mandatory interoperability, standards evolution, contract transparency) and measure whether new entrants emerge quickly or whether structural barriers suffice to maintain dominance.',
    'If most barriers persist without active maintenance, the constraint reclassifies toward lapsed_alternative reading; if most barriers collapse quickly when gatekeeping stops, the constraint reclassifies toward beneficiary_maintained reading (active defense is necessary). The hybrid reading is verified if the answer is domain-specific and time-dependent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(passive_vs_active_barrier_decomposition, empirical, 'Whether market dominance depends more on lapsed structure or active maintenance.').

omega_variable(
    founding_coordination_problem_persistence,
    'Is the original coordination problem (solving integration, switching costs, fragmentation risk) still live in contemporary market conditions, or has technological evolution (modularity, APIs, containerization, cloud infrastructure) already solved most of it?',
    'Independent assessment by entrants and technical experts: can a new competitor build viable alternatives if gatekeeping were relaxed? If yes, the founding problem is solved and present extraction is unjustified; if no, the problem is still live and present extraction is a coordination cost.',
    'If the founding problem is dead but extraction persists, the constraint reclassifies fully to Snare (false coordination framing). If the founding problem is live and inseparable from extraction, Tangled_Rope is verified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_coordination_problem_persistence, empirical, 'Whether the original coordination rationale for dominance remains valid.').

omega_variable(
    reading_distinction_under_policy_intervention,
    'If antitrust regulators mandate interoperability and standards evolution, does market dominance collapse quickly (supporting lapsed_alternative or beneficiary_maintained readings depending on why collapse occurs), or does it persist through selective customer loyalty and integration depth despite lower technical barriers (supporting hybrid reading)?',
    'Natural experiments from EU regulatory interventions (GDPR, DMA, interoperability mandates) in digital markets over 5-10 years: track whether new entrants emerge, whether incumbents'' market share erodes, and whether suppression intensity increases to compensate.',
    'Rapid collapse would falsify hybrid reading; persistence despite lower barriers would verify it. The direction and speed of adaptation is the key diagnostic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_distinction_under_policy_intervention, empirical, 'Whether regulatory relaxation of active barriers leaves market dominance intact (hybrid/lapsed) or causes collapse (beneficiary_maintained).').

omega_variable(
    committer_frame_sibling_coexistence,
    'Can the beneficiary_maintained reading and the hybrid reading be held simultaneously by different parties within a single institutional framework, or do they logically foreclose each other?',
    'Examine antitrust doctrine evolution: does the doctrine accommodate both readings (some dominance defended as coordination, some contested as pure extraction), or do courts settle on one reading and exclude the other?',
    'Coexistence indicates the readings are institutionally live in different contexts or factions; foreclosure indicates one reading has gained hegemony. The frame determination (which reading is ''real'') is itself the omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_sibling_coexistence, conceptual, 'The committer-frame under-determination: whether sibling readings logically coexist or foreclose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mark_tr_t5, market_naturalization__hybrid_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(mark_tr_t10, market_naturalization__hybrid_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(mark_tr_t15, market_naturalization__hybrid_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(mark_tr_t20, market_naturalization__hybrid_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(mark_tr_t25, market_naturalization__hybrid_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement(mark_tr_t30, market_naturalization__hybrid_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__hybrid_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mark_be_t5, market_naturalization__hybrid_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(mark_be_t10, market_naturalization__hybrid_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(mark_be_t15, market_naturalization__hybrid_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(mark_be_t20, market_naturalization__hybrid_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(mark_be_t25, market_naturalization__hybrid_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(mark_be_t30, market_naturalization__hybrid_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(mark_be_t40, market_naturalization__hybrid_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(mark_su_t5, market_naturalization__hybrid_reading, suppression_requirement, 5, 0.36).
narrative_ontology:measurement(mark_su_t10, market_naturalization__hybrid_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement(mark_su_t15, market_naturalization__hybrid_reading, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(mark_su_t20, market_naturalization__hybrid_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(mark_su_t25, market_naturalization__hybrid_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement(mark_su_t30, market_naturalization__hybrid_reading, suppression_requirement, 30, 0.51).
narrative_ontology:measurement(mark_su_t40, market_naturalization__hybrid_reading, suppression_requirement, 40, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__hybrid_reading, 0.18).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__lapsed_alternative_reading).

% DUAL FORMULATION NOTE:
% market_naturalization is a contested kernel decomposed into three constraint readings. The hybrid_reading (this file) claims dominance combines passive structural lapse with active maintenance, resulting in moderate extractiveness (0.58) and rising theater ratio (0.44). It coexists with beneficiary_maintained_reading (incumbents actively defend all barriers; higher extraction) and lapsed_alternative_reading (barriers are purely passive; lower extraction). The network linking all three enables cross-reading comparison and mismatch detection. Empirical resolution depends on counterfactual relaxation of active gatekeeping in controlled domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_naturalization__hybrid_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
