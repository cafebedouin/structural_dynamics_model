% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Market Dominance: Hybrid Lapse-and-Maintenance Structure
 *   domain: political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint instantiates the HYBRID READING of market naturalization:
 *   a market structure that combines genuine network-effect coordination with
 *   active maintenance of dominance through exclusionary conduct. The hybrid
 *   reading rejects both pure-lapse (the alternative simply disappeared and
 *   needs no defense) and pure-beneficiary-maintenance (the entire dominance
 *   is actively defended) framings. Instead, it holds that: (1) coordination
 *   value is real — users benefit from network effects and switching to
 *   fragmented alternatives is genuinely costly; (2) some switching costs are
 *   structural (data migration, retraining) and require no active incumbent
 *   defense to persist; (3) other barriers are actively maintained through
 *   incumbent conduct (predatory pricing, regulatory capture, compatibility
 *   lock-in) that goes beyond coordination efficiency; (4) the current
 *   extractiveness reflects BOTH components, making disentanglement
 *   empirically difficult but structurally important. The claim/metric
 *   independence rule applies: claimed_type is tangled_rope (genuine
 *   coordination PLUS asymmetric extraction requiring enforcement), while the
 *   metrics describe that mixed structure honestly — neither inflated nor
 *   deflated to match the claim. The measurement series shows extractiveness
 *   and suppression rising through t=20 then stabilizing, signaling a shift
 *   from growing active defense (ramping suppression) to maintenance
 *   steady-state (plateauing suppression), with theater_ratio rising and
 *   plateauing similarly — consistent with a constraint that ramped up
 *   anti-competitive enforcement, then settled into theatrical legitimation
 *   once the barriers were entrenched.
 *
 * KEY AGENTS:
 *   - incumbent_firms: agenda_setter + beneficiary (institutional/generational/arbitrage) — designs and enforces both the coordination structure and the exclusionary barriers
 *   - potential_entrants: payer (powerless/biographical/trapped) — bear the extraction directly; lack the network scale to overcome switching costs
 *   - network_effects_beneficiaries: beneficiary (organized/biographical/constrained) — gain genuine coordination value but are locked into the network
 *   - displaced_incumbents: payer (moderate/biographical/constrained) — lost position partly through coordination superiority, partly through active suppression
 *   - lapsed_alternative_providers: excluded (moderate/biographical/trapped) — exit was caused by switching costs; re-entry is blocked by both inertia and active suppression
 *   - consumer_mass: beneficiary + payer (powerless/immediate/constrained) — benefit from network externalities but pay through monopoly pricing
 *   - competition_authorities: observer (institutional/generational/analytical) — investigate the hybrid boundary: how much of dominance is coordination, how much is enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.62).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.58).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Dominance: Hybrid Lapse-and-Maintenance Structure").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, '06735399-7b56-4e14-b52d-ed0e736f0dd1').
narrative_ontology:cs_kernel_codification('06735399-7b56-4e14-b52d-ed0e736f0dd1', distributed).
narrative_ontology:cs_authority_grounding('06735399-7b56-4e14-b52d-ed0e736f0dd1', extraction).
narrative_ontology:cs_reading_relation('06735399-7b56-4e14-b52d-ed0e736f0dd1', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('06735399-7b56-4e14-b52d-ed0e736f0dd1', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('06735399-7b56-4e14-b52d-ed0e736f0dd1', foundational, coordination_mixed_with_active_suppression).
narrative_ontology:cs_axiom_status(coordination_mixed_with_active_suppression, holdable).
narrative_ontology:cs_axiom_grounding('06735399-7b56-4e14-b52d-ed0e736f0dd1', coordination_mixed_with_active_suppression, empirically_contingent).
narrative_ontology:cs_axiom('06735399-7b56-4e14-b52d-ed0e736f0dd1', foundational, switching_costs_partially_inert).
narrative_ontology:cs_axiom_status(switching_costs_partially_inert, holdable).
narrative_ontology:cs_axiom_grounding('06735399-7b56-4e14-b52d-ed0e736f0dd1', switching_costs_partially_inert, empirically_contingent).
narrative_ontology:cs_reference_frame('06735399-7b56-4e14-b52d-ed0e736f0dd1', mixed_coordination_extraction_arrangement).
narrative_ontology:cs_drift_state('06735399-7b56-4e14-b52d-ed0e736f0dd1', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('06735399-7b56-4e14-b52d-ed0e736f0dd1', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_firms).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, network_effects_beneficiaries).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, potential_entrants).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, displaced_incumbents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, consumer_mass).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, consumer_mass).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominant market position inherited through past competitive advantage (network effects, switching costs, lock-in effects) but now defended through active mechanisms: brand enforcement, lobbying, strategic partnerships, technical standards-setting. They collect extraction rents and set the rules governing entry and competition. Part of their position is truly inert (switching costs that persist without maintenance); part requires continuous active defense (regulatory capture, exclusionary conduct).
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_firms, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, incumbent_firms, beneficiary).

% Face barriers to entry that combine natural network-effect attrition (alternatives have lapsed because users remain on the incumbent's platform) with active suppression (licensing rules, compatibility barriers, predatory pricing specifically triggered by competitive threat). Their exit is not an option because the lapsed alternatives leave them nowhere to go; active suppression ensures entry attempts fail even when alternatives are theoretically available.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, potential_entrants, payer,
    powerless, biographical, trapped, regional).

% Users and complementary service providers (app developers, advertisers) who benefit from the concentration: single unified platform, standardized interfaces, critical mass for network effects to operate. They face constrained exit (the network is valuable but they cannot easily switch), but they derive genuine coordination benefit from it.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, network_effects_beneficiaries, beneficiary,
    organized, biographical, constrained, global).

% Previously dominant firms that lost market position because the coordination benefits of the winner's network effects proved decisive, but who remain partially active in niches. They pay through lost market share and market access restrictions; they are constrained to adjacent segments where the dominant firm has chosen not to enforce exclusion.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, displaced_incumbents, payer,
    moderate, biographical, constrained, regional).

% Benefit from network externalities (the market works, products are standardized) but face constrained prices and limited choice. Pay indirectly through higher monopoly prices and foregone innovation that would emerge from competitive alternatives.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, consumer_mass, beneficiary,
    powerless, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, consumer_mass, payer).

% Firms that once offered competing services but exited the market (and do not re-enter) because switching-cost barriers made their value proposition uncompetitive. They are excluded not by active suppression but by inertia: users remain locked to the incumbent because exit costs are real (data portability friction, retraining costs, network-lock dependencies). If they tried to re-enter, they would face both lapse (inertia-based switching cost) and active suppression (incumbent's predatory response).
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, lapsed_alternative_providers, excluded,
    moderate, biographical, trapped, regional).

% Investigate whether market dominance is the natural outcome of superior coordination (rope classification) or extractive monopoly maintained through anti-competitive conduct (snare classification). The hybrid reading argues it is both: genuine network-effect coordination PLUS active maintenance mechanisms that suppress alternatives beyond what coordination efficiency requires.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__hybrid_reading, incumbent_firms).
narrative_ontology:fixing_cost_class(market_naturalization__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the network-externality problem: a unified platform where all users converge delivers higher value than fragmented alternatives. Switching costs and lock-in ensure that coordination stays stable once established, reducing transaction frictions for users and complementary producers.
% TRANSFER_FUNCTION: Moves extraction rents from potential entrants (who are deterred from competing) and from consumers (who pay monopoly prices) to the dominant incumbent and to users who benefit from network effects and standardization. The transfer is split: some flows to incumbents as monopoly profit; some flows to network-effect beneficiaries as coordination surplus.
% ABSENT_VOICES: Lapsed-alternative providers (who once competed but have since exited and do not re-enter because the switching-cost barrier is too high) are structurally excluded. They would argue for lower switching costs and data portability but have no active voice in the market; their exclusion is the inertia itself, not active suppression directed at them. Potential entrants who might compete are excluded by both switching-cost inertia and incumbent suppression, but have no unified voice in current markets.
% DISAPPEARANCE_RATIONALE: The hybrid reading contests the disappearance verdict. If the constraint vanished: (a) the lapsed-inertia component would unwind slowly — switching costs would decay and some users would re-evaluate alternatives as they become cheaper and better, but the coordination benefit of the network would persist as a gravity well; (b) the active-maintenance component (predatory conduct, lobbying barriers) would cease immediately, allowing suppressed entrants to compete openly. The world would not rearrange overnight, but a new equilibrium with partial fragmentation and genuine competition would emerge over years.
% FOUNDING_PROBLEM: Early technology markets generated network-effect coordination problems: users needed a single standard to achieve value; the first mover to solve this (by achieving critical mass and locking users through switching costs) reaped genuine first-mover advantage and earned dominant position.
% FOUNDING_PROBLEM_CORROBORATION: The incumbent attests the founding problem remains live: switching costs and migration friction persist, so the incumbent's role in maintaining coordination is still necessary. Competition authorities and economic analysis from non-incumbent sources attest that the *founding problem* (coordination in immature markets) is largely solved and the *incumbent's dominance* persists through inertia (lapsed barriers requiring no active defense) PLUS active suppression (conduct that goes beyond defending coordination). The corroboration split reflects the reading's hybrid structure: founding problem was real, its solving generated genuine coordination, but the current persistence is mixed.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, contested).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.62 at plateau) because the constraint genuinely delivers coordination value, but also extracts rents from trapped potential entrants and from consumers paying monopoly prices. Suppression is substantial (0.58) because maintaining dominance against re-entry attempts and against lapsed alternatives requires active enforcement: predatory conduct, standards manipulation, interoperability barriers. Theater_ratio is elevated (0.41) because the incumbent uses legitimation narratives about network effects and coordination to justify conduct that is extractive beyond coordination necessity. The measurement series shows two phases: (t=0 to t=20) ramp-up of suppression and theater as the incumbent consolidates dominance and builds barriers; (t=20 to t=40) stabilization as those barriers become entrenched and require maintenance rather than growth. This pattern is consistent with a constraint that started as genuine coordination, ramped up active defense as threats emerged, then transitioned to maintenance theater once the barriers were locked in. Accessibility_collapse (0.68) reflects that lapsed alternatives have faded from user consciousness and re-entry is now blocked by both inertia and incumbent conduct; alternatives persist in principle but feel inaccessible to most users.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is profound: the incumbent and network-effect beneficiaries experience this constraint as coordination necessity (rope classification from their seats) while potential entrants experience it as pure extraction (snare classification from their seats). The hybrid reading does NOT split the difference — it asserts the constraint IS both simultaneously (tangled_rope: coordination + extraction requiring enforcement). The gap is not in perception but in structural position. A potential entrant and the incumbent have genuinely different relationships to the same constraint; the engine computes their per-seat types from the structural data, revealing this asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The hybrid reading generates DIFFERENT directionality for different stakeholder seats: (1) For the incumbent: d ≈ 0.1 (beneficiary, controls exit, holds institutional power) — the constraint subsidizes them and they set its terms; (2) For potential entrants and displaced incumbents: d ≈ 0.9 (target, trapped exit, powerless/moderate position) — the constraint extracts from them and they cannot reshape its terms; (3) For network-effect beneficiaries: d ≈ 0.5 (symmetric) — they benefit from coordination but pay through consumer prices and locked-in choice set; (4) For consumer mass: d ≈ 0.7 (partial target) — they benefit from network externalities but pay monopoly prices and are constrained in exit options. The engine derives these differences from the declared beneficiary/victim split and the power/exit atoms — this is not an override situation, but the seat divergence is substantial and is what the hybrid reading's structural claim produces.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading addresses mandatrophy by identifying where the founding problem (network-effect coordination in immature markets) has been solved but the incumbent's dominance persists. The mandatrophy question: 'Is the current incumbent role justified by ongoing coordination necessity?' The hybrid answer: 'Partly. Coordination value remains real and switching costs are genuine, but active suppression goes beyond what coordination efficiency requires.' This prevents false certification as either pure rope (ignoring the active suppression) or pure snare (ignoring the genuine coordination value). The measurement series showing rising theater_ratio is a mandatrophy signal: as the founding problem recedes (t=0 to t=20), the incumbent invests in legitimation theater rather than in improving the coordination service itself, suggesting the constraint has outlived its founding necessity and is now maintained by power rather than value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapse_vs_suppression_boundary,
    'What proportion of the incumbent''s market dominance is maintained by structural switching costs (lapsed alternatives, inertia, network-lock persistence) versus active incumbent suppression (predatory conduct, lobbying, technical barriers deliberately erected)?',
    'Regulatory sandbox experiments: open entry to potential competitors in controlled market segments and measure (a) rate of re-entry and competitive success when incumbent predatory conduct is legally prohibited, (b) rate of switching by users when lapsed alternatives are revived with modern features and (c) actual switching costs borne by users when they attempt exit. The ratio of observed switching to legal barriers measures the lapse component; the ratio of prevented entry to incumbent conduct measures the suppression component.',
    'High lapse / low suppression → reclassify as piton (inertial, theater-maintained). High suppression / low lapse → reclassify as snare (pure extraction). Current mixed reading stands only with both components significant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lapse_vs_suppression_boundary, empirical, 'The structural boundary between natural switching-cost inertia and active incumbent defense.').

omega_variable(
    coordination_efficiency_vs_extraction_efficiency,
    'At what point does the incumbent''s dominance extraction (monopoly pricing, reduced innovation, constrained choice) exceed the efficiency gains from network coordination?',
    'Comparative institutional analysis: measure consumer surplus and producer surplus changes if the market re-fragmented into competing platforms with lower network effects versus current consolidated structure; measure innovation rates (R&D spending, feature velocity, quality improvement) under monopoly versus competitive historical periods.',
    'If extraction exceeds coordination efficiency gains → snare classification; if coordination gains substantially exceed extraction → rope classification; if roughly balanced → tangled_rope stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_efficiency_vs_extraction_efficiency, empirical, 'Whether the incumbent''s rent extraction is justified by coordination surplus created.').

omega_variable(
    reading_scope_dependency,
    'Does this hybrid reading apply uniformly across market domains (software, finance, transportation, social media) or is the lapse-vs-suppression boundary domain-specific?',
    'Cross-domain comparative analysis: for each major market (OS platforms, payment networks, social networks, ride-sharing, e-commerce), measure (a) magnitude of switching costs (data portability friction, retraining, network lock), (b) magnitude of active incumbent suppression (anti-competitive conduct documented in regulatory findings), (c) rate of new-entrant competition and success.',
    'If boundary varies substantially by domain, decompose into separate constraint stories per domain (ε-invariance principle). If boundary is uniform, the hybrid reading''s scope is universal within technology markets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_scope_dependency, empirical, 'Whether the hybrid lapse-suppression structure generalizes across market domains or requires per-domain instantiation.').

omega_variable(
    kernel_reading_contest,
    'Which reading of the market_naturalization kernel is structurally accurate: pure lapse (alternatives died, no defense needed), pure beneficiary-maintenance (dominance is actively defended by incumbent capital), or hybrid (mixed lapse and active maintenance)?',
    'The three readings (beneficiary_maintained_reading, lapsed_alternative_reading, hybrid_reading) produce different ε values and different suppression profiles. Empirical resolution: measure the cost to the incumbent of defending dominance (lobbying spend, anti-competitive enforcement intensity, R&D diverted to lock-in versus product quality). High defense cost suggests beneficiary-maintenance reading. Low cost suggests lapse reading. Medium cost with domain-dependent variation suggests hybrid reading (this constraint).',
    'Terminal reading selection: the engine computes each reading as a separate constraint and the corpus''s aggregate classification determines which reading is structurally dominant in the market_naturalization kernel. No single reading is ''correct'' — the kernel is contested and all three readings remain live positions held by different parties (incumbent, competition authorities, lapsed-alternative advocates). The hybrid reading claims the mixed structure is most accurate empirically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The contested kernel''s reading selection problem: which framing of market dominance is most empirically sound.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(mark_tr_t5, market_naturalization__hybrid_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(mark_tr_t10, market_naturalization__hybrid_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(mark_tr_t15, market_naturalization__hybrid_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(mark_tr_t20, market_naturalization__hybrid_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(mark_tr_t25, market_naturalization__hybrid_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement(mark_tr_t30, market_naturalization__hybrid_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement(mark_tr_t35, market_naturalization__hybrid_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__hybrid_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mark_be_t5, market_naturalization__hybrid_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mark_be_t10, market_naturalization__hybrid_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(mark_be_t15, market_naturalization__hybrid_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(mark_be_t20, market_naturalization__hybrid_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(mark_be_t25, market_naturalization__hybrid_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(mark_be_t30, market_naturalization__hybrid_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(mark_be_t35, market_naturalization__hybrid_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement(mark_be_t40, market_naturalization__hybrid_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(mark_su_t5, market_naturalization__hybrid_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(mark_su_t10, market_naturalization__hybrid_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(mark_su_t15, market_naturalization__hybrid_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(mark_su_t20, market_naturalization__hybrid_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(mark_su_t25, market_naturalization__hybrid_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement(mark_su_t30, market_naturalization__hybrid_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(mark_su_t35, market_naturalization__hybrid_reading, suppression_requirement, 35, 0.59).
narrative_ontology:measurement(mark_su_t40, market_naturalization__hybrid_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__hybrid_reading, 0.18).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__lapsed_alternative_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested market_naturalization kernel. Sibling readings: beneficiary_maintained_reading (pure active defense by incumbent) and lapsed_alternative_reading (pure inertial lapse). The hybrid_reading (this file) claims both components are significant and structurally important. Each reading instantiates a different constraint with different ε, different suppression profiles, and different stakeholder structures. The three readings are linked via network.affects_constraints: they compete as alternative framings of the same underlying institutional dynamic. The corpus's aggregate classification of all three readings determines which framing is empirically dominant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_naturalization__hybrid_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
