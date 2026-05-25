% ============================================================================
% CONSTRAINT STORY: statutory_obsolescence_lag
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_obsolescence_lag, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: statutory_obsolescence_lag
 *   human_readable: Statutory Obsolescence Lag: Extraction Through Delayed Legal Modernization
 *   domain: legal/regulatory/institutional
 *
 * SUMMARY:
 *   Statutory obsolescence lag is a structural constraint where legal
 *   frameworks designed for predecessor technologies persist long after the
 *   technological and market conditions they addressed have been superseded.
 *   The constraint creates asymmetric extraction: incumbent industries that
 *   shaped the original statute benefit from competitors being locked out by
 *   anachronistic rules, while emerging industries face either illegal
 *   operation or high compliance costs under irrelevant regulations. The core
 *   tension is between the genuine coordination benefit of legal
 *   predictability (established industries can invest confidently) and the
 *   extraction mechanism of regulatory lock-in (new entrants cannot operate
 *   under modernized terms). This constraint exhibits different
 *   classification types from different structural positions: powerless
 *   emerging industries trapped by old rules see pure extraction (Snare);
 *   incumbents see coordination with embedded extraction (Tangled Rope);
 *   organized innovation coalitions see a temporary problem being solved
 *   through sandboxes and reform (Scaffold); legislative institutions see
 *   degraded ritual (Piton); and civilizational observers risk naturalizing
 *   the lag as inherent to law (false Mountain). The theater_ratio has risen
 *   from 0.35 to 0.68 over 15 years, indicating that regulatory theater
 *   (sandboxes, exemptions, interpretive guidance) has proliferated while the
 *   underlying statute remains unchanged — a diagnostic signature of Piton
 *   dynamics.
 *
 * KEY AGENTS:
 *   - Emerging Industries: Primary victim (powerless/trapped) — blockchain, rideshare, drone delivery, alternative energy technologies locked out by statutes written for predecessor technologies (banking, taxi medallions, aviation, fossil fuels)
 *   - Incumbent Industries: Primary beneficiary (powerful/mobile) — traditional banking, licensed taxi companies, aviation incumbents, fossil fuel companies benefiting from regulatory moat created by obsolete statutes
 *   - Regulatory Agencies: Institutional beneficiary (institutional/arbitrage) — maintain enforcement monopoly and institutional relevance through statutes; can propose revision but have incentive to preserve status quo
 *   - Legislative Bodies: Institutional actor (institutional/arbitrage) — can revise statutes but face political-economy barriers (incumbent lobbying, low salience to voters); enforce revision through inertia
 *   - Legal Specialists: Institutional beneficiary (institutional/arbitrage) — accumulate expertise and billable hours interpreting and litigating around anachronistic statutes
 *   - Innovation Coalition: Organized victim (organized/constrained) — startup associations, tech advocates, consumer groups advocating for modernization; constrained by political barriers but have agency through legislative action and jurisdictional arbitrage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_obsolescence_lag, 0.52).
domain_priors:suppression_score(statutory_obsolescence_lag, 0.58).
domain_priors:theater_ratio(statutory_obsolescence_lag, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_obsolescence_lag, extractiveness, 0.52).
narrative_ontology:constraint_metric(statutory_obsolescence_lag, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(statutory_obsolescence_lag, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_obsolescence_lag, tangled_rope).
narrative_ontology:human_readable(statutory_obsolescence_lag, "Statutory Obsolescence Lag: Extraction Through Delayed Legal Modernization").
narrative_ontology:topic_domain(statutory_obsolescence_lag, "legal/regulatory/institutional").

domain_priors:requires_active_enforcement(statutory_obsolescence_lag).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_obsolescence_lag, incumbent_industries).
narrative_ontology:constraint_beneficiary(statutory_obsolescence_lag, regulatory_agencies_with_enforcement_monopoly).
narrative_ontology:constraint_beneficiary(statutory_obsolescence_lag, legal_specialists).
narrative_ontology:constraint_victim(statutory_obsolescence_lag, emerging_industries).
narrative_ontology:constraint_victim(statutory_obsolescence_lag, consumers_under_anachronistic_rules).
narrative_ontology:constraint_victim(statutory_obsolescence_lag, regulatory_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING INDUSTRY (SNARE) — Trapped by statutes written for predecessor technologies. Cannot legally operate under modernized conditions; cannot exit without abandoning market entry. Suppression is total: the old statute is the only legal framework available. Experiences full extraction — regulatory rent flows to incumbents who shaped the original law.
constraint_indexing:constraint_classification(statutory_obsolescence_lag, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INCUMBENT INDUSTRY (TANGLED ROPE) — Benefits from statutory lock-in (competitors blocked) AND from genuine coordination function (predictable legal framework enables investment). Mobile (can lobby for revision), but does not exercise mobility because extraction is more profitable than modernization. Active enforcement through regulatory gatekeeping.
constraint_indexing:constraint_classification(statutory_obsolescence_lag, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY AGENCY (ROPE) — Experiences statute as coordination mechanism: clear rules enable predictable enforcement. Has arbitrage (can propose legislative revision), but institutional momentum makes revision costly. Net beneficiary of status quo (institutional budget justified by enforcement).
constraint_indexing:constraint_classification(statutory_obsolescence_lag, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INNOVATION COALITION (SCAFFOLD) — Organized agents (startup associations, tech lobbies, consumer advocates) perceive statutory lag as temporary coordination failure with exit path. Legislative modernization (sandbox regulations, tech-neutral statutory language, adaptive governance) represents a sunset clause. Suppression is high (must navigate political economy), but coalition sees pathway out. Theater is present but declining as modernization pressure builds.
constraint_indexing:constraint_classification(statutory_obsolescence_lag, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGISLATIVE SYSTEM (PITON) — Statutory modernization requires deliberate action; obsolete statutes persist through inertia. The legislative process is largely performative: periodic 'modernization' gestures occur (hearings, working groups, pilot programs) without fundamental revision of underlying statute. Theater ratio reflects this: extensive regulatory theater (sandboxes, exemptions, interpretive guidance) that maintains the pretense of modernization while preserving the old statute's extraction function. Theater has increased over time as gap between law and reality has widened.
constraint_indexing:constraint_classification(statutory_obsolescence_lag, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, statutory lag appears to be an inherent feature of legal systems: law always lags technology, and this gap is unchangeable by design. Legislative processes move slower than innovation. This perspective risks naturalizing a contingent institutional arrangement. However, structural data contradicts the mountain classification — deliberate choices (lobbying, capture, inertia tolerance) sustain the lag. Engine will identify this as a false summit.
constraint_indexing:constraint_classification(statutory_obsolescence_lag, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_obsolescence_lag_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(statutory_obsolescence_lag, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(statutory_obsolescence_lag, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(statutory_obsolescence_lag, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(statutory_obsolescence_lag, TR),
    TR >= 0.70.

:- end_tests(statutory_obsolescence_lag_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The originating statute creates a legal moat (competitors cannot legally operate under modern terms) that extracts value from entrants. But extraction is not total because workarounds (sandboxes, exemptions) create partial alternatives, reducing the pure blocking effect. The base value of 0.38 reflected early-stage technology (moderately novel); current value of 0.52 reflects maturation of the technology and hardening of incumbency — as the technology becomes obviously viable, the statute's lock-in function becomes more clearly extractive. Suppression (0.58): Moderate-high. Barriers to statutory revision are significant: incumbent lobbying, legislative inertia, network effects that lock consumers to regulated incumbents. But suppression is not total — jurisdictional arbitrage exists (move to progressive jurisdiction), workarounds exist (operate in legal gray zone or under exemption), and political coalition pressure does occasionally produce revision. Theater (0.64): Moderate-high and rising. The constraint exhibits significant theatrical governance: regulatory sandboxes create the appearance of modernization while the old statute remains; interpretive guidance updates the regulation without statutory change; exemptions for specific startups create exemption theater while keeping the statute intact. Theater has risen from 0.35 to 0.64 because as the gap between statute and reality widened, regulatory theater proliferated as a substitute for actual revision.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full range from Snare (emerging industry) to Rope (regulator) perspectives. The emerging industry sees pure obstruction with no coordination benefit; the incumbent sees legitimate coordination (predictable legal framework) with embedded benefit (protection from competition); the regulator sees routine enforcement coordination; the innovation coalition sees a temporary problem with legislative exit path; the legislature sees procedural theater masking inertia; the civilizational observer risks seeing natural law. The perspectival gap is extreme: the same legal statute is simultaneously (1) a trap for new entrants, (2) a coordination mechanism for incumbents, (3) a justification for regulatory budgets, (4) a problem to be solved through modernization, (5) a degraded ritual, and (6) an immutable feature of legal systems. The gaps reveal that statutory obsolescence lag is fundamentally asymmetric in its effects — it coordinates for some actors while extracting from others.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerless emerging industries facing trapped exit experience maximum directionality toward extraction (d≈0.95). They cannot legally operate and cannot exit without abandoning market entry. Powerful incumbents with mobile exit experience low extraction (d≈0.20) — they benefit from the statute and maintain mobility for lobbying purposes. Organized coalitions with constrained exit experience moderate extraction (d≈0.60) — they have agency through legislative action but face structural barriers. Institutional agents with arbitrage experience low extraction (d≈0.05-0.10) — regulators and legislators have the power to change the system and use their arbitrage to maintain beneficial status quo. The gap between powerless/trapped (high d) and institutional/arbitrage (low d) reveals the extraction asymmetry: the constraint actively extracts from those with no options while neutralizing those with alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through perspectival indexicality. From the incumbent's perspective (institutional/arbitrage), the statute is genuine coordination — it enables long-term investment in infrastructure, creates predictable legal liability, and coordinates behavior across the industry. This is real coordination value. From the emerging industry's perspective (powerless/trapped), the same statute is pure extraction — it prevents legal entry and forces actors into illegal operation or high-cost workarounds. These are not contradictory analyses; they are perspectival truths from different structural positions. The Tangled Rope classification from the institutional perspective confirms that coordination and extraction coexist in this constraint — it genuinely solves a coordination problem (legal predictability) while asymmetrically extracting from new entrants. The false Mountain (civilization perspective) is revealed by structural data: the lag is not an inherent feature of legal systems but a contingent artifact of political-economy choices. Jurisdictions with different amendment procedures show different lag times, confirming that lag is chosen, not immutable. Theater ratio rising over time indicates that regulatory theater (sandboxes, workarounds) has increasingly substituted for actual revision — a signature of constraint degradation, not fundamental immutability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_vs_systemic_lag,
    'Is statutory obsolescence lag primarily caused by incumbent capture and deliberate obstruction, or by genuine systemic limitations in legislative responsiveness?',
    'Comparative analysis across jurisdictions with different political-economy structures; examination of amendment frequency correlations with incumbent lobbying intensity vs. legislative capacity measures',
    'If primarily capture: classification remains Tangled Rope (active extraction). If primarily systemic: classification may shift toward Scaffold (temporary coordination failure) across more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_vs_systemic_lag, empirical, 'Intentionality and capture vs. systemic legislative lag').

omega_variable(
    emergence_timing_threshold,
    'What timeline threshold distinguishes legitimate lag in statutory modernization from extractive obstruction?',
    'Historical comparison of lag times for statutory revision in mature vs emerging technologies; correlation analysis of lag duration with incumbent market concentration',
    'If lag < 5 years: acceptable coordination friction. If lag > 10 years after technology maturation: extractive obstruction confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergence_timing_threshold, empirical, 'Lag duration threshold for extraction vs. coordination').

omega_variable(
    alternative_legal_framework_viability,
    'Do workarounds (regulatory sandboxes, exemptions, interpretive guidance) genuinely create alternative pathways for innovation, or do they maintain extraction while creating theatrical appearances of modernization?',
    'Empirical tracking of company growth and adoption rates within sandboxes vs. outside obsolete statutes; examination of whether sandbox graduates face statutory barriers upon graduation; cost accounting of compliance for innovations operating under workarounds vs. clean statutory authorization',
    'If workarounds are genuine alternatives: constraint is Scaffold (sunset via layered workarounds). If workarounds maintain old extraction while looking modern: constraint is Piton (theatrical modernization masking persistent lag).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_legal_framework_viability, empirical, 'Whether regulatory workarounds enable genuine innovation or maintain theatrical governance').

omega_variable(
    jurisdictional_arbitrage_escape_valve,
    'Does the availability of more modernized jurisdictions (other states, countries, data havens) provide meaningful exit for trapped industries, or is jurisdictional arbitrage blocked by network effects, consumer location, or regulatory retaliation?',
    'Mapping of actual jurisdiction-switching vs. fictional arbitrage; analysis of network effects (is relocating to progressive jurisdiction viable if key market is in restrictive jurisdiction?); correlation of jurisdictional competition with lag reduction rates',
    'If arbitrage is real: trapped exit option upgrades from trapped to constrained; classifications shift downward in extraction. If arbitrage is blocked: trapped remains accurate; extraction remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_arbitrage_escape_valve, empirical, 'Jurisdictional arbitrage as escape valve or illusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_obsolescence_lag, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stol_tr_t0, statutory_obsolescence_lag, theater_ratio, 0, 0.35).
narrative_ontology:measurement(stol_tr_t5, statutory_obsolescence_lag, theater_ratio, 5, 0.5).
narrative_ontology:measurement(stol_tr_t10, statutory_obsolescence_lag, theater_ratio, 10, 0.64).
narrative_ontology:measurement(stol_tr_t15, statutory_obsolescence_lag, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(stol_be_t0, statutory_obsolescence_lag, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(stol_be_t5, statutory_obsolescence_lag, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(stol_be_t10, statutory_obsolescence_lag, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(stol_be_t15, statutory_obsolescence_lag, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_obsolescence_lag, enforcement_mechanism).
narrative_ontology:affects_constraint(statutory_obsolescence_lag, regulatory_capture_dynamic).
narrative_ontology:affects_constraint(statutory_obsolescence_lag, innovation_suppression_via_compliance_cost).
narrative_ontology:affects_constraint(statutory_obsolescence_lag, jurisdictional_arbitrage_fragmentation).

% DUAL FORMULATION NOTE:
% Statutory obsolescence lag can be decomposed into (1) the structural lag itself (inherent legislative responsiveness limits) and (2) the intentional obstruction overlaid on that lag (incumbent capture preventing revision). These are separate constraints with different ε values. The intentional obstruction constraint (ε≈0.52, this story) operates downstream of the baseline lag constraint. A constraint story focused on purely systemic lag (without capture) would have lower extractiveness and might classify as Scaffold rather than Tangled Rope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statutory_obsolescence_lag, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
