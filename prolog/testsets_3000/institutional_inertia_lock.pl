% ============================================================================
% CONSTRAINT STORY: institutional_inertia_lock
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_inertia_lock, []).

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
 *   constraint_id: institutional_inertia_lock
 *   human_readable: The Sunk-Cost Regulatory Trap
 *   domain: political/technological
 *
 * SUMMARY:
 *   The sunk-cost regulatory trap emerges when a legacy regulatory framework
 *   persists not because it prevents market failure but because the
 *   organizational cost of reform exceeds the friction of accepting current
 *   inefficiency. The constraint operates between incumbent regulated firms
 *   (beneficiaries), new market entrants (victims), consumers (mixed), and
 *   the regulatory agency itself (constrained enforcer). The framework
 *   exhibits increasing theater over time — compliance rituals become
 *   decoupled from functional verification as regulatory complexity
 *   accumulates without substantive updating. The core tension: the
 *   regulatory framework was designed for the incumbent firms and market
 *   conditions of 1985 but remains in force in 2026, creating asymmetric
 *   compliance costs that serve as a barrier to entry rather than a mechanism
 *   for preventing harm. Reform would require legislative action, political
 *   capital, and incumbent industry resistance — costs that often exceed the
 *   present-value efficiency gains from enabling competition.
 *
 * KEY AGENTS:
 *   - Incumbent Regulated Firms: Primary beneficiary (institutional/arbitrage) — use sunk regulatory framework as competitive moat; benefit from entry barriers; have arbitrage options (lobbying, relocation, vertical integration)
 *   - Market Entrants: Primary victim (powerless/trapped) — face compliance costs designed for incumbent business models; cannot exit without abandoning market; bear disproportionate burden relative to incumbents
 *   - Consumer Aggregate: Secondary victim/beneficiary (moderate/constrained) — constrained not trapped; benefit from service continuity but pay efficiency premium; lack organized power to demand reform
 *   - Regulatory Agency: Institutional enforcer (organized/constrained) — bound by statute and procedural inertia; benefits from regulatory stability; cannot easily reform without legislative override; enforces framework even as its functional purpose decays
 *   - Legislative Authority: Structural actor (institutional/arbitrage) — has technical capacity to reform regulations but faces incumbent lobbying and diffuse consumer opposition; chooses status quo
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional sunk costs as immutable features of governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_inertia_lock, 0.52).
domain_priors:suppression_score(institutional_inertia_lock, 0.68).
domain_priors:theater_ratio(institutional_inertia_lock, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_inertia_lock, extractiveness, 0.52).
narrative_ontology:constraint_metric(institutional_inertia_lock, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(institutional_inertia_lock, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_inertia_lock, tangled_rope).
narrative_ontology:human_readable(institutional_inertia_lock, "The Sunk-Cost Regulatory Trap").
narrative_ontology:topic_domain(institutional_inertia_lock, "political/technological").

domain_priors:requires_active_enforcement(institutional_inertia_lock).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_inertia_lock, incumbent_regulated_industries).
narrative_ontology:constraint_beneficiary(institutional_inertia_lock, regulatory_bureaucracy).
narrative_ontology:constraint_victim(institutional_inertia_lock, market_entrants).
narrative_ontology:constraint_victim(institutional_inertia_lock, consumer_welfare).
narrative_ontology:constraint_victim(institutional_inertia_lock, regulatory_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARKET ENTRANT (SNARE) — New competitors face compliance costs designed for incumbent firms; legacy regulatory framework creates no exit path without abandoning market entry entirely. Maximum extraction: the regulatory burden is not designed to prevent harm but to preserve incumbent advantage. Cannot organize escape; bears full cost of sunk framework.
constraint_indexing:constraint_classification(institutional_inertia_lock, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSUMER AGGREGATE (TANGLED ROPE) — Constrained exit: consumers benefit from established service continuity but pay higher prices and endure slower innovation due to incumbent lock-in. Mixed extraction and coordination: the framework prevents catastrophic service disruption (coordination benefit) but also prevents efficiency gains (extraction cost). Lacks organized power to change the constraint.
constraint_indexing:constraint_classification(institutional_inertia_lock, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT REGULATED FIRM (ROPE) — Experiences the constraint as coordination: the regulatory framework prevents disruptive competition and stabilizes market share. High arbitrage capacity — can exit by relocating, lobbying for favorable amendments, or absorbing competitors. Net beneficiary. The regulatory cost is lower for incumbents who designed compliance processes around existing rules.
constraint_indexing:constraint_classification(institutional_inertia_lock, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AGENCY (TANGLED ROPE) — Organized institutional actor bound by statutes and procedural inertia. Benefits from the framework (political stability, well-defined jurisdiction, bureaucratic stability) and bears costs (updating regulations is politically costly, litigation risk, resource-intensive reform). Constrained exit: cannot easily abolish or radically restructure regulations without legislative action. Active enforcement required to maintain framework credibility.
constraint_indexing:constraint_classification(institutional_inertia_lock, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY RITUAL (PITON) — From a civilizational timescale, compliance filings, environmental impact assessments, and technical standards remain formally mandated but often performative. Original function (prevent market failure, protect public welfare) has decayed — compliance theaters obscure rather than reveal risk. Maintained through institutional inertia, not because it accomplishes stated goals. High theater ratio reflects gap between regulatory appearance and functional verification.
constraint_indexing:constraint_classification(institutional_inertia_lock, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal civilizational view, institutional inertia appears to be a structural law of complex organizations: once bureaucratic pathways are established, their organizational gravity makes change proportionally more difficult. However, this perspective risks naturalizing a contingent economic problem (sunk costs, incumbent advantage, asymmetric compliance burden) as an immutable feature of governance. The engine's false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(institutional_inertia_lock, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_inertia_lock_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_inertia_lock, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_inertia_lock, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_inertia_lock, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_inertia_lock, TR),
    TR >= 0.70.

:- end_tests(institutional_inertia_lock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The incumbent firms extract competitive advantage through an entry barrier that is regulatory rather than economic — new entrants must bear compliance costs that incumbents have long since amortized. However, the extraction is not maximal (0.66+) because some of the regulatory burden reflects genuine safety requirements, and consumers do retain some competitive options (other industries, substitutes). The measure reflects the net asymmetry: new entrants bear disproportionate cost relative to incumbents, but the framework is not purely predatory. Suppression (0.68): High. Significant barriers to escape include: (1) statutory requirements preventing regulatory dissolution, (2) incumbent industry lobbying against deregulation, (3) legislative gridlock making reform politically costly, (4) consumer ambivalence about change (fear of disruption outweighs efficiency gains for many voters). Agents cannot easily walk away. Theater ratio (0.64): Moderate-high and rising. Compliance rituals (environmental impact assessments, technical certifications, filing requirements) were designed to prevent harm but increasingly serve as performance indicators disconnected from actual risk. The theater has increased as regulatory complexity accumulated without substantive reform — new compliance layers were added on top of old ones, creating bureaucratic depth without functional updating.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how structural position determines classification even with fixed base properties. The market entrant sees a snare: high costs, no exit, pure extraction. The incumbent sees a rope: the framework stabilizes competition and they benefit from coordination benefits (predictable market, no disruption risk). The consumer sees tangled rope: mixed benefits (service continuity) and costs (higher prices, slower innovation) with constrained exit. The regulatory agency sees tangled rope from a different angle: benefits from administrative clarity, costs from political friction around reform. The piton perspective (regulatory ritual as degraded) reflects that compliance has become performative rather than functional. The mountain perspective risks naturalizing what is actually a contingent institutional problem — the 'immutable law' framing obscures the political choices that created and maintain the trap.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the structural flow of benefits and costs. Incumbents experience low d (beneficiary + arbitrage capacity) → negative effective extraction, making them see rope. Market entrants experience high d (victim + trapped status) → high f(d) → high experienced extraction, making them see snare. The regulatory agency experiences high organizational d (enforcer bound by statute + constrained exit, but with some institutional benefits) → moderate d → tangled rope classification. The consumer's directionality is mixed (partly beneficiary through service continuity, partly victim through price premium and innovation lag) with constrained rather than trapped exit → moderate d → tangled rope. The piton classification derives from theater ratio (0.64) exceeding 0.50, indicating performative maintenance rather than functional operation. The mountain classification at the analytical level is a false summit — the engine's cascade will flag this as naturalization of institutional sunk cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that incumbents and entrants experience fundamentally different classifications due to their different structural positions, not due to measurement ambiguity. The mandatrophy question ('Is this coordination or extraction?') is answered perspectivally: for incumbents, it is coordination (prevents disruptive competition, stabilizes market). For entrants, it is extraction (enforced competitive disadvantage). For the regulatory agency, it is mixed (both coordination and extraction at different institutional levels). The resolution is NOT to pick one 'true' type but to recognize that the perspectival gap IS the diagnostic feature. The constraint's extractiveness (0.52) reflects the population-level asymmetry between incumbents and entrants. The high suppression (0.68) and rising theater (0.64) indicate that the constraint persists through enforcement and institutional inertia rather than through genuine coordination benefits. The scaffold perspective is notably absent — there is no organized agent perceiving a sunset clause or path to modernization. This absence itself is diagnostic: unlike other tangled ropes that generate reform coalitions, the sunk-cost trap persists partly because no coalition perceives it as temporary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incumbent_compliance_cost_asymmetry,
    'How much of the regulatory compliance burden is intrinsic to preventing market failure versus intrinsic to preserving incumbent market position?',
    'Comparative regulatory cost analysis: cost for incumbent firm to maintain compliance versus actual cost for new entrant; decomposition of regulatory rules into safety-critical vs incumbent-protective vs theater-only categories',
    'If asymmetry is structural (safety rules are cheaper for incumbents): constraint is mostly tangled_rope coordination. If asymmetry is intentional (regulatory design favors incumbents): constraint is extractive snare from entrant perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_compliance_cost_asymmetry, empirical, 'Degree to which compliance cost asymmetry is intentional versus structural').

omega_variable(
    regulatory_reform_political_cost,
    'What is the actual political cost of updating the regulatory framework, and does it exceed the economic efficiency gain from allowing new entrants?',
    'Historical case studies of successful regulatory modernization (e.g., telecommunications, aviation); cost-benefit analysis comparing reform political capital versus incumbent lobbying resistance versus consumer welfare gains',
    'If reform cost < efficiency gain: constraint is avoidable tangled_rope with possible scaffold sunset. If reform cost > efficiency gain: constraint is persistent structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_reform_political_cost, empirical, 'Political economy of regulatory modernization').

omega_variable(
    consumer_exit_capacity,
    'Do consumers have genuine mobile alternatives (other industries, geographic relocation, service substitution) or are they actually trapped within the regulated sector?',
    'Market structure analysis: substitute goods availability, geographic scope of regulation, switching cost for consumers, availability of unregulated alternatives',
    'If alternatives exist: consumer exit is constrained not trapped; classification shifts from tangled_rope toward rope. If alternatives unavailable: consumers are trapped; snare classification becomes more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consumer_exit_capacity, empirical, 'Whether consumer exit options are truly constrained or effectively trapped').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_inertia_lock, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iil_tr_t0, institutional_inertia_lock, theater_ratio, 0, 0.35).
narrative_ontology:measurement(iil_tr_t5, institutional_inertia_lock, theater_ratio, 5, 0.48).
narrative_ontology:measurement(iil_tr_t10, institutional_inertia_lock, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(iil_be_t0, institutional_inertia_lock, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(iil_be_t5, institutional_inertia_lock, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(iil_be_t10, institutional_inertia_lock, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_inertia_lock, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_inertia_lock, incumbent_industry_moat).
narrative_ontology:affects_constraint(institutional_inertia_lock, regulatory_capture_dynamics).
narrative_ontology:affects_constraint(institutional_inertia_lock, consumer_choice_illusion).

% DUAL FORMULATION NOTE:
% The sunk-cost regulatory trap is downstream of specific incumbent/entrant dynamics but represents a distinct structural constraint on regulatory modernization. Related constraints (incumbent moats, regulatory capture) have their own extractiveness values reflecting specific industry features; this constraint's extractiveness reflects the generic institutional inertia mechanism that operates across regulatory domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_inertia_lock, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
