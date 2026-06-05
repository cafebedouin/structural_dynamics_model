% ============================================================================
% CONSTRAINT STORY: innovators_dilemma
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_innovators_dilemma, []).

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
 *   constraint_id: innovators_dilemma
 *   human_readable: The Innovator's Dilemma: Incumbent Constraint to Disruptive Entry
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Innovator's Dilemma describes a structural constraint where incumbent
 *   firms pursuing rational financial optimization become organizationally
 *   incapable of responding to disruptive technologies. The constraint
 *   operates at the intersection of shareholder capital discipline (a
 *   coordination mechanism that prevents wasteful low-margin investment) and
 *   market segmentation (which creates trapped populations in underserved,
 *   low-margin segments). The dilemma is that the same organizational
 *   structures and incentives that maximize incumbent profitability in
 *   existing markets actively suppress the incumbent's ability to pursue
 *   disruptive low-margin opportunities. Unlike simple market competition,
 *   which permits exit through product repositioning or resource
 *   reallocation, the innovator's dilemma creates an asymmetric structural
 *   bind: disruptive entrants face suppressed entry conditions and trapped
 *   market segments, while incumbents face suppressed innovation capacity and
 *   trapped strategic optionality. The constraint accumulates over time as
 *   market segmentation deepens, switching costs rise, and the profitability
 *   hierarchy becomes institutionalized in organizational structure and
 *   management incentives.
 *
 * KEY AGENTS:
 *   - Incumbent Firm Shareholders: Institutional beneficiary (institutional/arbitrage) — benefit from capital discipline and high-margin focus; can reallocate capital if dissatisfied
 *   - Incumbent Firm Management: Institutional beneficiary (institutional/constrained) — face fiduciary duty to prioritize profitable customers; structurally discouraged from low-margin exploration
 *   - Disruptive Entrant: Powerless victim (powerless/trapped) — face distribution barriers, capital constraints, and brand disadvantages; confined to low-margin segments
 *   - Underserved Market Segments: Powerless victim (powerless/trapped) — demand exists but is unserved because margins insufficient for incumbent economics; cannot access high-quality incumbent products at any price
 *   - Mid-Level Incumbent Managers: Moderate victim (moderate/constrained) — recognize opportunity but organizational structure and metrics suppress action; some agency through resource requests but constrained by profitability mandate
 *   - Disruptive Spinoff (if created): Institutional beneficiary (institutional/mobile) — separate P&L removes the profitability hierarchy that suppresses low-margin work; mobile exit available
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(innovators_dilemma, 0.52).
domain_priors:suppression_score(innovators_dilemma, 0.48).
domain_priors:theater_ratio(innovators_dilemma, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(innovators_dilemma, extractiveness, 0.52).
narrative_ontology:constraint_metric(innovators_dilemma, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(innovators_dilemma, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(innovators_dilemma, tangled_rope).
narrative_ontology:human_readable(innovators_dilemma, "The Innovator's Dilemma: Incumbent Constraint to Disruptive Entry").
narrative_ontology:topic_domain(innovators_dilemma, "economic/technological").

domain_priors:requires_active_enforcement(innovators_dilemma).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(innovators_dilemma, incumbent_firm_shareholders).
narrative_ontology:constraint_beneficiary(innovators_dilemma, incumbent_firm_management).
narrative_ontology:constraint_victim(innovators_dilemma, disruptive_entrants).
narrative_ontology:constraint_victim(innovators_dilemma, underserved_market_segments).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISRUPTIVE ENTRANT (SNARE) — New firms entering with low-margin innovations face structural suppression: incumbent distribution networks, customer lock-in, brand preference, and capital barriers block market access. The entrant's only viable path (underserved low-margin segment) offers insufficient revenue to fund scaling. Trapped by economic geography — cannot afford to compete in incumbent-controlled channels.
constraint_indexing:constraint_classification(innovators_dilemma, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: UNDERSERVED MARKET SEGMENTS (SNARE) — Customer populations in low-margin segments remain structurally excluded from incumbent offerings because those offerings are optimized for premium segments. No incumbent incentive to serve them. Trapped by demand profile — cannot afford premium prices; cannot exit to alternative suppliers (none exist at their price point).
constraint_indexing:constraint_classification(innovators_dilemma, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INCUMBENT FIRM MID-LEVEL MANAGERS (TANGLED ROPE) — Managers face a genuine coordination problem (maximizing shareholder value requires responsiveness to profitable customers) alongside extraction pressure (organizational structure and incentives foreclose lower-margin opportunities). Some mobility — can lobby for resource reallocation — but constrained by fiduciary duty and performance metrics. Experience both the logic of the dilemma and its constraints.
constraint_indexing:constraint_classification(innovators_dilemma, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT FIRM SHAREHOLDERS (ROPE) — Primary beneficiary. The constraint coordinates capital allocation toward high-margin segments, maximizing near-term returns. The firm's rational response to shareholder pressure is to dismiss low-margin opportunities (correctly, in pure financial terms). Exit via arbitrage — can reallocate capital if dissatisfied. Experience the constraint as optimal coordination.
constraint_indexing:constraint_classification(innovators_dilemma, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DISRUPTIVE INCUMBENT SPINOFF (ROPE) — When incumbent firms explicitly fund separate low-margin subsidiaries (e.g., IBM's personal computer division as spin-off), the organizational structure changes. Spinoff experiences the constraint as coordination only — separate P&L removes the profitability hierarchy that suppresses low-margin work. Mobile exit available (can be sold or wound down). Pure coordination function.
constraint_indexing:constraint_classification(innovators_dilemma, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational frame, the constraint exhibits both genuine coordination (shareholder capital discipline prevents wasteful low-margin investment) and structural extraction (that same discipline prevents serving legitimate demand from underserved segments). The dilemma is real — not a natural law but a genuine structural bind where no single actor can escape without loss.
constraint_indexing:constraint_classification(innovators_dilemma, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(innovators_dilemma_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(innovators_dilemma, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(innovators_dilemma, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(innovators_dilemma, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(innovators_dilemma_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The incumbent firm extracts value from underserved segments through suppressed competition (they don't serve these segments, preventing entrants from accessing either the segments or the profitable high-margin channels). The entrant faces a form of structural rent extraction — forced to operate at low margins with restricted access. However, extractiveness is not maximal because the incumbent is not actively extracting from the entrant (no predatory pricing, no sabotage) — rather, the incumbent's rational profit-maximization creates conditions that suppress entrant viability. The measurement tracks upward over the interval as the constraint accumulates: initial extractiveness reflects the market-access suppression; later measurements include organizational lock-in as the profitability hierarchy becomes embedded in incentive systems. Suppression (0.48): Moderate. Disruptive entrants face genuine barriers (capital, distribution, brand switching costs) but are not completely blocked — the underserved segment remains accessible and represents a viable (if constrained) entry point. Over time, suppression increases slightly as incumbents fortify distribution networks and customer lock-in deepens. Theater ratio (0.35): Low. The constraint's mechanism is largely functional, not performative. Incumbents genuinely do maximize shareholder value; managers genuinely do respond to profitable customers; the organizational structure genuinely does create the incentive hierarchy that suppresses low-margin work. There is minimal theater — the constraint works as described, with little performative cover story.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reflects the fundamental structural bind at the heart of the dilemma. Shareholders and top management see pure coordination (Rope) — rational capital allocation toward profitable segments maximizing returns. Disruptive entrants and underserved segments see pure extraction (Snare) — trapped in low-margin suppression with no exit. Mid-level managers see the mixed reality (Tangled Rope) — they understand both the coordination logic and its extractive consequences. The spinoff sees the constraint dissolve into pure coordination (Rope) — when organizational structure changes, the bind dissolves. The analytical observer at civilizational scale sees the dilemma as a genuine structural bind (Tangled Rope) — not resolvable by any single actor, not a coordination failure that can be fixed by information or alignment, but a real clash between two legitimate structural forces: capital discipline and innovation responsiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality reflects the agent's structural position in the extraction flow. Shareholders with arbitrage options experience low effective extraction — they benefit from the constraint and can reallocate if dissatisfied. Disruptive entrants trapped in low-margin segments experience maximum extraction — they cannot escape the margin structure or access higher-margin channels. Underserved customers trapped without alternative suppliers experience maximum extraction. Mid-level managers experience moderate extraction because they recognize the opportunity but face organizational constraints that suppress action — they have some agency (can request resources) but face suppression (fiduciary duty, performance metrics, profitability targets). The spinoff perspective (institutional/mobile) experiences the constraint as pure coordination because structural separation removes the profitability hierarchy that suppresses low-margin work — managers can prioritize low-margin innovation without fiduciary conflict. The analytical observer's directionality reflects the balanced structural position: the dilemma is real because both the coordination function (capital discipline) and the suppression mechanism (profitability hierarchy) are structural features that no single actor can escape without loss.
 *
 * MANDATROPHY ANALYSIS:
 *   The innovator's dilemma resolves mandatrophy by distinguishing genuine organizational constraint (Tangled Rope with both coordination and extraction) from simple extraction or coordination. A pure-extraction reading (Snare) would suggest that incumbents deliberately block disruptive innovation — intentional predation. A pure-coordination reading (Rope) would suggest that capital discipline is benign — no extraction occurs. The Tangled Rope classification captures the real structure: both coordination and extraction are structural, both are rational from the perspectives that experience them as coordination, and neither can be eliminated without loss. The constraint is not intentional predation (beneficiaries do not need to conspire — profit maximization produces the outcome naturally) and not benign coordination (genuine harm flows to trapped entrants and underserved segments) but a genuine structural bind where legitimate interests clash. Mandatrophy is resolved by accepting that the dilemma cannot be solved by choosing between coordination and extraction — the structure contains both, and structural change (spinoffs, governance change, market-segment separation) is required to unbind them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    profitability_hierarchy_contingency,
    'Is the incumbent''s inability to pursue low-margin innovation inherent to profit-maximization logic, or contingent on specific capital-structure assumptions?',
    'Comparative analysis of incumbent responses across different ownership structures: widely-held public firms vs. founder-controlled firms vs. cooperative ownership vs. state-owned enterprises. If low-margin innovation rates differ substantially by ownership, the constraint is contingent on capital structure, not inherent to incumbency.',
    'If contingent: constraint is a Tangled Rope that can be restructured via governance change (separate subsidiaries, cooperative models, or stakeholder governance). If inherent: constraint approaches Mountain classification — the dilemma is structural to competitive markets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(profitability_hierarchy_contingency, empirical, 'Whether profitability hierarchy is contingent on capital structure').

omega_variable(
    spinoff_autonomy_sustainability,
    'When incumbents create autonomous low-margin subsidiaries, do they maintain genuine independence, or does parental pressure to contribute to consolidated returns gradually reintroduce the profitability hierarchy?',
    'Longitudinal analysis of spinoff subsidiaries: budget autonomy, performance expectations, and independence trajectory over 5-10 years. Track whether low-margin focus persists or gets reoptimized toward higher margins after parental integration.',
    'If autonomy persists: spinoff model is a structural escape from the dilemma. If autonomy erodes: the constraint is more robust than Christensen suggests — parental pull toward profitability is difficult to resist even with formal separation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spinoff_autonomy_sustainability, empirical, 'Whether spinoff autonomy persists over time').

omega_variable(
    incumbency_vs_market_dynamics,
    'Is the observed failure of incumbents to respond to disruption driven by the organizational constraint described in the dilemma, or by underlying market dynamics (switching costs, network effects, sunk capital) that would constrain ANY actor in the incumbent''s position?',
    'Counterfactual analysis: would a hypothetically rational, unconstrained incumbent with perfect foresight still rationally choose to prioritize high-margin segments? If yes, the constraint is more fundamental than organizational; if no, the organizational constraint is causal.',
    'If fundamental: the dilemma is a structural feature of competition under uncertainty. If organizational: better governance structures (spinoffs, separate incentives) can overcome it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbency_vs_market_dynamics, conceptual, 'Whether constraint is organizational or market-structural').

omega_variable(
    entrant_advantage_sustainability,
    'Does the disruptive entrant''s advantage persist as the technology matures and scales, or does the incumbent''s superior resources eventually enable catch-up and recapture?',
    'Historical pattern analysis across multiple disruption cycles: telecommunications (mobile vs landline), photography (digital vs film), automotive (electric vs combustion). Track whether entrants maintain advantage post-scaling or if incumbents recapture market share.',
    'If entrant advantage persists: the dilemma creates permanent structural disadvantage for incumbents (long-term snare). If incumbents recapture: the dilemma is a temporary window; the constraint weakens once technology proves itself and high-margin routes emerge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(entrant_advantage_sustainability, empirical, 'Whether disruptive advantage persists or erodes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(innovators_dilemma, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(innov_tr_t0, innovators_dilemma, theater_ratio, 0, 0.28).
narrative_ontology:measurement(innov_tr_t5, innovators_dilemma, theater_ratio, 5, 0.32).
narrative_ontology:measurement(innov_tr_t10, innovators_dilemma, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(innov_be_t0, innovators_dilemma, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(innov_be_t5, innovators_dilemma, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(innov_be_t10, innovators_dilemma, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(innov_su_t0, innovators_dilemma, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(innov_su_t5, innovators_dilemma, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(innov_su_t10, innovators_dilemma, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(innovators_dilemma, resource_allocation).
narrative_ontology:affects_constraint(innovators_dilemma, market_segmentation_lock).
narrative_ontology:affects_constraint(innovators_dilemma, customer_demand_hierarchy).

% DUAL FORMULATION NOTE:
% The Innovator's Dilemma can be decomposed into three distinct constraints: (1) capital-allocation discipline (ε ≈ 0.08, Mountain candidate — shareholder capital discipline is rational) vs (2) market-segmentation lock (ε ≈ 0.45, Tangled Rope — incumbent distribution and brand create barriers) vs (3) profitability-hierarchy suppression (ε ≈ 0.52, Tangled Rope — organizational incentives suppress low-margin exploration). This story integrates all three perspectives into one constraint, but decomposition reveals the distinct mechanisms. The unified treatment is appropriate because the dilemma is precisely the interaction of these three elements — each is benign separately, but their combination creates the bind.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(innovators_dilemma, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
