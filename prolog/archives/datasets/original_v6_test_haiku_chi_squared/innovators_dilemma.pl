% ============================================================================
% CONSTRAINT STORY: innovators_dilemma
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: innovators_dilemma
 *   human_readable: The Innovator's Dilemma
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Innovator's Dilemma constrains incumbent firms through a hybrid
 *   mechanism that combines genuine coordination (listening to customers,
 *   optimizing for profitability) with systematic extraction (preventing
 *   disruptive alternatives, creating structural dependency on the
 *   incumbent's product roadmap). The constraint exhibits all perspectives of
 *   the DR classification system. To the disruptive innovator starting in a
 *   low-margin niche, it appears as a snare: capital barriers, incumbent
 *   defensibility, and market segment limitations create a trapped condition
 *   with no legitimate exit route. To the incumbent's frontline engineers, it
 *   appears as a snare: organizational hierarchy and resource allocation
 *   discipline prevent them from pursuing low-margin product development even
 *   when the threat is visible. To incumbent senior management and their
 *   premium customers, it appears as rope: the constraint is experienced as
 *   rational coordination on behalf of well-understood customer needs and
 *   margin protection. At the firm level over a generational timescale, the
 *   constraint becomes tangled rope: the very organizational structures that
 *   optimize for present profitability (active enforcement through
 *   accountability and capital discipline) simultaneously prevent adaptation
 *   and future viability. At the industry level over a civilizational
 *   timescale, it appears as piton: organizational forms and incentive
 *   structures persist through institutional inertia long after their
 *   functional utility has degraded. The analytical observer risks
 *   naturalizing this as a mountain—claiming it is an immutable law of
 *   organizational behavior—but the structural data reveals this as a false
 *   summit: the constraint is contingent on specific economic, informational,
 *   and organizational factors that can be altered.
 *
 * KEY AGENTS:
 *   - Incumbent Senior Management: Primary beneficiary (institutional/arbitrage) — captures short-term margin improvement and shareholder value; experiences constraint as benign coordination with customers
 *   - High-Margin Customer Segment: Secondary beneficiary (institutional/arbitrage) — receives continuous improvement in preferred product dimensions; experiences constraint as responsive service
 *   - Incumbent Firm (Long-term): Primary victim (powerful/mobile) — faces generational vulnerability to disruption; experiences constraint as organizational trap that it actively maintains
 *   - Incumbent Frontline Engineers: Secondary victim (moderate/trapped) — recognize disruptive threats but lack authority and resources to respond; trapped by organizational structure
 *   - Disruptive Innovator: Structural target (powerless/trapped) — constrained to low-margin niche by capital requirements and incumbent defensibility; cannot access high-margin segments via superior product alone
 *   - Potential Market for Disruptive Product: Unidentified victim (powerless/trapped) — latent demand that may be unmet; alternative products and market segments may be suppressed by incumbent focus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(innovators_dilemma, 0.52).
domain_priors:suppression_score(innovators_dilemma, 0.65).
domain_priors:theater_ratio(innovators_dilemma, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(innovators_dilemma, extractiveness, 0.52).
narrative_ontology:constraint_metric(innovators_dilemma, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(innovators_dilemma, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(innovators_dilemma, tangled_rope).
narrative_ontology:human_readable(innovators_dilemma, "The Innovator's Dilemma").
narrative_ontology:topic_domain(innovators_dilemma, "economic/technological").

domain_priors:requires_active_enforcement(innovators_dilemma).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(innovators_dilemma, incumbent_management).
narrative_ontology:constraint_beneficiary(innovators_dilemma, high_margin_customer_segment).
narrative_ontology:constraint_victim(innovators_dilemma, incumbent_firm_long_term_viability).
narrative_ontology:constraint_victim(innovators_dilemma, potential_disruptive_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISRUPTIVE INNOVATOR (SNARE) — Cannot exit the low-margin niche market constraint; trapped by capital requirements and incumbent defensibility. The structural barrier to competing in high-margin segments is insurmountable via legitimate product superiority in the early phases. d≈0.92, f(d)≈1.38, σ=1.1 → χ≈0.74.
constraint_indexing:constraint_classification(innovators_dilemma, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INCUMBENT FRONTLINE ENGINEER (SNARE) — Trapped by organizational structure and resource allocation discipline. Recognizes disruptive threat but has no legitimate authority to pursue low-margin product development; career and budget incentives align against the innovation. d≈0.88, f(d)≈1.28, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(innovators_dilemma, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT SENIOR MANAGEMENT (ROPE) — Experiences the constraint as rational coordination with their best customers. Investing in high-margin products that customers demand is optimal strategy in the short term. The constraint appears as benign: customers benefit, margins improve, shareholder value increases. d≈0.08, f(d)≈-0.11, σ=1.1 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(innovators_dilemma, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-MARGIN CUSTOMER SEGMENT (ROPE) — Benefits from incumbent focus and continuous improvement in their preferred products. The constraint is experienced as coordination: their needs are met, product quality improves, service is responsive. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.04. Net beneficiary.
constraint_indexing:constraint_classification(innovators_dilemma, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INCUMBENT FIRM LONG-TERM (TANGLED ROPE) — Over a generational timescale, the constraint becomes visible as a mixed hybrid. The organizational discipline that protects margins in the present (coordination function) simultaneously prevents adaptation to market disruption (extraction mechanism). The firm is both protected and trapped by its own rational processes. Requires active enforcement: the accountability structures and capital allocation discipline must be actively maintained to extract value in the present, even though they cause future vulnerability. d≈0.60, f(d)≈0.82, σ=1.1 → χ≈0.44.
constraint_indexing:constraint_classification(innovators_dilemma, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INCUMBENT INDUSTRY STRUCTURE (PITON) — At the civilizational timescale, the organizational form that created the dilemma (structured hierarchy optimizing for high-margin products, capital-intensive R&D, customer-responsive development) persists long after its functional utility has degraded. The constraint is maintained through institutional inertia: quarterly earnings targets, analyst expectations, board governance structures, career paths. Theater_ratio=0.58 reflects that senior management performance metrics still measure margin improvement and customer satisfaction, even as the underlying market is being disrupted. The constraint mechanism degrades but the organizational theatre persists.
constraint_indexing:constraint_classification(innovators_dilemma, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — This perspective risks naturalizing the constraint as an immutable law: 'Organizations are necessarily myopic; they cannot simultaneously optimize for present profitability and prepare for future disruption.' However, the structural data (ε=0.52, suppression=0.65, requires_active_enforcement=true) contradicts the mountain classification. This is a false summit. The dilemma is contingent on specific organizational structures, incentive systems, and information asymmetries — not a law of nature or economics.
constraint_indexing:constraint_classification(innovators_dilemma, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(innovators_dilemma_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(innovators_dilemma, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(innovators_dilemma, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(innovators_dilemma, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(innovators_dilemma, TR),
    TR >= 0.70.

:- end_tests(innovators_dilemma_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts value from disruptive innovators (prevented market entry) and from the incumbent firm itself (future viability at risk). But the extraction is not total—disruptors can eventually penetrate if their innovation crosses the performance threshold, and incumbents can adapt if they restructure. The measurement trajectory (0.25→0.52 over 6 periods) reflects the compound effect of the dilemma: as high-margin markets mature, the opportunity cost of ignoring disruption rises, and the structural lock-in becomes more severe. Suppression (0.65): High. Multiple barriers prevent disruptors from competing with incumbents: capital requirements to achieve scale, incumbent installed base and switching costs, incumbent customer relationships and service ecosystems, incumbent control of distribution channels, and incumbent marketing power. These are not absolute barriers (some disruptors succeed) but they are substantial and systematically favor incumbents. Theater ratio (0.58): Moderate. The constraint mechanism includes performative elements: senior management performance metrics measure customer satisfaction and margin improvement even as the underlying market is being disrupted elsewhere. The theater has increased over time (0.35→0.58) as the gap between measured performance (traditional metrics) and actual firm viability (ability to adapt to disruption) has widened. By time period 6, organizations are increasingly measuring margin performance and customer satisfaction metrics that no longer predict survival.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full spectrum of perspectival disagreement, with a crucial feature: the beneficiaries (incumbent management, high-margin customers) and victims (disruptive innovators, incumbent firm long-term) are not simply in different positions—they are pursuing different timescales and have different objectives. Senior management sees rope (benign coordination). The disruptive innovator sees snare (inescapable trap). The incumbent firm over a generational timescale sees itself as both beneficiary (present margins) and victim (future viability). The analytical observer risks seeing mountain (immutable law of organizations) when the structural data reveals contingency (tangled rope, piton). The perspectival gap here is not just observational but temporal: the present coordination is the future trap.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent Management: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; experiences positive extractiveness (margin improvement). High-Margin Customers: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary. Incumbent Firm (long-term): Victim + mobile (but constrained by its own structure) → d≈0.60, f(d)≈0.82. Generational perspective shows the firm as victim of its own rational present-optimization. Incumbent Engineers: Victim + trapped (by organizational hierarchy) → d≈0.88, f(d)≈1.28. High directionality; trapped by structure. Disruptive Innovator: Victim + trapped (by capital requirements and market barriers) → d≈0.92, f(d)≈1.38. Maximum victim status; trapped by structural barriers.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH TEMPORAL DECOMPOSITION: The mandatrophy (is this coordination or extraction?) is resolved by recognizing that the constraint operates on multiple timescales simultaneously. On the immediate timescale (quarterly earnings, annual planning), the constraint is rope: senior management is rationally coordinating with customers and delivering value. On the biographical timescale (5-10 years), the constraint is tangled rope: the organization is constrained by its own structures, and extraction becomes visible (managers unable to pursue innovation, disruptors unable to compete). On the generational timescale (20+ years), the constraint is snare: the incumbent firm itself becomes trapped, unable to adapt, vulnerable to disruption. The mandatrophy is not 'is this really coordination or extraction?' but 'on which timescale does the extraction become visible?' The analytical observer who naturalizes this as a mountain (immutable law) is collapsing multiple timescales into a false universality. The correct analysis: the constraint is rope on short timescales, tangled rope on medium timescales, and snare on long timescales. It is not a single constraint viewed from different angles—it is a constraint that transforms its type as the timescale extends.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    organizational_capability_transfer,
    'Can incumbent organizations successfully develop disruptive innovations in parallel organizational structures (skunkworks, spin-offs, separate P&L units) without cannibalizing high-margin revenue streams?',
    'Historical case analysis of incumbent attempts at parallel disruptive innovation (Xerox PARC, IBM PC division, automotive OEM EV programs); measurement of time-to-market, profitability trajectory, and integration into parent firm',
    'If successful transfer is possible: constraint is organizational architecture (scaffold with leadership change), not structural impossibility. If transfer fails: constraint is fundamental to hierarchy (true tangled rope or snare). If occasional successes are followed by reversion: constraint is piton (institutional inertia).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organizational_capability_transfer, empirical, 'Whether organizational structures can support parallel disruptive and incumbent innovation').

omega_variable(
    customer_demand_versus_latent_need,
    'Do high-margin customers represent the actual latent market demand, or does focusing on their stated preferences cause incumbents to miss broader market needs that disruptors capture?',
    'Post-disruption analysis: survey non-customers of incumbent who adopted disruptive alternative; measure willingness-to-pay for incumbent product if modified to disruptive specifications; track total addressable market shift',
    'If incumbents were genuinely optimizing for true demand: constraint is purely benign coordination (rope). If incumbents were misinterpreting customer preferences or ignoring larger markets: constraint is extraction mechanism disguised as customer service.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customer_demand_versus_latent_need, empirical, 'Whether incumbent customer focus reflects true market demand or creates blind spots').

omega_variable(
    capital_intensity_as_barrier,
    'Is the disruptive innovator''s initial low-margin niche market constraint fundamentally due to capital requirements, or due to organizational factors that could be overcome with different capital structures?',
    'Analysis of capital requirements: comparison of funded disruptors vs bootstrapped disruptors; measurement of path length to viable scale under different financing models',
    'If capital intensity is the fundamental barrier: constraint is partially mountain (physical law of production). If organizational factors dominate: constraint is tangled rope (contingent on incentive structures).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_intensity_as_barrier, empirical, 'Whether capital barriers or organizational factors drive the dilemma').

omega_variable(
    rational_versus_irrational_incumbent_response,
    'When incumbents recognize disruptive threats, are their failure-to-respond patterns due to organizational rationality (correct optimization for present), organizational irrationality (bias, hubris), or structural impossibility (tangled rope)?',
    'Cognitive science + organizational analysis: measure incumbent decision-maker awareness of threat; trace decision-making processes; identify where rationality breaks down or organizational structure prevents action',
    'If purely rational: constraint is structural (tangled rope). If significant irrationality: constraint is cognitive (snare for decision-makers due to bias). If mixed: constraint is hybrid. Resolution changes the mandatrophy analysis fundamentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_versus_irrational_incumbent_response, conceptual, 'Whether incumbent failure reflects rational constraint or decision-making bias').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(innovators_dilemma, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(innov_tr_t0, innovators_dilemma, theater_ratio, 0, 0.35).
narrative_ontology:measurement(innov_tr_t3, innovators_dilemma, theater_ratio, 3, 0.47).
narrative_ontology:measurement(innov_tr_t6, innovators_dilemma, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(innov_be_t0, innovators_dilemma, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(innov_be_t3, innovators_dilemma, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(innov_be_t6, innovators_dilemma, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(innovators_dilemma, resource_allocation).
narrative_ontology:affects_constraint(innovators_dilemma, incumbent_firm_adaptation).
narrative_ontology:affects_constraint(innovators_dilemma, market_entry_barriers).
narrative_ontology:affects_constraint(innovators_dilemma, organizational_capture_by_stakeholder).

% DUAL FORMULATION NOTE:
% The Innovator's Dilemma can be decomposed into three structurally distinct constraints: (1) Market Entry Barriers (upstream): the capital and defensibility barriers that disruptive innovators face in low-margin niches (ε≈0.45, Mountain from capital view, Snare from innovator view). (2) The Innovator's Dilemma (this story): the organizational mechanism that prevents incumbents from responding (ε≈0.52, Tangled Rope on generational timescale). (3) Incumbent Firm Adaptation Failure (downstream): the long-term consequence of failure to adapt (ε≈0.68, Snare on civilization timescale). These three constraints are linked: market barriers create low-margin niches (upstream), which trigger incumbent myopia (this constraint), which produces adaptation failures (downstream). Each has distinct ε values and distinct classification patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
