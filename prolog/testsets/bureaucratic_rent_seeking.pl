% ============================================================================
% CONSTRAINT STORY: bureaucratic_rent_seeking
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bureaucratic_rent_seeking, []).

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
 *   constraint_id: bureaucratic_rent_seeking
 *   human_readable: Bureaucratic Rent Seeking: Institutional Self-Preservation Through Regulatory Expansion
 *   domain: political_economy/institutional_capture
 *
 * SUMMARY:
 *   Bureaucratic rent-seeking describes the structural dynamic in which
 *   regulatory agencies expand rules, complexity, and enforcement scope in
 *   order to maintain and grow their budgets, authority, and staff,
 *   independent of whether additional regulation solves genuine coordination
 *   problems. The constraint exhibits the tangled rope signature: real
 *   coordination functions (safety standards, information standardization,
 *   collective action) coexist with asymmetric extraction (compliance costs
 *   disproportionately burden new entrants and small firms; large incumbents
 *   benefit from barriers to competition; career bureaucrats benefit from
 *   domain expansion). The theater ratio has risen from 0.42 to 0.68 over
 *   twenty years, reflecting that legislative oversight of agencies has
 *   become increasingly performative while agency autonomy has expanded. The
 *   extractiveness trend (0.32 → 0.58) shows how the constraint accumulates
 *   over time: each new regulation adds compliance cost; few regulations are
 *   repealed; the burden compounds. This is the signature of Goodhart drift —
 *   the metric (regulatory coverage) substitutes for the outcome (actual
 *   coordination), leading to extraction that increases independent of
 *   coordination benefit.
 *
 * KEY AGENTS:
 *   - Career Bureaucrats: Primary beneficiary (institutional/arbitrage) — expand authority, budgets, staff through domain expansion; enjoy job security and career advancement tied to regulatory scope
 *   - Regulatory Agencies: Primary beneficiary (institutional/arbitrage) — extract rent through budget expansion and authority growth; see coordination function as genuine but lack incentive to measure whether coordination/extraction ratio justifies added rules
 *   - Regulated Private Sector (Small/New): Primary victim (powerless/trapped) — bear compliance costs that scale with regulatory complexity; cannot exit jurisdiction or industry without total relocation
 *   - Consumers: Primary victim (powerless/trapped) — bear extraction through higher prices, reduced choice, reduced innovation; suppressed by information asymmetry and distributed costs
 *   - Large Incumbent Firms: Secondary beneficiary (powerful/mobile) — benefit from regulatory barriers that block competitors; can absorb compliance costs through scale; often help shape regulations to favor their cost structure
 *   - Legislative Oversight Bodies: Institutional observer (institutional/arbitrage) — maintain performative oversight; real agency autonomy exceeds legislative capacity to monitor
 *   - Reform Coalition: Organized victim/reformer (moderate/constrained) — see extraction clearly but constrained by inability to dismantle regulatory systems; benefit from some coordination functions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bureaucratic_rent_seeking, 0.58).
domain_priors:suppression_score(bureaucratic_rent_seeking, 0.65).
domain_priors:theater_ratio(bureaucratic_rent_seeking, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bureaucratic_rent_seeking, extractiveness, 0.58).
narrative_ontology:constraint_metric(bureaucratic_rent_seeking, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bureaucratic_rent_seeking, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bureaucratic_rent_seeking, tangled_rope).
narrative_ontology:human_readable(bureaucratic_rent_seeking, "Bureaucratic Rent Seeking: Institutional Self-Preservation Through Regulatory Expansion").
narrative_ontology:topic_domain(bureaucratic_rent_seeking, "political_economy/institutional_capture").

domain_priors:requires_active_enforcement(bureaucratic_rent_seeking).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bureaucratic_rent_seeking, career_bureaucrats).
narrative_ontology:constraint_beneficiary(bureaucratic_rent_seeking, regulatory_agencies).
narrative_ontology:constraint_victim(bureaucratic_rent_seeking, regulated_private_sector).
narrative_ontology:constraint_victim(bureaucratic_rent_seeking, consumers).
narrative_ontology:constraint_victim(bureaucratic_rent_seeking, innovation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL REGULATED FIRM (SNARE) — Trapped by regulatory compliance costs that scale with complexity regardless of firm size. Cannot exit the jurisdiction or industry without total relocation. Bears full extraction cost through licensing, reporting, environmental permits, safety inspections. No meaningful exit option. Pure extraction from this agent's position.
constraint_indexing:constraint_classification(bureaucratic_rent_seeking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSUMER (SNARE) — Trapped in regulated market structure. Regulatory costs are embedded in prices; consumer cannot avoid paying them and cannot organize exit. Suppressed by complexity (regulatory rules are opaque) and barrier to entry (new competitors can't break through regulatory moat). Experiences pure extraction through higher prices and reduced choice.
constraint_indexing:constraint_classification(bureaucratic_rent_seeking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE INCUMBENT FIRM (TANGLED ROPE) — Experiences genuine coordination function (regulatory safety/quality standards) alongside asymmetric extraction benefit. Can absorb compliance costs through scale and capital; often shapes regulations to favor their cost structure. Extraction runs toward this agent (through regulatory barriers that block competitors), but they also benefit from the coordination function (safety standards that build consumer trust). Mobile exit option (can relocate HQ/operations) but exercises it rarely because the regulatory moat protects them from competition.
constraint_indexing:constraint_classification(bureaucratic_rent_seeking, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AGENCY (ROPE) — Experiences constraint as coordination mechanism: enforcing standards, collecting information, standardizing practices across dispersed firms. Benefits from expansion (larger budget, staff, authority over sector). From the agency's internal perspective, the coordination function is real and essential. Theater ratio here is low (internal procedures are functional, not performative). The agency sees itself as solving collective action problems.
constraint_indexing:constraint_classification(bureaucratic_rent_seeking, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE OVERSIGHT (PITON) — Congressional committees nominally supervise regulatory agencies through hearings, budget control, and legislative mandates. In practice, oversight is largely performative: committees hold hearings and issue reports, but agency autonomy has expanded beyond legislative capacity to monitor. Theater ratio is high (performative oversight rituals persist; real agency behavior diverges from legislative intent). Piton derives from the theater gate — the oversight mechanism has atrophied while the ritual persists.
constraint_indexing:constraint_classification(bureaucratic_rent_seeking, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: REFORM COALITION (TANGLED ROPE) — Organized groups (small business associations, consumer advocates, policy reformers) see the bureaucratic rent-seeking as extractive but also acknowledge genuine coordination benefits from regulation. They experience constrained exit (can advocate for reform but cannot dismantle regulatory systems unilaterally). The coalition benefits from some regulatory functions (consumer protection, safety standards) while opposing the extractive overgrowth. Moderate power; genuine coordination function alongside extraction creates tangled rope.
constraint_indexing:constraint_classification(bureaucratic_rent_seeking, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks naturalizing bureaucratic rent-seeking as an inherent feature of complex modern society ('All regulatory systems have inefficiencies'; 'Bureaucracy always expands'). From this perspective, the constraint appears immutable — a natural law of institutional dynamics. However, the structural data reveals this as a false summit: bureaucratic expansion is not an immutable law but a contingent institutional arrangement enabled by suppression mechanisms (opacity, information asymmetry, distributed costs). The mountain classification reveals the naturalization fallacy.
constraint_indexing:constraint_classification(bureaucratic_rent_seeking, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_rent_seeking_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bureaucratic_rent_seeking, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_rent_seeking, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bureaucratic_rent_seeking, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bureaucratic_rent_seeking, TR),
    TR >= 0.70.

:- end_tests(bureaucratic_rent_seeking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts through regulatory compliance costs, barriers to entry, and suppressed alternatives. Extraction is real but not total — genuine coordination functions (safety standards, information sharing) justify some regulatory cost. The 0.58 reflects that roughly 50-60% of regulatory complexity is extractive overhead beyond the genuine coordination function. The 0.32 → 0.58 trajectory shows accumulation: regulations rarely sunset; compliance costs compound. Suppression (0.65): High. Barriers include: (1) opacity of regulatory rules and rationale (agents cannot easily evaluate whether regulation solves real problems), (2) distributed costs (each regulated firm bears small compliance cost; total costs are massive but invisible to any one agent), (3) barriers to entry (new firms cannot access markets without navigating regulatory gauntlet), (4) captured information (regulatory agencies monopolize data about compliance effectiveness). Suppression mechanisms prevent agents from organizing exit or reform. Theater ratio (0.68): High and rising. Legislative oversight of agencies is substantially performative: congressional hearings, committee reports, and budget negotiations follow ritual patterns but do not constrain agency behavior effectively. Agency autonomy has expanded beyond legislative capacity to monitor. The constraint's performative elements (regulatory ritual, oversight theater) have increased from 0.42 to 0.68, indicating that functional regulation has been progressively substituted with performative regulation. This is Goodhart drift applied to governance.
 *
 * PERSPECTIVAL GAP:
 *   The structural gap between beneficiary and victim perspectives is stark. The regulatory agency sees coordination (Rope) — enforcing safety standards, standardizing practices, enabling collective action. The small firm sees pure extraction (Snare) — compliance costs with no benefit, barriers to entry they cannot overcome. The large incumbent sees tangled rope — they appreciate the coordination function (which builds consumer confidence) and benefit from barriers that block competitors (which protects their market position). The reform coalition sees tangled rope with a different spin — they want the coordination without the extraction but lack power to separate them. The legislative oversight system sees its own ritual (Piton) — hearings and reports continue but real agency behavior escapes legislative control. The analytical observer risks false natural law (Mountain) — 'all bureaucracies expand' — but the structural data reveals this as naturalization: the expansion is enabled by suppression mechanisms (opacity, distributed costs, captured information) that could be addressed through transparency and measurement reforms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position relative to the extraction flow. Small firms and consumers are trapped (d → 0.95); they cannot exit and have no arbitrage options. Regulatory agencies are institutional beneficiaries with arbitrage options (d → 0.05); extraction flows toward them. Large incumbent firms have mobile options but benefit from the constraint (d → 0.35); they can arbitrage (relocate) but rarely do because the regulatory moat is valuable. The reform coalition is moderately constrained (d → 0.60); they face significant barriers to exiting or reforming the system but retain some advocacy power. The legislative system is institutional but has ceded real authority (d → 0.25); it was designed as the apex of regulatory authority but now exercises mostly performative oversight. The chi computation applies f(d) to produce effective extraction from each agent's perspective. Trapped agents experience maximum chi; arbitrage beneficiaries experience negative chi (the constraint subsidizes them); constrained and mobile agents experience moderate chi. This produces the perspectival gap: the snare perspective (from trapped agents) and the rope perspective (from agencies) are incompatible readings of the same constraint, but they are structurally determined by directionality differences.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by acknowledging that bureaucratic rent-seeking is a real hybrid coordination-extraction system, not a pure snare and not a pure rope. The decomposition strategy is (1) measure the genuine coordination function (call it C), (2) measure the extractive overhead (call it E), (3) if E > C, classification is Snare-dominant; if E ≈ C, classification is Tangled Rope; if C > E, classification is Rope-dominant. Current data suggests E ≈ 0.45, C ≈ 0.55 (rough estimates), yielding Tangled Rope as claimed type. The mandatrophy is resolved by making the mixture explicit: this is NOT a pure extraction system (which would classify as Snare) NOR a pure coordination system (which would classify as Rope), but a genuine hybrid where real coordination benefits are embedded in and exploited by extractive mechanisms. The theater_ratio rising from 0.42 to 0.68 is the key diagnostic: as performative oversight and regulatory ritual increase relative to functional regulation, the constraint is drifting from Tangled Rope (mixed but functional) toward Piton (degraded ritual). If theater rises above 0.75, the constraint transitions to Piton type — the coordination function atrophies; the extraction persists through inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_coordination_vs_extraction_cover,
    'How much of the regulatory complexity is genuine coordination (safety, information standardization, collective action) versus cover for extractive rent-seeking?',
    'Comparative analysis: regulatory regimes with different extractiveness levels within the same sector; historical analysis of regulatory expansion tied to specific coordination problems vs expansion tied to agency budget cycles',
    'If genuine coordination dominates (>70%): classification shifts toward Rope/Scaffold from multiple perspectives. If extraction dominates (>60%): classification shifts toward Snare/Tangled Rope. Current decomposition assumes ~55% coordination, 45% extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_coordination_vs_extraction_cover, empirical, 'Ratio of genuine coordination function to extractive overhead in regulatory system').

omega_variable(
    barrier_to_entry_intentionality,
    'Are regulatory barriers to entry a deliberate extraction mechanism or an unintended side effect of coordination rules?',
    'Longitudinal analysis of regulatory rule changes: correlation between barrier-raising changes and incumbent firm preferences; interviews with regulators about rule design rationale; historical analysis of when barriers were reduced and by whom',
    'If intentional: suppression is deliberately maintained; extraction is systematic. If unintended: suppression might be addressable through better regulatory design without sacrificing coordination. Changes the assessment of whether the constraint is a designed snare versus a corrupted coordination system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(barrier_to_entry_intentionality, empirical, 'Whether regulatory barriers to entry are intentional extraction or unintended externality').

omega_variable(
    consumer_preference_for_regulation,
    'To what extent do consumers voluntarily support regulatory requirements for safety/quality, and to what extent are they trapped by suppressed alternatives?',
    'Revealed preference analysis: willingness to pay for certified vs uncertified products; regulatory relaxation experiments; markets with optional compliance (organic certification, ratings systems) showing consumer demand',
    'If consumers prefer regulation: some suppression is consensual; the constraint is less purely extractive. If suppression is what creates appearance of preference: consumers are trapped; extraction is higher. Affects d values for consumer perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_preference_for_regulation, empirical, 'Degree of consumer preference for regulation versus regulatory capture of consumer choice').

omega_variable(
    fiscal_dependent_agencies,
    'What fraction of regulatory agencies'' budgets and career advancement depend on maintaining/expanding their regulatory domain?',
    'Budget structure analysis; career progression data for regulators; correlation between agency budgets and regulatory rule-making activity; comparison to agencies without direct fiscal incentives',
    'High dependency (>70%): bureaucratic rent-seeking becomes systemic feature; extractiveness pushes toward 0.65+. Low dependency (<30%): rent-seeking is behavioral choice, not structural incentive; extractiveness drops to ~0.40. Affects classification of agency perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiscal_dependent_agencies, empirical, 'Budget and career dependence of regulatory agencies on domain expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bureaucratic_rent_seeking, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brs_tr_t0, bureaucratic_rent_seeking, theater_ratio, 0, 0.42).
narrative_ontology:measurement(brs_tr_t10, bureaucratic_rent_seeking, theater_ratio, 10, 0.55).
narrative_ontology:measurement(brs_tr_t20, bureaucratic_rent_seeking, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(brs_be_t0, bureaucratic_rent_seeking, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(brs_be_t10, bureaucratic_rent_seeking, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(brs_be_t20, bureaucratic_rent_seeking, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bureaucratic_rent_seeking, enforcement_mechanism).
narrative_ontology:affects_constraint(bureaucratic_rent_seeking, regulatory_capture).
narrative_ontology:affects_constraint(bureaucratic_rent_seeking, innovation_suppression).
narrative_ontology:affects_constraint(bureaucratic_rent_seeking, small_firm_exit_barrier).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bureaucratic_rent_seeking, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
