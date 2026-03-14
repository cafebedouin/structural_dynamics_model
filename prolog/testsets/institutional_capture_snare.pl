% ============================================================================
% CONSTRAINT STORY: institutional_capture_snare
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_capture_snare, []).

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
 *   constraint_id: institutional_capture_snare
 *   human_readable: Institutional Capture Snare: Regulatory Extraction Through Normalization
 *   domain: institutional_politics/regulatory_capture
 *
 * SUMMARY:
 *   Institutional capture represents the transformation of a regulatory
 *   agency from an independent arbiter of public interest into a coordinator
 *   and protector of the industry it is mandated to regulate. This constraint
 *   exemplifies how a snare masquerades as coordination (rope). The regulated
 *   industry captures the regulator through multiple reinforcing mechanisms:
 *   revolving-door employment (regulators gain industry jobs, industry
 *   personnel staff regulatory agencies), information asymmetry (regulators
 *   depend on industry for technical expertise), and political economy
 *   (industry lobbying influences agency funding and leadership
 *   appointments). The result is regulatory enforcement redirected from
 *   systemic risk prevention toward competitive exclusion: enforcement
 *   actions target new market entrants or potential competitors rather than
 *   the incumbent industry's violations. The public interest constituency
 *   (the diffuse beneficiary of actual regulation) has no organized presence
 *   in the capture mechanism and bears the full extraction cost as
 *   protections dissolve. The constraint's theater ratio increases over time
 *   as regulatory processes maintain the appearance of rigorous review while
 *   the substance shifts toward industry preference. The suppression
 *   mechanism operates through normalization ('regulatory expertise requires
 *   industry consultation') rather than explicit coercion, making it more
 *   difficult to identify and reform than explicit extraction.
 *
 * KEY AGENTS:
 *   - Regulated Industry: Primary beneficiary (institutional/arbitrage) — captures regulatory discretion to exclude competitors and eliminate enforcement threat
 *   - Public Interest Constituency: Primary victim (powerless/trapped) — diffuse group (consumers, environmental stakeholders, new entrants, systemic risk prevention) with no organized presence; cannot exit regulatory system
 *   - Market Entrants/Competitors: Secondary victim (moderate/constrained) — face regulatory barriers designed by captured agency to exclude them, not to protect consumers
 *   - Regulatory Agency: Victim (powerful/mobile, at institutional level) — the agency's core function (independence and public-interest enforcement) is extracted away through personnel capture and political economy
 *   - Legislative Oversight: Institutional actor (institutional/constrained) — itself captured by industry lobbying, creating cascade where oversight atrophies to theater
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine coordination function AND extractive capture, risking naturalization of the hybrid as 'how regulation necessarily works'
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_capture_snare, 0.68).
domain_priors:suppression_score(institutional_capture_snare, 0.72).
domain_priors:theater_ratio(institutional_capture_snare, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_capture_snare, extractiveness, 0.68).
narrative_ontology:constraint_metric(institutional_capture_snare, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(institutional_capture_snare, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_capture_snare, snare).
narrative_ontology:human_readable(institutional_capture_snare, "Institutional Capture Snare: Regulatory Extraction Through Normalization").
narrative_ontology:topic_domain(institutional_capture_snare, "institutional_politics/regulatory_capture").

% --- Structural relationships ---
narrative_ontology:constraint_victim(institutional_capture_snare, public_interest_constituency).
narrative_ontology:constraint_victim(institutional_capture_snare, regulated_market_entrants).
narrative_ontology:constraint_victim(institutional_capture_snare, enforcement_agency_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC INTEREST (SNARE) — The diffuse beneficiary of regulatory oversight (consumers, environmental stakeholders, competitive markets, systemic risk prevention) has no organized presence in the capture mechanism. Cannot exit regulatory system; bears full extraction cost as regulations shift to benefit the regulated industry. Maximal experienced extraction — abstract collective with zero agency.
constraint_indexing:constraint_classification(institutional_capture_snare, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARKET ENTRANTS (SNARE) — New firms face regulatory barriers designed to exclude them, not to protect consumers. Regulatory capture redirects enforcement toward competitive threats rather than systemic risks. High exit cost (must lobby separately, absorb compliance burden) but cannot escape the regulatory frame. Significant extraction — constrained rather than trapped, but still snare-level.
constraint_indexing:constraint_classification(institutional_capture_snare, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATED INDUSTRY (ROPE) — Primary beneficiary. Experiences the constraint as coordination: regulatory clarity enables long-term investment and reduces uncertainty. Low or negative effective extraction — the industry perceives genuine benefit from participation in regulatory design. Arbitrage exit (can lobby or relocate) means low structural d. This perspective sees the constraint as collaborative governance, not capture.
constraint_indexing:constraint_classification(institutional_capture_snare, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: AGENCY INDEPENDENCE (SNARE) — The regulatory agency's mandate to enforce public interest becomes inverted toward industry protection. Enforcement discretion is captured through revolving-door hiring, regulatory capture via personnel, and normalization of industry-favorable interpretation. Powerful agents at the institutional level, yet snare-classified because the agency's structural independence (its core function) is extracted away. High suppression through institutional inertia and captured personnel; moderate exit cost (agency could theoretically refuse capture, but would face defunding and political pressure).
constraint_indexing:constraint_classification(institutional_capture_snare, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE OVERSIGHT (PITON) — Congress or parliament that theoretically oversees the agency is itself captured, creating a cascade: industry lobbies legislator, legislator defunds or removes agency leadership that resists capture, agency normalizes industry preferences. Theater ratio high (appearances of oversight and regulation persist while substance degrades). The oversight mechanism persists through institutional inertia even as its function has atrophied. Constrained rather than trapped (legislature could theoretically reallocate power, but faces political economy of campaign finance and revolving-door incentives).
constraint_indexing:constraint_classification(institutional_capture_snare, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, institutional capture represents a genuine coordination function (industry and regulator solving information asymmetry and compliance complexity) overlaid with asymmetric extraction (beneficiary captures the mechanism to exclude competitors and eliminate oversight). The constraint possesses both functions: real coordination benefit to the parties (industry and regulator understand each other) AND extractive redirection of enforcement toward competitive exclusion rather than systemic risk. The analytical observer sees both the rope and the snare simultaneously, classifying the compound structure as tangled rope. However, the base properties and snare-victim declarations suggest that the coordination function is subordinate to extraction, making snare the more accurate label.
constraint_indexing:constraint_classification(institutional_capture_snare, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_capture_snare_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_capture_snare, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_capture_snare, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_capture_snare, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_capture_snare, TR),
    TR >= 0.70.

:- end_tests(institutional_capture_snare_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The constraint extracts competitive advantage to the incumbent and reduces actual enforcement against that industry. The trajectory shows extractiveness increasing over time (0.35 → 0.68) as capture deepens and alternative regulatory pathways are eliminated. This is not maximum extraction (0.95+) because some residual enforcement and oversight persist, but the trend is toward total capture. Suppression (0.72): Very high. Multiple layers of suppression maintain the capture: (1) structural — industry controls agency's information sources and expertise; (2) political — industry lobbying controls agency funding and leadership; (3) epistemic — regulatory discourse normalizes industry consultation as legitimate expertise, not capture; (4) personnel — revolving-door incentives prevent agency independence. Theater ratio (0.55): Moderate but rising. The regulatory apparatus maintains appearances of review and enforcement while substance shifts. Regulatory filings, review meetings, and formal processes persist. Theater has increased over the interval (0.35 → 0.55) as the gap between procedure and substance widens. This is not piton-level theater (0.70+) because some real enforcement persists, but the theatrical component is substantial.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence across six types. Beneficiary (institutional/arbitrage) → Rope (sees coordination). Victims (powerless/trapped, moderate/constrained, powerful/mobile at institutional) → Snare (extraction dominates). Oversight (institutional/constrained) → Piton (theater persists, function atrophied). Analytical → Tangled Rope (sees both functions), but snare dominates in base properties. The gap is not due to measurement ambiguity; it is due to genuine structural difference in agent positions. The same regulatory mechanism benefits the incumbent and harms competitors and the public interest simultaneously. This is the defining signature of capture: one agent's rope is another agent's snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies dramatically by agent. The regulated industry has low d (0.05-0.15 beneficiary range) — they benefit and have arbitrage exit (can lobby alternatively or relocate). The public interest has high d (0.90-0.95) — trapped with no exit and bearing full extraction cost. Market entrants have high d (0.75-0.85) — constrained by regulatory barriers but can theoretically escape through capital mobility or political organization. The regulatory agency has paradoxical d (0.75-0.85 despite powerful status) — powerful institutions can still be snare victims if their core function (independence) is extracted. The analytical observer has moderate d (0.70-0.75) by default, but the true d depends on which structural position they occupy: if they are inside the regulatory system (higher d), if they are external analysts (lower d). The derivation chain runs: beneficiary/victim declaration → exit_options → power_atom → d → f(d) → χ. The snare classification emerges because d > 0.66 for all victim positions, yielding χ > 0.66 across perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   Institutional capture resolves the mandatrophy by distinguishing genuine coordination (rope properties shared with beneficiary) from extraction (snare properties imposed on victims). The constraint is NOT pure extraction (snare-only) because the beneficiary actually perceives and values the coordination function. The constraint is NOT pure coordination (rope-only) because the victims experience maximal extraction without coordination benefit. The tangled rope classification (from the analytical observer) is the synthetic view: the constraint possesses both functions, but extraction overwhelms coordination for most agents. The snare base classification reflects that the asymmetry is the fundamental feature — the constraint exists because extraction is more important to its structure than coordination. Reform would require either (a) redistributing coordination benefits to all parties (converting tangled rope to rope) or (b) eliminating the extraction while preserving coordination (which would likely fail because the coordination benefit exists only for the beneficiary).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_function_primacy,
    'Is the captured regulator-industry relationship primarily a coordination mechanism that reduces transaction costs, or is coordination merely the justification for asymmetric extraction?',
    'Compare regulatory outcomes in captured vs. independent regulatory agencies: do captured agencies produce lower transaction costs for all market participants, or only for the dominant incumbent? Measure: cost of compliance for market entrants; frequency of enforcement actions; ratio of preventive vs. reactive regulation.',
    'If coordination is primary: reclassify from Snare to Tangled Rope. If coordination is justificatory: classification as Snare is confirmed. This distinction determines whether the constraint can be reformed through better coordination or requires structural separation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_primacy, empirical, 'Whether capture''s coordination function is primary or secondary to extraction').

omega_variable(
    personnel_capture_vs_structural_capture,
    'Is institutional capture driven by individual agency personnel being compromised (revolving-door effect), or by structural incentive misalignment that would corrupt any personnel?',
    'Historical turnover analysis: does replacing captured personnel with public-interest-aligned individuals restore agency independence (personnel problem) or do new personnel become captured within 1-2 years (structural problem)? Empirical case: post-scandal agency leadership replacements; measure duration of independence.',
    'If personnel-driven: reform through hiring standards and conflict-of-interest rules. If structural: reform requires legislative realignment of agency mandate, funding, or independence. Misdiagnosis leads to failed reform attempts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(personnel_capture_vs_structural_capture, empirical, 'Whether capture is driven by personnel corruption or structural incentives').

omega_variable(
    competitive_vs_systemic_focus_misalignment,
    'When regulatory enforcement shifts toward competitors rather than systemic risks, is this intentional industry capture or unintended consequence of regulatory expertise concentration?',
    'Regulatory authority decision logs and enforcement rationale; survey of enforcement personnel on justification for action selection; comparison of enforcement pattern against stated agency priorities vs. implied priorities from industry preferences.',
    'If intentional capture: requires personnel and institutional reform. If unintended expertise bias: can be corrected through randomization of enforcement audits and decentralized authority. If both: systemic reform is necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_vs_systemic_focus_misalignment, empirical, 'Whether enforcement misalignment is intentional capture or unintended bias').

omega_variable(
    suppression_mechanism_transparency_paradox,
    'Why does institutional capture persist despite being widely known and documented? Is suppression maintained through active denial or passive acceptance?',
    'Narrative analysis of regulatory discourse: do agency documents acknowledge capture or deny it? Do personnel resist or cooperate? Survey of political economy research visibility: is capture research integrated into regulatory training or ignored? Measure: ratio of academic/journalistic documentation of capture vs. policy action taken.',
    'If active denial: suppression maintained through epistemic closure and gaslighting. If passive acceptance: suppression maintained through normalization (''that''s just how regulation works''). The mechanism determines the reform pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_transparency_paradox, empirical, 'Whether suppression of capture knowledge is active denial or passive acceptance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_capture_snare, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(icap_tr_t0, institutional_capture_snare, theater_ratio, 0, 0.35).
narrative_ontology:measurement(icap_tr_t10, institutional_capture_snare, theater_ratio, 10, 0.45).
narrative_ontology:measurement(icap_tr_t20, institutional_capture_snare, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(icap_be_t0, institutional_capture_snare, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(icap_be_t10, institutional_capture_snare, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(icap_be_t20, institutional_capture_snare, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_capture_snare, resource_allocation).
narrative_ontology:affects_constraint(institutional_capture_snare, market_entry_barrier_regulatory).
narrative_ontology:affects_constraint(institutional_capture_snare, agency_independence_degradation).
narrative_ontology:affects_constraint(institutional_capture_snare, competitive_exclusion_through_enforcement).

% DUAL FORMULATION NOTE:
% Institutional capture operates as a family of linked constraints: (1) agency_independence_degradation captures the structural extraction of the agency's core function; (2) market_entry_barrier_regulatory captures the enforcement redirection toward competitive exclusion; (3) competitive_exclusion_through_enforcement captures the downstream market effect. This story (institutional_capture_snare) models the overarching snare structure; downstream constraints model specific mechanism implementations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_capture_snare, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
