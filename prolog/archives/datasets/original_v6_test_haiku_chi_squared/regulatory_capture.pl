% ============================================================================
% CONSTRAINT STORY: regulatory_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture, []).

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
 *   constraint_id: regulatory_capture
 *   human_readable: Regulatory Capture
 *   domain: economic/political
 *
 * SUMMARY:
 *   Regulatory capture represents a systematic transformation of institutions
 *   designed for public protection into mechanisms for incumbent rent
 *   extraction. The constraint exhibits eight distinct perspectives — from
 *   natural law (Olsonian inevitability) to temporary coordination problem
 *   (scaffold with sunset via transparency/independence reforms) to pure
 *   extraction (snare from citizens' perspective). The same regulatory
 *   machinery that a captured agency experiences as coordinating market entry
 *   (rope) is experienced by entrants as a snare of compliance barriers. The
 *   theater_ratio (0.65) reflects that captured agencies maintain democratic
 *   procedural legitimacy (public comment, notice-and-comment rulemaking,
 *   advisory boards) while actual decision-making is dominated by incumbent
 *   industry input. Extractiveness has increased over the 50-year interval
 *   (0.35 → 0.58) as lobbying sophistication and regulatory complexity have
 *   grown, while transparency (open meetings, FOIA) has failed to prevent
 *   capture. Theater ratio has risen in parallel (0.40 → 0.65), indicating
 *   increasing gap between procedural appearance and functional reality.
 *
 * KEY AGENTS:
 *   - Incumbent Regulated Firms: Primary beneficiary (institutional/arbitrage) — benefit from regulatory barriers to entry, predictable rules, exclusion of competitors; can influence agency through revolving door, campaign finance, and technical expertise
 *   - General Public: Primary victim (powerless/trapped) — bears cost of reduced competition, higher prices, suppressed innovation; cannot exit regulatory regime or organize effectively
 *   - Competitive Entrants: Secondary victim (moderate/constrained) — face compliance costs and regulatory barriers designed to protect incumbents; can exit market but cannot reform agency
 *   - Regulatory Agency (Captured): Institutional actor (institutional/arbitrage) — experiences captured relationship as solving its coordination problems (stable funding, clear mandates, predictable stakeholders); aligns incentives with industry
 *   - Regulatory Reform Coalition: Organized opposition (organized/constrained) — consumer advocates, think tanks, watchdogs attempting to counteract capture; have some organized voice but limited exit options
 *   - Supranational Observer: External institutional actor (powerful/mobile) — international trade bodies, transnational NGOs with leverage through sanctions or cross-jurisdictional pressure
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional capture as inevitable feature of democracy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture, 0.58).
domain_priors:suppression_score(regulatory_capture, 0.68).
domain_priors:theater_ratio(regulatory_capture, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_capture, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(regulatory_capture, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture, tangled_rope).
narrative_ontology:human_readable(regulatory_capture, "Regulatory Capture").
narrative_ontology:topic_domain(regulatory_capture, "economic/political").

domain_priors:requires_active_enforcement(regulatory_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture, incumbent_firms).
narrative_ontology:constraint_beneficiary(regulatory_capture, regulated_industry).
narrative_ontology:constraint_victim(regulatory_capture, consumer_welfare).
narrative_ontology:constraint_victim(regulatory_capture, competitive_entrants).
narrative_ontology:constraint_victim(regulatory_capture, general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERAL PUBLIC (SNARE) — Citizens cannot exit the regulatory regime and have no organized voice in capture dynamics. They bear full cost of reduced competition, higher prices, and regulatory barriers to innovation. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80. Pure extraction masked as consumer protection.
constraint_indexing:constraint_classification(regulatory_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETITIVE ENTRANT (SNARE) — New firms face regulatory barriers designed to protect incumbents (licensing, standards, compliance costs). Exit option is constrained: leaving the market is the only option; reforming the agency is not. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.67. High extraction; compliance costs exceed ability to compete.
constraint_indexing:constraint_classification(regulatory_capture, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY REFORM COALITION (TANGLED ROPE) — Consumer advocates, think tanks, and reform-minded politicians see genuine coordination function (transparency, information asymmetry reduction) mixed with extraction (capture dynamics). They face constraints on exit (cannot abandon regulation entirely) but have some organized voice. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.43. Mixed: some extraction, some legitimate coordination.
constraint_indexing:constraint_classification(regulatory_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT REGULATED FIRM (ROPE) — Dominant firms experience the captured agency as a coordination mechanism: it ensures predictable rules, excludes competitors, and provides market stability. They have high exit options (relocate, engage in regulatory arbitrage across jurisdictions) and direct influence over agency staff. d≈0.10, f(d)≈0.05, σ=1.0 → χ≈0.03. Effective subsidy; firm sees the agency as solving coordination problems (market entry, standard-setting).
constraint_indexing:constraint_classification(regulatory_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY AGENCY (CAPTURED STATE) (ROPE) — From the agency's internal perspective, the captured relationship solves its own coordination problems: it has stable funding (industry support for agency budgets), clear objectives (industry stability), and predictable stakeholders (regulated firms with whom it negotiates). Staff career paths are aligned with industry preferences. d≈0.05, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Net beneficiary: the agency interprets its function as industry coordination.
constraint_indexing:constraint_classification(regulatory_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: SUPRANATIONAL OBSERVER (TANGLED ROPE) — International bodies (WTO, IMF, transnational NGOs) see both coordination function (regulatory predictability enables trade) and extraction (barriers to market entry, rent extraction). They have some exit options (trade sanctions, conditionality) and can leverage multiple jurisdictions. d≈0.48, f(d)≈0.57, σ=1.2 → χ≈0.40. Moderate extraction; complex mix of legitimate coordination and protectionist capture.
constraint_indexing:constraint_classification(regulatory_capture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: REGULATORY THEATER (PITON) — Public comment periods, environmental reviews, and administrative procedures persist to maintain the appearance of democratic accountability despite capture. theater_ratio=0.65 (high procedural performance, minimal functional impact on agency decisions). The regulatory process has become substantially performative: rules written by industry counsel, public comment periods ignored, advisory committees composed of incumbent firm representatives. d≈0.08, f(d)≈-0.04, σ=1.0 → χ≈-0.02. Piton classification from theater gate ≥0.70 (barely satisfied); the ceremonial structure survives through institutional inertia.
constraint_indexing:constraint_classification(regulatory_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, regulatory capture appears as an immutable consequence of collective action asymmetries: organized minorities (firms) always dominate disorganized majorities (consumers) in political processes. The Olsonian logic suggests capture is inherent to democratic regulation. However, structural data (ε=0.58, suppression=0.68, theater=0.65) contradicts mountain classification — the engine will compute false summit. Capture is contingent on institutional design choices (agency independence, conflict-of-interest rules, transparency mechanisms), not an inevitable law.
constraint_indexing:constraint_classification(regulatory_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint enables incumbents to extract rents through regulatory barriers (licensing, compliance costs, entry restrictions) that exceed the cost of legitimate market coordination. The value (not 0.70+) reflects that some regulatory functions are real (information asymmetry reduction, safety standards) and not purely extractive. The extraction is measurable: barriers to entry impose real costs on potential competitors and consumers. Suppression (0.68): High. Suppression operates through multiple channels: (1) complexity — regulatory compliance requires specialized expertise that favors large incumbent firms; (2) barriers to participation — public comment processes are ineffective against industry technical expertise; (3) asymmetric information — agencies capture data from industry, limiting outside scrutiny; (4) career risk — reformist agency staff face retaliation from industry allies. These are not absolute (whistleblowers exist, transparency advocates mobilize) but substantial. Theater ratio (0.65): High. Procedural legitimacy maintains democratic appearance: notice-and-comment rulemaking, advisory committees, public hearings. Yet functional decision-making is dominated by incumbent input. The regulatory process has become substantially performative — the ritual of public engagement persists while actual policy outcomes reflect industry preferences.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. Incumbent firms see coordination (Rope) — the agency provides stable rules and competitive protection. The general public sees pure extraction (Snare) — they have no voice and no exit. Competitive entrants see extraction with constrained options (Snare) — they can leave the market but cannot reform the system. The agency itself sees coordination (Rope) — the captured relationship solves its budgeting and mandate problems. Reform coalitions see mixed coordination and extraction (Tangled Rope) — some agency functions are legitimate but captured by incumbents. Supranational actors see moderate extraction with some mobility (Tangled Rope) — they can leverage trade pressure but are partly constrained by sovereignty norms. The piton perspective reveals performative legitimacy masking capture. The mountain perspective risks naturalizing capture as inevitable Olsonian logic, but structural data shows contingency: capture responds to agency design choices, not immutable laws.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent Regulated Firms: Beneficiary + arbitrage → d≈0.10, f(d)≈0.05. Net beneficiary. High exit options (relocate, arbitrage across jurisdictions) combined with direct influence (revolving door, technical input). Negative effective extraction. General Public: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Cannot exit regulatory regime; have no organized voice; bear full cost of capture through higher prices and suppressed innovation. Competitive Entrants: Victim + constrained → d≈0.85, f(d)≈1.15. High extraction. Can exit market but cannot reform agency; face barriers that exceed competitive returns. Reform Coalition: Victim + constrained + organized → d≈0.55, f(d)≈0.75. Moderate extraction. Have some organized voice and some exit options (legislative pressure, media campaigns, litigation) but face entrenched agency structure. Supranational Observer: Mixed status + mobile → d≈0.48, f(d)≈0.57. Moderate extraction. Have leverage through trade instruments but constrained by national sovereignty norms. Agency (Captured): Beneficiary + arbitrage → d≈0.05, f(d)≈-0.08. Net beneficiary. Capture solves agency's own coordination problems: stable funding from industry relationships, clear objectives, career paths aligned with industry.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY RESOLUTION: Regulatory capture is a tangled rope, not a snare, because it has a genuine (if perverted) coordination function — the captured agency still reduces information asymmetry about safety/quality and creates regulatory predictability. The agency's original mandate (consumer protection, fair competition) is subordinated to incumbent protection, but some coordination function persists. If the constraint were pure extraction (snare), it would have no beneficiary except the incumbent firm, and suppression would be total. But the constraint maintains procedural legitimacy, advisory structures, and (residual) coordination benefits — these mark tangled rope, not snare. The mandatrophy is resolved by recognizing that perspectives diverge based on structural position: from the powerless (general public) it appears as snare; from the institutional beneficiary (captured agency) it appears as rope; from the analytical observer it appears as mixed coordination-extraction. The type is not ambiguous — it is tangled rope from the system perspective — but the perspectival gap reveals that sandbagging of populations (powerless victims) under pretense of coordination (agency legitimacy) is the signature of hybrid extraction. Theater ratio elevation (0.40 → 0.65) indicates increasing gap between procedural performance and functional reality — classic piton drift toward pure theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_autonomy_threshold,
    'What degree of bureaucratic autonomy (budget control, appointment authority, transparency mandates) is necessary and sufficient to prevent capture?',
    'Comparative institutional analysis: regulatory outcomes across jurisdictions with different agency independence metrics; correlation between agency autonomy indicators and capture-free decision-making',
    'If threshold is achievable: scaffold perspective (capture as temporary problem) is validated. If threshold is too high: mountain perspective (capture as inevitable) dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_autonomy_threshold, empirical, 'Threshold of agency autonomy sufficient to prevent capture').

omega_variable(
    consumer_mobilization_feasibility,
    'Can consumer coalitions organize effectively enough to counterbalance incumbent industry mobilization in regulatory processes?',
    'Historical case studies of successful consumer intervention in regulatory proceedings; network analysis of advocacy organization effectiveness; longitudinal tracking of consumer-initiated rule changes',
    'If feasible: tangled rope from reform coalition perspective is sustainable (coordination function real). If not feasible: snare classification dominates all populace perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consumer_mobilization_feasibility, empirical, 'Feasibility of effective consumer mobilization in regulatory politics').

omega_variable(
    regulatory_arbitrage_containment,
    'Can multi-jurisdictional regulatory systems (federal/state, EU/national) prevent incumbent firms from forum-shopping to the most captured jurisdiction?',
    'Case studies of successful jurisdictional harmonization or forum-restriction mechanisms; analysis of firm behavior when facing conflicting regulatory regimes',
    'If containable: institutional/powerful agents'' mobile exit option is constrained, reducing their beneficiary status. If not containable: firms maintain arbitrage advantage, deepening capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_containment, empirical, 'Feasibility of preventing regulatory arbitrage by incumbent firms').

omega_variable(
    revolving_door_impact,
    'Does the revolving door between regulatory agencies and industry (staff movement, post-government consulting) structurally cause capture or merely select for already-aligned personnel?',
    'Causal analysis using policy discontinuities at staff transitions; measurement of decision changes when captured vs. non-aligned staff take key positions',
    'If causal: suppression gate is driven by personnel incentives (potentially addressable via structural change). If selection: suppression is deeper and requires cultural/institutional transformation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revolving_door_impact, empirical, 'Causal role of revolving door in regulatory capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcap_tr_t0, regulatory_capture, theater_ratio, 0, 0.4).
narrative_ontology:measurement(regcap_tr_t25, regulatory_capture, theater_ratio, 25, 0.52).
narrative_ontology:measurement(regcap_tr_t50, regulatory_capture, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(regcap_be_t0, regulatory_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(regcap_be_t25, regulatory_capture, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(regcap_be_t50, regulatory_capture, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_capture, information_asymmetry_financial_services).
narrative_ontology:affects_constraint(regulatory_capture, monopolistic_rent_extraction).
narrative_ontology:affects_constraint(regulatory_capture, revolving_door_conflict_of_interest).
narrative_ontology:affects_constraint(regulatory_capture, campaign_finance_influence).

% DUAL FORMULATION NOTE:
% Regulatory capture decomposes into multiple structurally distinct constraints: (1) agency autonomy failure (ε≈0.50, institutional design); (2) collective action asymmetry (ε≈0.65, political economy); (3) procedural theater (ε≈0.45, legitimacy maintenance); (4) revolving door incentives (ε≈0.55, personnel selection). This story models the integrated phenomenon; downstream constraints model specific mechanisms. All are linked by regulatory capture's affect on agency decision-making.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
