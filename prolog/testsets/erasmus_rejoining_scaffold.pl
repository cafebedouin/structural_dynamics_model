% ============================================================================
% CONSTRAINT STORY: erasmus_rejoining_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_erasmus_rejoining_scaffold, []).

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
 *   constraint_id: erasmus_rejoining_scaffold
 *   human_readable: UK's potential re-entry into the EU Erasmus+ student exchange program
 *   domain: political/education_policy
 *
 * SUMMARY:
 *   The UK's potential re-entry into the EU Erasmus+ student exchange program
 *   represents a classic Scaffold constraint: a temporary institutional
 *   restoration designed to bridge the gap between the old integrated regime
 *   (pre-Brexit) and a new permanent architecture (bilateral science
 *   agreements, independent student visa frameworks, or renegotiated full
 *   participation). The constraint exhibits low extractiveness (0.28) and
 *   moderate theater ratio (0.52) because the functional output — actual
 *   student mobility — is genuine and directly benefits all parties. The
 *   sunset clause is explicit: re-entry is framed as transitional, with the
 *   expectation that either (a) full institutional integration is restored
 *   within 5-10 years, or (b) alternative bilateral frameworks mature enough
 *   to replace the need for participation in an EU program. Theater has
 *   declined over the measurement interval (0.65 → 0.52) as the political
 *   controversy around re-entry has subsided and the program has shifted from
 *   symbolic gesture to operational coordination. Extractiveness has risen
 *   slightly (0.18 → 0.28) as negotiation has exposed the actual
 *   institutional costs of re-entry (visa regime management, funding
 *   adjustments, regulatory alignment), but remains low because these costs
 *   are distributed and proportional rather than asymmetric.
 *
 * KEY AGENTS:
 *   - UK University Sector: Primary beneficiary (organized/constrained) — experiences re-entry as restoration of coordination mechanism with constrained exit and low theater
 *   - UK Students: Primary beneficiary (moderate/constrained) — direct users of student mobility; constrained by visa and funding frameworks
 *   - UK Government / Department for Education: Hybrid actor (institutional/constrained) — benefits from coordination and cultural exchange but also extracts through immigration policy leverage
 *   - EU Commission and Member States: Secondary beneficiary (institutional/arbitrage) — symmetric interest in student mobility; high exit options; low extraction incentive
 *   - EU Partner Institutions: Tertiary beneficiary (institutional/arbitrage) — benefit from UK student inflow and cultural exchange without bearing extraction
 *   - Bilateral Science Agreement Framework: Degraded alternative (institutional/constrained) — represents piton state before re-entry; maintenance requires political will despite lower functional coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(erasmus_rejoining_scaffold, 0.28).
domain_priors:suppression_score(erasmus_rejoining_scaffold, 0.35).
domain_priors:theater_ratio(erasmus_rejoining_scaffold, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(erasmus_rejoining_scaffold, extractiveness, 0.28).
narrative_ontology:constraint_metric(erasmus_rejoining_scaffold, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(erasmus_rejoining_scaffold, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(erasmus_rejoining_scaffold, scaffold).
narrative_ontology:human_readable(erasmus_rejoining_scaffold, "UK's potential re-entry into the EU Erasmus+ student exchange program").
narrative_ontology:topic_domain(erasmus_rejoining_scaffold, "political/education_policy").

domain_priors:requires_active_enforcement(erasmus_rejoining_scaffold).
narrative_ontology:has_sunset_clause(erasmus_rejoining_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(erasmus_rejoining_scaffold, uk_university_sector).
narrative_ontology:constraint_beneficiary(erasmus_rejoining_scaffold, uk_students).
narrative_ontology:constraint_beneficiary(erasmus_rejoining_scaffold, eu_partner_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UK UNIVERSITIES (SCAFFOLD) — Organized actors (Russell Group, university leadership, student unions) experienced exclusion from Erasmus+ as a temporary institutional rupture. Re-entry represents negotiated restoration of a coordination mechanism with genuine sunset logic: the scaffolding is the transition period during which bilateral and alternative exchange agreements (like the new UK-EU Science and Innovation Partnership) are being built. Universities have constrained exit (cannot unilaterally rejoin without government negotiation) but can organize through sector bodies. The constraint exhibits low theater — actual student mobility is the functional output, not performative compliance. Extractiveness is low because universities benefit from program participation without significant asymmetric extraction.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: UK GOVERNMENT (TANGLED ROPE) — The UK government has both coordination benefits (restoring student mobility, maintaining cultural exchange, signaling openness to European cooperation) and extraction mechanisms (ability to set visa conditions for EU students, use participation as diplomatic leverage, negotiate preferential terms). Constrained exit — cannot unilaterally withdraw without diplomatic costs once re-entry is negotiated. Active enforcement required: government must maintain participation agreements and student visa frameworks. This agent experiences genuine hybrid coordination-extraction rather than pure coordination, particularly around immigration policy leverage.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EU INSTITUTIONS (ROPE) — The EU Commission and member states experience Erasmus+ re-entry as pure coordination: mutual student mobility, cultural exchange, and soft power benefits. High exit options (arbitrage) — the EU can include or exclude the UK without existential consequence. Low extraction — the program's architecture is symmetric by design; no single party gains disproportionate advantage. Extractiveness appears neutral from this perspective because the EU is the institutional beneficiary-designer of the program, not a victim of extraction.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIVIDUAL STUDENTS (SCAFFOLD) — Students in the UK and EU experience the re-entry as a temporary restoration of mobility they previously took for granted. The constraint is the transition period during which visa rules, funding arrangements, and recognition of qualifications are being renegotiated. Constrained exit — students cannot unilaterally access the program without government-level agreements. Low extractiveness: once re-entry is negotiated, students directly benefit without paying hidden costs. The scaffolding is the sunset clause built into the re-entry process: once new frameworks stabilize, the temporary institutional arrangement (negotiated re-entry) becomes permanent normal operation, and the constraint as such dissolves.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: BILATERAL ALTERNATIVES (PITON) — The UK-EU Science and Innovation Partnership (Horizon Europe association, bilateral research funding agreements) represents a degraded, partial substitute for full Erasmus+ participation. These agreements maintain some mobility and collaboration but lack Erasmus+'s scale and integration. Theater ratio is high (0.60-0.70) — bilateral agreements require repeated political negotiation and institutional maintenance even though their functional coordination is lower than the original program. This perspective sees Erasmus+ re-entry as replacing piton with genuine scaffold: moving from performative bilateral alternatives back to the integrated program.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, Erasmus+ re-entry is entangled with broader geopolitical repositioning: UK-EU integration on youth mobility has symbolic and strategic value in the context of great-power competition and Western institutional cohesion. The constraint exhibits both genuine coordination (student mobility is Pareto-improving for both UK and EU) and asymmetric extraction potential (the UK could use participation as a negotiating wedge for other trade benefits; the EU could restrict terms to extract diplomatic concessions). Extractiveness is moderate (0.28) because while extraction potential exists, the actual program design minimizes it. Active enforcement required because geopolitical pressures could shift participation from coordination toward extraction.
constraint_indexing:constraint_classification(erasmus_rejoining_scaffold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(erasmus_rejoining_scaffold_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(erasmus_rejoining_scaffold, TR),
    TR >= 0.70.

:- end_tests(erasmus_rejoining_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The Erasmus+ program is designed for symmetric coordination, and UK re-entry does not alter this fundamental architecture. Extractiveness is not zero because: (1) the UK government can use visa policy as a lever to extract concessions on other trade issues; (2) universities in more prestigious locations may attract disproportionate student flows; (3) the EU retains veto power over UK participation. However, these extraction potentials are structural design features minimized by the program's explicit coordination mandate. Suppression (0.35): Moderate. Students face visa barriers, funding complexity, and language/credential recognition requirements. Universities face regulatory alignment costs and institutional coordination overhead. But suppression is not severe because the program is legally negotiated and participation is voluntary; actors can and do exit if barriers become prohibitive. Theater ratio (0.52): Moderate. Re-entry negotiations involve significant political theater (parliamentary debates, sovereignty framing, symbolic statements about UK-EU relations), but the functional output is genuine student mobility. Theater has declined because the initial Brexit-era controversy has subsided and the program has shifted to technical implementation. Claimed type (Scaffold): The constraint exhibits all three defining features: (1) sunset clause — explicitly framed as temporary restoration pending new frameworks; (2) low theater — functional output is real; (3) declining suppression trajectory — as visa regimes and funding align, institutional barriers decline.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is between agents who experience re-entry as restoration of symmetric coordination (UK universities, EU institutions) and agents who experience it as politically contingent scaffolding with geopolitical leverage potential (UK government, analytical observer). UK universities see genuine Rope: the program solves a coordination problem with minimal coercion. The UK government sees Tangled Rope: coordination benefits exist, but immigration policy leverage enables asymmetric extraction. The EU sees Rope: symmetric student mobility benefits, arbitrage exit options, no extraction incentive. The analytical observer sees Tangled Rope: the constraint entangles genuine coordination with geopolitical positioning and provides both parties with extraction optionality if political relationships deteriorate. The bridging perspective is Students: they experience the constraint as Scaffold — a temporary institutional restoration that will dissolve once new frameworks stabilize.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (UK universities, UK students, EU institutions) experience low directionality (d ≈ 0.15-0.25) because they derive direct benefits from program participation without bearing asymmetric costs. The UK government experiences moderate directionality (d ≈ 0.40) because it benefits from coordination but also retains extraction options through visa policy. The analytical observer experiences high directionality (d ≈ 0.65) because geopolitical contingency creates asymmetric risk exposure — the constraint's stability depends on maintained cooperation, and either party can weaponize participation as leverage. The engine derives these values from the beneficiary/victim declarations (none here; the constraint is purely coordinative) and the exit options: actors with arbitrage options (EU) experience lower d; actors with constrained options (UK universities, government) experience higher d. No directionality overrides are needed because the structural derivation accurately captures the asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by clearly declaring beneficiaries (UK universities, students, EU institutions) and no victims. The absence of a victim group distinguishes this from Snare or Tangled Rope. The sunset clause distinguishes this from pure Rope — pure Rope has indefinite duration; Scaffold explicitly includes a sunset mechanism. The moderate theater ratio (0.52) and declining trajectory rules out Piton (theater_ratio ≥ 0.70). Extractiveness (0.28) is above pure Rope threshold (≤0.45) but the constraint is classified as Scaffold, not Rope, because the sunset clause and temporary institutional framing are the defining features, not the extractiveness value. The classification thus resolves the potential mandatrophy of 'is this coordination (Rope) or hybrid coordination-extraction (Tangled Rope)?' by answering: it is pure coordination (Rope properties) but with explicit sunset, making it Scaffold. The UK government's extraction optionality (via visa policy) is structural but dormant — it is not currently activated, and the program design minimizes incentive to activate it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    visa_regime_independence,
    'Will UK immigration policy remain independent enough to avoid using Erasmus+ participation as leverage for restrictive visa conditions on EU students?',
    'Comparative analysis of visa issuance rates and processing times for EU students pre-Brexit vs post-re-entry; tracking of explicit linkage between Erasmus+ negotiations and broader migration policy',
    'If UK maintains independent visa policy: constraint remains Scaffold/Rope. If UK uses visa restrictions as extraction mechanism: constraint degrades to Tangled Rope or Snare from EU perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(visa_regime_independence, empirical, 'Whether UK immigration policy remains independent from Erasmus+ leverage').

omega_variable(
    sunset_timeline_realism,
    'Is the 5-10 year sunset timeline for full re-integration realistic, or will Erasmus+ remain a politically contingent scaffolding indefinitely?',
    'Tracking of bilateral agreement maturity, institutional automation of student recognition, and political rhetoric around ''permanent re-entry'' vs ''negotiated participation''',
    'If sunset achieves: Scaffold classification confirmed. If timeline extends indefinitely: constraint degrades to Piton (performative re-entry without genuine integration).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_timeline_realism, empirical, 'Whether re-entry sunset timeline is realistically achievable').

omega_variable(
    mobility_symmetry_maintenance,
    'Will student mobility flows remain symmetric (equal numbers of UK and EU students exchanging) or will asymmetries emerge that favor one direction?',
    'Longitudinal data on student mobility direction; analysis of funding incentives and labor market attractiveness; tracking of destination choice patterns',
    'If symmetric: Rope classification from EU perspective confirmed. If asymmetric (e.g., more UK outflow): constraint may exhibit extractive properties for the receiving institution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mobility_symmetry_maintenance, empirical, 'Whether student mobility flows remain symmetric post-re-entry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(erasmus_rejoining_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eras_tr_t0, erasmus_rejoining_scaffold, theater_ratio, 0, 0.65).
narrative_ontology:measurement(eras_tr_t3, erasmus_rejoining_scaffold, theater_ratio, 3, 0.58).
narrative_ontology:measurement(eras_tr_t7, erasmus_rejoining_scaffold, theater_ratio, 7, 0.52).

% Extraction over time
narrative_ontology:measurement(eras_be_t0, erasmus_rejoining_scaffold, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(eras_be_t3, erasmus_rejoining_scaffold, base_extractiveness, 3, 0.24).
narrative_ontology:measurement(eras_be_t7, erasmus_rejoining_scaffold, base_extractiveness, 7, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(erasmus_rejoining_scaffold, information_standard).
narrative_ontology:affects_constraint(erasmus_rejoining_scaffold, uk_eu_trade_and_cooperation_agreement).
narrative_ontology:affects_constraint(erasmus_rejoining_scaffold, bilateral_science_innovation_partnership).

% DUAL FORMULATION NOTE:
% UK Erasmus+ re-entry is downstream of the broader UK-EU relationship and the Trade and Cooperation Agreement (TCA) framework. It is also partially displaced by the bilateral Science and Innovation Partnership, which serves as a piton-state alternative. These three constraints form a family: the TCA establishes the political framework (higher ε, potential extraction leverage), bilateral agreements represent degraded substitutes (piton with high theater), and Erasmus+ re-entry is the scaffold that may eventually integrate fully. Each has distinct ε values reflecting their different temporal horizons and extraction mechanics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
