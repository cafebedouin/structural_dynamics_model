% ============================================================================
% CONSTRAINT STORY: standards_body_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_standards_body_capture, []).

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
 *   constraint_id: standards_body_capture
 *   human_readable: Standards Body Capture by Dominant Firms
 *   domain: economic/governance/technical_standards
 *
 * SUMMARY:
 *   Standards bodies — formal institutions like ISO, ITU, IEEE, 3GPP, and W3C
 *   — coordinate technical interoperability across industries and markets.
 *   They provide genuine coordination function: without standards, firms
 *   cannot interoperate, and fragmentation reduces network value for all.
 *   However, dominant technology firms systematically capture standards
 *   bodies to embed their proprietary designs into 'open' standards, extract
 *   licensing revenue from competitors and new entrants, and lock markets
 *   against disruption. The constraint exhibits characteristics of tangled
 *   rope: real coordination function (interoperability) paired with
 *   asymmetric extraction (patent licensing, voting control, agenda-setting).
 *   The theater_ratio (0.68) reflects elaborate procedural rituals — public
 *   comment periods, diverse committee representation, transparent voting —
 *   that create appearance of democratic process while actual influence
 *   concentrates through dominant firms' funding control, pre-committee
 *   consensus, and technical expertise dominance. The extractiveness
 *   trajectory (0.35→0.58 over the measurement interval) shows how capture
 *   accumulates: early standards capture lower rents as alternative standards
 *   remain viable; later extraction rises as the captured standard becomes
 *   mandatory for market participation.
 *
 * KEY AGENTS:
 *   - Dominant Technology Firms: Primary beneficiary (institutional/arbitrage) — control voting blocs, fund committee operations, embed proprietary designs, extract patent licensing revenue
 *   - Market Entrants (Startups/Regional Competitors): Primary victim (powerless/trapped) — must comply with captured standards, pay licensing fees, cannot influence technical direction
 *   - Consumer Welfare / Interoperability Access: Secondary victim (powerless/trapped) — benefits from nominal standardization but pays hidden extraction via licensing fees embedded in prices
 *   - Regional Standards Committees: Secondary actor (moderate/constrained) — benefit from interoperability coordination but constrained by dominant firm influence and resource dependence
 *   - Open Standards Coalition: Organized competitors (organized/constrained) — seek alternative standards, open-source implementations, and regulatory exits; building sunset pathways
 *   - Standards Body Apparatus: Institutional actor (institutional/arbitrage) — maintains procedural theater; benefits from membership fees and dominant firm funding; sees own process as degraded (piton perspective)
 *   - Regulatory Authorities: Institutional actor (institutional/mobile) — have capacity to mandate interoperability or override captured standards but often lack political will or technical understanding to exercise it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(standards_body_capture, 0.58).
domain_priors:suppression_score(standards_body_capture, 0.65).
domain_priors:theater_ratio(standards_body_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(standards_body_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(standards_body_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(standards_body_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(standards_body_capture, tangled_rope).
narrative_ontology:human_readable(standards_body_capture, "Standards Body Capture by Dominant Firms").
narrative_ontology:topic_domain(standards_body_capture, "economic/governance/technical_standards").

domain_priors:requires_active_enforcement(standards_body_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(standards_body_capture, dominant_technology_firms).
narrative_ontology:constraint_beneficiary(standards_body_capture, incumbent_standard_controllers).
narrative_ontology:constraint_victim(standards_body_capture, competitive_market_entrants).
narrative_ontology:constraint_victim(standards_body_capture, consumer_welfare).
narrative_ontology:constraint_victim(standards_body_capture, interoperability_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARKET ENTRANT (SNARE) — Startups and smaller firms cannot avoid standards compliance; they must adopt incumbent standards or face market exclusion. Patent licensing, royalty structures, and participation requirements extract value while offering no exit option. Trapped: compliance is mandatory, costs are asymmetric, and alternatives are unavailable or equally controlled.
constraint_indexing:constraint_classification(standards_body_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL STANDARDS COMMITTEE (TANGLED ROPE) — Mid-tier participants benefit from standardization (coordinating interoperability across regional markets) but are constrained by resource limitations and influence concentration. Dominant firms fund committee operations, control voting blocs, and set agenda priorities. Mixed extraction and coordination: genuine interoperability function exists alongside asymmetric influence.
constraint_indexing:constraint_classification(standards_body_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DOMINANT TECHNOLOGY FIRM (ROPE) — Experiences the standards body as pure coordination mechanism: aligning their proprietary ecosystem with an open standard creates lock-in and cross-licensing revenue. Can exit (develop proprietary standard) but arbitrage value (controlling open standard definitions) makes exit irrational. Net beneficiary — extraction flows toward this actor.
constraint_indexing:constraint_classification(standards_body_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN STANDARDS COALITION (SCAFFOLD) — Organized actors (open-source communities, consumer advocacy, competitors' consortia) see the capture as a temporary institutional failure with structural exits: alternative standards bodies (IETF vs ITU historical competition), open-source implementations that ignore patents (Linux bypassing cellular patent pools), and regulation (EU interoperability mandates). Low effective extraction because coalition has agency and sunset paths.
constraint_indexing:constraint_classification(standards_body_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: STANDARDS BODY APPARATUS (PITON) — The formal standards body (ISO, ITU, IEEE, 3GPP) maintains elaborate procedural rituals (voting rules, technical committees, public comment periods) that appear to democratize but are largely performative. Dominant firms have already aligned on the technical direction through pre-committee consensus; formal votes ratify decisions made in private. Theater persists because alternatives haven't fully replaced it, not because it functions. High theater_ratio reflects gap between procedural appearance and actual influence distribution.
constraint_indexing:constraint_classification(standards_body_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some concentration of standard-setting power is inevitable: technical complexity requires expertise, expertise concentrates resources, and dominant firms have the most resources. This view naturalizes capture as an inescapable feature of complex systems governance. However, this is a false summit — the structural data shows that capture is contingent on regulatory architecture, not inherent to standardization itself.
constraint_indexing:constraint_classification(standards_body_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(standards_body_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(standards_body_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(standards_body_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(standards_body_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(standards_body_capture, TR),
    TR >= 0.70.

:- end_tests(standards_body_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Dominant firms extract value through multiple mechanisms: patent licensing royalties on competing implementations, ability to bundle proprietary services with standards-compliant products, and market exclusion of non-compliant entrants. However, extraction is not total (0.75+) because: (1) some standardization genuinely benefits dominators (interoperability increases market size), (2) open-source implementations sometimes work around patent pools, and (3) regulators occasionally mandate interoperability, reducing extraction. The trajectory shows accumulation: as the captured standard becomes mandatory for market participation, extraction increases. Suppression (0.65): High. Barriers to escaping the captured standard include network effects (switching costs increase with installed base), patent licensing requirements (essential patents are mandatory to license), technical complexity (competing with dominant-firm-embedded designs requires equivalent resources), and market concentration (few alternatives exist for many critical standards). Suppression is not total (0.80+) because regulatory intervention and alternative standards bodies offer bounded exits. Theater ratio (0.68): High. The standards body maintains elaborate procedural appearance — public comment periods, diverse national representation, technical committee structures, transparent voting — while actual influence concentrates through three mechanisms: (1) dominant firms fund committee operations and travel, (2) proprietary technical details and patent portfolios are pre-aligned in private before formal votes, (3) voting rules favor established players. The theater has increased over time as procedures have become more elaborate while actual decision-making has moved further into private pre-committee alignment.
 *
 * PERSPECTIVAL GAP:
 *   Why do the perspectives diverge so sharply? The dominant firm's rope classification reflects their genuine experience: the constraint solves coordination problems (reducing negotiation overhead), creates revenue opportunities (licensing essential patents), and builds switching costs (lock-in). From their position, extraction is minimal or directed toward them as beneficiaries. The market entrant's snare classification reflects equal structural reality: they face mandatory compliance (cannot exit), asymmetric costs (licensing fees they must pay but dominant firms avoid), and no beneficial coordination (the standard was designed before they entered, optimized for incumbents). Neither perspective is wrong — both are measuring the same constraint from structurally incompatible positions. The gap reveals the constraint's mixed character (tangled rope) precisely because no single classification fits all perspectives. If all perspectives produced snare, the constraint would be pure extraction. If all produced rope, it would be pure coordination. The gap between rope (beneficiary) and snare (victim) is diagnostic of asymmetric extraction layered onto coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to extraction flow. Dominant firms are beneficiaries with arbitrage options (can exit by creating proprietary standards but arbitrage value makes exit irrational) → low d → negative or minimal chi. Market entrants are victims with trapped status (must comply, cannot escape, cannot influence) → high d → high chi. Moderate-power regional committees are mixed (benefit from coordination but constrained by influence asymmetry) → mid-range d → moderate chi. The analytical perspective uses the civilization-scale scope that tends toward naturalizing institutional arrangements as laws, creating the false summit risk. The canonical d derivation applies: beneficiary + arbitrage → d≈0.05, victim + trapped → d≈0.95, moderate + constrained → d≈0.65. These map to f(d) values that modulate the experienced extractiveness. The directionality overrides are not needed — the structural data (beneficiaries, victims, exit options) produces accurate d values through the standard derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy through explicit declaration of both beneficiary (dominant_technology_firms) and victim (market_entrants, consumer_welfare) groups in base_properties. The tangled_rope classification requires three gates: (1) beneficiaries present (✓ dominant technology firms benefit from coordination and licensing), (2) victims present (✓ entrants bear asymmetric extraction costs), (3) requires_active_enforcement true (✓ patent licensing, voting rule enforcement, committee operations require ongoing institutional maintenance). The analytical perspective (mountain) is flagged as a false summit by the constraint compiler because the structural data contradicts the natural law framing: capture results from policy choices (patent licensing regimes, funding models, voting architectures) not from physical or logical limits. The mandatrophy is resolved by the perspectival multiplicity itself: the constraint is legitimately different types from different positions, and this polymorphism is the signature of tangled rope (asymmetric extraction hidden within coordination function). The false summit mountain perspective is explanatory — it shows how dominant actors naturalize contingent institutional arrangements — but it is explicitly rejected by the engine's false natural law detector.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    patent_licensing_necessity,
    'Are patent restrictions on standard implementations inherent to innovation incentives or contingent policy choice?',
    'Comparative analysis of standards with and without patent pools (e.g., WiFi with mandatory licensing vs Linux kernel without patent enforcement); measurement of innovation rates in each regime',
    'If inherent: extraction is justified cost of innovation (rope classification valid). If contingent: extraction is policy choice that could be reversed (snare classification confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patent_licensing_necessity, preference, 'Whether patent licensing in standards is necessary or chosen').

omega_variable(
    dominant_firm_necessity_for_coordination,
    'Could standards bodies achieve the same interoperability coordination with equal participant power distribution, or does dominance by coordinating firms reduce friction?',
    'Historical case analysis of standards developed by truly distributed consortia (e.g., W3C design vs WLAN standards design); measurement of time-to-completion, participant satisfaction, technical quality',
    'If dominance reduces friction: coordination function genuinely benefits from concentration (rope from moderate perspective valid). If distributed models work equally well: dominance is pure extraction (snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dominant_firm_necessity_for_coordination, empirical, 'Whether dominant firm coordination is necessary for standards efficiency').

omega_variable(
    alternative_standards_viability,
    'Do open-standards coalitions and competitors'' alternative standards bodies represent credible exit paths, or are they capturing subordinate segments that ultimately depend on dominant standards?',
    'Market share tracking of alternative standards; analysis of whether alternative implementations achieve meaningful interoperability without licensing dominant-firm patents; customer switching costs if alternative standards gain adoption',
    'If alternatives are viable: exit options are genuinely mobile/constrained for many actors (scaffold perspective valid). If alternatives fail: no real exit (snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_standards_viability, empirical, 'Whether alternative standards bodies offer genuine exits').

omega_variable(
    regulatory_capture_feedback,
    'Does standards body capture enable dominant firms to shape regulation in their favor, or are regulators genuinely independent?',
    'Analysis of regulatory filings referencing standards; tracking of whether regulators adopt standards that benefit dominant firms vs competing interests; examination of when regulators override or mandate alternative standards',
    'If feedback loop exists: capture is amplified across institutions (snare tightens). If regulators are independent: capture is bounded to standards body (scaffold perspective viable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_feedback, empirical, 'Whether standards capture amplifies through regulatory feedback').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(standards_body_capture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stdbody_tr_t0, standards_body_capture, theater_ratio, 0, 0.5).
narrative_ontology:measurement(stdbody_tr_t5, standards_body_capture, theater_ratio, 5, 0.62).
narrative_ontology:measurement(stdbody_tr_t10, standards_body_capture, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(stdbody_be_t0, standards_body_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stdbody_be_t5, standards_body_capture, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(stdbody_be_t10, standards_body_capture, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(standards_body_capture, information_standard).
narrative_ontology:affects_constraint(standards_body_capture, patent_pool_licensing).
narrative_ontology:affects_constraint(standards_body_capture, regulatory_capture_feedback).
narrative_ontology:affects_constraint(standards_body_capture, open_source_technology_sustainability).

% DUAL FORMULATION NOTE:
% Standards body capture is downstream of specific technical standardization decisions (WiFi, cellular, USB, etc.) but represents a distinct structural constraint on the governance mechanisms themselves. Each technical standard has its own extractiveness reflecting the empirical control distribution; standards body capture represents the meta-level constraint that enables such control to accumulate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
