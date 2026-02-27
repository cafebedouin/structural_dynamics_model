% ============================================================================
% CONSTRAINT STORY: msgs_asset_bundling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_msgs_asset_bundling, []).

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
 *   constraint_id: msgs_asset_bundling
 *   human_readable: Bundled Ownership of Knicks and Rangers under MSG Sports
 *   domain: economic/corporate_structure
 *
 * SUMMARY:
 *   Madison Square Garden Sports (MSGS) is the parent company of the New York
 *   Knicks (NBA) and New York Rangers (NHL), trading as a single publicly
 *   listed entity under the control of the Dolan family. This bundled
 *   ownership structure exemplifies a hybrid constraint combining genuine
 *   coordination benefits (simplified governance, consolidated venue
 *   management) with structural extraction mechanisms (suppressed price
 *   discovery, minority shareholder lock-in, cross-subsidy opacity). The
 *   constraint exhibits different classifications from the perspectives of
 *   powerless minority shareholders (Snare), institutional investors (Tangled
 *   Rope), the Dolan family (Rope), sports antitrust reformers (Scaffold with
 *   sunset clause), and the analytical observer evaluating capital market
 *   efficiency (Tangled Rope). The theater ratio (0.54) reflects that the
 *   bundling claim rests partially on operational synergy arguments that are
 *   not transparent—shared arena management is real, but whether it justifies
 *   the franchise-level coupling is contestable. The extractiveness
 *   trajectory (0.35 → 0.52 over the interval) indicates that as regulatory
 *   scrutiny has increased and minority shareholder protections have
 *   improved, the constraint's extraction mechanism has become more explicit
 *   and harder to justify as pure coordination.
 *
 * KEY AGENTS:
 *   - Dolan Family: Controlling shareholder (institutional/arbitrage) — primary beneficiary, retains unilateral control despite public listing
 *   - Minority Shareholders: Public equity holders (powerless/trapped) — forced to maintain bundled exposure, cannot selectively divest
 *   - Institutional Investors: Asset managers, pension funds (powerful/mobile) — benefit from consolidated portfolio option but constrained by inability to optimize individual exposures
 *   - Individual Fans / Local Market Consumers: Regional market participants (powerless/constrained) — geographic monopoly forces funding of both franchises through integrated pricing
 *   - Sports Antitrust Reformers: Congressional, state-level advocates (organized/constrained) — building sunset clause through regulatory pressure
 *   - Sports Industry Legacy System: Historical ownership patterns (institutional/arbitrage) — bundling persists through path-dependent momentum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(msgs_asset_bundling, 0.52).
domain_priors:suppression_score(msgs_asset_bundling, 0.68).
domain_priors:theater_ratio(msgs_asset_bundling, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(msgs_asset_bundling, extractiveness, 0.52).
narrative_ontology:constraint_metric(msgs_asset_bundling, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(msgs_asset_bundling, theater_ratio, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(msgs_asset_bundling, tangled_rope).
narrative_ontology:human_readable(msgs_asset_bundling, "Bundled Ownership of Knicks and Rangers under MSG Sports").
narrative_ontology:topic_domain(msgs_asset_bundling, "economic/corporate_structure").

domain_priors:requires_active_enforcement(msgs_asset_bundling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(msgs_asset_bundling, dolan_family).
narrative_ontology:constraint_beneficiary(msgs_asset_bundling, institutional_investors).
narrative_ontology:constraint_victim(msgs_asset_bundling, minority_shareholders).
narrative_ontology:constraint_victim(msgs_asset_bundling, competitive_sports_market).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY SHAREHOLDERS (SNARE) — Cannot exit without accepting substantial losses. Bundled structure forces investment in both franchises simultaneously, eliminating the option to selectively divest underperforming asset. No mechanism for separating the Knicks' asset value from the Rangers' operational costs. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(msgs_asset_bundling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIVIDUAL SPORTS FANS / LOCAL MARKET CONSUMERS (SNARE) — Cannot choose to fund only the Knicks or Rangers through ticket/merchandise purchases; bundled organizational structure inflates operational overhead. Revenue from one franchise subsidizes inefficient management in the other. Limited ability to exit — geographic monopoly on premier arena (Madison Square Garden). d≈0.88, f(d)≈1.32, σ=0.9 → χ≈0.62.
constraint_indexing:constraint_classification(msgs_asset_bundling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL INVESTORS / ASSET MANAGERS (TANGLED ROPE) — Benefit from consolidated revenue streams and simplified portfolio management (single ticker instead of two). Experience bundling as coordination mechanism enabling larger institutional positions. But also victimized by inability to optimize exposure: may want Knicks valuation without Rangers operational drag, or vice versa. d≈0.52, f(d)≈0.65, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(msgs_asset_bundling, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: DOLAN FAMILY / CONTROLLING SHAREHOLDERS (ROPE) — Primary beneficiary. Bundled structure consolidates voting control, reduces dilution of ownership across two franchises, simplifies tax/debt structures. Can arbitrage between franchise valuations, cross-subsidize underperforming assets, and maintain unified brand identity ('MSG Sports'). Retains unilateral operational control despite public stock listing. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(msgs_asset_bundling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SPORTS ANTITRUST REFORMERS / REGULATORY ADVOCATES (SCAFFOLD) — Organized agents (Congress, state legislatures, antitrust coalitions) increasingly scrutinize sports franchise ownership structures as artificial scarcity mechanisms. The bundled structure appears as a temporary coordination arrangement with an implicit sunset: if antitrust enforcement expands or franchise separation becomes mandatory, the bundling enforces a transition. Theater ratio remains moderate (0.54) because bundling is not performative—it has real operational effects—but its legitimacy is decaying. d≈0.45, f(d)≈0.50, σ=1.0 → χ≈0.27. Organized challenge to the structure's durability.
constraint_indexing:constraint_classification(msgs_asset_bundling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SPORTS INDUSTRY LEGACY SYSTEM (PITON) — From the viewpoint of historical franchise ownership patterns, bundling appears as vestigial institutional inertia. Historically, wealthy individuals owned single franchises; Dolan's model (consolidating NBA/NHL under one parent) was an innovation but is now maintained through path-dependent momentum rather than functional necessity. Theater_ratio=0.54 sits below piton threshold (0.70), but the theatrical elements (branding claims about 'integrated sports management') exceed functional gains. The constraint persists because alternatives haven't fully displaced it.
constraint_indexing:constraint_classification(msgs_asset_bundling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CAPITAL MARKETS EFFICIENCY (TANGLED ROPE) — From a civilizational view of market structure, bundling creates both coordination and extraction. Coordination: single governance simplifies investor decision-making and reduces transaction costs. Extraction: bundling suppresses price discovery, inflates the Knicks' valuation (proxy for prestige), and subsidizes Rangers performance with Knicks revenue, masking operational inefficiency. The constraint serves the market-concentration logic of the controlling shareholder at the cost of allocative efficiency. d≈0.65, f(d)≈1.00, σ=1.2 → χ≈0.52.
constraint_indexing:constraint_classification(msgs_asset_bundling, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(msgs_asset_bundling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(msgs_asset_bundling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(msgs_asset_bundling, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(msgs_asset_bundling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(msgs_asset_bundling, TR),
    TR >= 0.70.

:- end_tests(msgs_asset_bundling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The bundled structure creates real barriers to minority shareholder exit and distorts price discovery. The Dolan family captures value through consolidated control that they could not achieve under separated franchises. However, extractiveness is not extreme (would be 0.65+) because operational synergies are real (shared arena, unified governance), and institutional investors do benefit from simplified portfolio access. Suppression (0.68): High. The bundling suppresses alternative ownership structures, investor choice (cannot buy only Knicks or only Rangers), and transparent franchise valuation. Minority shareholders cannot exit without accepting losses; fans cannot allocate spending to preferred franchise without supporting underperforming asset. No regulatory mandate yet enforces separation, creating de facto monopoly on these assets under MSGS. Theater ratio (0.54): Moderate. The bundling narrative emphasizes operational synergy and 'integrated sports management,' but underlying transparency is limited. Financial reports do not break down franchise-level ROI or cross-subsidy flows, creating theater around efficiency claims. Yet the constraint is not primarily performative—it has real operational and financial effects—so theater is moderate rather than high.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. The Dolan family sees pure Rope: consolidated governance is coordination, reducing transaction costs and enabling unified branding. Institutional investors see Tangled Rope: they benefit from the single ticker but lose the ability to optimize exposure, experiencing both coordination and constraint. Minority shareholders see Snare: trapped in bundled ownership, unable to exit selectively, bearing hidden costs of Dolan's cross-subsidy decisions. Sports antitrust reformers see Scaffold: the bundling is temporary, increasingly fragile under regulatory scrutiny, with an implicit sunset clause as antitrust enforcement tightens. The analytical observer sees Tangled Rope at the civilizational scale: market efficiency is distorted by the controlling shareholder's ability to suppress price discovery and maintain artificial scarcity. No single perspective is 'wrong'—the presheaf structure captures how different positions in the constraint experience fundamentally different realities.
 *
 * DIRECTIONALITY LOGIC:
 *   Dolan family: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary; derives controlling leverage impossible under separate ownership. Minority shareholders: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; cannot exit, cannot optimize. Institutional investors: Mixed (beneficiary of consolidated access + victim of constraint on optimization) + mobile → d≈0.52, f(d)≈0.65. Significant extraction but with exit optionality. Individual fans: Victim + constrained → d≈0.88, f(d)≈1.32. Geographic monopoly prevents exit. Antitrust reformers: Organized + constrained → d≈0.45, f(d)≈0.50. Coalition has agency to change the structure but not yet sufficient power. Analytical observer: d≈0.65, f(d)≈1.00. Sees the full extraction mechanism but also real operational coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the bundled structure is NOT pure coordination (ruling out simple Rope) and NOT pure extraction (ruling out simple Snare). The Dolan family genuinely benefits from consolidation (coordination function present), but they also suppress alternatives for minority shareholders (extraction present). Both functions are structural, not observational artifacts. The Scaffold perspective identifies a real mechanism for mandatrophy resolution: regulatory pressure to unbundle. If antitrust enforcement mandates separation within 10-15 years, the current structure transitions from Tangled Rope to Scaffold (temporary with sunset). If regulation fails to materialize, the structure stabilizes as permanent Tangled Rope or drifts toward Snare as minority shareholder pressure increases. The theater ratio's increase over the interval (0.42 → 0.54) indicates that operational efficiency claims are becoming less credible relative to the control benefits the bundling provides to the family—classical Goodhart drift where the coordination narrative mask is wearing thin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    franchise_valuation_independence,
    'Does the bundled structure artificially inflate the Knicks'' franchise valuation by coupling it with the Rangers'' revenue base, and would separate valuations reveal material differences in underlying asset quality?',
    'Comparable transaction analysis comparing separately-held NBA franchises (Lakers, Warriors, Heat) and NHL franchises (Devils, Stars) to proxy valuations; spin-off announcement and market reaction analysis; investor surveys on willingness-to-pay for separate vs bundled exposure',
    'If Knicks overvalued by bundling: extractiveness rises to 0.62+ (shareholders bear hidden Knicks premium). If valuations are independent: bundling is neutral, classification shifts toward pure Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(franchise_valuation_independence, empirical, 'Whether bundling artificially inflates Knicks valuation').

omega_variable(
    antitrust_enforcement_timeline,
    'Will regulatory pressure to unbundle sports franchises materialize within the next 10 years (franchise separation mandate, cross-ownership restrictions)?',
    'Congressional antitrust subcommittee activity; state-level sports franchise regulation; FTC/DOJ litigation against other bundled structures; international precedents (European sports regulation)',
    'If mandate emerges: Scaffold classification confirmed, sunset is structural. If no regulatory change: bundling persists indefinitely, Tangled Rope / Rope classification stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antitrust_enforcement_timeline, empirical, 'Whether antitrust enforcement will mandate franchise separation').

omega_variable(
    operational_synergy_reality,
    'Do consolidated NBA/NHL operations under MSGS produce genuine cost savings or operational efficiencies, or is the bundling primarily a financial engineering mechanism with minimal operational integration?',
    'Detailed operating expense analysis; comparison of MSGS overhead ratios vs separately-operated franchises; employee survey on integration; shared resource utilization metrics (arena, analytics, front office)',
    'If genuine synergies exist: bundling is legitimate coordination, χ decreases. If pure financial engineering: bundling is extraction mechanism, χ increases to 0.58+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_synergy_reality, empirical, 'Whether operational synergies justify bundled structure').

omega_variable(
    controlling_shareholder_extraction_magnitude,
    'How much economic value does the Dolan family extract annually through bundled structure (tax arbitrage, dividend flow-through, cross-subsidy control) compared to a hypothetical separated ownership model?',
    'Forensic accounting analysis of dividend patterns, inter-company transfer pricing, tax deduction clustering; comparable family office structures; historical unbundling precedents (Berkshire Hathaway case study)',
    'If extraction > 5% EBITDA annually: χ ≥ 0.55, Snare classification gains traction. If extraction < 1%: χ < 0.40, Rope classification strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(controlling_shareholder_extraction_magnitude, empirical, 'Magnitude of family extraction through bundled control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(msgs_asset_bundling, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(msgs_tr_t0, msgs_asset_bundling, theater_ratio, 0, 0.42).
narrative_ontology:measurement(msgs_tr_t5, msgs_asset_bundling, theater_ratio, 5, 0.48).
narrative_ontology:measurement(msgs_tr_t10, msgs_asset_bundling, theater_ratio, 10, 0.54).

% Extraction over time
narrative_ontology:measurement(msgs_be_t0, msgs_asset_bundling, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(msgs_be_t5, msgs_asset_bundling, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(msgs_be_t10, msgs_asset_bundling, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(msgs_asset_bundling, resource_allocation).
narrative_ontology:affects_constraint(msgs_asset_bundling, sports_franchise_scarcity).
narrative_ontology:affects_constraint(msgs_asset_bundling, nba_nhl_monopoly_power).
narrative_ontology:affects_constraint(msgs_asset_bundling, capital_market_price_discovery).

% DUAL FORMULATION NOTE:
% The bundled asset constraint decomposes into three related claims: (1) whether sports franchises are artificially scarce (upstream constraint affecting bundling viability), (2) whether NBA/NHL hold monopoly power that bundling reinforces (parallel constraint), (3) whether capital market pricing reflects true franchise values under bundling vs separate ownership (downstream measurement constraint). This story focuses on the bundling structure itself; upstream constraints address the scarcity that makes bundling viable; downstream constraints address valuation distortion outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
