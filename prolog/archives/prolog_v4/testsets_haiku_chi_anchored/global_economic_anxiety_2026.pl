% ============================================================================
% CONSTRAINT STORY: global_economic_anxiety_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_economic_anxiety_2026, []).

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
 *   constraint_id: global_economic_anxiety_2026
 *   human_readable: The Global Economic Anxiety Snare
 *   domain: economic/social
 *
 * SUMMARY:
 *   The global economic anxiety snare, documented in Gallup's 2026 survey as
 *   the #1 concern in 107 countries, is not a perception problem but a
 *   structural extraction mechanism operating through wage stagnation,
 *   capital mobility, debt financing, and political distraction. Over the
 *   past decade, the wage share of GDP has fallen across OECD economies while
 *   financial extraction (debt spreads, asset management fees, predatory
 *   lending) has risen. Workers are trapped through geographic immobility
 *   (high housing costs), skill specificity, and lack of bargaining power.
 *   Capital is mobile, extracting returns through automated production and
 *   financial arbitrage. The anxiety itself becomes weaponized: anxiety
 *   drives consumer borrowing (expanding debt exposure), anxiety drives
 *   electoral volatility (creating opportunities for extractive political
 *   promises), anxiety drives acceptance of surveillance and authoritarian
 *   controls (trading privacy for promises of security). The snare is the
 *   coordination failure between globally integrated capital and nationally
 *   trapped labor.
 *
 * KEY AGENTS:
 *   - Precarious Workers: Primary victim (powerless/trapped) — gig workers, contingent employees, informal sector; no stable income or social safety net
 *   - Deindustrialized Regions: Primary victim (moderate/constrained) — communities experiencing capital flight, wage decline, disinvestment; constrained by moving costs and social ties
 *   - Households Under Median Income: Victim group (powerless/trapped) — median household wage growth near-zero since 2000 in developed economies despite productivity gains
 *   - Financial Extraction Layers: Beneficiary (institutional/arbitrage) — banks, private equity, asset managers capturing spread between policy rates and consumer lending rates
 *   - Political Establishment: Secondary beneficiary (powerful/mobile) — benefits from anxiety-driven voter attention, can pivot policy to appear responsive while delivering upward redistribution
 *   - Labor Coalitions: Organized victims (organized/constrained) — unions and worker organizations experience extraction but have coordination capacity; constrained by union-busting and legal barriers
 *   - Central Banks: Institutional coordinator (institutional/arbitrage) — solve inflation coordination problem; can selectively benefit through policy timing; exit through rate changes
 *   - Analytical Observer: Civilizational view (analytical/analytical) — reveals extraction as structural: wage share decline, capital mobility, debt dependency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_economic_anxiety_2026, 0.58).
domain_priors:suppression_score(global_economic_anxiety_2026, 0.68).
domain_priors:theater_ratio(global_economic_anxiety_2026, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_economic_anxiety_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_economic_anxiety_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(global_economic_anxiety_2026, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_economic_anxiety_2026, snare).
narrative_ontology:human_readable(global_economic_anxiety_2026, "The Global Economic Anxiety Snare").
narrative_ontology:topic_domain(global_economic_anxiety_2026, "economic/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_economic_anxiety_2026, financial_extraction_layers).
narrative_ontology:constraint_beneficiary(global_economic_anxiety_2026, political_distraction_beneficiaries).
narrative_ontology:constraint_victim(global_economic_anxiety_2026, precarious_workers).
narrative_ontology:constraint_victim(global_economic_anxiety_2026, households_under_median_income).
narrative_ontology:constraint_victim(global_economic_anxiety_2026, climate_vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (SNARE) — Trapped in gig economy, contingent employment, or informal sector. Economic anxiety is not a perception but a structural reality: no access to stable income, healthcare, or unemployment insurance. No exit option; bears full cost of extraction. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEINDUSTRIALIZED REGION (SNARE) — Manufacturing collapse, falling wage share, declining public investment. Communities are constrained: leaving requires social capital and transportation costs; staying means accepting lower living standards. Anxiety reflects real extractive dynamics: capital mobility extracts wealth while labor bears the cost. d≈0.85, f(d)≈1.18, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LABOR COALITION (TANGLED ROPE) — Unions and worker organizations experience mixed extraction and coordination. They organize collective action (coordination) but face suppression through union-busting, legal barriers, and wage stagnation. Can partially exit through strikes or organizing but constrained by employer power and antiunion legislation. d≈0.60, f(d)≈0.75, σ=1.0 → χ≈0.43.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CENTRAL BANK (ROPE) — Solves coordination problem of monetary stability, but can selectively benefit from anxiety through policy discretion. Interest rate cycles create arbitrage opportunities for those with access to cheap capital. Experiences constraint as coordination (managing inflation) with institutional exit options (policy changes, forward guidance). d≈0.20, f(d)≈0.08, σ=1.2 → χ≈0.006. Near-zero extraction due to arbitrage exit and institutional power.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FINANCIAL EXTRACTION APPARATUS (SNARE) — Banks, private equity, asset managers extracting through debt financing, predatory lending, asset stripping, fee extraction. Anxiety drives borrowing (consumer debt, student loans, mortgages); financial firms capture the spread. High extractiveness masked by appearing as 'market coordination.' d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.005. Negative extraction because beneficiary; but institutional suppression (regulatory capture, opacity) keeps extraction hidden.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: POLITICAL ESTABLISHMENT (TANGLED ROPE) — Benefits from anxiety through voter attention capture (security/jobs messaging), but also bears real constraint from demand for economic action. Coordination function: promises to address anxiety; extraction function: delivers symbolic action (tax cuts, trade wars) that redistribute upward while claiming populism. Mobile exit through rotation out of office; can pivot policy. d≈0.35, f(d)≈0.33, σ=1.0 → χ≈0.19.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL VIEW (SNARE) — From a civilizational perspective, the anxiety snare is the coordination failure between labor mobility and wage stagnation in late-stage financialization. Workers are trapped by: (1) geographic immobility (housing costs, family ties), (2) skill specificity, (3) capital control. Capital is mobile. The constraint is structural: wage share has fallen from 65% to 52% of GDP across OECD since 1970; households respond with anxiety and consumer debt. d≈0.82, f(d)≈1.15, σ=1.2 → χ≈0.79.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_economic_anxiety_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_economic_anxiety_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_economic_anxiety_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_economic_anxiety_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(global_economic_anxiety_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The snare extracts through multiple vectors: (1) wage stagnation (labor's share of GDP down from 65% to 52% since 1980), (2) debt financing (median household debt-to-income ratio up 40% since 2000), (3) asset inflation (housing, equity) benefiting capital over labor, (4) financial intermediation (fees, spreads). The 0.58 value reflects that extraction is substantial but not total — workers retain some exit options (reskilling, migration, unionization) and some policy channels remain open. If exit were completely blocked, extractiveness would approach 0.85+. Suppression (0.68): High. Multiple suppression vectors: (1) geographic immobility (housing costs prevent migration), (2) skill specificity (retraining requires time/capital), (3) union suppression (legal barriers, employer power), (4) information opacity (debt structure, financial fees), (5) political capture (tax policy favors capital over labor). Theater ratio (0.65): Moderate-high. Political responses to anxiety are substantially performative: trade wars, tax cuts, infrastructure promises deliver symbolic action without addressing wage share decline or worker power. Central bank forward guidance creates theater of control. Financial sector market-monitoring creates theater of stability. News coverage of 'economic confidence' substitutes for actual income growth. The ratio has increased over the decade as real wage growth has stalled — political theater has expanded to compensate.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Precarious workers and deindustrialized regions see a pure Snare (trapped, no exit, extraction rising). Labor coalitions see a Tangled Rope (can organize collective action but face suppression; some coordination function but asymmetric extraction). Central banks and financial institutions see a Rope (solving coordination problem) or near-zero extraction (beneficiaries with arbitrage options). Political establishment sees a mixed Tangled Rope (must appear responsive but can pivot policy). The analytical observer sees a structural Snare rooted in capital-labor mobility asymmetry. The perspectival gap indicates high structural complexity: no single policy fix addresses all perspectives simultaneously. Wage policy helps precarious workers but doesn't address regional deindustrialization. Unionization helps organized labor but excludes gig workers. Rate policy helps debtors but may tighten regional inequality. The gap is not epistemic but structural.
 *
 * DIRECTIONALITY LOGIC:
 *   Precarious workers: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. Deindustrialized regions: Victim + constrained → d≈0.85, f(d)≈1.18. High extraction. Labor coalitions: Victim + constrained but organized → d≈0.60, f(d)≈0.75. Mixed extraction; organization provides partial escape. Central banks: Beneficiary + arbitrage → d≈0.20, f(d)≈0.08. Near-zero extraction; institutional power and policy exit options. Financial apparatus: Beneficiary + arbitrage (institutional power, capital mobility) → d≈0.08, f(d)≈-0.08. Negative extraction (net beneficiary); but suppression through opacity keeps extraction mechanism hidden. Political establishment: Mixed beneficiary + mobile (can pivot policy) → d≈0.35, f(d)≈0.33. Low-moderate effective extraction; mobility through electoral cycles allows escape. Analytical observer: observes maximum extraction through structural immobility → d≈0.82, f(d)≈1.15. High effective extraction when accounting for civilizational timescale.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The global economic anxiety snare resolves the extraction vs coordination ambiguity through structural analysis of exit capacity. From the beneficiary's perspective (central banks, financial sector), the constraint appears as Rope (coordination) — they solve the monetary stability coordination problem. From the victim's perspective (trapped workers), the constraint is pure Snare — wages stagnate while debt accumulates. The mandatrophy is resolved by recognizing that BOTH are true but apply to different agents with different exit capacities. The extraction only becomes visible when you ask: 'Who can exit and who cannot?' Capital can exit (geographic mobility, portfolio rebalancing, policy arbitrage). Labor cannot (geographic immobility, skill specificity, debt lock-in). This is not a coordination problem (which would affect both parties symmetrically) but an extraction problem (which affects parties asymmetrically based on exit capacity). The Rope-reading is a beneficiary perspective that naturalizes the extraction as coordination. The Snare-reading is a victim perspective that reveals the asymmetry. The framework's job is to honor both perspectives while revealing WHY they diverge: different exit capacity → different d → different χ → different classification. The true classification is not 'Rope' or 'Snare' but 'asymmetrically extractive Rope-to-Snare across the presheaf of observations.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anxiety_measurement_fidelity,
    'Does Gallup''s question (''What is the most important problem facing your country today?'') capture extractive economic constraint or merely perception of economic difficulty?',
    'Cross-validation with objective metrics: correlation between reported anxiety and actual income volatility, access to credit, healthcare costs, housing affordability by respondent. Distinguish structural extraction from cyclical uncertainty.',
    'If objective extraction high: Snare classification confirmed. If perception high but objective metrics stable: constraint may be partly theatrical (Piton). If perception varies with media framing: mediation effect suggests lower base extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anxiety_measurement_fidelity, empirical, 'Whether anxiety reflects extractive structure or perception').

omega_variable(
    wage_share_causality,
    'Does stagnant wage share cause anxiety, or do both result from deeper structural shifts (automation, capital mobility, financialization)?',
    'Lag analysis: does wage share decline predict anxiety increases? Or do both lag policy shifts (union weakening, deregulation, capital controls removal)? Time-series econometrics with policy shocks.',
    'If wage share causes anxiety directly: Snare is the constraint (income extraction). If both lag policy: the constraint is the policy framework enabling extraction. If automation-driven: constraint may be technological (partial mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_share_causality, empirical, 'Whether wage stagnation directly causes anxiety or reflects shared causality').

omega_variable(
    exit_capacity_measurement,
    'How much of workers'' trapped exit status is geographic (housing immobility) vs skill-based (retraining barriers) vs behavioral (preference for community stability)?',
    'Survey of workers in anxious regions: Which factors constrain exit? What policy interventions would expand exit options (geographic mobility subsidies, retraining, remote work viability)?',
    'If geographic: housing market reform could unlock exit. If skill-based: labor market policy could unlock exit. If behavioral: exit capacity is higher than structural claim suggests; anxiety is not pure snare but partly Rope (coordination) if exit becomes available.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_capacity_measurement, empirical, 'Decomposition of exit barriers by type').

omega_variable(
    financial_extraction_opacity,
    'Do households understand the extraction vectors (debt spreads, fee structures, predatory lending) or experience anxiety as diffuse economic uncertainty?',
    'Financial literacy surveys + debt structure analysis. Do households in high-debt-service regions report anxiety at higher rates than those in low-debt regions with similar wage stagnation?',
    'If extraction is opaque: snare relies on suppression (lack of awareness). If transparent: snare relies on structural powerlessness (trapped regardless of awareness). Different policy implications for remediation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_extraction_opacity, empirical, 'Whether financial extraction is hidden or visible to victims').

omega_variable(
    political_distraction_beneficiary,
    'Do politicians and media deliberately amplify economic anxiety to drive electoral turnout and attention away from other extractive systems (environmental, health, legal)?',
    'Media analysis: track frequency of ''economy'' framing across news cycles relative to objective economic change. Do media mentions of economy peak during election cycles independent of actual economic indicators?',
    'If deliberately amplified: anxiety snare is a Tangled Rope (coordination + extraction hybrid) with deliberate beneficiary. If organic: snare is structural (Snare without organized beneficiary). Affects remediation (media accountability vs systemic reform).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_distraction_beneficiary, conceptual, 'Whether anxiety is deliberately manufactured or organically reflects conditions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_economic_anxiety_2026, 2016, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geax_tr_t0, global_economic_anxiety_2026, theater_ratio, 0, 0.52).
narrative_ontology:measurement(geax_tr_t5, global_economic_anxiety_2026, theater_ratio, 5, 0.6).
narrative_ontology:measurement(geax_tr_t10, global_economic_anxiety_2026, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(geax_be_t0, global_economic_anxiety_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(geax_be_t5, global_economic_anxiety_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(geax_be_t10, global_economic_anxiety_2026, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_economic_anxiety_2026, resource_allocation).
narrative_ontology:affects_constraint(global_economic_anxiety_2026, wage_stagnation_2026).
narrative_ontology:affects_constraint(global_economic_anxiety_2026, housing_affordability_crisis).
narrative_ontology:affects_constraint(global_economic_anxiety_2026, consumer_debt_trap).
narrative_ontology:affects_constraint(global_economic_anxiety_2026, union_suppression_legal_framework).

% DUAL FORMULATION NOTE:
% The global economic anxiety is downstream of deeper structural constraints: wage stagnation (extraction mechanism), housing affordability (geographic immobility), consumer debt (financial extraction vector), and union suppression (political extraction). Each of these constraints has its own ε value. The anxiety snare (ε=0.58) represents the aggregated effect of these vectors plus their psychological impact. Decompose into constraint family if detailed analysis of individual extraction mechanisms is needed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_economic_anxiety_2026, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
