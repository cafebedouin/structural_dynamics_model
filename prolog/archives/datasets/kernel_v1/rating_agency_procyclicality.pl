% ============================================================================
% CONSTRAINT STORY: rating_agency_procyclicality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rating_agency_procyclicality, []).

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
 *   constraint_id: rating_agency_procyclicality
 *   human_readable: Rating Agency Procyclicality in Financial Markets
 *   domain: financial_regulation/credit_markets
 *
 * SUMMARY:
 *   Rating agency procyclicality creates a structural constraint where
 *   backward-looking statistical models produce mechanically pro-trend
 *   ratings: during credit expansions, falling default rates and rising
 *   collateral values inflate agency ratings, enabling excessive leverage and
 *   asset price inflation. During busts, the same mechanics produce downward
 *   spirals of rating downgrades, fire-sales, and contagion. The constraint
 *   exhibits a clear temporal cycle with extraction concentrated on powerless
 *   borrowers and retail investors who cannot exit or influence the rating
 *   mechanism. Issuers and rating agencies benefit from inflated ratings
 *   during expansions; the cost cascades to retail investors and borrowers
 *   during contractions. The constraint is a genuine snare from the
 *   perspective of trapped agents, but exhibits mixed coordination-extraction
 *   dynamics from regulatory and institutional perspectives that view ratings
 *   as a necessary — if imperfect — credit allocation mechanism. The theater
 *   ratio (0.65) reflects that ratings function as ritualized market signals
 *   during booms (mechanically-produced, backward-looking) but claim to
 *   represent forward-looking credit risk assessment.
 *
 * KEY AGENTS:
 *   - Retail Investors: Primary victims (powerless/trapped) — hold securities selected partly on rating signals; bear losses during rating collapses; cannot exit or influence rating accuracy
 *   - Powerless Borrowers: Secondary victims (powerless/trapped) — exposed to credit markets where inflated ratings during booms enable over-leverage, systemic instability, and contagion during busts
 *   - Bank Issuers: Primary beneficiaries (institutional/constrained) — benefit from cheap funding access during expansion phase when ratings are inflated; face constraints during contraction when ratings collapse
 *   - Rating Agencies: Direct beneficiaries (institutional/arbitrage) — capture fee revenue from issuers without reputational penalty during expansions; arbitrage between accuracy and profitability
 *   - Regulatory Bodies: Institutional actors (organized/constrained) — coordinate credit allocation through rating reliance; locked into procyclical mechanism by legal constraints and path dependency
 *   - Credit Rating Models: Degraded institutional mechanism (piton) — theoretically sound but functionally procyclical; persists through inertia and profit incentives
 *   - Market Stability: Abstract victim (powerless/trapped) — no agency, no exit; bears cost of amplified credit cycles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rating_agency_procyclicality, 0.68).
domain_priors:suppression_score(rating_agency_procyclicality, 0.72).
domain_priors:theater_ratio(rating_agency_procyclicality, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rating_agency_procyclicality, extractiveness, 0.68).
narrative_ontology:constraint_metric(rating_agency_procyclicality, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rating_agency_procyclicality, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rating_agency_procyclicality, snare).
narrative_ontology:human_readable(rating_agency_procyclicality, "Rating Agency Procyclicality in Financial Markets").
narrative_ontology:topic_domain(rating_agency_procyclicality, "financial_regulation/credit_markets").

domain_priors:requires_active_enforcement(rating_agency_procyclicality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rating_agency_procyclicality, issuers_and_investment_banks).
narrative_ontology:constraint_beneficiary(rating_agency_procyclicality, rating_agencies).
narrative_ontology:constraint_victim(rating_agency_procyclicality, retail_investors).
narrative_ontology:constraint_victim(rating_agency_procyclicality, powerless_borrowers).
narrative_ontology:constraint_victim(rating_agency_procyclicality, market_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Trapped by information asymmetry and regulatory reliance on agency ratings. Bears full extraction during downturns when ratings collapse and rated securities become worthless. No exit option during credit expansion phase when ratings mask deteriorating fundamentals. Maximum experienced extraction.
constraint_indexing:constraint_classification(rating_agency_procyclicality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: POWERLESS BORROWER (SNARE) — Trapped in credit markets where inflated ratings during booms enable over-leverage that cascades into defaults during busts. Bears cost of systemic instability, asset price crashes, and contagion. Cannot exit or influence rating mechanics. Structural victim across market cycles.
constraint_indexing:constraint_classification(rating_agency_procyclicality, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: BANK ISSUER (TANGLED ROPE) — Benefits from inflated ratings during expansion phase (cheap funding access). Faces constraints during contraction (ratings downgrades, funding dry-up). Mixed extraction and coordination — the rating system coordinates credit allocation but asymmetrically favors issuers during good times. Moderate agency with constrained alternatives (can restructure, access Central Bank liquidity).
constraint_indexing:constraint_classification(rating_agency_procyclicality, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: RATING AGENCY (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: ratings allocate credit, issuers pay for ratings, agencies function as information standard. Low extraction cost because agencies capture fee revenue without reputational penalty during expansions. Exit option: arbitrage between regulatory reliance and accuracy (agencies maintain AAA-grade commercial models despite known procyclical bias).
constraint_indexing:constraint_classification(rating_agency_procyclicality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY BODY (TANGLED ROPE) — Coordinates credit market stability through rating reliance but is locked into the procyclical mechanism. Constrained by legal constraints (agencies are private actors), political economy (issuers lobby to preserve light-touch regulation), and path dependency (replacing rating agencies requires coordinated international action). Benefits from coordination function; pays cost of amplified cycles.
constraint_indexing:constraint_classification(rating_agency_procyclicality, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CREDIT RATING MODEL (PITON) — Theoretically sound (statistical loss prediction) but functionally degraded (backward-looking mechanics guarantee procyclicality). The model persists through institutional inertia despite known failure modes. Agencies arbitrage between maintaining the simple, profitable model and publicly acknowledging superior forward-looking approaches. Theater ratio reflects that agency ratings are ritualized market signals rather than accurate forward-looking assessments.
constraint_indexing:constraint_classification(rating_agency_procyclicality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal frame, procyclicality appears as an inherent feature of backward-looking risk models applied to dynamic markets: any model trained on historical data will mechanically produce pro-trend outputs. This perspective risks naturalizing what is a contingent choice (using backward-looking models) as an immutable mathematical constraint. Engine false-summit detection will identify beneficiaries and reveal this naturalization.
constraint_indexing:constraint_classification(rating_agency_procyclicality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rating_agency_procyclicality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rating_agency_procyclicality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rating_agency_procyclicality, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rating_agency_procyclicality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rating_agency_procyclicality, TR),
    TR >= 0.70.

:- end_tests(rating_agency_procyclicality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint mechanically extracts from powerless agents during contractions while enabling issuers to capture cheap funding during expansions. The asymmetry is structural: ratings are backward-looking, guaranteeing pro-trend drift. Base extractiveness reflects that the constraint concentrates downside risk on powerless agents while distributing upside benefits to organized beneficiaries during booms. Suppression (0.72): High. Retail investors and borrowers face multiple suppression mechanisms: information asymmetry (ratings are hard to dispute for non-experts), regulatory mandate (Basel III and post-Dodd-Frank rules require rating reliance for capital adequacy), institutional lock-in (all major institutions rely on the same agencies), and cognitive capture (ratings appear objective and forward-looking despite being backward-looking). Exit alternatives (developing independent credit assessment) are expensive and require coordination. Theater Ratio (0.65): Moderate-high. Ratings function as ritualized signals during booms when default correlations are low — mechanical models produce AAA grades for securities that later default at high rates. The ritualization is visible in the disconnect between stated methodology (forward-looking risk assessment) and actual mechanics (historical loss prediction). During contractions, the theater increases as ratings cascade downward mechanically, revealing the pro-trend bias but providing no corrective information (downgrade announcements trigger sales, not buying).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a stark perspectival gap between beneficiaries and victims. Issuers and rating agencies experience ratings as a coordination mechanism (Rope from their perspective) — issuers access credit, agencies allocate it, markets clear. Retail investors and borrowers experience pure extraction (Snare) — they absorb losses when ratings collapse. Regulatory bodies occupy a middle position (Tangled Rope) — they coordinate credit markets through rating reliance but are locked into a procyclical mechanism they cannot unilaterally exit. The credit rating model itself appears as a degraded institution (Piton) to observers who recognize the gap between stated methodology and actual pro-trend mechanics. The analytical observer risks naturalizing this as an inherent feature of backward-looking risk models (Mountain), but structural data reveals beneficiaries and their profit incentives, triggering false-summit detection. The perspectival gap is the diagnostic signal: if procyclicality were truly a natural law, all observers would experience it identically; instead, beneficiaries see coordination and victims see extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural position relative to the extraction flow. Retail investors (powerless/trapped) have d ≈ 0.95 — they are the target of extraction. Beneficiaries like issuers and rating agencies have d ≈ 0.10-0.20 — they benefit from the mechanism. Regulatory bodies (organized/constrained) have d ≈ 0.60 — they occupy a mixed position, both enabling the constraint and bearing costs from its procyclical amplification. The sigmoid f(d) translates these to effective power modifiers: trapped powerless agents experience f(d) ≈ 1.42 (maximal experienced extraction), while beneficiary institutions experience f(d) ≈ -0.12 (negative extraction, i.e., subsidy). The constraint is global in scope (σ(S) ≈ 1.2), which amplifies χ — worldwide retail investors and borrowers are exposed to a single unified rating mechanism. The resulting χ = ε × f(d) × σ(S) for powerless agents exceeds 0.90, confirming Snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is a genuine Snare from the perspective of powerless, trapped agents (retail investors, borrowers) but a Rope or Tangled Rope from the perspective of institutional beneficiaries and regulators. The 'true' classification is not singular — it is the perspectival presheaf. The constraint is a Snare for those who cannot exit; it is coordination (Rope) for those who benefit. The mandatrophy error would be to claim that 'rating agencies provide an objective coordination mechanism so this is Rope.' That claim depends on ignoring that beneficiaries actively maintain procyclicality despite knowing about it, and that powerless agents cannot exit. The mandatrophy is resolved by the engine's multi-perspective computation: Snare from the powerless perspective (claims the constraint), Rope from beneficiary perspective (denies or minimizes extraction), Tangled Rope from regulatory perspective (coordinates but with asymmetric burden). The constraint story resolves the conflict by showing all three are structurally correct — they describe different positions within the same constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forward_looking_technical_feasibility,
    'Are forward-looking rating models technically feasible or does fundamental uncertainty make backward-looking approaches inevitable?',
    'Comparative performance analysis of forward-looking models (stress-tested on 2008-2020 data, specifically on bust-phase prediction accuracy); analysis of whether superior models exist but are not deployed due to profit incentives',
    'If forward-looking models are technically feasible but not deployed: constraint is a pure extraction mechanism (Snare confirmed). If forward-looking modeling is fundamentally intractable: procyclicality is closer to natural law, and the constraint is a tragic coordination problem (reclassify toward Rope or Tangled Rope from beneficiary perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(forward_looking_technical_feasibility, empirical, 'Whether forward-looking rating models are technically feasible').

omega_variable(
    regulatory_enforcement_sufficiency,
    'Do existing regulatory constraints (Dodd-Frank rating agency provisions, EU risk adjustment requirements, post-2010 capital adequacy reforms) meaningfully reduce procyclical amplification or merely create theater?',
    'Time-series analysis of rating inflation and credit spread behavior pre- vs post-regulation; analysis of whether banks and investors still mechanically treat ratings as reliable (de facto regulatory reliance unchanged despite de jure constraints)',
    'If regulations are substantively effective: suppression metric should be lower (~0.40-0.50), reclassifying to Tangled Rope from more perspectives. If regulations are theater: suppression remains high (~0.70+), confirming Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_sufficiency, empirical, 'Whether post-2010 regulatory constraints meaningfully reduce procyclicality').

omega_variable(
    agency_conflict_necessity,
    'Is the conflict between rating accuracy and agency revenue growth inherent to the business model or a choice point where alternative structures (public rating bodies, non-profit models, fee structures disconnected from issuer funding) could eliminate the incentive misalignment?',
    'Comparative institutional analysis: performance of alternative models (public credit ratings bodies in some jurisdictions, academic rating approaches); analysis of whether issuers actively lobby against accuracy improvements that would reduce their access to cheap capital',
    'If alternatives are viable: beneficiary extraction is discretionary (Snare confirmed). If rating agency structure is the least-bad option: constraint moves toward Rope or Scaffold (transition pathway available). If alternatives are theoretically viable but politically blocked: omega becomes a ''preference'' class, and the constraint is a political choice, not an economic necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_conflict_necessity, preference, 'Whether rating agency incentive conflicts are inherent or choice points').

omega_variable(
    investor_rational_choice,
    'Do retail investors mechanically trust ratings despite known bias, or do they rationally accept ratings as the least-bad available signal given information asymmetry?',
    'Survey and behavioral data on investor reliance on ratings; analysis of retail investor portfolio composition during rating inflation periods (whether retail is actually concentrated in securities later downgraded); comparison of retail losses to institutional investor losses during busts',
    'If mechanical trust despite known bias: retail investors are victims of cognitive capture (identity_locked exit option), and theater_ratio should be higher (~0.80). If rational choice given asymmetry: constraint is Tangled Rope from retail perspective (coordination mechanism with unfair burden-sharing). Changes classification and exit option characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investor_rational_choice, empirical, 'Whether retail investor reliance on ratings is cognitive capture or rational choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rating_agency_procyclicality, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rapc_theater_expansion_start, rating_agency_procyclicality, theater_ratio, 0, 0.45).
narrative_ontology:measurement(rapc_theater_expansion_mid, rating_agency_procyclicality, theater_ratio, 3, 0.55).
narrative_ontology:measurement(rapc_theater_peak_before_bust, rating_agency_procyclicality, theater_ratio, 5, 0.68).
narrative_ontology:measurement(rapc_theater_bust_peak, rating_agency_procyclicality, theater_ratio, 7, 0.75).
narrative_ontology:measurement(rapc_theater_stabilization, rating_agency_procyclicality, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(rapc_extr_expansion_start, rating_agency_procyclicality, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rapc_extr_expansion_mid, rating_agency_procyclicality, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(rapc_extr_peak_before_bust, rating_agency_procyclicality, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(rapc_extr_bust_peak, rating_agency_procyclicality, base_extractiveness, 7, 0.82).
narrative_ontology:measurement(rapc_extr_stabilization, rating_agency_procyclicality, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rapc_supp_expansion_start, rating_agency_procyclicality, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(rapc_supp_peak, rating_agency_procyclicality, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(rapc_supp_stabilization, rating_agency_procyclicality, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rating_agency_procyclicality, resource_allocation).
narrative_ontology:affects_constraint(rating_agency_procyclicality, credit_spread_compression).
narrative_ontology:affects_constraint(rating_agency_procyclicality, asset_price_bubble_formation).
narrative_ontology:affects_constraint(rating_agency_procyclicality, fire_sale_cascades).

% DUAL FORMULATION NOTE:
% Rating agency procyclicality is the upstream constraint that enables downstream constraints (asset bubbles, fire-sale cascades). A separate constraint story on 'credit rating model degradation' would decompose the piton classification (backward-looking mechanics, institutional inertia) from the snare classification (extraction asymmetry, beneficiary incentives). The present story focuses on the extraction mechanism; an alternative story could focus on the degradation of the model itself relative to its stated purpose.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
