% ============================================================================
% CONSTRAINT STORY: north_african_currency_regimes
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_north_african_currency_regimes, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: north_african_currency_regimes
 *   human_readable: North African Currency Regime Constraints
 *   domain: monetary_policy/postcolonial_economics
 *
 * SUMMARY:
 *   North African currency regimes represent a structurally complex
 *   constraint system inherited from colonial institutions and maintained
 *   through ongoing postcolonial relationships with former metropolitan
 *   centers and international financial institutions. The constraint operates
 *   primarily through the CFA Franc arrangement (West and Central Africa) and
 *   similar fixed-peg regimes, which limit monetary policy autonomy while
 *   guaranteeing currency convertibility and capital flow predictability. The
 *   classification as Tangled Rope reflects that the regime simultaneously
 *   provides genuine coordination benefits (stable currency for trade,
 *   credible commitment to lenders) and mechanisms for asymmetric extraction
 *   (limited policy autonomy, externally-imposed interest rate constraints,
 *   vulnerability to external shocks). The theater ratio has increased from
 *   0.42 to 0.55 over the interval as the original coordination function has
 *   eroded — modern African economies require counter-cyclical monetary
 *   policy and development-oriented credit expansion that the external peg
 *   prevents — while the institutional ritual of colonial-era arrangements
 *   persists through inertia. The constraint exhibits six distinct
 *   perspectives that range from pure extraction (Snare for powerless trapped
 *   populations) through mixed coordination-extraction (Tangled Rope for
 *   regional moderate actors) to pure coordination (Rope for international
 *   financial beneficiaries) to aspiration toward exit (Scaffold for
 *   pan-African integration) to institutional degradation (Piton for
 *   colonial-era institutions) to naturalization (Mountain for analytical
 *   observers who risk treating path-dependent history as economic law).
 *
 * KEY AGENTS:
 *   - Domestic populations: Primary victims (powerless/trapped) — bear inflation imported from pegging partners, face credit constraints, and cannot pursue autonomous development strategies
 *   - Regional economic elites and governments: Secondary actors (moderate/constrained) — benefit from stable currency for trade but face constrained exits due to capital flight risk and creditor pressure
 *   - Former colonial centers (France, UK, other metropolitan powers): Primary beneficiaries (institutional/arbitrage) — maintain macroeconomic control and extract via seigniorage, capital flow predictability, and trade advantages
 *   - International financial institutions (IMF, World Bank): Secondary beneficiaries (institutional/arbitrage) — benefit from transparent, externally-disciplined monetary regimes that enable lending and structural adjustment
 *   - Pan-African integration movement: Organized agents (organized/mobile) — building regional currency cooperation and continental integration as alternative coordination pathways
 *   - Central Banks and Monetary Authorities: Institutional actors (institutional/arbitrage) — maintain colonial-era institutional structures; see own arrangements as degraded but continue them through path dependency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(north_african_currency_regimes, 0.58).
domain_priors:suppression_score(north_african_currency_regimes, 0.62).
domain_priors:theater_ratio(north_african_currency_regimes, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(north_african_currency_regimes, extractiveness, 0.58).
narrative_ontology:constraint_metric(north_african_currency_regimes, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(north_african_currency_regimes, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(north_african_currency_regimes, tangled_rope).
narrative_ontology:human_readable(north_african_currency_regimes, "North African Currency Regime Constraints").
narrative_ontology:topic_domain(north_african_currency_regimes, "monetary_policy/postcolonial_economics").

domain_priors:requires_active_enforcement(north_african_currency_regimes).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(north_african_currency_regimes, former_colonial_monetary_centers).
narrative_ontology:constraint_beneficiary(north_african_currency_regimes, regional_financial_elites).
narrative_ontology:constraint_beneficiary(north_african_currency_regimes, multinational_corporations).
narrative_ontology:constraint_victim(north_african_currency_regimes, domestic_populations).
narrative_ontology:constraint_victim(north_african_currency_regimes, local_currency_autonomy).
narrative_ontology:constraint_victim(north_african_currency_regimes, monetary_policy_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMESTIC POPULATION (SNARE) — Trapped by currency regimes that limit autonomous monetary policy and economic development pathways. Suppression operates through legal frameworks (currency board arrangements, fixed pegs to foreign currencies) that make exit formally impossible. Populations bear extraction through inflation imported from pegging partners, interest rate constraints, and restricted credit availability for local development.
constraint_indexing:constraint_classification(north_african_currency_regimes, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL ECONOMIC ACTORS (TANGLED ROPE) — Face constrained exit: formal mechanisms exist for currency regime change (leaving fixed peg, abandoning foreign peg), but costs are severe (capital flight, creditor backlash, IMF conditions). Benefit from stable currency for trade and remittance flows; bear extraction through loss of policy autonomy and asymmetric vulnerability to external shocks.
constraint_indexing:constraint_classification(north_african_currency_regimes, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COLONIAL CENTERS AND IFIs (ROPE) — Benefit from regimes that guarantee currency convertibility, capital flow transparency, and predictable macroeconomic management. Experience the constraint as coordination: pegged currencies enable trade, reduce transaction costs, and provide credible commitment devices for international lenders. High arbitrage options — these actors can redirect capital flows if regimes change, and international financial institutions can enforce compliance through conditionality.
constraint_indexing:constraint_classification(north_african_currency_regimes, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PAN-AFRICAN INTEGRATION MOVEMENT (SCAFFOLD) — Organized agents (African Union, ECOWAS, AfCFTA initiatives) see currency constraints as a temporary coordination failure with a generational sunset. Building alternative pathways: common currency projects (Afro, WAEMU reform), regional monetary cooperation, and trade union deepening aim to enable autonomous monetary policy without losing coordination benefits. Sunset logic applies: as intra-African trade deepens and regional institutions mature, the functional dependency on external currency anchors declines.
constraint_indexing:constraint_classification(north_african_currency_regimes, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: COLONIAL-ERA INSTITUTIONS (PITON) — CFA Franc arrangements and similar vestiges persist through institutional inertia long past their functional necessity. Theater ratio is high (0.55): ritual compliance with colonial-era reserve requirements, guaranteed convertibility at fixed rates, and institutional governance structures that preserve metropolitan control despite formal independence. The institutions see their own arrangements as degraded — maintained because alternatives are costly to coordinate, not because the original functionality remains.
constraint_indexing:constraint_classification(north_african_currency_regimes, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a civilizational perspective, currency constraints can appear as natural economic laws: global capital requires stable, externally-anchored monetary frameworks; small open economies cannot sustain autonomous currency regimes; integration into international finance necessitates surrendering monetary sovereignty. However, this perspective risks naturalizing what is actually a contingent institutional arrangement and historical path dependency. The engine's false summit detector will identify this as misclassified: North African currency regimes are products of colonial institutions and ongoing power imbalances, not immutable economic laws.
constraint_indexing:constraint_classification(north_african_currency_regimes, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(north_african_currency_regimes_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(north_african_currency_regimes, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(north_african_currency_regimes, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(north_african_currency_regimes, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(north_african_currency_regimes, TR),
    TR >= 0.70.

:- end_tests(north_african_currency_regimes_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through multiple channels: (1) seigniorage and reserve requirements that benefit metropolitan centers, (2) constrained credit availability that limits domestic development financing, (3) interest rate constraints that prevent counter-cyclical policy, (4) terms-of-trade vulnerability when pegging partners inflate. The extractiveness has increased from 0.35 to 0.58 over the interval as the original coordination function (enabling trade, reducing transaction costs) has become less salient relative to the extraction mechanism (policy constraints becoming more binding as economies diversify). Suppression (0.62): High. Multiple suppression mechanisms operate: legal frameworks (currency boards, fixed convertibility guarantees), capital account controls, institutional dependencies, creditor enforcement through IMF/World Bank conditionality, and (critically) internalized belief that autonomous regimes are infeasible. Theater ratio (0.55): Moderate-high. Ritual compliance with colonial-era governance structures persists — reserve requirements, metropolitan oversight, central bank independence from democratic accountability — despite the functional necessity of these requirements declining as African economies develop. The theater reflects that institutions maintain forms of control they no longer justify through actual coordination function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural phenomenon (currency peg to external anchor) produces radically different classifications from different positions. The beneficiary sees coordination (Rope); the trapped population sees extraction (Snare); the constrained regional actor sees mixed coordination-extraction (Tangled Rope); the organized integration movement sees a temporary coordination failure with an exit path (Scaffold); the colonial-era institution sees its own degraded ritual (Piton); the analytical observer risks naturalizing contingency as law (Mountain). The gap is not noise — it reveals the actual constraint structure. The constraint genuinely does provide coordination benefits (that's why the beneficiary classification holds). But those benefits have become increasingly distributed asymmetrically, with costs concentrated on powerless agents and suppression mechanisms preventing exit. The Tangled Rope classification at the moderate level is the diagnostic anchor: it shows that the constraint requires active enforcement (true), has beneficiaries (true, international finance), has victims (true, domestic policy autonomy), and operates in the 0.40 ≤ χ ≤ 0.90 range (true). The false summit (mountain) alerts us that naturalizing language ('small open economies need external anchors') is covering a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural relationship to the extraction flow. The metropolitan financial centers and international institutions benefit from currency stability and policy predictability, positioning them as beneficiaries with high arbitrage options (low d, negative effective extraction). The domestic populations bear all costs of constrained policy with no exit option (high d, high effective extraction). The regional moderate actors occupy an intermediate position: they benefit from stable currency for trade and remittance flows but face high costs of autonomous monetary policy (constrained exit, mixed beneficiary/victim status, moderate d). The organized pan-African movement has exit options (mobile exit through regional integration) that give them agency despite current constraint (lower d than trapped populations). The colonial-era institutions paradoxically occupy a position of low power despite institutional position (piton classification) because their function has eroded and they maintain themselves through inertia, not through control. The analytical observer attempting to classify the constraint faces the oracle gap: seeing mountain (naturalizing) prevents recognizing the contingent institutional arrangement underlying the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES MANDATROPHY BY PERSPECTIVAL MAPPING: The constraint avoids misclassification between pure coordination and pure extraction by declaring distinct perspectives that reveal different aspects of the same structural reality. From the international institutional perspective, the constraint genuinely is Rope — it solves the coordination problem of enabling cross-border trade and attracting capital. From the powerless population perspective, it genuinely is Snare — it prevents autonomous policy and extracts through constrained credit. From the moderate regional perspective, it genuinely is Tangled Rope — both benefits and extraction are real and significant. The mandatrophy resolution is not 'which type is correct?' but rather 'which structural aspects does each perspective reveal?' The constraint is not mis-labeled; it is multiply realized across the observation site. The false summit (mountain perspective) is correctly identified as misclassification by the engine's NL detector, because the accessibility_collapse and resistance metrics do not support natural law status — the constraint depends on ongoing institutional maintenance and creditor enforcement, not on immutable physical laws.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_legacy_binding,
    'To what extent does the structural constraint reflect colonial-era institutions versus ongoing voluntary participation in mutually beneficial arrangements?',
    'Historical institutional analysis; counterfactual modeling of autonomous currency regimes; comparative analysis of post-colonial currency choices in regions with vs without colonial-era constraints',
    'If primarily colonial legacy: classification shifts toward Snare (extraction mechanism is historical lock-in). If primarily voluntary: classification shifts toward Rope (coordination mechanism is legitimate). If mixed: Tangled Rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_legacy_binding, conceptual, 'Colonial legacy binding vs voluntary participation').

omega_variable(
    exit_cost_decomposition,
    'Are the high costs of currency regime exit primarily due to structural macroeconomic dependencies (real constraints) or primarily due to institutional/political sanctions for non-compliance?',
    'Comparative case analysis of successful currency regime changes; modeling of economic vulnerability vs institutional pressure; analysis of capital flight patterns during attempted exits',
    'If structural dependency dominates: suppression reflects real economic constraints (appropriate Snare classification). If institutional sanctions dominate: suppression reflects coercion (shifts toward pure extraction, Snare confirmed). If mixed: Tangled Rope justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_decomposition, empirical, 'Exit costs: structural vs institutional').

omega_variable(
    autonomous_monetary_policy_feasibility,
    'Given current North African institutional capacity, trade patterns, and capital markets, could autonomous monetary regimes operate without severe instability, or are external anchors currently necessary?',
    'Stress-testing of autonomous scenarios; analysis of inflation, currency stability, and reserve adequacy; comparison with middle-income countries that successfully maintain autonomous regimes',
    'If feasible with current capacity: constraint appears primarily extractive (Snare from powerless perspective confirmed). If infeasible: coordination benefit is genuine (Rope from beneficiary perspective gains credibility; Tangled Rope holds).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomous_monetary_policy_feasibility, empirical, 'Feasibility of autonomous monetary policy').

omega_variable(
    identity_fusion_depth,
    'To what extent have North African monetary elites internalized external currency anchors as necessary or natural, making alternative regimes cognitively unavailable even if institutionally possible?',
    'Elite discourse analysis; institutional culture studies; comparison of regime change rhetoric across cohorts and time periods; assessment of whether alternatives are framed as infeasible vs unthinkable',
    'If deep identity fusion: identity_locked exit option becomes salient for moderate/institutional perspectives (explains why capable actors don''t exit). If shallow: exit barriers are primarily material (constrained or trapped remain correct).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_depth, conceptual, 'Identity fusion with external currency anchors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(north_african_currency_regimes, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nacurr_tr_t0, north_african_currency_regimes, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nacurr_tr_t2, north_african_currency_regimes, theater_ratio, 2, 0.46).
narrative_ontology:measurement(nacurr_tr_t4, north_african_currency_regimes, theater_ratio, 4, 0.5).
narrative_ontology:measurement(nacurr_tr_t6, north_african_currency_regimes, theater_ratio, 6, 0.54).
narrative_ontology:measurement(nacurr_tr_t8, north_african_currency_regimes, theater_ratio, 8, 0.55).
narrative_ontology:measurement(nacurr_tr_t10, north_african_currency_regimes, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(nacurr_be_t0, north_african_currency_regimes, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nacurr_be_t2, north_african_currency_regimes, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(nacurr_be_t4, north_african_currency_regimes, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(nacurr_be_t6, north_african_currency_regimes, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(nacurr_be_t8, north_african_currency_regimes, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(nacurr_be_t10, north_african_currency_regimes, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(north_african_currency_regimes, resource_allocation).
narrative_ontology:boltzmann_floor_override(north_african_currency_regimes, 0.18).
narrative_ontology:affects_constraint(north_african_currency_regimes, sahel_resource_extraction_regimes).
narrative_ontology:affects_constraint(north_african_currency_regimes, postcolonial_institutional_path_dependency).
narrative_ontology:affects_constraint(north_african_currency_regimes, international_capital_flow_governance).

% DUAL FORMULATION NOTE:
% Currency regime constraints decompose into two related but distinct stories: (1) Currency Regime Institutional Structure (this story) — the formal legal and institutional arrangements that enforce the peg and constrain policy autonomy, ε ≈ 0.58. (2) Elite Cognitive Capture (downstream) — the identity fusion of North African monetary elites with external anchors, making alternatives cognitively unavailable even if institutionally possible, ε ≈ 0.72. Both stories affect the broader postcolonial institutional path dependency constraint, which influences resource extraction regimes in the Sahel region.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(north_african_currency_regimes, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
