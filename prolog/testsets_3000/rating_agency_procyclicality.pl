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
 *   constraint_id: rating_agency_procyclicality
 *   human_readable: Rating Agency Procyclicality in Financial Markets
 *   domain: financial_regulation/credit_markets
 *
 * SUMMARY:
 *   Rating agency procyclicality creates a structural constraint on credit
 *   market stability with asymmetric extraction concentrated on powerless
 *   borrowers and retail investors. The constraint exhibits a clear temporal
 *   cycle: during credit expansions, agencies' backward-looking models
 *   mechanically inflate ratings as default rates fall and collateral values
 *   rise, enabling excessive leverage and credit expansion. During
 *   contractions, the same models mechanically compress ratings as defaults
 *   accelerate and collateral values collapse, triggering forced
 *   deleveraging, margin calls, and amplified selling pressure. This
 *   procyclical feedback loop transforms a genuine coordination mechanism
 *   (credit assessment) into an extraction mechanism during downturns. The
 *   constraint's extractiveness rises sharply from expansion (0.15) through
 *   late expansion (0.32) to peak crisis (0.72), while theater ratio
 *   increases steadily from 0.40 to 0.72 as agencies maintain the facade of
 *   forward-looking assessment while mechanically following backward-looking
 *   data. The 2008 financial crisis and subsequent regulatory reform
 *   (Dodd-Frank, Basel III) provide the empirical basis for classification —
 *   regulatory reforms create alternative risk assessment pathways (stress
 *   testing, countercyclical buffers) that function as a sunset clause on the
 *   ratings-dependent constraint.
 *
 * KEY AGENTS:
 *   - Rating Agencies: Primary beneficiary (institutional/arbitrage) — capture coordination rents during expansion, face regulatory pressure during contraction; arbitrage regulatory capital requirements through ratings
 *   - Retail Investors: Primary victim (powerless/trapped) — depend on ratings; suffer losses from procyclical downgrades; cannot conduct independent assessment
 *   - Borrowers in Credit Markets: Secondary victim (moderate/constrained) — benefit from credit expansion but face covenant breach and refinancing impossibility during downturns
 *   - Large Financial Institutions: Powerful actor (powerful/mobile) — experience both coordination benefits and extraction; have sophisticated alternatives and regulatory arbitrage options
 *   - Financial Regulators: Organized agent (organized/constrained) — implementing post-2008 reforms to decouple bank lending from agency ratings through countercyclical capital rules and stress testing
 *   - Market Stability: Abstract victim (powerless/trapped) — cannot organize or exit; bears full cost of amplified financial cycles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rating_agency_procyclicality, 0.58).
domain_priors:suppression_score(rating_agency_procyclicality, 0.62).
domain_priors:theater_ratio(rating_agency_procyclicality, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rating_agency_procyclicality, extractiveness, 0.58).
narrative_ontology:constraint_metric(rating_agency_procyclicality, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rating_agency_procyclicality, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rating_agency_procyclicality, tangled_rope).
narrative_ontology:human_readable(rating_agency_procyclicality, "Rating Agency Procyclicality in Financial Markets").
narrative_ontology:topic_domain(rating_agency_procyclicality, "financial_regulation/credit_markets").

domain_priors:requires_active_enforcement(rating_agency_procyclicality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rating_agency_procyclicality, rating_agencies).
narrative_ontology:constraint_beneficiary(rating_agency_procyclicality, financial_institutions_at_cycle_peak).
narrative_ontology:constraint_victim(rating_agency_procyclicality, retail_investors).
narrative_ontology:constraint_victim(rating_agency_procyclicality, market_stability).
narrative_ontology:constraint_victim(rating_agency_procyclicality, borrowers_at_cycle_trough).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Powerless retail investors rely on ratings as their primary source of credit risk assessment. Trapped in the constraint through information asymmetry and inability to conduct independent due diligence. Experience full extraction during downturns when ratings collapse after boom-phase inflation. No meaningful exit options — they must either trust the ratings (accepting procyclical risk amplification) or exit securities markets entirely.
constraint_indexing:constraint_classification(rating_agency_procyclicality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Borrowers benefit from loose credit during boom phase (easier access, lower spreads) but face severe extraction during downturns (ratings downgrades trigger covenant breaches, refinancing becomes impossible at any price). Constrained by the structure of corporate debt — cannot easily exit long-term debt contracts. Experience both coordination benefits (credit availability mechanisms) and severe asymmetric extraction (forced defaults when ratings drive lending withdrawal).
constraint_indexing:constraint_classification(rating_agency_procyclicality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Rating agencies experience the constraint as coordination during credit booms. Their procyclical models enable efficient pricing of credit risk by market participants, generating accurate credit risk signals during the expansion phase. Agencies arbitrage regulatory capital requirements through ratings — financial institutions use ratings to optimize capital allocation. Peak-cycle perspective sees this as pure coordination (Rope) because the feedback mechanism is validating their models.
constraint_indexing:constraint_classification(rating_agency_procyclicality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Regulators classify the constraint as a temporary problem being addressed through structural reforms: Dodd-Frank macroprudential rules, stress testing, countercyclical capital buffers, and rating agency oversight mechanisms. These reforms create alternative risk assessment pathways that bypass ratings for regulatory capital purposes. Sunset logic applies — as countercyclical regulation matures and stress testing becomes more sophisticated, the agency procyclicality mechanism loses its regulatory force. Organized agents with exit path.
constraint_indexing:constraint_classification(rating_agency_procyclicality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The foundational credit analysis models (point-in-time vs through-the-cycle distinction, structural procyclicality in rating methodologies) persist through institutional inertia despite decades of documented failure during downturns. Agencies maintain the theatrical appearance of forward-looking assessment while their models mechanically amplify cyclical swings. The piton classification reflects that the primary function (stable long-term credit assessment) has atrophied, replaced by procyclical machine learning and mark-to-market mechanics, yet the institutional structure (agencies as gatekeepers) persists.
constraint_indexing:constraint_classification(rating_agency_procyclicality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Powerful institutions with sophisticated risk management experience the constraint as mixed: genuine coordination (ratings enable efficient market pricing and leverage optimization) layered over asymmetric extraction (agencies' procyclical downgrades trigger forced deleveraging and margin calls, forcing asset liquidation at worst times). Powerful agents have mobile exit options (alternative credit assessment, internal models, wholesale funding) but not freely mobile — regulatory capital requirements mandate rating usage. Experience real benefits during expansion and real extraction during contraction, with agency to navigate both phases through arbitrage.
constraint_indexing:constraint_classification(rating_agency_procyclicality, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% From a civilizational, universal perspective, procyclicality appears as an immutable property of backward-looking credit assessment: any model based on historical data will mechanically inflate risk during downturns when defaults accelerate, creating feedback loops inherent to how credit risk is measured. This perspective risks naturalizing a contingent institutional choice (backward-looking models, regulatory mandate for agency ratings, mark-to-market accounting) as a law of financial nature. Engine false summit detection will identify the naturalization.
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
    constraint_indexing:constraint_classification(rating_agency_procyclicality, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.58): Substantial. The constraint's base extraction is moderate-to-high because agencies capture rents through regulatory dependency — all market participants must use ratings for compliance purposes, and agencies face limited competition and reputational consequence. The backward-looking models amplify this by creating informational asymmetry: agencies control the data and methodology, retail investors cannot verify assessments. Suppression (0.62): High. Significant barriers to exiting agency dependency include regulatory mandates (bank capital rules, insurance regulations, pension fund rules require rating-based constraints), informational barriers (retail investors cannot conduct credit analysis independently), and institutional path dependence (legal documents reference ratings, making ratings the coordinating standard). However, suppression is not total — sophisticated institutions have alternatives, and regulatory reforms are creating exit pathways through countercyclical capital frameworks and stress testing. Theater ratio (0.65): Moderate-high. Agencies present their methodologies as forward-looking and economically rational while mechanically applying backward-looking model outputs. The performative element increases during downturns when agencies publicly justify massive downgrades as 'reflecting deteriorating fundamentals' rather than 'our models mechanically respond to data we cannot predict.' Theater increases over the interval as regulatory scrutiny forces more elaborate justification narratives.
 *
 * PERSPECTIVAL GAP:
 *   The procyclicality constraint creates one of the largest perspectival gaps in financial regulation. The beneficiary agency's peak-cycle Rope perspective is structurally accurate during expansions — the coordination function is real, the models are generating valid market-clearing prices, and the efficiency gains from centralized credit assessment are genuine. But the same institutional arrangement becomes a Snare to powerless investors in the contraction phase. The mountain perspective (procyclicality as natural law of backward-looking assessment) naturalizes a choice: agencies could adopt through-the-cycle methodologies, regulators could mandate forward-looking models, or institutional design could decouple lending from ratings. The scaffold perspective (sunset through macroprudential reform) is the regulatory hypothesis being tested post-2008. The piton perspective (rating methodologies persist despite documented failure) reflects that rating agencies, despite massive reputational damage in 2008, have not fundamentally changed backward-looking methodologies — the institutional structure survives through regulatory dependency and path dependence.
 *
 * DIRECTIONALITY LOGIC:
 *   Rating agencies derive d from their beneficiary status + arbitrage options (regulatory mandate ensures demand) → d ≈ 0.10 → f(d) ≈ -0.05 → negative χ. They benefit from the constraint. Retail investors derive d from victim status + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → maximum χ ≈ 0.82 (at global scope). They bear extraction. Borrowers derive d from victim status + constrained exit → d ≈ 0.70 → f(d) ≈ 1.00 → χ ≈ 0.58 (at national scope). Mixed experience. Powerful institutions derive d from mixed beneficiary/victim status + mobile exit → d ≈ 0.50 → f(d) ≈ 0.65 → χ ≈ 0.38 (at global scope). Navigable constraint. Regulators derive d from organized agent status attempting to decompose the extraction → d ≈ 0.55 → f(d) ≈ 0.75 → but organizational power reduces realized extraction. No beneficiary/victim declaration applies to regulators as structural actors — they are attempting to alter the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that procyclicality exhibits genuine coordination function (efficient credit pricing during expansions, valid risk signal during contractions) layered over genuine extraction (powerless agents bear amplified downside, no mechanism for risk-sharing). The tangled_rope classification is appropriate: the constraint requires active enforcement (regulatory mandates, institutional path dependence, standard-setting), possesses both beneficiaries (agencies, lenders at cycle peak) and victims (borrowers at cycle trough, retail investors). The scaffold perspective is structurally sound — countercyclical capital requirements, stress testing, and rating agency oversight create alternative coordination pathways that reduce reliance on backward-looking ratings. The piton classification for traditional methodologies reflects that through-the-cycle analysis has been theoretically superior for 30+ years, yet backward-looking rating models persist through regulatory dependency and institutional inertia, not because they work better. False summit detection for the mountain perspective: procyclicality appears immutable only if one naturalizes backward-looking methodologies as necessary; alternative methodologies exist and are being implemented, making this a contingent institutional choice rather than natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    point_in_time_vs_through_cycle,
    'Is procyclicality an inherent property of backward-looking risk measurement or a feature of regulatory-mandated ratings usage?',
    'Comparison of through-the-cycle rating stability in non-regulated credit assessment (private lending, peer-to-peer) vs regulatory-mandated ratings in public markets; analysis of whether voluntary adoption of forward-looking models eliminates procyclicality or merely redistributes it',
    'If inherent: mountain classification gains support. If regulatory-mandate feature: procyclicality is contingent institutional choice, mountain is false summit, scaffold sunset logic becomes more plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(point_in_time_vs_through_cycle, empirical, 'Whether procyclicality is inherent to backward-looking models or a regulatory mandate artifact').

omega_variable(
    countercyclical_regulation_effectiveness,
    'Do countercyclical capital buffers and macroprudential tools actually decouple bank lending from agency ratings, or do they merely redistribute extraction to new channels?',
    'Empirical test of bank lending volume, loan terms, and asset sales during regulatory tightening phases; measurement of whether macroprudential tools reduce forced selling driven by rating downgrades',
    'If effective: scaffold perspective validated, sunset logic is real. If ineffective: regulatory reform is theatrical, piton classification strengthened, constraint persists with new mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(countercyclical_regulation_effectiveness, empirical, 'Whether macroprudential regulation breaks the procyclical feedback loop').

omega_variable(
    machine_learning_amplification,
    'Do machine learning-based credit scoring models amplify procyclicality beyond traditional agency methodologies, or do they provide superior signal during downturns?',
    'Historical backtesting of ML-based credit models vs traditional agency ratings during 2008 and subsequent downturns; measurement of prediction error and sensitivity to recent data during stress periods',
    'If amplified: procyclicality is deepening as computational systems replace human judgment, extraction increasing. If superior: constraint is degrading as better models emerge, suggests mountain is temporary piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(machine_learning_amplification, empirical, 'Whether ML credit models amplify or reduce procyclical feedback').

omega_variable(
    structural_vs_behavioral_procyclicality,
    'How much of observed procyclicality is structural (ratings mechanically flow from backward-looking models) vs behavioral (agents overreact to ratings, creating informational cascades)?',
    'Analysis of rating change timing relative to actual credit deterioration; measurement of lending response to ratings vs independent credit metrics; separation of direct effect (lending follows ratings) from amplification (behavioral herding around ratings)',
    'If mostly structural: requires model redesign or regulatory mandate change. If mostly behavioral: educational interventions and alternative signaling mechanisms might reduce extraction without reforming ratings themselves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_vs_behavioral_procyclicality, empirical, 'Decomposition of structural vs behavioral procyclical feedback').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rating_agency_procyclicality, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rapc_tr_t0, rating_agency_procyclicality, theater_ratio, 0, 0.4).
narrative_ontology:measurement(rapc_tr_t3, rating_agency_procyclicality, theater_ratio, 3, 0.58).
narrative_ontology:measurement(rapc_tr_t5, rating_agency_procyclicality, theater_ratio, 5, 0.65).
narrative_ontology:measurement(rapc_tr_t8, rating_agency_procyclicality, theater_ratio, 8, 0.72).

% Extraction over time
narrative_ontology:measurement(rapc_be_t0, rating_agency_procyclicality, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(rapc_be_t3, rating_agency_procyclicality, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(rapc_be_t5, rating_agency_procyclicality, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(rapc_be_t8, rating_agency_procyclicality, base_extractiveness, 8, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rating_agency_procyclicality, information_standard).
narrative_ontology:boltzmann_floor_override(rating_agency_procyclicality, 0.08).
narrative_ontology:affects_constraint(rating_agency_procyclicality, leverage_procyclicality).
narrative_ontology:affects_constraint(rating_agency_procyclicality, collateral_value_feedback).
narrative_ontology:affects_constraint(rating_agency_procyclicality, regulatory_arbitrage_in_capital_markets).

% DUAL FORMULATION NOTE:
% Rating agency procyclicality is a distinct constraint family member, downstream of general procyclicality mechanisms but focused specifically on how information systems (agency ratings) amplify cycles. The upstream constraint (general leverage procyclicality) has higher empirical establishment and affects agency procyclicality through the collateral value feedback loop. The downstream constraint (regulatory arbitrage in capital markets) shows how powerful actors exploit agency procyclicality for extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rating_agency_procyclicality, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
