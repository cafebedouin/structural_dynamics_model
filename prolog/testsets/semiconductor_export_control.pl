% ============================================================================
% CONSTRAINT STORY: semiconductor_export_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_semiconductor_export_control, []).

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
 *   constraint_id: semiconductor_export_control
 *   human_readable: Semiconductor Export Control Regime
 *   domain: geopolitical_economics/technology_governance
 *
 * SUMMARY:
 *   Semiconductor export controls represent a hybrid coordination-extraction
 *   mechanism imposed by technologically leading states to manage
 *   geopolitical advantage. The regime emerged from legitimate Cold War
 *   military security concerns (preventing Soviet access to cutting-edge
 *   computing) but has evolved into a complex institutional structure that
 *   simultaneously coordinates trusted trade relationships among allies and
 *   extracts technological capacity from restricted states. The constraint
 *   exhibits strong perspectival divergence: restricted states experience
 *   pure extraction (Snare); leading manufacturers experience market
 *   segmentation benefit (Rope); allied nations experience mixed coordination
 *   and extraction (Tangled Rope); and the institutional apparatus itself
 *   shows signs of piton dynamics — increasing theater as verification costs
 *   accumulate relative to security gains. The extractiveness score (0.58)
 *   reflects moderate-to-high asymmetric distribution: restricted states bear
 *   maximum costs with zero benefit; leading manufacturers capture most
 *   gains; allied industries experience mixed extraction and coordination;
 *   and the regime's effectiveness decays as domestic alternatives mature.
 *   The theater ratio (0.55) indicates that the regime is increasingly
 *   performative — Entity List management, compliance certifications, and
 *   interagency reviews consume significant resources while the actual
 *   technical denial effect weakens as chip fabrication becomes more
 *   distributed and autonomous systems more modular.
 *
 * KEY AGENTS:
 *   - Restricted States (powerless/trapped): Primary victims — face absolute barriers to advanced semiconductor access through legitimate channels; bear maximum extraction cost with zero coordination benefit
 *   - Leading Semiconductor Manufacturers (institutional/arbitrage): Primary beneficiaries — control market segmentation, capture high-margin allied markets, maintain technological moat through export enforcement
 *   - Allied Nations' Government (powerful/mobile): Coalition orchestrator (institutional/constrained) — maintains regime through active enforcement but faces economic pressure from allies and technical pressure from diffusing technology
 *   - Allied Downstream Industries (organized/constrained): Secondary beneficiary/victim — benefit from competitor exclusion but face supply uncertainty and geopolitical exposure
 *   - Intermediate Tier Manufacturers (moderate/constrained): Secondary victims — bear disproportionate compliance burden relative to competitive advantage gained
 *   - Institutional Legacy System (institutional/arbitrage): Piton-classified institutional actor — maintains itself through inertia and theater despite declining functional effectiveness
 *   - Analytical Observer (analytical/analytical): Risk of naturalizing contingent institutional arrangement as immutable law of technology diffusion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(semiconductor_export_control, 0.58).
domain_priors:suppression_score(semiconductor_export_control, 0.72).
domain_priors:theater_ratio(semiconductor_export_control, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(semiconductor_export_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(semiconductor_export_control, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(semiconductor_export_control, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(semiconductor_export_control, tangled_rope).
narrative_ontology:human_readable(semiconductor_export_control, "Semiconductor Export Control Regime").
narrative_ontology:topic_domain(semiconductor_export_control, "geopolitical_economics/technology_governance").

domain_priors:requires_active_enforcement(semiconductor_export_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(semiconductor_export_control, controlling_state_security_interests).
narrative_ontology:constraint_beneficiary(semiconductor_export_control, domestic_semiconductor_manufacturers).
narrative_ontology:constraint_beneficiary(semiconductor_export_control, allied_technology_ecosystem).
narrative_ontology:constraint_victim(semiconductor_export_control, restricted_state_technology_access).
narrative_ontology:constraint_victim(semiconductor_export_control, downstream_chip_consuming_industries).
narrative_ontology:constraint_victim(semiconductor_export_control, global_supply_chain_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESTRICTED STATE (SNARE) — Faces absolute barriers to accessing advanced semiconductors through legitimate channels. Export controls create no coordination benefit for restricted states; they are pure extraction of technological capacity. No exit option exists within the constraint's frame. The state bears maximum cost with zero reciprocal benefit.
constraint_indexing:constraint_classification(semiconductor_export_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERMEDIATE-TIER MANUFACTURER (TANGLED ROPE) — Constrained by compliance costs, certification requirements, and customer verification demands. However, also benefits from export control enforcement: competitors in restricted markets cannot access cutting-edge inputs, reducing competitive pressure. Mixed experience — genuine coordination (compliance standards enable trusted trade relationships) plus asymmetric extraction (small manufacturers bear disproportionate compliance burden relative to advantage gained).
constraint_indexing:constraint_classification(semiconductor_export_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEADING MANUFACTURER (ROPE) — Controls which markets can access cutting-edge nodes; export controls enforce market segmentation that protects high-margin markets. Experiences the constraint as coordination: standardized export licensing enables predictable access to allied markets while excluding competitors. Net beneficiary with full exit optionality (can lobby for regime changes, invest in alternative markets, or establish regional production).
constraint_indexing:constraint_classification(semiconductor_export_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALLIED DOWNSTREAM INDUSTRY (TANGLED ROPE) — Automotive, telecommunications, aerospace sectors in allied nations depend on access to advanced chips but face supply uncertainty due to export control volatility. Benefits from market protection (competitors in non-aligned states cannot access same inputs), but constrained by allocation scarcity and geopolitical exposure. Active coordination function (supply agreements with allied manufacturers) plus extraction (price premiums, reduced supply flexibility, geopolitical vulnerability).
constraint_indexing:constraint_classification(semiconductor_export_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ALLIED COALITION STATE (SCAFFOLD) — Uses export controls as temporary coordination tool for managed technological advantage. Maintains regime through active enforcement but faces mounting pressure from contradictory incentives: allies need chips for economic competitiveness, restricted states pursue independent chip fabrication (reducing regime's effect), and costs of verification/enforcement escalate. Possesses agency to modify regime; sees sunset logic as technical alternatives emerge (advanced packaging, modular design, distributed fabrication).
constraint_indexing:constraint_classification(semiconductor_export_control, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: INSTITUTIONAL LEGACY SYSTEM (PITON) — The regulatory apparatus (ECRA, Entity List, CCL categories) persists through institutional inertia. The original coordination function (preventing military-use chip proliferation during centralized computing era) has largely atrophied — modern chip design is distributed, open-source components are pervasive, and advanced packaging decouples node size from capability. The regime maintains itself through theater (compliance certifications, interagency reviews, entity listings) rather than demonstrable security outcomes. Suppliers have internalized the compliance burden as normal operational cost.
constraint_indexing:constraint_classification(semiconductor_export_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — From a pure physics perspective, semiconductors follow Moore's Law decay curves; no state can indefinitely control access to exponentially improving technology. This perspective risks naturalizing the export control regime as inevitable ('technology diffusion is a law of physics'). However, the structural data contradicts the mountain classification — the regime is enforced through active suppression and institutional mechanisms, not through natural limits. The false summit reveals how geopolitical constraints naturalize themselves as technical inevitabilities.
constraint_indexing:constraint_classification(semiconductor_export_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semiconductor_export_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(semiconductor_export_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(semiconductor_export_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(semiconductor_export_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(semiconductor_export_control, TR),
    TR >= 0.70.

:- end_tests(semiconductor_export_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regime extracts technological capacity from restricted states with minimal coordination benefit — this is asymmetric by design. However, the extraction is not total (0.72+) because: (1) domestic chip fabrication is technically possible (reducing effective denial), (2) supply-chain workarounds exist (grey markets, transshipment), and (3) some technologies can be accessed through civilian dual-use channels. The value reflects that the regime functions as rent-extraction more effectively than as technical denial. Over the measurement interval, extractiveness increased from 0.35 to 0.58 as the regime expanded scope (broadening the Entity List, adding advanced packaging to restrictions, enforcing design-rule restrictions). Suppression (0.72): High. The mechanism operates through absolute barriers (licensing requirements, reexport controls, corporate liability for violations). There is no legitimate exit for restricted states — all formal channels are closed. Secondary victims (allied industries, intermediate manufacturers) face high compliance burden and career risk for violations. However, suppression is not maximal (0.85+) because: (1) grey markets function despite enforcement, (2) some personnel mobility allows knowledge transfer, and (3) physics publications are not subject to controls. Theater ratio (0.55): Moderate. The regime dedicates significant resources to verification (Entity List maintenance, interagency reviews, compliance certifications, export license processing) but the functional outcome is increasingly unclear. The theater increased over the interval (0.38 → 0.55) as the technical environment changed — node size became less correlated with strategic capability, open-source design tools proliferated, and distributed fabrication architectures emerged — but compliance infrastructure expanded to maintain appearance of control.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal — from Snare (restricted state perspective) to Rope (leading manufacturer perspective) represents the full range of experienced constraint type. The restricted state sees pure extraction with no coordination function. The leading manufacturer sees pure coordination (market segmentation enables predictable trade). Allied nations see mixed coordination (shared security interest, trusted supply relationships) and extraction (economic cost, supply volatility). The intermediate manufacturer sees mostly extraction with minor coordination benefit. The piton perspective reveals that the institutional apparatus maintains itself through theater even as the functional effect decays. The false-mountain analytical perspective risks naturalizing what is actually a contingent geopolitical arrangement ('technology diffusion is inevitable'). This gap reveals that no single type captures the constraint — it is genuinely hybrid from some perspectives and genuinely extractive from others. The classification is not wrong; it is position-dependent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) range from 0.95 (restricted states = maximum victims) to 0.10 (leading manufacturers = maximum beneficiaries). For restricted states (powerless, trapped), d approaches 1.0 — the sigmoid f(d) produces high effective extractiveness. For leading manufacturers (institutional, arbitrage), d approaches 0.0 — they experience negative or minimal extraction (the constraint subsidizes them through market segmentation). Allied industries (organized, constrained) occupy middle range: d ≈ 0.45-0.55. They benefit from competitor exclusion but face supply constraints and geopolitical exposure — the benefits and costs roughly balance, producing moderate chi. Intermediate manufacturers (moderate, constrained) have d ≈ 0.60 — they're closer to victims than beneficiaries despite some competitive protection. The piton institutional actor (institutional, arbitrage) has d ≈ 0.15 — it perpetuates itself through beneficiary relationships with leading manufacturers and government contracts. No directionality overrides are needed because the beneficiary/victim declarations capture the structural relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the tangled_rope classification is structurally correct: the regime DOES possess a genuine coordination function (building trusted trade relationships among allied semiconductor producers and users) AND exhibits clear asymmetric extraction (restricted states lose access, leading manufacturers capture market segmentation rents). The coordination component prevents this from being a pure Snare — there is real cooperation among allies. The extraction component prevents this from being a pure Rope — there is real asymmetry that benefits some and harms others. The piton perspective does not contradict the tangled_rope base classification; rather, it shows that the institutional mechanisms maintaining enforcement are increasingly theatrical (theater_ratio increasing from 0.38 to 0.55). This is consistent with a tangled_rope in decline: the extraction mechanism is becoming less efficient (more costs to maintain), potentially enabling a future sunset if the coalition fragments or technical alternatives mature. The mandatrophy is resolved: the classification is neither 'it's just coordination' (false — beneficiaries and victims are clearly differentiated) nor 'it's just extraction' (false — genuine coordination benefits exist for allied participants). It is genuinely tangled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_chip_fabrication_viability,
    'Can restricted states achieve technological parity through independent domestic chip fabrication within the export control regime''s time horizon?',
    'Tracking of restricted-state CMOS capability (node size, yield, design maturity); comparative analysis of foundry investment trajectories (SMIC, TSMC equivalents in restricted states); assessment of technical barriers vs capital barriers',
    'If YES: export controls create temporary advantage only — regime''s extraction mechanism decays as domestic alternatives mature (sunset becomes real). If NO: export controls provide sustained technological moat — regime is stable extraction mechanism despite institutional decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_chip_fabrication_viability, empirical, 'Whether domestic chip fabrication can achieve technical parity within regime timeline').

omega_variable(
    supply_chain_workaround_feasibility,
    'What fraction of restricted-market semiconductor demand can be met through supply chain workarounds (transshipment, reexport via intermediate states, grey markets, legacy inventory)?',
    'Intelligence assessment of known diversion routes; economic modeling of black-market pricing premiums; forensic analysis of semiconductor provenance in restricted states (require wafer markings, lithography signatures)',
    'If high feasibility (>60%): export controls function more as rent-extraction mechanism than denial mechanism — they don''t prevent access, they create pricing premium (shifts from Snare toward Tangled Rope for restricted state). If low feasibility: controls achieve actual denial — regime is functionally effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supply_chain_workaround_feasibility, empirical, 'Feasibility of supply-chain workarounds in restricted states').

omega_variable(
    regime_verification_saturation,
    'Is the current export control verification infrastructure reaching saturation (compliance costs exceeding enforcement gains) or sustaining exponential scaling?',
    'Trend analysis of Entity List growth rate, interagency review processing time, litigation over classification disputes, compliance cost per transaction in semiconductor sector',
    'If saturation: theater_ratio will increase, piton dynamics strengthen, regime becomes theatrical maintenance rather than functional control. If scaling sustainable: tangled_rope dynamics persist, regime remains effective coordination + extraction hybrid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regime_verification_saturation, empirical, 'Whether export control verification infrastructure sustains or saturates').

omega_variable(
    allied_coalition_cohesion,
    'Will economic pressure on allied states (chip price inflation, supply uncertainty) destabilize the coalition supporting export controls?',
    'Monitoring of alliance-state semiconductor prices vs non-alliance markets; tracking of public statements from allied industry groups; empirical measurement of lobbying intensity for regime relaxation',
    'If destabilization occurs: scaffold perspective confirmed — regime has sunset as coalition fragments. If cohesion holds: regime sustains through shared security incentive despite economic pain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_coalition_cohesion, empirical, 'Long-term cohesion of allied coalition supporting export controls').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(semiconductor_export_control, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(semicon_tr_t0, semiconductor_export_control, theater_ratio, 0, 0.38).
narrative_ontology:measurement(semicon_tr_t3, semiconductor_export_control, theater_ratio, 3, 0.47).
narrative_ontology:measurement(semicon_tr_t6, semiconductor_export_control, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(semicon_be_t0, semiconductor_export_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(semicon_be_t3, semiconductor_export_control, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(semicon_be_t6, semiconductor_export_control, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(semiconductor_export_control, enforcement_mechanism).
narrative_ontology:affects_constraint(semiconductor_export_control, chip_design_rule_restrictions).
narrative_ontology:affects_constraint(semiconductor_export_control, rare_earth_supply_chain_control).
narrative_ontology:affects_constraint(semiconductor_export_control, ai_capability_access_constraints).

% DUAL FORMULATION NOTE:
% Semiconductor export controls can be decomposed into multiple structurally distinct constraints: (1) advanced-node fabrication controls (ε ≈ 0.65, high technical barrier), (2) legacy-chip supply chain controls (ε ≈ 0.25, easily workaroundable), (3) design-rule restrictions (ε ≈ 0.45, moderate enforcement), and (4) packaging/integration controls (ε ≈ 0.35, emerging). This story represents the consolidated regime; specific technology nodes should be evaluated separately if ε measurements diverge significantly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
