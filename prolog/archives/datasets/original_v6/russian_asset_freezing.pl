% ============================================================================
% CONSTRAINT STORY: russian_asset_freezing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_russian_asset_freezing, []).

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
 *   constraint_id: russian_asset_freezing
 *   human_readable: Russian Asset Freezing and Sanctions Compliance
 *   domain: geopolitical/economic_sanctions
 *
 * SUMMARY:
 *   Russian asset freezing represents a coordinated multilateral sanction
 *   mechanism deployed following Russia's invasion of Ukraine in February
 *   2022. The constraint operates across multiple institutional levels:
 *   state-to-state sanctions coordination (G7, EU, UN-aligned powers),
 *   financial system enforcement (banks, asset custodians, compliance
 *   infrastructure), and individual asset holder experience (Russian citizens
 *   and entities with external wealth). The constraint exhibits genuine
 *   coordination function (preventing asset-laundering, maintaining unified
 *   geopolitical position, creating incentive structure for negotiation)
 *   alongside asymmetric extraction (from Russian asset holders who face
 *   total capital confiscation, from third-party neutral traders facing
 *   compliance friction, and from the global financial system bearing
 *   clearing and verification costs). The rising extractiveness over time
 *   (0.35 → 0.58) reflects a shift from initial broad-based coordination
 *   (clear sanctions signal) toward deeper institutional embedding with
 *   increasing performative compliance theater (0.25 → 0.55). The constraint
 *   is structurally a Tangled Rope from the analytical perspective:
 *   coordination benefits are real (prevents evasion, maintains allied
 *   position, creates incentive structure) but are coupled with asymmetric
 *   extraction and high suppression. The identity-locked perspective on the
 *   Russian state reveals a critical structural feature: the constraint may
 *   have become self-sustaining through identity fusion — the Russian state's
 *   legitimacy is now constituted partly through defiance of the sanctions
 *   regime, making negotiated exit politically infeasible even if materially
 *   advantageous.
 *
 * KEY AGENTS:
 *   - Frozen Russian Asset Holders: Primary victims (powerless/trapped) — face total capital seizure with no legal exit mechanism
 *   - Third-Party Trade Partners: Secondary victims (moderate/constrained) — bear compliance costs and supply chain friction; constrained by secondary sanctions risk
 *   - Sanctioning Western Governments: Primary beneficiaries (institutional/arbitrage) — capture geopolitical leverage, asset recovery, and economic pressure on adversary; can exit unilaterally
 *   - Financial Compliance Apparatus: Institutional actor maintaining performative enforcement (institutional/constrained) — supervision, verification, and reporting infrastructure
 *   - International Sanctions Coalition: Organized institutional actors (organized/constrained) — G7, EU, multilateral bodies; see constraint as temporary coordination with embedded sunset
 *   - Russian State: Captured institutional actor (institutional/constrained, identity_locked) — high suppression and structural resistance; constraint may be self-sustaining through identity fusion
 *   - Global Financial System: Distributed victim (analytical/analytical) — bears efficiency costs and transaction friction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(russian_asset_freezing, 0.58).
domain_priors:suppression_score(russian_asset_freezing, 0.72).
domain_priors:theater_ratio(russian_asset_freezing, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(russian_asset_freezing, extractiveness, 0.58).
narrative_ontology:constraint_metric(russian_asset_freezing, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(russian_asset_freezing, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(russian_asset_freezing, tangled_rope).
narrative_ontology:human_readable(russian_asset_freezing, "Russian Asset Freezing and Sanctions Compliance").
narrative_ontology:topic_domain(russian_asset_freezing, "geopolitical/economic_sanctions").

domain_priors:requires_active_enforcement(russian_asset_freezing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(russian_asset_freezing, sanctioning_western_governments).
narrative_ontology:constraint_beneficiary(russian_asset_freezing, downstream_asset_claimants).
narrative_ontology:constraint_victim(russian_asset_freezing, russian_asset_holders).
narrative_ontology:constraint_victim(russian_asset_freezing, global_financial_system_efficiency).
narrative_ontology:constraint_victim(russian_asset_freezing, third_party_trade_partners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FROZEN ASSET HOLDER (SNARE) — Russian citizens and entities with frozen assets face total exit blockade. No legal mechanism to recover assets without political resolution of sanctions. Complete suppression: assets are inaccessible, bank accounts locked, investment portfolios seized. The constraint is purely extractive from this position — coordination benefits are zero, extraction is maximal and unavoidable.
constraint_indexing:constraint_classification(russian_asset_freezing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THIRD-PARTY TRADE PARTNER (TANGLED ROPE) — Non-sanctioning countries and neutral trade partners benefit from sanctions enforcement (reduced Russian competition, market consolidation) while bearing coordination costs (supply chain disruption, clearing complexity, compliance burden). Exit options are constrained by downstream liability: violating sanctions triggers secondary sanctions. Mixed experience: genuine coordination function (preventing asset-laundering through neutral zones) alongside asymmetric extraction (resources diverted to compliance infrastructure).
constraint_indexing:constraint_classification(russian_asset_freezing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: SANCTIONING GOVERNMENT (ROPE) — Western governments experience asset freezing as a coordination mechanism: unified enforcement prevents sanctions evasion and demonstrates unified geopolitical position. Benefits are substantial (geopolitical leverage, asset recovery for restitution, economic pressure on adversary). Exit options are high (can lift sanctions unilaterally). Extraction experienced by this agent is negative — the constraint subsidizes their position. Pure coordination reading from this structural location.
constraint_indexing:constraint_classification(russian_asset_freezing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCIAL COMPLIANCE APPARATUS (PITON) — Banks, lawyers, compliance officers, and regulatory agencies maintain elaborate verification and reporting procedures that detect and report Russian asset holdings. Theater ratio is high (0.55): much compliance activity is performative documentation rather than substantive prevention. The apparatus has degraded from functional enforcement (initially tight controls in 2022) to ritualized compliance theater (standardized procedures, checkbox verification) maintained by institutional inertia. Sunset of functional enforcement reveals piton classification: the machinery persists not because it prevents evasion effectively but because dismantling it signals weakness.
constraint_indexing:constraint_classification(russian_asset_freezing, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SANCTIONS COALITION (SCAFFOLD) — Multilateral enforcement structures (UN, EU, G7) see asset freezing as a temporary coordination mechanism with embedded sunset logic. The coalition's explicit position is that sanctions are reversible upon political settlement. Theater is moderate (0.55) — genuine enforcement function exists (asset recovery) but is coupled with ritual demonstration of unity. Sunset clause is structural: sanctions are intended to incentivize Russian behavioral change, implying eventual termination upon compliance or negotiated settlement. Coalition perspective sees the constraint as temporary coordination, not permanent extraction.
constraint_indexing:constraint_classification(russian_asset_freezing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: RUSSIAN STATE (TANGLED ROPE / IDENTITY-LOCKED) — The Russian state experiences asset freezing with high suppression (0.72) but also real coordination function: asset freezing creates incentive structure for negotiation and demonstrates costs of sanctions. Exit options are constrained by political viability (lifting sanctions without concessions is domestically impossible, accepting sanctions is costly). The constraint is identity-locked at the state level: the Russian government's legitimacy is now partially constituted through defiance of the sanctions regime. This produces an inversion — the constraint becomes self-sustaining because rejecting it would require the state to adopt the sanctioners' framing (that Russian actions were wrong). Structural mobility exists (negotiate, comply, seek settlement) but is perceptually unavailable from within the identity frame.
constraint_indexing:constraint_classification(russian_asset_freezing, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — An observer might claim that state sovereignty and property rights create an immutable natural law preventing asset seizure without due process. However, the structural data contradicts this: asset freezing is a contingent legal mechanism, not a law of nature. It succeeds through coordinated institutional commitment, not through physical or logical necessity. The engine will flag this as a false summit: naturalization of a reversible policy choice as an inescapable law.
constraint_indexing:constraint_classification(russian_asset_freezing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(russian_asset_freezing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(russian_asset_freezing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(russian_asset_freezing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(russian_asset_freezing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(russian_asset_freezing, TR),
    TR >= 0.70.

:- end_tests(russian_asset_freezing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant value from frozen asset holders (capital confiscation without due process or compensation), from third-party traders (compliance friction, supply chain costs), and from the financial system (processing overhead). However, extractiveness is not maximal (0.70+) because genuine coordination function exists: the sanctions regime does prevent asset-laundering, does create material incentive structure for negotiation, and does maintain allied coherence. The trajectory from 0.35 to 0.58 reflects deepening institutional embedding — initial coordination function has been layered with increasing compliance theater. Suppression (0.72): High. Asset holders face total capital lockdown with no legal exit or recovery mechanism. Third-party traders face secondary sanctions threat (material barrier to exit). The Russian state faces domestic political costs to settlement that exceed the material costs of sanctions. Financial institutions face regulatory penalties for non-compliance (institutional suppression). Theater ratio (0.55): Moderate. The constraint retains genuine functional content (asset detection, clearing, incentive alignment) but increasingly manifests as ritualized compliance procedures (standardized reporting, checkpoint verification, institutional signaling). The theater trajectory (0.25 → 0.55) indicates the constraint's functional mechanism is degrading: initial enforcement success created conditions for evasion; enforcement has become increasingly performative (going through the motions) to maintain the illusion of effectiveness. Claimed type: Tangled Rope. Satisfies the requirement for both genuine coordination function (preventing evasion, maintaining allied position) and asymmetric extraction (capital confiscation, third-party friction, identity-lock in the Russian state).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. From the frozen asset holder's position (powerless/trapped), the constraint is pure Snare: maximum extraction, zero coordination benefit, total suppression. From the sanctioning government's position (institutional/arbitrage), the constraint is pure Rope: coordination function dominates, extraction runs toward them, exit options are complete. From the third-party trader's position (moderate/constrained), the constraint is Tangled Rope: genuine coordination function mixed with real extraction burden. From the compliance apparatus's position (institutional/constrained), the constraint is increasingly Piton: theater ratio rising, functional content degrading, constraint persists through institutional inertia. From the Russian state's position (institutional/identity-locked), the constraint is identity-locked Tangled Rope: material incentives for exit exist but are perceptually unavailable because accepting settlement would require the state to adopt the sanctioners' moral framing. The widest gap exists between perspectives 1 and 3: the powerless frozen holder experiences Snare while the sanctioning government experiences Rope — the same constraint, same metrics, opposite classifications based on structural position. This gap illustrates the perspectival nature of DR classification: the constraint is not intrinsically one type but manifests different types depending on where the observer sits.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation operates through each agent's structural position relative to extraction flow. Frozen asset holders (powerless/trapped) derive maximum d from victim status and zero exit capacity → d ≈ 0.95, producing high f(d) ≈ 1.42 and high experienced extraction. Sanctioning governments (institutional/arbitrage) derive minimum d from beneficiary status and maximal exit capacity → d ≈ 0.05, producing negative f(d) ≈ -0.12 and net subsidy to their position. Third-party traders (moderate/constrained) occupy middle position: constrained exit reduces their power relative to powerless agents, but their status as partial beneficiaries (market consolidation, competition reduction) and partial victims (compliance costs) produces d ≈ 0.65 → f(d) ≈ 1.00. The Russian state (institutional/constrained, identity_locked) derives d ≈ 0.60 from high suppression and victim status, modified upward by identity-lock (the agent's self-concept requires rejecting the constraint, making negotiated exit structurally unavailable despite potential material benefit). The identity-locked modifier increases effective d beyond what constrained exit alone would produce, because the agent cannot perceive the exit option from within their identity frame.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED. The constraint resolves the mandatrophy by disambiguating which perspectives legitimately perceive 'pure' types (Snare for frozen holders, Rope for sanctioning governments) and which perspectives perceive the mixed type (Tangled Rope for third parties, Russian state, and analytical observer). The extractiveness value (0.58) falls in the Tangled Rope range (0.40 ≤ χ ≤ 0.90 after directionality scaling), confirming that the analytical perspective's classification is Tangled Rope, not a mislabeled Snare or Rope. The mandate for Tangled Rope requires: (1) beneficiaries — sanctioning governments, yes; (2) victims — frozen holders and third-party traders, yes; (3) active enforcement — yes, the constraint requires ongoing institutional enforcement; (4) both coordination and asymmetric extraction — yes, both present. The theater ratio (0.55) indicates that enforcement functionality is degrading but has not yet reached piton threshold (0.70). The rising theater trajectory suggests the constraint is drifting toward piton over time: if theater reaches 0.70 while extractiveness remains high (0.58), the classification would shift to Piton (former coordination mechanism degraded into ritual). The mandatrophy is resolved at the moment of analysis (extractiveness 0.58 ≤ 0.70), requiring mandatrophy_resolved declaration. If the constraint persists beyond decade 2-3 with theater_ratio > 0.70 and extractiveness remaining high, a follow-up analysis would find it has become Piton: the coordination function (preventing evasion, maintaining allied position) has been replaced by theatrical enforcement while the underlying extraction mechanism (asset confiscation) continues through institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evasion_effectiveness_threshold,
    'What fraction of Russian assets successfully evade detection and freezing, and does this evasion rate exceed the coordination benefits of visible enforcement?',
    'Intelligence assessments of undetected asset flows; comparison of frozen asset value to estimated total Russian external wealth; analysis of evasion pathways (shell companies, cryptocurrency, third-party intermediaries)',
    'If evasion > 40%: asset freezing becomes largely theater, reclassifying toward Piton. If evasion < 15%: coordination function is genuine, stabilizing Tangled Rope. At evasion = 25-35%: mixed functionality — both coordination and theater are real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(evasion_effectiveness_threshold, empirical, 'Effectiveness threshold for asset freezing versus evasion rates').

omega_variable(
    sanctions_compliance_internalization,
    'Have third-party financial institutions internalized sanctions compliance as genuine legal obligation, or is compliance primarily driven by fear of secondary sanctions (external suppression)?',
    'Comparative analysis of compliance patterns in high-secondary-sanctions-risk jurisdictions vs low-risk jurisdictions; interviews with compliance officers; examination of compliance procedures in absence of enforcement pressure',
    'If internalized: suppression is structural (compliance is genuine coordination cost). If externally driven: suppression is performative (compliance is behavioral response to coercion, not value alignment). Affects whether constraint is Tangled Rope (genuine coordination + extraction) or Snare disguised as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctions_compliance_internalization, empirical, 'Whether sanctions compliance reflects internalized norms or external suppression').

omega_variable(
    sunset_viability,
    'Under what conditions would the sanctions regime terminate, and are those conditions structural constraints or political choices?',
    'Analysis of stated settlement conditions by sanctioning powers; comparison to historical precedent for sanctions reversal; examination of whether conditions are materially verifiable or politically negotiated',
    'If conditions are structural/verifiable: scaffold sunset is real, constraint has genuine termination point. If conditions are political: sunset is aspirational, constraint may become permanent piton. Affects whether international coalition truly sees constraint as temporary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_viability, conceptual, 'Whether sanctions sunset conditions are structural or purely political').

omega_variable(
    identity_lock_depth,
    'Is the Russian state''s resistance to sanctions-negotiated settlement driven by material costs analysis or by constitutive identity (the state''s self-concept requires defiance)?',
    'Historical precedent analysis of Russian negotiating positions; internal policy debates if accessible; comparative study of how other sanctioned states have negotiated vs Russia''s specific framing choices',
    'If purely material cost-benefit: state could rationally exit if conditions improve. If identity-locked: even improving external conditions won''t shift position because accepting sanctions settlement would require the state to adopt sanctioners'' moral framing. If identity-locked, constraint is self-sustaining regardless of material incentives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_depth, conceptual, 'Whether Russian resistance reflects material analysis or identity-constituted positions').

omega_variable(
    global_financial_system_efficiency_cost,
    'What is the total economic extraction from the global financial system due to asset freezing compliance costs, clearing delays, and transaction friction?',
    'Calculation of aggregate compliance infrastructure costs (staffing, systems, audits); measurement of transaction latency and friction for international commerce; estimation of trade delays due to sanctions clearance requirements',
    'If total efficiency cost < 0.5% of sanctioning-state GDP: extraction from financial system is acceptable collateral damage. If cost > 2% of global trade: the constraint is extracting significant value from neutral third parties, shifting classification from Tangled Rope toward Snare at the global efficiency perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_financial_system_efficiency_cost, empirical, 'Total economic cost of asset freezing to global financial system efficiency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(russian_asset_freezing, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(raf_tr_t0, russian_asset_freezing, theater_ratio, 0, 0.25).
narrative_ontology:measurement(raf_tr_t6, russian_asset_freezing, theater_ratio, 6, 0.4).
narrative_ontology:measurement(raf_tr_t12, russian_asset_freezing, theater_ratio, 12, 0.55).
narrative_ontology:measurement(raf_tr_t3, russian_asset_freezing, theater_ratio, 3, 0.32).

% Extraction over time
narrative_ontology:measurement(raf_be_t0, russian_asset_freezing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(raf_be_t6, russian_asset_freezing, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(raf_be_t12, russian_asset_freezing, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(raf_be_t3, russian_asset_freezing, base_extractiveness, 3, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(russian_asset_freezing, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(russian_asset_freezing, 0.12).
narrative_ontology:affects_constraint(russian_asset_freezing, secondary_sanctions_compliance).
narrative_ontology:affects_constraint(russian_asset_freezing, cryptocurrency_capital_flight).
narrative_ontology:affects_constraint(russian_asset_freezing, asset_recovery_restitution).

% DUAL FORMULATION NOTE:
% Asset freezing is a single constraint story but depends on three downstream constraints: secondary sanctions (enforcement mechanism), cryptocurrency evasion (rival enforcement pathway), and asset recovery restitution (downstream claim resolution). The network links show how the base constraint influences enforcement complexity across multiple institutional domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(russian_asset_freezing, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
