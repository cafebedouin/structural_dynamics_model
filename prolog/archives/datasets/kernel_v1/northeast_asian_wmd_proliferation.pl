% ============================================================================
% CONSTRAINT STORY: northeast_asian_wmd_proliferation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_northeast_asian_wmd_proliferation, []).

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
 *   constraint_id: northeast_asian_wmd_proliferation
 *   human_readable: Northeast Asian WMD Proliferation Constraint
 *   domain: geopolitical/security/strategic_weapons
 *
 * SUMMARY:
 *   The Northeast Asian WMD proliferation constraint operates as a
 *   multilayered system combining formal treaty law (NPT), economic
 *   enforcement (UN sanctions, secondary sanctions), technological monopoly
 *   (advanced state control of enrichment/reprocessing), and security
 *   guarantee (US extended deterrence). This architecture creates a
 *   structural tension between the stated goal (universal nonproliferation)
 *   and the de facto practice (asymmetric nonproliferation for non-nuclear
 *   states while established powers retain arsenals). The constraint exhibits
 *   all six DR types depending on observer position: for established nuclear
 *   powers, it is pure coordination (Rope) enabling stable deterrence; for
 *   non-nuclear regional states threatened by proliferation, it is extraction
 *   with no exit (Snare); for proliferating states, it is a mixed
 *   coordination-extraction mechanism with active enforcement (Tangled Rope);
 *   for the international nonproliferation regime, it is a temporary
 *   coordination structure with sunset logic as deep intrusive verification
 *   scales (Scaffold); for the NPT treaty text itself, it is an increasingly
 *   performative ritual (Piton); and from a strategic realist perspective, it
 *   appears immutable law (Mountain—false summit). The constraint's
 *   extractiveness has increased over the 1990-2026 interval as proliferant
 *   states accumulate technical capacity while suppression (sanctions,
 *   technology denial, diplomatic isolation) has intensified. Theater ratio
 *   has risen as NPT review conferences debate compliance while the core
 *   disarmament bargain remains unfulfilled.
 *
 * KEY AGENTS:
 *   - Established Nuclear Powers (US, Russia, China, UK, France): Institutional beneficiaries (arbitrage exit) — control legitimate nuclear status, maintain technology monopoly, extract prestige from nonproliferation norm while exempting themselves from disarmament discipline
 *   - Proliferating States (North Korea, Iran, potential others): Powerful but constrained actors (constrained exit) — face existential security threats and pursue deterrent capacity but bear full cost of sanctions, technology denial, and diplomatic isolation
 *   - Non-Nuclear Regional States (South Korea, Japan, other allies): Powerless victims (trapped exit) — bound by NPT commitments and alliance discipline to forgo nuclear weapons while absorbing security costs of regional proliferation
 *   - US Security Guarantor: Institutional beneficiary (arbitrage exit) — maintains extended deterrence commitments that substitute for proliferators' native capability; extracts geopolitical leverage from role as security provider
 *   - International Nonproliferation Regime (IAEA, NSG, MTCR): Organized actors (constrained exit) — coordinate verification and technology control but lack enforcement authority; see constraint as solvable through deeper institutionalization
 *   - NPT Treaty Structure: Institutional actor (arbitrage exit) — maintains treaty framework despite non-compliance by both nuclear powers and proliferators; persists through institutional inertia and lack of alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(northeast_asian_wmd_proliferation, 0.58).
domain_priors:suppression_score(northeast_asian_wmd_proliferation, 0.72).
domain_priors:theater_ratio(northeast_asian_wmd_proliferation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(northeast_asian_wmd_proliferation, extractiveness, 0.58).
narrative_ontology:constraint_metric(northeast_asian_wmd_proliferation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(northeast_asian_wmd_proliferation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(northeast_asian_wmd_proliferation, tangled_rope).
narrative_ontology:human_readable(northeast_asian_wmd_proliferation, "Northeast Asian WMD Proliferation Constraint").
narrative_ontology:topic_domain(northeast_asian_wmd_proliferation, "geopolitical/security/strategic_weapons").

domain_priors:requires_active_enforcement(northeast_asian_wmd_proliferation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(northeast_asian_wmd_proliferation, established_nuclear_powers).
narrative_ontology:constraint_beneficiary(northeast_asian_wmd_proliferation, us_security_guarantor).
narrative_ontology:constraint_beneficiary(northeast_asian_wmd_proliferation, regional_hegemonic_stability).
narrative_ontology:constraint_victim(northeast_asian_wmd_proliferation, proliferating_states).
narrative_ontology:constraint_victim(northeast_asian_wmd_proliferation, non_proliferation_treaty_signatories).
narrative_ontology:constraint_victim(northeast_asian_wmd_proliferation, regional_non_nuclear_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR REGIONAL STATE (SNARE) — Bound by NPT commitments but faces existential security threats from nuclear-armed neighbors. Cannot credibly exit the treaty without catastrophic diplomatic/economic cost; cannot remain secure within treaty bounds. Extraction is maximum: constrained to non-nuclear status while absorbing security costs of regional proliferation.
constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PROLIFERATING STATE (TANGLED ROPE) — Experiences genuine coordination function (deterrence stability via mutual vulnerability) alongside asymmetric extraction (sanctions, technology denial, permanent second-tier military status). High-cost but surmountable exit: program suspension faces domestic legitimacy crisis and reversibility risk. Active enforcement required to maintain constraint.
constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ESTABLISHED NUCLEAR POWER (ROPE) — Primary beneficiary. Controls technology, maintains monopoly on legitimate nuclear status, extracts prestige from NPT architecture while exempting itself from core nonproliferation discipline (continuous disarmament obligation unfulfilled). Experiences constraint as pure coordination: nonproliferation enables stable deterrence ordering.
constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL NONPROLIFERATION REGIME (SCAFFOLD) — IAEA inspections, technology control regimes (NSG), enrichment/reprocessing restrictions create temporary coordination with sunset logic. Organized actors (IAEA, treaty secretariat) see the constraint as solvable through deepening institutions and intrusive verification. Theater is moderate: inspection rituals have genuine verification function but also perform diplomatic theater.
constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NPT TREATY TEXT (PITON) — The 1968 treaty's core bargain (disarmament by nuclear powers in exchange for nonproliferation by others) has become largely performative. Nuclear powers retain arsenals, refuse comprehensive disarmament, yet demand perpetual compliance from non-nuclear states. The treaty persists through institutional inertia and lack of alternatives, not functional legitimacy. Theater ratio high: treaty review conferences debate compliance while the core exchange remains unfulfilled.
constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRATEGIC REALISM VIEW (MOUNTAIN) — From a realist perspective, the constraint appears immutable: security dilemma in a multipolar system drives nuclear acquisition regardless of treaty law. States cannot credibly rely on external security guarantees and will pursue WMD when strategically rational. This perspective naturalizes what is actually a structural policy choice (US extended deterrence, technology denial, sanctions regime). Engine will identify as false summit.
constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(northeast_asian_wmd_proliferation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(northeast_asian_wmd_proliferation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(northeast_asian_wmd_proliferation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(northeast_asian_wmd_proliferation, TR),
    TR >= 0.70.

:- end_tests(northeast_asian_wmd_proliferation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting asymmetric imposition. Established nuclear powers extract prestige, deterrence ordering, and geopolitical leverage from nonproliferation norms while systematically violating disarmament obligations. Proliferating states face substantial extraction: complete technology denial, comprehensive sanctions, diplomatic isolation, permanent military subordination. Non-nuclear regional states face extraction via forced dependency on US security guarantee (substitute for native deterrent capability). The value reflects that some coordination function exists (mutual deterrence stability) alongside significant asymmetry. Suppression (0.72): High. Enforcement mechanisms include UN sanctions, secondary sanctions (US IEEPA, EU asset freezes), IAEA inspection regimes, NSG export controls, MTCR missile technology restrictions, and diplomatic isolation. These create substantial barriers to proliferant acquisition. However, suppression is not absolute: sanctions-busting networks operate (dual-use technology procurement, shell company financing), some states provide alternative supply (sanctions-evasion coalitions), and intrusive inspection faces sovereignty constraints. Theater ratio (0.65): Moderate-high. NPT review conferences produce lengthy declarations on disarmament while established powers defer implementation. IAEA inspections have genuine verification function but also perform diplomatic reassurance theater. Sanctions enforcement shows political theater (announcing penalties, publicizing designations) alongside material enforcement. The ratio reflects that significant performative activity masks incomplete structural compliance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence. For established nuclear powers, the constraint is Rope—pure coordination that enables stable mutual deterrence. Their incentives align with nonproliferation. For non-nuclear regional states, the constraint is Snare—they have no exit that doesn't incur catastrophic cost. For proliferating states, it is Tangled Rope—they experience both the genuine coordination function (deterrence stability against neighbors) and the extraction function (sanctions, technology denial, permanent subordination). For the international regime, it is Scaffold—organized actors believe intrusive verification (deeper IAEA protocols, additional signatories to verification frameworks) can eventually solve the problem. For the NPT text, it is Piton—the treaty persists through institutional habit despite its core bargain failing. For the strategic realist, it appears Mountain—the security dilemma appears immutable, proliferation seems inevitable. This gap arises from differences in exit options (arbitrage for beneficiaries, trapped for victims), power (institutional for beneficiaries, powerless/moderate for victims), and time horizons (immediate for beneficiaries, biographical/generational for victims). The false summit diagnosis applies to the Mountain perspective: what appears as inevitable strategic logic is actually a contingent policy architecture (US extended deterrence + technology denial + sanctions + NPT legitimacy).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) vary sharply across institutional and state perspectives. Established nuclear powers occupy d ≈ 0.05 (full beneficiary): they benefit from NPT legitimacy while exempting themselves from core obligations. The US security guarantor occupies d ≈ 0.10 (beneficiary with slight extraction): extended deterrence commitment has costs (credibility maintenance, potential entanglement) but generates substantial geopolitical returns. Proliferating states occupy d ≈ 0.85 (strong victim): they bear sanctions costs, technology denial, and forced military subordination while the security environment (threats from neighbors) remains unchanged. Non-nuclear regional states occupy d ≈ 0.78 (victim): they are constrained to non-nuclear status but do not face direct sanctions; the extraction is indirect (forced dependency on external guarantor, vulnerability to guarantee withdrawal). The IAEA and international regime occupy d ≈ 0.65 (moderate victim): they lack enforcement authority and absorb compliance failure costs through institutional legitimacy erosion. The NPT text itself occupies d ≈ 0.70 (victim): the treaty's core bargain has failed to deliver disarmament, yet the treaty persists and is used to enforce nonproliferation, exposing it to legitimacy challenges. The analytical observer's Mountain perspective occupies canonical d ≈ 0.73, but the false summit detection will identify that d as misapplied because beneficiaries (established powers) and victims (proliferators, non-nuclear states) exist and are identifiable.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the Tangled Rope classification correctly captures the dual structure: genuine coordination function (mutual deterrence stability via nonproliferation) exists alongside asymmetric extraction (benefits accrue to established powers, costs imposed on proliferators). The constraint is not reducible to pure coordination (Rope) because the victims perceive Snare conditions (trapped, no exit, maximum extraction); it is not pure extraction (Snare) because some mutual stability benefit exists. The false summit diagnosis on the Mountain perspective reveals that the realist claim ('security dilemma is immutable') naturalizes a contingent policy architecture. If the US extended deterrence guarantee were withdrawn, or if technology denial regimes collapsed, or if sanctions eroded, the apparent immutability would evaporate—the constraint would transform to Tangled Rope from all perspectives. The Piton diagnosis on the NPT text reveals that performative compliance (review conferences, inspection rituals) has partially decoupled from functional constraint enforcement. Theater ratio at 0.65 indicates that ~35% of observed activity is non-performative enforcement, while ~65% is ritual. The Scaffold perspective shows that organized actors (IAEA, NSG) believe the constraint is temporally solvable through deeper institutionalization and intrusive verification—sunset logic applies if verification capacity scales and compliance improves. Mandatrophy is resolved by recognizing that the constraint performs different functions for different actors: coordination for beneficiaries (Rope), extraction for victims (Snare/Tangled Rope), aspiration for regime builders (Scaffold), ritual for the treaty text (Piton), and apparent law for the unattentive observer (Mountain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    us_extended_deterrence_credibility,
    'How credible is the US security guarantee for regional non-nuclear states, and does guarantee credibility vary by alliance formality or demonstrated commitment?',
    'Historical analysis of US response to regional security threats; correlation between alliance treaty status and actual US military intervention; regional state survey data on confidence in guarantee',
    'High credibility: non-nuclear state exits (constrained rather than trapped); constraint classified as Tangled Rope from their perspective. Low credibility: state perceives guarantee as cheap talk; constraint becomes Snare regardless of treaty language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_extended_deterrence_credibility, empirical, 'Credibility of US extended deterrence in Northeast Asia').

omega_variable(
    technology_denial_enforcement_capacity,
    'How effectively do technology denial regimes (NSG, IAEA safeguards, MTCR) actually prevent proliferant access to weapons-grade materials and delivery systems, versus merely increasing cost and timeline?',
    'Technical analysis of proliferant procurement networks; comparison of enrichment/reprocessing timelines with and without sanctions; assessment of dual-use technology leakage rates',
    'If denial is effective: suppression is structural (genuine barrier), constraint is Snare from proliferant perspective. If denial merely delays: suppression is stageable, constraint degrades toward Tangled Rope or Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_denial_enforcement_capacity, empirical, 'Effectiveness of international technology denial regimes').

omega_variable(
    regional_threat_perception_dynamics,
    'Is the regional security dilemma (each state''s weapons acquisition triggering neighbors'' acquisition) inevitable given geography and history, or contingent on specific diplomatic failures and alliance instability?',
    'Scenario analysis of alternative diplomatic architectures (multilateral security pacts, mutual inspection regimes, regional disarmament agreements); historical counterfactual on pre-1990s threat perception trajectories',
    'If inevitable: constraint is truly Mountain-like (realism view correct). If contingent: constraint is structurally alterable Tangled Rope, and false summit diagnosis applies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_threat_perception_dynamics, conceptual, 'Whether regional security dilemma is structurally inevitable or diplomatically contingent').

omega_variable(
    npt_disarmament_obligation_symmetry,
    'Do established nuclear powers'' systematic non-compliance with Article VI (continuous disarmament obligation) delegitimize the entire nonproliferation bargain, or is the norm separation (disarmament as aspirational, nonproliferation as mandatory) stable?',
    'Comparative analysis of nuclear arsenal trends 1970-2026; examination of NPT review conference language on disarmament vs enforcement; regional state statements on treaty legitimacy and compliance intent',
    'If delegitimized: NPT constraint becomes theater-heavy Piton or fails entirely; proliferant states claim symmetrical non-compliance. If stable: asymmetry persists and constraint functions as intended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(npt_disarmament_obligation_symmetry, conceptual, 'Legitimacy of asymmetric disarmament vs nonproliferation obligations in NPT').

omega_variable(
    sanctions_regime_sustainability,
    'Can international sanctions on proliferators be sustained indefinitely without gradual erosion through sanctions-busting trade networks, secondary sanctions fatigue, and geopolitical realignment?',
    'Time-series analysis of sanctions effectiveness (1990-2026); identification of sanctions-busting network density and secondary market pricing; examination of coalition cohesion (US-EU-Japan) under geopolitical strain',
    'If sustainable: suppression remains high (0.70+), constraint retains enforcement capacity. If eroding: suppression gradually declines toward 0.50, constraint transforms from Snare toward Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctions_regime_sustainability, empirical, 'Long-term sustainability of international sanctions regimes on proliferators').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(northeast_asian_wmd_proliferation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neawmd_tr_t0, northeast_asian_wmd_proliferation, theater_ratio, 0, 0.5).
narrative_ontology:measurement(neawmd_tr_t15, northeast_asian_wmd_proliferation, theater_ratio, 15, 0.62).
narrative_ontology:measurement(neawmd_tr_t30, northeast_asian_wmd_proliferation, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(neawmd_be_t0, northeast_asian_wmd_proliferation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(neawmd_be_t15, northeast_asian_wmd_proliferation, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(neawmd_be_t30, northeast_asian_wmd_proliferation, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(neawmd_su_t0, northeast_asian_wmd_proliferation, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(neawmd_su_t15, northeast_asian_wmd_proliferation, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(neawmd_su_t30, northeast_asian_wmd_proliferation, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(northeast_asian_wmd_proliferation, enforcement_mechanism).
narrative_ontology:affects_constraint(northeast_asian_wmd_proliferation, us_extended_deterrence_northeast_asia).
narrative_ontology:affects_constraint(northeast_asian_wmd_proliferation, technology_denial_regimes).
narrative_ontology:affects_constraint(northeast_asian_wmd_proliferation, regional_security_dilemma).

% DUAL FORMULATION NOTE:
% The WMD proliferation constraint is structurally dependent on US extended deterrence (which provides security substitute for non-nuclear weapons) and technology denial regimes (which create material barriers to acquisition). These upstream constraints have distinct extractiveness values: extended deterrence is approximately Rope (coordination), technology denial is approximately Snare (pure extraction mechanisms). The WMD proliferation constraint is their downstream effect—the combined impact of the upstream mechanisms produces the observed Tangled Rope classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(northeast_asian_wmd_proliferation, institutional, 0.08).
constraint_indexing:directionality_override(northeast_asian_wmd_proliferation, powerful, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
