% ============================================================================
% CONSTRAINT STORY: grey_market_evasion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_grey_market_evasion, []).

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
 *   constraint_id: grey_market_evasion
 *   human_readable: Grey Market Chip Smuggling Through Third-Country Intermediaries
 *   domain: technology_governance/export_control/semiconductor_policy
 *
 * SUMMARY:
 *   Grey market chip smuggling through third-country intermediaries creates a
 *   structural arbitrage opportunity that undermines export control
 *   effectiveness. The constraint operates through jurisdictional
 *   fragmentation: chips subject to US export controls are legally exported
 *   to intermediary jurisdictions (Malaysia, Singapore, UAE, Hong Kong) with
 *   legitimate end-use certificates, then re-exported to sanctioned entities
 *   (Chinese AI labs, military research institutes, Russian defense
 *   contractors) through shell company networks. The smuggling volume is
 *   substantial but difficult to measure precisely — Commerce Department
 *   estimates suggest 20-40% of advanced chips reaching sanctioned Chinese
 *   entities transit through grey market channels, with similar patterns for
 *   other sanctioned destinations. Enforcement actions (entity list
 *   additions, export license denials, criminal prosecutions) have increased
 *   significantly since 2018 but have not suppressed the underlying activity
 *   — smuggling networks adapt faster than enforcement mechanisms can
 *   respond. The constraint exhibits rising extractiveness (0.52 → 0.68 over
 *   2018-2024) as smuggling networks capture increasing monopoly rents,
 *   rising theater ratio (0.38 → 0.61) as entity list designations become
 *   performative rather than functional, and rising suppression requirement
 *   (0.28 → 0.42) as enforcement intensity increases without corresponding
 *   effectiveness gains. The structural delta from the upstream
 *   export_control_reversibility constraint is that grey markets create a
 *   persistent evasion channel that prevents export controls from achieving
 *   their strategic containment objectives, even when the controls are
 *   technically reversible.
 *
 * KEY AGENTS:
 *   - Smuggling Intermediaries: Primary beneficiary (powerful/arbitrage) — capture monopoly rents (2-3x markups) through jurisdictional arbitrage and network effects; powerful through information asymmetry and shell company proliferation
 *   - Transshipment Hub Jurisdictions: Secondary beneficiary (institutional/arbitrage) — Malaysia, Singapore, UAE gain economic activity and logistics infrastructure; arbitrage-grade exit through adjustable enforcement intensity
 *   - Sanctioned End Users: Mixed beneficiary/victim (institutional/constrained) — gain access to controlled technology but bear price premiums, supply uncertainty, and quality risk; constrained by sanctions but not trapped
 *   - Export Control Regime: Primary victim (powerless/trapped) — epistemic commons that cannot organize or exit; bears full cost of evasion through credibility erosion and strategic containment failure
 *   - Compliant Exporters: Secondary victim (moderate/constrained) — face competitive disadvantage and compliance burden; constrained by regulatory obligations and reputational risk
 *   - Enforcement Coalition: Mixed coordinator/victim (organized/constrained) — BIS, OFAC, allied authorities coordinate information sharing but experience extraction through resource constraints and jurisdictional limits
 *   - Entity List Process: Degraded mechanism (institutional/constrained) — formal designation process has atrophied into theater; shell companies proliferate faster than designations can be issued
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(grey_market_evasion, 0.68).
domain_priors:suppression_score(grey_market_evasion, 0.42).
domain_priors:theater_ratio(grey_market_evasion, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(grey_market_evasion, extractiveness, 0.68).
narrative_ontology:constraint_metric(grey_market_evasion, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(grey_market_evasion, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(grey_market_evasion, snare).
narrative_ontology:human_readable(grey_market_evasion, "Grey Market Chip Smuggling Through Third-Country Intermediaries").
narrative_ontology:topic_domain(grey_market_evasion, "technology_governance/export_control/semiconductor_policy").

domain_priors:requires_active_enforcement(grey_market_evasion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(grey_market_evasion, smuggling_intermediaries).
narrative_ontology:constraint_beneficiary(grey_market_evasion, sanctioned_end_users).
narrative_ontology:constraint_beneficiary(grey_market_evasion, transshipment_hubs).
narrative_ontology:constraint_victim(grey_market_evasion, export_control_regime).
narrative_ontology:constraint_victim(grey_market_evasion, compliant_exporters).
narrative_ontology:constraint_victim(grey_market_evasion, strategic_technology_containment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPORT CONTROL REGIME (SNARE) — The regime's effectiveness as a collective good is trapped by the structural arbitrage opportunities grey markets create. Cannot exit the verification crisis; bears full cost of evasion without ability to organize alternative enforcement. Maximum experienced extraction — the regime's credibility erodes while smuggling networks face minimal consequences.
constraint_indexing:constraint_classification(grey_market_evasion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPLIANT EXPORTER (SNARE) — Faces competitive disadvantage against grey market operators who bypass compliance costs. Constrained by regulatory obligations and reputational risk, cannot exit without abandoning the market entirely. Experiences substantial extraction through lost market share and compliance burden while competitors evade.
constraint_indexing:constraint_classification(grey_market_evasion, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TRANSSHIPMENT HUB (ROPE) — Malaysia, Singapore, and similar intermediaries experience the grey market as coordination: facilitating trade flows generates economic activity, employment, and logistics infrastructure development. Net beneficiary with arbitrage-grade exit — can adjust enforcement intensity based on geopolitical pressure without structural penalty.
constraint_indexing:constraint_classification(grey_market_evasion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: SMUGGLING INTERMEDIARY (ROPE) — Experiences the constraint as pure coordination: matching supply (chips subject to export control) with demand (sanctioned end users) through jurisdictional arbitrage. Powerful through network effects and information asymmetry; arbitrage-grade exit through shell company proliferation and route switching.
constraint_indexing:constraint_classification(grey_market_evasion, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SANCTIONED END USER (TANGLED ROPE) — Benefits from access to controlled technology but bears substantial costs: price premiums (2-3x list price), supply uncertainty, quality risk (counterfeit/defective chips), and operational security burden. Constrained by sanctions but not trapped — can pursue indigenous development or alternative suppliers. Mixed coordination (grey market solves access problem) and extraction (monopoly pricing, unreliable supply).
constraint_indexing:constraint_classification(grey_market_evasion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ENFORCEMENT COALITION (TANGLED ROPE) — Organized agents (BIS, OFAC, allied export control authorities) see genuine coordination function (information sharing, entity list updates, multilateral enforcement) but also experience extraction through resource constraints, jurisdictional limits, and political pressure. Constrained by sovereignty boundaries and enforcement capacity; cannot exit without abandoning strategic technology containment mission.
constraint_indexing:constraint_classification(grey_market_evasion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ENTITY LIST PROCESS (PITON) — The formal mechanism for designating bad actors has degraded into theater: shell companies proliferate faster than designations can be issued, and listed entities reconstitute under new names within weeks. The process persists through institutional inertia and political signaling value despite minimal functional deterrence. High theater ratio — the ritual of designation continues while smuggling networks adapt around it.
constraint_indexing:constraint_classification(grey_market_evasion, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, grey market evasion reveals both coordination function (grey markets solve real information and logistics problems in fragmented regulatory environments) and extraction (smuggling networks capture monopoly rents while externalizing strategic risk onto the international system). The constraint is not a natural law — it is a constructed institutional arrangement where enforcement capacity has not kept pace with network complexity.
constraint_indexing:constraint_classification(grey_market_evasion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(grey_market_evasion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(grey_market_evasion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(grey_market_evasion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(grey_market_evasion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(grey_market_evasion, TR),
    TR >= 0.70.

:- end_tests(grey_market_evasion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Smuggling intermediaries capture substantial monopoly rents (2-3x list price markups documented in Commerce Dept investigations) while externalizing strategic risk onto the export control regime. The regime's effectiveness as a collective good erodes — compliant exporters lose market share, sanctioned entities gain access to controlled technology, and strategic containment objectives fail. The extraction is not total (0.68 rather than 0.85+) because some enforcement actions succeed (criminal prosecutions, asset seizures) and some grey market transactions fail (counterfeit chips, interdicted shipments). Suppression (0.42): Moderate. Significant barriers to grey market participation include legal risk (criminal penalties, entity list designation), operational complexity (shell company management, logistics coordination), and information asymmetry (finding reliable smuggling networks). But suppression is not high — the barriers are surmountable for organized networks, and the economic incentives (2-3x markups) justify the risk. Suppression has increased over the interval (0.28 → 0.42) as enforcement intensity has risen, but not enough to suppress the activity. Theater ratio (0.61): Moderate-high. The entity list addition process is substantially performative: Commerce Dept adds dozens of shell companies quarterly, but listed entities reconstitute under new names within weeks. The designation ritual persists for political signaling value despite minimal functional deterrence. Enforcement actions generate headlines but do not meaningfully reduce smuggling volume. The theater has increased over the interval (0.38 → 0.61) as the gap between enforcement activity and effectiveness has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extraction visible from below (Archetype A). The export control regime and compliant exporters see pure extraction (Snare) — they bear costs without corresponding benefits and cannot exit. Smuggling intermediaries and transshipment hubs see coordination (Rope) — they are solving real logistics and information problems and capturing legitimate returns. Sanctioned end users see mixed coordination and extraction (Tangled Rope) — grey markets solve their access problem but extract monopoly rents. The enforcement coalition sees mixed coordination and extraction (Tangled Rope) — multilateral information sharing is genuine coordination, but resource constraints and jurisdictional limits create extraction. The entity list process sees its own degraded ritual (Piton) — the designation mechanism persists through inertia despite minimal functional deterrence. The analytical observer sees the constraint as constructed rather than natural — grey market evasion is not an immutable feature of export control regimes but a consequence of enforcement capacity lagging network complexity. The perspectival gap is wide: beneficiaries experience negative or low extraction while victims experience high extraction, and the gap is not resolvable through better information — it reflects genuine structural asymmetry in who benefits and who pays.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Smuggling intermediaries are primary beneficiaries with arbitrage-grade exit (can switch routes, shell companies, and jurisdictions at will) — low d, negative effective extraction (they experience the constraint as subsidy). Transshipment hubs are secondary beneficiaries with arbitrage-grade exit (can adjust enforcement intensity based on geopolitical pressure) — low d, low or negative effective extraction. Sanctioned end users are mixed beneficiaries/victims with constrained exit (can pursue indigenous development or alternative suppliers but face substantial switching costs) — moderate d, moderate effective extraction. The export control regime is the primary victim with trapped exit (cannot exit the verification crisis or organize alternative enforcement) — high d, maximum effective extraction. Compliant exporters are secondary victims with constrained exit (cannot exit without abandoning the market) — moderate-high d, substantial effective extraction. The enforcement coalition is a mixed coordinator/victim with constrained exit (cannot exit the strategic containment mission but faces resource and jurisdictional limits) — moderate d, moderate effective extraction. The entity list process is a degraded mechanism with constrained exit (institutional inertia prevents abandoning the designation ritual) — moderate d, but classification derives from theater gate rather than high extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (strategic technology containment through export controls) has not outlived its function — the strategic competition that motivates export controls remains active. However, the grey market evasion mechanism has created a persistent gap between the mandate and the constraint's actual operation: export controls are formally in place but functionally evaded at scale. This is not mandatrophy (the mandate is still live) but rather mandate-mechanism divergence: the enforcement mechanism has not kept pace with the evasion mechanism. The rising theater ratio (0.38 → 0.61) reflects this divergence — entity list designations and enforcement actions persist as political signals even as their functional deterrence erodes. The constraint is a snare from the regime's perspective because the evasion mechanism is structural (jurisdictional arbitrage, network effects, information asymmetry) rather than incidental, and the enforcement coalition lacks the resources and jurisdictional reach to suppress it. The omega variables identify the key empirical uncertainties: smuggling volume measurement (how much evasion is grey market vs. other methods?), transshipment hub complicity (passive or active?), enforcement effectiveness threshold (achievable or infeasible?), alternative sourcing substitutability (temporary or structural dependence?), and multilateral coordination potential (can allied regimes close the channels or will networks adapt?).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    smuggling_volume_measurement,
    'What fraction of controlled chips reaching sanctioned end users transit through grey market channels vs. other evasion methods (front companies, false end-user certificates, indigenous production)?',
    'Forensic supply chain analysis; customs data triangulation; chip serial number tracking in seized shipments; comparison of declared exports to third countries vs. their domestic consumption capacity',
    'If grey market share < 30%: extraction is overstated — other evasion methods dominate. If > 70%: grey market is the primary evasion vector and extractiveness may be understated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(smuggling_volume_measurement, empirical, 'Grey market share of total evasion volume').

omega_variable(
    transshipment_hub_complicity,
    'Are transshipment hubs passive facilitators (inadequate enforcement capacity) or active participants (deliberate non-enforcement for economic benefit)?',
    'Comparison of enforcement action rates across jurisdictions with similar trade volumes; correlation between hub jurisdiction economic incentives and enforcement intensity; leaked government communications or policy documents',
    'If passive: coordination story is stronger (hubs face genuine capacity constraints). If active: extraction story is stronger (hubs are beneficiaries, not neutral intermediaries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transshipment_hub_complicity, empirical, 'Whether transshipment hubs are complicit or capacity-constrained').

omega_variable(
    enforcement_effectiveness_threshold,
    'At what enforcement intensity does grey market evasion become economically unviable for smuggling networks?',
    'Historical analysis of enforcement surges (e.g., post-Huawei entity listing) and corresponding smuggling volume changes; cost-benefit modeling of smuggling operations under different interdiction rates',
    'If threshold is achievable with current resources: the constraint is a coordination problem (Tangled Rope from more perspectives). If threshold exceeds feasible enforcement capacity: the constraint is structural extraction (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness_threshold, empirical, 'Enforcement intensity required to suppress grey market activity').

omega_variable(
    alternative_sourcing_substitutability,
    'Can sanctioned end users substitute indigenous chips or non-controlled foreign chips for smuggled advanced chips without significant performance degradation?',
    'Technical analysis of chip performance requirements for sanctioned applications (AI training, supercomputing, advanced weapons systems); assessment of indigenous chip capabilities (SMIC 7nm, Huawei Kirin); time-series tracking of sanctioned entities'' technology advancement rates',
    'If substitutable: grey market extraction is temporary (Scaffold logic — sunset when indigenous production matures). If non-substitutable: grey market extraction is structural (Snare logic — sanctioned entities remain dependent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_sourcing_substitutability, empirical, 'Whether sanctioned end users can substitute alternative chip sources').

omega_variable(
    multilateral_enforcement_coordination,
    'Would coordinated enforcement by allied export control regimes (US, EU, Japan, South Korea, Taiwan) close grey market channels, or would smuggling networks route through non-allied jurisdictions?',
    'Game-theoretic modeling of smuggling network adaptation; historical case studies of multilateral sanctions regimes (Iran nuclear, North Korea); assessment of non-allied transshipment capacity (UAE, Turkey, Russia)',
    'If coordination closes channels: the constraint is a coordination problem with a solution path (Scaffold from enforcement coalition perspective). If networks adapt: the constraint is structural (Snare from regime perspective — enforcement is Sisyphean).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multilateral_enforcement_coordination, conceptual, 'Whether multilateral coordination can suppress grey market evasion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(grey_market_evasion, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(grey_mkt_theater_2018, grey_market_evasion, theater_ratio, 0, 0.38).
narrative_ontology:measurement(grey_mkt_theater_2020, grey_market_evasion, theater_ratio, 2, 0.45).
narrative_ontology:measurement(grey_mkt_theater_2022, grey_market_evasion, theater_ratio, 4, 0.54).
narrative_ontology:measurement(grey_mkt_theater_2024, grey_market_evasion, theater_ratio, 6, 0.61).

% Extraction over time
narrative_ontology:measurement(grey_mkt_extract_2018, grey_market_evasion, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(grey_mkt_extract_2020, grey_market_evasion, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(grey_mkt_extract_2022, grey_market_evasion, base_extractiveness, 4, 0.64).
narrative_ontology:measurement(grey_mkt_extract_2024, grey_market_evasion, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(grey_mkt_suppress_2018, grey_market_evasion, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(grey_mkt_suppress_2020, grey_market_evasion, suppression_requirement, 2, 0.35).
narrative_ontology:measurement(grey_mkt_suppress_2022, grey_market_evasion, suppression_requirement, 4, 0.39).
narrative_ontology:measurement(grey_mkt_suppress_2024, grey_market_evasion, suppression_requirement, 6, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(grey_market_evasion, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% Grey market evasion is downstream of export_control_reversibility. The upstream constraint establishes that export controls are technically reversible (can be lifted or tightened based on policy decisions), but the grey market evasion mechanism creates a persistent gap between formal control and actual technology flow. The two constraints have different extractiveness values: export_control_reversibility reflects the policy flexibility and compliance burden of the formal regime; grey_market_evasion reflects the monopoly rents and strategic risk externalization of the evasion mechanism. They are linked but structurally distinct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
