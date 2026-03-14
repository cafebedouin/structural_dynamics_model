% ============================================================================
% CONSTRAINT STORY: indo_pacific_security_architecture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indo_pacific_security_architecture, []).

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
 *   constraint_id: indo_pacific_security_architecture
 *   human_readable: Indo-Pacific Security Architecture: Coordination and Asymmetric Extraction
 *   domain: geopolitical/security
 *
 * SUMMARY:
 *   The Indo-Pacific security architecture represents a layered system of
 *   military alliances, basing agreements, technology-sharing protocols, and
 *   defense treaties centered on US power projection and regional deterrence.
 *   Over the interval 2015-2025, the architecture has intensified in response
 *   to Chinese military modernization and strategic assertiveness, while
 *   simultaneously losing some legitimacy as purely defensive coordination
 *   due to the explicit containment logic and asymmetric burden distribution.
 *   The constraint exhibits the full range of DR classifications: it
 *   functions as genuine coordination (rope) from the US perspective, mixed
 *   coordination-extraction (tangled rope) from allied regional powers, pure
 *   extraction (snare) from non-aligned states, alternative pathways
 *   (scaffold) from regional economic integration movements, institutional
 *   inertia (piton) from Cold War alliance structure perspectives, and
 *   possibly false naturalization (mountain) from realist IR analysts. The
 *   extractiveness has increased over the interval from 0.38 to 0.58, driven
 *   by escalating military expenditures, technology restrictions on allies,
 *   and expansion of basing rights. Theater ratio has increased from 0.35 to
 *   0.48, reflecting growth in performative alliance management (summit
 *   rhetoric, coordination theater, symbolic commitments) relative to
 *   substantive security coordination. Suppression remains high (0.65) due to
 *   structural barriers to exit: non-aligned states face extreme costs for
 *   realignment; allied powers are locked into defense treaties; and regional
 *   economic alternatives are constrained by security imperatives.
 *
 * KEY AGENTS:
 *   - United States Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — gains force projection, alliance centrality, technology export markets, and strategic positioning with high exit optionality
 *   - Allied Regional Powers (Japan, South Korea, Australia, India, Philippines): Secondary beneficiary and constraint target (organized/constrained) — gain security provision and market access but lose strategic autonomy; embedded in treaty structures with asymmetric burden-sharing
 *   - Non-Aligned Regional States (Indonesia, Thailand, Vietnam, Malaysia): Primary victims (powerless/trapped) — face structural dependence on great power patronage with limited exit options; compliance extracted through security dependence and economic coercion
 *   - China: Constrained external challenger (powerful/mobile) — excluded from alliance structures yet constrained by encirclement; developing parallel institutions with partial success but facing technology sanctions and supply chain restrictions
 *   - Regional Economic Integration Movements (ASEAN, RCEP): Organized alternative (organized/mobile) — represent genuine security architecture alternatives with sunset logic but suppressed by security constraints on trade and technology sharing
 *   - Cold War Alliance Institutions: Institutional actor (institutional/analytical) — NATO model structures persisting through inertia; primary function atrophied but maintained through institutional path-dependence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indo_pacific_security_architecture, 0.58).
domain_priors:suppression_score(indo_pacific_security_architecture, 0.65).
domain_priors:theater_ratio(indo_pacific_security_architecture, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indo_pacific_security_architecture, extractiveness, 0.58).
narrative_ontology:constraint_metric(indo_pacific_security_architecture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(indo_pacific_security_architecture, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indo_pacific_security_architecture, tangled_rope).
narrative_ontology:human_readable(indo_pacific_security_architecture, "Indo-Pacific Security Architecture: Coordination and Asymmetric Extraction").
narrative_ontology:topic_domain(indo_pacific_security_architecture, "geopolitical/security").

domain_priors:requires_active_enforcement(indo_pacific_security_architecture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indo_pacific_security_architecture, united_states_military_industrial).
narrative_ontology:constraint_beneficiary(indo_pacific_security_architecture, allied_regional_powers).
narrative_ontology:constraint_victim(indo_pacific_security_architecture, non_aligned_states).
narrative_ontology:constraint_victim(indo_pacific_security_architecture, regional_economic_integration).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ALIGNED STATES (SNARE) — Small and medium powers without alliance backing face structural dependence on great power patronage. Exit from the architecture requires geopolitical realignment with extreme costs: loss of security guarantees, economic sanctions, diplomatic isolation. The constraint extracts compliance through suppression of alternatives.
constraint_indexing:constraint_classification(indo_pacific_security_architecture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ALLIED REGIONAL POWERS (TANGLED ROPE) — Benefit from security provision and market access within the architecture but face embedded asymmetric extraction. Defense agreements constrain foreign policy autonomy; military basing rights subordinate regional strategies to US interests. Active enforcement: mutual defense treaties, command structures, technology-sharing restrictions. Genuine coordination function (deterrence of regional aggression) coupled with unequal burden-sharing and technology dependency.
constraint_indexing:constraint_classification(indo_pacific_security_architecture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US MILITARY-INDUSTRIAL (ROPE) — Primary beneficiary experiencing the constraint as pure coordination. Forward-deployed forces, basing rights, technology exports, and strategic positioning all solve genuine coordination problems (maintaining regional power balance, deterring peer competition). US experiences immediate gains from force projection and alliance centrality. Exit options abundant — US can shift military posture globally. Net flow of extraction runs toward this agent.
constraint_indexing:constraint_classification(indo_pacific_security_architecture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CHINA (TANGLED ROPE) — Faces constraints from the architecture as both victim and external challenger. Excluded from formal alliance structures but constrained by encirclement dynamics; contained supply chains and technology restrictions. Asymmetric extraction through sanctions regime and military containment. Yet China also extracts from the system through Belt and Road alternatives and regional economic integration. Mobile exit options exist (building parallel institutions) but costly. Active enforcement: US-led technology sanctions, military exclusion from peacekeeping, supply chain diversification constraints.
constraint_indexing:constraint_classification(indo_pacific_security_architecture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: REGIONAL ECONOMIC INTEGRATION (SCAFFOLD) — ASEAN, RCEP, and broader regional cooperation mechanisms represent alternative coordination pathways with sunset logic. Low effective extraction because organizers have agency and see exit through deepened economic interdependence. Theater low (genuine economic coordination, not performative security theater). However, suppressed by security architecture constraints on trade diversification and technology sharing. Sunset clause implicit: as economic integration matures, security architecture's monopoly on regional coordination weakens.
constraint_indexing:constraint_classification(indo_pacific_security_architecture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: COLD WAR ALLIANCE STRUCTURES (PITON) — The Indo-Pacific architecture mirrors Cold War NATO institutions now applied to the Asia-Pacific, despite radically different geopolitical conditions. Theater ratio reflects performative alliance management (rhetorical commitment, symbolic summits, coordination theater) that persists through institutional inertia rather than functional necessity. Primary function (containment of Soviet threat) has atrophied; residual structures maintained through institutional path-dependence. Low extractiveness here reflects that the performance is losing grip on structural reality.
constraint_indexing:constraint_classification(indo_pacific_security_architecture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a pure international relations realist position, the security architecture reflects immutable structural facts: the distribution of power, the anarchic nature of the international system, and the security dilemma create inexorable incentives for alliance formation. The constraints appear as natural consequences of state behavior under anarchy. However, the structural data reveals false summitry: the specific institutional forms (bilateral defense treaties, military basing, technology restrictions) are contingent, not natural. Alternative security architectures (collective security, economic interdependence, neutrality frameworks) are historically possible.
constraint_indexing:constraint_classification(indo_pacific_security_architecture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indo_pacific_security_architecture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indo_pacific_security_architecture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indo_pacific_security_architecture, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indo_pacific_security_architecture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indo_pacific_security_architecture, TR),
    TR >= 0.70.

:- end_tests(indo_pacific_security_architecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The architecture extracts strategic compliance through security dependence, military burden-sharing requirements, and technology restrictions. However, it's not as extreme as pure extraction (snare level ≥0.66) because genuine coordination functions exist — deterrence of regional aggression, protection of sea lanes, prevention of hegemonic domination — that most participating actors acknowledge. The increase from 0.38 to 0.58 reflects escalating containment logic against China and intensifying military expenditures. Suppression (0.65): High. Exit barriers are severe: non-aligned states face diplomatic isolation and economic sanctions for realignment; allied powers are locked into defense treaties with high defection costs; regional economic alternatives are constrained by security imperatives that prioritize military alignment over economic integration. Suppression is enforced through active mechanisms (treaty enforcement, sanctions threats, security dependence maintenance). Theater ratio (0.48): Moderate and rising. Traditional alliance coordination (genuine deterrence, mutual defense, intelligence sharing) persists alongside increasing performative alliance management (summit theater, symbolic commitments, rhetorical escalation). The rise from 0.35 to 0.48 reflects growth in coordination theater relative to substantive security provision, consistent with Piton degradation patterns.
 *
 * PERSPECTIVAL GAP:
 *   Maximal perspectival gap across perspectives. US sees coordination (Rope), allies see mixed coordination-extraction (Tangled Rope), non-aligned see pure extraction (Snare), China sees constrained tangled extraction (Tangled Rope), regional integration sees alternative pathways (Scaffold), Cold War institutions see degraded ritual (Piton), analytical observer sees natural law (Mountain, false). No two perspectives produce identical classifications. The gap reveals structural complexity: the same institutional apparatus functions fundamentally differently depending on agent position within it.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to extraction flows. US military-industrial (beneficiary + arbitrage exit) derives low d → negative χ → pure coordination experience. Allied powers (partial beneficiary + constrained exit) derive moderate d → moderate χ → mixed experience. Non-aligned states (victims + trapped exit) derive high d → high χ → pure extraction experience. China (victim + mobile exit) derives high d but with partial arbitrage capacity → moderate-high χ → constrained tangled rope. Regional economic movements (alternative pathway + mobile exit) derive low effective d because they represent outside options → low χ despite suppression → scaffold with agency. Cold War institutions (degraded function + analytical exit) derive neutral d because they're observing, not participating → piton classification from theater gate. The analytical observer (no structural position, analytical exit) would derive neutral d except the false mountain classification reveals naturalizing tendency.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that all six types are legitimate perspectival readings. The US military-industrial complex genuinely experiences coordination (Rope) — alliance structures do solve their strategic problems with minimal extraction overhead. Allied powers genuinely experience tangled extraction (Tangled Rope) — they coordinate deterrence but lose autonomy and bear unequal burdens. Non-aligned states genuinely experience pure extraction (Snare) — they are coerced into alignment with suppression of alternatives. No single type is 'the answer' — the presheaf over observer positions IS the answer. The mandatrophy is resolved by recognizing that the constraint simultaneously performs its stated function (regional security coordination) and performs extraction asymmetrically (concentrating benefits among allies, imposing costs on non-aligned states, constraining regional alternatives). Both are true. The false mountain perspective reveals the risk of realist naturalization: the constraint appears immutable only from the standpoint of an analyst who has internalized the anarchic international system assumption. From economic integration perspectives, alternatives exist. From inside allied states, treaty escape is possible. From non-aligned perspectives, realignment carries high but surmountable costs. Only from the globally-analytical position divorced from structural interest does the architecture appear unchangeable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_coordination_vs_containment,
    'What proportion of the Indo-Pacific architecture functions as genuine security coordination versus unilateral containment of China?',
    'Structural analysis of military deployments, treaty language, and technological integration; comparison with Cold War NATO alliance structures; assessment of defensive vs offensive capability emphasis',
    'If predominantly coordination: classification shifts toward Rope from regional powers'' perspectives. If predominantly containment: classification shifts toward Snare for non-aligned states and Tangled Rope for allies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_coordination_vs_containment, empirical, 'Whether architecture is security coordination or containment mechanism').

omega_variable(
    economic_integration_exit_credibility,
    'Are regional economic integration pathways (RCEP, ASEAN++) genuinely viable exits from security architecture dependence, or are they constrained by security imperatives?',
    'Analysis of technology transfer restrictions, sanctions on ASEAN members engaging with China, capital flow restrictions, and supply chain reorganization costs; comparison of economic gains from security alignment vs regional integration',
    'If viable: Scaffold sunset logic confirmed and extractiveness of security architecture decreases. If constrained: economic integration suppressed and security architecture''s effective extraction increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_integration_exit_credibility, empirical, 'Credibility of regional economic integration as alternative pathway').

omega_variable(
    alliance_burden_sharing_asymmetry,
    'Is the unequal burden distribution (US force projection benefits vs allied defense spending requirements) a feature of genuine coordination or a mechanism for extracting strategic compliance?',
    'Longitudinal analysis of defense spending burden, technology transfer terms, command structure asymmetries, and allied states'' strategic autonomy over time; counterfactual: what would regional security arrangements look like without US backing?',
    'If feature of coordination: Rope/Tangled Rope split is justified. If extraction mechanism: all allied perspectives shift toward Snare or high-χ Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alliance_burden_sharing_asymmetry, empirical, 'Whether burden-sharing asymmetry is coordination feature or extraction mechanism').

omega_variable(
    institutional_persistence_vs_adaptation,
    'Do Cold War-era alliance structures persist due to genuine security requirements or institutional inertia and sunk costs?',
    'Analysis of why bilateral defense treaties remain preferred over collective security mechanisms; assessment of institutional reform attempts and their failures; comparison with alternative security architectures proposed but not adopted',
    'If institutional inertia dominant: Piton classification confirmed and theater_ratio should increase. If security requirements drive structure: classification shifts toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_persistence_vs_adaptation, conceptual, 'Whether alliance structures persist from necessity or inertia').

omega_variable(
    china_exit_strategy_viability,
    'Is China''s development of parallel institutional alternatives (BRI, RCEP leadership, military modernization) a genuine exit from the security architecture or a reconfiguration within constrained parameters?',
    'Assessment of whether China''s alternatives reduce dependence on or vulnerability to the security architecture; analysis of supply chain independence, alliance formation capacity, and regional strategic autonomy over 10-20 year horizon',
    'If genuine exit: China classification becomes more mobile and extractiveness decreases. If constrained: China remains tangled in the architecture and extractiveness persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(china_exit_strategy_viability, empirical, 'Viability of China''s alternative institutional pathways as security architecture exit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indo_pacific_security_architecture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indo_tr_t0, indo_pacific_security_architecture, theater_ratio, 0, 0.35).
narrative_ontology:measurement(indo_tr_t5, indo_pacific_security_architecture, theater_ratio, 5, 0.42).
narrative_ontology:measurement(indo_tr_t10, indo_pacific_security_architecture, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(indo_be_t0, indo_pacific_security_architecture, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(indo_be_t5, indo_pacific_security_architecture, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(indo_be_t10, indo_pacific_security_architecture, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indo_pacific_security_architecture, enforcement_mechanism).
narrative_ontology:affects_constraint(indo_pacific_security_architecture, south_china_sea_territorial_disputes).
narrative_ontology:affects_constraint(indo_pacific_security_architecture, taiwan_strait_military_balance).
narrative_ontology:affects_constraint(indo_pacific_security_architecture, asean_strategic_autonomy).
narrative_ontology:affects_constraint(indo_pacific_security_architecture, technology_supply_chain_decoupling).

% DUAL FORMULATION NOTE:
% The Indo-Pacific security architecture is upstream of specific territorial and military constraints (South China Sea disputes, Taiwan strait balance) and causal to technology decoupling and ASEAN autonomy constraints. Each downstream constraint has its own extractiveness reflecting the specific domain; the architecture story represents the meta-institutional constraint that structures those domain-specific problems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indo_pacific_security_architecture, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
