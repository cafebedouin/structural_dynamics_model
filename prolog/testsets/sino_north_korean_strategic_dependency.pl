% ============================================================================
% CONSTRAINT STORY: sino_north_korean_strategic_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sino_north_korean_strategic_dependency, []).

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
 *   constraint_id: sino_north_korean_strategic_dependency
 *   human_readable: Sino-North Korean Strategic Dependency
 *   domain: geopolitical/strategic/economic
 *
 * SUMMARY:
 *   China's strategic relationship with North Korea creates a constraint that
 *   simultaneously functions as pure coordination (preventing humanitarian
 *   collapse and nuclear instability), pure extraction (leveraging North
 *   Korean dependency for behavioral compliance and regional influence),
 *   temporary accommodation (awaiting systemic change), degraded
 *   institutional ritual (Cold War alliance theater), and natural
 *   geopolitical law (depending on the observer's position). The constraint
 *   manifests extractiveness of 0.58 — moderate-high — with increasing trend
 *   over the measurement interval as Chinese leverage has grown while North
 *   Korea's alternative sources have remained limited. Suppression is high
 *   (0.72) because North Korea faces lethal exit barriers: severing Chinese
 *   support would trigger energy shortage, food insecurity, and regime
 *   collapse. Theater ratio (0.55) reflects mixed strategic substance and
 *   diplomatic ritual — genuine coordination need exists, but significant
 *   performative activity (state visits, alliance rhetoric, symbolic trade)
 *   maintains the appearance of relationship depth beyond its functional
 *   core.
 *
 * KEY AGENTS:
 *   - North Korean Leadership (DPRK): Primary victim (powerless/trapped) — structurally dependent on Chinese energy and food; faces regime collapse if support withdrawn; trapped with no viable exit
 *   - North Korean Population: Secondary victim (moderate/constrained) — receives survival coordination (food/energy) but also experiences regime suppression enabled by Chinese support; faces high barriers to exit (regime control, border closure, international asylum barriers)
 *   - China's Strategic Position: Primary beneficiary (institutional/arbitrage) — gains regional buffer against US influence, nuclear proliferation control leverage, security coordination benefits, ability to extract behavioral compliance
 *   - China's Economic Interests: Secondary beneficiary (institutional/arbitrage) — gains mineral resource access, trade monopoly leverage, sanctions-compliant profit capture; experiences pure coordination in trade relationship
 *   - International Sanctions Coalition (US/Japan/ROK/UN): Organized victim (organized/constrained) — sees constraint as undermining sanctions effectiveness, constrained by China's veto power, organized but unable to enforce coordination
 *   - Global Non-Proliferation Regime (IAEA/NPT): Systemic observer (organized/constrained) — sees constraint as temporary accommodation with potential sunset logic, constrained by geopolitical limits but holding long-term transformation pathway
 *   - Cold War Alliance Framework: Institutional artifact (institutional/arbitrage) — maintains theatrical bilateral relationship increasingly divorced from strategic function; persists through inertia rather than substance
 *   - Analytical Observer: Universal/civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable geopolitical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sino_north_korean_strategic_dependency, 0.58).
domain_priors:suppression_score(sino_north_korean_strategic_dependency, 0.72).
domain_priors:theater_ratio(sino_north_korean_strategic_dependency, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sino_north_korean_strategic_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(sino_north_korean_strategic_dependency, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sino_north_korean_strategic_dependency, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sino_north_korean_strategic_dependency, tangled_rope).
narrative_ontology:human_readable(sino_north_korean_strategic_dependency, "Sino-North Korean Strategic Dependency").
narrative_ontology:topic_domain(sino_north_korean_strategic_dependency, "geopolitical/strategic/economic").

domain_priors:requires_active_enforcement(sino_north_korean_strategic_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sino_north_korean_strategic_dependency, china_regional_influence).
narrative_ontology:constraint_beneficiary(sino_north_korean_strategic_dependency, china_strategic_buffer).
narrative_ontology:constraint_victim(sino_north_korean_strategic_dependency, north_korea_economic_autonomy).
narrative_ontology:constraint_victim(sino_north_korean_strategic_dependency, north_korea_political_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NORTH KOREAN LEADERSHIP (SNARE) — China provides 90%+ of energy imports, ~40% of food imports, and critical financial lifeline. Exit from dependency would trigger regime collapse via energy shortage, famine, and economic implosion. Leadership is trapped with no alternative patron available; all exit routes are lethal. Maximum experienced extraction as China leverages this dependency for behavioral control, policy concessions, and strategic positioning. Pure extraction with minimal coordination benefit — the constraint exists to extract compliance.
constraint_indexing:constraint_classification(sino_north_korean_strategic_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NORTH KOREAN POPULATION (TANGLED ROPE) — Structurally constrained by regime control and economic dependency, but also derives genuine coordination benefits from Chinese food aid and energy transfers that prevent mass starvation. The relationship contains both extraction (regime leverages dependency to suppress dissent) and coordination (China's provision prevents catastrophic collapse). Population experiences the constraint as constrained exit — could theoretically flee but faces regime punishment, border barriers, and lack of receiving countries. Moderate extraction with mixed coordination and coercion.
constraint_indexing:constraint_classification(sino_north_korean_strategic_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CHINA'S REGIONAL STRATEGIC POSITION (ROPE) — Experiences the constraint primarily as coordination of regional buffer against US influence, coordination of nuclear proliferation control, and security coordination against destabilizing regime collapse. China has exit options (could abandon support, but faces costs) and derives clear benefits. The constraint solves a genuine coordination problem: preventing regime collapse that would create humanitarian crisis, refugee flows, and potential US military presence on Chinese border. Net beneficiary with positive extraction — this actor sees pure coordination.
constraint_indexing:constraint_classification(sino_north_korean_strategic_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: CHINA'S ECONOMIC INTERESTS (ROPE) — Pure economic coordination. China gains from: access to North Korean mineral resources (rare earths, tungsten, magnesite), trade route positioning through DPRK territory, leverage over sanctions enforcement, and ability to profit from North Korean economic dependency through monopoly pricing. Economic extraction is substantial but embedded in coordination framework — the constraint solves the mutual trade and resource access problem. Beneficiary with arbitrage options; experiences as coordination mechanism.
constraint_indexing:constraint_classification(sino_north_korean_strategic_dependency, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL SANCTIONS COALITION (TANGLED ROPE) — Organized states coordinating nuclear proliferation control through sanctions, but constrained by China's strategic veto and willingness to breach sanctions coordination through continued support. The constraint undermines sanctions coordination (China's support enables North Korea to evade pressure) while simultaneously serving as the only mechanism preventing regime collapse and catastrophic humanitarian crisis. Organized agents see both coordination failure (sanctions ineffective) and coordination necessity (preventing instability). Constrained exit because full sanctions enforcement requires China's participation, which it refuses. Mixed extraction and coordination.
constraint_indexing:constraint_classification(sino_north_korean_strategic_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: GLOBAL NON-PROLIFERATION REGIME (SCAFFOLD) — The constraint represents a temporary accommodation with sunset logic. As alternative power sources (renewables), supply chain diversification, and sanctions pressure increase, China's ability to extract behavioral compliance through energy dependency faces natural erosion. The non-proliferation framework sees the Sino-DPRK relationship as a degrading but still-functional temporary measure. Reform pathways (renewable energy transfer, sanctions-compliant trade, multilateral security guarantees) could progressively reduce North Korea's extraction sensitivity to Chinese pressure. Theater_ratio lower here because the regime's nuclear program represents genuine military coordination even if energy supply is leverage point.
constraint_indexing:constraint_classification(sino_north_korean_strategic_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: COLD WAR BILATERAL FRAMEWORK (PITON) — From the civilizational perspective, the strategic relationship is substantially degraded from its original function. The 1961 mutual defense treaty is largely theatrical — China would not honor it in case of regime collapse, and both parties know this. The relationship persists through institutional inertia: continued economic support maintains the appearance of alliance solidarity and prevents the diplomatic humiliation of abandoning the last communist state. Theater_ratio reflects that much diplomatic activity (state visits, symbolic trade, joint statements) maintains the theater of alliance rather than serving current strategic coordination. The underlying strategic function (bipolar balance) has degraded; institutional forms persist.
constraint_indexing:constraint_classification(sino_north_korean_strategic_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY VIEW (MOUNTAIN) — From the universal/civilizational analytical perspective, the constraint appears as a natural law of geopolitics: any major power must manage a weak, nuclear-armed buffer state on its border; dependency relationships are inevitable consequences of power asymmetry; the structural position of North Korea makes Chinese support a necessary fact. This perspective risks naturalizing what is actually a contingent institutional arrangement shaped by 1960s alliance politics, Cold War rhetoric, and elite identity commitments. The engine's false summit detector will flag this as naturalization — the empirical data reveals significant contingency and plasticity in the relationship rather than immutable structural necessity.
constraint_indexing:constraint_classification(sino_north_korean_strategic_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sino_north_korean_strategic_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sino_north_korean_strategic_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sino_north_korean_strategic_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sino_north_korean_strategic_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sino_north_korean_strategic_dependency, TR),
    TR >= 0.70.

:- end_tests(sino_north_korean_strategic_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. China extracts behavioral compliance, policy concessions (restraint on nuclear testing, ballistic missile launches), and acceptance of Chinese economic monopoly through leveraging energy/food dependency. The extraction is substantial but not total (maximum would be 0.70+) because genuine mutual survival coordination exists — China does not want regime collapse and DPRK does provide some strategic value. Measurement trend (0.42→0.52→0.58) reflects increasing Chinese leverage as international isolation strengthens dependency. Suppression (0.72): High. North Korea faces catastrophic exit barriers: energy shortage leading to famine, regime collapse, military force, international isolation. Suppression is structural (material barriers) not merely internalized — the regime would gladly exit if alternatives existed. Theater ratio (0.55): Moderate. Strategic substance exists (buffer coordination is real), but significant diplomatic theater masks the asymmetry (state visits, mutual defense treaty rhetoric, alliance solidarity narratives). Theater has increased over interval as Cold War legitimation framework has worn thin while relationship persists through institutional momentum.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. North Korean leadership sees pure snare — lethal dependency, no viable exit, maximum extraction. North Korean population sees tangled rope — receives survival benefits but also experiences regime suppression. China's strategic position sees pure rope — genuine coordination problem solved, net beneficiary. Sanctions coalition sees failed coordination (rope attempted, extraction interfering). Non-proliferation regime sees temporary scaffold with sunset potential. Cold War framework sees degraded piton. Analytical observer risks false summit (naturalizing as inevitable geopolitical law). The perspectival gap reveals that 'the constraint' is actually multiple structurally distinct phenomena: (1) strategic buffer coordination, (2) economic monopoly extraction, (3) behavioral leverage mechanism, (4) humanitarian survival transfer, (5) Cold War institutional artifact. These are bound together empirically but analytically distinct.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: China's institutional power + arbitrage exit options → low d (0.10-0.25) → negative f(d) → chi benefits China. North Korea's powerless position + trapped exit → high d (0.90-0.98) → f(d) ≈ 1.42 → maximum chi directed at DPRK. North Korean population's moderate power + constrained exit → medium-high d (0.65-0.75) → f(d) ≈ 0.95-1.10 → moderate extracted chi. Sanctions coalition's organized power + constrained exit (constrained by China's veto) → medium-high d (0.55-0.65) → f(d) ≈ 0.75-0.95 → moderate chi reflecting failed coordination role. The constraint's direction is unambiguous: extraction flows from China toward North Korea; coordination flows from both toward security stability.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION CANDIDATE: The Sino-North Korean relationship should potentially be decomposed into multiple constraint stories with different ε values: (A) Strategic_Buffer_Coordination (ε ≈ 0.15, Rope) — genuine security coordination around regime stability and US containment; (B) Economic_Monopoly_Extraction (ε ≈ 0.72, Snare) — pure extraction through control of rare earth minerals, trade monopoly, and pricing leverage; (C) Behavioral_Leverage_Mechanism (ε ≈ 0.65, Snare) — extraction of nuclear policy concessions through energy/food dependency; (D) Humanitarian_Survival_Transfer (ε ≈ 0.08, Rope) — pure coordination of food/energy to prevent famine and humanitarian collapse. The unified story (sino_north_korean_strategic_dependency at ε=0.58) represents the aggregate of these overlapping mechanisms. Mandatrophy resolution requires recognizing that calling this 'coordination' (rope) misses the pure extraction mechanisms, while calling it 'snare' misses the genuine survival coordination. The tangled_rope classification at 0.58 is accurate at aggregate level but obscures the structural composition. Future work should create the constraint family with separate stories for each mechanism, linked through network.affects_constraints to show how they bundle together in the real relationship.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    china_exit_calculus_uncertainty,
    'Would China''s costs of full strategic withdrawal from North Korea exceed its benefits from sanctions cooperation and improved US relations?',
    'Cost-benefit modeling including: humanitarian crisis costs (refugee flows, famine death toll), geopolitical costs (loss of buffer, US military presence), domestic costs (party legitimacy as communist ally sponsor), economic costs (mining contracts, trade leverage loss) vs benefits (sanctions cooperation, US relationship improvement, reduced proliferation risk)',
    'If withdrawal costs >> benefits: China has no credible exit option, constraint is structurally locked, classification remains snare for DPRK. If withdrawal costs ≈ benefits: China has hidden arbitrage option, constraint becomes more symmetric, DPRK classification could shift toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(china_exit_calculus_uncertainty, empirical, 'Uncertainty in China''s exit cost calculation from strategic support').

omega_variable(
    extraction_mechanism_intentionality,
    'Is China actively leveraging dependency for behavioral extraction, or is extraction a byproduct of mutual survival equilibrium?',
    'Historical analysis of explicit quid pro quo demands (e.g., nuclear policy concessions tied to aid levels); correlation between Chinese aid suspension and North Korean policy compliance; interviews with Chinese policymakers on intentionality; analysis of alternative aid delivery mechanisms that could exist if coordination rather than leverage was primary goal',
    'If intentional extraction: classification as snare from DPRK perspective is solidly justified, theater_ratio lower. If byproduct of equilibrium: constraint becomes purer tangled_rope, theater_ratio higher (relationship appears as unavoidable survival coordination rather than deliberate leverage).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_intentionality, empirical, 'Whether extraction is deliberate leverage or byproduct of survival equilibrium').

omega_variable(
    alternative_patron_availability,
    'Could North Korea credibly substitute Chinese energy/food supplies with alternative sources (Russia, Middle East, sanctions-compliant sources) if current Chinese support were withdrawn?',
    'Detailed supply-chain mapping; assessment of Russian capacity to replace Chinese energy deliveries; analysis of sanctions enforcement on alternative patrons; evaluation of renewable energy technology transfer feasibility from third parties',
    'If alternatives exist: North Korea has exit options beyond regime collapse, classification shifts from snare toward tangled_rope or constrained mobile. If no credible alternatives: trap is genuine structural fact, snare classification confirmed from DPRK perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_patron_availability, empirical, 'Availability of alternative patrons and supply sources').

omega_variable(
    sanctions_regime_effectiveness_counterfactual,
    'Would international sanctions produce regime collapse or forced policy change if China fully cooperated, or is the regime''s behavior independent of sanctions pressure?',
    'Counterfactual modeling of North Korean economy under full sanctions with Chinese cooperation vs current trajectory; analysis of regime response to previous sanctions escalations; assessment of whether nuclear program is driven by security doctrine (immutable) vs sanctions compensation (plastic)',
    'If sanctions would be effective with Chinese cooperation: constraint enables continued nuclear development, extraction is strategically valuable to China, snare classification justified. If regime would develop program regardless: China''s support is primarily humanitarian coordination, constraint becomes less extractive, classifications shift toward rope/scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctions_regime_effectiveness_counterfactual, conceptual, 'Counterfactual effectiveness of sanctions under full international cooperation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sino_north_korean_strategic_dependency, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sinokn_tr_t0, sino_north_korean_strategic_dependency, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sinokn_tr_t10, sino_north_korean_strategic_dependency, theater_ratio, 10, 0.48).
narrative_ontology:measurement(sinokn_tr_t20, sino_north_korean_strategic_dependency, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(sinokn_be_t0, sino_north_korean_strategic_dependency, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sinokn_be_t10, sino_north_korean_strategic_dependency, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(sinokn_be_t20, sino_north_korean_strategic_dependency, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sino_north_korean_strategic_dependency, enforcement_mechanism).
narrative_ontology:affects_constraint(sino_north_korean_strategic_dependency, north_korean_nuclear_weapons_program).
narrative_ontology:affects_constraint(sino_north_korean_strategic_dependency, china_us_strategic_competition).
narrative_ontology:affects_constraint(sino_north_korean_strategic_dependency, korean_peninsula_military_balance).

% DUAL FORMULATION NOTE:
% This constraint is upstream to the nuclear proliferation dynamics and US-China strategic competition. Its extractiveness and suppression metrics directly affect how coercive these downstream constraints must become. The relationship could be decomposed into multiple stories (strategic buffer, economic extraction, behavioral leverage, humanitarian coordination) if empirical analysis reveals the mechanisms have distinct ε-values and require separate lifecycle tracking.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sino_north_korean_strategic_dependency, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
