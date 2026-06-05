% ============================================================================
% CONSTRAINT STORY: us_sanctions_belarus_2022
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_sanctions_belarus_2022, []).

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
 *   constraint_id: us_sanctions_belarus_2022
 *   human_readable: U.S. Sanctions on Belarus (2022)
 *   domain: geopolitical/sanctions/economic_coercion
 *
 * SUMMARY:
 *   U.S. sanctions on Belarus beginning in 2022 create a multi-perspective
 *   constraint where the same policy tool operates simultaneously as (1) a
 *   coordination mechanism between allied states, (2) an extraction mechanism
 *   imposing costs on civilians without political leverage, and (3) a
 *   performative policy theater. The constraint exemplifies how geopolitical
 *   coercion distributes harms asymmetrically: allied governments and
 *   opposition groups experience coordination benefits and rhetorical
 *   leverage; the Belarusian civilian economy and working population
 *   experience trapped extraction with no exit options. The rising theater
 *   ratio (0.35 → 0.58 over 12 months) reflects that public justification for
 *   sanctions increasingly invokes abstract principles ('isolating
 *   authoritarianism,' 'international rules-based order') while tangible
 *   outcomes (regime behavior change, opposition strengthening) remain
 *   unclear or negative. Theater rise correlates with extraction growth: as
 *   sanctions bite harder, the gap between stated objectives and observable
 *   impact widens, requiring more performative messaging to sustain public
 *   support.
 *
 * KEY AGENTS:
 *   - Belarusian Civilian Economy: Primary victim (powerless/trapped) — exposed to supply chain collapse, currency devaluation, inflation, capital flight with zero negotiating power
 *   - Belarusian Working Population: Primary victim (powerless/trapped) — bears direct costs of job losses, wage suppression, reduced purchasing power, reduced access to goods
 *   - Belarusian Opposition Movement: Secondary beneficiary + victim (organized/mobile) — receives international messaging support and potential resource flows, but loses civilian support base as economy collapses
 *   - U.S. Policy Apparatus: Primary beneficiary (institutional/arbitrage) — experiences sanctions as coordination tool with unilateral control over escalation/de-escalation
 *   - Allied Nations (EU, Canada, UK): Secondary beneficiary (institutional/arbitrage) — gain diplomatic coordination credit and unified messaging capability
 *   - EU Border States: Secondary victim (moderate/constrained) — bear regional spillover costs (refugee pressure, trade disruption) without ability to negotiate independently
 *   - Lukashenko Regime: Notional target, ambiguous outcome — sanctions may strengthen grip through rally-around-flag effect; regime accesses contraband supply routes and Russian support
 *   - Analytical Observer: Observes full tangled structure — sanctions deliver coordination and extraction simultaneously; the mandatrophy question is whether civilian extraction is justified by coordination benefits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_sanctions_belarus_2022, 0.68).
domain_priors:suppression_score(us_sanctions_belarus_2022, 0.72).
domain_priors:theater_ratio(us_sanctions_belarus_2022, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_sanctions_belarus_2022, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_sanctions_belarus_2022, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_sanctions_belarus_2022, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_sanctions_belarus_2022, snare).
narrative_ontology:human_readable(us_sanctions_belarus_2022, "U.S. Sanctions on Belarus (2022)").
narrative_ontology:topic_domain(us_sanctions_belarus_2022, "geopolitical/sanctions/economic_coercion").

domain_priors:requires_active_enforcement(us_sanctions_belarus_2022).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_sanctions_belarus_2022, us_strategic_interests).
narrative_ontology:constraint_beneficiary(us_sanctions_belarus_2022, opposition_to_lukashenko).
narrative_ontology:constraint_victim(us_sanctions_belarus_2022, belarus_civilian_economy).
narrative_ontology:constraint_victim(us_sanctions_belarus_2022, belarus_working_population).
narrative_ontology:constraint_victim(us_sanctions_belarus_2022, belarus_export_dependent_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BELARUSIAN CIVILIAN ECONOMY (SNARE) — Cannot exit sanctions regime without political concessions. Faces extraction through currency collapse, supply chain disruption, inflation, and capital flight. No exit options available. d≈0.93, f(d)≈1.39, σ=1.0 → χ≈0.63.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BELARUSIAN WORKING POPULATION (SNARE) — Bears direct costs of job losses, wage suppression, reduced access to goods. Trapped by geography and lack of exit options. No ability to negotiate terms. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.65.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: BELARUSIAN OPPOSITION MOVEMENT (TANGLED ROPE) — Benefits from sanctions rhetoric (coordination messaging against Lukashenko) but bears costs when sanctions damage the economy (victims' support base). Has some organizational exit options through diaspora networks and international support. Mixed position. d≈0.58, f(d)≈0.73, σ=1.0 → χ≈0.50.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: U.S. POLICY APPARATUS (ROPE) — Experiences sanctions as a coordination mechanism: coordinating with allies on enforcement, signaling policy commitment, organizing collective pressure. High exit optionality (can adjust, remove, or escalate sanctions unilaterally). d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.06. Net beneficiary through coordination framing.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALLIED NATIONS (ROPE) — Coordination with U.S. on sanctions enforcement. Extract diplomatic credit for 'principled stance.' Have exit options (can unilaterally lift, modify, or selectively enforce). d≈0.12, f(d)≈-0.05, σ=1.1 → χ≈-0.06. Net beneficiary through unified messaging.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EU BORDER STATES (SNARE) — Bear secondary costs: refugee pressure, trade disruption, border management costs, currency pressure from regional economic collapse. Constrained by EU coordination requirements; cannot unilaterally lift sanctions or negotiate independently. d≈0.78, f(d)≈1.12, σ=0.9 → χ≈0.62.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: INTERNATIONAL SANCTIONS ARCHITECTURE (PITON) — Sanctions persist as a tool through institutional inertia despite mixed evidence on effectiveness in achieving political change. Theater ratio high: public messaging about 'isolating authoritarian regimes' persists even as evidence accumulates that sanctions entrench target regimes and harm civilian populations. d≈0.05, f(d)≈-0.11, σ=1.0 → χ≈-0.03. Classified as piton because theater_ratio=0.58 suggests significant performative component in sanctions justification.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — Sanctions function simultaneously as (1) coordination mechanism between allied states (rope function), (2) extraction from civilian population (snare function), and (3) performative policy theater (piton function). The structure contains genuine coordination benefits (allied solidarity, clear signaling) AND asymmetric extraction (harm to non-combatants with no political leverage). Beneficiaries: allied coordination, opposition rhetoric. Victims: civilians. Enforcer (U.S.) maintains control over escalation. ε=0.68, suppression=0.72 confirms tangled hybrid structure. d≈0.50, f(d)≈0.65, σ=1.1 → χ≈0.49.
constraint_indexing:constraint_classification(us_sanctions_belarus_2022, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_sanctions_belarus_2022_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_sanctions_belarus_2022, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_sanctions_belarus_2022, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_sanctions_belarus_2022, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_sanctions_belarus_2022, TR),
    TR >= 0.70.

:- end_tests(us_sanctions_belarus_2022_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Sanctions impose significant costs on the Belarusian economy through asset freezes, trade restrictions, financial system isolation, and import/export controls. These costs fall disproportionately on civilians and workers rather than regime leadership. The extraction is not minimal (thus not rope), but the presence of stated coordination benefits (allied unity, opposition support) prevents classification as pure snare at the analytical level. Suppression (0.72): High. Victims have extremely limited exit options: cannot escape geography, cannot negotiate sanctions terms, cannot influence U.S. policy decisions. The suppression operates through both institutional mechanisms (legal trade restrictions) and market mechanisms (currency collapse, capital flight). Theater ratio (0.58): Moderate-high. Sanctions are justified through abstract principles ('isolating authoritarianism,' 'defending democracy,' 'rules-based order') while evidence of effectiveness is mixed or negative. The gap between public messaging and observable outcomes has widened over the 12-month interval, requiring increasingly theatrical justification. Theater rise from 0.35 to 0.58 reflects accumulating pressure to sustain support for a policy with unclear success metrics.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence across the observation site. The U.S. policy apparatus sees coordination (Rope) — allied states unified, clear signal sent, U.S. maintains control. Allied nations see modest coordination (Rope) — diplomatic credit, unified messaging, low cost to themselves. The Belarusian opposition sees mixed benefit and harm (Tangled Rope) — international support for messaging but damaged domestic support base. EU border states see extraction (Snare) — regional spillover costs with constrained exit options. The Belarusian civilian economy and workers see pure extraction (Snare) — all costs, no control, no exit. The international sanctions architecture appears as degraded ritual (Piton) — persists through inertia despite mixed evidence on effectiveness. The analytical observer sees the full tangled structure: genuine coordination benefits for some actors (allied states, opposition rhetoric) combined with asymmetric extraction from those with no political leverage (civilians). This is a diagnostic case for how indexical classification exposes distributional asymmetry: the same policy is experienced as coordination by powerful actors and extraction by powerless ones.
 *
 * DIRECTIONALITY LOGIC:
 *   Belarusian civilians/workers: Victims + trapped → d≈0.94, f(d)≈1.40. Maximum extraction experienced. Belarusian opposition: Mixed beneficiary + victim + mobile → d≈0.60, f(d)≈0.80. Receives coordination messaging benefits but loses civilian support base. U.S. policy apparatus: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary through coordination control. Allied nations: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.05. Modest beneficiary through diplomatic coordination. EU border states: Victim + constrained → d≈0.78, f(d)≈1.12. Significant extraction without unilateral exit options. International sanctions architecture: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.11. Piton classification comes from theater gate (0.58 > 0.50 threshold) and accumulated evidence of performance gap.
 *
 * MANDATROPHY ANALYSIS:
 *   CORE MANDATROPHY: The constraint classifies as Snare (ε=0.68, suppression=0.72, χ≥0.66 across victim perspectives) but includes genuine coordination benefits for allied states and opposition rhetoric. This appears to violate the snare definition ('pure extraction, minimal coordination'). Resolution: This is not a classification error but a structural insight. The snare classification is CORRECT FROM THE VICTIM'S PERSPECTIVE (powerless/trapped agent), where χ≈0.63-0.65. From the beneficiary's perspective (U.S. policy apparatus), it classifies as Rope (χ≈-0.06). The mandatrophy is resolved by recognizing that 'snare' and 'rope' are not absolute types — they are perspectival observations indexed to (P,T,E,S). The constraint IS a snare for the civilians experiencing it AND a rope for the allied states coordinating it. Both classifications are correct. The policy-level mandatrophy question is whether the coordination benefits to allied states and opposition rhetoric justify the extraction imposed on civilians. That is a VALUE QUESTION, not a classification question. The framework's role is to make the asymmetry VISIBLE: extraction is asymmetric, experienced as snare by those with no leverage and rope by those with control. The theatrical justification ('isolating authoritarianism') obscures this asymmetry. The theater ratio rising from 0.35 to 0.58 indicates that as extraction accumulates, the performance burden increases.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sanctions_effectiveness_threshold,
    'At what extraction level do sanctions cross from legitimate pressure tool to counterproductive collective punishment?',
    'Comparative analysis of sanctions regimes and political outcomes; econometric evidence on civilian harm vs policy change; longitudinal tracking of target regime behavior',
    'If threshold < current extraction (0.68): sanctions are counterproductive punishment, snare classification confirmed for all affected groups. If threshold > 0.80: extraction may be justified by policy leverage, tangled_rope classification sustained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanctions_effectiveness_threshold, empirical, 'Sanctions effectiveness threshold vs civilian harm').

omega_variable(
    regime_entrenchment_mechanism,
    'Do sanctions strengthen or weaken the Lukashenko regime''s grip on power in practice?',
    'Historical comparison: Belarus political stability indices pre/post-2022 sanctions; defection rates from security apparatus; popular support surveys; regime revenue sources (contraband, Russia, informal economy)',
    'If strengthening: sanctions are extraction mechanism benefiting regime (invert beneficiary declaration). If weakening: sanctions may have policy leverage (justify tangled_rope over pure snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_entrenchment_mechanism, empirical, 'Whether sanctions strengthen or weaken target regime').

omega_variable(
    civilian_vs_regime_distribution,
    'What fraction of sanction costs fall on civilians vs regime leadership / security apparatus?',
    'Granular economic impact analysis: inflation effects on wage earners vs oligarch asset values; access to restricted goods by income quintile; capital flight patterns; regime revenue from contraband vs legitimate trade',
    'If >80% civilian: pure snare classification. If >60% regime: tangled_rope classification sustained (mixed). If mixed with regime mitigation: asymmetry pronounced, supports snare reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_vs_regime_distribution, empirical, 'Distribution of sanction costs between civilians and regime').

omega_variable(
    opposition_coordination_benefit,
    'Do international sanctions materially strengthen the organizational capacity or international standing of Belarusian opposition movements?',
    'Tracking of opposition funding, international diplomatic access, organizational reach pre/post-sanctions; comparison with non-sanctioned autocracies'' opposition capacity',
    'If significant: validates tangled_rope as delivering real opposition benefit alongside civilian harm (mixed extraction). If negligible: opposition benefits are rhetorical only, reduces justification for snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opposition_coordination_benefit, empirical, 'Whether sanctions strengthen Belarusian opposition capacity').

omega_variable(
    alternative_pressure_mechanisms,
    'Would diplomatic engagement, economic partnership, or conditional assistance achieve U.S. policy goals with lower civilian extraction than sanctions?',
    'Counterfactual modeling; comparison with cases where engagement successfully influenced authoritarian behavior; cost-benefit analysis of diplomatic vs coercive approaches',
    'If yes: sanctions are not the only available coordination tool; choice to use high-extraction method (0.68) becomes policy decision, not structural necessity. Strengthens snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_pressure_mechanisms, preference, 'Availability of lower-extraction pressure alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_sanctions_belarus_2022, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ussby_tr_t0, us_sanctions_belarus_2022, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ussby_tr_t6, us_sanctions_belarus_2022, theater_ratio, 6, 0.5).
narrative_ontology:measurement(ussby_tr_t12, us_sanctions_belarus_2022, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(ussby_be_t0, us_sanctions_belarus_2022, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ussby_be_t6, us_sanctions_belarus_2022, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(ussby_be_t12, us_sanctions_belarus_2022, base_extractiveness, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_sanctions_belarus_2022, enforcement_mechanism).
narrative_ontology:affects_constraint(us_sanctions_belarus_2022, russian_sanctions_cascade).
narrative_ontology:affects_constraint(us_sanctions_belarus_2022, belarus_regime_legitimacy).
narrative_ontology:affects_constraint(us_sanctions_belarus_2022, eu_eastern_border_stability).

% DUAL FORMULATION NOTE:
% U.S. sanctions on Belarus are structurally distinct from (1) Russian sanctions cascade (different enforcement coalition, different extraction mechanisms) and (2) Belarus regime legitimacy constraint (regime's internal claim to power). The network links indicate that sanctions policy affects both downstream constraints: escalation to Russian sanctions may intensify the cascade effect, while civilian extraction may alter regime legitimacy dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_sanctions_belarus_2022, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
