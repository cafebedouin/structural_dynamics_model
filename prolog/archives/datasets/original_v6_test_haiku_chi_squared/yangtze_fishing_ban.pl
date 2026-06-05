% ============================================================================
% CONSTRAINT STORY: yangtze_fishing_ban
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_yangtze_fishing_ban, []).

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
 *   constraint_id: yangtze_fishing_ban
 *   human_readable: Yangtze River Fishing Ban
 *   domain: economic/environmental/political
 *
 * SUMMARY:
 *   The Yangtze River fishing ban, implemented by the Chinese government
 *   starting in 2020 with expansion through 2024, represents a structural
 *   tension between ecological restoration (a genuine collective action
 *   problem requiring government coordination to prevent tragedy of the
 *   commons) and concentrated livelihood extraction imposed on artisanal
 *   fishing communities without proportional compensation or universal
 *   enforcement. The constraint exhibits properties of both coordination
 *   (solving overfishing via binding prohibition) and extraction
 *   (concentrating costs on powerless populations while benefits accrue to
 *   diffuse future beneficiaries and institutional actors). Initial
 *   implementation focused on ecosystem justification and state-level
 *   coordination; over time, the theater component increased as enforcement
 *   inconsistencies emerged, transition programs proved inadequate, and
 *   enforcement burden fell disproportionately on small-scale fishers while
 *   industrial and well-connected actors found exemptions or alternative
 *   income streams. The constraint qualifies as Tangled Rope because it
 *   possesses both: (1) genuine coordination function — preventing tragedy of
 *   the commons and restoring fish populations — and (2) asymmetric
 *   extraction — imposing lifetime livelihood loss on artisanal fishers while
 *   providing diffuse, long-delayed benefits to other actors. The base
 *   extractiveness increased from 0.15 (early period: justified by ecological
 *   emergency) to 0.62 (current: enforcement burden + inadequate transition
 *   support reveals extraction structure). Theater ratio increased from 0.35
 *   to 0.55 as implementation inconsistencies became visible.
 *
 * KEY AGENTS:
 *   - Artisanal Fishers: Primary victims (powerless/trapped) — banned from intergenerational livelihood with inadequate compensation; geographic and skill-based immobility
 *   - Fishing Communities (Regional): Secondary victims and partial beneficiaries (moderate/constrained) — constrained by ban but gain long-term ecosystem benefits; mixed structural position
 *   - Chinese Government/State Apparatus: Primary beneficiary and enforcer (institutional/arbitrage) — coordinates ecosystem restoration, controls enforcement, captures international environmental credibility
 *   - Downstream Water Users: Beneficiary (institutional/arbitrage) — dam operators, irrigation systems, drinking water infrastructure benefit from water quality improvement and reduced sedimentation
 *   - Environmental NGOs and Scientists: Organized advocates (organized/constrained) — promote and monitor ban; dependent on state enforcement; benefit from policy goal realization
 *   - Fishery Transition Programs: Temporary support mechanism (organized/constrained) — designed with sunset clause as generational workforce turns over and alternative livelihoods develop
 *   - Fishing Gear Supply Industry: Victim with theatrical persistence (institutional/constrained) — loses market; persists through performative compliance and export rebranding
 *   - Analytical Observer: Sees potential natural law (analytical/analytical) — risks naturalizing contingent policy choice as immutable ecological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(yangtze_fishing_ban, 0.62).
domain_priors:suppression_score(yangtze_fishing_ban, 0.68).
domain_priors:theater_ratio(yangtze_fishing_ban, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(yangtze_fishing_ban, extractiveness, 0.62).
narrative_ontology:constraint_metric(yangtze_fishing_ban, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(yangtze_fishing_ban, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(yangtze_fishing_ban, tangled_rope).
narrative_ontology:human_readable(yangtze_fishing_ban, "Yangtze River Fishing Ban").
narrative_ontology:topic_domain(yangtze_fishing_ban, "economic/environmental/political").

domain_priors:requires_active_enforcement(yangtze_fishing_ban).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(yangtze_fishing_ban, yangtze_ecosystem_recovery).
narrative_ontology:constraint_beneficiary(yangtze_fishing_ban, downstream_water_users).
narrative_ontology:constraint_beneficiary(yangtze_fishing_ban, future_generations).
narrative_ontology:constraint_victim(yangtze_fishing_ban, artisanal_fishers).
narrative_ontology:constraint_victim(yangtze_fishing_ban, fishing_gear_suppliers).
narrative_ontology:constraint_victim(yangtze_fishing_ban, regional_fishing_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED ARTISANAL FISHER (SNARE) — Banned from primary livelihood with no alternative income source, no transition period, and no meaningful compensation relative to lifetime earnings. Trapped by geography (river is local livelihood) and skill set (fishing is intergenerational practice). d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.77.
constraint_indexing:constraint_classification(yangtze_fishing_ban, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL FISHING COMMUNITY (TANGLED ROPE) — Constrained by ban but also potentially benefits from ecosystem recovery (fish population restoration, water quality improvement benefit local populations over 15-20 year horizon). Active enforcement required; enforcement is resource-intensive. Mixed coordination (ecosystem restoration) and extraction (livelihood prohibition). d≈0.70, f(d)≈1.08, σ=0.9 → χ≈0.59.
constraint_indexing:constraint_classification(yangtze_fishing_ban, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CHINESE STATE APPARATUS (ROPE) — Coordinates ecosystem restoration across provinces; benefits from restored fish populations, improved water quality, and international environmental credibility. Has arbitrage exit (can relax ban if costs exceed benefits; has resources to sustain enforcement). Sees constraint primarily as coordination mechanism for collective action problem (tragedy of the commons). d≈0.08, f(d)≈-0.08, σ=1.1 → χ≈-0.05.
constraint_indexing:constraint_classification(yangtze_fishing_ban, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: DOWNSTREAM WATER INFRASTRUCTURE (ROPE) — Hydroelectric dams, irrigation systems, drinking water systems benefit from improved water quality and reduced sediment disruption. Pure coordination benefit; no extraction perceived. d≈0.05, f(d)≈-0.10, σ=1.1 → χ≈-0.05.
constraint_indexing:constraint_classification(yangtze_fishing_ban, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ENVIRONMENTAL NGOS AND SCIENTISTS (TANGLED ROPE) — Organized actors advocating for ban and monitoring compliance. Benefits from ecosystem restoration mandate (their policy goal achieved). Constrained by government authority (cannot unilaterally modify ban; dependent on state enforcement). Enforcement is active and visible. d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.20.
constraint_indexing:constraint_classification(yangtze_fishing_ban, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FISHERY TRANSITION PROGRAMS (SCAFFOLD) — Government-funded livelihood transition (aquaculture training, alternative employment, pension programs) with designed sunset: programs scale down as ecosystem recovery provides alternative employment (eco-tourism, ecosystem service monitoring) and as generational workforce turns over. Theater: implementation inconsistency across provinces. Has sunset clause (programs sunset as economy transitions to service-based livelihoods). d≈0.45, f(d)≈0.48, σ=0.9 → χ≈0.24.
constraint_indexing:constraint_classification(yangtze_fishing_ban, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: FISHING GEAR SUPPLY INDUSTRY (PITON) — Victim of ban (lost market); persists through theatrical compliance and claims of 'transition products' for export markets. Theater_ratio≈0.62. Institutional actor that maintains activity through performative rebranding rather than functional adaptation. d≈0.80, f(d)≈1.22, σ=1.0 → χ≈0.76.
constraint_indexing:constraint_classification(yangtze_fishing_ban, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scale, the Yangtze's carrying capacity for wild fish populations had reached zero under sustained industrial fishing pressure. The ban represents recognition of an immutable ecological constraint (extinction trajectory is irreversible below population thresholds). Emerges naturally from ecosystem dynamics. However, the structural data (ε=0.62, suppression=0.68, theater=0.55) contradicts pure mountain classification — the engine will compute this as a false summit, revealing that the 'law of nature' framing naturalizes what is actually a reversible policy choice.
constraint_indexing:constraint_classification(yangtze_fishing_ban, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(yangtze_fishing_ban_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(yangtze_fishing_ban, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(yangtze_fishing_ban, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(yangtze_fishing_ban, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(yangtze_fishing_ban, TR),
    TR >= 0.70.

:- end_tests(yangtze_fishing_ban_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The ban solves a genuine tragedy of the commons — Yangtze wild fish populations faced extinction under sustained industrial fishing pressure. However, the solution imposes concentrated, permanent costs on artisanal fishers (livelihood loss, no viable alternatives in many regions) while distributing benefits diffusely (ecosystem restoration benefits all downstream users over 20+ year horizon). Government transition programs exist but are significantly underfunded relative to lifetime earnings impact — benefit replacement < 50% in most cases. The extractiveness increased over time as inadequate transition support and enforcement inconsistency became visible. Suppression (0.68): High. Fishers face multiple barriers to exit or resistance: (1) geographic immobility (river is local livelihood), (2) skill-specific training (fishing knowledge not transferable to most alternative employment), (3) age barriers (older fishers ineligible for retraining programs), (4) enforcement threat (illegal fishing carries criminal penalties, not just fines). Alternative livelihoods (eco-tourism guide, aquaculture, environmental monitoring) are available but in limited quantity and require transition investment. Theater ratio (0.55): Moderate. Implementation theater includes: government announcements emphasizing ecological restoration and international environmental credibility; transition program rhetoric emphasizing 'just transition' despite inadequate funding; enforcement visibility focused on high-profile patrols while corruption/exemptions occur. However, functional enforcement is substantial — the ban does prevent most fishing activity, so theater < 0.70. The increase from 0.35 to 0.55 reflects growing gaps between announced transition support and actual implementation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates significant perspectival divergence. The artisanal fisher sees pure extraction (Snare) — a permanent prohibition with no viable alternatives and inadequate compensation, enforced through criminal penalties. The state apparatus sees coordination (Rope) — solving a collective action problem, restoring ecosystem health, coordinating across provincial enforcement zones. Environmental organizations see mixed coordination and partial extraction (Tangled Rope) — genuine ecological benefit but unfair distribution of costs. Transition programs see a temporary coordination mechanism with sunset (Scaffold) — designed to phase out as alternative employment develops. The fishing gear industry sees institutional degradation (Piton) — a lost market sustained through theatrical compliance and export rebranding. The downstream water infrastructure sees pure coordination benefit (Rope) — improved water quality and reduced sedimentation require no sacrifice from their perspective. The analytical observer risks seeing natural law (Mountain) — framing the ban as necessary response to immutable ecological limits — but the structural data reveals contingency: the coordination could have been achieved through alternative mechanisms (fishing quotas, territorial rights, eco-certification) that distribute costs differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Artisanal fishers: Victims + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; powerless agents with zero exit options (geographic/skill immobility). Government state apparatus: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; can adjust enforcement or permit exemptions if costs exceed benefits. Downstream water users: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.10. Pure beneficiary from state perspective; gain water quality with no cost. Environmental NGOs: Organized advocates + constrained → d≈0.35, f(d)≈0.32. Low-moderate extraction; dependent on state enforcement but have agency through advocacy. Transition programs: Organized implementation + constrained → d≈0.45, f(d)≈0.48. Low-moderate extraction; structured with sunset as alternative livelihoods develop. Fishing gear industry: Victim + constrained → d≈0.80, f(d)≈1.22. High extraction; lost market but can partially exit through export rebranding and theatrical compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (ε=0.62, above 0.46 threshold): The constraint avoids false natural law classification through structural decomposition. The temptation to classify as Mountain ('ecological necessity') is strong: Yangtze wild fish faced extinction; ban is required to prevent irreversible loss. However, the structural data reveals this as a false summit. (1) The ban is not inherent to ecological limits — alternative coordination mechanisms exist (fishing quotas, territorial allocation, eco-certification) that achieve ecosystem recovery with different cost distributions. (2) The asymmetric extraction (concentrated costs on artisanal fishers, diffuse benefits to downstream users and future generations) reveals contingent policy choice, not natural law. (3) The enforcement inconsistency and inadequate transition support indicate that the 'natural' solution is actually contested and unevenly implemented. The correct classification is Tangled Rope: genuine coordination function (ecosystem restoration) + asymmetric extraction (unequal burden distribution) + active enforcement (state monopoly on enforcement mechanism). The theater_ratio (0.55) is below the 0.70 piton threshold, confirming that functional enforcement exists (not just theatrical compliance). The mandate that the constraint must possess both coordination and asymmetric extraction is satisfied: (a) Coordination: ecosystem recovery prevents extinction, benefits all downstream actors. (b) Extraction: artisanal fishers bear disproportionate costs while powerless to influence policy. (c) Active enforcement: criminal penalties, patrol systems, monitoring required to maintain prohibition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecosystem_recovery_timeline,
    'How long until Yangtze fish populations recover to 1980s levels, and what populations are recoverable vs permanently extinct?',
    'Long-term ecological monitoring data; genetic analysis of remaining populations; comparison with post-industrial fishery recovery baselines from other rivers (Rhine, Danube, North American examples)',
    'If recovery timeline < 10 years: ban is justified pure coordination (Rope from state perspective). If > 30 years: extraction component increases (Snare from fisher perspective becomes more pronounced). If some populations permanently extinct: ban represents acceptance of irreversibility (raises question of whether coordination was possible earlier).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecosystem_recovery_timeline, empirical, 'Timeline for ecosystem recovery under ban').

omega_variable(
    livelihood_transition_sufficiency,
    'Do government transition programs provide income replacement sufficient for affected fishers to maintain pre-ban living standards?',
    'Longitudinal income tracking of displaced fishers; comparison of transition program participants vs control groups; analysis of pension adequacy and alternative employment uptake',
    'If sufficient (>80% income replacement): snare classification weakens; extraction component reduces. If insufficient (<50%): snare classification strengthens; suppression component increases (fishers cannot exit or afford alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(livelihood_transition_sufficiency, empirical, 'Whether transition programs adequately replace fisher incomes').

omega_variable(
    enforcement_equity_and_corruption,
    'Is the ban enforced uniformly across provinces and socioeconomic classes, or do wealthy actors and state-connected interests continue fishing through corruption/exemptions?',
    'Comparative analysis of enforcement intensity by province; monitoring of illegal fishing rates among elites vs artisanal fishers; inspection of exemption patterns',
    'If enforcement is inequitable: victim group (artisanal fishers) bears disproportionate extraction; snare classification correct. If enforcement is uniform: extraction component reduces; tangled_rope classification more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_equity_and_corruption, empirical, 'Uniformity and equity of ban enforcement').

omega_variable(
    alternative_protein_substitution,
    'Do alternative protein sources (aquaculture, imports, plant-based) adequately substitute for Yangtze wild fish in regional diets and markets?',
    'Market analysis of substitution rates; nutritional adequacy assessment; price impacts on low-income consumers; cultural acceptability of alternatives',
    'If substitution is effective and affordable: ban is coordination mechanism (benefits downstream users without harming food security). If substitution is incomplete: extraction component increases (food insecurity or market distortion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_protein_substitution, empirical, 'Adequacy of alternative protein sources').

omega_variable(
    transnational_coordination_failure,
    'Does the ban apply only to Chinese nationals, or does it constrain fishing by foreign fleets? Are neighboring countries enforcing compatible bans?',
    'International agreements analysis; monitoring of fleet composition in enforcement zones; comparison with Cambodia, Laos, Vietnam, Myanmar policies',
    'If ban is unilateral (China only): coordination failure undermines effectiveness; Snare classification strengthens (Chinese fishers sacrifice while foreign competitors benefit). If multinational coordination exists: pure coordination mechanism (Rope classification more accurate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transnational_coordination_failure, empirical, 'Whether ban is unilateral or part of transnational coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(yangtze_fishing_ban, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(yrfb_tr_t0, yangtze_fishing_ban, theater_ratio, 0, 0.35).
narrative_ontology:measurement(yrfb_tr_t3, yangtze_fishing_ban, theater_ratio, 3, 0.48).
narrative_ontology:measurement(yrfb_tr_t6, yangtze_fishing_ban, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(yrfb_be_t0, yangtze_fishing_ban, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(yrfb_be_t3, yangtze_fishing_ban, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(yrfb_be_t6, yangtze_fishing_ban, base_extractiveness, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(yangtze_fishing_ban, resource_allocation).
narrative_ontology:affects_constraint(yangtze_fishing_ban, upstream_dam_system_coordination).
narrative_ontology:affects_constraint(yangtze_fishing_ban, southeast_asian_fishery_governance).
narrative_ontology:affects_constraint(yangtze_fishing_ban, aquaculture_industry_expansion).

% DUAL FORMULATION NOTE:
% The fishing ban decomposes into two related constraints: (1) Ecosystem carrying capacity (Mountain: biological limit on wild fish populations under exploitation) with ε≈0.05; (2) Institutional coordination mechanism (Tangled Rope: policy choice on how to enforce the limit) with ε=0.62. The ban story addresses the institutional constraint; the ecosystem story addresses the natural limit they were responding to. The ban influences aquaculture expansion (substitution effect) and upstream dam coordination (joint ecosystem restoration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(yangtze_fishing_ban, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
