% ============================================================================
% CONSTRAINT STORY: climate_policy_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_policy_extraction, []).

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
 *   constraint_id: climate_policy_extraction
 *   human_readable: The Decarbonization Snare (Climate-Identity Collision)
 *   domain: environmental/political
 *
 * SUMMARY:
 *   The decarbonization snare emerges at the collision between planetary
 *   climate necessity and regional economic identity. Federal and
 *   international climate policies impose extractive costs on fossil
 *   fuel-dependent regions without adequate transition mechanisms, creating a
 *   pure extraction constraint. The regional economy — historically built on
 *   hydrocarbon wealth, employment, and political autonomy — experiences
 *   decarbonization as identity destruction and economic collapse. The
 *   constraint exhibits high suppression because workers and communities lack
 *   exit options: retraining is inadequate, geographic relocation is
 *   economically and socially catastrophic, and alternative economic
 *   development is underfunded and unproven. The theater_ratio (0.58)
 *   reflects performative 'just transition' language from federal policy
 *   apparatus that promises worker support without delivering material
 *   transition funding. Base extractiveness has increased from 0.35 to 0.68
 *   over the interval as policy mandates have tightened while transition
 *   mechanisms have failed to materialize. The constraint's mandatrophy is
 *   resolved by distinguishing it from temporary transition coordination
 *   (scaffold) — the snare persists because funding, timeline, and worker
 *   agency are all insufficient, making this pure extraction rather than
 *   managed coordination.
 *
 * KEY AGENTS:
 *   - Regional Fossil Fuel Workers: Primary victims (powerless/trapped) — experience maximum extraction through livelihood loss, retraining failure, community collapse
 *   - Hydrocarbon-Dependent Communities: Primary victims (moderate/constrained) — experience extraction of economic structure, regional autonomy, and cultural identity
 *   - Regional Political Coalition: Organized victims (organized/constrained) — organized resistance but trapped by federal authority and capital constraints
 *   - Major Energy Conglomerates: Secondary beneficiary (powerful/arbitrage) — can arbitrage away from stranded assets, investing in renewables; experience tangled rope (coordination + asymmetric extraction)
 *   - Global Climate Coalition: Beneficiary (institutional/arbitrage) — experiences decarbonization as pure coordination; blind to regional extraction because costs are borne elsewhere
 *   - Federal Climate Policy Apparatus: Institutional maintainer (institutional/arbitrage) — maintains performative 'just transition' theater while underfunding material transition mechanisms; piton classification reflects degradation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing political extraction as thermodynamic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_policy_extraction, 0.68).
domain_priors:suppression_score(climate_policy_extraction, 0.72).
domain_priors:theater_ratio(climate_policy_extraction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_policy_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_policy_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_policy_extraction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_policy_extraction, snare).
narrative_ontology:human_readable(climate_policy_extraction, "The Decarbonization Snare (Climate-Identity Collision)").
narrative_ontology:topic_domain(climate_policy_extraction, "environmental/political").

% --- Structural relationships ---
narrative_ontology:constraint_victim(climate_policy_extraction, regional_fossil_fuel_workers).
narrative_ontology:constraint_victim(climate_policy_extraction, hydrocarbon_dependent_communities).
narrative_ontology:constraint_victim(climate_policy_extraction, climate_action_political_space).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOSSIL FUEL WORKER (SNARE) — Trapped in a regional economy entirely dependent on hydrocarbon extraction. Decarbonization policy is experienced as pure extraction: loss of livelihood, community collapse, identity displacement. No viable exit options within the region. Maximum suppression through lack of alternative employment, retraining barriers, and geographic immobility. Experienced extractiveness approaches 1.0 — the constraint destroys the material and cultural substrate of their life without compensation mechanism.
constraint_indexing:constraint_classification(climate_policy_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL COMMUNITY (SNARE) — Broader community trapped by economic structure. While some exit is theoretically possible (migration, economic diversification), barriers are severe: real estate collapse, family/social networks, regional identity collapse, inadequate transition funding. The constraint extracts community cohesion, cultural continuity, and regional autonomy. The decarbonization mandate is experienced as externally imposed extraction without consultation or benefit-sharing.
constraint_indexing:constraint_classification(climate_policy_extraction, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGIONAL POLITICAL COALITION (SNARE) — Organized resistance movement (industry associations, elected officials, labor unions). Experiences decarbonization as extraction of regional political power and economic autonomy. Despite organization, exit is constrained — federal policy overrides regional preference, fossil fuel assets cannot be abandoned without massive losses, and coalition's power is declining relative to climate-focused constituencies. Organized but subordinated — the snare classification persists because the constraint actively suppresses alternative pathways (carbon pricing that protects regional assets, transition funding that preserves community structure).
constraint_indexing:constraint_classification(climate_policy_extraction, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR ENERGY CONGLOMERATE (TANGLED ROPE) — Large multinational can arbitrage: move capital to other regions, divest from stranded assets, invest in renewable energy and carbon capture technology. Decarbonization policy creates mixed effects — it closes some revenue streams but opens others (green energy, carbon credits, regulatory positioning). The constraint has a coordination function (capital allocation to cleaner assets) but the extraction mechanism is real (legacy assets are written off, workers and communities bear the transition cost while capital moves freely). Tangled rope classification reflects both the coordination function and the asymmetric extraction.
constraint_indexing:constraint_classification(climate_policy_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL CLIMATE COALITION (ROPE) — International climate movement, developed-nation environmental constituencies, climate scientists, and climate-focused policy bodies. Experiences decarbonization policy as pure coordination: mobilizing capital toward clean energy, reducing collective action problems in emissions reduction, enabling global cooperation on climate targets. The constraint is genuinely beneficial for this constituency — it coordinates global action on a commons problem. No experience of extraction (costs are borne elsewhere). However, from the snare perspectives, this is precisely the blindness: the coalition experiences coordination because the extraction is happening far from them.
constraint_indexing:constraint_classification(climate_policy_extraction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: FEDERAL CLIMATE POLICY APPARATUS (PITON) — Federal agencies, policy makers, and climate science institutions that implement decarbonization mandates. The apparatus maintains theater around 'just transition' language, promises of workforce retraining, and economic diversification grants — but delivery is minimal. Theater_ratio is high because the policy apparatus performs climate leadership while underfunding transition mechanisms. The original function (reduce emissions) remains, but it is increasingly detached from the actual coordination problem (managing regional collapse, preserving worker welfare). Piton classification reflects this degradation: the institutions maintain their own legitimacy through performative language while the material constraint persists unresolved.
constraint_indexing:constraint_classification(climate_policy_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — From a civilizational/universal view, one might argue that thermodynamic limits, carbon budgets, and planetary boundaries create an immutable constraint on fossil fuel use — decarbonization is a natural law. However, this perspective naturalizes what is actually a political choice about WHO BEARS THE COST. The thermodynamic limit is real, but the distribution mechanism (regional workers bear full cost, capital relocates, global coalition captures benefit) is contingent. The engine's false summit detector identifies this as naturalization of a politically constructed snare. The 'natural law' framing obscures the extraction mechanism.
constraint_indexing:constraint_classification(climate_policy_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_policy_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_policy_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_policy_extraction, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_policy_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_policy_extraction, TR),
    TR >= 0.70.

:- end_tests(climate_policy_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts approximately two-thirds of the potential welfare gain from climate policy from regional workers and communities, transferring it to capital owners and global climate constituencies. Extraction is measured as loss of livelihood, asset value, community cohesion, and regional autonomy relative to the counterfactual of managed transition. The trajectory (0.35→0.68) reflects acceleration as policy mandates tighten while transition mechanisms fail to materialize. Suppression (0.72): High. Regional workers face multiple suppression mechanisms: geographic immobility (family, cultural networks, real estate collapse), skill mismatch (hydrocarbon skills do not transfer to renewable energy), inadequate retraining funding, publication bias in policy literature (successful transitions are highlighted; failures are ignored or blamed on local incompetence), and political inequality (fossil fuel lobby has more influence on transition timelines than regional workers). Theater ratio (0.58): Moderate-high. Federal policy apparatus maintains substantial performative content: 'just transition' rhetoric, workforce retraining promises, economic diversification grants are announced and publicized but delivery is minimal. The gap between rhetorical commitment (100%) and material delivery (~15-20% of actual transition costs) is substantial. However, unlike the pure piton (0.70+), some genuine coordination is occurring — renewable energy investment is real, some worker transitions do succeed, and policy apparatus is not purely theatrical. The moderate theater reflects the gap between promise and delivery.
 *
 * PERSPECTIVAL GAP:
 *   The decarbonization snare demonstrates radical perspectival divergence. The fossil fuel worker sees snare (extraction with no exit). The regional community sees snare (identity erasure). The organized coalition sees snare (political subordination). The energy conglomerate sees tangled rope (coordination with asymmetric extraction). The global climate coalition sees rope (pure coordination). The federal apparatus sees its own piton (performative transition). The civilizational observer sees a false summit (thermodynamic necessity). These are not different measurements of the same phenomenon — they reflect genuine structural differences. The worker's snare is not 'perceived' as snare; it IS snare from their structural position because the constraint actively extracts and suppresses. The global coalition's rope is not 'misperception'; it IS rope from their position because they experience coordination without extraction. The perspectival gaps cannot be closed by better information — they reflect real distributional asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Each victim's directionality value is determined by their exit options and structural position. Fossil fuel workers (powerless/trapped) have d≈0.95 — nearly complete targets of extraction, with minimal exit options. Regional communities (moderate/constrained) have d≈0.75 — more exit possible through relocation or diversification, but at severe cost. Regional political coalition (organized/constrained) has d≈0.60 — organized enough to resist but trapped by federal authority and capital flight. Energy conglomerates (powerful/arbitrage) have d≈0.10 — almost complete beneficiaries with full exit options. Global climate coalition (institutional/arbitrage) has d≈0.05 — full beneficiaries with no extraction experienced. The federal apparatus (institutional/arbitrage) has d≈0.00 in its own perception (beneficiary) but d≈0.20 if measured against its stated transition commitments (partial victim of its own under-delivery). The snare classification is robust across all victim perspectives because suppression and extractiveness are high, exit options are severely constrained, and no genuine coordination function exists from the victims' structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The decarbonization snare resolves the mandatrophy by demonstrating that climate necessity (thermodynamic truth) and extractive policy design (political contingency) are structurally distinct. The thermodynamic limit is real: we must decarbonize to avoid catastrophic warming. But the EXTRACTION MECHANISM — who bears the cost and who captures the benefit — is politically constructed and therefore changeable. The snare classification persists NOT because decarbonization is impossible or wrong, but because current policy design imposes costs on powerless victims without adequate compensation or exit options. Converting the snare to a scaffold would require: (1) funding regional transition at 50%+ of asset replacement cost, (2) timeline extension to allow genuine economic development, (3) worker and community agency in transition design (not top-down mandates). If these conditions were met, the constraint would remain extractive in terms of regional preference (workers prefer fossil fuel economy), but suppression would decline (exit options improve) and the constraint would become manageable rather than catastrophic. The piton and scaffold perspectives are not 'softer' than snare — they represent materially different constraint structures. The false summit risks naturalizing the snare as 'necessary' when it is actually a failure of policy design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transition_funding_threshold,
    'At what level of per-capita transition funding does regional decarbonization shift from snare to scaffold?',
    'Comparative analysis of regional transition outcomes: regions with 50%+ asset replacement funding show measurable economic resilience; regions with <20% show community collapse. Identify the threshold where workers experience exit options rather than trap.',
    'If threshold achievable: snare can be converted to managed transition (scaffold). If threshold exceeds political will: snare persists indefinitely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transition_funding_threshold, empirical, 'Transition funding level that converts snare to scaffold').

omega_variable(
    regional_alternative_development,
    'Can regional economic alternative (renewable manufacturing, carbon capture hubs, advanced materials) be developed at sufficient scale and speed to absorb displaced workers before social collapse?',
    'Historical comparison with industrial transitions (coal-to-natural gas shifts, automotive manufacturing relocations); modeling of renewable energy job creation rates vs. fossil fuel job losses in specific regions.',
    'If viable and fast: snare is temporary (scaffold classification justified). If non-viable or too slow: snare persists across generational timescale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_alternative_development, empirical, 'Whether regional alternative development can match job displacement').

omega_variable(
    identity_substitution_possibility,
    'Can regional identity be credibly reframed from ''hydrocarbon heartland'' to ''renewable energy leader'' without appearing as erasure of legitimate community history?',
    'Ethnographic and political economy analysis: do communities that successfully rebrand report improved agency and reduced grievance? Or does identity displacement fuel long-term political alienation regardless of economic outcomes?',
    'If identity substitution is credible: political support for transition increases. If experienced as erasure: snare persists as grievance-generating extraction mechanism even if material transition succeeds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_substitution_possibility, conceptual, 'Whether regional identity can be reframed without alienation').

omega_variable(
    federal_enforcement_capacity,
    'Can federal climate policy be enforced against organized regional resistance without triggering destabilizing political backlash?',
    'Political economy analysis of enforcement mechanisms, regional electoral power, and coalition-building dynamics. Comparison with historical regulatory conflicts (air quality, water protection, labor standards).',
    'If enforcement succeeds: snare persists but is contained at regional scale. If enforcement fails: snare morphs into national political crisis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_enforcement_capacity, empirical, 'Whether federal climate enforcement can succeed without political destabilization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_policy_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_tr_t0, climate_policy_extraction, theater_ratio, 0, 0.28).
narrative_ontology:measurement(climate_tr_t5, climate_policy_extraction, theater_ratio, 5, 0.42).
narrative_ontology:measurement(climate_tr_t10, climate_policy_extraction, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(climate_be_t0, climate_policy_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(climate_be_t5, climate_policy_extraction, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(climate_be_t10, climate_policy_extraction, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_policy_extraction, resource_allocation).
narrative_ontology:affects_constraint(climate_policy_extraction, carbon_pricing_distributional_asymmetry).
narrative_ontology:affects_constraint(climate_policy_extraction, renewable_energy_supply_chain_extraction).

% DUAL FORMULATION NOTE:
% The decarbonization snare is upstream of more specific climate policy constraints. Carbon pricing mechanisms inherit the extraction structure of this broader snare — pricing that fails to compensate stranded assets and displaced workers is experienced as extraction layered atop the decarbonization snare. Renewable energy supply chains (battery minerals, panel manufacturing) create parallel extraction mechanisms in developing regions, making the global climate transition itself extractive at scale.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_policy_extraction, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
