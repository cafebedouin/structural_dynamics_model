% ============================================================================
% CONSTRAINT STORY: deforestation_commons
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deforestation_commons, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deforestation_commons
 *   human_readable: Deforestation Commons Trap
 *   domain: environmental/economic/social
 *
 * SUMMARY:
 *   Tropical deforestation represents a structural snare where immediate
 *   economic incentives for land conversion are deeply misaligned with
 *   ecosystem stability and indigenous welfare. The constraint exhibits the
 *   defining properties of pure extraction: high extractiveness concentrated
 *   on powerless agents (indigenous communities, future generations, regional
 *   climate) with minimal coordination benefit; suppression maintained
 *   through state power (property law, concessions, police enforcement),
 *   market structure (global commodity prices, corporate consolidation), and
 *   epistemic closure (naturalizing extraction as inevitable). The trajectory
 *   shows increasing extractiveness and suppression over the 20-year
 *   interval, with theater rising as conservation rhetoric, carbon credits,
 *   and certification schemes proliferate without reducing actual
 *   deforestation. Multiple perspectives reveal that the same structural
 *   phenomenon appears as an immutable economic law (mountain, false summit),
 *   a coordination mechanism for export firms (rope), a revenue source for
 *   states (tangled_rope), a capture opportunity for conservation
 *   organizations (tangled_rope), and an emerging alternative pathway for
 *   indigenous land recognition (scaffold). The snare classification is
 *   stable across the indigenous and future-generation perspectives — these
 *   are the agents who bear extraction with no exit.
 *
 * KEY AGENTS:
 *   - Indigenous Communities: Primary victim (powerless/trapped) — structurally excluded from property rights despite customary claims; livelihoods destroyed; subject to displacement and cultural erasure
 *   - Future Generations & Climate System: Primary victim (powerless/trapped at generational timescale) — excluded from decision-making; absorb carbon debt and ecosystem collapse
 *   - Timber and Agricultural Export Corporations: Primary beneficiary (institutional/arbitrage) — capture commodity prices, concession rents, and export value; suppression (enforcement against indigenous land claims) is their coordination cost
 *   - National States: Secondary beneficiary (institutional/constrained) — extract licensing revenue and export taxes; coordinate rural employment but constrained by climate commitments
 *   - Conservation & Rights Organizations: Intermediate actor (organized/constrained) — participate in certification and PES schemes that legitimize extraction while maintaining symbolic indigenous recognition
 *   - Indigenous Coalitions & Rights Networks: Emerging organized agent (organized/mobile) — building transnational pressure to shift from trapped to organized status through legal and political mobilization
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing extractive institutions as economic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deforestation_commons, 0.68).
domain_priors:suppression_score(deforestation_commons, 0.72).
domain_priors:theater_ratio(deforestation_commons, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deforestation_commons, extractiveness, 0.68).
narrative_ontology:constraint_metric(deforestation_commons, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(deforestation_commons, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deforestation_commons, snare).
narrative_ontology:human_readable(deforestation_commons, "Deforestation Commons Trap").
narrative_ontology:topic_domain(deforestation_commons, "environmental/economic/social").

domain_priors:requires_active_enforcement(deforestation_commons).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deforestation_commons, '50b0626a-7c15-4f83-ad22-843a3170ffed').
narrative_ontology:cs_kernel_codification('50b0626a-7c15-4f83-ad22-843a3170ffed', distributed).
narrative_ontology:cs_authority_grounding('50b0626a-7c15-4f83-ad22-843a3170ffed', extraction).
narrative_ontology:cs_reading_relation('50b0626a-7c15-4f83-ad22-843a3170ffed', deforestation_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('50b0626a-7c15-4f83-ad22-843a3170ffed', deforestation_indigenous_land_reading, forecloses).
narrative_ontology:cs_axiom('50b0626a-7c15-4f83-ad22-843a3170ffed', foundational, forest_conversion_economically_necessary).
narrative_ontology:cs_axiom_status(forest_conversion_economically_necessary, holdable).
narrative_ontology:cs_axiom_grounding('50b0626a-7c15-4f83-ad22-843a3170ffed', forest_conversion_economically_necessary, empirically_contingent).
narrative_ontology:cs_axiom('50b0626a-7c15-4f83-ad22-843a3170ffed', foundational, state_sovereignty_over_resources).
narrative_ontology:cs_axiom_status(state_sovereignty_over_resources, holdable).
narrative_ontology:cs_axiom_grounding('50b0626a-7c15-4f83-ad22-843a3170ffed', state_sovereignty_over_resources, conventional).
narrative_ontology:cs_axiom('50b0626a-7c15-4f83-ad22-843a3170ffed', foundational, indigenous_stewardship_efficacy).
narrative_ontology:cs_axiom_status(indigenous_stewardship_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('50b0626a-7c15-4f83-ad22-843a3170ffed', indigenous_stewardship_efficacy, empirically_contingent).
narrative_ontology:cs_reference_frame('50b0626a-7c15-4f83-ad22-843a3170ffed', state_development_maximization).
narrative_ontology:cs_drift_state('50b0626a-7c15-4f83-ad22-843a3170ffed', contemporary_climate_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('50b0626a-7c15-4f83-ad22-843a3170ffed', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deforestation_commons, timber_extraction_corporations).
narrative_ontology:constraint_beneficiary(deforestation_commons, agricultural_export_firms).
narrative_ontology:constraint_beneficiary(deforestation_commons, state_revenue_collection).
narrative_ontology:constraint_victim(deforestation_commons, indigenous_communities).
narrative_ontology:constraint_victim(deforestation_commons, future_generations).
narrative_ontology:constraint_victim(deforestation_commons, regional_climate_stability).
narrative_ontology:constraint_victim(deforestation_commons, biodiversity_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS COMMUNITIES (SNARE) — Structurally trapped. Possess no exit option from forest-dependent livelihoods; property rights recognized only ceremonially while state concessions override customary claims. Bear full extraction cost through displacement, livelihood destruction, and cultural erosion. Zero coordination benefit; pure subordination to extractive claims. Maximum experienced extraction — powerless agents with no alternatives.
constraint_indexing:constraint_classification(deforestation_commons, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FUTURE GENERATIONS & CLIMATE SYSTEM (SNARE) — Structurally excluded from decision-making; cannot negotiate, organize, or exit. Absorb extraction in the form of reduced carbon sinks, climate destabilization, and collapsed ecosystems. No voice in current institutional arrangements; no recourse mechanism. Trapped on a civilizational timescale.
constraint_indexing:constraint_classification(deforestation_commons, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: EXTRACTION CORPORATIONS (ROPE) — Experience the constraint as efficient coordination. Access to concessions, transport networks, and export markets are successfully coordinated. Suppression (enforcement of property claims against indigenous resistance) is the coordination cost — externalized but necessary for smooth extraction. Net beneficiary; sees constraint as enabling their function.
constraint_indexing:constraint_classification(deforestation_commons, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NATIONAL STATES (TANGLED ROPE) — Mixed position. Coordinate timber revenue, agricultural export growth, and rural employment through concession systems. Genuine coordination function. Simultaneously extract rent through licensing fees and taxes. Constrained by international climate commitments (Paris, UNFCCC) that raise exit costs if deforestation accelerates. Institutional power mediates the constraint but state revenue depends on continued extraction.
constraint_indexing:constraint_classification(deforestation_commons, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSERVATION & RIGHTS ORGANIZATIONS (TANGLED ROPE) — Organized agents with constrained exit. Participate in the constraint through carbon credit schemes, sustainable forestry certification, and indigenous land recognition programs — these ostensibly coordinate conservation with development. But the coordination is asymmetric: corporations capture certification benefits while indigenous communities remain materially displaced. Organizations face pressure (funding, political access) to legitimize extractive schemes through certification. Moderate extraction; genuine organizing power but structural capture.
constraint_indexing:constraint_classification(deforestation_commons, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: RESTORATION & ALTERNATIVE ECONOMY (SCAFFOLD) — Emerging pathways: payment for ecosystem services (PES), indigenous land title recognition, regenerative agriculture, and forest restoration create parallel coordination mechanisms with sunset logic. These reduce the extractive mechanism's value by creating alternative land-use economics. Theater is moderate (some PES projects are performative carbon accounting) but the structural direction is away from deforestation as inevitable. Sunset clause: as PES markets mature and indigenous land rights are legally codified, deforestation-as-extraction loses institutional support.
constraint_indexing:constraint_classification(deforestation_commons, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — Risk perspective: from a civilizational/universal view, economic pressure on finite natural resources is inherent to capitalism; commons degradation is a law of economic mechanics (tragedy of the commons). Sees deforestation as inevitable unless fundamentally restructured. This perspective naturalizes what is actually a contingent institutional arrangement (property law, concession systems, state sovereignty over resources). Engine will flag as false summit: institutional structures are changeable; the 'inevitability' is constructed, not natural.
constraint_indexing:constraint_classification(deforestation_commons, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deforestation_commons_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(deforestation_commons, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deforestation_commons, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(deforestation_commons, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deforestation_commons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting the massive asymmetry in benefits and costs. Corporations and states capture commodity rents while indigenous communities bear livelihood destruction. Future generations bear carbon debt (global climate externality). The extraction is not maximal (0.85+) because some coordination function exists — timber, agricultural commodities, and rural employment are genuinely produced. But the distribution is radically asymmetric: the beneficiaries capture surplus while the costs are externalized to powerless agents with no exit. Suppression (0.72): High, reflecting state enforcement of property claims and corporate exclusion of indigenous land management. Suppression includes: police enforcement against land defenders, legal structures that override customary rights, military protection of concessions, and political repression of environmental movements. This is not total suppression (0.95+) because some indigenous communities do organize effective resistance, and some states face international pressure. Theater ratio (0.55): Moderate, reflecting that while extractive mechanisms are directly functional (timber does get logged, commodities do get exported), conservation rhetoric, certification schemes, and carbon credits have grown substantially without reducing actual deforestation rates. The theater has been rising as corporations adopt 'sustainable forestry' labels and states sign climate commitments while concession rates persist. The trajectory from 0.35 to 0.55 reflects the increasing gap between conservation messaging and extraction reality. Rising suppression (0.58 to 0.72) reflects intensifying state and corporate enforcement against land defense movements as resistance has organized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a profound perspectival gap between beneficiaries and victims. Corporations see coordination (Rope) — the constraint enables efficient market access and commodity flow. States see mixed coordination and revenue (Tangled Rope) — they coordinate export economies while extracting rent and suppressing resistance. Conservation organizations see a mixed system where certification and PES schemes partially coordinate sustainability (Tangled Rope) — but from indigenous perspective, these same schemes are capture mechanisms that legitimize ongoing extraction. Indigenous communities see pure extraction (Snare) — no coordination benefit, only dispossession and livelihood loss. Future generations are excluded from the perspectival framework entirely (no current voice, no current power or exit options). The analytical observer risks seeing inevitable economic law (Mountain, false summit) — 'developing countries must exploit natural resources to finance growth' — which naturalizes what is actually a contingent institutional choice (property regimes, concession systems, commodity trade structures). The scaffold perspective (emerging indigenous land title and PES alternatives) offers a structural exit from the snare if alternative land-use economics can scale.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each agent class is determined by their structural position relative to the extraction flow. Indigenous communities as powerless/trapped agents experience maximum directionality toward extraction (d ≈ 0.95) because they are pure targets with no exit capacity. Corporations as institutional/arbitrage beneficiaries experience minimum directionality (d ≈ 0.05) because they are net beneficiaries with multiple exit options (can shift to other commodities, other territories, other sectors). States are intermediate (d ≈ 0.45) because they benefit from extraction revenue but are constrained by climate commitments and international pressure. Future generations have no voice in current directionality calculations — they are structurally excluded from the (P,T,E,S) tuple. The snare classification is robust across the most powerless perspectives: indigenous communities and the climate system bear pure extraction with no coordination benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by indexing to agent power and exit options. From the perspective of indigenous communities (powerless/trapped), this is unambiguously a Snare: high extraction, high suppression, minimal coordination function, no exit. From the perspective of extraction corporations (institutional/arbitrage), this is Rope: coordination with externalized suppression cost. The analytical observer risks false-summit error by naturalizing the extractive structure as inevitable economic law. The snare classification is the structural truth — it correctly identifies that the constraint's primary function is to extract value from powerless agents while suppressing alternatives. This is not a case of legitimate coordination with asymmetric distribution; it is extraction with minimal coordination function (timber production and export are real but are not the constraint's primary purpose — suppression of indigenous land claims is). The theater ratio supports this: rising conservation rhetoric and certification with stable deforestation rates indicates performative legitimation, not functional coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_land_title_closure,
    'Does full legal recognition of indigenous land rights actually prevent industrial deforestation, or does it merely shift extraction mechanisms to adjacent territories and debt-based coercion?',
    'Comparative analysis of deforestation rates on titled vs. untitled indigenous lands; tracking of corporate tactics post-titling (debt traps, land-grabbing via intermediaries, political pressure for title revocation)',
    'If effective: indigenous title is genuine constraint on extraction (snare becomes rope or tangled_rope from indigenous perspective). If shifting: extraction mechanism persists through market capture and coercion; title provides symbolic recognition without material protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_land_title_closure, empirical, 'Whether indigenous land title legally prevents industrial deforestation').

omega_variable(
    pes_additionality_and_permanence,
    'Do payment-for-ecosystem-services schemes actually prevent deforestation that would otherwise occur, or do they subsidize conservation that would happen anyway (permanence, additionality, and leakage failures)?',
    'Counterfactual analysis of forest cover in PES areas vs. comparable unsubsidized areas; longitudinal tracking of land-use after PES contracts expire; measurement of carbon leakage (displacement to unmonitored territories)',
    'If additionality confirmed: PES represents genuine coordination mechanism and scaffold dynamics are structural. If additionality fails: PES is theater (carbon accounting fraud); scaffold sunset is aspirational rather than real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pes_additionality_and_permanence, empirical, 'Whether PES schemes actually prevent deforestation or merely subsidize inevitable conservation').

omega_variable(
    state_enforcement_capacity_and_rent_seeking,
    'Does state enforcement of deforestation bans actually reduce forest loss, or does it primarily shift extraction to corruption and informal concessions that the state tacitly tolerates in exchange for bribes?',
    'Analysis of enforcement rates, conviction rates for illegal logging, satellite monitoring of protected areas; corruption indices for forest agencies; comparison of official vs. satellite-detected forest loss',
    'If enforcement effective: suppression metric is accurate; state is genuine constraint. If enforcement fails: suppression is theater; extraction persists through corruption; state benefit is rent extraction from tolerated illegal activity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_capacity_and_rent_seeking, empirical, 'Whether state enforcement actually reduces deforestation or masks rent-seeking through corruption').

omega_variable(
    global_supply_chain_responsibility,
    'Do consumer-country import restrictions and corporate supply-chain verification actually reduce tropical deforestation, or do they merely displace extraction to weaker-governed territories and unverified supply chains?',
    'Tracking of forest loss in ''cleaned'' vs. ''uncleaned'' supply-chain regions; measurement of corporate scope-3 emissions and downstream deforestation; enforcement rate of import restrictions and corporate commitments',
    'If effective: global coordination mechanism emerging; snare being partially converted to tangled_rope. If ineffective: responsibility claims are theater; global extraction persists with added performative layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_supply_chain_responsibility, empirical, 'Whether supply-chain responsibility reduces deforestation or merely displaces it').

omega_variable(
    indigenous_organizing_and_coalition_power,
    'Can indigenous communities achieve collective bargaining power through transnational organizing (indigenous networks, international forums, legal support) to shift from trapped to organized/constrained status?',
    'Measurement of indigenous land defense success rates; tracking of land reclaimed through legal action or political pressure; analysis of indigenous federation capacity and resource access; comparison of outcomes in highly organized vs. fragmented regions',
    'If coalition power emerges: powerless agents upgrade to organized; classification shifts from snare to tangled_rope from indigenous perspective. If organizing fails or is suppressed: trap remains structural despite symbolic recognition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_organizing_and_coalition_power, empirical, 'Whether indigenous coalition organizing can upgrade from trapped to organized status').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deforestation_commons, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defcom_tr_t0, deforestation_commons, theater_ratio, 0, 0.35).
narrative_ontology:measurement(defcom_tr_t10, deforestation_commons, theater_ratio, 10, 0.48).
narrative_ontology:measurement(defcom_tr_t20, deforestation_commons, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(defcom_be_t0, deforestation_commons, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(defcom_be_t10, deforestation_commons, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(defcom_be_t20, deforestation_commons, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(defcom_su_t0, deforestation_commons, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(defcom_su_t10, deforestation_commons, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(defcom_su_t20, deforestation_commons, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deforestation_commons, resource_allocation).
narrative_ontology:affects_constraint(deforestation_commons, carbon_sink_capacity).
narrative_ontology:affects_constraint(deforestation_commons, indigenous_land_rights_recognition).
narrative_ontology:affects_constraint(deforestation_commons, commodity_price_volatility).
narrative_ontology:affects_constraint(deforestation_commons, biodiversity_loss_cascade).

% DUAL FORMULATION NOTE:
% Deforestation operates at the intersection of multiple constraint families. The extractiveness metric here (0.68) captures the asymmetric distribution of benefits and costs between corporations/states and indigenous/future generations. A separate constraint story ('commodity_export_economy_structure') would model the coordination function that deforestation serves (commodity production, rural employment, state revenue) with lower extractiveness (0.35-0.45). A third story ('indigenous_land_tenure_insecurity') would isolate the property-rights dispute with even higher extraction (0.75+). These three stories form a decomposed family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deforestation_commons, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
