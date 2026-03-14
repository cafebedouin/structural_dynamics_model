% ============================================================================
% CONSTRAINT STORY: indigenous_resource_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indigenous_resource_rights, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: indigenous_resource_rights
 *   human_readable: Indigenous Resource Rights Extraction and Institutional Suppression
 *   domain: political_economy/resource_governance
 *
 * SUMMARY:
 *   Indigenous resource rights constraints represent a multi-century
 *   institutional extraction mechanism operating through colonial land tenure
 *   systems, asymmetric legal recognition of resource claims, and suppression
 *   of alternative governance frameworks. The constraint exhibits high
 *   extractiveness (0.68) and severe suppression (0.75) because indigenous
 *   communities lack both structural exit options and cultural freedom to
 *   conceptualize alternatives outside the imposed resource regime. The
 *   theater ratio (0.68) reflects the substantial performative apparatus —
 *   indigenous consultation processes, benefit-sharing agreements,
 *   environmental impact assessments, and international rights declarations —
 *   that maintain the appearance of indigenous participation while actual
 *   resource sovereignty and long-term benefit flow remain concentrated in
 *   extractive corporations and state agencies. The constraint classifies as
 *   Snare from the powerless indigenous community perspective, while
 *   appearing as Rope to extractive corporations (who benefit and have exit
 *   options) and as degraded Piton when viewed through the international
 *   human rights regime lens (which declares rights without enforcement
 *   mechanisms).
 *
 * KEY AGENTS:
 *   - Indigenous Communities: Primary victim (powerless/trapped and identity_locked) — structurally and identitatively bound to contested lands; bear environmental and health costs while resource wealth flows outward
 *   - Extractive Corporations: Primary beneficiary (institutional/arbitrage) — capture resource value and rents; have exit options and arbitrage flexibility across jurisdictions
 *   - State Resource Agencies: Secondary beneficiary (institutional/arbitrage) — capture tax revenue and licensing fees; maintain legal monopoly on resource alienation
 *   - Indigenous Rights Movements: Organized victim-coalition (organized/constrained) — experience both genuine coordination benefits (coalition capacity) and extraction costs (repression, resource constraints)
 *   - International Development States: Secondary beneficiary (powerful/mobile) — benefit from stable commodity supply chains; coordinate through trade relationships
 *   - International Indigenous Rights Regime: Institutional actor (institutional/arbitrage) — maintains performative rights declarations with minimal enforcement; degraded Piton classification
 *   - Ecosystem/Intergenerational Access: Abstract victim (powerless/trapped) — environmental degradation concentrates locally; resource depletion undermines future indigenous livelihood
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indigenous_resource_rights, 0.68).
domain_priors:suppression_score(indigenous_resource_rights, 0.75).
domain_priors:theater_ratio(indigenous_resource_rights, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indigenous_resource_rights, extractiveness, 0.68).
narrative_ontology:constraint_metric(indigenous_resource_rights, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(indigenous_resource_rights, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indigenous_resource_rights, snare).
narrative_ontology:human_readable(indigenous_resource_rights, "Indigenous Resource Rights Extraction and Institutional Suppression").
narrative_ontology:topic_domain(indigenous_resource_rights, "political_economy/resource_governance").

domain_priors:requires_active_enforcement(indigenous_resource_rights).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indigenous_resource_rights, extractive_corporations).
narrative_ontology:constraint_beneficiary(indigenous_resource_rights, state_resource_agencies).
narrative_ontology:constraint_beneficiary(indigenous_resource_rights, global_commodity_markets).
narrative_ontology:constraint_victim(indigenous_resource_rights, indigenous_communities).
narrative_ontology:constraint_victim(indigenous_resource_rights, ecosystem_integrity).
narrative_ontology:constraint_victim(indigenous_resource_rights, intergenerational_resource_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS COMMUNITIES (SNARE) — Trapped by colonial land tenure systems, economic dependency on resource-extraction-adjacent labor, geographic immobility, and legal asymmetry in resource claim recognition. No viable exit from the constraint without abandoning territorial identity and land base. Extraction is severe: resource wealth flows outward while environmental degradation and health impacts concentrate locally. Suppression mechanisms include legal formalism that denies indigenous claim legitimacy, militarized protection of extraction sites, and epistemic suppression of indigenous knowledge systems that might identify alternatives.
constraint_indexing:constraint_classification(indigenous_resource_rights, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INDIGENOUS COMMUNITIES (SNARE, IDENTITY-LOCKED) — At generational time horizon, the constraint appears as identity lock rather than simple material trapping. Indigenous identity is constituted through relationship to ancestral lands and resources. Exit from the constraint would require abandoning not just location but the identity framework itself — becoming 'indigenous' without the lands from which indigeneity draws meaning. This is structurally different from material poverty; the agent is identity-fused with the constraint space. Suppression persists even when material barriers weaken because the identity frame prevents recognition of alternatives.
constraint_indexing:constraint_classification(indigenous_resource_rights, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXTRACTIVE CORPORATION (ROPE) — Benefits from the constraint and experiences it as coordination: securing resource access, managing supply chains, navigating regulatory frameworks. The corporation has exit options (arbitrage) — can move operations to jurisdictions with weaker indigenous rights regimes, can divest from specific projects, can substitute resources. From this position, the constraint appears as a coordination mechanism enabling profitable resource extraction. No experienced extraction; net beneficiary.
constraint_indexing:constraint_classification(indigenous_resource_rights, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIGENOUS RIGHTS MOVEMENT (TANGLED ROPE) — Organized agents (indigenous federations, NGOs, regional coalitions) experience mixed coordination and extraction. The constraint enables indigenous coalition-building and international norm-sharing (genuine coordination benefit). But the movement also bears costs: repression, legal costs, limited resources to match corporate legal capacity. Exit options are constrained — leaving the movement means abandoning collective voice. Chi is moderate-high because the coalition has agency and some exit paths, but faces real coercive pressure.
constraint_indexing:constraint_classification(indigenous_resource_rights, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL DEVELOPMENT STATE (TANGLED ROPE) — Wealthy nations in the Global North coordinate resource imports (real coordination benefit) while also extracting value through pricing asymmetries and commodity dependence of source countries. Mobile exit options (can source from alternative suppliers, invest in substitutes) but also genuine coordination function (stable supply chains, trade relationships). Moderate extraction experienced by this institutional actor.
constraint_indexing:constraint_classification(indigenous_resource_rights, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL INDIGENOUS RIGHTS REGIME (PITON) — UN Declaration on the Rights of Indigenous Peoples (UNDRIP), ILO Convention 169, and similar instruments create the appearance of indigenous rights protection while lacking enforcement mechanisms and subordinate to state sovereignty doctrines. The regime is substantially performative: high theater ratio (international declarations without domestic implementation), degraded function (non-binding or weakly binding), maintained through institutional inertia. The regime gives the impression of addressing the constraint while the underlying extraction mechanism persists.
constraint_indexing:constraint_classification(indigenous_resource_rights, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW FRAME (MOUNTAIN) — From a civilizational perspective, some tension between resource access and indigenous land stewardship is inherent to human settlement patterns and economic development — an immutable constraint of organizing human communities at scale. However, this perspective risks naturalizing a contingent institutional arrangement (colonial land tenure + resource markets + suppression of indigenous governance systems) as an inherent limit. The engine's false summit detector will flag this as inappropriate naturalization: the constraint is structurally contingent, not a law of nature.
constraint_indexing:constraint_classification(indigenous_resource_rights, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indigenous_resource_rights_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indigenous_resource_rights, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indigenous_resource_rights, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indigenous_resource_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indigenous_resource_rights, TR),
    TR >= 0.70.

:- end_tests(indigenous_resource_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Extractive corporations and state agencies capture majority resource value while indigenous communities receive small fraction of economic rent. Historical trend shows increasing extractiveness as commodity markets expand and scale of operations grows (measurement trajectory: 0.52 → 0.60 → 0.68). Suppression (0.75): Very high. Multiple overlapping suppression mechanisms: (1) Legal — colonial land tenure denies indigenous claim legitimacy, state sovereignty doctrine subordinates indigenous rights; (2) Economic — resource extraction creates dependency on wage labor while restricting alternative livelihoods; (3) Coercive — military/police protection of extraction sites, criminalization of protest; (4) Epistemic — indigenous knowledge systems marginalized in favor of state/corporate management frameworks. Theater ratio (0.68): High and increasing. International human rights declarations, indigenous consultation processes, environmental impact assessments, benefit-sharing agreements create performative apparatus suggesting indigenous participation while actual governance and resource control remain concentrated. Theater has increased over the interval as international attention to indigenous rights has created pressure for visible consultation processes that lack enforcement or actual authority transfer.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The indigenous community trapped by the constraint sees Snare (maximum extraction, no exit). The extractive corporation benefiting from the constraint sees Rope (coordination mechanism enabling profitable operations). The organized indigenous rights movement sees Tangled Rope (genuine coalition benefits alongside real costs). The international human rights regime sees Piton (performative declarations with degraded enforcement function). The civilizational analytical observer risks Mountain classification (naturalizing resource extraction as inherent to development) — a false summit the engine should detect. The gap reveals that the constraint is not a natural law or pure coordination problem, but a highly contingent institutional arrangement (colonial land tenure + global commodity markets + suppression of alternative governance) that persistently extracts value from powerless agents while distributing costs to trapped and identity-locked populations.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by structural position relative to the extraction flow. Indigenous communities: d ≈ 0.95 (full targets, powerless/trapped/identity_locked, zero exit options, victim status) → f(d) ≈ 1.42 (maximum experienced extraction chi). Extractive corporations: d ≈ 0.10 (beneficiary status, institutional power, arbitrage exit, non-victim) → f(d) ≈ -0.01 (experienced extraction runs toward them, not away). Indigenous rights movement coalition: d ≈ 0.60 (mixed victim-beneficiary: benefit from coalition coordination, but bear repression costs; constrained exit) → f(d) ≈ 0.75 (moderate-high experienced extraction). International development states: d ≈ 0.35 (beneficiary from supply security, but mobile exit if supply disrupted) → f(d) ≈ 0.35 (low-moderate experienced extraction). The perspectival gaps reflect fundamental differences in structural position: those who benefit from the constraint's persistence (corporations, states, importing nations) classify it as coordination (Rope); those who bear costs without exit (indigenous communities) classify it as pure extraction (Snare); those with organized but constrained agency see mixed dynamics (Tangled Rope).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STRUCTURE: The constraint exhibits high extractiveness (0.68 > 0.46) without proportional coordination benefits from the perspective of the primary victims (indigenous communities). From the beneficiary perspectives (corporations, states), it appears as coordination — they benefit while solving a real problem (securing resource supply, generating government revenue). From the victim perspectives, it appears as pure extraction with suppressed coordination function. The mandatrophy is partially resolved by recognizing that this is NOT a case where the beneficiary's coordination interpretation is correct — the 'coordination' experienced by extractors is asymmetric extraction from the victim perspective. The international rights regime's Piton classification (performative without function) suggests the mandatrophy is maintained through theater: visible rights declarations prevent reframing as pure Snare while actual resource control and extraction mechanisms persist unchanged. True mandatrophy resolution would require either: (1) genuine transfer of resource sovereignty to indigenous communities (reclassifying as Scaffold with sunset toward indigenous governance) or (2) explicit acknowledgment that the constraint is Snare with no coordination benefit to trapped agents (mandatrophy unresolved, constraint as designed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_vs_coercion_threshold,
    'At what level of indigenous participation and benefit-sharing does extraction transition from snare to tangled rope? What constitutes meaningful consent versus performative consultation?',
    'Analysis of cases with varying benefit-sharing arrangements; tracking of community satisfaction and resource retention over multi-decade timescales; comparison of outcomes where indigenous communities held decision-making power vs advisory power',
    'If meaningful consent is achievable within current frameworks: some constraints reclassify as tangled_rope or scaffold (temporary pending alternative livelihoods). If institutional barriers prevent genuine consent: constraint remains snare across all observables.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_coercion_threshold, empirical, 'Threshold for genuine consent vs performative consultation').

omega_variable(
    alternative_livelihood_viability,
    'Are sustainable alternative livelihoods (sustainable forestry, ecotourism, traditional resource harvesting scaled to market demand) genuinely viable substitutes for extraction income, or are they constrained by market access and capital requirements?',
    'Comparative case study of indigenous communities pursuing extraction vs alternatives; cost-benefit analysis including health/environmental externalities; longitudinal income tracking across 20+ year periods',
    'If viable alternatives exist: suppression metric decreases, exit options improve from trapped to constrained. If alternatives are structurally unavailable: suppression remains high, constraint persists as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_livelihood_viability, empirical, 'Viability of sustainable alternative livelihoods').

omega_variable(
    epistemic_sovereignty_restoration,
    'Can indigenous knowledge systems and self-determined resource governance frameworks provide functionally equivalent or superior resource stewardship compared to state/corporate management, and would restoration of epistemic authority constitute genuine exit from the constraint?',
    'Longitudinal analysis of resource governance outcomes in regions with varying degrees of indigenous authority restoration; comparison of ecosystem health, cultural continuity, and community wellbeing metrics; assessment of institutional barriers to epistemic sovereignty',
    'If restoration is feasible: constraint reclassifies as scaffold (temporary pending governance transition). If institutional barriers are insurmountable: constraint remains snare; identity-lock perspective becomes primary analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_sovereignty_restoration, conceptual, 'Feasibility and impact of epistemic sovereignty restoration').

omega_variable(
    identity_lock_permanence,
    'Is the identity-lock mechanism (indigenous identity constituted through ancestral lands) permanent and immutable, or can collective identity reframe to support alternative territorial relationships?',
    'Historical analysis of indigenous communities maintaining identity across forced displacement; ethnographic study of identity reframing in diaspora communities; assessment of which elements of indigenous identity are land-dependent vs culturally-sustained',
    'If identity-lock is permanent: escape from constraint requires cultural death (unacceptable cost). If identity is malleable: constraint becomes constrained rather than trapped, enabling different exit pathways.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_permanence, conceptual, 'Permanence and mutability of land-based indigenous identity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indigenous_resource_rights, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irr_tr_t0, indigenous_resource_rights, theater_ratio, 0, 0.45).
narrative_ontology:measurement(irr_tr_t40, indigenous_resource_rights, theater_ratio, 40, 0.58).
narrative_ontology:measurement(irr_tr_t80, indigenous_resource_rights, theater_ratio, 80, 0.68).

% Extraction over time
narrative_ontology:measurement(irr_be_t0, indigenous_resource_rights, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(irr_be_t40, indigenous_resource_rights, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(irr_be_t80, indigenous_resource_rights, base_extractiveness, 80, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indigenous_resource_rights, resource_allocation).
narrative_ontology:boltzmann_floor_override(indigenous_resource_rights, 0.25).
narrative_ontology:affects_constraint(indigenous_resource_rights, colonial_land_tenure_systems).
narrative_ontology:affects_constraint(indigenous_resource_rights, global_commodity_markets).
narrative_ontology:affects_constraint(indigenous_resource_rights, state_sovereignty_doctrine).

% DUAL FORMULATION NOTE:
% Indigenous resource rights constraint is downstream of and structurally coupled with colonial land tenure systems (which establish the legal framework denying indigenous claim recognition) and global commodity markets (which create extraction incentive structures). The constraint family includes separate stories for legal framework suppression, market extraction mechanisms, and epistemic sovereignty denial, each with distinct ε values. This story represents the integrated extraction mechanism across all three domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
