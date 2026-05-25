% ============================================================================
% CONSTRAINT STORY: israeli_palestinian_power_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israeli_palestinian_power_asymmetry, []).

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
 *   constraint_id: israeli_palestinian_power_asymmetry
 *   human_readable: Israeli-Palestinian Power Asymmetry and Structural Extraction
 *   domain: geopolitical/political_economy
 *
 * SUMMARY:
 *   The Israeli-Palestinian power asymmetry represents a structural
 *   constraint characterized by extreme military, economic, and institutional
 *   disparity operating within a framework of occupation and territorial
 *   control. The constraint exhibits extraction across multiple dimensions:
 *   land appropriation through settlements, resource control (water aquifers,
 *   electromagnetic spectrum), legal asymmetry (military law applied to
 *   Palestinian population vs. civil law for Israeli settlers), movement
 *   restrictions through checkpoint systems, and political domination through
 *   institutional structures inherited from the Oslo framework that embed
 *   Palestinian dependence on Israeli coordination. The constraint's
 *   classification as Snare derives from the combination of maximal
 *   suppression (0.75) through structural enforcement mechanisms, high
 *   extractiveness (0.68) concentrated on the Palestinian population with
 *   asymmetric benefit to Israeli state apparatus and settler economy, and
 *   minimal genuine coordination function — the framing of the constraint as
 *   security coordination masks extraction that persists through coercive
 *   enforcement rather than consent. The theater ratio (0.58) reflects
 *   performative aspects of the peace process (decades of negotiation without
 *   structural resolution) that provide legitimacy theater while extraction
 *   continues. Mandatrophy resolution at this extractiveness level requires
 *   analysis of whether coordination functions exist that would justify
 *   Tangled Rope classification; the analysis concludes that while security
 *   coordination narratives exist, the extraction magnitude and asymmetry
 *   exceed what coordination requirements justify, confirming Snare
 *   classification.
 *
 * KEY AGENTS:
 *   - Palestinian Population: Primary victims (powerless/trapped) — face material confinement, legal asymmetry, economic dependency through permit and checkpoint systems
 *   - Palestinian Political Elite/PA: Victims with identity_locked characteristics (powerless-to-moderate/identity_locked) — structurally mobile but identity-fused with roles within institutional framework created by occupation
 *   - Palestinian Civil Society: Secondary victims (moderate/constrained) — operate within constraint structure providing humanitarian/governance functions while facing resource control extraction
 *   - Israeli State Apparatus: Primary beneficiary (institutional/arbitrage) — controls security, resources, settlement expansion; benefits from coordination framing while extracting asymmetrically
 *   - Israeli Settler Economy: Beneficiary (powerful/arbitrage) — directly benefits from land appropriation and resource access asymmetries
 *   - International Mediation Infrastructure: Organized agents attempting coordination (organized/constrained) — two-state framework represents nominal sunset but increasingly implausible given demographic/territorial facts
 *   - International Legal Regime: Institutional actor (institutional/arbitrage) — provides legitimacy and theater while enforcement mechanisms remain weak
 *   - Analytical Observer: Civilian perspective (analytical/analytical) — sees extraction clearly from civilizational timescale when theater is penetrated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israeli_palestinian_power_asymmetry, 0.68).
domain_priors:suppression_score(israeli_palestinian_power_asymmetry, 0.75).
domain_priors:theater_ratio(israeli_palestinian_power_asymmetry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israeli_palestinian_power_asymmetry, extractiveness, 0.68).
narrative_ontology:constraint_metric(israeli_palestinian_power_asymmetry, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(israeli_palestinian_power_asymmetry, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israeli_palestinian_power_asymmetry, snare).
narrative_ontology:human_readable(israeli_palestinian_power_asymmetry, "Israeli-Palestinian Power Asymmetry and Structural Extraction").
narrative_ontology:topic_domain(israeli_palestinian_power_asymmetry, "geopolitical/political_economy").

domain_priors:requires_active_enforcement(israeli_palestinian_power_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israeli_palestinian_power_asymmetry, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(israeli_palestinian_power_asymmetry, israeli_settler_economy).
narrative_ontology:constraint_victim(israeli_palestinian_power_asymmetry, palestinian_population).
narrative_ontology:constraint_victim(israeli_palestinian_power_asymmetry, palestinian_governance_capacity).
narrative_ontology:constraint_victim(israeli_palestinian_power_asymmetry, regional_peace_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN POPULATION (SNARE) — Faces material confinement (checkpoint systems, movement restrictions, geographic fragmentation), legal asymmetry (military law vs civilian law), and economic dependency (permit systems, resource access control). Exit costs are maximal; alternatives suppressed through enforcement. Experiences the constraint as pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(israeli_palestinian_power_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PALESTINIAN POLITICAL ELITE / GOVERNANCE STRUCTURE (SNARE with IDENTITY_LOCKED) — Structurally mobile (could negotiate exit, form alternative institutions) but identity-locked through institutional capture: PA governance structure inherited from Oslo Accords creates dependency on Israeli coordination and international funding routed through mechanisms that perpetuate the asymmetry. Elite identity is fused with the role within this constrained system. Perceived as unchangeable from within despite theoretical structural mobility.
constraint_indexing:constraint_classification(israeli_palestinian_power_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: PALESTINIAN CIVIL SOCIETY (TANGLED ROPE) — Constrained by permit requirements and resource access but also gains functional coordination through NGO networks, health services, education systems that operate within the constraint structure. Faces high extraction (resource control, movement restrictions) but also genuine coordination function (humanitarian assistance, local governance, service provision). Exit cost is substantial but not absolute — civil society operates with agency within constraints.
constraint_indexing:constraint_classification(israeli_palestinian_power_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ISRAELI STATE APPARATUS (ROPE) — Experiences the constraint as coordination mechanism: security protocols, settlement expansion, resource allocation, diplomatic positioning all function as integrated system. Benefits from prioritized access to resources, security asymmetry, and international legitimacy through democratic institutions. Perceives constraint as solving legitimate coordination problem (security), not as extraction. Arbitrage-capable: can exit to alternative security arrangements but benefits from current system.
constraint_indexing:constraint_classification(israeli_palestinian_power_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL PEACE/MEDIATION INFRASTRUCTURE (SCAFFOLD) — Two-state solution framework, international law precedents, UN resolutions create temporary coordination structure with nominal sunset (negotiated resolution). Theater ratio reflects performative aspects of peace process (endless rounds of talks without structural change). Organized actors (UN, international mediators, third-party states) see the constraint as addressable through institutional redesign, but exit path requires restructuring power asymmetry that benefits state apparatus. High suppression of alternative frameworks (BDS, annexation models, unilateral declarations).
constraint_indexing:constraint_classification(israeli_palestinian_power_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL REGIME (PITON) — International humanitarian law, occupation doctrine, settlement legality principles exist as institutional framework but function increasingly performatively: enforcement mechanisms are weak, selective application creates legitimacy gaps, and the regime persists through institutional inertia rather than effective constraint on extractive behavior. Theater ratio high due to performative aspects (ICJ opinions without enforcement, GA resolutions without compliance mechanisms).
constraint_indexing:constraint_classification(israeli_palestinian_power_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational timescale and global scope, the constraint exhibits all characteristics of pure extraction: asymmetric military/economic power, suppression of exit alternatives, extraction of resources (land, water, tax revenue), and minimal coordination function relative to extraction magnitude. Theater (peace process performances) masks extraction. Extraction persists through enforcement rather than consent.
constraint_indexing:constraint_classification(israeli_palestinian_power_asymmetry, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israeli_palestinian_power_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israeli_palestinian_power_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israeli_palestinian_power_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(israeli_palestinian_power_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(israeli_palestinian_power_asymmetry, TR),
    TR >= 0.70.

:- end_tests(israeli_palestinian_power_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Extraction occurs across multiple structural dimensions: land (settlement expansion averaging 4-6% annually in West Bank), water resources (Israeli control of 80% of shared aquifers), tax revenue (Israeli military administration collects Palestinian taxes), and political power (Palestinian governance authority constrained by Israeli security veto). The 0.68 value reflects that while extraction is severe, some coordination functions exist (PA health/education provision within constraints) and not all Palestinian activity is extractive — some coordination value is created despite the asymmetry. Suppression (0.75): High. Structural enforcement mechanisms include military presence, permit systems controlling 98% of Palestinian movement across Area C, checkpoint infrastructure, settlement-induced territorial fragmentation, legal asymmetry (Palestinian civilians under military law with limited due process), and suppression of alternative political frameworks (delegitimization of BDS, restriction of Palestinian sovereignty claims). Exit options are severely constrained for the entire Palestinian population — economic emigration is restricted, political alternatives are suppressed through legal/security mechanisms, and geographic alternatives are blocked by closure policies. Theater ratio (0.58): Moderate-high. The constraint generates significant performative activity: peace process negotiations (Oslo framework, Annapolis, bilateral talks) that continue despite structural entrenchment, international statements of support for Palestinian statehood that carry no enforcement mechanisms, and humanitarian rhetoric accompanying extraction mechanisms (framing settlement expansion as security necessity, portraying movement restrictions as counterterrorism). The theater increased over the interval as the extractive reality diverged from peace process rhetoric.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The Israeli state apparatus perceives Rope (coordination mechanism solving security problem); Palestinian population perceives Snare (pure extraction with suppression). The international legal regime perceives Piton (theater-dominated degraded framework); the international mediation infrastructure perceives Scaffold (nominal sunset through two-state solution, though demographic facts increasingly render this implausible). Palestinian civil society perceives Tangled Rope (both coordination function through service provision and extraction through resource control). This perspectival range from Rope to Snare across different agents with the same structural constraint reflects the extreme power asymmetry — the beneficiary's experience of coordination precisely mirrors the victim's experience of extraction because the 'coordination' being coordinated is the extraction itself. The false-summit risk appears when the constraint is framed as a natural law of security necessity (Mountain perspective) — the analytical observer at civilizational scope sees through this to recognize contingent institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows the structural relationships: beneficiary agents (Israeli state, settler economy) have arbitrage-level exit capacity and institutional/powerful power levels, deriving low-to-negative d values (0.05-0.20) that produce negative or low χ, reflecting their benefit from the constraint. Victim agents (Palestinian population) have trapped or identity_locked exit options and powerless power levels, deriving high d values (0.89-0.95) that produce high f(d) (1.28-1.42), multiplying base extractiveness into maximal experienced χ. The identity_locked perspective for Palestinian elite reflects that while they are structurally capable of institutional redesign (could theoretically negotiate differently or form alternative governance structures), their identity as political actors is constituted through the roles available within the Oslo framework and its successors — exit would require abandoning not just positions but professional/political identity. This distinguishes them from trapped agents (who face material barriers) while acknowledging they experience the constraint as unchangeable from within their identity frame.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution at extractiveness 0.68 requires examining whether coordination functions justify the Tangled Rope classification or whether extraction dominates sufficiently for Snare classification. ANALYSIS: The constraint exhibits genuine coordination functions at the level of Palestinian governance (PA provides health/education services, civil administration) and Israeli security (constraint coordinates enforcement through checkpoint/settlement architecture). However, the asymmetry and extraction magnitude exceed what these coordination requirements justify: (1) Palestinian civil services could be provided through alternative institutional arrangements without the settlement expansion and resource control asymmetry; (2) Israeli security coordination could function with alternative architectures (non-territorial security measures, proportionate movement restrictions) that don't require resource extraction; (3) the constraint's enforced asymmetry produces extraction that IS THE CONSTRAINT's primary function, not a secondary effect. The Snare classification is confirmed because the coordination framing serves to legitimize extraction that could not justify itself on coordination merits alone. The performance of coordination (peace process theater, humanitarian gestures) masks that extraction is the primary function and coordination is secondary theater. This resolves the mandatrophy: Snare is analytically correct despite the existence of coordination narratives because those narratives function to obscure extraction, not to constitute genuine mutual coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'What proportion of the constraint''s structural features serve genuine security coordination versus asymmetric extraction of resources and political power?',
    'Comparative analysis: military doctrine requirements vs. settlement expansion patterns; security checkpoint functions vs. movement restriction impacts; resource allocation mechanisms vs. demographic outcomes',
    'If coordination dominates (>60%): reclassify from Snare toward Tangled Rope. If extraction dominates (>70%): confirms Snare with minimal coordination function. This determines whether the constraint is primarily a security mechanism or primarily an extraction mechanism using security framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Proportion of structural features serving coordination versus extraction').

omega_variable(
    identity_lock_depth_palestinian_elite,
    'Is the Palestinian political elite''s entrenchment in the current system primarily due to external structural barriers (trapped) or to identity fusion with roles within the system (identity_locked)?',
    'Analysis of PA elite decision-making: would institutional redesign alone remove behavioral constraints? Do elite actors demonstrate cognitive/identity barriers to exit even when structural barriers are modeled as removable? Post-transition analysis if Palestinian governance restructures.',
    'If identity_locked (cognitive binding): elite perception shift required for structural change — reform programs targeting external barriers miss the internal binding mechanism. If trapped (material barriers): removing barriers creates different outcome trajectories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_depth_palestinian_elite, empirical, 'Whether Palestinian elite entrenchment is structural or identity-based').

omega_variable(
    suppression_mechanism_internalization,
    'How much of the measured suppression (0.75) reflects structural barriers (checkpoints, legal asymmetry, permit systems) versus internalized behavioral suppression (risk aversion, learned helplessness, normalized constraints)?',
    'Longitudinal studies of movement patterns post-barrier removal; comparative analysis of Palestinian behavior in areas with vs. without formal restrictions; post-resolution outcome tracking to separate structural from internalized suppression',
    'If structural (>60%): barrier removal produces immediate behavior change. If internalized (>50%): suppression persists after barrier removal, requiring additional interventions targeting learned patterns.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Proportion of suppression that is structural versus internalized').

omega_variable(
    international_legal_regime_enforcement_gap,
    'Is the international legal regime functioning as a genuine constraint on extraction (Rope/Tangled Rope) or as performative cover (Piton) for extraction that continues despite legal prohibitions?',
    'Tracking enforcement outcomes: ICJ rulings vs. actual compliance; GA resolutions vs. policy change; treaty obligations vs. settlement expansion rates; selective application to Israeli vs. Palestinian actors',
    'If Rope function: legal regime creates actual constraints, reclassify. If Piton (performative): legal regime legitimates extraction while providing theater of constraint, confirms current classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_legal_regime_enforcement_gap, empirical, 'Whether international legal regime constrains or legitimates extraction').

omega_variable(
    two_state_sunset_viability,
    'Is the two-state solution framework a genuine sunset clause (Scaffold perspective realistic) or an aspirational narrative that has lost structural plausibility?',
    'Demographic analysis: settler population growth vs. Palestinian autonomy capacity; territorial continuity feasibility given settlement distribution; resource control mechanisms (water, airspace) separability',
    'If sunset viable: Scaffold perspective is analytically sound, constraint could be temporary. If sunset implausible: two-state framing is theater, reclassify international actors from Scaffold toward Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(two_state_sunset_viability, empirical, 'Whether two-state solution represents viable sunset or aspirational theater').

omega_variable(
    military_domination_coalitional_resistance,
    'Can Palestinian organizing overcome military asymmetry through coalitional power dynamics, or does the asymmetry prevent organized resistance from reaching critical mass?',
    'Historical analysis of resistance movements: BDS, intifadas, civil noncooperation; modeling of tipping points; comparison to other asymmetric occupations where resistance achieved structural gains',
    'If coalitional power possible: powerless agents could transition to organized (dynamic coalition extension), changing classification for some perspectives from Snare toward contested Tangled Rope. If military asymmetry precludes coalitional capacity: Snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_domination_coalitional_resistance, empirical, 'Whether military asymmetry precludes organized Palestinian resistance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israeli_palestinian_power_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isra_tr_t0, israeli_palestinian_power_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(isra_tr_t10, israeli_palestinian_power_asymmetry, theater_ratio, 10, 0.5).
narrative_ontology:measurement(isra_tr_t20, israeli_palestinian_power_asymmetry, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(isra_be_t0, israeli_palestinian_power_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(isra_be_t10, israeli_palestinian_power_asymmetry, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(isra_be_t20, israeli_palestinian_power_asymmetry, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israeli_palestinian_power_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(israeli_palestinian_power_asymmetry, palestinian_governance_capacity).
narrative_ontology:affects_constraint(israeli_palestinian_power_asymmetry, israeli_settler_expansion).
narrative_ontology:affects_constraint(israeli_palestinian_power_asymmetry, regional_security_architecture).
narrative_ontology:affects_constraint(israeli_palestinian_power_asymmetry, water_resource_asymmetry).
narrative_ontology:affects_constraint(israeli_palestinian_power_asymmetry, legal_asymmetry_occupation_law).

% DUAL FORMULATION NOTE:
% The Israeli-Palestinian power asymmetry is an overarching constraint that structures multiple downstream constraints including Palestinian governance capacity (constrained by institutional dependence), Israeli settler expansion (enabled by resource control), regional security architecture (justifying the asymmetry), water resource allocation (enforcing scarcity), and legal asymmetry (operationalizing the power differential). This story models the meta-constraint; downstream stories model specific extraction mechanisms. Each downstream constraint has its own ε and perspectives; this story establishes the structural foundation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(israeli_palestinian_power_asymmetry, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
