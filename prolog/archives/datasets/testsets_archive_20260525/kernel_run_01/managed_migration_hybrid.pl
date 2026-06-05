% ============================================================================
% CONSTRAINT STORY: managed_migration_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_managed_migration_hybrid, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: managed_migration_hybrid
 *   human_readable: Managed Migration Hybrid: Border Control with Graduated Rights and Multilateral Cooperation
 *   domain: international_law/political_philosophy/migration_studies
 *
 * SUMMARY:
 *   The managed migration hybrid represents a specific reading of how states
 *   should exercise border authority: retaining sovereign control but
 *   channeling it through graduated rights frameworks (temporary workers,
 *   refugees, family reunification) and multilateral cooperation mechanisms
 *   (UNHCR, IOM, bilateral/regional treaties). This constraint is one of
 *   three contested readings of the border normative status kernel. The
 *   sovereignty_primary reading asserts that states retain absolute authority
 *   and should privilege national interest; the freedom_of_movement_primary
 *   reading asserts that human mobility rights should override borders; the
 *   managed_migration_hybrid (this reading) attempts to balance both through
 *   institutional arrangements. The structural tension is that the managed
 *   framework benefits receiving-state institutions and multilateral
 *   coordination apparatus (which gain legitimacy and authority through
 *   managing flows), extracts from excluded migrants (who fall outside treaty
 *   categories and experience maximum suppression), and produces mixed
 *   outcomes for qualified treaty migrants, displaced citizens, and
 *   origin-state actors. The constraint's extractiveness (0.52) and theater
 *   ratio (0.61) show increasing institutionalization over the measurement
 *   interval: the framework becomes more elaborate and more performative as
 *   treaties accumulate and categories proliferate without shrinking the
 *   excluded population.
 *
 * KEY AGENTS:
 *   - Excluded Migrants: Primary victim (powerless/trapped) — fall outside established treaty categories; no legal status, institutional voice, or recourse; bear full cost of border control enforcement
 *   - Displaced Citizens: Secondary victim (powerless/trapped, diffuse) — indigenous or resident population economically/socially displaced by uncontrolled flows; externalized from treaty negotiations; political voice depends on electoral salience
 *   - Qualified Treaty Migrants: Mixed experience (moderate/constrained) — benefit from legal status and institutional recognition within treaty categories; experience extraction through work restrictions, time limits, sponsorship dependency
 *   - Receiving State Institutions: Primary beneficiary (institutional/arbitrage) — gain legitimacy, labor supply control, and institutional capacity through managing flows; experience constraint as coordination mechanism enabling multiple competing goods
 *   - Multilateral Coordination Apparatus: Secondary beneficiary (institutional/arbitrage) — UNHCR, IOM, bilateral treaties capture authority and legitimacy through orchestrating graduated rights; genuine coordination function but derives institutional power from the constraint
 *   - Migrant-Origin Coalition: Mixed (organized/constrained) — origin-state governments and diaspora groups gain institutional voice through labor agreements and family reunification pathways; experience extraction through remittance dependency and labor-subsidy dynamics
 *   - Legacy Refugee Convention Apparatus: Degraded institutional actor (institutional/arbitrage) — 1951 Refugee Convention framework becomes increasingly performative as new categories proliferate and original design becomes outdated; maintains legitimacy through compliance theater
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the hybrid as an immutable constraint on political order rather than recognizing it as a contingent institutional bargain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(managed_migration_hybrid, 0.52).
domain_priors:suppression_score(managed_migration_hybrid, 0.58).
domain_priors:theater_ratio(managed_migration_hybrid, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(managed_migration_hybrid, extractiveness, 0.52).
narrative_ontology:constraint_metric(managed_migration_hybrid, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(managed_migration_hybrid, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(managed_migration_hybrid, tangled_rope).
narrative_ontology:human_readable(managed_migration_hybrid, "Managed Migration Hybrid: Border Control with Graduated Rights and Multilateral Cooperation").
narrative_ontology:topic_domain(managed_migration_hybrid, "international_law/political_philosophy/migration_studies").

domain_priors:requires_active_enforcement(managed_migration_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(managed_migration_hybrid, formalized).
narrative_ontology:cs_authority_grounding(managed_migration_hybrid, lineage).
narrative_ontology:cs_interpretation_layer_present(managed_migration_hybrid).
narrative_ontology:cs_kernel_id(managed_migration_hybrid, border_normative_status).
narrative_ontology:cs_reading_relation(managed_migration_hybrid, sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation(managed_migration_hybrid, freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_axiom(managed_migration_hybrid, foundational, graduated_rights_institutional_mediation_legitimacy).
narrative_ontology:cs_axiom_status(graduated_rights_institutional_mediation_legitimacy, holdable).
narrative_ontology:cs_axiom(managed_migration_hybrid, foundational, border_enforcement_compatible_with_mobility_recognition).
narrative_ontology:cs_axiom_status(border_enforcement_compatible_with_mobility_recognition, holdable).
narrative_ontology:cs_reference_frame(managed_migration_hybrid, post_1951_refugee_convention_managed_cooperation).
narrative_ontology:cs_drift_state(managed_migration_hybrid, contemporary_climate_displacement_era, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(managed_migration_hybrid, receiving_state_institutions).
narrative_ontology:constraint_beneficiary(managed_migration_hybrid, multilateral_coordination_capacity).
narrative_ontology:constraint_victim(managed_migration_hybrid, excluded_migrants).
narrative_ontology:constraint_victim(managed_migration_hybrid, displaced_citizens).
narrative_ontology:constraint_victim(managed_migration_hybrid, treaty_category_friction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MIGRANT (SNARE) — Falls outside treaty categories (temporary worker, refugee, family reunification). No exit option. Bears full suppression cost: legal barriers, absence from negotiated agreements, zero institutional voice. Maximum experienced extraction — institutional capacity gains benefit; migrant bears cost with no compensating coordination function.
constraint_indexing:constraint_classification(managed_migration_hybrid, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISPLACED CITIZEN / UNMANAGED FLOW (SNARE) — Indigenous or resident population displaced by uncontrolled migration flows; lacks political voice to renegotiate treaty categories that exclude their interests. Experiences the constraint as pure extraction: their economic vulnerability and political marginality are externalized to manage receiving-state institutional capacity. No coordination benefit; high suppression.
constraint_indexing:constraint_classification(managed_migration_hybrid, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: QUALIFIED TREATY MIGRANT (TANGLED ROPE) — Fits into established category (temporary worker, refugee, family reunification). Experiences both coordination and extraction: treaty framework provides legal status and mobility (coordination benefit) but within constrained parameters (work restrictions, time limits, sponsorship dependency). Moderate power. Constrained exit — can appeal or seek reclassification, but at significant cost.
constraint_indexing:constraint_classification(managed_migration_hybrid, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: RECEIVING STATE INSTITUTIONS (ROPE) — Benefits from managed framework: controlled labor supply, vetted refugee intake, family reunification processed through institutional capacity. Experiences the constraint as primarily coordinating: manages competing claims (capital needs labor, citizens demand stability, international treaties require participation). Net beneficiary with real coordination function — institutional capacity is the enabling mechanism.
constraint_indexing:constraint_classification(managed_migration_hybrid, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: MULTILATERAL COORDINATION APPARATUS (ROPE) — Benefits from managed framework: treaties, international law, multilateral institutions (UNHCR, IOM, regional agreements) capture legitimacy and authority through orchestrating graduated rights. Experiences constraint as coordination mechanism — genuinely solves collective action problem of managing cross-border flows without pure sovereigntist collapse or open-borders paralysis. Net beneficiary; sees the constraint as functional.
constraint_indexing:constraint_classification(managed_migration_hybrid, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MIGRANT-ORIGIN COALITION (TANGLED ROPE) — Origin-state governments and diaspora groups gain institutional voice through managed framework (family reunification categories, labor agreements, diaspora remittance corridors) but experience extraction: labor-exporting states subsidize destination-state economies, remittance dependency creates exit barriers for migrants. Mixed coordination and extraction. Organized power but constrained by asymmetric bargaining.
constraint_indexing:constraint_classification(managed_migration_hybrid, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: LEGACY REFUGEE CONVENTION APPARATUS (PITON) — 1951 Refugee Convention designed for mid-20th-century displacement; now substantially performative. Theater_ratio high: institutional compliance (processing, status determination) maintains legitimacy while effectiveness declines (climate refugees, political asylum from allied states, internal displacement). Net beneficiary (institutions expand in response to conventions) but with degraded function. Maintained through inertia and cost of renegotiation.
constraint_indexing:constraint_classification(managed_migration_hybrid, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the tension between sovereignty and human mobility is a structural invariant of political order. No framework can fully resolve it — state capacity requires borders; human dignity requires mobility. This perspective naturalizes the hybrid as an immutable constraint on political possibility. However, the structural data (beneficiaries, enforcement mechanisms, treaty malleability) contradicts the mountain classification — the engine will identify this as a false summit, revealing naturalization of what is actually a contingent institutional bargain.
constraint_indexing:constraint_classification(managed_migration_hybrid, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(managed_migration_hybrid_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(managed_migration_hybrid, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(managed_migration_hybrid, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(managed_migration_hybrid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(managed_migration_hybrid, TR),
    TR >= 0.70.

:- end_tests(managed_migration_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting mixed coordination and extraction. The framework provides genuine coordination for qualified treaty migrants and receiving-state institutions (coordination function reduces pure extractiveness). However, excluded migrants and displaced citizens experience substantial extraction with minimal coordination benefit. The intermediate value reflects the weighted average across agent positions: some agents experience rope-level coordination (institutional beneficiaries), others experience snare-level extraction (excluded migrants). Measurement trajectory shows extractiveness rising from 0.38 to 0.52 over 20 years, indicating that despite narrative of expanding opportunity, the framework has become increasingly extractive through bureaucratic ossification and category proliferation without corresponding expansion of inclusion. Suppression (0.58): Moderate-high. Excluded migrants face legal barriers (non-status), administrative barriers (processing delays, burden of proof), and absence from negotiating power. Qualified migrants face work restrictions, time limits, sponsorship dependency. Displaced citizens face political marginalization. Suppression is structural but not absolute — some escape routes exist (appeals, reclassification, undocumented transition, return migration). Theater ratio (0.61): Moderate-high and increasing. The legacy Refugee Convention and emerging frameworks (family reunification, temporary worker visas) show increasing performative content. Border processing, status determination, compliance monitoring become increasingly elaborate (theater rising from 0.48 to 0.61) while actual protection/opportunity scope remains static or shrinks relative to demand. Theater rise indicates that the framework increasingly functions through institutional legitimacy (states appear to manage flows responsibly) rather than through expanding actual opportunity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates five distinct classifications from the same structural data. Excluded migrants (powerless/trapped) classify as Snare: pure extraction with maximum experienced suppression and no coordination benefit. Qualified treaty migrants (moderate/constrained) classify as Tangled Rope: both coordination (treaty framework, legal status) and extraction (restrictions, dependency) present. Receiving state institutions (institutional/arbitrage) classify as Rope: primary function is coordination (managing competing claims), beneficiary status enables arbitrage (labor supply, selective intake). Multilateral apparatus (institutional/arbitrage) also classifies as Rope: genuine coordination function enabled by arbitrage status. Migrant-origin coalition (organized/constrained) classifies as Tangled Rope: institutional voice (coordination benefit) mixed with labor subordination (extraction). Legacy Refugee Convention (institutional/arbitrage) classifies as Piton: performed legitimacy (theater_ratio 0.61) masks degraded function. Analytical observer (analytical/analytical) risks classifying as Mountain: civilizational-level tension between sovereignty and mobility treated as immutable structural invariant. The perspectival gap reveals that the constraint is not a single objective phenomenon but a bundle of different structural relationships: coordination for some agents, extraction for others, degradation for institutions, and potential false naturalization from the analytical view.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective derive from the agent's structural position relative to the extraction flow. Excluded migrants (powerless/trapped) occupy d ≈ 0.95 — full target status, maximum experienced extraction through f(d) ≈ 1.42. Receiving state institutions (institutional/arbitrage) occupy d ≈ 0.05 — full beneficiary status, derive net positive benefit, experience negative or near-zero χ. Qualified treaty migrants (moderate/constrained) occupy d ≈ 0.65 — mixed position, experience both coordination benefits (treaty framework) and extraction costs (restrictions); f(d) ≈ 1.00 produces intermediate χ. Multilateral apparatus (institutional/arbitrage) occupies d ≈ 0.10 — beneficiary, derives institutional power from coordination function. Migrant-origin coalition (organized/constrained) occupies d ≈ 0.55 — ambiguous middle position: gains diplomatic voice (beneficiary) but labor subordination (victim); net d reflects this balance. Displaced citizens' d is difficult to compute without explicit political power measurement; conservatively positioned at d ≈ 0.90 (victim + trapped, but diffuse rather than organized, slightly higher mobility than excluded migrants). The perspectival gap is pronounced: beneficiaries see coordination (rope/arbitrage chi near zero), victims see extraction (snare chi > 0.66), moderate agents see mixed experience (tangled_rope chi ≈ 0.40-0.50).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING PERSPECTIVE: This constraint's mandatrophy is resolved by recognizing it as ONE READING of a contested boundary (border normative status kernel). The constraint is neither pure coordination (Rope) nor pure extraction (Snare) — it is Tangled Rope precisely because the managed migration framework claims to do both: respect state sovereignty AND support human mobility. The mandatrophy dissolves when we ask: whose coordination and whose extraction? For receiving states and multilateral institutions, it coordinates multiple goods (labor supply, humanitarian compliance, institutional legitimacy). For excluded migrants and displaced citizens, it extracts through asymmetric boundary-setting. The framework itself (treaties, multilateral institutions, graduated rights categories) functions to manage this tension rather than resolve it. The tension is preserved in the institutional design — excluded migrants must exist to give meaning to the boundary, and the boundary must remain somewhat arbitrary to accommodate political renegotiation without fundamental framework collapse. The constraint's mandatrophy is not resolable as 'which type is correct?' but as 'this reading preserves the tension between sovereignty and mobility as a managed institutional practice rather than resolving it toward either pure sovereignty or pure movement freedom.' The Tangled Rope classification is structurally accurate — genuine coordination exists (for qualified migrants, for institutional capacity) alongside genuine extraction (for excluded, for displaced).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_category_plasticity,
    'Are the graduated rights categories (temporary worker, refugee, family reunification) fixed structural categories or fluid negotiable boundaries that can expand to include currently-excluded migrants?',
    'Historical analysis of category expansion: post-WWII refugee redefinition (1967 Protocol), EU free movement expansion, recent humanitarian corridors. Identification of factors driving renegotiation (political pressure, humanitarian crises, labor market demand). Counterfactual: would adding new categories change the constraint from tangled_rope to rope?',
    'If categories are plastic: the constraint is a contingent institutional arrangement subject to renegotiation. If categories are functionally fixed: the constraint approaches snare (excluded migrants trapped in permanent non-category). Classification could shift from tangled_rope to rope (if expansion accelerates) or snare (if categories ossify).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_category_plasticity, empirical, 'Whether graduated rights categories are fixed or renegotiable').

omega_variable(
    multilateral_enforcement_capacity,
    'Does the multilateral coordination apparatus (UNHCR, IOM, bilateral agreements) actually enforce compliance with graduated rights, or does it mainly provide institutional theater that masks unilateral state behavior?',
    'Audit of enforcement mechanisms: binding vs advisory status of treaties, state non-compliance rates, consequences for violating graduated rights protocols, comparison of nominal treaty commitments to actual intake/processing of each category. Case analysis: examples where multilateral apparatus blocked or forced state action vs examples where state acted unilaterally despite treaty.',
    'If enforcement is real: multilateral apparatus is genuine coordination mechanism (rope perspective validated). If mainly theater: the coordination function is decorative, and the constraint is closer to snare (states extract from all groups using treaties as legitimacy cover). Could reclassify from tangled_rope toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(multilateral_enforcement_capacity, empirical, 'Whether multilateral apparatus enforces graduated rights or provides institutional theater').

omega_variable(
    excluded_migrant_structural_permanence,
    'Is the category of ''excluded migrants'' (those falling outside treaty categories) a permanent structural feature or a temporary residual that shrinks as categories expand and implementation matures?',
    'Time-series analysis: proportion of cross-border migration falling within formal treaty categories across decades; trend analysis of whether category coverage expands or stagnates; identification of migrants in ''gray zones'' (undocumented, quasi-legal, pending reclassification). Structural analysis: what creates the excluded category? (Policy design, administrative capacity, political choice, or inherent limitations?)',
    'If exclusion is permanent feature: snare classification for excluded migrants is stable; constraint is systematically asymmetric. If exclusion shrinks over time: constraint exhibits positive drift toward rope; fewer agents experience maximum extraction. Classification trajectory could show tangled_rope shifting toward rope (if institutional capacity expands) or toward snare (if political backlash hardens exclusion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_migrant_structural_permanence, empirical, 'Whether excluded migrant category is structural permanence or temporary residual').

omega_variable(
    citizen_displacement_visibility,
    'Is the ''displaced citizen'' victim category visible and politically organized, or is it a diffuse externality that bears suppression without coherent advocacy?',
    'Political analysis: documentation of displaced citizen grievance articulation, electoral salience, legislative response, data quality of displacement measurement. Comparison of displaced citizen political voice (organized coalitions, voting power, media representation) vs excluded migrant voice. Structural question: do displaced citizens have institutional channels to renegotiate treaty categories, or are their interests externalized as background conditions?',
    'If organized with voice: displaced citizens are moderate/constrained agents (tangled_rope or constrained rope). If invisible/diffuse: they are powerless/trapped agents (snare). This affects whether displacement is recognized in negotiation (could shift constraint toward rope or scaffold) or remains latent (constraint remains tangled_rope or snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_displacement_visibility, empirical, 'Whether displaced citizens are organized with political voice or diffuse without advocacy').

omega_variable(
    reading_boundary_ambiguity,
    'This reading occupies a middle position between sovereignty_primary (state border authority) and freedom_of_movement_primary (human mobility rights). Where does the boundary between ''respecting state sovereignty'' and ''extractive border control'' actually lie? Can multilateral frameworks accommodate both readings, or does the managed hybrid foreclose one or the other?',
    'Comparative legal analysis: do states operating under managed migration frameworks report that treaties respect their sovereignty, or do they experience treaties as constraint on sovereign authority? Parallel analysis from migrant/origin-state perspective: do they perceive frameworks as respecting human mobility rights, or as extraction mechanism? Case studies of framework renegotiation or exit (Brexit migration clauses, US asylum policy changes) revealing which reading dominates.',
    'If managed hybrid genuinely accommodates both readings: it coexists with both siblings. If one reading dominates practice: the hybrid forecloses or degrades the other. Could reveal that the reading is actually a disguised sovereignty_primary (extraction using institutional language) or disguised freedom_of_movement_primary (constrained by political reality). This directly determines whether reading_relations should be forecloses vs coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether managed hybrid boundary between sovereignty and mobility rights is stable or disguises one reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(managed_migration_hybrid, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(migr_tr_t0, managed_migration_hybrid, theater_ratio, 0, 0.48).
narrative_ontology:measurement(migr_tr_t10, managed_migration_hybrid, theater_ratio, 10, 0.58).
narrative_ontology:measurement(migr_tr_t20, managed_migration_hybrid, theater_ratio, 20, 0.61).

% Extraction over time
narrative_ontology:measurement(migr_be_t0, managed_migration_hybrid, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(migr_be_t10, managed_migration_hybrid, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(migr_be_t20, managed_migration_hybrid, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(managed_migration_hybrid, enforcement_mechanism).
narrative_ontology:affects_constraint(managed_migration_hybrid, international_refugee_law_legitimacy).
narrative_ontology:affects_constraint(managed_migration_hybrid, labor_arbitrage_asymmetric_terms).
narrative_ontology:affects_constraint(managed_migration_hybrid, citizenship_boundary_permeability).

% DUAL FORMULATION NOTE:
% The managed migration hybrid is one reading within a constraint family decomposed across multiple observables: this story models the balance/tension reading; upstream constraints (sovereignty_primary reading, freedom_of_movement_primary reading) model alternative framings of the same border phenomenon. Each reading gets its own constraint_id and its own ε value reflecting the empirical status of that reading's core premises. This file (managed_migration_hybrid) ε=0.52 reflects the actual structure of multilateral managed migration frameworks; sibling files model alternative framings with their own ε values and classification implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(managed_migration_hybrid, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
