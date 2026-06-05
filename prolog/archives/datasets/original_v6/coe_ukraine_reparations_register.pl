% ============================================================================
% CONSTRAINT STORY: coe_ukraine_reparations_register
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coe_ukraine_reparations_register, []).

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
 *   constraint_id: coe_ukraine_reparations_register
 *   human_readable: Council of Europe's Register of Damage for Ukraine
 *   domain: geopolitical/legal
 *
 * SUMMARY:
 *   The Council of Europe's Register of Damage for Ukraine represents an
 *   attempt to institutionalize war crime accountability and future
 *   reparations claims through comprehensive documentation of losses caused
 *   by Russia's invasion. Established with support from US, Canada, Japan,
 *   and EU institutions, the register seeks to create an evidentiary
 *   foundation for potential future reparations proceedings against the
 *   Russian state. However, the constraint exhibits structural tension
 *   between its coordination function (documenting claims, establishing legal
 *   precedent, creating shared institutional framework for accountability)
 *   and its extraction function (imposing documentation burden on claimants,
 *   deferring resolution indefinitely, creating gatekeeping barriers). The
 *   constraint classifies as Tangled Rope from analytical perspective:
 *   genuine coordination benefit (the register creates institutional
 *   infrastructure that enables accountability mechanisms) combined with
 *   asymmetric extraction (individual claimants bear documentation burden
 *   while enforcement outcomes remain uncertain and potentially decades
 *   away). The theater ratio (0.58) reflects that much of the register's
 *   legitimacy derives from invocation of previous reparations frameworks
 *   (WWII, Holocaust, Yugoslav wars) that historically failed to deliver
 *   meaningful reparations. Individual claimants experience the register as
 *   Snare: trapped in documentation burden, dependent on mechanism whose
 *   success is uncertain, with no exit option except abandoning all claims.
 *
 * KEY AGENTS:
 *   - Individual Ukrainian claimants (powerless/trapped) — bear documentation burden and temporal uncertainty; primary extraction victims
 *   - Ukrainian state apparatus (organized/constrained) — administrator and beneficiary; must fund infrastructure while uncertain of enforcement
 *   - Western coalition states (institutional/arbitrage) — beneficiaries of coordination infrastructure; can adjust commitment levels
 *   - International legal institutions (institutional/constrained) — trapped in using register as evidentiary foundation despite methodological constraints
 *   - Accountability and documentation NGOs (organized/constrained) — view register as temporary infrastructure with genuine sunset post-conflict
 *   - Historical reparations precedent system (institutional/arbitrage) — invoked for legitimacy but structurally degraded from previous failures
 *   - Analytical observer (analytical/analytical) — risks naturalizing institutional constraints as inevitable features of international law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coe_ukraine_reparations_register, 0.52).
domain_priors:suppression_score(coe_ukraine_reparations_register, 0.68).
domain_priors:theater_ratio(coe_ukraine_reparations_register, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coe_ukraine_reparations_register, extractiveness, 0.52).
narrative_ontology:constraint_metric(coe_ukraine_reparations_register, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(coe_ukraine_reparations_register, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coe_ukraine_reparations_register, tangled_rope).
narrative_ontology:human_readable(coe_ukraine_reparations_register, "Council of Europe's Register of Damage for Ukraine").
narrative_ontology:topic_domain(coe_ukraine_reparations_register, "geopolitical/legal").

domain_priors:requires_active_enforcement(coe_ukraine_reparations_register).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coe_ukraine_reparations_register, ukrainian_state).
narrative_ontology:constraint_beneficiary(coe_ukraine_reparations_register, western_coalition_states).
narrative_ontology:constraint_beneficiary(coe_ukraine_reparations_register, future_claims_mechanism).
narrative_ontology:constraint_victim(coe_ukraine_reparations_register, ukrainian_claimants).
narrative_ontology:constraint_victim(coe_ukraine_reparations_register, displaced_persons).
narrative_ontology:constraint_victim(coe_ukraine_reparations_register, restoration_timeline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL UKRAINIAN CLAIMANT (SNARE) — Displaced persons, property loss claimants, and families of victims face maximum extraction through the reparations register's structural design. They bear the cost of documentation burden (gathering evidence, legal representation, language barriers), temporal deferral (claims registered today may take decades to adjudicate), and political contingency (reparations depend on future international enforcement that may never materialize). No exit option: cannot opt out of victim status, cannot recover losses through alternative mechanisms, trapped in dependence on a mechanism whose success is uncertain. High suppression: structural barriers to participation include documentation requirements in wartime, access to legal expertise, and information asymmetries about claims procedures.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UKRAINIAN STATE (TANGLED ROPE) — Experiences both coordination benefit and extraction. Coordination: the register legitimizes Ukraine's legal position in future reparations proceedings, establishes evidentiary basis for claims against Russia, and creates institutional infrastructure for justice mechanisms. Extraction: the state must fund registration infrastructure, legal analysis, and claims processing while uncertain whether international enforcement will ever produce reparations. Constrained exit: cannot refuse to use the mechanism (abandoning reparations claims entirely) without massive political cost; must continue participation even as mechanism absorbs resources. Active enforcement required: the Ukrainian state is both administrator and beneficiary, creating dual institutional roles. Requirements for enforcement fall on Ukraine's underfunded legal system.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WESTERN COALITION (ROPE) — Experiences the register primarily as coordination mechanism. Coalition benefits from: (1) shared institutional framework for potential future reparations enforcement, (2) legal documentation that supports sanctions legitimacy and war crimes prosecution narratives, (3) coordination on damage assessment methodology across allied states. These are genuine coordination benefits with minimal coercive overhead. Exit: coalition members can arbitrage by adjusting their commitment levels (funding, diplomatic support) without abandoning the mechanism entirely. The register's primary function from this perspective is information standard and coordination infrastructure. Low net extraction experienced by beneficiary coalition.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL LEGAL INSTITUTIONS (TANGLED ROPE) — International courts (ICJ, ICC), UN bodies, and potential future tribunals face both coordination and extraction pressures. Coordination: the register creates shared evidentiary foundation that all international legal mechanisms can reference, reducing duplicative documentation efforts. Extraction: the register also embeds institutional assumptions (what counts as damage, how claims are prioritized) that constrain how future legal proceedings can operate. These institutions are trapped in constrained participation: they must reference the register's findings because it will become the canonical damage assessment, but they may disagree with its methodology or prioritization. Active enforcement through normative pressure — the register's authority derives from institutionalization rather than explicit legal mandate.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HISTORICAL REPARATIONS PRECEDENT SYSTEM (PITON) — The register invokes and relies on previous reparations mechanisms (WWII German reparations, Holocaust restitution, Yugoslav wars proceedings) but these historical precedents are largely degraded institutional forms. Previous reparations frameworks have operated at 5-15% of claimed amounts, with decades-long delays, and bureaucratic complexity that excludes most claimants. The register performs continuity with this precedent system — claims that Ukraine's reparations will follow 'historical models' — but the historical models themselves failed to deliver. Theater ratio (0.58) reflects that much of the register's functionality is performative invocation of precedent rather than structural innovation. The mechanism maintains theatrical legitimacy ('we are following the proven reparations model') while structural failures of that model persist unchanged.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ACCOUNTABILITY AND DOCUMENTATION COALITION (SCAFFOLD) — Human rights organizations, documentation NGOs, and accountability mechanisms see the register as temporary coordination infrastructure with a genuine sunset clause. These agents view the register as a transitional mechanism: (1) during active conflict, documentation infrastructure is necessary because conflict prevents normal legal proceedings; (2) once conflict ends, the register becomes input to permanent legal institutions (courts, commissions). Exit: these organizations have meaningful agency — they can choose to participate at different levels, redirect resources to other accountability mechanisms, or advocate for alternative approaches. Theater ratio constraints are lower for this perspective because the focus is on documentation quality rather than performative legitimacy. The sunset is real: the register has no function post-conflict except as archived evidence for courts.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some gap between documented claims and enforceable reparations is structurally inherent to international law: (1) no world government exists to enforce reparations, (2) sovereignty protections make asset seizure legally impossible, (3) statute of limitations and evidence degradation are inevitable physical/temporal limits. This perspective treats the reparations gap as an unchangeable feature of international relations itself. However, structural data contradicts this: the gap is not natural — it reflects deliberate institutional choices (no enforcement mechanism, sovereignty protections, time limits) rather than physical limits. This perspective risks false summit classification, revealing how geopolitical powerlessness is naturalized as inevitable.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coe_ukraine_reparations_register_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coe_ukraine_reparations_register, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coe_ukraine_reparations_register, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coe_ukraine_reparations_register, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(coe_ukraine_reparations_register, TR),
    TR >= 0.70.

:- end_tests(coe_ukraine_reparations_register_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The register imposes substantial documentation burden on claimants in wartime conditions, creates temporal deferral uncertainty (claims registered now, enforcement potentially decades away), and depends entirely on international political will for enforcement. However, extractiveness is not extreme because the register also creates genuine coordination benefits: establishing legal precedent, creating shared institutional framework, and documenting claims that might otherwise be lost. The trajectory shows increasing extractiveness from 0.35 to 0.52 as the register matures and claimants realize that enforcement mechanisms remain underdeveloped despite documentation expansion. Suppression (0.68): High. Significant structural barriers include: wartime documentation constraints (fragmented records, displaced persons), language/cultural barriers to legal participation, expertise requirements for complex international legal procedures, asymmetric information about claims procedures, and political contingency on enforcement. These are not total barriers (some claimants can and do navigate them) but create substantial suppression of effective participation. Theater ratio (0.58): Moderate-high. The register performs continuity with historical reparations frameworks (WWII, Holocaust, Yugoslav wars), invoking precedent to legitimize its authority. However, previous reparations mechanisms operated at 5-15% of claimed amounts with decades-long delays, creating structural precedent for failure. The register's theater reflects that much of its legitimacy is performative invocation of 'proven' models rather than innovation in enforcement mechanisms. Theater has increased from 0.42 to 0.58 as documentation volumes expand without corresponding enforcement infrastructure development.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Individual claimants see pure extraction (Snare): trapped in documentation burden, dependent on uncertain enforcement, no exit. Ukrainian state sees mixed coordination and extraction (Tangled Rope): genuine benefit from legal infrastructure but constrained by resource burden and enforcement uncertainty. Western coalition sees coordination (Rope): shared legal framework with minimal cost and meaningful arbitrage options. International legal institutions see constrained participation (Tangled Rope): must reference register's findings but may disagree with methodology. Accountability NGOs see temporary infrastructure (Scaffold): genuine sunset as mechanism transitions to permanent legal institutions post-conflict. Historical reparations systems see degraded precedent (Piton): theater-maintained invocation of failed models. Analytical observer risks naturalizing institutional constraints (Mountain): treating enforcement gaps as inevitable features of international law rather than deliberate institutional choices. The perspectival gap reflects that the register simultaneously coordinates among institutional actors and extracts from vulnerable claimants.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values for each perspective are determined by structural position within the extraction flow. Individual claimants (powerless/trapped) have d ≈ 0.95: maximum target status, no exit, maximum experienced extraction. Ukrainian state (organized/constrained) has d ≈ 0.55: organized power provides some agency, but constrained exit (cannot refuse participation) limits arbitrage capacity. Western coalition (institutional/arbitrage) has d ≈ 0.15: institutional power and full arbitrage capacity (can adjust commitment without abandoning mechanism) puts them in beneficiary position. International legal institutions (institutional/constrained) have d ≈ 0.50: equal power but constrained by epistemic dependence on register's findings. Accountability NGOs (organized/constrained) have d ≈ 0.45: organized and partly mobile (can exit to alternative accountability mechanisms) but constrained by interdependence with other coalition members. The analytical observer (analytical/analytical) has d ≈ 0.72: external perspective with no structural position but at risk of rationalizing constraints as inevitable.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution (NOT YET RESOLVED) requires determining whether the register is primarily a coordination mechanism (Rope/Scaffold) being incorrectly labeled as extraction (Snare), or primarily an extraction mechanism (Snare/Tangled Rope) being incorrectly framed as justice. The constraint contains genuine coordination function: establishing legal precedent, creating shared institutional framework for accountability, enabling documentation that enables future justice mechanisms. But it also contains asymmetric extraction: claimants bear documentation burden while beneficiaries (state, coalition, international institutions) capture coordination benefits. The Tangled Rope classification holds at analytical level: both functions are structurally present and irreducible. The mandatrophy is resolved by recognizing that the classification varies by perspective — it is NOT a mandatrophy paradox but a legitimate perspectival divergence. Individual claimants experience Snare (maximum extraction, no coordination benefit). Organized actors (state, coalition, institutions) experience Tangled Rope or Rope (genuine coordination with constrained or moderate extraction). This perspectival stratification is the actual structure, not a classification ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_feasibility,
    'Will any mechanism for enforcing reparations against Russia actually materialize, or is the register purely symbolic documentation of unpayable claims?',
    'Observation of: (1) whether ICC convictions lead to asset seizure or warrant enforcement, (2) whether future negotiations include reparations enforcement provisions, (3) whether any mechanism recovers >10% of documented claims within 50 years',
    'If enforcement materializes: register is foundational infrastructure for real reparations mechanism (Tangled Rope confirmed). If enforcement fails: register becomes archive of unredeemable claims (Snare for claimants, Piton system confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism_feasibility, empirical, 'Whether international reparations enforcement mechanisms will actually be established and enforce against Russia').

omega_variable(
    documentation_burden_justice_equity,
    'Does the register''s documentation requirement create justice gatekeeping that systematically excludes lower-income, less-educated, or displaced claimants who cannot navigate complex legal procedures?',
    'Empirical analysis of who registers claims (socioeconomic stratification of claimant base), comparison of claim approval rates by demographic groups, analysis of representation rates (percentage of eligible claimants who actually register)',
    'If high exclusion: register is extraction mechanism disguised as justice (Snare confirmed). If low exclusion: register is genuine coordination mechanism (Rope strengthened). If moderate: Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_burden_justice_equity, empirical, 'Whether documentation requirements create systematic exclusion of lower-income or less-educated claimants').

omega_variable(
    temporal_deferral_legitimacy,
    'Does temporal deferral of reparations resolution (claims registered now, enforcement possibly decades away) constitute unjust extraction from claimants, or acceptable coordination overhead for complex international proceedings?',
    'Comparative analysis with other reparations mechanisms (WWII, Holocaust, Yugoslav wars) examining: (1) average time from claim registration to payment, (2) inflation adjustment mechanisms, (3) psychological impact on claimant cohorts',
    'If deferral is extractive: chi values increase (higher effective extraction), Snare and Tangled Rope classifications strengthened. If deferral is acceptable overhead: classifications shift toward Rope and Scaffold, theater ratio justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_deferral_legitimacy, empirical, 'Whether temporal deferral of reparations constitutes extraction or acceptable coordination complexity').

omega_variable(
    russian_asset_identification_capacity,
    'Can international institutions actually identify, locate, and legally seize Russian state and oligarch assets to fund reparations, or does sovereignty protection and asset concealment make enforcement mechanically impossible?',
    'Audit of: (1) frozen Russian assets held in Western banks (SWIFT sanctions), (2) mechanisms for converting frozen assets to reparations funding, (3) legal precedent for non-consensual asset transfer between sovereign states',
    'If mechanical feasibility exists: register becomes credible enforcement foundation (Tangled Rope to Rope shift). If infeasible: register is documentation of confiscation targets that will never be seized (Piton classification, false summit revealed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(russian_asset_identification_capacity, empirical, 'Whether enforcement mechanism can actually identify and seize Russian assets for reparations').

omega_variable(
    ukrainian_state_capture_risk,
    'Does the register''s institutional structure create opportunity for Ukrainian state to prioritize politically-connected claimants or use reparations mechanism for state capture?',
    'Institutional analysis of register governance (decision-making authority, appeals procedures, transparency), observation of claim approval patterns by claimant political connections, audits of state allocation of reparations if enforcement occurs',
    'If capture risk is high: beneficiary role shifts from ''Ukrainian state'' to ''politically-connected elites'', converting Tangled Rope to Snare for ordinary claimants. If capture risk is low: Tangled Rope stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ukrainian_state_capture_risk, empirical, 'Whether register governance creates state capture risk for reparations allocation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coe_ukraine_reparations_register, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coe_dam_tr_t0, coe_ukraine_reparations_register, theater_ratio, 0, 0.42).
narrative_ontology:measurement(coe_dam_tr_t3, coe_ukraine_reparations_register, theater_ratio, 3, 0.52).
narrative_ontology:measurement(coe_dam_tr_t6, coe_ukraine_reparations_register, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(coe_dam_be_t0, coe_ukraine_reparations_register, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(coe_dam_be_t3, coe_ukraine_reparations_register, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(coe_dam_be_t6, coe_ukraine_reparations_register, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coe_ukraine_reparations_register, enforcement_mechanism).
narrative_ontology:affects_constraint(coe_ukraine_reparations_register, international_sanctions_regime).
narrative_ontology:affects_constraint(coe_ukraine_reparations_register, russian_asset_freezing).
narrative_ontology:affects_constraint(coe_ukraine_reparations_register, icc_prosecution_ukraine).
narrative_ontology:affects_constraint(coe_ukraine_reparations_register, post_conflict_reconciliation_mechanisms).

% DUAL FORMULATION NOTE:
% The reparations register decomposes into two structurally distinct constraints: (1) documentation and evidence collection (coordination infrastructure, ε ≈ 0.25-0.35, Rope/Scaffold), (2) enforcement mechanism and reparations distribution (extraction and political contingency, ε ≈ 0.60-0.75, Tangled Rope/Snare). This constraint story addresses the integrated mechanism; separate stories should decompose enforcement mechanism (higher extractiveness due to political contingency) and documentation function (lower extractiveness, higher coordination benefit).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coe_ukraine_reparations_register, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
