% ============================================================================
% CONSTRAINT STORY: rn_proteus_adoption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rn_proteus_adoption, []).

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
 *   constraint_id: rn_proteus_adoption
 *   human_readable: Royal Navy's adoption of the Leonardo Proteus uncrewed helicopter
 *   domain: technological/military_procurement
 *
 * SUMMARY:
 *   The Royal Navy's two-year experimental contract to integrate Leonardo's
 *   Proteus uncrewed helicopter into operational service beginning 2026
 *   creates a hybrid constraint combining genuine military modernization
 *   (coordination function) with industrial policy extraction and
 *   institutional career obsolescence. The constraint exhibits tangled rope
 *   structure: Leonardo receives market validation and pathway to NATO export
 *   sales (extraction beneficiary); the Navy gains extended sortie duration
 *   and reduced crew fatigue (coordination benefit); helicopter pilots face
 *   career obsolescence without retraining pathways (extraction victim);
 *   fleet operations gain unmanned capability but lose human judgment
 *   redundancy (mixed outcome). The theater ratio (0.58) reflects moderate
 *   performative content: NATO interoperability justification is largely
 *   inert (piton-like), yet the underlying acquisition logic is defensible on
 *   modernization grounds. Extractiveness rises from 0.22 (initial
 *   procurement announcement) to 0.38 (two-year contract execution) as the
 *   career disruption window becomes concrete and institutional inertia
 *   around pilot retraining becomes apparent.
 *
 * KEY AGENTS:
 *   - Leonardo Defence Systems: Primary beneficiary (institutional/arbitrage) — gains market validation, operational data, pathway to NATO exports; can repurpose technology to allied navies
 *   - Helicopter Pilot Community: Primary victim (powerless/trapped) — faces career obsolescence during two-year window; limited exit options within service; civilian helicopter markets cannot absorb displaced naval pilots
 *   - Royal Navy Fleet Operations Command: Secondary beneficiary and victim (organized/constrained) — gains coordination benefits (reduced crew, extended range) but bears integration risk and operational redundancy loss
 *   - UK Defence Industrial Policy Coalition: Organized beneficiary (organized/constrained) — uses Proteus adoption to advance autonomous naval operations strategy; sunset logic provides exit path if technology fails
 *   - NATO Interoperability Standards Framework: Performative justification actor (institutional/constrained) — cited as rationale for adoption; functionally degraded (piton characteristics); allied navies operate incompatible unmanned systems
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing procurement-driven decisions as engineering imperatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rn_proteus_adoption, 0.38).
domain_priors:suppression_score(rn_proteus_adoption, 0.42).
domain_priors:theater_ratio(rn_proteus_adoption, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rn_proteus_adoption, extractiveness, 0.38).
narrative_ontology:constraint_metric(rn_proteus_adoption, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(rn_proteus_adoption, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rn_proteus_adoption, tangled_rope).
narrative_ontology:human_readable(rn_proteus_adoption, "Royal Navy's adoption of the Leonardo Proteus uncrewed helicopter").
narrative_ontology:topic_domain(rn_proteus_adoption, "technological/military_procurement").

domain_priors:requires_active_enforcement(rn_proteus_adoption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rn_proteus_adoption, leonardo_defence_systems).
narrative_ontology:constraint_beneficiary(rn_proteus_adoption, naval_modernization_advocates).
narrative_ontology:constraint_victim(rn_proteus_adoption, helicopter_pilot_community).
narrative_ontology:constraint_victim(rn_proteus_adoption, operational_flexibility).
narrative_ontology:constraint_victim(rn_proteus_adoption, tactical_decision_latitude).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HELICOPTER PILOT COMMUNITY (SNARE) — Naval aviators face career obsolescence as Proteus integration proceeds. Exit is constrained by service commitments, seniority locks, and limited civilian helicopter markets. The two-year contract creates a window where pilot skills are devalued but replacement infrastructure not yet operational. Maximum extraction: career termination risk without alternative employment paths within the service.
constraint_indexing:constraint_classification(rn_proteus_adoption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ROYAL NAVY FLEET OPERATIONS COMMAND (TANGLED ROPE) — Experiences both coordination benefits (reduced crew requirements, extended sortie duration, lower fatigue) and extraction mechanisms (operational risk concentration, loss of human judgment redundancy, dependence on Leonardo software/support). Constrained by defense budget pressures and NATO interoperability mandates. Benefits from modernization rhetoric but bears integration costs.
constraint_indexing:constraint_classification(rn_proteus_adoption, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEONARDO DEFENCE SYSTEMS (ROPE) — Primary beneficiary. Two-year experimental contract provides market validation, operational data, and pathway to NATO export contracts. Exit via arbitrage: can repurpose Proteus development to other allied navies (France, Germany, South Korea). Extraction flow runs toward Leonardo; they solve a genuine coordination problem (unmanned aviation standards) while capturing licensing and support revenue.
constraint_indexing:constraint_classification(rn_proteus_adoption, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UK DEFENCE INDUSTRIAL POLICY COALITION (SCAFFOLD) — Government technology strategy actors (DASA, UKRI, defence contractors) see Proteus adoption as a temporary bridge to autonomous naval operations. The experimental contract has explicit sunset logic: success criteria at two years trigger full acquisition; failure triggers pilot reclassification or retirement. Theater remains moderate because the coalition can point to measurable integration metrics. Suppression is low relative to the constraint's true extractiveness because policy actors retain exit options and decision authority.
constraint_indexing:constraint_classification(rn_proteus_adoption, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NATO INTEROPERABILITY STANDARDS FRAMEWORK (PITON) — The NATO drone interoperability requirement that nominally justifies Proteus adoption is largely inert. Allied navies operate distinct unmanned systems (U.S. MQ-8 Fire Scout, French ASN301); true interoperability has not materialized despite years of standardization effort. The requirement persists through institutional inertia — cited as rationale for adoption but functionally degraded. Theater ratio elevated (estimated 0.70): the interoperability argument is performative justification for a procurement decision driven by industrial policy and budget mechanics.
constraint_indexing:constraint_classification(rn_proteus_adoption, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ENGINEERING MATURITY VIEW (MOUNTAIN) — From an engineering perspective, human-to-unmanned transition in maritime operations has immutable constraints rooted in signal propagation, latency, and cognitive load. Uncrewed helicopters cannot respond to tactical ambiguity without human intervention; human intervention adds latency. The observer risks naturalizing this as an inherent law. However, the constraint's extractiveness (0.38) suggests institutional rather than natural origins — the bottleneck is procurement architecture and career incentive structure, not physics.
constraint_indexing:constraint_classification(rn_proteus_adoption, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rn_proteus_adoption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rn_proteus_adoption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rn_proteus_adoption, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(rn_proteus_adoption, TR),
    TR >= 0.70.

:- end_tests(rn_proteus_adoption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint combines genuine modernization value (unmanned helicopter capability extends naval reach) with significant rent-seeking (Leonardo's contract position, defense budget concentration). The intermediate value reflects that the Navy is not being purely coerced — modernization serves real operational goals — but the distribution of costs (pilot career obsolescence) and benefits (Leonardo revenue, Navy modernization credit) is asymmetric. Suppression (0.42): Moderate. Helicopter pilots have constrained but not zero exit options: they can request transfer to other services, exit the military entirely, or retrain for remote operations roles. However, career seniority locks, service commitment length, and limited civilian helicopter markets create significant barriers. The Navy's institutional status creates suppression through budget authority and procurement discretion — alternatives (continuing with piloted helicopters, licensing U.S. Fire Scout) are subordinated to the Proteus pathway. Theater ratio (0.58): Moderate-high. NATO interoperability justification is substantially performative (piton characteristics): allied navies operate incompatible unmanned systems, and genuine interoperability has not materialized despite standardization efforts. The theater reflects that the acquisition decision is driven by industrial policy and modernization narrative more than operational necessity. As the two-year contract executes, theater increases (to 0.58) as success metrics are defined retrospectively and integration challenges are reframed as learning opportunities.
 *
 * PERSPECTIVAL GAP:
 *   Helicopter pilots perceive pure extraction (snare): their career is devalued, no transition pathway is guaranteed, and the constraint operates without meaningful consent. Leonardo and defense industrial policy actors perceive pure coordination (rope/scaffold): they are solving a genuine problem (extending naval unmanned capability, advancing NATO interoperability) and capturing legitimate returns. The Royal Navy operations command perceives tangled rope: they gain real capability benefits but also lose human judgment redundancy and become dependent on Leonardo's software/support ecosystem. The NATO standards framework is inert (piton): cited as rationale but functionally degraded, persisting through institutional inertia. The analytical observer risks naturalizing this as an engineering necessity (mountain), but the structural data reveals it as a contingent institutional arrangement (extractiveness 0.38 reflects human careers and industrial policy, not physics).
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality is derived from their structural position within the constraint. Leonardo (institutional/arbitrage) acts as a beneficiary with exit options — they can repurpose technology to other NATO allies, reducing their dependence on Royal Navy adoption. Helicopter pilots (powerless/trapped) experience maximum extraction because they cannot exit without significant career cost and no alternative pathway is guaranteed. Fleet operations command (organized/constrained) experiences moderate extraction because they retain some decision authority and gain coordination benefits, but are constrained by budget pressure and institutional interoperability mandates. The defense industrial policy coalition (organized/constrained) sees the constraint as a controlled transition with sunset logic — success triggers full acquisition, failure triggers pilot reclassification. Each perspective's d-value is determined by this structural map: beneficiaries with exit options get low d (negative chi), victims without exit get high d (high chi), mixed-benefit agents get moderate d. The piton classification for NATO standards reflects their inertial role — they provide rhetorical justification without functional content.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing between genuine coordination (modernization of naval capability, extension of operational reach) and pure extraction (pilot career obsolescence, defense budget capture by Leonardo). The tangled rope classification correctly identifies both: there is a real coordination function (unmanned helicopter integration solves a genuine military problem) AND asymmetric extraction (pilot displacement without guaranteed retraining, Leonardo's capture of industrial policy rents). The perspectival gap is not a failure of the framework but a necessary outcome: from the pilot's perspective, this is a snare (extraction without coordination benefit). From Leonardo's perspective, this is a rope (coordination with legitimate returns). From the analytical observer's perspective, this is a tangled rope (both elements present, both structurally necessary to explain the constraint's persistence). The scaffold perspective (two-year sunset) is structurally sound: if Proteus fails to meet operational maturity, the constraint dissolves and pilots are not displaced. The piton perspective reveals that NATO interoperability justification is largely theatrical — the real driver is industrial policy and modernization narrative. By mapping all six types from different structural positions, the framework prevents the false natural law (mountain) classification that would naturalize a procurement decision as an engineering imperative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pilot_labor_absorption,
    'Will the Royal Navy retrain and redeploy displaced helicopter pilots into alternative roles (remote operations, maintenance, training) or accept career termination?',
    'Personnel policy review; analysis of historical navy workforce transitions; interviews with naval aviation career planners',
    'If retraining pathways exist: extraction is constrained (victims have exits). If no pathways: extraction becomes severe (snare classification correct for pilot community).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pilot_labor_absorption, preference, 'Whether displaced helicopter pilots have retraining and redeployment opportunities').

omega_variable(
    proteus_operational_maturity,
    'Will Leonardo Proteus achieve operational reliability targets (>85% mission success) by end of two-year contract, or will performance failures justify pilot continuation?',
    'Technical performance data; comparison to stated readiness goals; operational incident logs',
    'If mature: scaffold sunset is real (transition pathway exists). If immature: Proteus adoption fails and helicopter pilots are not displaced — constraint dissolves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proteus_operational_maturity, empirical, 'Whether Leonardo Proteus achieves operational maturity targets').

omega_variable(
    nato_interoperability_enforcement,
    'Is NATO drone interoperability a genuine binding requirement for Royal Navy procurement, or a rhetorical justification for a domestically-driven acquisition?',
    'NATO bureaucratic documents; comparison of Proteus specs to official NATO standards; analysis of whether interoperability failure would trigger abandonment',
    'If genuine requirement: constraint reflects external coordination pressure (reduces claim on pure extraction). If rhetorical: piton classification confirmed — requirement is performative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nato_interoperability_enforcement, empirical, 'Whether NATO interoperability is a genuine binding requirement').

omega_variable(
    industrial_policy_extraction_magnitude,
    'What fraction of the Proteus acquisition cost (£X million over two years) represents genuine operational value versus industrial policy subsidies to Leonardo?',
    'Cost-benefit analysis of Proteus capability vs. alternative platforms (piloted helicopter upgrades, existing Fire Scout variants); Leonardo profit margin analysis',
    'If >40% represents subsidy: extractiveness rises to 0.50+. If <20%: constraint approaches pure coordination (rope classification justified).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industrial_policy_extraction_magnitude, empirical, 'Magnitude of industrial policy subsidy embedded in Proteus acquisition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rn_proteus_adoption, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rnprot_tr_t0, rn_proteus_adoption, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rnprot_tr_t1, rn_proteus_adoption, theater_ratio, 1, 0.5).
narrative_ontology:measurement(rnprot_tr_t2, rn_proteus_adoption, theater_ratio, 2, 0.58).

% Extraction over time
narrative_ontology:measurement(rnprot_be_t0, rn_proteus_adoption, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(rnprot_be_t1, rn_proteus_adoption, base_extractiveness, 1, 0.32).
narrative_ontology:measurement(rnprot_be_t2, rn_proteus_adoption, base_extractiveness, 2, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rn_proteus_adoption, enforcement_mechanism).
narrative_ontology:affects_constraint(rn_proteus_adoption, nato_maritime_drone_standardization).
narrative_ontology:affects_constraint(rn_proteus_adoption, uk_defence_industrial_strategy).

% DUAL FORMULATION NOTE:
% The Proteus adoption constraint is downstream of NATO drone standardization requirements but represents a distinct structural phenomenon. NATO standardization (epsilon ~0.15) is a coordination problem; Proteus adoption (epsilon 0.38) adds industrial policy extraction and career disruption. These should be modeled as separate constraints linked via affects_constraints to enable separate analysis of coordination vs extraction components.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rn_proteus_adoption, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
