% ============================================================================
% CONSTRAINT STORY: enforcement_technology_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_enforcement_technology_lock_in, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: enforcement_technology_lock_in
 *   human_readable: License Plate Recognition Vendor Lock-In
 *   domain: urban_planning/public_resource_allocation/transportation_policy
 *
 * SUMMARY:
 *   License plate recognition (LPR) systems were initially adopted by
 *   municipalities to automate parking enforcement and reduce labor costs.
 *   Over a 12-year deployment period, the constraint evolved from a
 *   coordination mechanism into vendor lock-in with substantial extraction.
 *   The theater_ratio increased from 0.25 to 0.58 as the original enforcement
 *   function atrophied into vendor dependency management, data privacy
 *   compliance theater, and surveillance infrastructure maintenance. Base
 *   extractiveness rose from 0.35 to 0.68 as proprietary data formats,
 *   multi-year contracts with auto-renewal clauses, and integration
 *   dependencies trapped municipalities in vendor relationships. Suppression
 *   increased from 0.40 to 0.72 as exit barriers accumulated: data migration
 *   costs, staff retraining requirements, enforcement gaps during transition,
 *   and political backlash from enforcement interruptions. The constraint
 *   demonstrates piton dynamics from the municipal transportation department
 *   perspective — the original function has degraded but the system persists
 *   through institutional inertia and budget capture. However, the analytical
 *   perspective classifies it as tangled_rope because the extraction
 *   mechanism remains functionally active, not merely performative.
 *
 * KEY AGENTS:
 *   - Parking Technology Vendors: Primary beneficiary (institutional/arbitrage) — capture recurring revenue through proprietary lock-in and contract terms; can exit to other municipalities
 *   - Municipal Fiscal Autonomy: Primary victim (powerless/trapped) — loses procurement flexibility and budget control; faces prohibitive exit costs through sunk investments and operational dependencies
 *   - Privacy Advocates: Secondary victim (moderate/constrained) — face surveillance infrastructure expansion with limited ability to prevent deployment once contracts are signed; can lobby for restrictions but not reversal
 *   - Municipal Transportation Departments: Institutional actor (institutional/constrained) — maintain systems through inertia; recognize functional atrophy but lack exit pathways
 *   - Open-Source Enforcement Coalition: Organized agents (organized/mobile) — building alternative pathways through open-source LPR and municipal data cooperatives; see sunset logic
 *   - Surveillance Infrastructure Advocates: Secondary beneficiary (powerful/mobile) — benefit from normalized surveillance infrastructure that parking enforcement legitimizes; can pivot to other deployment justifications
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(enforcement_technology_lock_in, 0.68).
domain_priors:suppression_score(enforcement_technology_lock_in, 0.72).
domain_priors:theater_ratio(enforcement_technology_lock_in, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(enforcement_technology_lock_in, extractiveness, 0.68).
narrative_ontology:constraint_metric(enforcement_technology_lock_in, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(enforcement_technology_lock_in, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(enforcement_technology_lock_in, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(enforcement_technology_lock_in, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(enforcement_technology_lock_in, piton).
narrative_ontology:human_readable(enforcement_technology_lock_in, "License Plate Recognition Vendor Lock-In").
narrative_ontology:topic_domain(enforcement_technology_lock_in, "urban_planning/public_resource_allocation/transportation_policy").

domain_priors:requires_active_enforcement(enforcement_technology_lock_in).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(enforcement_technology_lock_in, 'd2baa7fa-d014-4753-aa38-94423fa264ca').
narrative_ontology:cs_kernel_codification('d2baa7fa-d014-4753-aa38-94423fa264ca', formalized).
narrative_ontology:cs_authority_grounding('d2baa7fa-d014-4753-aa38-94423fa264ca', lineage).
narrative_ontology:cs_interpretation_layer_present('d2baa7fa-d014-4753-aa38-94423fa264ca').
narrative_ontology:cs_reading_relation('d2baa7fa-d014-4753-aa38-94423fa264ca', enforcement_technology_lock_in__property_tax_entitlement_reading, influences).
narrative_ontology:cs_reading_relation('d2baa7fa-d014-4753-aa38-94423fa264ca', enforcement_technology_lock_in__equity_redistribution_reading, influences).
narrative_ontology:cs_axiom('d2baa7fa-d014-4753-aa38-94423fa264ca', foundational, cost_recovery_pricing_legitimacy).
narrative_ontology:cs_axiom_status(cost_recovery_pricing_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d2baa7fa-d014-4753-aa38-94423fa264ca', cost_recovery_pricing_legitimacy, instrumental).
narrative_ontology:cs_axiom('d2baa7fa-d014-4753-aa38-94423fa264ca', secondary, enforcement_automation_necessity).
narrative_ontology:cs_axiom_status(enforcement_automation_necessity, holdable).
narrative_ontology:cs_axiom_grounding('d2baa7fa-d014-4753-aa38-94423fa264ca', enforcement_automation_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('d2baa7fa-d014-4753-aa38-94423fa264ca', municipal_police_power_allocation).
narrative_ontology:cs_drift_state('d2baa7fa-d014-4753-aa38-94423fa264ca', post_lpr_deployment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d2baa7fa-d014-4753-aa38-94423fa264ca', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(enforcement_technology_lock_in, parking_technology_vendors).
narrative_ontology:constraint_beneficiary(enforcement_technology_lock_in, surveillance_infrastructure_advocates).
narrative_ontology:constraint_victim(enforcement_technology_lock_in, municipal_fiscal_autonomy).
narrative_ontology:constraint_victim(enforcement_technology_lock_in, privacy_advocates).
narrative_ontology:constraint_victim(enforcement_technology_lock_in, alternative_enforcement_pathways).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(enforcement_technology_lock_in, open_source_enforcement_coalition).
narrative_ontology:constraint_victim(enforcement_technology_lock_in, municipal_transportation_departments).
narrative_ontology:constraint_vindicates(enforcement_technology_lock_in, technology_inevitability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Vendors provide LPR systems under multi-year contracts with proprietary data formats and auto-renewal clauses. They capture recurring revenue through vendor lock-in mechanisms: data migration costs, integration dependencies, and staff training on vendor-specific systems. Vendors can exit to other municipalities if any single contract becomes unprofitable, giving them arbitrage-level exit options. They experience the constraint as pure coordination — providing technology that municipalities voluntarily adopt.
narrative_ontology:constraint_stakeholder(enforcement_technology_lock_in, parking_technology_vendors, beneficiary,
    institutional, immediate, arbitrage, national).

% Transportation departments procure and operate LPR systems. They set enforcement policy and manage vendor relationships. Over time, they recognize that the original enforcement function has atrophied into vendor dependency management and data privacy compliance theater, but they lack exit pathways due to sunk costs, operational dependencies, and political risk of enforcement gaps during transition. They bear the costs of vendor lock-in through constrained procurement flexibility and budget capture.
narrative_ontology:constraint_stakeholder(enforcement_technology_lock_in, municipal_transportation_departments, agenda_setter,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(enforcement_technology_lock_in, municipal_transportation_departments, payer).

% Municipal fiscal autonomy is an abstract collective good — the municipality's ability to control its own budget and procurement decisions. LPR vendor lock-in extracts from this autonomy through proprietary data formats, multi-year contracts, and integration dependencies that trap municipalities in vendor relationships. As an abstract good with no advocate, it has no exit options and bears maximum extraction.
narrative_ontology:constraint_stakeholder(enforcement_technology_lock_in, municipal_fiscal_autonomy, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_non_agent(enforcement_technology_lock_in, municipal_fiscal_autonomy).

% Privacy advocates lobby for data retention limits, usage restrictions, and transparency requirements for LPR systems. They face surveillance infrastructure expansion with limited ability to prevent deployment once vendor contracts are signed. They can influence policy at the margins (data retention periods, audit requirements) but cannot reverse deployment decisions. Exit is constrained by the political momentum behind enforcement automation and the technology-inevitability framing.
narrative_ontology:constraint_stakeholder(enforcement_technology_lock_in, privacy_advocates, payer,
    moderate, biographical, constrained, regional).

% Organized advocates for open-source LPR systems (OpenALPR, municipal data cooperatives) and municipal data sovereignty. They are building alternative enforcement pathways that bypass proprietary vendor lock-in. They have mobile exit options because they can choose which municipalities to work with and can pivot to other open-source infrastructure projects if LPR efforts stall. They benefit from the constraint's existence because vendor lock-in creates demand for their alternative solutions.
narrative_ontology:constraint_stakeholder(enforcement_technology_lock_in, open_source_enforcement_coalition, beneficiary,
    organized, generational, mobile, regional).

% Law enforcement agencies and security-focused policymakers who benefit from normalized surveillance infrastructure that parking enforcement legitimizes. LPR systems deployed for parking enforcement create data streams and technical capacity that can be repurposed for broader surveillance objectives. They have mobile exit options because they can pivot to other surveillance deployment justifications (public safety, counterterrorism) if parking enforcement becomes politically untenable.
narrative_ontology:constraint_stakeholder(enforcement_technology_lock_in, surveillance_infrastructure_advocates, beneficiary,
    powerful, biographical, mobile, national).

% Alternative enforcement methods (meter readers, permit systems without LPR, open-source LPR) that are excluded from consideration once proprietary LPR systems are deployed. These alternatives would object to vendor lock-in if they had a voice in procurement decisions, but they are structurally excluded by path dependencies and the technology-inevitability framing. As abstract alternatives rather than organized agents, they have no exit options.
narrative_ontology:constraint_stakeholder(enforcement_technology_lock_in, alternative_enforcement_pathways, excluded,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_non_agent(enforcement_technology_lock_in, alternative_enforcement_pathways).

% The analytical observer sees the constraint as tangled_rope at the civilizational scale: genuine coordination (parking enforcement automation) coupled with asymmetric extraction (vendor capture, surveillance expansion). The observer recognizes that the piton classification from the municipal transportation department perspective is a valid local observation but understates the constraint's active extraction mechanism.
narrative_ontology:constraint_stakeholder(enforcement_technology_lock_in, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(enforcement_technology_lock_in, parking_technology_vendors).
narrative_ontology:fixing_cost_class(enforcement_technology_lock_in, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Automating parking enforcement to reduce labor costs and increase compliance rates. LPR systems solve the genuine coordination problem of detecting parking violations without deploying meter readers to manually check every vehicle.
% TRANSFER_FUNCTION: The arrangement transfers municipal budget autonomy and procurement flexibility from municipalities to parking technology vendors through proprietary lock-in mechanisms. It also transfers privacy (location data, movement patterns) from vehicle owners to surveillance infrastructure operators. Revenue flows from municipalities to vendors through recurring contract payments.
% ABSENT_VOICES: Alternative enforcement pathways (meter readers, permit systems without LPR, open-source LPR) are excluded from procurement decisions once proprietary systems are deployed. Municipal fiscal autonomy (the abstract collective good of budget control) has no advocate in vendor contract negotiations. Privacy advocates are present but structurally constrained — they can lobby for restrictions but cannot prevent deployment.
% DISAPPEARANCE_RATIONALE: If LPR systems disappeared overnight, municipalities would need to rearrange enforcement operations: redeploying meter readers, redesigning permit systems, or adopting open-source alternatives. Parking enforcement would continue but through different mechanisms. The vendor lock-in is not a natural fact — it is a contingent institutional arrangement that shapes municipal operations.
% FOUNDING_PROBLEM: High labor costs and low compliance rates for parking enforcement in the early 2000s. Municipalities faced budget pressure to reduce meter reader headcount while maintaining or increasing parking revenue. LPR systems were adopted to automate violation detection and reduce enforcement costs.
% FOUNDING_PROBLEM_CORROBORATION: Municipal budget offices and transportation departments attest that labor cost reduction was the original driver. However, privacy advocates and fiscal policy analysts argue that the founding problem (labor costs) has been superseded by vendor lock-in costs that now exceed the labor savings. Open-source enforcement coalition members provide corroboration from outside the beneficiary set: they document cases where vendor contract costs exceed the original meter reader budgets, suggesting the founding problem is no longer the primary justification for system persistence.
narrative_ontology:disappearance_verdict(enforcement_technology_lock_in, world_rearranges).
narrative_ontology:founding_problem_status(enforcement_technology_lock_in, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MUNICIPAL FISCAL AUTONOMY (SNARE) — Once LPR systems are deployed, municipalities face vendor lock-in through proprietary data formats, multi-year contracts with auto-renewal clauses, and integration dependencies. Exit costs include data migration expenses, retraining staff, and political backlash from enforcement gaps during transition. The municipality is structurally trapped by sunk costs and operational dependencies.
constraint_indexing:constraint_classification(enforcement_technology_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PRIVACY ADVOCATES (TANGLED ROPE) — LPR systems do solve a genuine coordination problem (parking enforcement without meter readers), but the surveillance infrastructure extracts privacy costs asymmetrically. Privacy advocates have constrained exit: they can lobby for data retention limits and usage restrictions, but cannot prevent deployment once vendor contracts are signed. Mixed coordination and extraction.
constraint_indexing:constraint_classification(enforcement_technology_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PARKING TECHNOLOGY VENDORS (ROPE) — Vendors experience the constraint as pure coordination: they provide enforcement technology that municipalities voluntarily adopt. The lock-in mechanisms (proprietary formats, contract terms) are standard business practices from their perspective. Net beneficiaries with arbitrage exit — they can walk away from any single municipality and sell to others.
constraint_indexing:constraint_classification(enforcement_technology_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MUNICIPAL TRANSPORTATION DEPARTMENTS (PITON) — Transportation departments recognize that LPR enforcement has become substantially performative: the original function (efficient parking enforcement) has atrophied into vendor dependency management and data privacy theater. Departments maintain the systems through institutional inertia and budget capture, not because they optimize enforcement. The constraint persists as performance of technological modernity.
constraint_indexing:constraint_classification(enforcement_technology_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OPEN-SOURCE ENFORCEMENT COALITION (SCAFFOLD) — Organized advocates for open-source LPR systems and municipal data sovereignty see vendor lock-in as a temporary coordination failure. Open-source alternatives (OpenALPR, municipal data cooperatives) are building exit pathways. This perspective sees a sunset: as open-source tools mature and municipalities share implementation costs, proprietary lock-in loses force. Estimated sunset: 8-15 years for open-source adoption to reach critical mass.
constraint_indexing:constraint_classification(enforcement_technology_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, LPR lock-in represents a genuine coordination problem (automating enforcement) coupled with asymmetric extraction (vendor capture of municipal autonomy and privacy erosion). The constraint requires active enforcement through contract renewal and data integration dependencies. The analytical classification is tangled_rope, not piton, because the extraction mechanism is still functionally active, not merely theatrical.
constraint_indexing:constraint_classification(enforcement_technology_lock_in, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(enforcement_technology_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(enforcement_technology_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(enforcement_technology_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(enforcement_technology_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(enforcement_technology_lock_in, TR),
    TR >= 0.70.

:- end_tests(enforcement_technology_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Vendors capture recurring revenue through proprietary lock-in mechanisms that were not transparent at initial adoption. Municipalities lose procurement flexibility, budget autonomy, and negotiating leverage. The extraction is not total (0.68 rather than 0.85+) because some municipalities have successfully renegotiated contracts or deployed open-source alternatives, demonstrating that exit is costly but not impossible. Suppression (0.72): High. Exit barriers include proprietary data formats requiring expensive migration, multi-year contracts with auto-renewal and penalty clauses, staff trained only on vendor-specific systems, integration dependencies with other municipal IT systems, and political risk of enforcement gaps during transition. Suppression is structural (external barriers) rather than internalized — municipalities recognize the lock-in but face material obstacles to exit. Theater_ratio (0.58): Moderate-high. The original enforcement function (automated parking violation detection) has partially atrophied into vendor relationship management, data privacy compliance theater, and surveillance infrastructure maintenance. However, the systems still perform some genuine enforcement work, so the theater ratio is not as high as a pure piton (0.75+). The increase over time reflects functional degradation as vendor dependency management consumes more resources relative to enforcement output. Accessibility_collapse (0.45): Moderate. Alternative enforcement pathways (meter readers, permit systems, open-source LPR) remain accessible in principle, but LPR deployment creates path dependencies that make alternatives appear obsolete or politically infeasible. The collapse is partial because open-source alternatives are emerging. Resistance (0.62): Moderate-high. Privacy advocates, fiscal conservatives, and open-source proponents actively resist LPR expansion and vendor lock-in. Resistance is substantial but not universal — many stakeholders accept the technology-inevitability framing.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a four-way perspectival split. Vendors see pure coordination (rope) — they provide technology that municipalities voluntarily adopt, and lock-in is standard business practice. Municipal transportation departments see degraded function maintained through inertia (piton) — the original enforcement purpose has atrophied into vendor dependency management. Privacy advocates and municipal fiscal autonomy see mixed coordination and extraction (tangled_rope from privacy advocates, snare from fiscal autonomy) — genuine enforcement automation coupled with asymmetric extraction of privacy and procurement flexibility. The open-source coalition sees a temporary problem with a sunset (scaffold) — open-source alternatives are building exit pathways. The analytical observer resolves this as tangled_rope at the civilizational scale: the constraint solves a genuine coordination problem (parking enforcement automation) but embeds asymmetric extraction (vendor capture, surveillance expansion) that requires active enforcement through contract terms and data integration dependencies. The piton classification from the municipal transportation department perspective is a valid local observation but understates the constraint's active extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Parking technology vendors are primary beneficiaries with arbitrage exit options — they experience the constraint as pure coordination (rope classification) because lock-in mechanisms are standard business practices from their perspective. They can walk away from any single municipality and sell to others, giving them low directionality (d ≈ 0.1-0.2) and negative or near-zero effective extraction. Municipal fiscal autonomy is the primary victim with trapped exit options — the constraint extracts procurement flexibility and budget control. As a powerless agent (abstract collective good with no advocate), it has high directionality (d ≈ 0.85-0.95) and experiences maximum effective extraction. Privacy advocates are secondary victims with constrained exit — they face surveillance expansion but retain some agency through lobbying and litigation. Moderate power and constrained exit give them mid-range directionality (d ≈ 0.55-0.65) and moderate effective extraction. Municipal transportation departments are institutional actors with constrained exit — they recognize functional atrophy but lack exit pathways due to sunk costs and operational dependencies. Their directionality (d ≈ 0.50-0.60) reflects mixed experience: they benefit from automation but bear vendor dependency costs. The open-source coalition has mobile exit options and sees a sunset, giving them low directionality (d ≈ 0.25-0.35) and low effective extraction despite moderate power.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint has not resolved its mandatrophy because the original mandate (efficient parking enforcement) persists alongside the degraded function (vendor dependency management). The mandate is still live — municipalities need parking enforcement — but the implementation has accumulated extractive overhead that exceeds the coordination benefit for some agents. The mandatrophy would resolve if either: (1) open-source alternatives mature to the point where vendor lock-in becomes obviously unnecessary (scaffold sunset), or (2) municipalities recognize that the enforcement function can be achieved through less extractive means (permit systems, meter readers, open-source LPR) and the vendor lock-in is revealed as pure extraction. The current state is mandatrophy-in-progress: the coordination function is real but the extraction is accumulating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    open_source_viability,
    'Can open-source LPR systems achieve feature parity and reliability comparable to proprietary systems at municipal scale?',
    'Comparative deployment studies: accuracy rates, maintenance costs, and operational reliability of open-source vs proprietary systems across municipalities of varying size',
    'If viable: scaffold perspective confirmed — open-source sunset is real and vendor lock-in is temporary. If not viable: lock-in is structural rather than contingent, and the piton classification understates the constraint''s persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_viability, empirical, 'Whether open-source LPR can match proprietary system performance').

omega_variable(
    data_portability_threshold,
    'What level of data format standardization would enable municipalities to switch vendors without prohibitive migration costs?',
    'Cost-benefit analysis of standardization mandates; identification of minimum interoperability requirements that preserve vendor competition while enabling municipal exit',
    'If low threshold: regulatory intervention could break lock-in cheaply (scaffold logic). If high threshold: lock-in is deeply structural and regulatory solutions are prohibitively expensive (snare logic persists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_portability_threshold, empirical, 'Standardization requirements for vendor switching feasibility').

omega_variable(
    surveillance_function_creep,
    'Is LPR infrastructure deployment primarily driven by parking enforcement needs or by broader surveillance objectives using parking as political cover?',
    'Analysis of LPR data usage patterns: proportion of queries for parking enforcement vs other law enforcement purposes; correlation between deployment advocacy and surveillance policy agendas',
    'If parking-driven: the constraint is primarily about vendor lock-in (piton/tangled_rope). If surveillance-driven: parking enforcement is cover for a snare targeting civil liberties, and the extractiveness is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surveillance_function_creep, empirical, 'Whether parking enforcement is primary function or cover story').

omega_variable(
    cs_framing_ambiguity,
    'Is the kernel ''curb space legitimacy'' or the authority structure ''municipal legislative power over public resources''?',
    'Examination of which element is stabilized and which absorbs drift: if curb allocation rules change frequently but municipal authority remains constant, the authority is the kernel. If authority is contested but allocation rules are stable, the rules are the kernel.',
    'If kernel is authority structure: the readings are about WHO decides, not WHAT is decided, and the commitment system is about legitimacy of municipal power itself. If kernel is allocation rules: the readings are about WHAT rules are legitimate, and municipal authority is the interpretive layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_ambiguity, conceptual, 'Whether the kernel is the allocation rules or the authority structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(enforcement_technology_lock_in, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lpr_lock_tr_t0, enforcement_technology_lock_in, theater_ratio, 0, 0.25).
narrative_ontology:measurement(lpr_lock_tr_t3, enforcement_technology_lock_in, theater_ratio, 3, 0.38).
narrative_ontology:measurement(lpr_lock_tr_t6, enforcement_technology_lock_in, theater_ratio, 6, 0.48).
narrative_ontology:measurement(lpr_lock_tr_t9, enforcement_technology_lock_in, theater_ratio, 9, 0.55).
narrative_ontology:measurement(lpr_lock_tr_t12, enforcement_technology_lock_in, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(lpr_lock_be_t0, enforcement_technology_lock_in, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lpr_lock_be_t3, enforcement_technology_lock_in, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(lpr_lock_be_t6, enforcement_technology_lock_in, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(lpr_lock_be_t9, enforcement_technology_lock_in, base_extractiveness, 9, 0.65).
narrative_ontology:measurement(lpr_lock_be_t12, enforcement_technology_lock_in, base_extractiveness, 12, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lpr_lock_su_t0, enforcement_technology_lock_in, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(lpr_lock_su_t3, enforcement_technology_lock_in, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(lpr_lock_su_t6, enforcement_technology_lock_in, suppression_requirement, 6, 0.63).
narrative_ontology:measurement(lpr_lock_su_t9, enforcement_technology_lock_in, suppression_requirement, 9, 0.7).
narrative_ontology:measurement(lpr_lock_su_t12, enforcement_technology_lock_in, suppression_requirement, 12, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(enforcement_technology_lock_in, enforcement_mechanism).
narrative_ontology:affects_constraint(enforcement_technology_lock_in, residential_permit_pricing).
narrative_ontology:affects_constraint(enforcement_technology_lock_in, curb_space_allocation_rules).
narrative_ontology:affects_constraint(enforcement_technology_lock_in, municipal_data_sovereignty).

% DUAL FORMULATION NOTE:
% The enforcement technology lock-in is structurally distinct from the curb space allocation rules it enforces. The allocation rules (property_tax_entitlement vs public_resource_pricing vs equity_redistribution) have their own extractiveness values reflecting the distributional consequences of different pricing regimes. The enforcement technology lock-in has its own extractiveness reflecting vendor capture and surveillance expansion. These are separate constraints linked through the network: the choice of allocation rule determines the demand for enforcement infrastructure, which creates the opportunity for vendor lock-in.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(enforcement_technology_lock_in, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
