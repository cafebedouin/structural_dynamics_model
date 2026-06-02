% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__facility_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__facility_constraint_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: caa_section_111d_delegation__facility_constraint_reading
 *   human_readable: CAA Section 111(d) Facility-Constraint Reading: EPA Delegation Limited to Individual Facility Improvements
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   Section 111(d) of the Clean Air Act directs EPA to establish emission
 *   standards for existing sources 'achievable by the best system of emission
 *   reduction.' The facility-constraint reading interprets this mandate as
 *   limited to measures implementable at individual facilities: heat-rate
 *   improvements, carbon capture and storage, efficiency upgrades. This
 *   reading protects the coal-generating sector from mandates to shift
 *   generation mix, retire aging facilities, or accelerate renewable
 *   procurement. The constraint operates at the intersection of statutory
 *   interpretation, administrative law, and constitutional delegation
 *   doctrine. The facility-constraint reading was embattled by the Obama-era
 *   Clean Power Plan (which attempted generation-shifting through state-level
 *   rate-based standards) and secured by the Trump-era Affordable Clean
 *   Energy rule, then upheld in principle by West Virginia v. EPA (2022),
 *   which held that generation-shifting cannot be mandated without clear
 *   statutory authorization. The facility-constraint reading is one pole of a
 *   deep interpretive contest within environmental law and constitutional
 *   doctrine. It has identifiable beneficiaries (coal sector, state
 *   autonomy), identifiable victims (climate emissions reduction objectives,
 *   renewable energy transition actors), and significant suppression
 *   (regulatory ceiling prevents EPA from using its most powerful leverage
 *   point). This story instantiates the facility-constraint reading as a
 *   tangled rope: genuine coordination functions exist (EPA standard-setting
 *   enables interstate coordination, efficiency improvements benefit multiple
 *   actors), but the coordination is intertwined with asymmetric extraction
 *   that shields incumbent industries and compresses climate action
 *   timelines.
 *
 * KEY AGENTS:
 *   - EPA (Environmental Protection Agency): Institutional actor (institutional/arbitrage within this reading) — regulatory authority constrained by statutory interpretation; can set facility-level standards but not mandate generation-shifting or accelerated retirement
 *   - Coal-Generating Sector: Primary beneficiary (institutional/arbitrage) — protected from forced retirement, generation-shifting mandates, or portfolio transformation; benefits from regulatory ceiling that preserves economic life of existing assets
 *   - State Energy Authorities: Secondary beneficiary (institutional/arbitrage) — preserve autonomy over energy-mix decisions; cannot be forced to mandate renewable procurement or coal retirement at pace EPA might prefer if authority were broader
 *   - Climate Emissions Reduction Objectives: Primary victim (powerless/trapped) — abstract collective goal that cannot organize; trapped within regulatory framework that makes meeting decarbonization timelines structurally difficult; no exit from facility-constraint ceiling
 *   - Renewable Energy and Decarbonization Coalition: Secondary victim (organized/constrained) — benefit from some facility-level efficiency standards but constrained by inability to mandate systemic generation-shifting; face compressed climate timeline with restricted leverage
 *   - Coal-Dependent Communities: Mixed victim (moderate/constrained) — benefit from near-term job protection through continued coal operations but face long-term stranding risk and lose opportunity for managed transition planning
 *   - Analytical Observer: Civilizational position (analytical/analytical) — constitutional delegation doctrine frames facility-constraint as natural law; false summit risk exists if doctrine is invoked to naturalize what is actually a contingent policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.58).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.62).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "CAA Section 111(d) Facility-Constraint Reading: EPA Delegation Limited to Individual Facility Improvements").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, 'd926bccb-f346-44ea-9b28-8fe45a8f27db').
narrative_ontology:cs_kernel_codification('d926bccb-f346-44ea-9b28-8fe45a8f27db', formalized).
narrative_ontology:cs_authority_grounding('d926bccb-f346-44ea-9b28-8fe45a8f27db', extraction).
narrative_ontology:cs_interpretation_layer_present('d926bccb-f346-44ea-9b28-8fe45a8f27db').
narrative_ontology:cs_reading_relation('d926bccb-f346-44ea-9b28-8fe45a8f27db', caa_section_111d_delegation__systemic_transformation_reading, forecloses).
narrative_ontology:cs_axiom('d926bccb-f346-44ea-9b28-8fe45a8f27db', foundational, delegation_limits_source_measure_scope).
narrative_ontology:cs_axiom_status(delegation_limits_source_measure_scope, holdable).
narrative_ontology:cs_axiom_grounding('d926bccb-f346-44ea-9b28-8fe45a8f27db', delegation_limits_source_measure_scope, deontological).
narrative_ontology:cs_axiom('d926bccb-f346-44ea-9b28-8fe45a8f27db', foundational, major_questions_doctrine_applies_to_generation_authority).
narrative_ontology:cs_axiom_status(major_questions_doctrine_applies_to_generation_authority, holdable).
narrative_ontology:cs_axiom_grounding('d926bccb-f346-44ea-9b28-8fe45a8f27db', major_questions_doctrine_applies_to_generation_authority, deontological).
narrative_ontology:cs_created_at('d926bccb-f346-44ea-9b28-8fe45a8f27db', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_generating_facilities).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, fossil_fuel_electricity_sector).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, state_energy_autonomy).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_emissions_reduction_targets).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_transition_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE REDUCTION TARGETS (SNARE) — Structural binding to facility-level measures only. Cannot mandate generation-shifting, coal retirement, or systemic energy-mix transformation. Trapped within a regulatory ceiling that makes meeting decarbonization timelines structurally impossible. No exit; full extraction of the atmospheric carbon budget without reciprocal reduction capacity.
constraint_indexing:constraint_classification(caa_section_111d_delegation__facility_constraint_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RENEWABLE ENERGY COALITION (TANGLED ROPE) — Organized actors benefit from some facility-level measures (efficiency standards, capture technologies drive innovation) but are constrained by inability to mandate accelerated coal retirement or generation-shifting. Experiences coordination function (efficiency improvements require coordinated standard-setting) alongside asymmetric extraction (climate deadline creates time pressure; facility-constraint rule caps their leverage). Constrained because they can work within the facility framework but cannot escape to systemic alternatives.
constraint_indexing:constraint_classification(caa_section_111d_delegation__facility_constraint_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COAL SECTOR / FOSSIL FUEL GENERATORS (ROPE) — Primary beneficiary. Facility-constraint rule protects assets from forced retirement, generation-shifting mandates, or accelerated portfolio transformation. Can implement heat-rate improvements and carbon capture (if economically advantageous) while avoiding systemic restructuring. Arbitrage: can choose which measures to adopt; protective regulatory ceiling allows continued operations. Experiences constraint as pure coordination: EPA regulates their efficiency, not their existence.
constraint_indexing:constraint_classification(caa_section_111d_delegation__facility_constraint_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE REGULATORY AUTONOMY (ROPE) — Preserves state authority over energy mix and generation portfolio composition. States can define what counts as the 'best system' for their jurisdictions; cannot be forced to mandate wholesale generation-shifting or rapid coal retirement. Benefits from regulatory ceiling that delegates systemic energy choices to state legislatures rather than centralizing in EPA standard-setting. Arbitrage: can choose implementation path within facility constraints; genuine coordination function exists (EPA + states jointly optimize within agreed bounds).
constraint_indexing:constraint_classification(caa_section_111d_delegation__facility_constraint_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: COAL-DEPENDENT COMMUNITIES (TANGLED ROPE) — Mixed position. Facility-constraint reading protects jobs and tax bases through continued coal operations (coordination benefit). But inability to mandate orderly sectoral transition creates long-term stranding risk — when climate pressure eventually forces systemic change, the transition becomes sudden and unprepared rather than gradual. Constrained: they benefit from near-term protection but lose opportunity for managed transition planning. The constraint is both stabilizing and precarious.
constraint_indexing:constraint_classification(caa_section_111d_delegation__facility_constraint_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CONSTITUTIONAL VIEW (MOUNTAIN) — From a civilizational/universal perspective grounded in constitutional law, the facility-constraint interpretation appears as a natural law: delegation doctrine structurally limits EPA authority to measures implementable at individual sources, not systemic generation-shifting (which would require legislative action). The nondelegation principle and major questions doctrine are presented as immutable constraints on administrative rulemaking. However, structural data (identifiable beneficiaries, suppression mechanism, asymmetric extraction) suggests this is a contested reading, not a natural law. The engine will compute this as a false summit: constitutional doctrine is being invoked to naturalize a contestable policy choice.
constraint_indexing:constraint_classification(caa_section_111d_delegation__facility_constraint_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(caa_section_111d_delegation__facility_constraint_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(caa_section_111d_delegation__facility_constraint_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The facility-constraint reading creates measurable asymmetry: coal sector can continue operations, state autonomy is protected, but climate reduction objectives are constrained to measures with limited aggregate impact. The extraction is not maximal (efficiency measures do reduce emissions; coordination functions exist for state-level standard-setting) but is substantial (generational-scale decarbonization requires systemic change; facility-only measures will not meet 1.5°C targets). The 0.58 value reflects that this is hybrid coordination-extraction, not pure extraction. Suppression (0.62): Moderate-high. Regulatory ceiling is explicit and enforced through doctrine (major questions interpretation prevents EPA from asserting generation-shifting authority). The suppression is not maximal because states retain authority to mandate renewable procurement or coal retirement through their own legislation (Exit not absolutely blocked, merely channeled away from EPA). Theater ratio (0.68): Moderate-high. Facility-level measures generate regulatory performance metrics (efficiency improvements, carbon capture deployment) that create appearance of progress on climate while structural decarbonization remains blocked. The theater increases over the interval as the gap between facility-only measures and emissions reduction targets becomes more obvious, yet the facility-constraint reading persists through constitutional doctrine invocation (performative citation of major questions principle). Theater rises from 0.55 to 0.68 as the contradiction between claimed climate commitment and actual systemic inaction becomes more visible.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival range is exceptional and diagnostic. Coal generators see pure coordination (Rope) — they coordinate with EPA on efficiency standards; the constraint is solving a problem they face (how to operate profitably under emissions pressure). State authorities see pure coordination (Rope) — they coordinate with EPA on implementable standards and retain energy-mix autonomy. The renewable coalition sees mixed coordination-extraction (Tangled Rope) — efficiency coordination exists but generation-shifting is blocked. Climate objectives see pure extraction (Snare) — the facility constraint creates structural impossibility for meeting targets with no countervailing coordination benefit. The analytical observer risks seeing constitutional law as a natural law (Mountain) — delegation doctrine presented as immutable — but beneficiary presence and suppression mechanism indicate false summit: constitutional doctrine is being invoked to naturalize a contestable policy choice. The gap between the beneficiary's 'this is just coordination' and the victim's 'this is an impossible extraction' is the full range of DR classification for a single constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's effective extractiveness (χ) is computed from base extractiveness (ε = 0.58), agent power level, exit options, and derived directionality value (d). The coal sector (beneficiary + arbitrage) experiences low or negative effective extraction — they are net winners of the constraint. Climate objectives (victim + trapped) experience maximum effective extraction — they have no exit and bear full cost of the regulatory ceiling. State authorities (beneficiary + arbitrage) experience low effective extraction — they benefit from preserved autonomy. The renewable coalition (victim + organized + constrained) experiences moderate effective extraction — they have some agency and can work within efficiency frameworks, but cannot escape to systemic alternatives. The temporal trajectory shows extractiveness rising over the measurement interval (0.42 → 0.58) as the gap between emissions targets and facility-only measures widens; this accumulation reflects that initial implementation (heat-rate improvements) seemed plausible as a pathway, but as decarbonization deadlines approach and the sufficiency gap becomes undeniable, the extraction mechanism becomes more visible and more severe.
 *
 * MANDATROPHY ANALYSIS:
 *   The facility-constraint reading avoids mandatrophy through its hybrid classification (tangled_rope, not pure snare) by identifying genuine coordination functions: EPA-state standard-setting coordination for efficiency measures, voluntary facility-level carbon capture deployment, state-level energy planning. However, the mandatrophy is latent: if empirical analysis (omega variable 3) determines that facility-only measures are insufficient to meet decarbonization targets, the classification under the facility-constraint reading becomes pure snare (extraction without coordination benefit). The resolution is not to correct the classification but to recognize that the facility-constraint reading itself produces mandatrophy — it creates a constraint type (tangled rope with high suppression) that forecloses its own coordination objective (systemic decarbonization). The mandatrophy is irresolvable within this reading without moving to the systemic_transformation_reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    best_system_scope_ambiguity,
    'Does ''best system of emission reduction'' encompass only facility-level operational improvements, or does it include systemic changes to generation portfolio, energy-mix transformation, and demand-side measures?',
    'Statutory text analysis (CAA § 111(d)); legislative history and committee intent; prior EPA interpretations before West Virginia v. EPA (2022); comparative regulatory regimes (EU ETS, state renewable portfolio standards)',
    'If facility-only: coal sector protected, climate timeline compressed, state autonomy preserved — this reading''s structural logic holds. If systemic: EPA gains generation-shifting authority, coal retirement accelerated, systemic transformation enabled — systemic_transformation_reading becomes operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(best_system_scope_ambiguity, conceptual, 'Semantic scope of ''best system of emission reduction'' in Section 111(d)').

omega_variable(
    major_questions_doctrine_application,
    'Is EPA''s authority to mandate generation-shifting (coal retirement, renewable procurement mandates) a ''major question'' requiring clear Congressional authorization, or a routine exercise of delegated rulemaking power?',
    'Analysis of West Virginia v. EPA (2022) major questions test; comparison with precedents establishing clear Congressional authorization thresholds (Clean Air Act § 112, § 204); examination of statutory language and EPA''s historical interpretation',
    'If major question: facility-constraint reading is constitutionally mandated, not merely prudential. If routine delegation: EPA''s silence on generation-shifting authority is interpretive choice, not constitutional limit. Structural consequence: false summit credibility rises or falls with doctrine application.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(major_questions_doctrine_application, conceptual, 'Whether generation-shifting mandates trigger major questions doctrine').

omega_variable(
    temporal_feasibility_of_systemic_transition,
    'Given carbon budgets consistent with 1.5°C warming targets, is facility-level optimization sufficient to meet decarbonization timelines, or is systemic generation-shifting structurally necessary?',
    'Comparative analysis: maximum feasible heat-rate improvements + carbon capture deployment trajectories vs. emissions pathway modeling for Paris Agreement scenarios; identification of cumulative gap between facility-only measures and target reduction rates',
    'If facility-only sufficient: climate extraction is overstated; renewable coalition has structural pathway to success through efficiency + innovation. If systemic necessary: facility-constraint reading creates structural impossibility; extractive asymmetry is irremediable without reading change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_feasibility_of_systemic_transition, empirical, 'Whether facility-level measures suffice for 1.5°C decarbonization timelines').

omega_variable(
    commonwealth_v_epa_precedent_scope,
    'Did West Virginia v. EPA (2022) foreclose the systemic_transformation_reading, or merely establish that EPA must articulate clear statutory basis before mandating generation-shifting?',
    'Textual analysis of West Virginia holding and reasoning; examination of whether statutory amendment could revive generation-shifting authority; assessment of whether Chief Justice Roberts'' major questions reasoning depends on specific delegation gaps or on structural limits to administrative power',
    'If West Virginia forecloses systemic reading: this facility-constraint reading is the only viable interpretation within current constitutional law. If statutory amendment possible: systemic reading remains live, merely awaiting Congressional action. Determines whether the readings coexist_with or facility-reading forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commonwealth_v_epa_precedent_scope, conceptual, 'Whether West Virginia v. EPA forecloses systemic generation-shifting interpretation').

omega_variable(
    state_autonomy_vs_federal_coordination,
    'Is state-level energy autonomy a genuine coordination benefit (different states optimize for different conditions, decentralized experimentation) or cover for regulatory capture (states protect incumbent industries from federal pressure)?',
    'Empirical comparison: correlation between state autonomy and renewable energy adoption rates; case studies of state energy policy divergence; measurement of coordination benefits vs capture overhead',
    'If genuine coordination: state autonomy perspective (Rope) is structurally correct. If capture mechanism: state autonomy serves as extraction cover; classification shifts toward Snare. Changes whether beneficiary designation is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_autonomy_vs_federal_coordination, empirical, 'Whether state autonomy in energy policy produces coordination benefits or enables capture').

omega_variable(
    false_summit_natural_law_ambiguity,
    'Is the facility-constraint interpretation of Section 111(d) a natural law of constitutional delegation doctrine (an immutable limit on EPA authority), or a contested reading that benefits identifiable actors and can be revised through statutory amendment or doctrinal evolution?',
    'Historical analysis of constitutional delegation doctrine and nondelegation principle; examination of whether major questions doctrine is stable legal principle or contextual application; assessment of whether facility-constraint reading depends on West Virginia v. EPA (contingent) or on timeless constitutional limits (immutable)',
    'If natural law: facility-constraint reading is structurally inevitable; systemic_transformation_reading is constitutionally foreclosed. If contested reading: facility-constraint reading is one of multiple coherent interpretations; beneficiary protection is contingent, revisable. Determines mountain vs tangled_rope classification credibility and false summit detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_ambiguity, conceptual, 'Whether facility-constraint interpretation is immutable constitutional law or contingent doctrinal choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa111d_fac_tr_t0, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(caa111d_fac_tr_t5, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 5, 0.62).
narrative_ontology:measurement(caa111d_fac_tr_t10, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(caa111d_fac_be_t0, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(caa111d_fac_be_t5, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(caa111d_fac_be_t10, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(caa111d_fac_su_t0, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(caa111d_fac_su_t5, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(caa111d_fac_su_t10, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation__systemic_transformation_reading).

% DUAL FORMULATION NOTE:
% The facility-constraint reading and systemic_transformation_reading are sibling readings of a single contested kernel (caa_section_111d_delegation) rather than separate constraints in a family. Both stories must be populated to represent the interpretive contest. The facility-constraint reading (this file) has ε=0.58 (tangled rope); the systemic reading will have lower ε (closer to rope or scaffold) by virtue of having lower suppression and higher coordination benefit. The network linkage is bidirectional — each reading's story cites the other in affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
