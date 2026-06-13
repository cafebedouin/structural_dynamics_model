% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__corporate_moat_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
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
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive Open-Source License as Corporate Proprietary Moat
 *   domain: intellectual_property/software_governance
 *
 * SUMMARY:
 *   Permissive open-source licenses (MIT, Apache 2.0, BSD) remove legal
 *   friction from code reuse, enabling rapid ecosystem innovation. This
 *   reading frames the license as a mechanism that ENABLES corporate
 *   extraction: enterprises incorporate volunteer-maintained code into
 *   proprietary products, capture market rents, and provide no reciprocal
 *   contribution or compensation. The maintainers are victimized not by
 *   malice but by the asymmetric structure the license creates—they chose to
 *   contribute to the commons believing in collective benefit, but the
 *   permissive terms allow corporations to defect from commons maintenance
 *   while retaining the value of the contributed code. The extraction is
 *   active (corporations enforce proprietary derivative secrecy) and
 *   structural (the license itself contains no reciprocity requirement).
 *
 * KEY AGENTS:
 *   - enterprise_software_corporations: Primary beneficiary. Extract uncompensated value from permissively-licensed code by incorporating it into proprietary products, capturing market rents, and maintaining code secrecy. Institutional power, arbitrage-grade exit options.
 *   - individual_open_source_maintainers: Primary victims. Contribute code under the permissive license, then discover it incorporated into proprietary competitors without compensation or credit. Powerless individually; identity-locked to open-source values (cannot easily switch to copyleft without fragmenting their own user base). Biographical time horizon; constrained exit.
 *   - alternative_open_source_projects: Secondary victims. Compete with proprietary derivatives of permissively-licensed code. Corporations can undercut them by adding proprietary layers and enterprise support. Moderate power; constrained exit (switch to copyleft = break compatibility).
 *   - downstream_users: Beneficiary (low friction to integration) but also secondary payers (corporations pass along some proprietary-derivative costs). Organized power; mobile exit options.
 *   - open_source_stewardship_bodies: Agenda-setter. Chose permissive licensing over copyleft or other models. Constrained by need to maintain legitimacy with both volunteer and corporate communities. Their institutional choice shapes the extraction landscape.
 *   - venture_capital_backed_startups: Beneficiary-payer hybrid. Use permissive code as foundation, then go proprietary or get acquired. Powerful players with arbitrage-grade exit (can exit to acquisition).
 *   - academic_researchers: Observer seat. Study the extraction dynamics and document the sustainability crisis in volunteer-maintained projects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.68).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.52).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive Open-Source License as Corporate Proprietary Moat").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "intellectual_property/software_governance").

domain_priors:requires_active_enforcement(permissive_license_text__corporate_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, 'd3327e7b-deaa-47b0-a256-7247ecc71a23').
narrative_ontology:cs_kernel_codification('d3327e7b-deaa-47b0-a256-7247ecc71a23', fixed_text).
narrative_ontology:cs_authority_grounding('d3327e7b-deaa-47b0-a256-7247ecc71a23', extraction).
narrative_ontology:cs_reading_relation('d3327e7b-deaa-47b0-a256-7247ecc71a23', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('d3327e7b-deaa-47b0-a256-7247ecc71a23', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('d3327e7b-deaa-47b0-a256-7247ecc71a23', foundational, permissive_license_enables_uncompensated_corporate_extraction).
narrative_ontology:cs_axiom_status(permissive_license_enables_uncompensated_corporate_extraction, holdable).
narrative_ontology:cs_axiom_grounding('d3327e7b-deaa-47b0-a256-7247ecc71a23', permissive_license_enables_uncompensated_corporate_extraction, empirically_contingent).
narrative_ontology:cs_axiom('d3327e7b-deaa-47b0-a256-7247ecc71a23', foundational, maintainer_victimhood_through_structural_asymmetry).
narrative_ontology:cs_axiom_status(maintainer_victimhood_through_structural_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('d3327e7b-deaa-47b0-a256-7247ecc71a23', maintainer_victimhood_through_structural_asymmetry, deontological).
narrative_ontology:cs_reference_frame('d3327e7b-deaa-47b0-a256-7247ecc71a23', permissive_license_maximum_reuse_freedom).
narrative_ontology:cs_drift_state('d3327e7b-deaa-47b0-a256-7247ecc71a23', contemporary_corporate_capture_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d3327e7b-deaa-47b0-a256-7247ecc71a23', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, enterprise_software_corporations).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, individual_open_source_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, alternative_open_source_projects).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__corporate_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__corporate_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) and rising because the constraint enables asymmetric value capture: enterprises take decades of volunteer labor and market it proprietary without reciprocal obligation. Suppression is moderate (0.52) because the constraint is defended not by overt coercion but by corporations' ability to maintain source-code secrecy (a structural suppression mechanism) and by the maintainers' identity-lock to open-source ideals (internalized suppression—they believe contributing to the commons is virtuous even when exploited). Theater is moderate-rising (0.22→0.41) because as awareness of the extraction grows, more rhetorical energy goes to justifying why permissive licensing is 'good for innovation' (performative maintenance of the narrative that voluntary uncompensated work is freedom, not exploitation). The measurement series track both the rising awareness of extraction and the intensifying performance required to sustain the narrative.
 *
 * PERSPECTIVAL GAP:
 *   From the corporate beneficiary seat, the constraint is genuine coordination—permissive licensing solved reuse friction and enabled rapid innovation that benefits everyone (downstream users, alternative projects, new startups). From the maintainer seat, the same structure operates as enforced extraction: their work fuels corporate moats they cannot compete with, yet they remain unpaid and ideologically committed to the 'open' framing that justifies their own exploitation. The engine computes this divergence per-seat from the structural data (beneficiary vs. victim, power differential, exit options). The claim ASSERTS snare; the metrics support it; the disagreement is between seats, not between claim and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Enterprises (institutional power, arbitrage-grade exit via acquisition or proprietary fork) sit near d=0.0 (full beneficiary end). Maintainers (powerless, identity-locked exit, biographical time horizon) sit near d=1.0 (full target end). The asymmetry is structural: corporations have multiple paths to value extraction (incorporation, proprietary layers, resale/acquisition) while maintainers have only one path (open contribution) and see little return. Alternative open-source projects sit mid-to-high on d (d≈0.7) because they face competitive pressure from corporate derivatives but retain some exit options (copyleft migration, commercialization, shutdown). Downstream users sit near d=0.4 (moderate beneficiary: they gain reuse friction reduction but also carry some costs as proprietary derivatives inflate prices or reduce feature parity with open versions).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does NOT show mandatrophy. The founding problem (legal friction in reuse) remains real and is solved by permissive licensing. However, the reading reveals that solving the reuse problem enabled an extraction problem the founding mandate never intended. This is not mandatrophy (founding problem dead, constraint persists) but rather MISSION CREEP: the constraint's success at solving reuse friction enabled new structural asymmetries that now drive extraction independent of the founding mandate. A mandatrophy reading would require the original problem to be dead (it is not—reuse friction remains a real coordination problem). The extraction reading does NOT require mandatrophy; it requires structural asymmetry in who benefits from the solution (it is present).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_persistence,
    'Is the measured suppression of maintainer resistance structural (corporations'' ability to maintain proprietary secrecy, legal barriers to tracking use) or internalized (maintainers'' identity-fusion with open-source ideals, belief that extraction is fair exchange for ''freedom'')?',
    'Post-extraction-removal tracking: if permissive licensing were replaced with mandatory reciprocity, do maintainers'' motivation and activism levels rise (indicating suppression was internalized), or do they persist at current levels (indicating suppression is structural to the market dynamics)? Secondary mechanism: ethnographic study of maintainer exit reasoning—are they leaving because of economic pressure (structural) or because they feel ideologically complicit (internalized)?',
    'If suppression is internalized, removing the permissive constraint alone would not restore maintainer agency—the identity lock persists. If structural, restoring reciprocity rights would quickly restore resistance and renegotiation power. The difference shapes what remedy is needed (compensation/copyleft vs. therapy/identity work).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_persistence, empirical, 'Structural vs. internalized suppression in open-source maintainer dynamics').

omega_variable(
    commons_vs_moat_frame_rivalry,
    'Is permissive licensing primarily a commons-coordination mechanism (maximizing collective benefit through legal simplicity) or a corporate-moat mechanism (enabling uncompensated extraction)? Can both readings coexist for the same constraint, or does one reading''s truth preclude the other?',
    'Comparative analysis: measure maintainer satisfaction and ecosystem sustainability under permissive vs. copyleft licensing in matched domains. If both model permissive as generative (commons reading) and permissive as extractive (moat reading) simultaneously, the readings coexist; if the data shows one model dominant, that reading forecloses the other''s core claim.',
    'If readings coexist, the kernel is genuinely contested and both constraints are valid. If one forecloses the other, the losing reading''s core premise is unsustainable (empirically or logically). This shapes which reading the engine should weight for remediation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_vs_moat_frame_rivalry, conceptual, 'Whether commons-coordination and corporate-extraction readings can both describe the same permissive-license constraint').

omega_variable(
    volunteering_authenticity_and_exit,
    'When individual maintainers contribute code under permissive licenses, are they genuinely volunteering (informed consent to uncompensated work) or are they making a choice under constrained conditions (no institutional funding, no viable alternative for getting code used, identity-bound to open-source beliefs)?',
    'Historical counterfactual: if maintainers had been offered compensation (via bounties, sponsorship, or GPL-enforced reciprocity) at the time of initial contribution, would the contribution patterns differ? Survey of maintainers: when they started, did they understand the permissive license enabled proprietary use, and did they consent to that outcome?',
    'If contributions were made under constraint or with limited information, the framing shifts from ''free choice to volunteer'' to ''structured coercion into volunteering.'' This reframes the constraint from snare (victims aware, constrained exit) to something closer to structural coercion. If they were fully informed and chose voluntarily, the snare reading holds but the victim characterization weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(volunteering_authenticity_and_exit, empirical, 'Whether open-source contributions are genuine volunteering or constrained choices').

omega_variable(
    alternative_kernel_reading_feasibility,
    'Could the permissive-license kernel be instantiated WITHOUT enabling corporate extraction (the commons-coordination reading)? Or is corporate-moat extraction a necessary structural consequence of removing legal friction from reuse?',
    'Thought experiment: design a permissive-license framework that removes reuse friction BUT includes mechanisms to prevent uncompensated extraction (e.g., attribution requirements, mandatory contribution paths, tiered licensing). If such a framework is logically coherent and enables reuse, the moat reading is not foreclosed by the permissive premise; if such a framework collapses back into either copyleft (adds reciprocity) or extraction (loses reuse friction), the readings may be foreclosing competitors.',
    'If a middle path exists, the readings coexist and both are structurally viable. If no middle path exists (remove friction → enable extraction, or prevent extraction → recreate friction), the readings may be logically in tension.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_kernel_reading_feasibility, conceptual, 'Whether permissive-license commons-coordination and corporate-moat extraction are logically separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__corporate_moat_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__corporate_moat_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__corporate_moat_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__corporate_moat_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__corporate_moat_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(perm_tr_t25, permissive_license_text__corporate_moat_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__corporate_moat_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__corporate_moat_reading, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__corporate_moat_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__corporate_moat_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__corporate_moat_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(perm_be_t25, permissive_license_text__corporate_moat_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__corporate_moat_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(perm_su_t5, permissive_license_text__corporate_moat_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(perm_su_t10, permissive_license_text__corporate_moat_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(perm_su_t15, permissive_license_text__corporate_moat_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement(perm_su_t20, permissive_license_text__corporate_moat_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(perm_su_t25, permissive_license_text__corporate_moat_reading, suppression_requirement, 25, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(permissive_license_text__corporate_moat_reading, 0.18).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__copyleft_counterfactual_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, software_labor_market_compensation_crisis).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'permissive_license_text'. Sibling readings are 'commons_coordination_reading' (permissive licensing maximizes collective freedom and ecosystem health) and 'copyleft_counterfactual_reading' (permissive licensing enables exploitation; viral reciprocity is the necessary alternative). The three readings instantiate the same kernel (the permissive-license framework) but from different structural seats: corporate beneficiaries see commons coordination; individual maintainers see extraction; copyleft advocates see foreclosure of reciprocity. Each reading is valid from its seat; the constraint family tracks the kernel's contestation across these structural positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(permissive_license_text__corporate_moat_reading, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
