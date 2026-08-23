% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Dual Practice Equilibrium: Domain-Partitioned Legitimacy
 *   domain: political_history/modernization/institutional_change
 *
 * SUMMARY:
 *   In processes of state formation and colonial modernization, a stable
 *   equilibrium often emerges in which the state claims authority over
 *   public, administrative, and fiscal domains (enforcing Gregorian
 *   calendars, Western dress codes, bureaucratic language) while traditional
 *   authorities retain control over private, ritual, and agricultural domains
 *   (lunar festivals, kinship rites, home attire). This constraint story
 *   captures the dual-practice equilibrium reading of the
 *   legitimacy-of-practice-standardization kernel: legitimacy is
 *   domain-partitioned, compliance is strategic rather than internalized, and
 *   no convergence is expected. The constraint coordinates state and
 *   traditional elites by preventing direct conflict, while extracting
 *   compliance costs from practitioners who must maintain dual repertoires.
 *
 * KEY AGENTS:
 *   - state_authority: Primary agenda-setter in public/administrative domain (institutional/constrained)
 *   - traditional_authority: Primary agenda-setter in private/ritual domain (organized/constrained)
 *   - dual_practitioners: Primary targets â bear material and cognitive costs of compartmentalized compliance (powerless/constrained)
 *   - integrationist_reformers: Excluded voices advocating unified practice (moderate/constrained)
 *   - institutional_historians: Analytical observers documenting the equilibrium (analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.55).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.45).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Dual Practice Equilibrium: Domain-Partitioned Legitimacy").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/modernization/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'c69732d6-21b4-4f2f-a7ea-a539a0d951f6').
narrative_ontology:cs_kernel_codification('c69732d6-21b4-4f2f-a7ea-a539a0d951f6', distributed).
narrative_ontology:cs_authority_grounding('c69732d6-21b4-4f2f-a7ea-a539a0d951f6', distributed).
narrative_ontology:cs_reading_relation('c69732d6-21b4-4f2f-a7ea-a539a0d951f6', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('c69732d6-21b4-4f2f-a7ea-a539a0d951f6', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('c69732d6-21b4-4f2f-a7ea-a539a0d951f6', foundational, practice_legitimacy_is_domain_specific).
narrative_ontology:cs_axiom_status(practice_legitimacy_is_domain_specific, holdable).
narrative_ontology:cs_axiom_grounding('c69732d6-21b4-4f2f-a7ea-a539a0d951f6', practice_legitimacy_is_domain_specific, conventional).
narrative_ontology:cs_axiom('c69732d6-21b4-4f2f-a7ea-a539a0d951f6', foundational, state_traditional_boundary_is_mutually_reinforcing).
narrative_ontology:cs_axiom_status(state_traditional_boundary_is_mutually_reinforcing, holdable).
narrative_ontology:cs_axiom_grounding('c69732d6-21b4-4f2f-a7ea-a539a0d951f6', state_traditional_boundary_is_mutually_reinforcing, conventional).
narrative_ontology:cs_reference_frame('c69732d6-21b4-4f2f-a7ea-a539a0d951f6', domain_partitioned_equilibrium).
narrative_ontology:cs_drift_state('c69732d6-21b4-4f2f-a7ea-a539a0d951f6', modernization_encounter, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c69732d6-21b4-4f2f-a7ea-a539a0d951f6', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_authority).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, dual_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the standards for public administration, taxation, and official ceremony. Enforces Gregorian calendar use, Western dress codes in bureaucratic settings, and official language policy. Collects predictable tax revenue and claims symbolic monopoly over public time and bureaucratic procedure.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_authority, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_authority, beneficiary).

% Sets and guards the norms for ritual, agricultural, and kinship domains. Enforces lunar calendar observance for festivals, traditional dress for rites, and customary law for marriage and inheritance. Retains deference, ritual offerings, and social standing tied to sacred and domestic life.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority, beneficiary).

% Navigate both domains daily: file taxes by the Gregorian calendar and plant or celebrate by the lunar one; wear Western suits to government offices and courts, then change into traditional clothing for home, temple, or village ceremony. Bear the material cost of dual wardrobes and the cognitive load of switching codes, treating each repertoire as context-specific rather than personally authentic.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, dual_practitioners, payer,
    powerless, biographical, constrained, national).

% Advocate for a single unified national cultureâwhether through complete modernist assimilation or complete traditional restorationâand are systematically excluded from the policymaking table because the equilibrium explicitly rejects convergence in favor of compartmentalization.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, integrationist_reformers, excluded,
    moderate, generational, constrained, national).

% Document the emergence and persistence of dual-practice arrangements across empires, colonial administrations, and post-colonial states, comparing cases where the domain partition held and cases where it collapsed into unitary authority.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, institutional_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents direct conflict between state and traditional authorities by partitioning domains of control, allowing both to claim legitimacy without contesting the same practices.
% TRANSFER_FUNCTION: Moves compliance labor, material resources, and symbolic deference from practitioners to state and traditional authorities according to the governing domain.
% ABSENT_VOICES: Integrationist reformers who reject compartmentalization in favor of unified national culture, and subaltern groups whose practices fit neither public administrative nor private ritual categories.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished, state and traditional authorities would confront each other directly over calendars, dress, and ritual; practitioners would face conflicting demands rather than compartmentalized ones; the social equilibrium that avoids open conflict would collapse.
% FOUNDING_PROBLEM: How to extend state authority over public administration and taxation without provoking total resistance from embedded traditional institutions and their constituencies.
% FOUNDING_PROBLEM_CORROBORATION: Administrative historians document the fiscal and military imperatives that drove state expansion; traditional authorities attest the threat to ritual autonomy that the partition was designed to avert. Independent ethnographers corroborate the cost burden on practitioners, attesting from outside the beneficiary set.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects the substantial but not total extraction from practitioners: dual wardrobes, calendar literacy, and cognitive switching are real costs, but the arrangement avoids the higher costs of total war or forced assimilation. Suppression (0.45) is moderate: each authority enforces its domain, but enforcement is primarily boundary-maintenance rather than totalizing. Theater ratio (0.55) is comparatively high because compliance is explicitly strategic â the suit is worn for the state, the kimono for the temple, with no expectation that either reflects authentic identity. Accessibility collapse (0.40) is moderate: alternatives (using lunar dates for taxes, wearing ritual dress to court) are possible but penalized. Resistance (0.35) is moderate-low because the equilibrium offers something to both major authorities and most practitioners prefer predictable compartmentalization to unpredictable conflict.
 *
 * PERSPECTIVAL GAP:
 *   State and traditional authorities experience the constraint as coordination: it secures their respective spheres and prevents direct challenges. Practitioners experience it as extraction: they pay the material and cognitive costs of dual repertoires. The engine should compute a wide divergence between agenda-setter seats and payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   State_authority and traditional_authority are beneficiaries with low directionality (subsidized by the constraint's stability). Dual_practitioners are victims with high directionality (targeted by both domains' demands). Integrationist_reformers are excluded but would be targets if included. The asymmetry is structural: the same arrangement that coordinates elites compartmentalizes subalterns.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading prevents mislabeling by requiring both coordination and extraction. A pure snare reading would ignore the genuine conflict-prevention function that benefits both state and traditional authorities. A pure rope reading would ignore the asymmetric cost burden on practitioners. The tangled_rope classification captures that the equilibrium is functional but not free.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'This constraint is the dual_practice_equilibrium_reading of kernel legitimacy_of_practice_standardization. How does its structural classification differ from sibling readings?',
    'Compare across the three reading files: endogenous_displacement_reading locates legitimacy in voluntary utility-driven adoption; exogenous_override_reading locates it in state decree; this reading locates it in domain-partitioned authority.',
    'Determines whether the constraint is read as coordination (equilibrium), evolution (displacement), or extraction (override).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Reading position within the legitimacy_of_practice_standardization kernel').

omega_variable(
    equilibrium_stability,
    'Is the domain-partitioned dual practice a durable equilibrium, or a transient phase before one authority displaces the other?',
    'Longitudinal historical comparison: if dual practice persists across three or more generations without convergence, equilibrium; if it collapses into unitary state or traditional authority, transient.',
    'If transient, the constraint is scaffold-like; if durable, the tangled_rope classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equilibrium_stability, empirical, 'Durability of the state-traditional domain partition').

omega_variable(
    compliance_internality,
    'Does strategic (non-internalized) dual compliance eventually produce identity fusion or persistent cognitive burden that raises effective extraction?',
    'Ethnographic study of practitioner identity over biographical time: whether compartmentalization becomes a stable habitus or a continuous source of stress.',
    'If internalized, the constraint shifts toward identity_coordination with lower theater_ratio; if persistently strategic, theater_ratio remains high and extraction is experienced as ongoing performance tax.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_internality, empirical, 'Strategic compliance vs internalized identity under dual practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 50, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is the dual_practice_equilibrium_reading of the legitimacy_of_practice_standardization kernel. Sibling readings (endogenous_displacement_reading, exogenous_override_reading) instantiate different constraints from the same contested kernel. They are linked as a constraint family under the Îµ-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
