% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Endogenous Displacement Reading of Practice Standardization Legitimacy
 *   domain: political_history/modernization_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the endogenous displacement reading of
 *   the contested kernel 'legitimacy of practice standardization.' In
 *   political history and modernization studies—exemplified by debates over
 *   dress, calendar, and legal reforms in twentieth-century
 *   nation-building—this reading holds that practice changes become
 *   legitimate when they emerge from voluntary adoption driven by perceived
 *   utility or cultural evolution. The constraint coordinates a progressive
 *   national narrative while extracting legitimacy from traditional authority
 *   holders and rural communities by reframing state-imposed reforms as
 *   organic cultural shifts.
 *
 * KEY AGENTS:
 *   - modernizing_elites: Primary beneficiary (powerful/mobile) — collects retrospective legitimacy for reform agendas
 *   - traditional_authority_holders: Primary target (moderate/identity_locked) — bears delegitimization of customary practice
 *   - rural_communities: Secondary target (powerless/constrained) — experiences coercion narrated as voluntary choice
 *   - state_historiography_institutions: Agenda setter (institutional/constrained) — enforces the endogenous narrative
 *   - modernization_theorists: Agenda setter (institutional/mobile) — establishes evaluative standards
 *   - critical_historians: Analytical observer (moderate/analytical) — documents the archival gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.55).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.58).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Endogenous Displacement Reading of Practice Standardization Legitimacy").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/modernization_studies").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__endogenous_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'b9c595b7-d72f-4eec-ac86-e3d100a3191f').
narrative_ontology:cs_kernel_codification('b9c595b7-d72f-4eec-ac86-e3d100a3191f', distributed).
narrative_ontology:cs_authority_grounding('b9c595b7-d72f-4eec-ac86-e3d100a3191f', distributed).
narrative_ontology:cs_reading_relation('b9c595b7-d72f-4eec-ac86-e3d100a3191f', legitimacy_of_practice_standardization__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('b9c595b7-d72f-4eec-ac86-e3d100a3191f', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('b9c595b7-d72f-4eec-ac86-e3d100a3191f', foundational, voluntary_adoption_confers_legitimacy).
narrative_ontology:cs_axiom_status(voluntary_adoption_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b9c595b7-d72f-4eec-ac86-e3d100a3191f', voluntary_adoption_confers_legitimacy, conventional).
narrative_ontology:cs_axiom('b9c595b7-d72f-4eec-ac86-e3d100a3191f', foundational, coercion_undermines_sustainable_transition).
narrative_ontology:cs_axiom_status(coercion_undermines_sustainable_transition, holdable).
narrative_ontology:cs_axiom_grounding('b9c595b7-d72f-4eec-ac86-e3d100a3191f', coercion_undermines_sustainable_transition, empirically_contingent).
narrative_ontology:cs_reference_frame('b9c595b7-d72f-4eec-ac86-e3d100a3191f', organic_modernization_framework).
narrative_ontology:cs_drift_state('b9c595b7-d72f-4eec-ac86-e3d100a3191f', post_revisionist_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b9c595b7-d72f-4eec-ac86-e3d100a3191f', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernization_theorists).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_authority_holders).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rural_communities).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, voluntarist_institutional_change).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and promulgate the theoretical framework that evaluates practice changes through adoption-curve evidence and utility-perception metrics. They train graduate students, referee publications, and establish what counts as rigorous explanation in modernization studies, collecting career prestige and grant funding from the paradigm.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernization_theorists, agenda_setter,
    institutional, generational, mobile, global).

% Derive political and historical legitimacy from the narrative that their reform agendas succeeded because populations found them useful, not because they were coerced. This insulation from charges of authoritarianism or foreign mimicry secures their retrospective standing and enables future reform claims.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_elites, beneficiary,
    powerful, biographical, mobile, national).

% Produce textbooks, museum exhibits, and official commemorations that frame calendar, dress, and legal reforms as organic national choices. Their institutional mandate requires presenting the nation as a coherent actor voluntarily modernizing, making acknowledgment of top-down coercion structurally difficult.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_historiography_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Preside over customary practices and adjudication roles that are delegitimized by the endogenous displacement narrative. Their displacement is reinterpreted as natural obsolescence rather than political defeat, foreclosing redress or recognition of their institutional destruction.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_authority_holders, payer,
    moderate, generational, identity_locked, regional).

% Subject to legal and administrative penalties for non-compliance with dress, calendar, and language reforms that official history later describes as their own voluntary cultural evolution. Their lived experience of coercion is archived out of the legitimacy narrative and their resistance is labeled temporary friction.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, rural_communities, payer,
    powerless, biographical, constrained, local).

% Uncover and publish archival records of state coercion, police enforcement, and administrative sanction behind supposedly voluntary reforms. They map the divergence between the endogenous narrative and the documentary record but face institutional resistance to reframing canonical national histories.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, critical_historians, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_elites).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__endogenous_displacement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a non-coercive narrative framework for understanding how societies transition from traditional to modern practices, reducing the perceived need for ongoing social conflict by interpreting change as organic, self-propelling, and nationally unified.
% TRANSFER_FUNCTION: Moves historical legitimacy and moral authority from traditional practice-holders and resistant rural communities to modernizing elites and the nation-state, by reframing state-imposed reforms as endogenous cultural evolution driven by perceived utility.
% ABSENT_VOICES: Local customary-court jurists and subaltern resistance leaders who experienced enforcement firsthand are absent from the official historiography; their testimony survives primarily in oral tradition and suppressed provincial archives, not in the national narrative or textbook canon.
% DISAPPEARANCE_RATIONALE: If the endogenous legitimacy framework vanished, national historiographies would require rewriting to acknowledge state coercion behind dress, calendar, and legal reforms; modernizing elites would lose retrospective insulation from authoritarian charges; and the academic field of modernization studies would lose a central explanatory paradigm.
% FOUNDING_PROBLEM: How to legitimize rapid institutional and cultural modernization without the permanent stain of authoritarian imposition or foreign mimicry; how to explain why traditional practices gave way to modern ones in a way that preserves national self-respect.
% FOUNDING_PROBLEM_CORROBORATION: Critical historians and subaltern studies scholars attest from outside the benefiting parties that the founding problem was addressed through coercion and later narrated as endogenous. Modernizing elites and state institutions attest the voluntary-adoption narrative from within the beneficiary set; no corroboration from outside the beneficiaries supports the pure voluntary-adoption claim.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate-to-high because the endogenous narrative systematically transfers legitimacy from traditional and rural seats to modernizing elites, erasing documented coercion. Suppression (0.58) reflects active narrative gatekeeping through textbook control, archive restriction, and academic hiring. Theater_ratio (0.45) captures the performative dimension: official histories stage voluntary adoption while provincial records show enforcement. Accessibility_collapse (0.50) is moderate because critical historiography survives in academic niches despite mainstream marginalization. Resistance (0.52) is moderate: traditional authorities resist delegitimization, and critical historians publish counter-evidence, but both operate against institutional headwinds. The metrics are authored independently of the tangled_rope claim; divergence would signal misclassification.
 *
 * PERSPECTIVAL GAP:
 *   From the modernizing elite and state historiography seats, the constraint appears as necessary nation-building coordination: without an endogenous narrative, reforms remain stained by authoritarianism and foreign imposition. From the traditional authority and rural community seats, the same constraint operates as extraction: their resistance is rewritten as friction, their displacement as obsolescence, and their archives are sealed. The modernization theorist seat experiences the constraint as genuine analytical framework; the critical historian seat experiences it as systematic distortion. The engine should compute strong seat divergence between beneficiaries and payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Modernizing elites are declared beneficiaries with mobile exit, placing them near d=0.0 (subsidized by the constraint's legitimacy transfer). Modernization theorists are declared beneficiaries with mobile exit, placing them near d=0.1. State historiography institutions have constrained exit as agenda setters, placing them at moderate-low d (they enforce but are also bound by the narrative). Traditional authority holders are declared victims with identity_locked exit, placing them near d=1.0 (full target). Rural communities are declared victims with constrained exit, also near the target end. Critical historians as observers with analytical exit are excluded from chi computation. The structural asymmetry between identity_locked traditional authorities and mobile modernizing elites drives high effective extraction for the former.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling this constraint as pure coordination (rope) by requiring declared victims and active enforcement, which captures the narrative gatekeeping and archive suppression that sustain the endogenous story against counter-evidence. It also prevents mislabeling as pure extraction (snare) by acknowledging the genuine coordination function: the endogenous narrative does reduce perceived social conflict and provides a coherent identity framework for modernizing societies. The mandatrophy risk here is obsolescence: as critical historiography accumulates, the founding problem (legitimizing modernization) may be dead, but the constraint persists through institutional inertia. Current measurements show theater_ratio below 0.5 and founding_problem_status contested rather than dead, so piton classification is not yet warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_voluntary_archival_gap,
    'To what extent were state-imposed practice changes (dress, calendar, language) actually coerced at the local level, and to what extent did voluntary adoption genuinely follow?',
    'Systematic provincial archival excavation and oral-history collection comparing enforcement records with adoption timelines.',
    'If coercion was widespread and sustained, the endogenous reading''s extractiveness is higher than its coordination function; if voluntary adoption dominated, the reading is closer to a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_voluntary_archival_gap, empirical, 'Archival gap between official endogenous narrative and local enforcement records').

omega_variable(
    nation_building_vs_historical_accuracy,
    'Does the endogenous displacement narrative serve as necessary nation-building coordination, or does it systematically distort historical understanding for elite benefit?',
    'Comparative analysis across multiple nation-states: do states with stronger endogenous narratives show higher social cohesion but poorer historical literacy?',
    'If primarily coordination, classification shifts toward rope; if primarily elite legitimation with distortion, classification shifts toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nation_building_vs_historical_accuracy, conceptual, 'Coordination function versus extraction through historical distortion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 24, 0.5).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 32, 0.48).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 40, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 32, 0.65).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% The kernel 'legitimacy_of_practice_standardization' decomposes into three structurally distinct readings: exogenous_override (state decree legitimates), endogenous_displacement (voluntary adoption legitimates), and dual_practice_equilibrium (domain-partitioned legitimacy). Each reading has a distinct epsilon, stakeholder structure, and classification. This story instantiates the endogenous_displacement reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
