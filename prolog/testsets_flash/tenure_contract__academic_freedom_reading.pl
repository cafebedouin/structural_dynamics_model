% ============================================================================
% CONSTRAINT STORY: tenure_contract__academic_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__academic_freedom_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: tenure_contract__academic_freedom_reading
 *   human_readable: Academic Freedom via Tenure Contract
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint describes the tenure contract in higher education as a
 *   mechanism for protecting academic freedom. From this reading, tenure
 *   decouples a researcher's survival from institutional or political
 *   pressures, thereby enabling high-risk, critical, and truth-seeking
 *   inquiry. It is presented as a coordination mechanism that benefits
 *   society by ensuring robust intellectual discourse and knowledge
 *   production, even when inconvenient to powerful actors. The metrics
 *   reflect this ideal-type operation, with low extraction and suppression,
 *   primarily targeting external pressures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.2).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.1).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Academic Freedom via Tenure Contract").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__academic_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, 'c30b2c34-2544-48f7-9282-65d0bebd5bff').
narrative_ontology:cs_kernel_codification('c30b2c34-2544-48f7-9282-65d0bebd5bff', formalized).
narrative_ontology:cs_authority_grounding('c30b2c34-2544-48f7-9282-65d0bebd5bff', lineage).
narrative_ontology:cs_interpretation_layer_present('c30b2c34-2544-48f7-9282-65d0bebd5bff').
narrative_ontology:cs_reading_relation('c30b2c34-2544-48f7-9282-65d0bebd5bff', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('c30b2c34-2544-48f7-9282-65d0bebd5bff', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('c30b2c34-2544-48f7-9282-65d0bebd5bff', foundational, intellectual_independence_is_foundational).
narrative_ontology:cs_axiom_status(intellectual_independence_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('c30b2c34-2544-48f7-9282-65d0bebd5bff', intellectual_independence_is_foundational, deontological).
narrative_ontology:cs_axiom('c30b2c34-2544-48f7-9282-65d0bebd5bff', foundational, truth_seeking_requires_protection).
narrative_ontology:cs_axiom_status(truth_seeking_requires_protection, holdable).
narrative_ontology:cs_axiom_grounding('c30b2c34-2544-48f7-9282-65d0bebd5bff', truth_seeking_requires_protection, instrumental).
narrative_ontology:cs_reference_frame('c30b2c34-2544-48f7-9282-65d0bebd5bff', post_1940_aaup_statement_of_principles).
narrative_ontology:cs_drift_state('c30b2c34-2544-48f7-9282-65d0bebd5bff', contemporary_political_polarization, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c30b2c34-2544-48f7-9282-65d0bebd5bff', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, students).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, public_discourse).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, political_actors_seeking_control).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, institutional_administrators_seeking_conformity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the pursuit of knowledge by protecting researchers from external pressures, ensuring that inquiry can proceed without fear of reprisal, thereby fostering intellectual independence and robust critical discourse.
% TRANSFER_FUNCTION: Transfers job security and intellectual autonomy to tenured faculty, in exchange for their commitment to truth-seeking and high-quality research, while transferring a degree of control away from external political and internal administrative actors.
% ABSENT_VOICES: Political actors seeking to control academic narratives, and institutional administrators prioritizing conformity over critical inquiry, are structurally resisted by tenure. They would argue for greater accountability and responsiveness to immediate societal or institutional needs, but their influence is curtailed by the protections tenure affords.
% DISAPPEARANCE_RATIONALE: If tenure vanished overnight, academic institutions would quickly become more susceptible to political and financial pressures. Research agendas would likely shift towards safer, more fundable, or politically palatable topics. Faculty would face increased precarity, potentially chilling controversial or critical inquiry. The quality and independence of public discourse would degrade as a result.
% FOUNDING_PROBLEM: The problem of ensuring intellectual independence and protecting scholars from arbitrary dismissal or political interference, particularly in fields of inquiry that challenge established norms or powerful interests.
% FOUNDING_PROBLEM_CORROBORATION: Academic associations, civil liberties organizations, and historical analyses of academic freedom cases consistently corroborate that the threat to intellectual independence remains live. While the specific forms of pressure may evolve, the fundamental problem of protecting high-risk inquiry persists, as attested by ongoing debates and legal challenges from outside the immediate beneficiary group of tenured faculty.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tenure_contract__academic_freedom_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__academic_freedom_reading_tests).
:- end_tests(tenure_contract__academic_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.2) and suppression (0.1) reflect the ideal function of tenure: it extracts minimal direct cost from faculty (beyond the initial probationary period) and primarily suppresses external pressures on research, not internal dissent. The theater ratio is low (0.05) because, in this reading, the mechanism is genuinely functional for its stated purpose. Accessibility collapse is high (0.8) because, once tenure is granted, the protection against arbitrary dismissal is substantial. Resistance is low (0.05) from within the academic community, as the system is largely accepted as beneficial for academic freedom, though external political actors may resist its effects.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of tenured faculty, this constraint is a pure Rope, providing essential protection for their work. From the perspective of external political actors or institutional administrators seeking conformity, it is a barrier to control, experienced as a low-level Snare or Tangled Rope due to its resistance to their influence. Students and the public are indirect beneficiaries, gaining from the quality and independence of research.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are primary beneficiaries (d=0.0-0.1) due to the protection and stability tenure provides. Students and the public are also beneficiaries (d=0.1-0.2) through access to independent research. Political actors and institutional administrators seeking to control research agendas are the targets/victims (d=0.8-0.9), as tenure limits their ability to exert direct influence or impose conformity. The constraint subsidizes academic independence by extracting from those who would suppress it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling academic freedom as pure extraction by focusing on the protective function of tenure. It acknowledges that the mandate (protecting truth-seeking) is still live and actively served by the constraint, rather than having atrophied into mere inertia or rent-seeking. The low theater ratio and extractiveness support this view, distinguishing it from a Piton or Snare where the original mandate is lost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine instantiation of academic freedom, or is it primarily a mechanism for institutional extraction or demographic reproduction?',
    'Empirical analysis of tenure review outcomes (e.g., correlation between tenure denial and research critical of institutional interests, or demographic patterns in tenure awards vs. research output).',
    'If primarily academic freedom, the classification as Rope holds. If found to be primarily extraction or reproduction, the classification would shift to Snare or Tangled Rope, respectively, for those readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''academic_freedom_reading'' of the ''tenure_contract'' kernel. Sibling readings (''institutional_extraction_reading'', ''demographic_reproduction_reading'') offer alternative structural interpretations.').

omega_variable(
    truth_seeking_efficacy,
    'To what extent does tenure actually enable high-risk, truth-seeking inquiry, versus merely protecting established academics?',
    'Longitudinal studies tracking research topics and outcomes of tenured vs. non-tenured faculty, particularly in politically sensitive or institutionally challenging areas.',
    'If tenure''s effect on truth-seeking is weak, the ''academic_freedom_reading'' loses empirical grounding, potentially shifting its classification towards a more inertial ''Piton'' or a less effective ''Rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(truth_seeking_efficacy, empirical, 'Assesses the empirical effectiveness of tenure in fostering academic freedom.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__academic_freedom_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tenu_tr_t10, tenure_contract__academic_freedom_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(tenu_tr_t20, tenure_contract__academic_freedom_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__academic_freedom_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(tenu_be_t10, tenure_contract__academic_freedom_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(tenu_be_t20, tenure_contract__academic_freedom_reading, base_extractiveness, 20, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__academic_freedom_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(tenu_su_t10, tenure_contract__academic_freedom_reading, suppression_requirement, 10, 0.09).
narrative_ontology:measurement(tenu_su_t20, tenure_contract__academic_freedom_reading, suppression_requirement, 20, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, identity_coordination).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, institutional_research_funding_allocation).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, university_governance_structures).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'tenure_contract' kernel, focusing on its role in academic freedom. Other readings (institutional_extraction_reading, demographic_reproduction_reading) exist and are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
