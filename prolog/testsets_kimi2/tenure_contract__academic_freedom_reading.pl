% ============================================================================
% CONSTRAINT STORY: tenure_contract__academic_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tenure_contract__academic_freedom_reading
 *   human_readable: Academic Tenure as Freedom-of-Inquiry Protection
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the academic_freedom_reading of the
 *   tenure_contract kernel. It treats the standing tenure arrangement as a
 *   protective coordination mechanism that decouples researcher survival from
 *   political and institutional displeasure, enabling high-risk inquiry. From
 *   this reading, faculty are coordinated beneficiaries who gain
 *   independence, students and research consumers receive diffuse quality
 *   benefits, and external political actors bear the cost of suppressed
 *   retaliation leverage. The constraint is claimed as tangled_rope because
 *   the same structure that coordinates research independence actively
 *   suppresses external political control, constituting asymmetric extraction
 *   from political actors who lose their ability to punish inconvenient
 *   research. Sibling readings (institutional_extraction_reading,
 *   demographic_reproduction_reading) share the same referent but author
 *   different epsilon values and victim structures.
 *
 * KEY AGENTS:
 *   - tenured_faculty: Primary beneficiary (organized/constrained) â receives job security and research independence
 *   - external_political_actors: Primary target (powerful/constrained) â loses leverage over research personnel and content
 *   - higher_education_institutions: Agenda setter (institutional/constrained) â administers and defends tenure against political dismantlement
 *   - students: Secondary beneficiary (moderate/mobile) â receives downstream research quality benefits
 *   - research_consumers: Diffuse beneficiary (organized/global) â receives unfiltered research output
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.52).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.78).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Academic Tenure as Freedom-of-Inquiry Protection").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__academic_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, '3a39599e-e5c3-4a11-b1c8-4cc9cbccad3a').
narrative_ontology:cs_kernel_codification('3a39599e-e5c3-4a11-b1c8-4cc9cbccad3a', formalized).
narrative_ontology:cs_authority_grounding('3a39599e-e5c3-4a11-b1c8-4cc9cbccad3a', practice).
narrative_ontology:cs_interpretation_layer_present('3a39599e-e5c3-4a11-b1c8-4cc9cbccad3a').
narrative_ontology:cs_reading_relation('3a39599e-e5c3-4a11-b1c8-4cc9cbccad3a', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a39599e-e5c3-4a11-b1c8-4cc9cbccad3a', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('3a39599e-e5c3-4a11-b1c8-4cc9cbccad3a', foundational, tenure_prerequisite_to_truth_seeking).
narrative_ontology:cs_axiom_status(tenure_prerequisite_to_truth_seeking, holdable).
narrative_ontology:cs_axiom_grounding('3a39599e-e5c3-4a11-b1c8-4cc9cbccad3a', tenure_prerequisite_to_truth_seeking, empirically_contingent).
narrative_ontology:cs_axiom('3a39599e-e5c3-4a11-b1c8-4cc9cbccad3a', foundational, institutional_neutrality_toward_research_content).
narrative_ontology:cs_axiom_status(institutional_neutrality_toward_research_content, holdable).
narrative_ontology:cs_axiom_grounding('3a39599e-e5c3-4a11-b1c8-4cc9cbccad3a', institutional_neutrality_toward_research_content, deontological).
narrative_ontology:cs_reference_frame('3a39599e-e5c3-4a11-b1c8-4cc9cbccad3a', professional_autonomy_tradition).
narrative_ontology:cs_drift_state('3a39599e-e5c3-4a11-b1c8-4cc9cbccad3a', contemporary_political_polarization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3a39599e-e5c3-4a11-b1c8-4cc9cbccad3a', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, students).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, research_consumers).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, external_political_actors).
narrative_ontology:constraint_vindicates(tenure_contract__academic_freedom_reading, academic_freedom_doctrine).
narrative_ontology:constraint_vindicates(tenure_contract__academic_freedom_reading, institutional_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive contractual job security and procedural protections that insulate their research and teaching from political retaliation and institutional displeasure. They bear the costs of reduced mobility and extended probationary periods but gain the ability to pursue high-risk, long-term inquiry without fear of termination for inconvenient findings.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    organized, biographical, constrained, national).

% Benefit from faculty willingness to teach and research controversial topics without fear of external retaliation. They do not direct the tenure arrangement but receive improved research quality and classroom autonomy as downstream effects of faculty independence.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, students, beneficiary,
    moderate, biographical, mobile, national).

% Lose the ability to punish or terminate researchers whose findings challenge political orthodoxies, policy preferences, or partisan interests. Their leverage over publicly funded research and higher education is structurally blocked by tenure protections, forcing them to pursue slower, costlier avenues of influence such as legislation or funding cuts.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, external_political_actors, payer,
    powerful, immediate, constrained, national).

% Administer tenure review processes, defend tenure lines against political pressure, and bear the long-term financial and staffing rigidity of permanent appointments. They set the formal rules but are themselves constrained by accreditation norms, faculty governance, and competitive pressures to maintain tenure systems.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, higher_education_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Receive unfiltered research findings that might otherwise be suppressed by political or commercial pressure. Their benefit is diffuse and mediated through publication and citation systems rather than direct interaction with the tenure arrangement.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, research_consumers, beneficiary,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects long-term, high-risk inquiry and teaching from short-term political retaliation and institutional convenience by creating a procedural and contractual barrier between researcher employment decisions and the content of their research or speech.
% TRANSFER_FUNCTION: Transfers job security, procedural due process, and institutional insulation from universities to individual researchers, while transferring the cost of that insulation to political actors and institutional authorities who lose direct leverage over research content and personnel decisions.
% ABSENT_VOICES: Contingent faculty, graduate students, and alt-ac researchers lack tenure protections and are structurally excluded from faculty governance conversations about tenure reform; their perspectives on whether tenure actually stabilizes truth-seeking or merely stratifies the academic labor market are absent from this reading's framing.
% DISAPPEARANCE_RATIONALE: Without tenure protections, researchers would face immediate employment vulnerability to political backlash, donor pressure, and institutional displeasure; high-risk inquiry would contract toward safer topics, and the political economy of research would reorganize around funding accessibility and ideological acceptability rather than epistemic merit.
% FOUNDING_PROBLEM: Research and teaching that challenges political orthodoxy, powerful interests, or popular sentiment is vulnerable to retaliation via non-renewal, termination, or funding withdrawal, creating systematic pressure toward conformity and away from high-risk truth-seeking.
% FOUNDING_PROBLEM_CORROBORATION: Historians of higher education and academic labor economists outside the direct beneficiary pool document extensive pre-tenure political interference in appointments and terminations; contemporary cases of political pressure on researchers in jurisdictions with weakened tenure protections corroborate that the founding problem remains active.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__academic_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__academic_freedom_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__academic_freedom_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__academic_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52) is moderate-to-substantial because the constraint actively suppresses political actors' preferred behavior (retaliation) to sustain the coordination function. Suppression (0.78) is high because tenure's persistence depends on active institutional defense against political dismantlement. Theater ratio (0.35) reflects moderate performative maintenance: some tenure defense is ritualistic, but the core procedural protections remain functional. Accessibility collapse (0.68) is moderately high because meaningful alternatives (long-term contracts with equivalent protections) are understood but largely unavailable in the US system. Resistance (0.72) is high due to sustained political and legislative attacks on tenure in multiple jurisdictions. The temporal series show rising extraction and suppression requirements as political contestation intensifies over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The tenured faculty seat and the external political actor seat should compute as strongly divergent types: faculty experience low effective extraction (benefiting from coordination) while political actors experience high effective extraction (their control is suppressed). The institution seat sits near symmetric, bearing the administrative and financial costs of tenure while gaining accreditation and competitive benefits. The engine computes this divergence from the structural beneficiary/victim declarations and exit modulations.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are declared beneficiaries with constrained exit (tenure binds them to the institution but protects them), yielding low directionality. External political actors are declared victims with constrained exit (they must work through slow political and legislative processes to change tenure), yielding high directionality. Students and research consumers are beneficiaries with mobile exit, yielding low directionality. The institution is agenda_setter with constrained exit, sitting near symmetric. No directionality overrides are needed because the structural derivation captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling this constraint as either a pure rope (which would ignore the genuine extraction from political actors) or a pure snare (which would deny the real coordination benefits for faculty and research quality). The classification captures the hybrid nature: the same tenure structure that coordinates research independence extracts from political control. If the founding problem (political retaliation) were dead but the structure persisted, it would drift toward piton or snare; the founding_problem_status remains live, corroborated by ongoing political pressure, justifying the tangled_rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tenure_kernel_reading_contest,
    'Does the tenure contract primarily coordinate the protection of high-risk inquiry, or does it function as institutional extraction and demographic gatekeeping?',
    'Comparative analysis of tenure''s effects across disciplines and jurisdictions, measuring whether tenure-track expansion correlates with research risk-taking or with labor stratification and demographic homogeneity.',
    'If extraction and gatekeeping dominate, this reading''s classification as tangled_rope understates the constraint''s extractive dimension; if protective coordination dominates, the sibling readings overstate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenure_kernel_reading_contest, conceptual, 'Whether tenure is protective coordination or extractive gatekeeping').

omega_variable(
    political_actor_victim_status,
    'Are external political actors genuine victims of extraction, or are they necessary parties whose influence must be constrained for the coordination function to operate?',
    'Comparative analysis of whether political actors'' loss of leverage over researchers produces measurable social costs (accountability deficits) or merely removes an illegitimate coordination failure.',
    'Reclassifies political actors from targets to non-participants, shifting the constraint toward rope; alternatively, confirms their victim status and strengthens the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_actor_victim_status, preference, 'Whether political actor suppression is victimization or legitimate coordination cost').

omega_variable(
    contingent_labor_exclusion,
    'Does the tenure system''s exclusion of contingent faculty from its protections represent a separable labor-market failure, or an inherent feature of the tenure contract?',
    'Cross-institutional comparison of tenure-track versus contingent labor ratios and working conditions in jurisdictions with varying tenure strength.',
    'If inseparable, the coordination story is partial and the constraint carries unacknowledged extraction from contingent labor, validating the institutional_extraction_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingent_labor_exclusion, empirical, 'Whether contingent faculty exclusion is separable from tenure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__academic_freedom_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tenu_tr_t8, tenure_contract__academic_freedom_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(tenu_tr_t16, tenure_contract__academic_freedom_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(tenu_tr_t24, tenure_contract__academic_freedom_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(tenu_tr_t32, tenure_contract__academic_freedom_reading, theater_ratio, 32, 0.31).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__academic_freedom_reading, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__academic_freedom_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tenu_be_t8, tenure_contract__academic_freedom_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(tenu_be_t16, tenure_contract__academic_freedom_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(tenu_be_t24, tenure_contract__academic_freedom_reading, base_extractiveness, 24, 0.45).
narrative_ontology:measurement(tenu_be_t32, tenure_contract__academic_freedom_reading, base_extractiveness, 32, 0.48).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__academic_freedom_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__academic_freedom_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(tenu_su_t8, tenure_contract__academic_freedom_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(tenu_su_t16, tenure_contract__academic_freedom_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(tenu_su_t24, tenure_contract__academic_freedom_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(tenu_su_t32, tenure_contract__academic_freedom_reading, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__academic_freedom_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the tenure_contract kernel. The academic_freedom_reading, institutional_extraction_reading, and demographic_reproduction_reading share the same referent (the tenure contract arrangement) but author different epsilon values, beneficiary/victim structures, and coordination functions. They form a constraint family linked by sibling relations in cs_structure and network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
