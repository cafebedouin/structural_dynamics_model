% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__institutional_extraction_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: tenure_contract__institutional_extraction_reading
 *   human_readable: Tenure Contract â Institutional Extraction Reading
 *   domain: higher_education/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This reading treats the tenure contract not as a bulwark of academic
 *   freedom but as a rigid property right that early-career winners
 *   permanently defend, creating a two-tier academic labor market. The
 *   constraint extracts from contingent faculty (through depressed wages,
 *   exclusion from benefits, and job insecurity) and students (through
 *   tuition directed toward tenured compensation and reduced instructional
 *   investment), while tenured faculty capture the rent as a durable claim on
 *   institutional resources. The kernel is contested: sibling readings
 *   emphasize freedom of inquiry or demographic gatekeeping rather than
 *   extraction.
 *
 * KEY AGENTS:
 *   - tenured_faculty (organized/mobile): Primary beneficiary â captures permanent job security and wage premiums
 *   - contingent_faculty (powerless/identity_locked): Primary target â bears flexibility costs, exclusion from benefits, and wage compression
 *   - students (powerless/constrained): Secondary target â bears tuition inflation and reduced instructional investment
 *   - university_administration (institutional/arbitrage): Agenda setter â enforces tenure rules and maintains the employment divide
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.78).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.72).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure Contract â Institutional Extraction Reading").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher_education/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, 'c0032219-254f-4bda-8961-6d29bc67d501').
narrative_ontology:cs_kernel_codification('c0032219-254f-4bda-8961-6d29bc67d501', formalized).
narrative_ontology:cs_authority_grounding('c0032219-254f-4bda-8961-6d29bc67d501', lineage).
narrative_ontology:cs_interpretation_layer_present('c0032219-254f-4bda-8961-6d29bc67d501').
narrative_ontology:cs_reading_relation('c0032219-254f-4bda-8961-6d29bc67d501', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('c0032219-254f-4bda-8961-6d29bc67d501', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('c0032219-254f-4bda-8961-6d29bc67d501', foundational, tenure_primary_function_rent_extraction).
narrative_ontology:cs_axiom_status(tenure_primary_function_rent_extraction, holdable).
narrative_ontology:cs_axiom_grounding('c0032219-254f-4bda-8961-6d29bc67d501', tenure_primary_function_rent_extraction, empirically_contingent).
narrative_ontology:cs_axiom('c0032219-254f-4bda-8961-6d29bc67d501', secondary, contingent_labor_bears_structural_cost).
narrative_ontology:cs_axiom_status(contingent_labor_bears_structural_cost, holdable).
narrative_ontology:cs_axiom_grounding('c0032219-254f-4bda-8961-6d29bc67d501', contingent_labor_bears_structural_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('c0032219-254f-4bda-8961-6d29bc67d501', classical_tenure_compact).
narrative_ontology:cs_drift_state('c0032219-254f-4bda-8961-6d29bc67d501', post_1970_labor_market_transformation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c0032219-254f-4bda-8961-6d29bc67d501', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent appointments with strong job security, governance rights, and compensation premiums. They defend tenure as necessary for academic freedom while capturing a stable, long-term claim on departmental budgets and instructional resources.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty, beneficiary,
    organized, generational, mobile, national).

% Teach on semester-to-semester or annual contracts with low wages, limited benefits, and no governance voice. Their professional identity is fused with academic career paths, making exit to non-academic labor markets cognitively costly despite poor prospects for tenure-line conversion.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_faculty, payer,
    powerless, immediate, identity_locked, national).

% Pay tuition that supports institutional salary structures including tenured compensation, while receiving reduced instructional investment as universities shift teaching loads to low-cost contingent labor. Transfer and exit carry high transaction costs.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, students, payer,
    powerless, biographical, constrained, national).

% Administers tenure clocks, promotion committees, and hiring lines. Maintains the two-tier employment structure through budget allocation and HR policy, often citing shared governance and accreditation norms while managing institutional risk.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, university_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__institutional_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically, securing long-term institutional commitment to enable high-risk, low-initial-yield research and protecting inquiry from political or commercial interference.
% TRANSFER_FUNCTION: Moves job security, benefits, and wage premiums from contingent instructional labor and students (via tuition directed to tenured compensation) to tenured faculty as a permanent property-like claim on departmental budgets.
% ABSENT_VOICES: Contingent faculty are excluded from tenure-line governance bodies and faculty senates at many institutions; students are excluded from tenure evaluation committees; non-tenure alternative-university models are marginalized in research-university discourse.
% DISAPPEARANCE_RATIONALE: The contemporary university budget and staffing model is built around the tenure-contingency divide. If tenure vanished, institutions would reallocate instructional spending, hiring ratios would shift, tuition cost structures would face pressure, and the academic labor market would reorganize.
% FOUNDING_PROBLEM: How to secure long-term scholarly commitment and protect academic inquiry from political retaliation, short-term funding cycles, and commercial pressure in an era before robust legal protections or alternative funding structures.
% FOUNDING_PROBLEM_CORROBORATION: Tenured faculty and AAUP historical records attest the founding problem. Labor economists, contingent faculty organizers, and higher-education policy researchers outside the beneficiary set argue the problem is now addressed by alternative mechanisms (federal grants, employment law, union contracts) or was always secondary to the cartelization function; corroboration from non-beneficiaries that the original problem remains acute is weak.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__institutional_extraction_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__institutional_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__institutional_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because tenure decouples compensation from market clearing and current instructional need; suppression is high (0.72) because alternative employment models are structurally excluded from research universities by accreditation norms and shared governance. Theater_ratio is moderate (0.45) because tenure review performs rigorous meritocratic assessment while simultaneously functioning as a lottery ticket for future rent. The measurement series run on one shared time grid so every metric is authored at every examined time point, showing extraction accumulation and enforcement hardening over the neoliberal university era.
 *
 * PERSPECTIVAL GAP:
 *   The tenured faculty seat and the contingent faculty seat should compute to very different types: from the tenured position the arrangement is a justified coordination mechanism protecting long-term inquiry; from the contingent and student positions the same structure operates as enforced extraction. The engine computes this divergence from the structural data â the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are beneficiaries (collect the rent, mobile exit, low d). Contingent faculty and students are targets (bear the costs, constrained or identity-locked exit, high d). University administration sits in between as enforcer (agenda setter, arbitrage exit, moderate d). The beneficiary and victim declarations map directly to these structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting scholarly inquiry from short-term pressures) is likely dead or solvable by alternative means, yet the arrangement persists because tenured faculty are organized to defend it and the costs are diffuse across contingent labor and students. This is a classic mandatrophy pattern: the mandate has outlived its function, but the constraint remains because the beneficiaries are concentrated and the payers are dispersed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the institutional_extraction_reading of the tenure_contract kernel. How would classification change if the academic_freedom_reading or demographic_reproduction_reading were adopted instead?',
    'Compare sibling stories'' epsilon values and beneficiary/victim structures; the kernel reading with lowest epsilon and no victim declarations would reclassify toward rope or scaffold.',
    'If the academic_freedom_reading were adopted, extractiveness would drop and the constraint would likely classify as rope or scaffold; if demographic_reproduction_reading, victim profile shifts to demographic minorities but extraction logic remains similar.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Kernel reading sibling divergence for tenure contract').

omega_variable(
    contingent_suppression_ambiguity,
    'Is the suppression experienced by contingent faculty structural (lack of alternative academic jobs) or internalized (professional identity fusion preventing exit to non-academic labor markets)?',
    'Post-exit trajectory studies: if contingent faculty who leave academia show reduced perceived constraint, suppression was partly internalized; if economic hardship persists, structural.',
    'If internalized, effective extraction exceeds structural measure; if purely structural, standard labor-market remedies (job creation) would suffice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingent_suppression_ambiguity, empirical, 'Structural vs internalized suppression in contingent academic labor').

omega_variable(
    tenure_coordination_extraction_boundary,
    'Does tenure still perform a non-extractable coordination function (long-term research commitment) or has the extraction function fully subsumed it?',
    'Comparative analysis of tenure and non-tenure research institutions: if research output and academic freedom indicators hold without tenure, coordination is separable from extraction.',
    'If separable, tenure is a tangled rope or snare; if inseparable, the high extraction is partly the necessary price of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenure_coordination_extraction_boundary, empirical, 'Whether tenure''s coordination and extraction components are structurally separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_extraction_tr_t0, tenure_contract__institutional_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tenure_extraction_tr_t10, tenure_contract__institutional_extraction_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(tenure_extraction_tr_t20, tenure_contract__institutional_extraction_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(tenure_extraction_tr_t30, tenure_contract__institutional_extraction_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(tenure_extraction_tr_t40, tenure_contract__institutional_extraction_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(tenure_extraction_tr_t50, tenure_contract__institutional_extraction_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(tenure_extraction_be_t0, tenure_contract__institutional_extraction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(tenure_extraction_be_t10, tenure_contract__institutional_extraction_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(tenure_extraction_be_t20, tenure_contract__institutional_extraction_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(tenure_extraction_be_t30, tenure_contract__institutional_extraction_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(tenure_extraction_be_t40, tenure_contract__institutional_extraction_reading, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(tenure_extraction_be_t50, tenure_contract__institutional_extraction_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tenure_extraction_su_t0, tenure_contract__institutional_extraction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(tenure_extraction_su_t10, tenure_contract__institutional_extraction_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(tenure_extraction_su_t20, tenure_contract__institutional_extraction_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(tenure_extraction_su_t30, tenure_contract__institutional_extraction_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(tenure_extraction_su_t40, tenure_contract__institutional_extraction_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(tenure_extraction_su_t50, tenure_contract__institutional_extraction_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
