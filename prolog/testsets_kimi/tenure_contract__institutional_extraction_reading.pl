% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Tenure as Institutional Rent Extraction
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the institutional_extraction_reading
 *   of the tenure_contract kernel: the claim that tenure functions not as a
 *   coordination mechanism for academic freedom but as a permanent
 *   rent-extraction device that entrenches early winners, rigidifies
 *   employment, and loads flexibility costs onto contingent faculty and
 *   students. The constraint operates through a formalized peer-review and
 *   governance kernel grounded in the AAUP tradition, but its contemporary
 *   persistence is read here as depending on the suppression of alternative
 *   employment models and the identity-lock of tenured beneficiaries. The
 *   authored metrics are independent of the sibling academic_freedom_reading
 *   and are not averaged across the kernel.
 *
 * KEY AGENTS:
 *   - tenured_faculty: Primary beneficiary (powerful/identity_locked) â captures rent via permanent claims and governs the scarcity of tenure lines
 *   - contingent_faculty: Primary target (powerless/trapped) â bears flexibility costs, job insecurity, and sunk-credential lock-in
 *   - students: Secondary target (moderate/constrained) â pays tuition supporting the dual labor structure while receiving less stable instruction
 *   - university_administration: Agenda setter (institutional/constrained) â administers the two-tier system, could alter it but faces governance and accreditation constraints
 *   - prospective_scholars: Excluded voice (powerless/trapped) â competes for vanishing tenure-track lines, excluded from employment policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.82).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.78).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, snare).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure as Institutional Rent Extraction").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, 'eb285eb6-150a-48e7-88dc-5656f7b1522d').
narrative_ontology:cs_kernel_codification('eb285eb6-150a-48e7-88dc-5656f7b1522d', formalized).
narrative_ontology:cs_authority_grounding('eb285eb6-150a-48e7-88dc-5656f7b1522d', lineage).
narrative_ontology:cs_interpretation_layer_present('eb285eb6-150a-48e7-88dc-5656f7b1522d').
narrative_ontology:cs_reading_relation('eb285eb6-150a-48e7-88dc-5656f7b1522d', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb285eb6-150a-48e7-88dc-5656f7b1522d', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('eb285eb6-150a-48e7-88dc-5656f7b1522d', foundational, permanent_claim_entails_extraction).
narrative_ontology:cs_axiom_status(permanent_claim_entails_extraction, holdable).
narrative_ontology:cs_axiom_grounding('eb285eb6-150a-48e7-88dc-5656f7b1522d', permanent_claim_entails_extraction, empirically_contingent).
narrative_ontology:cs_axiom('eb285eb6-150a-48e7-88dc-5656f7b1522d', foundational, rigidity_loads_cost_to_periphery).
narrative_ontology:cs_axiom_status(rigidity_loads_cost_to_periphery, holdable).
narrative_ontology:cs_axiom_grounding('eb285eb6-150a-48e7-88dc-5656f7b1522d', rigidity_loads_cost_to_periphery, empirically_contingent).
narrative_ontology:cs_reference_frame('eb285eb6-150a-48e7-88dc-5656f7b1522d', permanent_tenure_claim_norm).
narrative_ontology:cs_drift_state('eb285eb6-150a-48e7-88dc-5656f7b1522d', contingent_majority_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eb285eb6-150a-48e7-88dc-5656f7b1522d', '').
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

% Hold permanent appointments with procedural termination protections, stable salaries, and benefits. Govern hiring and curriculum through faculty senates and peer-review committees. Their professional identity is fused with the tenure-track distinction, leading them to defend the scarcity of tenure lines as meritocratic and necessary for quality.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty, beneficiary,
    powerful, generational, identity_locked, national).

% Teach on semester-to-semester contracts with low wages, no benefits, and no job security. Absorb enrollment volatility and course cancellations that tenure insulates senior faculty from. Doctoral credentials and sunk training costs trap them in academia with few viable exit paths.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_faculty, payer,
    powerless, immediate, trapped, national).

% Pay tuition that funds the tenure infrastructure while receiving instruction increasingly delivered by underpaid contingent faculty with high turnover. Face reduced mentorship and curricular continuity. Alternative credentialing pathways exist but carry labor-market stigma and uncertain returns.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, students, payer,
    moderate, biographical, constrained, national).

% Administers the dual labor market, using contingent appointments to cover instructional needs while preserving tenure lines for research prestige and faculty governance compliance. Could in principle alter employment models but is constrained by shared governance agreements, accreditation norms, and faculty senate resistance.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).

% Doctoral students and early-career researchers competing for a shrinking pool of tenure-track lines. Bear the opportunity costs of prolonged training and would object to the contraction of permanent positions, but are structurally excluded from hiring committees and employment policy governance.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, prospective_scholars, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__institutional_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement purports to solve researcher retention and institutional memory by offering lifetime employment security; in this reading, the founding conditions for that coordination have expired and the mechanism now functions to lock in resource claims for early winners.
% TRANSFER_FUNCTION: Moves employment flexibility costs, salary stability, and departmental governance power from contingent faculty to tenured faculty, while loading instructional resource costs onto student tuition and transferring labor-supply risk to the contingent periphery.
% ABSENT_VOICES: Contingent faculty are present in the workplace but excluded from tenure-line governance bodies and faculty senates that set employment policy. Prospective scholars competing for vanishing tenure-track lines bear the opportunity costs but are excluded from hiring committees. Students pay tuition but do not participate in employment governance.
% DISAPPEARANCE_RATIONALE: If tenure vanished, universities would reallocate tenure-line budgets, contingent faculty would gain bargaining power and income, tuition pressure might shift, and the current two-tier labor market would collapse. Departments would reorganize around renewable contracts or unionized permanent non-tenure tracks.
% FOUNDING_PROBLEM: Mid-20th century need to attract research talent during higher education expansion and protect faculty from politically motivated termination.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists and higher education historians outside the tenured beneficiary class document the shift from faculty shortage to PhD oversupply; contingent faculty organizers and education policy researchers attest the political-retaliation rationale is no longer the live constraint on speech.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__institutional_extraction_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.82) is high because the constraint permanently allocates departmental resources and instructional flexibility to a shrinking tenured core while expanding a low-cost contingent periphery. Suppression (0.78) is high because persistence depends on actively suppressing alternative permanent-track models and maintaining the credential barrier that traps PhDs in the contingent pool. Theater_ratio (0.62) reflects that a substantial share of peer-review ritual and faculty governance now serves to maintain the scarcity of tenure lines and the legitimacy of the meritocratic tournament rather than to improve research quality. Accessibility_collapse (0.72) is high because viable alternative academic career structures have largely disappeared from the US system. Resistance (0.45) is moderate: contingent faculty organizing and student advocacy exert pressure, but tenured faculty identity-lock and administrative risk-aversion dampen reform.
 *
 * PERSPECTIVAL GAP:
 *   The tenured faculty seat should compute as near-beneficiary (low d, subsidy from the constraint), experiencing the arrangement as deserved reward for merit and necessary for scholarly quality. The contingent faculty seat should compute as near-target (high d, amplified extraction), experiencing the same arrangement as precarity and deferred exploitation. The student seat should compute as mid-to-high target because tuition funds the infrastructure while instructional quality becomes more variable. The engine derives this divergence from the structural role declarations and exit modulations.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are declared beneficiaries with identity_locked exit: their professional self-concept is fused with the tenure distinction, so they defend the constraint as natural and just, yielding d near 0.0. Contingent faculty are declared victims with trapped exit: PhD credential specificity and scarcity of non-academic pathways lock them in, yielding d near 1.0. Students are victims with constrained exit: degree signaling and accreditation limit arbitrage, yielding d around 0.65. University administration sits as agenda_setter with constrained exit; they do not primarily capture the rent but enforce the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â mid-century faculty shortage and political retaliation risk â is dead. The constraint persists as a snare because its removal would require confronting tenured faculty power and restructuring university budgets, while the coordination story (academic freedom) is still deployed as cover. The R5 genealogy interview (founding_problem_status: dead, disappearance_verdict: world_rearranges) flags this as mandatrophy: the arrangement's justification is obsolete but the extraction continues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tenure_rent_vs_coordination,
    'Is the tenure system structurally separable from the rent it conveys, or does lifetime appointment necessarily entail the extraction modeled here?',
    'Comparative analysis of non-tenure systems with strong academic freedom protections (e.g., UK permanent contracts, German civil service, unionized contingent tracks in Canadian and Australian universities).',
    'If separable, this reading is validated as snare; if inseparable, the extraction may be the necessary cost of a coordination function, pushing toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenure_rent_vs_coordination, conceptual, 'Whether tenure rent is separable from coordination').

omega_variable(
    contingency_causation,
    'Does tenure cause the expansion of contingent labor, or would fiscal austerity and enrollment pressures produce contingency regardless of tenure''s existence?',
    'Cross-institutional regression controlling for endowment, public funding, and union density; natural experiments from institutions that abolished tenure.',
    'If tenure is not the causal driver, the extraction story weakens and the constraint may be better read as piton or rope with external fiscal pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingency_causation, empirical, 'Causal role of tenure in contingent labor expansion').

omega_variable(
    student_cost_attribution,
    'Do students actually bear costs via tuition because of tenure, or does contingent labor substitution already offset tenure costs while tuition rises for other reasons?',
    'Cost-accounting studies tracing instructional spend per student to tenure-line versus contingent salaries; tuition trend decomposition.',
    'If tuition rises are decoupled from tenure costs, students may not be victims of this constraint, narrowing the victim set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(student_cost_attribution, empirical, 'Student tuition cost attribution to tenure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_inst_ext_tr_t0, tenure_contract__institutional_extraction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tenure_inst_ext_tr_t10, tenure_contract__institutional_extraction_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(tenure_inst_ext_tr_t20, tenure_contract__institutional_extraction_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(tenure_inst_ext_tr_t30, tenure_contract__institutional_extraction_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement(tenure_inst_ext_tr_t40, tenure_contract__institutional_extraction_reading, theater_ratio, 40, 0.6).
narrative_ontology:measurement(tenure_inst_ext_tr_t50, tenure_contract__institutional_extraction_reading, theater_ratio, 50, 0.62).

% Extraction over time
narrative_ontology:measurement(tenure_inst_ext_be_t0, tenure_contract__institutional_extraction_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(tenure_inst_ext_be_t10, tenure_contract__institutional_extraction_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(tenure_inst_ext_be_t20, tenure_contract__institutional_extraction_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(tenure_inst_ext_be_t30, tenure_contract__institutional_extraction_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(tenure_inst_ext_be_t40, tenure_contract__institutional_extraction_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(tenure_inst_ext_be_t50, tenure_contract__institutional_extraction_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(tenure_inst_ext_su_t0, tenure_contract__institutional_extraction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(tenure_inst_ext_su_t10, tenure_contract__institutional_extraction_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(tenure_inst_ext_su_t20, tenure_contract__institutional_extraction_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(tenure_inst_ext_su_t30, tenure_contract__institutional_extraction_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(tenure_inst_ext_su_t40, tenure_contract__institutional_extraction_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(tenure_inst_ext_su_t50, tenure_contract__institutional_extraction_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the tenure_contract kernel. The sibling readings (academic_freedom_reading, demographic_reproduction_reading) instantiate structurally distinct constraints from the same kernel. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
