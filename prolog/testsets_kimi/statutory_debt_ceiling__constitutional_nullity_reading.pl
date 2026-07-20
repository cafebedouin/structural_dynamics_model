% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__constitutional_nullity_reading, []).

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
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Statutory Debt Ceiling â Constitutional Nullity Reading
 *   domain: constitutional law / political economy / fiscal governance
 *
 * SUMMARY:
 *   This constraint story models the statutory debt ceiling under the
 *   constitutional nullity reading: the view that the debt ceiling is void
 *   under Section 4 of the Fourteenth Amendment, which commands that the
 *   validity of the public debt 'shall not be questioned.' Under this
 *   reading, the statute is legally inoperative, Treasury borrows as required
 *   by appropriations, and congressional authorization votes are ceremonial.
 *   The constraint persists as institutional inertia and political
 *   theaterâa piton rather than a functional limit. The high theater_ratio
 *   and zero extractiveness reflect the divergence between legal reality
 *   (nullity) and political performance (ongoing votes and standoffs). Key
 *   agents include congressional leadership (agenda-setter maintaining the
 *   theater), the Treasury Department (operating under constitutional
 *   command), and US taxpayers (bearing diffuse uncertainty costs).
 *
 * KEY AGENTS:
 *   - congressional_leadership: Agenda-setter â maintains the statutory voting ceremony and could repeal the dead-letter law.
 *   - treasury_department: Payer â bears operational costs of managing borrowing under legal uncertainty despite constitutional nullity.
 *   - us_taxpayers: Payer â bears diffuse costs of political theater and potential market instability.
 *   - federal_judiciary: Observer â potential adjudicator, currently uninvolved.
 *   - constitutional_scholars: Observer â attests to the Fourteenth Amendment's superseding effect.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.2).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, piton).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling â Constitutional Nullity Reading").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional law / political economy / fiscal governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, 'a0898cb9-4065-40b3-b313-32d67d21ca28').
narrative_ontology:cs_kernel_codification('a0898cb9-4065-40b3-b313-32d67d21ca28', fixed_text).
narrative_ontology:cs_authority_grounding('a0898cb9-4065-40b3-b313-32d67d21ca28', lineage).
narrative_ontology:cs_interpretation_layer_present('a0898cb9-4065-40b3-b313-32d67d21ca28').
narrative_ontology:cs_reading_relation('a0898cb9-4065-40b3-b313-32d67d21ca28', statutory_debt_ceiling__coordination_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('a0898cb9-4065-40b3-b313-32d67d21ca28', statutory_debt_ceiling__extraction_snare_reading, coexists_with).
narrative_ontology:cs_axiom('a0898cb9-4065-40b3-b313-32d67d21ca28', foundational, public_debt_shall_not_be_questioned).
narrative_ontology:cs_axiom_status(public_debt_shall_not_be_questioned, holdable).
narrative_ontology:cs_axiom_grounding('a0898cb9-4065-40b3-b313-32d67d21ca28', public_debt_shall_not_be_questioned, conventional).
narrative_ontology:cs_axiom('a0898cb9-4065-40b3-b313-32d67d21ca28', foundational, statutory_ceiling_ab_initio_void).
narrative_ontology:cs_axiom_status(statutory_ceiling_ab_initio_void, holdable).
narrative_ontology:cs_axiom_grounding('a0898cb9-4065-40b3-b313-32d67d21ca28', statutory_ceiling_ab_initio_void, conventional).
narrative_ontology:cs_reference_frame('a0898cb9-4065-40b3-b313-32d67d21ca28', fourteenth_amendment_supremacy).
narrative_ontology:cs_drift_state('a0898cb9-4065-40b3-b313-32d67d21ca28', contemporary_political_standoff_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a0898cb9-4065-40b3-b313-32d67d21ca28', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, us_taxpayers).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, fourteenth_amendment_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the statutory debt limit and schedules periodic authorization votes; preserves the ceremonial practice even though the constraint is legally inoperative under the nullity reading. Could repeal the statute but does not, because the votes serve political signaling and leverage functions unrelated to actual borrowing capacity.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congressional_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Executes borrowing operations to meet federal obligations; under this reading, it disregards the statutory ceiling as constitutionally void and borrows as required by appropriations, yet still allocates staff and legal resources to managing the political interface around the ceremonial limit.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department, payer,
    institutional, biographical, constrained, national).

% Bear the diffuse costs of political theater, legislative time consumed by ceiling standoffs, and episodic market uncertainty, despite the legal argument that default is constitutionally impossible.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, us_taxpayers, payer,
    moderate, biographical, constrained, national).

% Has not been required to issue a definitive ruling on the nullity reading; stands ready to adjudicate if a justiciable case arises, but treats the constitutional text as potentially self-executing.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% Analyze the supremacy of the Fourteenth Amendment's public debt clause over conflicting statutes; provide external corroboration for the nullity reading and document the legal lineage of the constitutional command.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the statutory debt ceiling serves no operative coordination function; Treasury borrowing is coordinated solely through the constitutional appropriations process and the Fourteenth Amendment's public debt command.
% TRANSFER_FUNCTION: No operative transfer occurs; the statute is legally inoperative, and any attempted restriction on borrowing is void ab initio under Section 4.
% ABSENT_VOICES: Members of Congress who treat the ceiling as a binding fiscal brake and the general public conditioned to understand the ceiling as a necessary restraint are present in political discourse but structurally overridden by the constitutional framework; absent is an independent corroborating party outside the legal academy that attests the ceiling remains a necessary coordination device under this reading.
% DISAPPEARANCE_RATIONALE: The constraint is already legally inoperative; its disappearance would merely eliminate ceremonial congressional votes and associated political theater, without altering Treasury's constitutional borrowing authority or day-to-day operations.
% FOUNDING_PROBLEM: The debt ceiling was originally enacted in 1917 and consolidated in 1939 to aggregate congressional bond authorizations into a single statutory limit, replacing case-by-case approval of each debt issue.
% FOUNDING_PROBLEM_CORROBORATION: Congressional records and legislative historians attest to the original aggregation purpose. However, no party outside the political beneficiaries of the theater attests that this problem remains live under current fiscal architecture; constitutional law scholars and Treasury legal counsel outside the congressional agenda-setting sphere attest that the public debt clause supersedes the statutory mechanism.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_unchanged).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 0.0, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).
:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because the constraint is legally void; suppression is low (0.2) because the statute is not enforced against Treasury borrowing, though political pressure to treat it as binding persists. Theater_ratio is high (0.85) because the constraint's persistence is almost entirely performativeâCongress continues to vote, presidents negotiate, and markets react to a legally empty threat. Accessibility_collapse is low (0.2) because the alternative (constitutional borrowing under Section 4) is legally available and always has been. Resistance is moderate (0.4) because political actors who favor fiscal brinkmanship actively contest the nullity reading and maintain the statutory apparatus.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (congressional leadership) experiences the constraint as a procedural tradition and political leverage tool; the payer seats (Treasury, taxpayers) experience it as an unnecessary source of administrative burden and uncertainty. The analytical observer seats (constitutional scholars, federal judiciary) experience it as a settled legal nullity. The engine should compute high divergence between the agenda-setter (theater maintenance) and payers (diffuse cost), while analytical seats see the constraint as inert.
 *
 * DIRECTIONALITY LOGIC:
 *   No explicit beneficiaries or victims are declared in base_properties because the constraint extracts nothing. Congressional leadership sits near the agenda-setter pole and gains political utility from the theater, but this is not formal extraction captured by the metric. Treasury and taxpayers are seated as payers to reflect the diffuse cost of maintaining the ceremonial structure, yet their directionality defaults toward symmetric because no extraction is structurally declared and their exit is politically constrained rather than locked by the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as piton prevents mislabeling the debt ceiling as a snare (there is no concentrated extraction) or as a scaffold (there is no sunset clause and no transitional coordination). It also prevents mislabeling it as a mountain: it is not a natural law but a dead-letter statute. The zero extractiveness distinguishes it from the extraction_snare reading of the same kernel, while the high theater_ratio captures its inertial persistence despite functional atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_void_vs_political_coercion,
    'If the debt ceiling is legally void, why does Treasury continue to treat the statutory limit as a binding planning constraint and why do markets price ceiling standoffs?',
    'Executive branch legal memoranda and market pricing data during binding ceiling episodes; observation of whether Treasury continues to invoke ''extraordinary measures'' despite the nullity reading.',
    'If Treasury defers to the statute despite the nullity reading, the effective suppression and extractiveness are higher than zero, potentially shifting the computed type toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_void_vs_political_coercion, empirical, 'Whether legal nullity translates into actual Treasury behavior or remains theoretical.').

omega_variable(
    committer_foreclosure_uncertainty,
    'Does the constitutional nullity reading definitively foreclose the coordination scaffold reading, or do they represent incommensurable legal and political frameworks?',
    'Supreme Court precedent or definitive departmental legal opinion adopting the nullity reading and explicitly holding the statute inoperative.',
    'If foreclosure is judicially established, the kernel collapses toward the nullity reading; if not, the readings remain in permanent coexistence, contaminating each other''s classification stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_foreclosure_uncertainty, conceptual, 'Whether the nullity reading logically eliminates the coordination reading or merely coexists with it.').

omega_variable(
    diffuse_cost_quantification,
    'What is the measurable economic and administrative cost of debt ceiling theater (market volatility, Treasury staffing, legislative time) borne by diffuse payers?',
    'Economic studies of volatility premiums during standoffs, Congressional Budget Office cost estimates of legislative time, and Treasury staff hour allocations.',
    'Quantifying the diffuse cost would clarify whether the piton classification is stable or whether the theater itself constitutes a hidden extraction mechanism sufficient to reclassify the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diffuse_cost_quantification, empirical, 'Quantifiable economic cost of maintaining the ceremonial constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(stat_tr_t20, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(stat_tr_t40, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(stat_tr_t60, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 60, 0.65).
narrative_ontology:measurement(stat_tr_t80, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 80, 0.78).
narrative_ontology:measurement(stat_tr_t100, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 100, 0.85).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(stat_be_t20, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 20, 0.0).
narrative_ontology:measurement(stat_be_t40, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 40, 0.0).
narrative_ontology:measurement(stat_be_t60, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 60, 0.0).
narrative_ontology:measurement(stat_be_t80, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 80, 0.0).
narrative_ontology:measurement(stat_be_t100, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 100, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(stat_su_t20, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement(stat_su_t40, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement(stat_su_t60, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 60, 0.15).
narrative_ontology:measurement(stat_su_t80, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 80, 0.18).
narrative_ontology:measurement(stat_su_t100, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, extraction_snare_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the statutory_debt_ceiling kernel, decomposed per the Îµ-invariance principle. The constitutional nullity reading treats the debt ceiling as void and inoperative; the coordination scaffold reading treats it as valid procedural coordination; the extraction snare reading treats it as a weaponized extraction mechanism. Each reading carries a distinct Îµ and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
