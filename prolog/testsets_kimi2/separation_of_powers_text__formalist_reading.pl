% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__formalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__formalist_reading, []).

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
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Formalist Nondelegation and Strict Separation of Powers
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This constraint story instantiates the formalist reading of the
 *   separation-of-powers kernel: the claim that Articles IâIII establish
 *   strict, impermeable boundaries and that Congress may not delegate
 *   legislative authority to administrative agencies. Under this reading, the
 *   modern administrative state is largely unconstitutional. The constraint
 *   extracts governance capacity from agencies while claiming to coordinate
 *   democratic accountability through exclusive congressional lawmaking.
 *   Administrative agencies enter the victim set; Congress and regulated
 *   entities are positioned as beneficiaries. The high Îµ and suppression
 *   metrics reflect the reading's aggressive invalidation of agency authority
 *   and its foreclosure of functionalist alternatives. The claim is
 *   tangled_rope â a genuine coordination claim (democratic accountability)
 *   coupled with asymmetric extraction (regulatory paralysis) â and the
 *   metrics are authored independently to measure that extraction.
 *
 * KEY AGENTS:
 *   - administrative_agencies: Primary target (institutional/constrained) â stripped of legislative-like rulemaking authority
 *   - federal_judiciary: Agenda-setter (institutional/analytical) â enforces strict boundaries via judicial review
 *   - congressional_institutionalists: Beneficiary (institutional/constrained) â retain formal monopoly on legislation
 *   - regulated_entities: Secondary beneficiary (powerful/mobile) â gain from regulatory paralysis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.82).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.88).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Formalist Nondelegation and Strict Separation of Powers").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, '878a029a-635f-4803-afa3-164f4a163f7e').
narrative_ontology:cs_kernel_codification('878a029a-635f-4803-afa3-164f4a163f7e', fixed_text).
narrative_ontology:cs_authority_grounding('878a029a-635f-4803-afa3-164f4a163f7e', lineage).
narrative_ontology:cs_interpretation_layer_present('878a029a-635f-4803-afa3-164f4a163f7e').
narrative_ontology:cs_reading_relation('878a029a-635f-4803-afa3-164f4a163f7e', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('878a029a-635f-4803-afa3-164f4a163f7e', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('878a029a-635f-4803-afa3-164f4a163f7e', foundational, nondelegation_doctrine_absolute).
narrative_ontology:cs_axiom_status(nondelegation_doctrine_absolute, holdable).
narrative_ontology:cs_axiom_grounding('878a029a-635f-4803-afa3-164f4a163f7e', nondelegation_doctrine_absolute, conventional).
narrative_ontology:cs_axiom('878a029a-635f-4803-afa3-164f4a163f7e', foundational, separation_as_impermeable_boundary).
narrative_ontology:cs_axiom_status(separation_as_impermeable_boundary, holdable).
narrative_ontology:cs_axiom_grounding('878a029a-635f-4803-afa3-164f4a163f7e', separation_as_impermeable_boundary, deontological).
narrative_ontology:cs_reference_frame('878a029a-635f-4803-afa3-164f4a163f7e', strict_separation_framework).
narrative_ontology:cs_drift_state('878a029a-635f-4803-afa3-164f4a163f7e', contemporary_administrative_state, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('878a029a-635f-4803-afa3-164f4a163f7e', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, congressional_institutionalists).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, regulated_entities).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, administrative_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Execute and elaborate regulatory statutes passed by Congress. Under the formalist reading, their rulemaking and adjudicative actions that fill statutory gaps are constitutionally suspect, exposing them to judicial invalidation and stripping them of legislative-like authority. They cannot easily reconstitute their authority outside the constitutional framework; their survival depends on judicial tolerance or constitutional amendment.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, administrative_agencies, payer,
    institutional, generational, constrained, national).

% Interprets the Constitution to invalidate congressional delegations and agency actions that transgress strict branch boundaries. Under this reading, the judiciary actively polices the line between legislation and execution, gaining power to strike down statutes and rules. They can change doctrine but currently enforce formalism.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Members and institutional defenders of Congress who retain the formal monopoly on legislative power under this reading. They benefit from the prestige and authority of being the sole lawmaking body, though they may pay a cost in governance capacity.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, congressional_institutionalists, beneficiary,
    institutional, generational, constrained, national).

% Corporations and industries subject to agency regulation. They benefit from the regulatory paralysis that results when agencies cannot adapt rules to changing conditions, because Congress moves slowly and may lack technical capacity to legislate finely.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulated_entities, beneficiary,
    powerful, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__formalist_reading, diffuse).
narrative_ontology:fixing_cost_class(separation_of_powers_text__formalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves democratic accountability by ensuring that only the directly elected legislature makes law, preventing unelected bureaucrats from exercising legislative power and maintaining a clear chain of responsibility to voters.
% TRANSFER_FUNCTION: Transfers adaptive governance capacity away from expert administrative agencies back to a Congress that may lack the time, expertise, or political will to legislate finely, and toward courts that gain authority to invalidate rules.
% ABSENT_VOICES: Administrative law scholars and agency staff who view intelligible-principle delegation as constitutionally necessary for modern governance are excluded from the interpretive framework; their functionalist arguments are ruled out a priori by the formalist premise.
% DISAPPEARANCE_RATIONALE: If the formalist reading disappeared, the administrative state would operate under broad delegations with intelligible principles; agencies would promulgate rules without fear of wholesale constitutional invalidation on separation-of-powers grounds, and Congress would resume delegating technical lawmaking to expert bodies.
% FOUNDING_PROBLEM: The founding generation feared concentration of power and unaccountable rule by unelected officials; the formalist reading was built to prevent legislative power from migrating to the executive branch or administrative apparatus.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding corroborate the anti-monarchical, anti-concentration intent, but contemporary public-administration scholars and many legislators from outside the formalist movement attest that the problem has been reframed: modern complexity requires delegated expertise, and the formalist cure is worse than the disease.
narrative_ontology:disappearance_verdict(separation_of_powers_text__formalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__formalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__formalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__formalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__formalist_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the formalist reading invalidates the core operating mechanism of the administrative state, transferring adaptive lawmaking capacity either to a gridlocked Congress or to void. Suppression is very high (0.88) because the constraint forecloses the functionalist alternative (intelligible-principle delegation) that agencies and Congress rely upon; judicial doctrine actively suppresses that alternative. Theater is low-moderate (0.25) because formalist arguments are sincerely held jurisprudential positions, though performative originalism can inflate the ratio. Accessibility collapse is high (0.75) because once the formalist frame is accepted, functionalist agency governance appears constitutionally illegitimate. Resistance is substantial (0.70) because the administrative state, its scholarly defenders, and congressional delegators actively resist the reading.
 *
 * PERSPECTIVAL GAP:
 *   The agency seat experiences the constraint as pure extraction â its core functions are constitutionally suspect. The judiciary seat experiences it as legitimate enforcement of the founding charter. Congressional institutionalists experience it as a restoration of their constitutional role. Regulated entities experience it as a welcome reduction in adaptive rulemaking. These divergences are structurally predicted by the directionality derivation: agencies are declared victims with constrained exit, while regulated entities are beneficiaries with mobile exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (congressional_institutionalists, regulated_entities) derive low directionality â the constraint subsidizes their position (retained prestige, reduced regulatory burden). Victims (administrative_agencies) derive high directionality â the constraint extracts their governance capacity. The judiciary, as agenda_setter, sits between: it gains authority from enforcement but does not collect material rents, so it is not declared a beneficiary in the receipt sense.
 *
 * MANDATROPHY ANALYSIS:
 *   The formalist reading prevents mandatrophy mislabeling by preserving its genuine coordination claim: democratic accountability and the avoidance of arbitrary bureaucratic power. A snare classification would erase that claim, implying the coordination story is pure cover. A rope classification would erase the asymmetric extraction from agencies. Tangled_rope captures both: the coordination function (accountability) is real, but the same structure that delivers it also extracts massively from the administrative apparatus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_formalist_boundary,
    'Is the strict separation of powers a natural structural feature of the constitutional text, or a constructed formalist reading that benefits specific institutional actors?',
    'Historical-linguistic analysis of the founding-era text and practice, cross-referenced with institutionalist accounts of which actors gain authority under the reading.',
    'If the boundary is constructed rather than natural, the constraint''s legitimacy shifts from Mountain-like to contingent, strengthening the Tangled Rope classification and weakening its immunity from revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_formalist_boundary, conceptual, 'Whether formalist separation is inherent in the text or a constructed reading').

omega_variable(
    delegation_necessity_empirical,
    'Can modern governance function without broad delegation, or does the formalist reading produce systemic incapacity?',
    'Comparative analysis of governance output in jurisdictions with strict nondelegation norms versus those with permissive delegation, measured by regulatory latency and policy failure rates.',
    'If modern governance cannot function without delegation, the coordination claim is undermined and the constraint trends toward Snare; if it can, the coordination claim is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_necessity_empirical, empirical, 'Empirical necessity of delegation for modern governance').

omega_variable(
    agency_self_censorship_vs_judicial_strike,
    'Is the suppression of agency authority driven primarily by active judicial invalidation (structural) or by anticipatory self-censorship by agencies (internalized)?',
    'Compare rates of judicial invalidation on nondelegation grounds to rates of abandoned or narrowed rulemakings citing constitutional risk.',
    'If internalized, effective extraction exceeds the structural measure because agencies constrain themselves even when courts might not act.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_self_censorship_vs_judicial_strike, empirical, 'Structural judicial suppression versus internalized agency self-censorship').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sop_text_formalist_tr_t0, separation_of_powers_text__formalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sop_text_formalist_tr_t4, separation_of_powers_text__formalist_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(sop_text_formalist_tr_t8, separation_of_powers_text__formalist_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(sop_text_formalist_tr_t12, separation_of_powers_text__formalist_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(sop_text_formalist_tr_t16, separation_of_powers_text__formalist_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(sop_text_formalist_tr_t20, separation_of_powers_text__formalist_reading, theater_ratio, 20, 0.25).

% Extraction over time
narrative_ontology:measurement(sop_text_formalist_be_t0, separation_of_powers_text__formalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sop_text_formalist_be_t4, separation_of_powers_text__formalist_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(sop_text_formalist_be_t8, separation_of_powers_text__formalist_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(sop_text_formalist_be_t12, separation_of_powers_text__formalist_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(sop_text_formalist_be_t16, separation_of_powers_text__formalist_reading, base_extractiveness, 16, 0.78).
narrative_ontology:measurement(sop_text_formalist_be_t20, separation_of_powers_text__formalist_reading, base_extractiveness, 20, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(sop_text_formalist_su_t0, separation_of_powers_text__formalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sop_text_formalist_su_t4, separation_of_powers_text__formalist_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(sop_text_formalist_su_t8, separation_of_powers_text__formalist_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(sop_text_formalist_su_t12, separation_of_powers_text__formalist_reading, suppression_requirement, 12, 0.78).
narrative_ontology:measurement(sop_text_formalist_su_t16, separation_of_powers_text__formalist_reading, suppression_requirement, 16, 0.84).
narrative_ontology:measurement(sop_text_formalist_su_t20, separation_of_powers_text__formalist_reading, suppression_requirement, 20, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, unitary_executive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the separation_of_powers_text constraint family. The kernel (the constitutional text and early practice) is read by formalists as requiring strict boundaries, by functionalists as permitting flexible delegation, and by unitary-executive theorists as concentrating executive control. Each reading emits a distinct Îµ, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
