% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Interpretation
 *   domain: constitutional/law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the originalist reading of the
 *   us_constitution_interpretive kernel: the claim that constitutional
 *   meaning was fixed at ratification and interpretive authority derives from
 *   fidelity to framers' intent or original public meaning. It operates as a
 *   binding interpretive methodology in the federal judiciary, constraining
 *   which constitutional arguments are cognizable and which policy outcomes
 *   are constitutionally available. The constraint is claimed as a neutral
 *   legal method (rope-like coordination against arbitrary judging) while the
 *   authored metrics describe asymmetric extraction from modern governance
 *   and unenumerated rights. The engine measures that divergence; the metrics
 *   and claim are authored independently.
 *
 * KEY AGENTS:
 *   - originalist_judiciary: Primary agenda_setter (institutional/analytical) â administers the interpretive constraint through constitutional doctrine and precedent.
 *   - originalist_legal_network: Secondary beneficiary (organized/mobile) â captures institutional prestige and appointment influence.
 *   - federalism_advocates, religious_liberty_claimants, property_rights_defenders: Primary beneficiaries (organized to powerful) â receive policy space and doctrinal shelter.
 *   - unenumerated_rights_claimants: Primary target (powerless/trapped) â bear extraction through foregone constitutional protections.
 *   - federal_regulatory_expansion_advocates: Secondary target (institutional/constrained) â bear extraction through narrowed federal power.
 *   - living_constitutionalist_jurists: Excluded seat (institutional/constrained) â structurally absent from originalist courts despite academic prominence.
 *   - constitutional_historians: Analytical observer (analytical/analytical) â tests empirical claims without doctrinal authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.62).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.58).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "Originalist Reading of Constitutional Interpretation").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "constitutional/law/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, 'fb28cdb1-683e-41fa-9b37-aa3a704edf11').
narrative_ontology:cs_kernel_codification('fb28cdb1-683e-41fa-9b37-aa3a704edf11', fixed_text).
narrative_ontology:cs_authority_grounding('fb28cdb1-683e-41fa-9b37-aa3a704edf11', lineage).
narrative_ontology:cs_interpretation_layer_present('fb28cdb1-683e-41fa-9b37-aa3a704edf11').
narrative_ontology:cs_reading_relation('fb28cdb1-683e-41fa-9b37-aa3a704edf11', us_constitution_interpretive__living_constitution_reading, forecloses).
narrative_ontology:cs_reading_relation('fb28cdb1-683e-41fa-9b37-aa3a704edf11', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('fb28cdb1-683e-41fa-9b37-aa3a704edf11', foundational, communicative_content_fixed_at_enactment).
narrative_ontology:cs_axiom_status(communicative_content_fixed_at_enactment, holdable).
narrative_ontology:cs_axiom_grounding('fb28cdb1-683e-41fa-9b37-aa3a704edf11', communicative_content_fixed_at_enactment, conventional).
narrative_ontology:cs_axiom('fb28cdb1-683e-41fa-9b37-aa3a704edf11', foundational, interpreter_bound_by_original_meaning).
narrative_ontology:cs_axiom_status(interpreter_bound_by_original_meaning, holdable).
narrative_ontology:cs_axiom_grounding('fb28cdb1-683e-41fa-9b37-aa3a704edf11', interpreter_bound_by_original_meaning, deontological).
narrative_ontology:cs_reference_frame('fb28cdb1-683e-41fa-9b37-aa3a704edf11', original_public_meaning_1787).
narrative_ontology:cs_drift_state('fb28cdb1-683e-41fa-9b37-aa3a704edf11', contemporary_administrative_state, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fb28cdb1-683e-41fa-9b37-aa3a704edf11', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, originalist_legal_network).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises judicial review by interpreting constitutional text according to original public meaning or framers' intent. Controls the authoritative application of the constraint through opinions, precedent, and constitutional doctrine. Their methodological commitment constrains which historical sources and arguments count as legitimate.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Federalist Society and affiliated institutions that select, train, and promote originalist jurists. Benefits from institutional prestige, funding, judicial-clerkship pipelines, and sustained influence over federal appointments.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_legal_network, beneficiary,
    organized, generational, mobile, national).

% State sovereignty and limited-federal-power proponents who benefit from originalist structural limits on federal commerce, spending, and enforcement powers. Their policy preferences gain doctrinal footing when federal power is narrowly construed.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    organized, biographical, mobile, national).

% Litigants seeking exemptions or protections under originalist interpretations of the Free Exercise and Establishment Clauses. Originalist historical frameworks often yield broader religious accommodations than secular-framework or evolving-standards readings.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, religious_liberty_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Landowners and commercial actors who benefit from narrow readings of the Takings Clause, limited regulatory takings doctrine, and constraints on federal economic regulation under originalist frameworks.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    powerful, biographical, mobile, national).

% Individuals seeking constitutional protection for privacy, reproductive autonomy, intimate association, or dignity under substantive due process or Ninth Amendment theories. Originalism treats these claims as jurisprudentially illegitimate because the rights are not enumerated in the text or historically established in 1787 or 1868.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Administrative agencies and progressive legislators pursuing modern regulatory, welfare, and environmental programs. Constrained by originalist limits on delegation, commerce power, and federal spending that narrow the constitutional space for New Deal-style and post-New Deal governance.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    institutional, generational, constrained, national).

% Judges and scholars whose interpretive methodology treats constitutional meaning as evolving with societal values. Structurally excluded from originalist courts; their arguments are ruled out of bounds in originalist jurisprudence despite significant academic prominence and historical judicial usage.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, living_constitutionalist_jurists, excluded,
    institutional, generational, constrained, national).

% Professional historians assessing the empirical accuracy of originalist historical claims about 1787 or 1868 meaning. Neither collect rents nor bear costs under the constraint; provide external epistemic testing of the originalist narrative without institutional authority to alter doctrine.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes constitutional interpretation around a fixed historical referent, reducing judicial discretion and preventing constitutional meaning from fluctuating with transient judicial majorities or political pressures.
% TRANSFER_FUNCTION: Moves interpretive authority from contemporary democratic majorities and adaptive courts to the understood communicative content of the ratified text; moves policy space from federal regulators and unenumerated rights seekers to state governments, enumerated rights holders, and historical proprietors.
% ABSENT_VOICES: Living constitutionalist jurists and popular constitutionalism advocates are structurally excluded from originalist courts; their interpretive methods are treated as jurisprudentially illegitimate, and their preferred constitutional outcomes lack doctrinal footing in originalist majorities.
% DISAPPEARANCE_RATIONALE: If originalism vanished overnight, federal courts would revert to evolving-standards or prudential modes of interpretation, unenumerated rights would likely regain doctrinal footing, federal regulatory power would expand, and the current conservative majority's constitutional holdings would lack methodological grounding.
% FOUNDING_PROBLEM: Judicial activism and lack of objective constraint on unelected judges interpreting open-textured constitutional provisions; fear that constitutional meaning was being invented rather than discovered by courts.
% FOUNDING_PROBLEM_CORROBORATION: Originalist jurists attest the problem is still live, citing Roe and the administrative state as judicial inventions. Progressive legal historians and living constitutionalist scholars attest the problem was manufactured as a political reaction to the New Deal and Warren Court; they corroborate that the administrative state and modern rights frameworks were stable constitutional settlements before originalism's late-twentieth-century revival.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__originalist_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the constraint systematically transfers policy space from modern regulatory and rights frameworks to 18th- and 19th-century understandings, with material consequences for governance. Suppression (0.58) is moderate-high because the constraint's persistence depends on active gatekeeping: judicial appointments, law-school orthodoxy, and the professional exclusion of non-originalist methodologies. Theater ratio (0.45) reflects considerable performative historical argumentation that professional historians frequently contest. Accessibility collapse (0.60) is moderate-high: within originalist courts, non-originalist arguments collapse as viable legal moves, even though they flourish in the broader legal academy. Resistance (0.58) is moderate: sustained academic and political opposition from progressive scholars and democratic legislators. The temporal series tracks originalism's trajectory from academic revival (t=0) through judicial institutionalization (t=40) to a plateau where enforcement is mature and pushback has emerged.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (originalist judiciary) experiences the constraint as fidelity to law and neutral restraint on arbitrary power. The payer seats (unenumerated rights claimants, federal regulators) experience the same constraint as the entrenchment of 18th-century power distributions and the foreclosure of modern democratic adaptation. The beneficiary seats experience doctrinal shelter. The excluded seat (living constitutionalist jurists) experiences it as a jurisprudential wall. The engine computes these divergent seat types from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (federalism advocates, religious liberty claimants, property rights defenders, originalist legal network) receive low directionality because the constraint subsidizes their preferred outcomes and institutional position. Victims (unenumerated rights claimants, federal regulatory expansion advocates) receive high directionality because the constraint extracts from their policy space and constitutional claims. The originalist judiciary sits near the beneficiary end in terms of institutional power and analytical exit, but their directionality is structurally complex because they both administer and are bound by the method. No override is needed because the structural derivation captures the relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â constraining judicial activism â was arguably live in the Warren Court era. However, the arrangement has persisted and intensified beyond the initial problem, becoming a vehicle for conservative policy entrenchment. The coordination function (genuine restraint on arbitrary judging) prevents classification as a pure snare, while the asymmetric extraction (transferring power to states and historical proprietors) prevents classification as a pure rope. Tangled rope is the structurally accurate classification: both coordination and extraction are real, operating through the same doctrinal structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'How does the originalist_reading of the us_constitution_interpretive kernel differ structurally from the living_constitution_reading and popular_constitutionalism_reading, and what would change if either sibling reading were adopted instead?',
    'Comparison of the compiled constraint stories for all three readings in the kernel family; analysis of beneficiary-victim structure and epsilon divergence across readings.',
    'If sibling readings were adopted, the beneficiary-victim structure would invert or redistribute: federal regulatory power and unenumerated rights would shift from victim to beneficiary status, and the extraction profile would migrate to different seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'This constraint is one reading of a contested kernel; epsilon is stable only under this specific reading.').

omega_variable(
    historical_recoverability_of_meaning,
    'Can original public meaning be recovered with sufficient precision to genuinely constrain modern constitutional cases, given incomplete historical records, linguistic change, and contested historical methodology?',
    'Advances in corpus linguistics, archival history, and empirical studies of 18th- and 19th-century language usage; external audit of originalist historical claims by professional historians.',
    'If original meaning is unrecoverable or systematically ambiguous, the constraint functions as an interpretive aesthetic rather than a genuine legal restraint, increasing theater_ratio and weakening the coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_recoverability_of_meaning, empirical, 'Whether the historical foundation of originalism is empirically adequate to support its jurisprudential weight.').

omega_variable(
    originalism_neutrality,
    'Does originalism operate as a politically neutral method of interpretation, or does it systematically privilege conservative political outcomes across domains?',
    'Longitudinal outcome analysis comparing originalist decisions to non-originalist decisions across a range of constitutional issues, controlling for judicial ideology and case selection.',
    'If originalism is systematically outcome-predictable along ideological lines, the coordination function (neutral judicial restraint) is partly cover for partisan extraction, pushing the effective classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_neutrality, empirical, 'Whether originalism is a neutral method or an outcome-determinative ideology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_const_orig_tr_t0, us_constitution_interpretive__originalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(us_const_orig_tr_t10, us_constitution_interpretive__originalist_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(us_const_orig_tr_t20, us_constitution_interpretive__originalist_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(us_const_orig_tr_t30, us_constitution_interpretive__originalist_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(us_const_orig_tr_t40, us_constitution_interpretive__originalist_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(us_const_orig_tr_t50, us_constitution_interpretive__originalist_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(us_const_orig_be_t0, us_constitution_interpretive__originalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(us_const_orig_be_t10, us_constitution_interpretive__originalist_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(us_const_orig_be_t20, us_constitution_interpretive__originalist_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(us_const_orig_be_t30, us_constitution_interpretive__originalist_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(us_const_orig_be_t40, us_constitution_interpretive__originalist_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(us_const_orig_be_t50, us_constitution_interpretive__originalist_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(us_const_orig_su_t0, us_constitution_interpretive__originalist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(us_const_orig_su_t10, us_constitution_interpretive__originalist_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(us_const_orig_su_t20, us_constitution_interpretive__originalist_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(us_const_orig_su_t30, us_constitution_interpretive__originalist_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(us_const_orig_su_t40, us_constitution_interpretive__originalist_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(us_const_orig_su_t50, us_constitution_interpretive__originalist_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
