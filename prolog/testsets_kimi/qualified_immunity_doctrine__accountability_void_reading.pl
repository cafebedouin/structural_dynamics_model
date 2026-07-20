% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__accountability_void_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__accountability_void_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity Doctrine â Accountability Void Reading
 *   domain: constitutional/law_enforcement/civil_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the accountability_void reading of the
 *   qualified immunity doctrine kernel. It treats the doctrine not as a
 *   necessary protection for law enforcement but as a systematically
 *   extractive mechanism that guarantees impunity for constitutional
 *   violations. Under this reading, the judiciary actively enforces a
 *   near-absolute bar to civil liability, stripping Congress's statutory
 *   remedy of practical effect and concentrating the costs of official
 *   misconduct on victims who lack alternative recourse.
 *
 * KEY AGENTS:
 *   - shielded_officers: Primary beneficiary (organized/mobile) â absorbs avoided liability costs
 *   - constitutional_violation_victims: Primary target (powerless/trapped) â bears uncompensated constitutional injury with no remedy path
 *   - federal_judiciary: Agenda-setter (institutional/analytical) â administers the doctrine through precedent and gatekeeping
 *   - civil_rights_litigation_bar: Observer (organized/constrained) â resists the doctrine but loses systematically
 *   - congressional_reformers: Observer (institutional/analytical) â seeks statutory abolition without success
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, 0.88).
domain_priors:suppression_score(qualified_immunity_doctrine__accountability_void_reading, 0.82).
domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity Doctrine â Accountability Void Reading").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "constitutional/law_enforcement/civil_rights").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__accountability_void_reading, '813513f7-0044-4b32-93a5-bab78f153ce7').
narrative_ontology:cs_kernel_codification('813513f7-0044-4b32-93a5-bab78f153ce7', formalized).
narrative_ontology:cs_authority_grounding('813513f7-0044-4b32-93a5-bab78f153ce7', lineage).
narrative_ontology:cs_interpretation_layer_present('813513f7-0044-4b32-93a5-bab78f153ce7').
narrative_ontology:cs_reading_relation('813513f7-0044-4b32-93a5-bab78f153ce7', qualified_immunity_doctrine__protective_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('813513f7-0044-4b32-93a5-bab78f153ce7', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('813513f7-0044-4b32-93a5-bab78f153ce7', foundational, impunity_is_the_operative_output).
narrative_ontology:cs_axiom_status(impunity_is_the_operative_output, holdable).
narrative_ontology:cs_axiom_grounding('813513f7-0044-4b32-93a5-bab78f153ce7', impunity_is_the_operative_output, empirically_contingent).
narrative_ontology:cs_axiom('813513f7-0044-4b32-93a5-bab78f153ce7', secondary, constitutional_remedy_is_illusory_for_victims).
narrative_ontology:cs_axiom_status(constitutional_remedy_is_illusory_for_victims, holdable).
narrative_ontology:cs_axiom_grounding('813513f7-0044-4b32-93a5-bab78f153ce7', constitutional_remedy_is_illusory_for_victims, empirically_contingent).
narrative_ontology:cs_reference_frame('813513f7-0044-4b32-93a5-bab78f153ce7', constitutional_tort_liability_default).
narrative_ontology:cs_drift_state('813513f7-0044-4b32-93a5-bab78f153ce7', contemporary_qualified_immunity_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('813513f7-0044-4b32-93a5-bab78f153ce7', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, shielded_officers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, constitutional_violation_victims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are shielded from personal financial liability for constitutional violations under the qualified immunity doctrine. Benefit from dismissal of civil rights claims before discovery or trial. Face no personal monetary consequences for unconstitutional conduct unless a court finds an identical prior precedent, and even then immunity is often granted. The constraint subsidizes their professional risk.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, shielded_officers, beneficiary,
    organized, biographical, mobile, national).

% Bear the uncompensated costs of constitutional violations. When courts grant qualified immunity, their claims are dismissed, often without discovery, leaving them to absorb medical expenses, lost income, physical injury, and dignitary harm. They cannot access the remedy Congress created in 42 U.S.C. Â§ 1983 because the judiciary has layered the immunity barrier above it.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, constitutional_violation_victims, payer,
    powerless, immediate, trapped, national).

% Creates, maintains, and applies the qualified immunity doctrine through appellate precedent and Supreme Court case law. Controls the gatekeeping function by determining whether a constitutional right was clearly established at the time of the violation. The doctrine persists through judicial choice, stare decisis, and the objective reasonableness test invented in Harlow v. Fitzgerald.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Represents constitutional tort plaintiffs and advocates for abolition of qualified immunity in courts, Congress, and public discourse. Argues that the doctrine contradicts the text and history of Â§ 1983. Faces systematic dismissal of client claims and has been unable to persuade the federal judiciary to overrule the doctrine.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_litigation_bar, observer,
    organized, generational, constrained, national).

% Members of Congress who have introduced legislation to abolish or modify qualified immunity. Occupy an analytical and legislative seat seeking to restore the statutory liability regime through federal statute, but have been unable to overcome procedural hurdles and opposition from law enforcement interest groups.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, congressional_reformers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__accountability_void_reading, shielded_officers).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__accountability_void_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, there is no genuine coordination problem solved by the doctrine. The asserted need to protect officers from bad-faith litigation to enable vigorous policing is treated as a pretext rather than a real coordination function. The actual function is the systematic extraction of impunity from the constitutional remediation system.
% TRANSFER_FUNCTION: Moves the legal and financial costs of constitutional violations from shielded officers (and the state apparatus that supports them) to victims, who absorb the injury without monetary or dignitary recovery. Also transfers accountability itself â the deterrent and expressive functions of tort liability are nullified and absorbed by the violated party.
% ABSENT_VOICES: Victims whose claims are dismissed on qualified immunity grounds before discovery are structurally absent from the appellate record; their experiences of unconstitutional conduct rarely reach a jury or even full factual development. Taxpayers who fund municipal indemnification and defense costs are also excluded from the judicial calculus.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, thousands of previously barred constitutional tort suits would proceed past the pleading stage, officers would face personal liability exposure for the first time in decades, municipalities would renegotiate indemnification insurance and training budgets, and the civil rights litigation docket would expand dramatically. The legal world would rearrange around a revived Â§ 1983 remedial regime.
% FOUNDING_PROBLEM: The doctrine was judicially invented in Pierson v. Ray (1967) and transformed in Harlow v. Fitzgerald (1982) to protect government officials from personal liability and unwarranted distraction, replacing common-law good-faith defenses with an objective immunity test.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights plaintiffs' attorneys and legal scholars attest the original problem of bad-faith litigation against officers was never empirically substantiated at the scale the doctrine now addresses. Empirical studies of Â§ 1983 litigation rates from outside the judiciary and police unions show no epidemic of frivolous suits against officers that would justify the modern scope of the immunity. No independent non-beneficiary source corroborates the founding problem at its current scale.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__accountability_void_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__accountability_void_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__accountability_void_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__accountability_void_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__accountability_void_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the doctrine blocks the vast majority of constitutional tort claims before discovery, decoupling officer conduct from legal consequence. Suppression is high (0.82) because the constraint's persistence depends on judicial enforcement that excludes the statutory remedy Congress enacted. Theater ratio is moderate-high (0.55): courts perform elaborate 'clearly established law' analyses that produce predictable immunity grants, signaling that a growing share of doctrinal activity is performative maintenance of the extraction structure rather than genuine rights-adjudication. Resistance at 0.68 reflects sustained but unsuccessful opposition from civil rights advocates and legislative reformers. Accessibility collapse at 0.75 indicates that once the doctrine is understood, the civil litigation path nearly collapses, though political alternatives remain weakly available.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary and shielded officers experience the constraint as a necessary and legitimate feature of constitutional litigation â a protection without which officials could not function. Victims experience the identical doctrinal structure as a sealed door to the courthouse. The engine computes this divergence from the structural data: beneficiaries face near-zero effective extraction while targets face severe extraction amplified by trapped exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Shielded officers are declared beneficiaries â the constraint subsidizes them by eliminating liability risk, pushing their directionality toward the beneficiary pole. Constitutional violation victims are declared victims â they bear the extraction directly through uncompensated injury and barred remedies, pushing their directionality toward the full-target pole. The federal judiciary sits as agenda-setter with analytical exit options, experiencing low personal extraction while administering the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the doctrine as a protective scaffold or rope by requiring identifiable victims with no remedy path and a beneficiary set that collects avoided liability. The protective_scaffold reading would need to show genuine coordination (officers protected from frivolous suits) without asymmetric extraction (v uncompensated victims). Because victims exist and the transfer function moves harm to them rather than risk away from officers, the accountability_void reading classifies as snare rather than scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clearly_established_law_indeterminacy,
    'Does the ''clearly established law'' prong function as a neutral legal standard or as a discretionary extraction mechanism?',
    'Empirical analysis of qualified immunity grant rates correlated with circuit, judicial identity, and plaintiff demographics; comparative study of state and federal jurisdictions that have abolished or modified the doctrine.',
    'If the standard is structurally indeterminate and outcome-correlated with non-legal factors, the snare classification strengthens. If it operates as a coherent rule-like constraint with predictable outputs, the classification may shift toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearly_established_law_indeterminacy, empirical, 'Indeterminacy of the clearly established law test').

omega_variable(
    kernel_reading_validity,
    'Does the accountability_void reading capture the operative structure of qualified immunity, or does the protective_scaffold reading more accurately describe its function?',
    'Cross-jurisdictional natural experiments comparing jurisdictions with and without qualified immunity; empirical study of litigation rates, officer behavior, and indemnification costs in states that have abolished the doctrine.',
    'If the protective_scaffold reading is more descriptively accurate, this constraint should be reclassified as scaffold or rope rather than snare. If the accountability_void reading holds, the extraction classification and high epsilon remain warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_validity, conceptual, 'Contested kernel reading between accountability void and protective scaffold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__accountability_void_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qi_av_tr_t0, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qi_av_tr_t10, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(qi_av_tr_t20, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(qi_av_tr_t30, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(qi_av_tr_t40, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(qi_av_tr_t50, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(qi_av_be_t0, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(qi_av_be_t10, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(qi_av_be_t20, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(qi_av_be_t30, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(qi_av_be_t40, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(qi_av_be_t50, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 50, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(qi_av_su_t0, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(qi_av_su_t10, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(qi_av_su_t20, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(qi_av_su_t30, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(qi_av_su_t40, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(qi_av_su_t50, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__accountability_void_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__constitutional_fidelity_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'qualified immunity' conflates three structurally distinct constraints: an accountability void (snare), a protective scaffold (scaffold), and an illegitimate judicial invention (commitment-system challenge). Each reading carries a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
