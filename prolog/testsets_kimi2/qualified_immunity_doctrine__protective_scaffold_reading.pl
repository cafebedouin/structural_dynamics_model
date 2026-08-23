% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__protective_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__protective_scaffold_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__protective_scaffold_reading
 *   human_readable: Qualified Immunity Doctrine â Protective Scaffold Reading
 *   domain: constitutional/law_enforcement/civil_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the protective_scaffold_reading of the
 *   qualified_immunity_doctrine kernel. The reading frames qualified immunity
 *   as a necessary judicial protection that enables government
 *   officersâparticularly law enforcementâto perform discretionary duties
 *   vigorously without paralyzing fear of personal damages liability and
 *   bad-faith litigation. From this seat, the doctrine solves a genuine
 *   coordination problem (maintaining effective public administration and law
 *   enforcement) while asymmetrically externalizing litigation costs and
 *   remedy denial onto victims of constitutional violations. The sibling
 *   readings are accountability_void_reading (pure extraction framing) and
 *   constitutional_fidelity_reading (illegitimacy framing).
 *
 * KEY AGENTS:
 *   - federal_judiciary: Agenda-setter (institutional/analytical) â administers the 'clearly established law' test and controls doctrinal evolution
 *   - law_enforcement_officers: Beneficiary (organized/constrained) â shielded from personal liability; cannot opt out of the protection but benefit from it
 *   - civil_rights_plaintiffs: Payer (powerless/trapped) â bear uncompensated constitutional harms and litigation costs when claims are dismissed pre-merits
 *   - civil_rights_bar: Excluded (organized/trapped) â attorneys screened out by early dismissal; would object if present in the rulemaking locus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, 0.58).
domain_priors:suppression_score(qualified_immunity_doctrine__protective_scaffold_reading, 0.72).
domain_priors:theater_ratio(qualified_immunity_doctrine__protective_scaffold_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__protective_scaffold_reading, tangled_rope).
narrative_ontology:human_readable(qualified_immunity_doctrine__protective_scaffold_reading, "Qualified Immunity Doctrine â Protective Scaffold Reading").
narrative_ontology:topic_domain(qualified_immunity_doctrine__protective_scaffold_reading, "constitutional/law_enforcement/civil_rights").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__protective_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__protective_scaffold_reading, 'f9b8d750-e638-4e9a-90d1-dc88a50280f8').
narrative_ontology:cs_kernel_codification('f9b8d750-e638-4e9a-90d1-dc88a50280f8', formalized).
narrative_ontology:cs_authority_grounding('f9b8d750-e638-4e9a-90d1-dc88a50280f8', lineage).
narrative_ontology:cs_interpretation_layer_present('f9b8d750-e638-4e9a-90d1-dc88a50280f8').
narrative_ontology:cs_reading_relation('f9b8d750-e638-4e9a-90d1-dc88a50280f8', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9b8d750-e638-4e9a-90d1-dc88a50280f8', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('f9b8d750-e638-4e9a-90d1-dc88a50280f8', foundational, immunity_required_for_vigorous_enforcement).
narrative_ontology:cs_axiom_status(immunity_required_for_vigorous_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('f9b8d750-e638-4e9a-90d1-dc88a50280f8', immunity_required_for_vigorous_enforcement, empirically_contingent).
narrative_ontology:cs_axiom('f9b8d750-e638-4e9a-90d1-dc88a50280f8', secondary, indemnification_inadequate_alternative).
narrative_ontology:cs_axiom_status(indemnification_inadequate_alternative, holdable).
narrative_ontology:cs_axiom_grounding('f9b8d750-e638-4e9a-90d1-dc88a50280f8', indemnification_inadequate_alternative, empirically_contingent).
narrative_ontology:cs_reference_frame('f9b8d750-e638-4e9a-90d1-dc88a50280f8', common_law_officer_protection).
narrative_ontology:cs_drift_state('f9b8d750-e638-4e9a-90d1-dc88a50280f8', contemporary_rights_accountability_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f9b8d750-e638-4e9a-90d1-dc88a50280f8', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_plaintiffs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the qualified immunity doctrine by applying the 'clearly established law' test at the motion-to-dismiss and summary-judgment stages. Controls doctrinal evolution through appellate precedent and decides whether officers are shielded from damages liability under 42 U.S.C. Â§1983.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Are shielded from personal monetary damages and the litigation costs of trial when courts find no 'clearly established' precedent matching their conduct. Cannot waive the protection; it attaches as a matter of judicial doctrine. Benefit from reduced liability exposure while performing discretionary duties.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers, beneficiary,
    organized, biographical, constrained, national).

% Bear uncompensated constitutional harms and litigation costs when their Â§1983 claims are dismissed pre-merits on qualified immunity grounds. Cannot bypass the doctrine by stipulating to facts or waiving damages; the screen is controlled by the court. No alternative federal remedy exists for the constitutional violation once immunity is granted.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_plaintiffs, payer,
    powerless, immediate, trapped, national).

% Would represent plaintiffs in merits litigation but are screened out by early dismissal. Their objectionâthat the doctrine denies clients remedy for egregious violationsâis not heard in the judicial forum that maintains the rule, because the motion practice excludes trial-level development of the record.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_bar, excluded,
    organized, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__protective_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a chilling effect on discretionary government action by shielding officers from the threat of personal damages liability and burdensome litigation, thereby preserving vigorous law enforcement and public administration.
% TRANSFER_FUNCTION: Transfers the cost of constitutional violations from officers (and the governments that employ them) to the victims, who absorb the uncompensated harm and bear litigation expenses for suits dismissed before trial.
% ABSENT_VOICES: Civil rights plaintiffs whose claims are dismissed before discovery; state legislators who have enacted statutory alternatives but lack authority to alter federal common-law doctrine; and criminal defendants subjected to unconstitutional conduct that lacks a precisely on-point precedent mirror.
% DISAPPEARANCE_RATIONALE: Officers would face personal exposure to damages under Â§1983; municipalities would expand indemnification and insurance regimes; litigation volume would increase as more claims reached merits discovery; and the current equilibrium where most excessive-force and wrongful-search suits are dismissed pre-trial would collapse.
% FOUNDING_PROBLEM: The threat of personal liability and voluminous insubstantial litigation against government officers performing discretionary functions could deter vigorous law enforcement and public administration.
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement unions and the Department of Justice attest the problem remains live, citing officer recruitment and retention risks. Civil rights organizations and a minority of sitting federal judges attest the problem is overstated or solved by near-universal municipal indemnification; empirical scholarship outside the beneficiary set finds little evidence of the chilling effect that justified the doctrine's expansion.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__protective_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__protective_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__protective_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the doctrine genuinely reduces litigation volume and liability exposure for officers, but the 'clearly established' prong operates as a coarse screen that denies remedy even for egregious violations lacking precise factual precedent. Suppression is high (0.72) because the constraint collapses the alternative remedy path (damages under Â§1983) through judge-made procedural thresholds; plaintiffs cannot bypass the screen. Theater ratio is moderate-high (0.48) because courts perform elaborate 'clearly established' analyses that often function as ritualized outcomes in favor of officers, especially in excessive-force cases. Accessibility collapse is high (0.78) because once a plaintiff understands the precedent-matching requirement, the likelihood of merits resolution collapses. Resistance is moderate (0.52) because state legislative reforms, academic criticism, and dissenting opinions actively contest the doctrine, but the Supreme Court majority has not budged.
 *
 * PERSPECTIVAL GAP:
 *   The officer seat experiences the constraint as protective coordination (d near beneficiary); the plaintiff seat experiences it as extraction (d near target). The federal judiciary experiences it as an interpretive tool that balances competing values (d near symmetric but agenda-setting). The engine computes this divergence from structural data: officers are declared beneficiaries with constrained exit (they cannot waive the protection), plaintiffs are declared victims with trapped exit (no alternative remedy), and the judiciary holds institutional power with analytical exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are law_enforcement_officers: the constraint subsidizes them by removing a major cost and risk of office. Victims are civil_rights_plaintiffs: the constraint extracts from them by denying damages and shifting litigation costs onto them. The federal_judiciary is the agenda_setter that enforces the asymmetry through the 'clearly established' test; it does not collect the extraction but controls its distribution. Civil_rights_bar is excluded from the bargaining table and would bear the constraint's costs if admitted.
 *
 * MANDATROPHY ANALYSIS:
 *   The protective reading resists classification as pure snare because it names a live coordination function (preventing chilling effects on enforcement). However, the R5 genealogy interview documents that the founding problemâunindemnified personal liability chilling enforcementâis contested: empirical evidence outside the beneficiary set finds that officers are almost universally indemnified by their employers, suggesting the problem the doctrine was built to solve may be dead. If the founding problem is dead but the arrangement persists, the mandatrophy flag fires, signaling potential piton drift. The current metrics (moderate theater, rising extraction) do not yet indicate full piton status, but the temporal trajectory bears watching.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_chilling_effect,
    'Does the threat of personal liability without qualified immunity actually chill vigorous law enforcement, or is this effect adequately mitigated by existing indemnification and departmental representation?',
    'Comparative empirical study of officer behavior and departmental policy in jurisdictions that have statutorily limited or abolished qualified immunity.',
    'If no chilling effect is found, the coordination justification collapses and the constraint reads as extraction; if a real effect is demonstrated, it supports the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_chilling_effect, empirical, 'Whether the protective justification rests on a testable empirical claim').

omega_variable(
    protective_vs_accountability_framing,
    'Does the protective framing (officers as beneficiaries needing shielding) and the accountability framing (victims as targets denied remedy) identify the same constraint at different epsilon values, or do they imply different enforceable rules?',
    'Structural comparison of the two readings'' beneficiary/victim sets and base_extractiveness claims.',
    'If the epsilon values are substantially different, the readings instantiate different constraints per epsilon-invariance; if similar, they are observer-indexed perceptions of one constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protective_vs_accountability_framing, conceptual, 'Whether sibling readings describe one constraint or multiple constraints').

omega_variable(
    qi_kernel_reading_decomposition,
    'This constraint is the protective_scaffold_reading of the qualified_immunity_doctrine kernel. What structural element do the three sibling readings most sharply disagree on: the empirical necessity of the doctrine, the legitimacy of its judicial origin, or the distributive identity of its victims?',
    'Cross-reading audit of where each reading places epsilon, who it names as beneficiary, and what it treats as the constraint''s referent.',
    'Clarifies whether the kernel decomposes into a constraint family linked by network.affects_constraints or remains a single contested constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(qi_kernel_reading_decomposition, conceptual, 'Committer-frame uncertainty about kernel decomposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__protective_scaffold_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t0, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qual_tr_t10, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(qual_tr_t20, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(qual_tr_t30, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(qual_tr_t40, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 40, 0.46).
narrative_ontology:measurement(qual_tr_t50, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(qual_be_t0, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qual_be_t10, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(qual_be_t20, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(qual_be_t30, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(qual_be_t40, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement(qual_be_t50, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t0, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(qual_su_t10, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(qual_su_t20, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(qual_su_t30, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(qual_su_t40, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(qual_su_t50, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__protective_scaffold_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
