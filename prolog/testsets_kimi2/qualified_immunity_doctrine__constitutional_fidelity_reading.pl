% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__constitutional_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__constitutional_fidelity_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity Doctrine (Constitutional Fidelity Reading)
 *   domain: constitutional/law_enforcement/civil_rights
 *
 * SUMMARY:
 *   This constraint story captures the constitutional fidelity reading of the
 *   qualified immunity doctrine: the judicially invented rule that shields
 *   government officials from civil liability for constitutional violations
 *   unless the right was 'clearly established' at the time of conduct. From
 *   this reading, the doctrine is an illegitimate common-law fabrication that
 *   lacks authorization in the text of Section 1983, the Constitution, or any
 *   statute. It expands federal judicial power by giving judges gatekeeping
 *   authority over constitutional remedies, while denying both plaintiffs and
 *   officers a legitimate legal framework. The reading treats the doctrine as
 *   a hybrid coordination-extraction mechanism: it coordinates law
 *   enforcement by shielding officers, but its primary structural effect is
 *   the extraction of remedial rights from civil rights plaintiffs and the
 *   concentration of interpretive power in the federal judiciary.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Primary agenda-setter and beneficiary (institutional/identity_locked) â controls doctrine and expands power through docket gatekeeping
 *   - civil_rights_plaintiffs: Primary payer (powerless/trapped) â bear the loss of constitutional remedies
 *   - constitutional_dissent_judges: Analytical observers (moderate/analytical) â record illegitimacy without institutional power to change it
 *   - congress: Excluded institutional actor (institutional/constrained) â possesses legislative override capacity but remains sidelined by judicial common-law autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.82).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.78).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, tangled_rope).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity Doctrine (Constitutional Fidelity Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional/law_enforcement/civil_rights").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, '03094f80-3989-4dd1-b54e-c1281e64521d').
narrative_ontology:cs_kernel_codification('03094f80-3989-4dd1-b54e-c1281e64521d', fixed_text).
narrative_ontology:cs_authority_grounding('03094f80-3989-4dd1-b54e-c1281e64521d', lineage).
narrative_ontology:cs_interpretation_layer_present('03094f80-3989-4dd1-b54e-c1281e64521d').
narrative_ontology:cs_reading_relation('03094f80-3989-4dd1-b54e-c1281e64521d', qualified_immunity_doctrine__protective_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('03094f80-3989-4dd1-b54e-c1281e64521d', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_axiom('03094f80-3989-4dd1-b54e-c1281e64521d', foundational, constitutional_text_supreme_over_common_law_invention).
narrative_ontology:cs_axiom_status(constitutional_text_supreme_over_common_law_invention, holdable).
narrative_ontology:cs_axiom_grounding('03094f80-3989-4dd1-b54e-c1281e64521d', constitutional_text_supreme_over_common_law_invention, conventional).
narrative_ontology:cs_axiom('03094f80-3989-4dd1-b54e-c1281e64521d', foundational, remedial_rights_may_not_be_judicially_abrogated).
narrative_ontology:cs_axiom_status(remedial_rights_may_not_be_judicially_abrogated, holdable).
narrative_ontology:cs_axiom_grounding('03094f80-3989-4dd1-b54e-c1281e64521d', remedial_rights_may_not_be_judicially_abrogated, deontological).
narrative_ontology:cs_reference_frame('03094f80-3989-4dd1-b54e-c1281e64521d', constitutional_remedial_framework_1871).
narrative_ontology:cs_drift_state('03094f80-3989-4dd1-b54e-c1281e64521d', contemporary_qualified_immunity_regime, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('03094f80-3989-4dd1-b54e-c1281e64521d', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_plaintiffs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Created and maintains the qualified immunity doctrine through common-law adjudication, expanding institutional control over which constitutional claims reach discovery or trial. Derives docket-control authority and constitutional-interpretation supremacy from the doctrine's malleable 'clearly established law' test. Could abandon the doctrine via en banc or Supreme Court reversal but is institutionally incentivized to retain gatekeeping power.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, beneficiary).

% Individuals alleging constitutional violations by government officials. Must identify a 'clearly established' right with precise factual correspondence to survive a motion to dismiss; most claims are dismissed before discovery. No alternative federal forum exists for many constitutional torts after Bivens retrenchment. Cannot opt out of the doctrine; statutory and constitutional damages remedies are effectively nullified.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_plaintiffs, payer,
    powerless, immediate, trapped, national).

% Individual judges who write dissents and concurrences arguing the doctrine lacks historical or constitutional basis. They highlight the judicial fabrication of the doctrine but lack the votes to overturn it. Their observations enter the record without altering the constraint's enforcement.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_dissent_judges, observer,
    moderate, biographical, analytical, national).

% Possesses constitutional authority to legislate remedies for constitutional violations but has not enacted a statutory override of qualified immunity. Legislative proposals exist but are stalled by political polarization and law-enforcement opposition; the judiciary's assertion of common-law control sidelines congressional remedial power.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, congress, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__constitutional_fidelity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The doctrine purports to shield government officials from personal liability to enable vigorous execution of their duties without fear of debilitating litigation or second-guessing by courts and juries.
% TRANSFER_FUNCTION: Transfers the ability to obtain redress for constitutional violations from civil rights plaintiffs to the federal judiciary, which gains gatekeeping authority over constitutional claims, while officers receive cost-free dismissal protection.
% ABSENT_VOICES: Civil rights plaintiffs whose claims are dismissed on qualified immunity grounds without ever reaching discovery; congressional majorities that would legislate a statutory damages remedy but are pre-empted by judicial doctrine; officers who would prefer clear statutory rules over judge-made ambiguity.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished, civil rights plaintiffs would gain access to discovery and trial for constitutional violations, officers would face personal liability exposure likely prompting indemnification statutes or insurance markets, and the federal judiciary would lose its gatekeeping control over the 'clearly established law' threshold. The constitutional remedial landscape would reorganize around statutory and common-law tort frameworks.
% FOUNDING_PROBLEM: The doctrine was articulated to protect public officials from the burden and distraction of insubstantial litigation and to ensure that capable candidates are not deterred from public service by fear of personal liability.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court majority asserts the problem remains live. However, extensive empirical scholarship from outside the judiciary (e.g., Joanna Schwartz's studies) demonstrates that insubstantial litigation is rare, that officers are almost always indemnified, and that the doctrine was invented in 1967 without basis in the common law of 1871; corroboration from the legislative record of Section 1983 and academic historians supports the dead-status reading.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__constitutional_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the doctrine strips away the central remedial mechanism Congress provided in 1871 for constitutional violations, replacing it with a judicially invented 'clearly established' barrier that defeats most claims before discovery. Suppression (0.78) is high because the constraint persists through active judicial enforcement â motions to dismiss, appellate affirmances, and the refusal to reach constitutional merits â while alternative statutory and common-law avenues have been deliberately narrowed. Theater ratio (0.55) reflects the growing gap between the Court's historical narrative (rooting immunity in 1871 common law) and the scholarly consensus that the doctrine is a twentieth-century invention with no authentic pedigree. Accessibility collapse (0.80) captures the near-total closure of federal remedial alternatives for constitutional violations once the doctrine is understood. Resistance (0.60) reflects persistent dissents, academic critique, and legislative proposals that have not yet overcome institutional lock-in.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (federal judiciary) experiences the constraint as a necessary and traditional incident of judicial power â a common-law prerogative that protects government functions. The payer seat (civil rights plaintiffs) experiences the identical structure as an unconstitutional denial of the remedial promise of Section 1983. The engine computes this divergence from the structural asymmetry in power and exit options: the judiciary has generational time horizons and identity-locked institutional continuity, while plaintiffs face immediate harm with trapped exit.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary occupies the beneficiary end of the directionality spectrum: it created the doctrine, administers its malleable 'clearly established law' test, and derives institutional authority from controlling access to constitutional remedies. Its identity-locked exit (stare decisis, institutional prestige tied to interpretive supremacy) reinforces low directionality. Civil rights plaintiffs sit at the full-target end: they bear the cost of dismissed claims, have no alternative federal forum for many constitutional torts, and are structurally trapped in a judiciary that invents barriers to their statutory and constitutional rights. Dissenting judges and excluded legislators have analytical or constrained exits but do not alter the structural flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine was originally articulated in Pierson v. Ray (1967) to solve a plausible coordination problem â protecting officers from bad-faith litigation â but its founding problem is now empirically contested and largely dead. Officers are almost universally indemnified; the real function has migrated to docket control and judicial power preservation. The constitutional fidelity reading prevents mislabeling this as a scaffold (it lacks a sunset clause and legitimate authorization) or a rope (it is not a neutral coordination mechanism). By identifying the judiciary as the concentrated beneficiary rather than officers, the reading resists the protective-scaffold cover story and classifies the constraint according to its present structural effect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint one reading of the qualified_immunity_doctrine kernel, and how would classification change under the protective_scaffold or accountability_void readings?',
    'Compare the three sibling constraints in the kernel family; this reading''s illegitimacy claim is reading-indexed.',
    'Under protective_scaffold, the doctrine might compute as scaffold or rope; under accountability_void, as snare. This reading''s classification as tangled_rope reflects the hybrid coordination-extraction function viewed from a seat that denies the coordination''s legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Kernel reading position for constitutional fidelity frame').

omega_variable(
    historical_pedigree_contest,
    'Does the doctrine have any legitimate basis in the common law of 1871 or is it entirely a twentieth-century judicial invention?',
    'Historical archival research into 1871 common-law immunities; the Court''s historical claims in Pierson and Harlow have been challenged by scholars like Charles Wallace and Karen Blum.',
    'If wholly invented, the ''illegitimate'' characterization strengthens; if some germ exists, the reading may need to acknowledge a distorted lineage rather than pure fabrication.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_pedigree_contest, empirical, 'Whether qualified immunity has authentic historical roots').

omega_variable(
    judicial_power_motive,
    'Is the judiciary''s maintenance of qualified immunity primarily driven by docket-control and institutional supremacy, or by a good-faith belief in officer protection?',
    'Analysis of judicial behavior (e.g., Pearson v. Callahan''s discretionary sequencing, the refusal to hear merits before immunity) and opinion rhetoric measuring functional vs. protective emphasis.',
    'If docket-control dominates, the beneficiary set is correctly identified as judiciary; if protection dominates, officers should be listed as beneficiaries and directionality shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_power_motive, conceptual, 'Judicial motive behind qualified immunity maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qid_cfr_tr_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qid_cfr_tr_t15, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(qid_cfr_tr_t30, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(qid_cfr_tr_t42, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 42, 0.5).
narrative_ontology:measurement(qid_cfr_tr_t50, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 50, 0.52).
narrative_ontology:measurement(qid_cfr_tr_t57, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 57, 0.55).

% Extraction over time
narrative_ontology:measurement(qid_cfr_be_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qid_cfr_be_t15, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(qid_cfr_be_t30, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(qid_cfr_be_t42, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 42, 0.75).
narrative_ontology:measurement(qid_cfr_be_t50, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 50, 0.8).
narrative_ontology:measurement(qid_cfr_be_t57, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 57, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(qid_cfr_su_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(qid_cfr_su_t15, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(qid_cfr_su_t30, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(qid_cfr_su_t42, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 42, 0.72).
narrative_ontology:measurement(qid_cfr_su_t50, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 50, 0.76).
narrative_ontology:measurement(qid_cfr_su_t57, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 57, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__accountability_void_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'qualified immunity' conflates three structurally distinct constraints: a protective scaffold for officers, a systematic accountability void for victims, and an illegitimate judicial power expansion. Each reading carries a different epsilon, beneficiary structure, and classification. They form a constraint family linked by the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
