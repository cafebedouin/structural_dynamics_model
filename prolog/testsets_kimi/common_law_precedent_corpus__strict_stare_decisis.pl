% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis: Precedent as Binding Backward Constraint
 *   domain: legal/jurisprudential
 *
 * SUMMARY:
 *   This constraint instantiates the strict_stare_decisis reading of the
 *   common_law_precedent_corpus kernel: prior judicial decisions constitute a
 *   binding backward constraint, and any departure demands extraordinary
 *   justification. It is a jurisprudential commitment system with lineage
 *   authority, where the kernel (the doctrine of precedent) is formalized
 *   through judicial rules and institutional practice. The constraint
 *   coordinates legal expectations but asymmetrically extracts from parties
 *   seeking doctrinal innovation and from subordinate courts. The structural
 *   divergence between the appellate bench (agenda-setter with
 *   continuity-derived legitimacy) and the litigant/trial-court seats
 *   (targets of the binding force) is central.
 *
 * KEY AGENTS:
 *   - Appellate judiciary (agenda_setter/beneficiary): institutional power, constrained exit â administers precedent and derives legitimacy from continuity.
 *   - Reliance interests (beneficiary): powerful, mobile exit â gain stability and predictability.
 *   - Normative litigants (payer): moderate power, constrained exit â bear costs of challenging precedent.
 *   - Trial judiciary (payer): institutional power, constrained exit â bound by appellate holdings.
 *   - Legal scholars (observer): analytical seat.
 *   - Public interest advocates (excluded): moderate power, absent from direct agenda access.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.62).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.58).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.62).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis: Precedent as Binding Backward Constraint").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal/jurisprudential").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, '7f290d70-c962-4f1c-963b-6d0a56d4c3a9').
narrative_ontology:cs_kernel_codification('7f290d70-c962-4f1c-963b-6d0a56d4c3a9', formalized).
narrative_ontology:cs_authority_grounding('7f290d70-c962-4f1c-963b-6d0a56d4c3a9', lineage).
narrative_ontology:cs_interpretation_layer_present('7f290d70-c962-4f1c-963b-6d0a56d4c3a9').
narrative_ontology:cs_reading_relation('7f290d70-c962-4f1c-963b-6d0a56d4c3a9', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_reading_relation('7f290d70-c962-4f1c-963b-6d0a56d4c3a9', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('7f290d70-c962-4f1c-963b-6d0a56d4c3a9', foundational, precedent_binds_as_positive_law).
narrative_ontology:cs_axiom_status(precedent_binds_as_positive_law, holdable).
narrative_ontology:cs_axiom_grounding('7f290d70-c962-4f1c-963b-6d0a56d4c3a9', precedent_binds_as_positive_law, conventional).
narrative_ontology:cs_axiom('7f290d70-c962-4f1c-963b-6d0a56d4c3a9', foundational, overruling_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(overruling_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('7f290d70-c962-4f1c-963b-6d0a56d4c3a9', overruling_requires_extraordinary_justification, conventional).
narrative_ontology:cs_reference_frame('7f290d70-c962-4f1c-963b-6d0a56d4c3a9', classical_stare_decisis_continuity).
narrative_ontology:cs_drift_state('7f290d70-c962-4f1c-963b-6d0a56d4c3a9', contemporary_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f290d70-c962-4f1c-963b-6d0a56d4c3a9', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, reliance_interests).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, normative_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, trial_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the overruling power but is bound by its own prior holdings and the institutional expectation that departure requires extraordinary justification. Derives legitimacy from doctrinal continuity and generational authority, yet sacrifices flexibility to adapt to changed norms.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, appellate_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__strict_stare_decisis, appellate_judiciary, beneficiary).

% Commercial and property actors who benefit from legal stability. Their planning and investment depend on settled rules remaining fixed. They may lobby for statutory change but generally gain from high switching costs in legal doctrine.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, reliance_interests, beneficiary,
    powerful, biographical, mobile, national).

% Parties seeking to challenge or overturn existing precedent. Must meet an extraordinary justification standard to secure overruling, facing higher litigation burdens, narrower doctrinal pathways, and asymmetric procedural costs.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, normative_litigants, payer,
    moderate, immediate, constrained, national).

% Must apply binding appellate precedent even when it appears wrong or outdated. Distinguishing cases is permitted but heavily policed; overt refusal or creative circumvention invites reversal and professional sanction.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, trial_judiciary, payer,
    institutional, biographical, constrained, national).

% Analyze and critique the rigidity of precedent from outside the bench. They shape discourse on whether strict adherence is justified but do not decide cases or bear direct costs of the constraint.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_scholars, observer,
    analytical, generational, analytical, national).

% Would argue for overruling precedent on behalf of broader social interests but lack standing or direct access to the appellate agenda. Their perspectives are filtered through the parties that happen to litigate.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, public_interest_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides predictability and stability in legal ordering by ensuring that prior judicial resolutions constrain future decision-makers, reducing arbitrary variance in case outcomes across time and court level.
% TRANSFER_FUNCTION: Moves doctrinal control from present litigants and contemporary judges to past judicial majorities; the cost of normative change is transferred to challengers who must surmount an extraordinary justification threshold.
% ABSENT_VOICES: Future litigants whose claims are shaped by precedent not yet challenged; legislative bodies that could override statutory precedent but are excluded from constitutional interpretation; public interest advocates without standing to force reconsideration.
% DISAPPEARANCE_RATIONALE: If precedent ceased to bind, trial courts would decide cases de novo, appellate courts would lose a primary instrument of institutional continuity, and reliance interests would face radical uncertainty; legal ordering would shift toward ad hoc balancing or comprehensive legislative codification.
% FOUNDING_PROBLEM: Early common law needed a mechanism to resolve disputes consistently across time and geographic space without relying on a comprehensive legislative code.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians attest to the founding problem from outside the beneficiary set. Modern public-choice scholars and comparative lawyers contest whether the problem persists in statutory-heavy regimes, attesting from analytical and outsider seats that the coordination function has partially migrated to legislatures.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.62 because the extraordinary-justification threshold imposes a substantial procedural and rhetorical tax on normative change, entrenching past holdings beyond their contemporary merit. Suppression is 0.58 because alternatives (overruling, distinguishing, statutory override) exist but are actively narrowed by doctrinal filters and professional sanctions. Theater ratio is 0.25: most adherence is functional, though some rhetorical performance of continuity occurs. Accessibility collapse is 0.70 because once the binding nature of precedent is accepted, simply ignoring it is not a viable alternative within the legal order. Resistance is 0.45 due to sustained academic critique and occasional judicial deviance.
 *
 * PERSPECTIVAL GAP:
 *   The appellate judiciary experiences the constraint as a source of institutional authority and generational continuity (low effective extraction), while normative litigants and trial courts experience it as a rigid barrier to adaptive justice (high effective extraction). The engine computes this divergence from the shared structural data: same constraint, different directionalities based on beneficiary/victim declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Appellate judiciary and reliance interests are structural beneficiaries: they gain legitimacy and predictability (d near the beneficiary end). Normative litigants and trial courts are structural targets: they bear the costs of doctrinal inertia and subordination (d near the target end). Legal scholars occupy an analytical seat with neutral d. Excluded public interest advocates have no voice but would face high d if seated.
 *
 * MANDATROPHY ANALYSIS:
 *   Strict stare decisis risks mandatrophy if the founding problem (dispute resolution without comprehensive legislative codes) is dead but the doctrine persists to protect entrenched reliance interests. The R5 genealogy flags contested status, preventing automatic classification as settled coordination. The temporal measurements show only modest theater_ratio creep, insufficient to trigger piton reclassification, but the contested founding problem status keeps the mandatrophy question live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precedent_binding_naturalness,
    'Does the binding force of precedent derive from an inherent feature of legal reasoning or from a contingent institutional convention?',
    'Historical comparative analysis of legal systems with and without strict stare decisis; if functionally similar legal stability arises through other mechanisms, the binding force is conventional.',
    'If natural, classification trends toward Mountain; if conventional, the constraint remains a constructed coordination mechanism with extractive potential and active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_binding_naturalness, conceptual, 'Whether precedent binding is a natural law or constructed convention.').

omega_variable(
    extraordinary_justification_content,
    'What factual or normative content satisfies the ''extraordinary justification'' threshold for overruling precedent?',
    'Empirical survey of successful and failed overruling motions mapping claimed justifications to outcomes.',
    'A vague or manipulable standard increases extractiveness by leaving discretion with the agenda-setter; a rigid standard increases constraint severity but reduces arbitrary variance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraordinary_justification_content, empirical, 'Empirical content of the extraordinary justification standard.').

omega_variable(
    coordination_extraction_severability,
    'Can the coordination benefit of legal stability be decoupled from the extractive cost of binding bad precedent?',
    'Institutional design analysis of sunset clauses for precedent, tiered binding force, or mandatory reconsideration intervals.',
    'If severable, the constraint could be reformed toward Rope or Scaffold; if inseparable, it remains Tangled Rope with built-in asymmetric extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_severability, conceptual, 'Whether stability and entrenchment are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(strict_stare_decisis_tr_t0, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0, 0.15).
narrative_ontology:measurement(strict_stare_decisis_tr_t10, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 10, 0.18).
narrative_ontology:measurement(strict_stare_decisis_tr_t20, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 20, 0.2).
narrative_ontology:measurement(strict_stare_decisis_tr_t30, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 30, 0.23).
narrative_ontology:measurement(strict_stare_decisis_tr_t40, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 40, 0.25).
narrative_ontology:measurement(strict_stare_decisis_tr_t50, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 50, 0.27).
narrative_ontology:measurement(strict_stare_decisis_tr_t60, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(strict_stare_decisis_be_t0, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(strict_stare_decisis_be_t10, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(strict_stare_decisis_be_t20, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(strict_stare_decisis_be_t30, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(strict_stare_decisis_be_t40, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(strict_stare_decisis_be_t50, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 50, 0.63).
narrative_ontology:measurement(strict_stare_decisis_be_t60, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(strict_stare_decisis_su_t0, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(strict_stare_decisis_su_t10, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(strict_stare_decisis_su_t20, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(strict_stare_decisis_su_t30, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(strict_stare_decisis_su_t40, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(strict_stare_decisis_su_t50, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(strict_stare_decisis_su_t60, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, pluralist_balancing).

% DUAL FORMULATION NOTE:
% The common_law_precedent_corpus kernel decomposes into three structurally distinct readings: strict_stare_decisis (high rigidity, backward binding), evolutionary_framework (adaptive reinterpretation), and pluralist_balancing (context-dependent weighting). Each reading instantiates a different constraint with distinct epsilon, beneficiaries, and enforcement profiles. They compete for dominance within the same judicial system but are logically separable as commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
