% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: Strict Stare Decisis in Common Law Precedent
 *   domain: legal/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the 'strict stare decisis' reading of common
 *   law precedent, where prior judicial decisions are considered highly
 *   binding and departure requires extraordinary justification. This reading
 *   emphasizes stability and predictability in the law, often at the cost of
 *   adaptability to evolving social norms. The constraint is claimed as a
 *   Rope by its proponents (emphasizing coordination), but its metrics
 *   reflect a Tangled Rope due to the significant extraction from those
 *   seeking legal change and the active enforcement required to maintain its
 *   rigidity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.65).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.75).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.65).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis in Common Law Precedent").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal/jurisprudence").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, '7cd4944f-a81e-4af1-8737-a2c6ad7f0468').
narrative_ontology:cs_kernel_codification('7cd4944f-a81e-4af1-8737-a2c6ad7f0468', formalized).
narrative_ontology:cs_authority_grounding('7cd4944f-a81e-4af1-8737-a2c6ad7f0468', lineage).
narrative_ontology:cs_interpretation_layer_present('7cd4944f-a81e-4af1-8737-a2c6ad7f0468').
narrative_ontology:cs_reading_relation('7cd4944f-a81e-4af1-8737-a2c6ad7f0468', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_reading_relation('7cd4944f-a81e-4af1-8737-a2c6ad7f0468', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('7cd4944f-a81e-4af1-8737-a2c6ad7f0468', foundational, precedent_as_binding_rule).
narrative_ontology:cs_axiom_status(precedent_as_binding_rule, holdable).
narrative_ontology:cs_axiom_grounding('7cd4944f-a81e-4af1-8737-a2c6ad7f0468', precedent_as_binding_rule, conventional).
narrative_ontology:cs_axiom('7cd4944f-a81e-4af1-8737-a2c6ad7f0468', foundational, judicial_restraint_as_virtue).
narrative_ontology:cs_axiom_status(judicial_restraint_as_virtue, holdable).
narrative_ontology:cs_axiom_grounding('7cd4944f-a81e-4af1-8737-a2c6ad7f0468', judicial_restraint_as_virtue, deontological).
narrative_ontology:cs_reference_frame('7cd4944f-a81e-4af1-8737-a2c6ad7f0468', classical_legal_positivism).
narrative_ontology:cs_drift_state('7cd4944f-a81e-4af1-8737-a2c6ad7f0468', contemporary_social_change_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7cd4944f-a81e-4af1-8737-a2c6ad7f0468', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, judicial_legitimacy).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, legal_certainty_doctrine).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, litigants_seeking_norm_change).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, social_movements_for_reform).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, legal_practitioners).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, lower_court_judges).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the ultimate interpreters of law, they are bound by precedent but also hold the power to overrule it, though this power is exercised with extreme caution under strict stare decisis. Their legitimacy is tied to maintaining stability and predictability in the law.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, supreme_court_justices, agenda_setter,
    institutional, generational, constrained, national).

% Are strictly bound by higher court precedent and must apply it even if they disagree with the outcome or believe it to be outdated. Their professional identity is deeply tied to upholding the rule of law as established by precedent, making departure extremely costly to their careers.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, lower_court_judges, payer,
    organized, biographical, identity_locked, regional).

% Face significant hurdles when challenging established legal norms, as strict adherence to precedent means their cases are often decided before arguments are heard. Their only path to change is through extraordinary justification, which is resource-intensive and rarely successful.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, litigants_seeking_norm_change, payer,
    powerless, immediate, trapped, local).

% Seek to change legal norms to reflect evolving societal values. Strict stare decisis makes the judiciary a highly resistant institution to such changes, forcing movements to pursue legislative or constitutional amendment paths, which are often slower and more difficult.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, social_movements_for_reform, payer,
    organized, generational, constrained, national).

% Benefit from the predictability and stability that strict stare decisis provides, allowing them to advise clients with a high degree of certainty about legal outcomes. This reduces litigation risk and streamlines legal processes.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_practitioners, beneficiary,
    moderate, biographical, mobile, national).

% Analyze the application and evolution of precedent, critiquing its rigidity or flexibility. They provide intellectual frameworks for understanding the constraint but do not directly participate in its enforcement or suffer its direct costs.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures stability, predictability, and fairness in the application of law by requiring courts to follow prior judicial decisions. This coordinates judicial behavior across different courts and over time.
% TRANSFER_FUNCTION: Transfers the burden of legal change from the judiciary to other branches of government (legislature) or requires extraordinary justification from litigants, preserving judicial resources and maintaining the perceived neutrality of the courts.
% ABSENT_VOICES: Future generations whose values may diverge significantly from established precedent are structurally absent from the original decision-making process. Their interests are represented only through the difficult process of legislative reform or the rare overruling of precedent.
% DISAPPEARANCE_RATIONALE: If strict stare decisis vanished, legal outcomes would become highly unpredictable, leading to chaos in the judicial system. Every case could be re-litigated from first principles, undermining the rule of law and legal certainty. The entire legal system would need to be fundamentally restructured.
% FOUNDING_PROBLEM: The problem of arbitrary judicial decision-making and lack of legal certainty, where similar cases could be decided differently based on individual judges' whims, leading to injustice and undermining public trust in the legal system.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and practitioners widely corroborate that the problem of arbitrary decision-making remains a live concern, and that some form of adherence to precedent is essential for a functioning legal system. However, the *degree* of adherence (strict vs. flexible) is contested.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the strict adherence to past decisions imposes substantial costs on litigants and social movements seeking to update the law, forcing them into difficult and often unsuccessful battles against established norms. Suppression is also high (0.75) as the legal system actively resists challenges to precedent, requiring extraordinary justification and often denying avenues for change. Theater ratio is moderate (0.20) as the justification for strict adherence (stability, certainty) is genuinely valued, but the rhetoric sometimes masks the inertial resistance to necessary adaptation. The increasing extractiveness and suppression over time reflect the growing tension between legal rigidity and societal evolution.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of judicial institutions and legal practitioners, strict stare decisis is a necessary coordination mechanism for a stable legal system. From the perspective of litigants and social movements, it is an extractive mechanism that entrenches existing power structures and resists necessary social evolution. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Judicial legitimacy and legal certainty are the primary beneficiaries, as the constraint reinforces their perceived stability and authority (low directionality). Litigants seeking norm change and social movements for reform are the primary targets, bearing the costs of legal rigidity and facing high barriers to achieving their goals (high directionality). Legal practitioners benefit from predictability (low directionality), while judges, particularly in lower courts, are identity-locked into upholding precedent, making them both beneficiaries of the system's order and payers of its rigidity (moderate directionality).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    justification_threshold_ambiguity,
    'What constitutes ''extraordinary justification'' for departing from precedent, and is this threshold consistently applied or subject to judicial discretion?',
    'Empirical analysis of overruling decisions over time, coding for stated justifications and their consistency across different judicial panels and eras.',
    'If the threshold is inconsistent or highly discretionary, the constraint''s suppression is effectively higher and more arbitrary, pushing it closer to a Snare. If consistently high, it reinforces the Tangled Rope classification by confirming the structural difficulty of change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justification_threshold_ambiguity, empirical, 'Ambiguity in the criteria for overruling precedent.').

omega_variable(
    legitimacy_vs_adaptability_tradeoff,
    'At what point does the pursuit of legal certainty and judicial legitimacy (via strict stare decisis) begin to undermine the legal system''s adaptability and responsiveness to societal change, thereby eroding its long-term legitimacy?',
    'Longitudinal studies correlating judicial rigidity with public trust in the judiciary, legislative interventions to override judicial decisions, and the rise of extra-legal avenues for social change.',
    'If rigidity demonstrably erodes long-term legitimacy, the constraint''s claimed coordination function (maintaining trust) becomes theatrical, pushing it towards a Piton or Snare. If adaptability is shown to be less critical for legitimacy, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_vs_adaptability_tradeoff, conceptual, 'The inherent tension between legal stability and social adaptation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 10, 0.12).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 20, 0.15).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 30, 0.17).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 40, 0.19).
narrative_ontology:measurement(comm_tr_t50, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(comm_be_t50, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(comm_su_t10, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(comm_su_t50, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, constitutional_interpretation_originalism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, legislative_process_gridlock).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the common_law_precedent_corpus kernel. Its rigidity influences other legal interpretive constraints and can exacerbate legislative gridlock by shifting the burden of legal change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
