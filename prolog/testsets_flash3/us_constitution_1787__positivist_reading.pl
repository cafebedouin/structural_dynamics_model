% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__positivist_reading, []).

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
 *   constraint_id: us_constitution_1787__positivist_reading
 *   human_readable: US Constitution (Positivist Reading): Text + Amendments
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents a positivist reading of the US Constitution,
 *   where its meaning is primarily derived from the explicit text and formal
 *   amendments, with judicial interpretation strictly constrained to these
 *   sources. This reading emphasizes democratic accountability through the
 *   amendment process and limits judicial activism. It is one reading of the
 *   'us_constitution_1787' kernel, distinct from originalist and living
 *   constitutionalist interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.35).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.45).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "US Constitution (Positivist Reading): Text + Amendments").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, '5fc15e2f-9c77-4f79-8c2a-2327e82b2844').
narrative_ontology:cs_kernel_codification('5fc15e2f-9c77-4f79-8c2a-2327e82b2844', fixed_text).
narrative_ontology:cs_authority_grounding('5fc15e2f-9c77-4f79-8c2a-2327e82b2844', lineage).
narrative_ontology:cs_interpretation_layer_present('5fc15e2f-9c77-4f79-8c2a-2327e82b2844').
narrative_ontology:cs_reading_relation('5fc15e2f-9c77-4f79-8c2a-2327e82b2844', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5fc15e2f-9c77-4f79-8c2a-2327e82b2844', us_constitution_1787__living_reading, coexists_with).
narrative_ontology:cs_axiom('5fc15e2f-9c77-4f79-8c2a-2327e82b2844', foundational, textual_supremacy).
narrative_ontology:cs_axiom_status(textual_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('5fc15e2f-9c77-4f79-8c2a-2327e82b2844', textual_supremacy, conventional).
narrative_ontology:cs_axiom('5fc15e2f-9c77-4f79-8c2a-2327e82b2844', foundational, amendment_as_primary_change_mechanism).
narrative_ontology:cs_axiom_status(amendment_as_primary_change_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('5fc15e2f-9c77-4f79-8c2a-2327e82b2844', amendment_as_primary_change_mechanism, conventional).
narrative_ontology:cs_reference_frame('5fc15e2f-9c77-4f79-8c2a-2327e82b2844', constitutional_text_as_supreme_law).
narrative_ontology:cs_drift_state('5fc15e2f-9c77-4f79-8c2a-2327e82b2844', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5fc15e2f-9c77-4f79-8c2a-2327e82b2844', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, electorate).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, judicial_activists).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, rule_of_law).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, popular_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the clarity that constitutional meaning is primarily derived from the text and formal amendments, empowering it as the primary vehicle for democratic change and policy. Its power is constrained by the text but expanded by the amendment process.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, legislative_branch, beneficiary,
    institutional, generational, constrained, national).

% Benefits from the principle that constitutional change is a democratic process, primarily through amendments, rather than evolving judicial interpretation. This grants them ultimate sovereignty over the fundamental law, albeit through a difficult process.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, electorate, beneficiary,
    organized, generational, constrained, national).

% Administers and interprets the Constitution, but under this reading, its role is strictly limited to the text and its formal amendments. This constrains its power to innovate or 'find' new rights not explicitly stated, making it a target of the constraint itself when it seeks to expand its interpretive authority.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, judicial_branch, agenda_setter,
    institutional, civilizational, constrained, national).

% Bear the cost of this reading as it directly limits their preferred method of constitutional interpretation, which often involves deriving meaning beyond the explicit text. Their professional identity is often tied to a more expansive view of judicial power.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, judicial_activists, payer,
    powerful, biographical, identity_locked, national).

% Would argue that while the text is paramount, its meaning is fixed at the time of ratification and should be interpreted according to the framers' original intent, which this positivist reading does not strictly require. They are excluded from the core interpretive method.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, originalist_scholars, excluded,
    analytical, generational, analytical, national).

% Would argue that constitutional meaning must evolve with society and that the text provides an aspirational framework, not a rigid set of rules. This reading's emphasis on the text and formal amendments excludes their preferred interpretive method.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, living_constitutionalists, excluded,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, democratically accountable framework for constitutional governance by limiting judicial discretion and channeling fundamental change through the formal amendment process, ensuring broad consensus for foundational shifts.
% TRANSFER_FUNCTION: Transfers interpretive authority from unelected judges (beyond the text) to the democratically elected legislature and the amendment process, ensuring that fundamental law reflects popular will.
% ABSENT_VOICES: Originalist scholars would argue for a stricter historical interpretation, while living constitutionalists would advocate for a more dynamic, evolving meaning. Both are structurally excluded from this reading's core interpretive methodology, which prioritizes the plain text and formal amendments.
% DISAPPEARANCE_RATIONALE: If this positivist reading vanished, the interpretive landscape would become far more fluid. Judicial interpretation would likely expand, potentially leading to more frequent and less democratically accountable constitutional changes. The legislative branch's role in constitutional evolution would diminish, and the electorate's direct influence would be less clear.
% FOUNDING_PROBLEM: The problem of ensuring a stable, legitimate, and democratically accountable fundamental law, preventing arbitrary rule by unelected officials and providing a clear mechanism for societal adaptation.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, political scientists, and public discourse consistently attest to the ongoing challenge of balancing stability, legitimacy, and democratic accountability in constitutional interpretation. The debate over judicial review and the amendment process remains central to American political life, corroborating the founding problem's live status from outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__positivist_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__positivist_reading_tests).
:- end_tests(us_constitution_1787__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) as it limits the interpretive freedom of the judiciary, which can be seen as a cost to those who prefer a more dynamic interpretation. Suppression is moderate (0.45) because this reading requires active intellectual and institutional defense against alternative interpretive methods, particularly those that expand judicial power. Theater ratio is low (0.1) as the core function of text-bound interpretation is largely genuine, though debates over 'plain meaning' can introduce some performativity. The metrics reflect a constraint that is actively maintained to channel constitutional change through formal, democratic means.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the legislative branch and the electorate, this reading is a 'rope' that ensures democratic control over fundamental law. From the perspective of judicial activists, it can feel like a 'snare' that unduly restricts their interpretive role and prevents necessary adaptation. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative branch and the electorate are beneficiaries, as this reading empowers their role in constitutional change. The judicial branch, particularly 'judicial activists,' are targets, as their interpretive scope is constrained. Other interpretive schools (originalists, living constitutionalists) are excluded, as their methodologies are not central to this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_drift_measurement,
    'How precisely can ''textual meaning'' be defined and measured, given the inherent ambiguities of language and the historical context of its drafting?',
    'Development of formal semantic analysis tools for legal texts, or a consensus among legal scholars on a methodology for determining ''plain meaning'' that is robust against interpretive bias.',
    'If ''textual meaning'' can be precisely and objectively determined, the constraint''s suppression of judicial activism is more legitimate. If it remains inherently ambiguous, the constraint''s effectiveness in limiting judicial discretion is reduced, and its extractiveness may be higher due to the performative nature of ''textual'' arguments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_drift_measurement, conceptual, 'Ambiguity in defining and measuring ''textual meaning'' in constitutional interpretation.').

omega_variable(
    amendment_process_accessibility,
    'Is the formal amendment process sufficiently accessible and responsive to democratic will to serve as the primary mechanism for constitutional change, or does its difficulty effectively ''trap'' the electorate?',
    'Empirical analysis of amendment proposals, success rates, and the political capital required for passage over time, compared to the rate of societal change and evolving public opinion.',
    'If the amendment process is effectively inaccessible, the positivist reading''s claim of democratic accountability is weakened, and the constraint''s suppression of alternative change mechanisms (like judicial evolution) becomes more extractive. If it is reasonably accessible, the democratic legitimacy is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_accessibility, empirical, 'The practical accessibility and responsiveness of the constitutional amendment process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1787, us_constitution_1787__positivist_reading, theater_ratio, 1787, 0.05).
narrative_ontology:measurement(us_c_tr_t1850, us_constitution_1787__positivist_reading, theater_ratio, 1850, 0.07).
narrative_ontology:measurement(us_c_tr_t1900, us_constitution_1787__positivist_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(us_c_tr_t1950, us_constitution_1787__positivist_reading, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_1787__positivist_reading, theater_ratio, 2000, 0.095).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_1787__positivist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1787, us_constitution_1787__positivist_reading, base_extractiveness, 1787, 0.2).
narrative_ontology:measurement(us_c_be_t1850, us_constitution_1787__positivist_reading, base_extractiveness, 1850, 0.25).
narrative_ontology:measurement(us_c_be_t1900, us_constitution_1787__positivist_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(us_c_be_t1950, us_constitution_1787__positivist_reading, base_extractiveness, 1950, 0.33).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_1787__positivist_reading, base_extractiveness, 2000, 0.34).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_1787__positivist_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1787, us_constitution_1787__positivist_reading, suppression_requirement, 1787, 0.3).
narrative_ontology:measurement(us_c_su_t1850, us_constitution_1787__positivist_reading, suppression_requirement, 1850, 0.35).
narrative_ontology:measurement(us_c_su_t1900, us_constitution_1787__positivist_reading, suppression_requirement, 1900, 0.4).
narrative_ontology:measurement(us_c_su_t1950, us_constitution_1787__positivist_reading, suppression_requirement, 1950, 0.42).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_1787__positivist_reading, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_1787__positivist_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__living_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the US Constitution (us_constitution_1787) kernel. Each reading represents a different structural constraint on governance and interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
