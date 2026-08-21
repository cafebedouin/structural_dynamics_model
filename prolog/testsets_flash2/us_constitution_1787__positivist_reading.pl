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
 *   where meaning is derived strictly from the text and formal amendments,
 *   with judicial interpretation constrained to these sources. It is one
 *   reading of the 'us_constitution_1787' kernel, distinct from originalist
 *   and living constitutionalist interpretations. This reading emphasizes
 *   democratic accountability through the amendment process and limits
 *   judicial activism.
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
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, 'dbef8425-07ef-40c7-918c-3a9bfddac007').
narrative_ontology:cs_kernel_codification('dbef8425-07ef-40c7-918c-3a9bfddac007', fixed_text).
narrative_ontology:cs_authority_grounding('dbef8425-07ef-40c7-918c-3a9bfddac007', lineage).
narrative_ontology:cs_interpretation_layer_present('dbef8425-07ef-40c7-918c-3a9bfddac007').
narrative_ontology:cs_reading_relation('dbef8425-07ef-40c7-918c-3a9bfddac007', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('dbef8425-07ef-40c7-918c-3a9bfddac007', us_constitution_1787__living_reading, coexists_with).
narrative_ontology:cs_axiom('dbef8425-07ef-40c7-918c-3a9bfddac007', foundational, textual_supremacy).
narrative_ontology:cs_axiom_status(textual_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('dbef8425-07ef-40c7-918c-3a9bfddac007', textual_supremacy, conventional).
narrative_ontology:cs_axiom('dbef8425-07ef-40c7-918c-3a9bfddac007', foundational, amendment_as_sole_legitimate_change).
narrative_ontology:cs_axiom_status(amendment_as_sole_legitimate_change, holdable).
narrative_ontology:cs_axiom_grounding('dbef8425-07ef-40c7-918c-3a9bfddac007', amendment_as_sole_legitimate_change, deontological).
narrative_ontology:cs_reference_frame('dbef8425-07ef-40c7-918c-3a9bfddac007', constitutional_text_as_supreme_law).
narrative_ontology:cs_drift_state('dbef8425-07ef-40c7-918c-3a9bfddac007', contemporary_judicial_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dbef8425-07ef-40c7-918c-3a9bfddac007', '').
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

% Benefits from the clarity that constitutional meaning is primarily derived from the text and formal amendments, empowering it as the primary vehicle for democratic change. Its actions are constrained by the text but not by evolving judicial interpretations beyond the text.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, legislative_branch, beneficiary,
    institutional, generational, constrained, national).

% Benefits from the democratic accountability of constitutional change through the amendment process, rather than through unelected judicial interpretation. Their will, expressed through elected representatives, is the ultimate source of constitutional meaning.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, electorate, beneficiary,
    organized, generational, mobile, national).

% Administers and interprets the Constitution, but is constrained to the plain text and formal amendments. Its role is to apply the law as written, not to evolve its meaning. This limits its power to impose new constitutional meanings.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, judicial_branch, agenda_setter,
    institutional, civilizational, constrained, national).

% Bear the cost of this reading by having their preferred method of constitutional change (judicial reinterpretation) curtailed. Their professional identity is often tied to a more expansive view of judicial power, making 'exit' from this interpretive stance difficult.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, judicial_activists, payer,
    powerful, biographical, identity_locked, national).

% Observe this reading as a partial victory against 'living constitutionalism' but still find it insufficiently tethered to the original intent of the framers. They would prefer a more historically constrained interpretation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, originalists, observer,
    analytical, generational, analytical, national).

% Observe this reading as overly rigid and unresponsive to societal change. They argue that a text-bound approach fails to address contemporary challenges and leads to an ossified Constitution.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, living_constitutionalists, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, democratically accountable framework for constitutional governance by limiting interpretation to the written text and formal amendment process, ensuring that fundamental law changes only through broad consensus.
% TRANSFER_FUNCTION: Transfers interpretive authority from unelected judges to the democratically elected legislative bodies and the amendment process, ensuring popular sovereignty over constitutional evolution.
% ABSENT_VOICES: Those who believe that constitutional meaning must dynamically adapt to societal changes without formal amendment are marginalized; their arguments for an evolving, aspirational text are not given primary weight in this framework.
% DISAPPEARANCE_RATIONALE: If this reading vanished, judicial interpretation would likely become more expansive, potentially leading to a less predictable and democratically accountable constitutional order. The balance of power between branches would shift, and the amendment process would lose its central role in constitutional evolution.
% FOUNDING_PROBLEM: To establish a durable framework for government that balances stability with the capacity for change, ensuring that fundamental law is clear, accessible, and subject to popular will through a defined amendment process.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and political scientists outside the immediate beneficiaries attest that the problem of balancing stability and change in constitutional law remains live. Debates over judicial review and democratic legitimacy continue to highlight the ongoing relevance of this founding problem.
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.35) as it primarily extracts from those who prefer a more expansive judicial role, but it also provides a clear, stable framework. Suppression is moderate (0.45) as it actively suppresses alternative interpretive methods in favor of textualism and amendment. Theater ratio is low (0.1) because the constraint's function (text-bound interpretation) is largely consistent with its stated purpose. The constraint is claimed as a 'rope' because it provides a clear coordination mechanism for constitutional change and interpretation, with identifiable beneficiaries (legislature, electorate) and a relatively low level of extraction compared to a 'snare'.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the legislative branch and the electorate, this reading is a legitimate and democratically sound framework. From the perspective of judicial activists, it is an undue restriction on judicial power. The engine's per-seat classification will reflect these divergent experiences based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative branch and the electorate are beneficiaries, as this reading empowers their role in constitutional change. The judicial branch, while administering the constraint, is also constrained by it, placing it closer to a symmetric position. Judicial activists are the primary victims, as their preferred interpretive methods are curtailed. Originalists and living constitutionalists are observers, representing alternative readings.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_ambiguity,
    'Is the ''plain meaning'' of the text truly objective, or does it inherently involve subjective interpretation, blurring the line between positivism and other readings?',
    'Empirical study of judicial decision-making across different interpretive methodologies, assessing the degree of convergence on ''plain meaning'' versus divergence based on unstated interpretive assumptions.',
    'If ''plain meaning'' is found to be highly subjective, the positivist reading''s claim to objective, text-bound interpretation is weakened, potentially increasing its effective extractiveness by masking interpretive choices as textual necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_ambiguity, conceptual, 'Ambiguity regarding the objectivity of ''plain meaning'' in constitutional interpretation.').

omega_variable(
    democratic_legitimacy_vs_minority_rights,
    'Does the emphasis on democratic amendment as the sole legitimate path for constitutional change adequately protect minority rights against majoritarian oppression, or does it create a ''tyranny of the majority''?',
    'Comparative analysis of constitutional systems with varying amendment difficulty and judicial review powers, examining outcomes for minority groups over time.',
    'If this reading is found to systematically disadvantage minorities, its claim to democratic legitimacy is complicated, potentially reclassifying it as a ''tangled_rope'' or ''snare'' for those groups, despite its coordination function for the majority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_vs_minority_rights, preference, 'Tension between democratic legitimacy of amendment process and protection of minority rights.').

omega_variable(
    kernel_reading_distinction,
    'What specific structural element of the ''us_constitution_1787'' kernel do the positivist, originalist, and living readings differ on, and how does this reading''s interpretation of that element shape its classification?',
    'Detailed textual and historical analysis of key constitutional clauses (e.g., ''due process,'' ''equal protection'') across all three interpretive traditions, identifying the precise point of divergence in their application.',
    'Clarifying the point of divergence would sharpen the boundaries between the readings, potentially revealing that what appears as a single kernel is, in fact, a set of distinct, though related, constraints. This would validate the ε-invariance principle by showing that each reading indeed instantiates a structurally unique constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Clarifying the structural point of divergence between the positivist, originalist, and living readings of the US Constitution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__positivist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_1787__positivist_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_1787__positivist_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_1787__positivist_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_1787__positivist_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_1787__positivist_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__positivist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(us_c_be_t10, us_constitution_1787__positivist_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(us_c_be_t20, us_constitution_1787__positivist_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(us_c_be_t30, us_constitution_1787__positivist_reading, base_extractiveness, 30, 0.34).
narrative_ontology:measurement(us_c_be_t40, us_constitution_1787__positivist_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(us_c_be_t50, us_constitution_1787__positivist_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__positivist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(us_c_su_t10, us_constitution_1787__positivist_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(us_c_su_t20, us_constitution_1787__positivist_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(us_c_su_t30, us_constitution_1787__positivist_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement(us_c_su_t40, us_constitution_1787__positivist_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(us_c_su_t50, us_constitution_1787__positivist_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
