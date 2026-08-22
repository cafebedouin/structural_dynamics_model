% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Positivist Reading of U.S. Constitutional Meaning (Text Plus Amendments)
 *   domain: constitutional law / legal theory / political philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the positivist reading of the U.S.
 *   Constitution: constitutional meaning is fixed by the enacted text plus
 *   democratically ratified amendments, and judicial interpretation is
 *   disciplined to stay within textual boundaries. It is distinct from
 *   originalism (which adds historical intent) and living constitutionalism
 *   (which permits evolving meaning). The constraint operates through
 *   judicial self-discipline, appellate review, and professional legal norms
 *   that treat non-textual arguments as interpretively out of bounds. It is
 *   presented as a coordination mechanism for democratic legitimacy and
 *   predictability, but it also asymmetrically extracts from those who would
 *   seek constitutional recognition outside the text or its amendment
 *   pathway.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Primary agenda_setter (institutional/constrained) â administers the textual constraint through precedent and review
 *   - popular_majorities: Primary beneficiary (organized/mobile) â control constitutional change through the amendment process
 *   - unenumerated_rights_claimants: Primary target (moderate/constrained) â blocked from non-textual constitutional protections
 *   - constitutional_scholars: Analytical observer (analytical) â tracks gaps between positivist theory and judicial practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.58).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.62).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "Positivist Reading of U.S. Constitutional Meaning (Text Plus Amendments)").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "constitutional law / legal theory / political philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, 'e56ef18e-a65f-4bd4-80be-7ae646f19fda').
narrative_ontology:cs_kernel_codification('e56ef18e-a65f-4bd4-80be-7ae646f19fda', fixed_text).
narrative_ontology:cs_authority_grounding('e56ef18e-a65f-4bd4-80be-7ae646f19fda', lineage).
narrative_ontology:cs_interpretation_layer_present('e56ef18e-a65f-4bd4-80be-7ae646f19fda').
narrative_ontology:cs_reading_relation('e56ef18e-a65f-4bd4-80be-7ae646f19fda', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e56ef18e-a65f-4bd4-80be-7ae646f19fda', us_constitution_1787__living_reading, coexists_with).
narrative_ontology:cs_axiom('e56ef18e-a65f-4bd4-80be-7ae646f19fda', foundational, enacted_text_binds_judiciary).
narrative_ontology:cs_axiom_status(enacted_text_binds_judiciary, holdable).
narrative_ontology:cs_axiom_grounding('e56ef18e-a65f-4bd4-80be-7ae646f19fda', enacted_text_binds_judiciary, conventional).
narrative_ontology:cs_axiom('e56ef18e-a65f-4bd4-80be-7ae646f19fda', foundational, amendment_sole_legitimate_evolution).
narrative_ontology:cs_axiom_status(amendment_sole_legitimate_evolution, holdable).
narrative_ontology:cs_axiom_grounding('e56ef18e-a65f-4bd4-80be-7ae646f19fda', amendment_sole_legitimate_evolution, conventional).
narrative_ontology:cs_reference_frame('e56ef18e-a65f-4bd4-80be-7ae646f19fda', enacted_text_supremacy).
narrative_ontology:cs_drift_state('e56ef18e-a65f-4bd4-80be-7ae646f19fda', contemporary_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e56ef18e-a65f-4bd4-80be-7ae646f19fda', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, popular_majorities).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, unenumerated_rights_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers constitutional interpretation under the positivist reading, enforcing textual boundaries through appellate review, precedent, and professional discipline. Judges are constrained from non-textual reasoning but retain discretion over textual ambiguity and application. Their institutional legitimacy depends on appearing bound by enacted text.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, federal_judiciary, agenda_setter,
    institutional, biographical, constrained, national).

% Benefit from the amendment-centric mechanism for constitutional change, which channels evolution through democratic consensus. The positivist reading locates legitimate constitutional development in the Article V process rather than judicial creativity, giving majorities structural control over the constitutional order.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, popular_majorities, beneficiary,
    organized, generational, mobile, national).

% Seek constitutional protections for rights not explicitly named in the constitutional text or its amendments. Under the positivist reading, they must either find a textual hook or pursue an amendment, both of which are high-barrier pathways. Their primary alternative â persuading courts to recognize evolving rights â is structurally blocked.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, unenumerated_rights_claimants, payer,
    moderate, biographical, constrained, national).

% Analyze and document the gaps between positivist theory and actual judicial practice. They track how often courts depart from pure textualism, assess the functionality of the amendment process, and evaluate whether the positivist reading coordinates interpretive stability or operates as a constraint on constitutional evolution.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for constitutional interpretation by binding judges to enacted text and democratically ratified amendments, reducing arbitrary judicial decision-making and preserving democratic legitimacy in a system of unelected judges.
% TRANSFER_FUNCTION: Moves interpretive authority from judicial moral and political reasoning to the constitutional text and the amendment process; transfers the cost of constitutional change from courts to the democratic Article V mechanism.
% ABSENT_VOICES: Living constitutionalist scholars and advocates for unenumerated rights are present in legal discourse but structurally excluded from winning in textualist adjudication; their arguments are treated as interpretively illegitimate within the positivist framework even when advanced by counsel.
% DISAPPEARANCE_RATIONALE: If the positivist constraint vanished overnight, constitutional adjudication would lose its textual anchor, judges would revert to unbounded purposive or pragmatic reasoning, the amendment process would lose its privileged status as the sole legitimate vehicle for change, and the balance of constitutional authority would shift dramatically toward judicial discretion.
% FOUNDING_PROBLEM: Judicial interpretation of a written constitution risks becoming arbitrary rule by unelected life-tenured judges, undermining democratic legitimacy and the rule of law by substituting judicial preference for enacted popular will.
% FOUNDING_PROBLEM_CORROBORATION: Textualist scholars and some democratic theorists attest the problem is still live, citing judicial overreach in non-textual rights adjudication. Living constitutionalist scholars and rights advocates attest the problem has shifted: textualism now functions as a mechanism of judicial restraint that sometimes blocks necessary constitutional protections; empirical political scientists outside the textualist camp corroborate that the Article V amendment process is effectively inoperative in the contemporary era, suggesting the coordination function has atrophied.
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__positivist_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate because the constraint genuinely limits arbitrary judicial power while also blocking non-textual rights claims. Suppression (0.62) reflects the active enforcement of textualist norms through law school training, clerk selection, appellate reversal of non-textual reasoning, and professional reputation costs. Theater_ratio (0.28) is moderate-low: judges routinely invoke textual fidelity while engaging in purposive or pragmatic reasoning, but the textual constraint is not merely performative. Accessibility_collapse (0.60) captures how, within the positivist framework, non-textual interpretive methods appear prima facie illegitimate. Resistance (0.52) reflects sustained opposition from both originalist and living-constitutionalist quarters.
 *
 * PERSPECTIVAL GAP:
 *   From the federal judiciary's seat, the positivist reading is a professional discipline that preserves democratic legitimacy and methodological clarity. From the unenumerated rights claimant's seat, the same structure appears as an actively enforced block on constitutional evolution that leaves only the politically impossible amendment path. The engine computes this divergence from the structural data: same constraint, opposite directionalities depending on whether the agent's interests are channeled through or blocked by the text-plus-amendments mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Popular majorities sit near the beneficiary end (low d) because the amendment-centric mechanism channels constitutional change toward democratic consensus, effectively subsidizing their control over the constitutional order. Unenumerated rights claimants sit near the target end (high d) because the textual constraint blocks their primary pathway to constitutional recognition and funnels them into a high-barrier amendment process. The federal judiciary sits closer to symmetric (moderate d) because textualism both constrains their discretion and grants them institutional legitimacy through methodological clarity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â arbitrary judicial power threatening democratic legitimacy â is contested rather than clearly dead, which prevents automatic piton classification. However, the mismatch between the amendment process as 'primary democratic mechanism' and its contemporary empirical inoperability raises a mandatrophy flag: the coordination function may have atrophied while the constraint persists as a theater of democratic legitimacy. The contested founding_problem_status and the rising-then-stabilizing theater_ratio support this reading without forcing premature reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_boundary_indeterminacy,
    'How much residual judicial discretion remains in determining ''what the text says,'' and does this indeterminacy undermine the positivist constraint''s claimed extraction profile?',
    'Systematic coding of judicial opinions for textual vs. purposive/originalist reasoning beneath textualist rhetoric; measurement of inter-judge variance in textual outcomes.',
    'If textual boundaries are highly indeterminate, the constraint''s suppression of non-textual reasoning is partly theatrical, raising theater_ratio and potentially shifting classification toward piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_boundary_indeterminacy, empirical, 'Residual discretion within textual interpretation').

omega_variable(
    amendment_process_viability,
    'Has the Article V amendment process become functionally inoperative, transforming the positivist reading from a genuine coordination mechanism into a one-way ratchet against constitutional evolution?',
    'Empirical analysis of amendment success rates, comparative study of amendment difficulty across constitutional systems, and assessment of whether the de facto frozen text is treated as a bug or feature by positivist proponents.',
    'If the amendment process is effectively dead, the positivist reading''s coordination function (democratic constitutional change) is hollow, and the constraint extracts interpretive flexibility without providing the promised democratic outlet.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_viability, empirical, 'Amendment process functionality under positivism').

omega_variable(
    positivism_originalism_overlap,
    'Do positivist judges consistently smuggle originalist or purposive assumptions into ''textual'' interpretation, blurring the structural boundary between the positivist and originalist readings?',
    'Lexical and conceptual analysis of positivist judicial opinions for reliance on ratification-era meaning, historical context, or expected applications that exceed pure enacted-text semantics.',
    'If overlap is systematic, the positivist reading may not be a distinct constraint but a rhetorical variant of originalism, requiring decomposition or reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivism_originalism_overlap, conceptual, 'Structural separability of positivism from originalism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_const_positivist_tr_t0, us_constitution_1787__positivist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(us_const_positivist_tr_t10, us_constitution_1787__positivist_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(us_const_positivist_tr_t20, us_constitution_1787__positivist_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(us_const_positivist_tr_t30, us_constitution_1787__positivist_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(us_const_positivist_tr_t40, us_constitution_1787__positivist_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(us_const_positivist_tr_t50, us_constitution_1787__positivist_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(us_const_positivist_be_t0, us_constitution_1787__positivist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(us_const_positivist_be_t10, us_constitution_1787__positivist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(us_const_positivist_be_t20, us_constitution_1787__positivist_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(us_const_positivist_be_t30, us_constitution_1787__positivist_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(us_const_positivist_be_t40, us_constitution_1787__positivist_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(us_const_positivist_be_t50, us_constitution_1787__positivist_reading, base_extractiveness, 50, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(us_constitution_1787__positivist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, living_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'constitutional interpretation' decomposes into at least three structurally distinct constraints: originalist_reading (historically bound), positivist_reading (text-bound, not historically bound), and living_reading (evolving meaning). Each has different epsilon values, beneficiary/victim structures, and directionality profiles. They compete for institutional adoption within the same legal community and must be modeled as separate linked stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
