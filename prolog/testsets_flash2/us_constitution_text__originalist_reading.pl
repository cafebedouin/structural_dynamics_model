% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__originalist_reading, []).

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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: US Constitution: Originalist Interpretation
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   This constraint describes the originalist reading of the US Constitution,
 *   which posits that constitutional meaning is fixed at the time of
 *   ratification and must be recovered through historical inquiry into the
 *   original public understanding. This reading acts as a rigid constraint on
 *   judicial interpretation, suppressing adaptive approaches and benefiting
 *   conservative legal movements by legitimizing outcomes aligned with
 *   historical norms. Post-ratification practice is deemed irrelevant unless
 *   it sheds light on original meaning. This is one reading of the
 *   'us_constitution_text' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.65).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.78).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "US Constitution: Originalist Interpretation").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, '217f1a38-7aaf-4a69-8509-8ff5c19cb493').
narrative_ontology:cs_kernel_codification('217f1a38-7aaf-4a69-8509-8ff5c19cb493', fixed_text).
narrative_ontology:cs_authority_grounding('217f1a38-7aaf-4a69-8509-8ff5c19cb493', lineage).
narrative_ontology:cs_interpretation_layer_present('217f1a38-7aaf-4a69-8509-8ff5c19cb493').
narrative_ontology:cs_reading_relation('217f1a38-7aaf-4a69-8509-8ff5c19cb493', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('217f1a38-7aaf-4a69-8509-8ff5c19cb493', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('217f1a38-7aaf-4a69-8509-8ff5c19cb493', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('217f1a38-7aaf-4a69-8509-8ff5c19cb493', constitutional_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('217f1a38-7aaf-4a69-8509-8ff5c19cb493', foundational, judicial_role_is_to_recover_original_meaning).
narrative_ontology:cs_axiom_status(judicial_role_is_to_recover_original_meaning, holdable).
narrative_ontology:cs_axiom_grounding('217f1a38-7aaf-4a69-8509-8ff5c19cb493', judicial_role_is_to_recover_original_meaning, conventional).
narrative_ontology:cs_reference_frame('217f1a38-7aaf-4a69-8509-8ff5c19cb493', framers_intent_supremacy).
narrative_ontology:cs_drift_state('217f1a38-7aaf-4a69-8509-8ff5c19cb493', contemporary_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('217f1a38-7aaf-4a69-8509-8ff5c19cb493', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, originalist_judges).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, rights_claimants_not_historically_grounded).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, progressive_legal_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the originalist reading by gaining institutional dominance and legitimacy for its policy preferences, framing them as historically mandated rather than politically chosen. Actively promotes and defends this interpretive method.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_movement, beneficiary,
    institutional, generational, arbitrage, national).

% Are bound by the original public understanding of the Constitution, requiring extensive historical research and adherence to past meanings. They enforce this interpretive method in their rulings, shaping legal outcomes based on historical evidence.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, originalist_judges, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the costs when their claims for rights (e.g., privacy, evolving equality standards) are rejected because they lack explicit grounding in the 18th or 19th-century public understanding of the Constitution. Their legal avenues are severely constrained.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, rights_claimants_not_historically_grounded, payer,
    powerless, immediate, trapped, national).

% Find their interpretive methods (e.g., living constitutionalism) suppressed or marginalized within the dominant legal discourse. Their academic work and advocacy for adaptive constitutional meaning face significant institutional resistance.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, progressive_legal_scholars, payer,
    moderate, generational, constrained, national).

% Judges who would interpret the Constitution as an evolving document, adapting its principles to contemporary circumstances, find their approach systematically undermined or rejected by the originalist majority, limiting their influence and the scope of their rulings.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, living_constitutionalist_judges, excluded,
    institutional, biographical, constrained, national).

% Experiences the impact of constitutional rulings shaped by originalist interpretation, which can lead to legal outcomes that diverge from contemporary societal values or needs. Their ability to influence constitutional meaning is primarily through political processes, not direct legal challenge to interpretive methods.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, general_public, payer,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:fixing_cost_class(us_constitution_text__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, stable, and purportedly objective method for interpreting the US Constitution, aiming to limit judicial discretion and ensure consistency with the framers' intent.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary societal values and evolving legal principles to historical evidence of original public understanding, benefiting those whose policy goals align with past meanings and extracting from those whose claims rely on adaptive interpretation.
% ABSENT_VOICES: Scholars and judges advocating for a 'living constitution' or a purely positivist approach are actively marginalized or excluded from the dominant interpretive discourse, their methods deemed illegitimate by originalist proponents. They would argue for a more flexible or procedural understanding of constitutional meaning.
% DISAPPEARANCE_RATIONALE: If originalism as a dominant interpretive method vanished overnight, the legal landscape would immediately shift. Judges would be free to adopt other interpretive methods, leading to different constitutional outcomes, particularly in areas of evolving rights. The conservative legal movement would lose a key pillar of its institutional power.
% FOUNDING_PROBLEM: To prevent judicial activism and ensure that constitutional meaning remains tethered to the original intent of the framers, providing a stable and predictable legal framework.
% FOUNDING_PROBLEM_CORROBORATION: Originalist proponents (e.g., Federalist Society, originalist scholars) attest that judicial activism remains a live problem requiring originalism. Critics (e.g., living constitutionalists, civil rights advocates) argue that the problem of judicial overreach is overstated or that originalism itself leads to anachronistic and unjust outcomes, making the founding problem 'dead' or transformed.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__originalist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because the method systematically disfavors rights claims not explicitly grounded in historical practice, effectively extracting from those seeking to adapt constitutional principles to modern contexts. Suppression (0.78) is high due to the institutional power of originalist proponents in the judiciary and legal academy, which actively marginalizes alternative interpretive methods. Theater ratio (0.15) is low, as the commitment to historical inquiry is generally genuine, though critics argue its application can be selective. The increasing extractiveness and suppression over time reflect the growing institutionalization and enforcement of originalism.
 *
 * PERSPECTIVAL GAP:
 *   Originalist judges and the conservative legal movement perceive this constraint as a necessary 'rope' for judicial restraint and constitutional fidelity, ensuring stability and preventing arbitrary judicial power. However, rights claimants and progressive scholars experience it as a 'snare' that actively extracts from evolving rights and suppresses adaptive legal thought, leveraging historical interpretation to achieve specific political outcomes. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The conservative legal movement and originalist judges are clear beneficiaries (low d) as this reading provides a powerful framework for their policy and judicial goals. Rights claimants whose arguments lack historical grounding and progressive legal scholars are targets (high d) as their claims and methods are systematically disfavored or suppressed. The general public is a payer, experiencing the outcomes of this interpretive method, which may diverge from contemporary values.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_recoverability,
    'To what extent is ''original public understanding'' genuinely recoverable through historical methods, or is it inherently subject to contemporary interpretive biases?',
    'Meta-analysis of historical scholarship on specific constitutional provisions, assessing consensus levels and the influence of modern frameworks on historical interpretation.',
    'If original meaning is largely unrecoverable or heavily biased, the constraint''s ''mountain'' claim of objectivity collapses, revealing a ''snare'' of selective historical application. If highly recoverable, it strengthens the ''rope'' aspect of judicial restraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_recoverability, empirical, 'The empirical feasibility and objectivity of recovering original public understanding.').

omega_variable(
    judicial_discretion_reduction,
    'Does originalism genuinely reduce judicial discretion, or does it merely shift the locus of discretion from policy choices to historical interpretation (e.g., selecting historical evidence, resolving ambiguities)?',
    'Comparative analysis of judicial opinions under originalist vs. non-originalist methods, quantifying the range of plausible outcomes and the interpretive choices made by judges.',
    'If discretion is merely shifted, the coordination function (limiting judicial power) is theatrical, increasing the ''theater_ratio'' and pushing the classification towards ''snare'' or ''piton''. If discretion is genuinely reduced, it reinforces the ''rope'' aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_reduction, conceptual, 'Whether originalism effectively limits judicial discretion or merely re-channels it.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative interpretive methods structural (institutional power, judicial appointments) or internalized (scholars self-censor, fear of professional marginalization)?',
    'Analysis of academic publication trends, hiring practices in law schools, and judicial confirmation hearings over time. If suppression persists after institutional shifts, it suggests internalized mechanisms.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them. If purely structural, changes in institutional power could more readily alter the interpretive landscape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for legal interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_text__originalist_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(us_c_tr_t1985, us_constitution_text__originalist_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_text__originalist_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_text__originalist_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_text__originalist_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_text__originalist_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(us_c_be_t1985, us_constitution_text__originalist_reading, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_text__originalist_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_text__originalist_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_text__originalist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_text__originalist_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(us_c_su_t1985, us_constitution_text__originalist_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_text__originalist_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_text__originalist_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_text__originalist_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
