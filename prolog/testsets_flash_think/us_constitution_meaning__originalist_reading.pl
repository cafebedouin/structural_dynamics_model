% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__originalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: Originalist Reading of U.S. Constitutional Meaning
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint story models the 'originalist reading' of U.S.
 *   Constitutional meaning, where judges are bound by the historical public
 *   meaning of the text at the time of its ratification or amendment. This
 *   interpretive theory aims to provide a stable, objective basis for
 *   constitutional law, limiting judicial discretion. However, its
 *   application often results in the suppression of contemporary rights
 *   claims lacking historical precedent, leading to significant extraction
 *   from certain groups. The constraint is claimed as a Tangled Rope,
 *   reflecting its dual function of coordinating judicial decision-making
 *   while imposing asymmetric costs.
 *
 * KEY AGENTS:
 *   - Originalist Judges: Primary agenda-setters (institutional/constrained) – enforce the interpretive method.
 *   - Counter-Majoritarian Constraint Advocates: Primary beneficiaries (organized/mobile) – benefit from limited judicial discretion.
 *   - Rights Claimants Lacking Historical Support: Primary targets/payers (powerless/trapped) – bear the costs of suppressed claims.
 *   - Living Constitutionalist Scholars: Excluded voices (organized/constrained) – their interpretive framework is actively opposed.
 *   - Legal Positivist Scholars: Analytical observers (analytical/analytical) – study the interpretive debate.
 *   - General Public: Payer (moderate/constrained) – experiences the societal impact of rulings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.78).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.85).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "Originalist Reading of U.S. Constitutional Meaning").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, '806e2bca-57bf-46e1-a0e7-b87f26675d43').
narrative_ontology:cs_kernel_codification('806e2bca-57bf-46e1-a0e7-b87f26675d43', fixed_text).
narrative_ontology:cs_authority_grounding('806e2bca-57bf-46e1-a0e7-b87f26675d43', lineage).
narrative_ontology:cs_interpretation_layer_present('806e2bca-57bf-46e1-a0e7-b87f26675d43').
narrative_ontology:cs_reading_relation('806e2bca-57bf-46e1-a0e7-b87f26675d43', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('806e2bca-57bf-46e1-a0e7-b87f26675d43', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('806e2bca-57bf-46e1-a0e7-b87f26675d43', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('806e2bca-57bf-46e1-a0e7-b87f26675d43', constitutional_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('806e2bca-57bf-46e1-a0e7-b87f26675d43', foundational, judicial_fidelity_to_original_meaning).
narrative_ontology:cs_axiom_status(judicial_fidelity_to_original_meaning, holdable).
narrative_ontology:cs_axiom_grounding('806e2bca-57bf-46e1-a0e7-b87f26675d43', judicial_fidelity_to_original_meaning, deontological).
narrative_ontology:cs_reference_frame('806e2bca-57bf-46e1-a0e7-b87f26675d43', original_public_meaning_fidelity).
narrative_ontology:cs_drift_state('806e2bca-57bf-46e1-a0e7-b87f26675d43', contemporary_judicial_practice, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('806e2bca-57bf-46e1-a0e7-b87f26675d43', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_judges).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, rights_claimants_lacking_historical_support).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, living_constitutionalist_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges who adhere to originalist interpretive methods, believing they are bound by the historical public meaning of the Constitution. They actively enforce this interpretive constraint, shaping legal outcomes and judicial precedent.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, constrained, national).

% Legal scholars, political activists, and organizations who advocate for originalism as a means to limit judicial discretion and prevent outcomes they view as inconsistent with the nation's founding principles. They benefit from the stability and predictability originalism purports to offer.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates, beneficiary,
    organized, generational, mobile, national).

% Individuals or groups seeking to assert rights or legal claims that are not explicitly supported by the historical public meaning of the Constitution at the time of its ratification or amendment. Their claims are often suppressed or denied under originalist interpretation.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, rights_claimants_lacking_historical_support, payer,
    powerless, immediate, trapped, national).

% Academics and legal practitioners who advocate for a 'living' Constitution, where its principles evolve with societal values and contemporary circumstances. Their interpretive methods are often dismissed or actively opposed by originalist frameworks, limiting their influence in judicial discourse.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, living_constitutionalist_scholars, excluded,
    organized, biographical, constrained, national).

% Scholars who focus on the formal validity and institutional recognition of law, rather than its moral content or historical meaning. They observe the originalist debate as a contest over interpretive authority within the legal system.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, legal_positivist_scholars, observer,
    analytical, biographical, analytical, national).

% The citizenry whose lives are affected by constitutional rulings. While some may benefit from perceived stability, others may find their contemporary values and needs unaddressed by a historically fixed interpretation, bearing the costs of suppressed social change.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, general_public, payer,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for constitutional interpretation, aiming to limit judicial discretion and ensure fidelity to the original understanding of the text.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary societal values and evolving norms to historical evidence of original intent or public meaning, effectively shifting power from majoritarian processes to historical constraints.
% ABSENT_VOICES: Living constitutionalist scholars, rights claimants whose arguments rely on evolving societal standards, and those advocating for a more adaptable constitutional framework are often marginalized or excluded from the dominant discourse within originalist legal circles.
% DISAPPEARANCE_RATIONALE: If originalism as an interpretive method vanished overnight, constitutional interpretation would immediately shift, likely leading to different legal outcomes, especially in areas like privacy, equality, and executive power. The judiciary's role and legitimacy would be fundamentally re-evaluated, and legal education would undergo significant reform.
% FOUNDING_PROBLEM: To prevent judicial activism, ensure fidelity to the original compact, and maintain the Constitution's authority as supreme law rather than a malleable document subject to the whims of individual judges or changing political tides.
% FOUNDING_PROBLEM_CORROBORATION: Originalist legal scholars, conservative political movements, and some judicial appointments attest to the ongoing problem of judicial overreach and the need for interpretive constraint. Critics (e.g., living constitutionalists, some legal historians) argue the founding problem is misdiagnosed, that originalism creates new problems, or that the 'problem' is a political rather than a legal one.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because originalism, while providing a framework, often denies or limits rights based on historical absence, imposing significant costs on those whose claims are not historically grounded. Suppression is very high (0.85) as the interpretive method actively forecloses alternative readings and judicial outcomes, requiring consistent enforcement by judges and legal institutions. Theater ratio is moderate (0.40): while there is genuine intellectual and legal work in historical reconstruction, a portion of the effort is performative, emphasizing fidelity to a 'fixed' meaning to legitimize specific outcomes. Accessibility collapse is high (0.75) because the interpretive method significantly narrows the range of viable legal arguments. Resistance is also high (0.70) due to ongoing academic, legal, and political challenges from those advocating for alternative interpretive approaches.
 *
 * PERSPECTIVAL GAP:
 *   Originalist judges and their advocates perceive this constraint as a necessary 'rope' for judicial legitimacy and constitutional stability, ensuring the rule of law. From the perspective of rights claimants and living constitutionalist scholars, it operates as a 'snare' or 'tangled_rope,' extracting from marginalized groups by denying evolving rights and suppressing alternative interpretations. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges and counter-majoritarian advocates are beneficiaries, as the constraint empowers their interpretive framework and limits outcomes they oppose. Rights claimants and living constitutionalist scholars are victims, as their claims and interpretive approaches are systematically disadvantaged or excluded. The general public is a payer, experiencing the societal costs of a fixed constitutional meaning that may not align with contemporary needs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to prevent judicial activism and ensure fidelity to the original compact is still 'live' for its proponents. However, critics argue that the original problem has evolved, and originalism itself has become a mechanism for political outcomes rather than pure interpretive constraint, suggesting a potential for mandatrophy where the original function is superseded by an extractive one. The high and increasing extractiveness and suppression metrics, alongside the 'contested' status of the founding problem, indicate this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_epistemology,
    'To what extent is it genuinely possible to ascertain the ''original intent'' or ''original public meaning'' of constitutional text, especially for concepts not contemplated at the time of ratification?',
    'Further historical and linguistic scholarship, combined with philosophical analysis of interpretive limits. Consensus among diverse historians and linguists on the knowability of specific meanings.',
    'If original meaning is largely unknowable or indeterminate for many modern issues, the constraint''s claim to objectivity weakens, potentially reclassifying it closer to a Snare (if used for pure extraction) or Piton (if maintained theatrically).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_epistemology, empirical, 'The epistemic limits of historical constitutional interpretation.').

omega_variable(
    original_public_meaning_scope,
    'How broadly or narrowly should ''original public meaning'' be applied? Does it encompass general principles or only specific historical applications?',
    'Judicial consensus on interpretive methodology, or legislative clarification of interpretive rules (though this itself would be constitutionally contested).',
    'A narrow application increases extractiveness by limiting rights to specific historical instances; a broad application (e.g., ''original principles'') might reduce extractiveness but risks blurring the line with living constitutionalism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_scope, conceptual, 'Ambiguity in the scope and application of original public meaning.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of non-originalist outcomes primarily structural (e.g., stare decisis, judicial appointments) or internalized (e.g., judges'' self-restraint, legal education)?',
    'Analysis of judicial behavior in the absence of strong external enforcement, or studies on the impact of legal education on interpretive methods.',
    'If internalized, the effective suppression is higher than structural measures suggest, as judges carry the constraint with them. If purely structural, removing external barriers would lead to faster interpretive shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in judicial interpretation.').

omega_variable(
    framing_under_determination,
    'Is the originalist framing the only defensible approach to constitutional interpretation, or do alternative framings (e.g., living constitutionalism, positivism) offer equally coherent, albeit different, classifications?',
    'Philosophical and legal debate, and the ongoing contestation within the judiciary and academia. The persistence and coherence of sibling readings (living_constitutionalist_reading, positivist_reading) serve as evidence.',
    'If alternative framings are equally coherent, the classification of ''us_constitution_meaning'' as a kernel with multiple readings is validated. If originalism were demonstrably the only coherent framing, the kernel itself might collapse into a single, universally accepted constraint (unlikely).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'Under-determination of constitutional meaning framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_meaning__originalist_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_meaning__originalist_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_meaning__originalist_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_meaning__originalist_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_meaning__originalist_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_meaning__originalist_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_meaning__originalist_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(us_c_be_t1980, us_constitution_meaning__originalist_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_meaning__originalist_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_meaning__originalist_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_meaning__originalist_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_meaning__originalist_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_meaning__originalist_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(us_c_su_t1980, us_constitution_meaning__originalist_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_meaning__originalist_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_meaning__originalist_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_meaning__originalist_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_meaning__originalist_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'us_constitution_meaning' kernel. Its structural properties and classification differ significantly from sibling readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
