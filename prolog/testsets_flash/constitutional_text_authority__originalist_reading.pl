% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Textual Authority
 *   domain: constitutional_law/legal_theory/interpretive_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the originalist reading of constitutional
 *   meaning, asserting that the Constitution's meaning is fixed at the time
 *   of its ratification and that judicial authority derives from discerning
 *   this historical public understanding. It acts as a rigid constraint on
 *   judicial discretion, requiring historical evidence to gate permissible
 *   outcomes and making it difficult to recognize unenumerated rights or for
 *   post-ratification social change to alter meaning without formal
 *   amendment. The constraint is claimed as a 'tangled_rope' because it
 *   offers a coordination function (predictability, rule of law) but also
 *   involves significant extraction by limiting the interpretive agency of
 *   certain groups and individuals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.4).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.6).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Reading of Constitutional Textual Authority").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "constitutional_law/legal_theory/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, '6b5590b1-0aa1-4984-820b-31f70b0861cc').
narrative_ontology:cs_kernel_codification('6b5590b1-0aa1-4984-820b-31f70b0861cc', fixed_text).
narrative_ontology:cs_authority_grounding('6b5590b1-0aa1-4984-820b-31f70b0861cc', lineage).
narrative_ontology:cs_interpretation_layer_present('6b5590b1-0aa1-4984-820b-31f70b0861cc').
narrative_ontology:cs_reading_relation('6b5590b1-0aa1-4984-820b-31f70b0861cc', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b5590b1-0aa1-4984-820b-31f70b0861cc', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('6b5590b1-0aa1-4984-820b-31f70b0861cc', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('6b5590b1-0aa1-4984-820b-31f70b0861cc', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('6b5590b1-0aa1-4984-820b-31f70b0861cc', foundational, judicial_discretion_constrained_by_original_meaning).
narrative_ontology:cs_axiom_status(judicial_discretion_constrained_by_original_meaning, holdable).
narrative_ontology:cs_axiom_grounding('6b5590b1-0aa1-4984-820b-31f70b0861cc', judicial_discretion_constrained_by_original_meaning, deontological).
narrative_ontology:cs_reference_frame('6b5590b1-0aa1-4984-820b-31f70b0861cc', original_public_meaning_framework).
narrative_ontology:cs_drift_state('6b5590b1-0aa1-4984-820b-31f70b0861cc', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('6b5590b1-0aa1-4984-820b-31f70b0861cc', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_judges).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, conservative_legal_scholars).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, political_factions_aligned_with_originalism).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, judicial_activists).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, proponents_of_unenumerated_rights).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, social_progressives).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, separation_of_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges who adhere to the originalist interpretive methodology, believing that the Constitution's meaning is fixed at the time of its ratification. They enforce this reading through their rulings, limiting judicial discretion and requiring historical evidence for constitutional claims. Their careers and professional identity are often tied to this interpretive approach.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, identity_locked, national).

% Academics and legal theorists who develop and promote originalist theories. Their work provides the intellectual foundation and justification for originalist judicial decisions, and their influence grows with the adoption of this reading in courts.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, conservative_legal_scholars, beneficiary,
    organized, generational, constrained, national).

% Political groups and parties whose policy goals are advanced by a strict, historically-bound interpretation of the Constitution, particularly regarding federal power, individual rights, and social issues. They benefit from the stability and predictability (from their perspective) that originalism offers.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, political_factions_aligned_with_originalism, beneficiary,
    powerful, generational, mobile, national).

% Judges and legal practitioners who advocate for a more dynamic interpretation of the Constitution, believing it should adapt to contemporary societal needs. They find their interpretive methods constrained and their rulings challenged by the originalist reading.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, judicial_activists, payer,
    moderate, biographical, constrained, national).

% Advocates for rights not explicitly listed in the Constitution but derived from its broader principles or evolving societal understanding. The originalist reading makes it significantly harder to establish or protect such rights, as it demands a historical basis for their recognition.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, proponents_of_unenumerated_rights, payer,
    powerless, generational, identity_locked, national).

% Social and political movements seeking to advance civil rights, equality, and social justice through legal means. They often find the originalist reading an impediment to legal reforms that address contemporary issues not contemplated at the time of ratification.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, social_progressives, payer,
    organized, generational, constrained, national).

% Scholars who research the historical context and original public meaning of the Constitution. While their work is central to originalist arguments, they often critique the selective use of history or the methodological rigor of judicial originalism.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, legal_historians, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for constitutional interpretation, aiming to limit judicial discretion and ensure fidelity to the original intent or public meaning of the framers, thereby coordinating legal expectations across different branches of government and over time.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary judicial discretion and evolving societal norms to historical evidence and the fixed meaning at ratification. This constrains the ability of some groups to achieve legal recognition for new rights or social changes, while empowering those whose views align with historical interpretations.
% ABSENT_VOICES: Future generations, whose values and societal conditions may diverge significantly from those at ratification, are structurally absent from the interpretive process, as their input is deemed irrelevant to the Constitution's fixed meaning. Their interests are represented only through the lens of historical interpretation or the difficult Article V amendment process.
% DISAPPEARANCE_RATIONALE: If the originalist reading vanished overnight, judicial decision-making would immediately shift towards more flexible interpretive methods. The legal landscape would rapidly rearrange, potentially leading to the recognition of new rights, reinterpretation of existing powers, and a significant change in the balance of power between the judiciary and other branches, as well as between federal and state governments.
% FOUNDING_PROBLEM: The problem of judicial overreach and the perceived politicization of the judiciary, where judges might impose their own policy preferences under the guise of constitutional interpretation, undermining the democratic process and the rule of law.
% FOUNDING_PROBLEM_CORROBORATION: Originalist proponents, conservative legal scholars, and some political factions attest that the problem of judicial activism remains live, citing contemporary court decisions they view as exceeding proper judicial bounds. Critics, including living constitutionalists and social progressives, acknowledge the historical concern but argue that a rigid originalism creates new problems of democratic deficit and societal stagnation, making the 'live' status contested.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).
:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.4) reflects the cost imposed on those seeking to adapt constitutional meaning to contemporary issues, particularly for unenumerated rights or social changes. Suppression (0.6) is moderate-high due to the active enforcement by originalist judges and the suppression of alternative interpretive methodologies within the legal system. Theater ratio (0.2) is low, as the commitment to historical inquiry is generally genuine, though debates exist about its application. Accessibility collapse (0.7) is high because, for originalists, alternative interpretive paths are largely foreclosed. Resistance (0.5) is moderate, reflecting ongoing academic and political contestation.
 *
 * PERSPECTIVAL GAP:
 *   Originalist judges and aligned political factions perceive this reading as a 'rope' or even a 'mountain' – a necessary, natural constraint that ensures fidelity to the Constitution and prevents judicial overreach. For judicial activists and proponents of unenumerated rights, it operates more like a 'snare' or 'tangled_rope', actively extracting interpretive flexibility and suppressing the recognition of evolving rights. The engine's computation of per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges and conservative legal scholars are clear beneficiaries (d near 0.0) as their interpretive framework gains institutional dominance. Political factions aligned with originalism also benefit by advancing their policy agenda. Judicial activists, proponents of unenumerated rights, and social progressives are targets (d near 1.0) as their interpretive and policy goals are actively constrained or suppressed by this reading. Legal historians act as observers, providing critical input but not directly benefiting or paying in the same structural sense.
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist reading aims to prevent mandatrophy by ensuring the Constitution's meaning does not drift from its original purpose. However, critics argue that by rigidly fixing meaning, it creates a different form of mandatrophy where the Constitution's relevance to contemporary problems atrophies, forcing social change to either conform to outdated interpretations or seek difficult amendment processes. The 'founding_problem_status' being 'contested' reflects this ongoing debate about whether the original problem of judicial overreach is still the primary concern, or if the constraint itself has become a new problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_determinacy_ambiguity,
    'Is the ''original public meaning'' of the Constitution genuinely discoverable and determinate, or is historical inquiry inherently subject to interpretive biases and indeterminacy?',
    'Consensus among non-partisan legal historians on the determinacy of specific constitutional provisions, or empirical studies on the consistency of originalist judicial outcomes across different historical contexts.',
    'If indeterminate, the originalist reading''s claim to objectivity and constraint on judicial discretion is weakened, potentially reclassifying it closer to a ''tangled_rope'' or ''snare'' where the ''historical meaning'' serves as a cover for contemporary policy preferences. If determinate, its ''rope'' or ''mountain'' claims are strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_determinacy_ambiguity, empirical, 'The discoverability and determinacy of original public meaning.').

omega_variable(
    originalism_vs_living_constitutionalism_framing,
    'Is the originalist reading a genuine attempt to constrain judicial power, or is it a strategic interpretive tool used by specific political factions to achieve policy outcomes that would otherwise be unattainable?',
    'Analysis of the consistency of originalist application across different policy areas, particularly when it conflicts with the policy preferences of its proponents. Longitudinal studies of judicial behavior and political alignment.',
    'If primarily strategic, the constraint''s ''extractiveness'' and ''suppression'' metrics would be re-evaluated upwards, and its classification would shift more definitively towards a ''snare'' or ''tangled_rope'' from all seats, as its coordination function would be revealed as cover. If genuinely principled, its ''rope'' aspects would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_vs_living_constitutionalism_framing, conceptual, 'The principled vs. strategic nature of originalist interpretation.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''constitutional_text_authority'' kernel. What would a ''living_constitutionalist_reading'' or ''positivist_reading'' change structurally?',
    'Comparative analysis of judicial decisions and legal scholarship under each reading, identifying specific legal outcomes and power distributions that differ.',
    'A ''living_constitutionalist_reading'' would likely increase judicial discretion, facilitate recognition of unenumerated rights, and reduce extraction from social progressives. A ''positivist_reading'' would emphasize formal enactment over historical meaning or moral content, potentially shifting the basis of authority and the nature of suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Structural differences between originalist, living constitutionalist, and positivist readings of constitutional authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1970, constitutional_text_authority__originalist_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(cons_tr_t1980, constitutional_text_authority__originalist_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(cons_tr_t1990, constitutional_text_authority__originalist_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(cons_tr_t2000, constitutional_text_authority__originalist_reading, theater_ratio, 2000, 0.17).
narrative_ontology:measurement(cons_tr_t2010, constitutional_text_authority__originalist_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(cons_tr_t2024, constitutional_text_authority__originalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t1970, constitutional_text_authority__originalist_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(cons_be_t1980, constitutional_text_authority__originalist_reading, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(cons_be_t1990, constitutional_text_authority__originalist_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(cons_be_t2000, constitutional_text_authority__originalist_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(cons_be_t2010, constitutional_text_authority__originalist_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(cons_be_t2024, constitutional_text_authority__originalist_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1970, constitutional_text_authority__originalist_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(cons_su_t1980, constitutional_text_authority__originalist_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(cons_su_t1990, constitutional_text_authority__originalist_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(cons_su_t2000, constitutional_text_authority__originalist_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(cons_su_t2010, constitutional_text_authority__originalist_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(cons_su_t2024, constitutional_text_authority__originalist_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__positivist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, judicial_review_doctrine).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, unenumerated_rights_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'constitutional_text_authority' kernel. Each reading represents a different structural constraint with its own ε, beneficiaries, and victims. They are linked here to show their interrelationship within the broader legal theory domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
