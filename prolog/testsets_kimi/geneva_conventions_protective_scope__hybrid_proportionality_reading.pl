% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Geneva Conventions Protective Scope â Hybrid Proportionality Reading
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid proportionality reading of the
 *   Geneva Conventions protective scope kernel. It treats the treaty
 *   framework as establishing distinct protective regimes (AP I for
 *   international armed conflict; AP II/Common Article 3 for
 *   non-international armed conflict) where proportionality analysis mediates
 *   the application of force and protective status. The reading is one of
 *   three in a contested kernel; siblings are the state-centric reading
 *   (strict Article 4 combatant criteria) and the universal rights reading
 *   (human-rights-based universal floor regardless of conflict
 *   classification). The structural feature of this reading is that the
 *   proportionality calculus and conflict-classification gateway create
 *   interpretive ambiguity that scales with the stronger party's
 *   military-legal capacity, producing asymmetric extraction through a
 *   genuine coordination mechanism.
 *
 * KEY AGENTS:
 *   - state_parties: Agenda-setter (institutional/global) â control treaty ratification and conflict classification
 *   - stronger_belligerents: Beneficiary (powerful/global) â capture interpretive flexibility and operational latitude
 *   - weaker_belligerents: Payer (moderate/global) â bear classification uncertainty and protective gaps
 *   - civilian_populations: Payer (powerless/global) â bear humanitarian cost of proportionality balancing
 *   - international_judiciary: Observer (institutional/global) â post-hoc adjudication without enforcement
 *   - icrc: Observer (institutional/global) â monitoring and confidential diplomacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.61).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.54).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Geneva Conventions Protective Scope â Hybrid Proportionality Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'ef56e3e7-5e25-47dc-a179-875da116b1d0').
narrative_ontology:cs_kernel_codification('ef56e3e7-5e25-47dc-a179-875da116b1d0', formalized).
narrative_ontology:cs_authority_grounding('ef56e3e7-5e25-47dc-a179-875da116b1d0', lineage).
narrative_ontology:cs_interpretation_layer_present('ef56e3e7-5e25-47dc-a179-875da116b1d0').
narrative_ontology:cs_reading_relation('ef56e3e7-5e25-47dc-a179-875da116b1d0', geneva_conventions_protective_scope__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('ef56e3e7-5e25-47dc-a179-875da116b1d0', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('ef56e3e7-5e25-47dc-a179-875da116b1d0', foundational, protective_scope_scales_by_conflict_type).
narrative_ontology:cs_axiom_status(protective_scope_scales_by_conflict_type, holdable).
narrative_ontology:cs_axiom_grounding('ef56e3e7-5e25-47dc-a179-875da116b1d0', protective_scope_scales_by_conflict_type, conventional).
narrative_ontology:cs_axiom('ef56e3e7-5e25-47dc-a179-875da116b1d0', foundational, proportionality_mediates_application).
narrative_ontology:cs_axiom_status(proportionality_mediates_application, holdable).
narrative_ontology:cs_axiom_grounding('ef56e3e7-5e25-47dc-a179-875da116b1d0', proportionality_mediates_application, instrumental).
narrative_ontology:cs_reference_frame('ef56e3e7-5e25-47dc-a179-875da116b1d0', treaty_based_graduated_protection).
narrative_ontology:cs_drift_state('ef56e3e7-5e25-47dc-a179-875da116b1d0', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ef56e3e7-5e25-47dc-a179-875da116b1d0', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, stronger_belligerents).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_belligerents).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and ratified the 1949 Geneva Conventions and 1977 Additional Protocols. Through official statements and military legal opinions, determine whether a situation qualifies as an international or non-international armed conflict, triggering distinct protective regimes. Retain sovereign control over classification decisions and proportionality assessments.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_parties, agenda_setter,
    institutional, generational, constrained, global).

% Possess advanced military-legal institutions capable of framing conflicts, conducting proportionality analyses, and generating legal opinions that classify opponents as unprivileged belligerents or characterize conflicts as non-international. Exercise operational discretion within interpretive gaps created by the scaling framework.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, stronger_belligerents, beneficiary,
    powerful, biographical, constrained, global).

% Non-state armed groups or militarily inferior state forces whose fighters risk classification as unprivileged belligerents outside full AP I protections. Depend on adversary or third-party recognition for protective status and cannot unilaterally secure AP I application.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_belligerents, payer,
    moderate, biographical, trapped, global).

% Inhabit conflict zones where the level of legal protection depends on how states classify the conflict and balance military necessity against civilian harm. Experience protection gaps when proportionality analysis is conducted by the attacking party.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_populations, payer,
    powerless, immediate, trapped, global).

% Adjudicate war crimes and conflict classification through international criminal tribunals and the ICC. Attempt to clarify proportionality standards and classification criteria post hoc, but rely on state cooperation and face enforcement limitations.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_judiciary, observer,
    institutional, generational, analytical, global).

% Monitors compliance and promotes IHL adherence. Provides confidential representations to parties but cannot override state classification decisions. Operates under consent-based access constraints.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, icrc, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__hybrid_proportionality_reading, stronger_belligerents).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__hybrid_proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a graduated legal framework for protecting victims of armed conflict based on formal conflict classification, providing baseline rules that prevent total war and regulate conduct between belligerents.
% TRANSFER_FUNCTION: Moves legal certainty and protective status from weaker belligerents and civilian populations to stronger parties through interpretive ambiguity in conflict classification and proportionality analysis.
% ABSENT_VOICES: Victims in unclassified or frozen conflicts where no state acknowledges armed conflict status; non-state actors excluded from treaty drafting and with limited standing to challenge classification before international tribunals.
% DISAPPEARANCE_RATIONALE: If the hybrid proportionality framework vanished, states would lose the primary legal architecture for legitimizing distinctions between combatant categories; humanitarian organizations would lose treaty-based access arguments; weaker parties would lose even nominal protective claims, while stronger parties would lose a key source of interpretive flexibility.
% FOUNDING_PROBLEM: How to regulate the conduct of hostilities and protect war victims after WWII without eliminating state sovereign prerogatives to classify conflicts and determine military necessity.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of 1949 diplomatic conferences and 1974-1977 Additional Protocol negotiations show state delegates affirming the need for graduated protections reflecting state consent and military reality. Critical legal scholars and human rights advocates outside the state-party framework contest that the compromise was necessary, arguing that a universal protective floor would have been achievable.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.61, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) is moderate-high because the proportionality gateway and conflict-classification scale allow stronger parties to recategorize conflicts and narrow protections systematically. Suppression (0.54) is moderate because the lex specialis doctrine suppresses human rights alternatives and states actively enforce their preferred classifications through military legal opinions and diplomatic refusal. Theater ratio (0.48) is moderate because states perform compliance with Geneva while exploiting interpretive gaps, especially in counter-terrorism and drone warfare. Accessibility collapse (0.67) is substantial because once a conflict is classified by the stronger party, legal alternatives collapse for weaker parties who lack standing or capacity to challenge. Resistance (0.48) is moderate from human rights advocates and some international judicial actors, but fragmented.
 *
 * PERSPECTIVAL GAP:
 *   From the state_party and stronger_belligerent seat, the constraint appears as a necessary legal architecture that civilizes warfare while preserving sovereign and operational flexibility. From the weaker_belligerent and civilian_population seat, the same structure appears as a classification trap where protective status is determined by the opponent's legal arguments. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   stronger_belligerents are declared beneficiaries (dampened directionality, subsidy-like relationship to the interpretive ambiguity); weaker_belligerents and civilian_populations are declared victims (amplified directionality toward full target); state_parties as agenda_setters sit between, maintaining the framework; international_judiciary and icrc are observers with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both coordination and extraction elements. A pure rope reading would ignore the systematic advantage stronger parties derive from classification ambiguity. A pure snare reading would ignore the genuine protective function the framework provides when applied in good faith. The tangled rope classification captures that the same legal mechanism coordinates humanitarian protection and extracts legal advantage, requiring active enforcement (military legal opinions, tribunal jurisdiction, state diplomatic pressure) to hold the structure in place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_manipulation_ambiguity,
    'Does the proportionality calculus structurally privilege military necessity over humanitarian protection because the attacker conducts the balancing?',
    'Comparative empirical analysis of targeting decisions and international judicial review outcomes to determine whether proportionality assessments systematically favor operational goals.',
    'If proportionality is structurally biased, the constraint''s extractiveness is higher than the coordination function suggests, and the hybrid reading leans toward snare-like operation in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_manipulation_ambiguity, conceptual, 'Structural bias in proportionality balancing').

omega_variable(
    conflict_classification_capture,
    'To what extent do stronger parties systematically classify conflicts as non-international to invoke lower AP II/Common Article 3 thresholds rather than AP I?',
    'Quantitative review of state conflict classifications, ICRC access negotiations, and international tribunal jurisdiction decisions across asymmetric conflicts since 1977.',
    'If classification capture is widespread, the conflict-type gateway functions primarily as an extraction mechanism for stronger parties, and the coordination benefit is concentrated in symmetric interstate wars.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conflict_classification_capture, empirical, 'Empirical prevalence of conflict classification manipulation').

omega_variable(
    geneva_kernel_reading_underdetermination,
    'Does the Geneva Conventions treaty text underdetermine the choice between graduated conflict-type scaling, strict state-centric combatant criteria, and universal human rights floors?',
    'Comparative analysis of state practice, opinio juris, and international judicial reasoning to determine whether the kernel is genuinely ambiguous or whether one reading has achieved interpretive dominance.',
    'If the kernel is underdetermined, all three readings remain live and the constraint''s classification depends on which community''s interpretation is adopted. If one reading has achieved dominance, the others become doctrinal residues.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geneva_kernel_reading_underdetermination, conceptual, 'Kernel text ambiguity across sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(gene_tr_t9, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 9, 0.3).
narrative_ontology:measurement(gene_tr_t18, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(gene_tr_t27, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 27, 0.44).
narrative_ontology:measurement(gene_tr_t36, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 36, 0.46).
narrative_ontology:measurement(gene_tr_t45, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 45, 0.48).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gene_be_t9, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 9, 0.4).
narrative_ontology:measurement(gene_be_t18, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 18, 0.5).
narrative_ontology:measurement(gene_be_t27, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 27, 0.56).
narrative_ontology:measurement(gene_be_t36, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 36, 0.59).
narrative_ontology:measurement(gene_be_t45, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 45, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gene_su_t9, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 9, 0.38).
narrative_ontology:measurement(gene_su_t18, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 18, 0.48).
narrative_ontology:measurement(gene_su_t27, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 27, 0.54).
narrative_ontology:measurement(gene_su_t36, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 36, 0.56).
narrative_ontology:measurement(gene_su_t45, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 45, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, universal_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the geneva_conventions_protective_scope kernel. The natural-language label 'Geneva protections' conflates three structurally distinct readings: hybrid proportionality (this file), state-centric (strict Article 4 criteria), and universal rights (human rights floor). Each has distinct epsilon values, beneficiary/victim structures, and classification implications. They are modeled as separate constraints linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
