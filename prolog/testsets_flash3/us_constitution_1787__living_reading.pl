% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__living_reading, []).

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
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: US Constitution (Living Reading)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'living constitution' reading of the US
 *   Constitution, where its meaning is understood to evolve with societal
 *   values and needs, serving as an aspirational framework rather than a
 *   fixed text. This reading allows for the expansion of rights and
 *   adaptation to modern challenges, but also introduces potential for
 *   judicial discretion and elite capture of 'evolving norms.' It is one of
 *   several competing interpretations of the US Constitution.
 *
 * KEY AGENTS:
 *   - judicial_activists: Primary agenda-setter (institutional/constrained) — actively shapes constitutional law.
 *   - evolving_social_movements: Primary beneficiary (organized/mobile) — benefits from flexible interpretation.
 *   - originalist_scholars: Primary payer (moderate/identity_locked) — bears the cost of interpretive shifts.
 *   - states_rights_advocates: Secondary payer (organized/constrained) — sees positions undermined by expansive interpretations.
 *   - average_citizen: Diffuse beneficiary/payer (powerless/trapped) — benefits from rights expansion, bears costs of judicial discretion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__living_reading, 0.45).
domain_priors:suppression_score(us_constitution_1787__living_reading, 0.3).
domain_priors:theater_ratio(us_constitution_1787__living_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__living_reading, rope).
narrative_ontology:human_readable(us_constitution_1787__living_reading, "US Constitution (Living Reading)").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, '86f81609-8d38-44d1-bb9b-926ef6ae61b4').
narrative_ontology:cs_kernel_codification('86f81609-8d38-44d1-bb9b-926ef6ae61b4', fixed_text).
narrative_ontology:cs_authority_grounding('86f81609-8d38-44d1-bb9b-926ef6ae61b4', lineage).
narrative_ontology:cs_interpretation_layer_present('86f81609-8d38-44d1-bb9b-926ef6ae61b4').
narrative_ontology:cs_reading_relation('86f81609-8d38-44d1-bb9b-926ef6ae61b4', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('86f81609-8d38-44d1-bb9b-926ef6ae61b4', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('86f81609-8d38-44d1-bb9b-926ef6ae61b4', foundational, constitutional_meaning_evolves).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves, holdable).
narrative_ontology:cs_axiom_grounding('86f81609-8d38-44d1-bb9b-926ef6ae61b4', constitutional_meaning_evolves, conventional).
narrative_ontology:cs_axiom('86f81609-8d38-44d1-bb9b-926ef6ae61b4', foundational, constitution_as_aspirational_framework).
narrative_ontology:cs_axiom_status(constitution_as_aspirational_framework, holdable).
narrative_ontology:cs_axiom_grounding('86f81609-8d38-44d1-bb9b-926ef6ae61b4', constitution_as_aspirational_framework, instrumental).
narrative_ontology:cs_reference_frame('86f81609-8d38-44d1-bb9b-926ef6ae61b4', evolving_societal_consensus).
narrative_ontology:cs_drift_state('86f81609-8d38-44d1-bb9b-926ef6ae61b4', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('86f81609-8d38-44d1-bb9b-926ef6ae61b4', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, judicial_activists).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, evolving_social_movements).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, originalist_scholars).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, states_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, average_citizen).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, average_citizen).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, evolving_standards_of_decency).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, substantive_due_process).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges who interpret the Constitution as a living document, adapting its meaning to contemporary societal values and needs. They actively shape constitutional law to address modern issues, often expanding rights or limiting governmental power beyond the text's original intent. Their authority is derived from the perceived necessity of a flexible constitution.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, judicial_activists, agenda_setter,
    institutional, biographical, constrained, national).

% Advocacy groups and social movements that seek to expand rights or achieve social justice through constitutional interpretation. They benefit from a living constitution's flexibility, which allows for the recognition of new rights (e.g., privacy, LGBTQ+ rights) not explicitly enumerated in the original text. Their influence is through public discourse and legal challenges.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, evolving_social_movements, beneficiary,
    organized, generational, mobile, national).

% Legal academics and jurists who adhere to the belief that the Constitution's meaning is fixed at the time of its adoption. They bear the cost of seeing their interpretive methodology sidelined or explicitly rejected by courts adopting a living constitutionalism approach, leading to outcomes they view as illegitimate. Their identity is deeply tied to historical fidelity.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_scholars, payer,
    moderate, civilizational, identity_locked, national).

% Political groups and legal practitioners who argue for a more limited federal government and greater autonomy for individual states. They often find their positions undermined by expansive interpretations of federal power or individual rights under a living constitution, which can preempt state laws or policies. Their options are political action or litigation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, states_rights_advocates, payer,
    organized, generational, constrained, national).

% Benefits from the expansion of individual rights and protections that a living constitution can provide, adapting to modern challenges. However, they also bear the cost of judicial decisions that may be perceived as undemocratic or that shift power dynamics without direct electoral accountability. Their participation is primarily through voting and civic engagement.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, average_citizen, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, average_citizen, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a flexible framework for governance that can adapt to unforeseen societal changes and moral developments without requiring frequent formal amendment, thus maintaining constitutional stability while allowing for progress.
% TRANSFER_FUNCTION: Transfers interpretive authority from the original framers' intent to contemporary judicial and societal understandings, leading to shifts in rights, powers, and limitations between federal and state governments, and between individuals and the state.
% ABSENT_VOICES: Future generations who might prefer a more fixed or different constitutional interpretation are not directly represented in current 'living' interpretations, though their interests are often invoked. The 'dead hand' of the past is explicitly rejected, but the 'unborn hand' of the future is also unrepresented.
% DISAPPEARANCE_RATIONALE: If the living reading of the Constitution vanished, the legal system would immediately face a crisis of legitimacy and interpretation. Courts would struggle to apply an 18th-century document to 21st-century problems, leading to widespread legal uncertainty, a collapse of many established rights, and immense pressure for a new constitutional convention or a complete re-evaluation of judicial review.
% FOUNDING_PROBLEM: The framers of the Constitution understood that a document written in one era might not adequately address the challenges and moral understandings of future generations, risking obsolescence or revolution if too rigid.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists widely corroborate the framers' intent for a durable but adaptable document, acknowledging the difficulty of anticipating future societal needs. Legal scholars across various interpretive schools, even those critical of living constitutionalism, agree that a completely static constitution would be problematic for a dynamic society.
narrative_ontology:disappearance_verdict(us_constitution_1787__living_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__living_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__living_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_1787__living_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__living_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__living_reading_tests).
:- end_tests(us_constitution_1787__living_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the transfer of interpretive power from a fixed text to a more dynamic, judicially mediated process, which can impose costs on those who prefer a more stable or text-bound interpretation. Suppression (0.30) is moderate, as alternative readings are not entirely suppressed but are often outmaneuvered in legal discourse. Theater ratio (0.10) is low, indicating that the interpretive function is genuine, though its application may be contested. Accessibility collapse (0.40) is moderate, as alternative interpretive paths exist but are constrained by judicial precedent. Resistance (0.50) is significant, reflecting ongoing debates and challenges from other interpretive schools.
 *
 * PERSPECTIVAL GAP:
 *   Judicial activists and social movements experience this as a necessary and beneficial 'rope' that allows for societal progress and justice. Originalist scholars and states' rights advocates, however, experience it as a 'tangled rope' or even a 'snare,' where their preferred constitutional order is undermined by an unmoored judiciary. The engine's per-seat classification will reflect these divergent experiences based on their declared power, exit options, and roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Judicial activists and evolving social movements are beneficiaries (low d) as they gain interpretive power and see their values reflected in constitutional law. Originalist scholars and states' rights advocates are targets (high d) as their interpretive framework and policy preferences are often overridden. The average citizen is a mixed beneficiary/payer, benefiting from expanded rights but potentially bearing the costs of judicial overreach.
 *
 * MANDATROPHY ANALYSIS:
 *   The living reading prevents mandatrophy by ensuring the Constitution remains relevant and functional in a changing world, thus avoiding the obsolescence that a purely static interpretation might induce. However, it introduces a different risk: that the 'mandate' of adaptation could be captured by specific elite interests, leading to an 'evolving' constitution that serves a narrow agenda rather than broad societal needs. The omegas address this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_capture_of_evolving_norms,
    'To what extent do ''evolving societal norms'' reflect genuine broad consensus versus the preferences of specific judicial or intellectual elites?',
    'Empirical sociological studies of public opinion on constitutional issues, compared with judicial outcomes. Analysis of the composition and ideological leanings of influential legal scholars and judges.',
    'If elite capture is substantial, the ''living reading'' functions more as a ''snare'' for the broader populace, as constitutional meaning is shaped by a narrow group rather than genuinely evolving society. If broad consensus is reflected, it reinforces the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_of_evolving_norms, empirical, 'Assesses the democratic legitimacy of ''evolving norms'' in living constitutionalism.').

omega_variable(
    interpretive_stability_vs_flexibility,
    'What is the optimal balance between constitutional stability (predictability of meaning) and flexibility (adaptability to change), and does the living reading achieve this balance?',
    'Comparative legal analysis across different constitutional systems with varying interpretive traditions. Long-term studies of judicial decision-making and public trust in the judiciary under different interpretive regimes.',
    'If the living reading leads to excessive instability or unpredictability, it could undermine the rule of law, pushing it towards a ''tangled rope'' or ''snare'' due to lack of clear guidance. If it provides necessary flexibility without undue instability, it reinforces its ''rope'' function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_stability_vs_flexibility, conceptual, 'Examines the trade-off between stability and flexibility in constitutional interpretation.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''living reading'' of the US Constitution, or is it a distinct, functionally separate constraint that merely uses the Constitution as a legitimizing narrative?',
    'Analysis of judicial opinions: do they genuinely engage with the text and history, even while reinterpreting it, or do they primarily invoke ''evolving norms'' as a standalone justification? If the latter, it suggests a separate constraint.',
    'If it''s a separate constraint, its extractiveness and suppression might be higher, as it would be less constrained by the kernel itself. If it''s a genuine reading, its classification as a ''rope'' is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishes a genuine reading from a legitimizing narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__living_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_1787__living_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_1787__living_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_1787__living_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_1787__living_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_1787__living_reading, theater_ratio, 50, 0.09).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_1787__living_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(us_c_tr_t70, us_constitution_1787__living_reading, theater_ratio, 70, 0.1).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__living_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(us_c_be_t10, us_constitution_1787__living_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(us_c_be_t20, us_constitution_1787__living_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(us_c_be_t30, us_constitution_1787__living_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(us_c_be_t40, us_constitution_1787__living_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(us_c_be_t50, us_constitution_1787__living_reading, base_extractiveness, 50, 0.43).
narrative_ontology:measurement(us_c_be_t60, us_constitution_1787__living_reading, base_extractiveness, 60, 0.44).
narrative_ontology:measurement(us_c_be_t70, us_constitution_1787__living_reading, base_extractiveness, 70, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__living_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(us_c_su_t10, us_constitution_1787__living_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(us_c_su_t20, us_constitution_1787__living_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(us_c_su_t30, us_constitution_1787__living_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement(us_c_su_t40, us_constitution_1787__living_reading, suppression_requirement, 40, 0.27).
narrative_ontology:measurement(us_c_su_t50, us_constitution_1787__living_reading, suppression_requirement, 50, 0.29).
narrative_ontology:measurement(us_c_su_t60, us_constitution_1787__living_reading, suppression_requirement, 60, 0.3).
narrative_ontology:measurement(us_c_su_t70, us_constitution_1787__living_reading, suppression_requirement, 70, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, judicial_review_doctrine).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, federal_supremacy_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three primary readings of the US Constitution (1787). It is linked to the originalist and positivist readings as competing interpretations of the same kernel. Each reading instantiates a distinct constraint with its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
