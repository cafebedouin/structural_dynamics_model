% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__popular_constitutionalism_reading, []).

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
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: US Constitutional Meaning: Popular Constitutionalism Reading
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint describes the 'popular constitutionalism' reading of US
 *   constitutional meaning, where the Constitution's interpretation is
 *   primarily shaped by popular political movements and democratic
 *   contestation, rather than being solely the domain of judicial
 *   interpretation. It challenges judicial supremacy, asserting that
 *   constitutional meaning emerges from ongoing political struggle. This
 *   reading is one of several competing interpretations of the US
 *   Constitution, forming a kernel of contested authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.6).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.4).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "US Constitutional Meaning: Popular Constitutionalism Reading").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, 'a1f828af-f993-4ded-a4f9-814da7c3ec7e').
narrative_ontology:cs_kernel_codification('a1f828af-f993-4ded-a4f9-814da7c3ec7e', fixed_text).
narrative_ontology:cs_authority_grounding('a1f828af-f993-4ded-a4f9-814da7c3ec7e', practice).
narrative_ontology:cs_interpretation_layer_present('a1f828af-f993-4ded-a4f9-814da7c3ec7e').
narrative_ontology:cs_reading_relation('a1f828af-f993-4ded-a4f9-814da7c3ec7e', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1f828af-f993-4ded-a4f9-814da7c3ec7e', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('a1f828af-f993-4ded-a4f9-814da7c3ec7e', foundational, popular_sovereignty_is_supreme_interpreter).
narrative_ontology:cs_axiom_status(popular_sovereignty_is_supreme_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('a1f828af-f993-4ded-a4f9-814da7c3ec7e', popular_sovereignty_is_supreme_interpreter, deontological).
narrative_ontology:cs_axiom('a1f828af-f993-4ded-a4f9-814da7c3ec7e', secondary, judicial_review_is_not_final_word).
narrative_ontology:cs_axiom_status(judicial_review_is_not_final_word, holdable).
narrative_ontology:cs_axiom_grounding('a1f828af-f993-4ded-a4f9-814da7c3ec7e', judicial_review_is_not_final_word, conventional).
narrative_ontology:cs_reference_frame('a1f828af-f993-4ded-a4f9-814da7c3ec7e', democratic_self_governance_framework).
narrative_ontology:cs_drift_state('a1f828af-f993-4ded-a4f9-814da7c3ec7e', contemporary_judicial_activism_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a1f828af-f993-4ded-a4f9-814da7c3ec7e', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_political_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, minority_groups_dependent_on_judicial_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These movements actively contest and shape constitutional meaning through protest, advocacy, and electoral pressure. They benefit from a framework that legitimizes their interpretive role, seeing their victories as the true expression of constitutional will.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_political_movements, beneficiary,
    organized, generational, mobile, national).

% Legislative majorities, particularly those aligned with popular movements, gain authority to interpret the Constitution through statute and policy, rather than being solely bound by judicial precedent. They benefit from increased interpretive latitude.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, biographical, constrained, national).

% These actors (often within the judiciary or legal academy) advocate for the Supreme Court as the final arbiter of constitutional meaning. They 'pay' by seeing their preferred interpretive authority challenged and diluted by popular contestation, leading to less stable legal outcomes.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates, payer,
    institutional, generational, constrained, national).

% Minority groups, historically reliant on counter-majoritarian judicial review for the protection of their rights, are vulnerable under a popular constitutionalism framework. They risk having their rights subjected to the shifting tides of political opinion and majoritarian will, potentially losing protections.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, minority_groups_dependent_on_judicial_protection, payer,
    powerless, generational, trapped, national).

% While still exercising judicial review, the Supreme Court's interpretive authority is contested and subject to popular and political pressure. Its role shifts from sole arbiter to one voice among many, albeit a powerful one, in the ongoing constitutional dialogue.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, supreme_court, agenda_setter,
    institutional, civilizational, constrained, national).

% Academics and legal theorists analyze the dynamics of constitutional interpretation, documenting the interplay between popular movements, political branches, and the judiciary. They observe the constraint's operation without directly benefiting or paying.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legal_scholars, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__popular_constitutionalism_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate how constitutional meaning is collectively determined and adapted through ongoing democratic processes and political contestation, ensuring responsiveness to the popular will.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over the Constitution from an elite, unelected judiciary to a broader array of political actors, including popular movements and legislative majorities.
% ABSENT_VOICES: Those who believe in a purely apolitical, fixed constitutional meaning, or those who lack the organizational capacity and resources to effectively participate in large-scale popular movements. Their absence means their preferred stable, judicially-enforced meaning is less likely to prevail.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the US constitutional system would revert to a more judicially-centric model, fundamentally altering political dynamics, the locus of power, and the avenues for constitutional change. Popular movements would lose a key avenue for influencing fundamental law.
% FOUNDING_PROBLEM: To prevent an unelected judiciary from becoming the sole, final arbiter of fundamental law, ensuring that constitutional meaning remains responsive to the will of the people and democratic self-governance.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists, historians, and legal scholars (e.g., those studying democratic theory, social movements, or judicial power) outside the immediate benefiting parties corroborate the ongoing tension between judicial supremacy and popular sovereignty in constitutional interpretation. Legislative hearing testimony and public opinion research also support the continued relevance of this tension.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.60) because while it empowers popular majorities, it can extract from those who lose political contests, particularly minority groups. Suppression is moderate (0.40) as the constraint itself is about enabling contestation, but counter-movements and institutional resistance can still suppress popular efforts. Theater ratio is low (0.10) as this reading emphasizes active political struggle and genuine interpretive engagement, not inert performance. Resistance is high (0.75) because the very nature of this reading is ongoing contestation against other interpretive frameworks.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of popular constitutionalism experience this as a legitimate, democratic process for constitutional evolution, seeing it as a 'rope' that coordinates popular will. Opponents, particularly those concerned with judicial finality or minority rights, experience it as a 'snare' that destabilizes settled law and exposes vulnerable groups to majoritarian pressures. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Popular political movements and legislative majorities are beneficiaries, as this reading legitimizes their role in shaping constitutional meaning. Judicial finality advocates and minority groups dependent on judicial protection are victims, as their preferred avenues for constitutional stability or rights protection are undermined or made vulnerable. The Supreme Court acts as an agenda-setter, but its authority is contested within this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling popular constitutionalism as either pure coordination (Rope) or pure extraction (Snare). It acknowledges the genuine coordination function of democratic contestation in shaping meaning, while also recognizing the asymmetric extraction from those who lose the interpretive battles, particularly minority groups. The ongoing contestation means its mandate is live, though its efficacy for all parties is debated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_supremacy_boundary,
    'Is judicial review a necessary counter-majoritarian check to protect fundamental rights, or an anti-democratic usurpation of popular sovereignty?',
    'Empirical studies on the long-term impact of judicial vs. popular constitutionalism on rights protection, and philosophical analysis of democratic legitimacy.',
    'If judicial review is deemed necessary, the extractiveness from minority groups under popular constitutionalism is higher; if deemed usurpation, the extractiveness from popular movements under judicial supremacy is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_supremacy_boundary, conceptual, 'Ambiguity regarding the legitimate locus of final constitutional interpretive authority.').

omega_variable(
    minority_protection_efficacy,
    'Does popular constitutionalism adequately protect minority rights, or does it expose them to majoritarian tyranny?',
    'Comparative historical analysis of rights outcomes under periods of strong popular constitutionalism versus strong judicial supremacy, and case studies of specific minority groups.',
    'If minority rights are consistently undermined, the ''victim'' status of minority groups is amplified, pushing the constraint closer to a Snare for those seats. If protections emerge through popular movements, the victim status is mitigated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_efficacy, empirical, 'The actual impact of popular constitutionalism on vulnerable minority groups.').

omega_variable(
    interpretive_stability_tradeoff,
    'Is the dynamism and responsiveness of popular constitutionalism worth the potential loss of stable, settled constitutional meaning?',
    'Analysis of the costs of legal uncertainty and the benefits of constitutional adaptability, assessed through legal, economic, and political lenses.',
    'If stability is highly valued, the ''payer'' status of judicial finality advocates is amplified. If adaptability is paramount, the benefits to popular movements are amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_stability_tradeoff, preference, 'The normative tradeoff between constitutional dynamism and stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1960, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(us_c_tr_t1975, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(us_c_tr_t2005, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1960, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1960, 0.5).
narrative_ontology:measurement(us_c_be_t1975, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(us_c_be_t2005, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1960, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(us_c_su_t1975, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1975, 0.38).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(us_c_su_t2005, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__living_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'us_constitution_interpretive' kernel. Each reading presents a different structural account of constitutional authority and meaning, with differing ε values and stakeholder impacts. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
