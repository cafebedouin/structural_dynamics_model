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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: US Constitution: Popular Constitutionalism Reading
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'popular constitutionalism'
 *   reading of the US Constitution's interpretive authority. It posits that
 *   constitutional meaning is primarily shaped by popular political movements
 *   and democratic contestation, rather than being solely determined by
 *   judicial interpretation. This reading challenges judicial supremacy,
 *   viewing constitutional development as an ongoing political struggle. The
 *   claimed type is 'tangled_rope' because it offers a genuine coordination
 *   function (integrating popular will) but also involves asymmetric
 *   extraction (from those who rely on stable judicial finality).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.65).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.4).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "US Constitution: Popular Constitutionalism Reading").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, '62bab42d-6389-4d20-a7d2-4ec950d4c5a1').
narrative_ontology:cs_kernel_codification('62bab42d-6389-4d20-a7d2-4ec950d4c5a1', fixed_text).
narrative_ontology:cs_authority_grounding('62bab42d-6389-4d20-a7d2-4ec950d4c5a1', distributed).
narrative_ontology:cs_reading_relation('62bab42d-6389-4d20-a7d2-4ec950d4c5a1', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('62bab42d-6389-4d20-a7d2-4ec950d4c5a1', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('62bab42d-6389-4d20-a7d2-4ec950d4c5a1', foundational, popular_sovereignty_as_interpretive_source).
narrative_ontology:cs_axiom_status(popular_sovereignty_as_interpretive_source, holdable).
narrative_ontology:cs_axiom_grounding('62bab42d-6389-4d20-a7d2-4ec950d4c5a1', popular_sovereignty_as_interpretive_source, deontological).
narrative_ontology:cs_axiom('62bab42d-6389-4d20-a7d2-4ec950d4c5a1', foundational, judicial_supremacy_is_anti_democratic).
narrative_ontology:cs_axiom_status(judicial_supremacy_is_anti_democratic, holdable).
narrative_ontology:cs_axiom_grounding('62bab42d-6389-4d20-a7d2-4ec950d4c5a1', judicial_supremacy_is_anti_democratic, deontological).
narrative_ontology:cs_reference_frame('62bab42d-6389-4d20-a7d2-4ec950d4c5a1', democratic_self_governance_framework).
narrative_ontology:cs_drift_state('62bab42d-6389-4d20-a7d2-4ec950d4c5a1', contemporary_judicial_activism_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('62bab42d-6389-4d20-a7d2-4ec950d4c5a1', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_political_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, minority_groups_dependent_on_judicial_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These movements gain interpretive authority and influence over constitutional meaning through direct political action, protests, and electoral pressure, rather than relying solely on judicial processes. Their success is tied to mobilizing public opinion and legislative action.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_political_movements, beneficiary,
    organized, biographical, mobile, national).

% Legislative majorities benefit from a constitutional interpretation that grants them significant power to define and implement constitutional meaning through statute, rather than being strictly constrained by judicial review. This allows them to enact policies reflecting current popular will.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, generational, constrained, national).

% These claimants challenge the notion of an elite, unelected judiciary as the sole or primary arbiter of constitutional meaning, advocating for a more democratic and accessible process. They benefit from a framework that validates popular input.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants, beneficiary,
    moderate, immediate, mobile, local).

% These advocates, often within the judiciary or legal academy, bear the cost of diminished judicial authority and the instability of constitutional meaning when it is subject to constant political contestation. They seek clear, stable, and judicially enforced constitutional settlements.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates, payer,
    institutional, generational, constrained, national).

% These groups rely on counter-majoritarian judicial review to protect their rights and interests from majoritarian political pressures. Under popular constitutionalism, they face increased vulnerability as their protections become subject to political struggle rather than judicial finality.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, minority_groups_dependent_on_judicial_protection, payer,
    powerless, generational, trapped, national).

% While popular constitutionalism challenges judicial supremacy, the Supreme Court still acts as an agenda-setter by engaging with popular movements, legitimizing or resisting their claims, and ultimately shaping the terms of constitutional debate, even if not always dictating the final meaning.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for integrating popular will and political contestation into the ongoing process of constitutional interpretation, aiming to ensure that the Constitution remains responsive to the democratic aspirations of the people.
% TRANSFER_FUNCTION: Transfers interpretive authority and the power to shape constitutional meaning from an elite judiciary to popular political movements and legislative majorities, shifting the locus of constitutional development.
% ABSENT_VOICES: Those who believe in a purely apolitical, objective constitutional meaning, detached from popular sentiment, are often marginalized in this framework. They would argue for a more stable, judicially-driven interpretation.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the constitutional landscape would revert to a more judicially-centric model, with less direct popular input and potentially more stable, but less democratically responsive, interpretations. Political movements would lose a key avenue for constitutional influence.
% FOUNDING_PROBLEM: The problem of democratic legitimacy for an unelected judiciary holding final interpretive authority over a foundational document, potentially leading to an 'imperial judiciary' detached from popular will.
% FOUNDING_PROBLEM_CORROBORATION: Scholars of political science and legal history, as well as various political commentators and activists, corroborate the ongoing tension between judicial power and democratic self-governance, supporting the view that this problem remains central to American constitutionalism.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) reflects the cost borne by those who prefer stable, judicially-determined constitutional meaning, particularly minority groups whose rights may be vulnerable to majoritarian shifts. Suppression (0.40) is moderate, as this reading actively suppresses the finality of judicial pronouncements in favor of ongoing political debate. Theater ratio (0.20) is low, as the political contestation is a genuine, active process, not merely performative. Resistance (0.75) is high, reflecting the ongoing struggle against judicial supremacy and the active efforts of popular movements to assert their interpretive authority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of popular movements, this is a legitimate and necessary democratic corrective, a 'rope' that ensures responsiveness. From the perspective of judicial finality advocates or vulnerable minorities, it can appear as a 'snare' that destabilizes rights and undermines counter-majoritarian protections. The engine's classification as 'tangled_rope' captures this hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Popular political movements, legislative majorities, and anti-elitist claimants are beneficiaries, as this reading empowers them to shape constitutional meaning. Judicial finality advocates and minority groups dependent on judicial protection are victims, as their preferred mode of constitutional stability and protection is undermined. The Supreme Court, while challenged, still plays an agenda-setting role by engaging with and responding to these popular pressures.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by acknowledging the genuine coordination function of integrating popular will into constitutional development, while simultaneously recognizing the extractive costs imposed on those who lose interpretive authority or stability. It avoids reducing the complex interplay of democratic forces and legal structures to a simple 'snare' by highlighting the active, contested nature of constitutional meaning-making.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stability_vs_responsiveness_tradeoff,
    'What is the optimal balance between constitutional stability (judicial finality) and democratic responsiveness (popular constitutionalism)?',
    'Empirical studies on the long-term effects of different interpretive regimes on social cohesion, minority rights, and governmental effectiveness, combined with normative philosophical debate.',
    'A resolution favoring stability might lead to reclassification towards a ''rope'' for judicial finality advocates; favoring responsiveness might reinforce the ''tangled_rope'' classification for popular movements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_vs_responsiveness_tradeoff, preference, 'The irreducible normative choice between constitutional stability and democratic responsiveness.').

omega_variable(
    minority_protection_efficacy,
    'How effectively can minority rights be protected under a popular constitutionalism framework, compared to a judicially supreme one?',
    'Comparative legal and political analysis of jurisdictions with varying degrees of judicial review and popular constitutional engagement, focusing on outcomes for vulnerable groups.',
    'If minority protection is demonstrably weaker, the ''extractiveness'' for minority groups would be higher, potentially pushing the classification closer to a ''snare'' from their seat. If alternative political protections emerge, extractiveness might be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_efficacy, empirical, 'Empirical question about the efficacy of minority rights protection under popular constitutionalism.').

omega_variable(
    interpretive_authority_locus,
    'Is the ultimate interpretive authority of the Constitution located in the judiciary, the legislature, or the people?',
    'Conceptual analysis of constitutional theory, historical practice, and the formal and informal mechanisms of constitutional change and enforcement. This is a foundational question of political philosophy.',
    'A resolution affirming judicial supremacy would challenge the core premise of this reading, potentially reclassifying it as a ''snare'' from the perspective of popular movements. A resolution affirming popular sovereignty would strengthen its ''rope'' aspects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_authority_locus, conceptual, 'Fundamental conceptual disagreement over the ultimate locus of constitutional interpretive authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(us_c_be_t10, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(us_c_be_t20, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(us_c_be_t30, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(us_c_be_t40, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(us_c_be_t50, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(us_c_su_t10, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(us_c_su_t20, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(us_c_su_t30, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(us_c_su_t40, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(us_c_su_t50, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__living_constitution_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the 'us_constitution_interpretive' kernel. Its structural delta from other readings (originalist, living constitutionalism) is documented in omegas and cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
