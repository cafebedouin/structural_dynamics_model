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
 *   viewing the Constitution as a text whose meaning is forged in political
 *   struggle. The claimed type is 'tangled_rope' because it offers a genuine
 *   coordination function (democratic responsiveness) but also involves
 *   asymmetric extraction (from those who rely on stable judicial finality,
 *   particularly minorities).
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
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, '6fa3dd88-1084-4d9e-a16a-f5bbc79d790e').
narrative_ontology:cs_kernel_codification('6fa3dd88-1084-4d9e-a16a-f5bbc79d790e', fixed_text).
narrative_ontology:cs_authority_grounding('6fa3dd88-1084-4d9e-a16a-f5bbc79d790e', distributed).
narrative_ontology:cs_reading_relation('6fa3dd88-1084-4d9e-a16a-f5bbc79d790e', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6fa3dd88-1084-4d9e-a16a-f5bbc79d790e', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('6fa3dd88-1084-4d9e-a16a-f5bbc79d790e', foundational, popular_sovereignty_as_interpretive_authority).
narrative_ontology:cs_axiom_status(popular_sovereignty_as_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('6fa3dd88-1084-4d9e-a16a-f5bbc79d790e', popular_sovereignty_as_interpretive_authority, deontological).
narrative_ontology:cs_axiom('6fa3dd88-1084-4d9e-a16a-f5bbc79d790e', foundational, judicial_supremacy_is_anti_democratic).
narrative_ontology:cs_axiom_status(judicial_supremacy_is_anti_democratic, holdable).
narrative_ontology:cs_axiom_grounding('6fa3dd88-1084-4d9e-a16a-f5bbc79d790e', judicial_supremacy_is_anti_democratic, deontological).
narrative_ontology:cs_reference_frame('6fa3dd88-1084-4d9e-a16a-f5bbc79d790e', democratic_self_governance_framework).
narrative_ontology:cs_drift_state('6fa3dd88-1084-4d9e-a16a-f5bbc79d790e', contemporary_judicial_activism_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6fa3dd88-1084-4d9e-a16a-f5bbc79d790e', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_political_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, minorities_dependent_on_judicial_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These movements gain interpretive authority and influence over constitutional meaning through direct political action, protests, and electoral pressure, rather than relying solely on judicial pronouncements. They benefit from a more fluid and democratically responsive constitutional interpretation.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_political_movements, beneficiary,
    organized, biographical, mobile, national).

% Legislative majorities benefit from the ability to shape constitutional meaning through statute and policy, with less deference to judicial review. This allows them to enact their policy preferences more directly, reflecting current democratic will.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, biographical, constrained, national).

% These claimants challenge the notion of an elite, unelected judiciary holding final interpretive authority. They benefit from a framework that decentralizes constitutional interpretation and empowers broader public participation.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants, beneficiary,
    moderate, immediate, mobile, local).

% These advocates, often within the judiciary or legal academy, bear the cost of diminished judicial authority and the instability of constitutional meaning. They seek clear, settled constitutional interpretations and a final arbiter, which popular constitutionalism challenges.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates, payer,
    institutional, generational, constrained, national).

% Minority groups, historically reliant on counter-majoritarian judicial review for the protection of their rights, are victims of this reading. Their constitutional protections become more vulnerable to shifting political winds and majoritarian pressures, increasing their precarity.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, minorities_dependent_on_judicial_protection, payer,
    powerless, generational, trapped, national).

% While popular constitutionalism challenges judicial supremacy, the Supreme Court still plays a role in mediating constitutional disputes. Under this reading, its authority is contested and its interpretations are subject to greater political pushback and potential override by other branches or popular movements.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the process by which constitutional meaning is debated and established, allowing for democratic input and adaptation to societal changes through political rather than purely legal channels.
% TRANSFER_FUNCTION: Transfers interpretive authority over the Constitution from an elite judiciary to a broader array of political actors and popular movements, shifting the locus of constitutional power.
% ABSENT_VOICES: Those who believe in a fixed, stable constitutional meaning, insulated from political passions, are often marginalized. Their arguments for judicial finality and counter-majoritarian protection are actively resisted by popular movements.
% DISAPPEARANCE_RATIONALE: If this reading vanished, constitutional interpretation would revert to a more judicially-centric model, with less direct input from popular movements and legislative majorities. The political landscape would shift, and the mechanisms for constitutional change would become more formal and less responsive to democratic contestation.
% FOUNDING_PROBLEM: The problem of ensuring that constitutional meaning remains responsive to the will of the people and prevents an unelected judiciary from becoming the sole arbiter of fundamental law.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists outside the direct beneficiaries attest to the ongoing tension between judicial power and democratic self-governance. Public opinion polls often show distrust in judicial overreach, corroborating the live status of this founding problem.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) reflects the cost borne by those who prefer judicial finality and stable constitutional settlement, as their preferred mode of interpretation is undermined. Suppression (0.40) is moderate, as this reading actively resists judicial claims to finality but does not fully suppress judicial review itself. Theater ratio (0.20) is low, as the democratic contestation is a genuine, active process, not mere performance. Resistance (0.75) is high, reflecting the ongoing struggle against judicial supremacy. Accessibility collapse (0.30) is low, as alternative interpretive modes (originalism, living constitutionalism) remain live options, though contested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of popular movements, this is a legitimate 'rope' that ensures democratic accountability. From the perspective of judicial finality advocates, it is a 'snare' that destabilizes law and undermines minority rights. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Popular movements, legislative majorities, and anti-elitist claimants are beneficiaries, as this reading empowers their interpretive claims. Judicial finality advocates and minorities dependent on judicial protection are victims, as their preferred mode of constitutional stability and protection is challenged. The Supreme Court acts as an agenda-setter, but its authority is contested and its interpretations are subject to greater political pushback.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling democratic contestation as pure extraction by acknowledging the genuine coordination function of popular sovereignty in shaping fundamental law. However, it also highlights the extractive costs for those who lose out in this contest, particularly vulnerable minorities, preventing it from being mislabeled as a pure 'rope'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_responsiveness_vs_minority_rights,
    'At what point does democratic responsiveness in constitutional interpretation become a threat to fundamental minority rights, and how is this boundary adjudicated?',
    'Empirical analysis of outcomes for minority groups under periods of heightened popular constitutionalism versus periods of strong judicial review, combined with normative debate on the limits of majoritarianism.',
    'If popular constitutionalism consistently leads to the erosion of minority rights, its extractive component would be re-evaluated as higher, potentially shifting its classification towards a ''snare'' for those groups. If robust mechanisms for minority protection emerge within a popular framework, the classification might shift towards a more balanced ''tangled_rope'' or even ''rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_responsiveness_vs_minority_rights, empirical, 'The tension between majoritarian constitutional interpretation and counter-majoritarian protection of minority rights.').

omega_variable(
    judicial_supremacy_contestation_level,
    'To what extent is judicial supremacy genuinely contested and undermined by popular movements, versus merely being rhetorically challenged while remaining functionally dominant?',
    'Analysis of legislative overrides of judicial decisions, successful constitutional amendments driven by popular movements, and the actual enforcement of judicial rulings in the face of political opposition.',
    'If judicial supremacy remains functionally dominant despite rhetorical challenges, the ''suppression'' metric for judicial finality advocates would be lower, and the ''extractiveness'' from popular movements would be higher, potentially shifting the overall classification towards a ''snare'' for popular movements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_supremacy_contestation_level, empirical, 'The actual versus rhetorical power of popular constitutionalism against judicial supremacy.').

omega_variable(
    framing_of_constitutional_stability,
    'Is constitutional stability inherently a good, or is it a mechanism that entrenches existing power structures and resists necessary change?',
    'Conceptual analysis and normative debate regarding the purpose of a constitution: as a fixed framework for governance versus a living document for ongoing democratic self-determination.',
    'If stability is viewed as a mechanism of entrenchment, the ''extractiveness'' from popular movements would be lower, as they are challenging an extractive status quo. If stability is viewed as a necessary good, the ''extractiveness'' from judicial finality advocates would be higher, as their preferred state is undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_of_constitutional_stability, conceptual, 'The normative framing of constitutional stability as a value versus a tool of power.').


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
% This is one of three readings of the 'us_constitution_interpretive' kernel, alongside 'originalist_reading' and 'living_constitution_reading'. Each reading represents a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
