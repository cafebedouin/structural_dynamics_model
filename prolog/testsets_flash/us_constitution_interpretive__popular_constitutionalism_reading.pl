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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism Reading of US Constitutional Meaning
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint describes the 'popular constitutionalism' reading of US
 *   constitutional meaning, where the Constitution's interpretation is
 *   primarily shaped by popular political movements and democratic
 *   contestation, rather than being solely the domain of judicial
 *   interpretation. It challenges judicial supremacy and posits that
 *   constitutional meaning emerges from ongoing political struggle. This
 *   reading is one of several competing interpretations of the US
 *   Constitution, forming a 'kernel' of interpretive authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.4).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.3).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "Popular Constitutionalism Reading of US Constitutional Meaning").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, '6009a036-5936-455e-af26-c64aa8b3030e').
narrative_ontology:cs_kernel_codification('6009a036-5936-455e-af26-c64aa8b3030e', fixed_text).
narrative_ontology:cs_authority_grounding('6009a036-5936-455e-af26-c64aa8b3030e', distributed).
narrative_ontology:cs_reading_relation('6009a036-5936-455e-af26-c64aa8b3030e', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6009a036-5936-455e-af26-c64aa8b3030e', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('6009a036-5936-455e-af26-c64aa8b3030e', foundational, popular_sovereignty_is_supreme_interpreter).
narrative_ontology:cs_axiom_status(popular_sovereignty_is_supreme_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('6009a036-5936-455e-af26-c64aa8b3030e', popular_sovereignty_is_supreme_interpreter, deontological).
narrative_ontology:cs_axiom('6009a036-5936-455e-af26-c64aa8b3030e', foundational, judicial_review_is_not_final).
narrative_ontology:cs_axiom_status(judicial_review_is_not_final, holdable).
narrative_ontology:cs_axiom_grounding('6009a036-5936-455e-af26-c64aa8b3030e', judicial_review_is_not_final, conventional).
narrative_ontology:cs_reference_frame('6009a036-5936-455e-af26-c64aa8b3030e', founding_era_popular_sovereignty).
narrative_ontology:cs_drift_state('6009a036-5936-455e-af26-c64aa8b3030e', contemporary_judicial_supremacy_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6009a036-5936-455e-af26-c64aa8b3030e', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_political_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_supremacy_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, minority_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, those_seeking_constitutional_finality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These movements gain interpretive authority and influence over constitutional meaning through direct political action, protests, and electoral pressure, rather than relying solely on judicial pronouncements. They benefit from a more fluid and contestable constitutional landscape.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_political_movements, beneficiary,
    organized, generational, mobile, national).

% Legislative majorities find their policy preferences more easily translated into constitutional practice when judicial review is less final and popular will holds greater sway in interpretation. This allows for more direct democratic control over constitutional development.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, biographical, constrained, national).

% Individuals and groups who distrust elite institutions, including the judiciary, benefit from a framework that decentralizes constitutional authority and empowers ordinary citizens and their elected representatives in shaping fundamental law.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants, beneficiary,
    moderate, biographical, mobile, local).

% Those who believe in the judiciary as the ultimate arbiter of constitutional meaning find their authority challenged and diminished. They bear the cost of a less stable and less judicially-controlled constitutional order, facing constant contestation of judicial rulings.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_supremacy_advocates, payer,
    institutional, generational, constrained, national).

% Minority groups often rely on counter-majoritarian judicial protection to safeguard their rights against popular will. In a popular constitutionalism framework, their protections may become more vulnerable to democratic contestation and shifting political tides, increasing the burden of defending their rights.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, minority_rights_advocates, payer,
    organized, generational, constrained, national).

% Individuals and institutions that require clear, stable, and final constitutional settlements for planning and social order find this framework introduces greater uncertainty and ongoing political struggle over fundamental questions. The cost is a lack of definitive resolution.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, those_seeking_constitutional_finality, payer,
    moderate, biographical, constrained, national).

% While still issuing rulings, their interpretive authority is seen as less final and more subject to popular and legislative override or contestation. They must navigate a political environment where their pronouncements are not the last word, but rather part of an ongoing dialogue.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, supreme_court_justices, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for constitutional meaning to adapt and remain legitimate in the face of evolving societal values and democratic demands, preventing ossification and promoting popular engagement with fundamental law.
% TRANSFER_FUNCTION: Transfers interpretive authority and the power to shape constitutional meaning from an exclusive judicial elite to a broader array of political actors and popular movements, shifting the locus of constitutional development.
% ABSENT_VOICES: Those who believe in a purely apolitical, technocratic, or historically fixed constitutional meaning are marginalized; they would argue for a more constrained interpretive process, but their arguments are often dismissed as anti-democratic or elitist within this framework.
% DISAPPEARANCE_RATIONALE: If popular constitutionalism vanished, the US constitutional system would revert to a more judicially-centric model, with the Supreme Court's interpretations holding greater finality. Political movements would lose a key avenue for constitutional influence, and the dynamic interplay between popular will and constitutional meaning would diminish, fundamentally altering the nature of American governance.
% FOUNDING_PROBLEM: The problem of how a written constitution, drafted in one era, can remain relevant and legitimate across generations without becoming either anachronistic or arbitrarily reinterpreted by a small elite.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists outside the direct beneficiaries attest to the ongoing tension between judicial supremacy and democratic self-governance, confirming that the problem of constitutional adaptation and popular legitimacy remains central to American political thought and practice.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).
:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.4) reflects the cost borne by those who prefer judicial finality and stable constitutional settlements, as their preferred mode of constitutional development is challenged. Suppression (0.3) is present in the active political and academic efforts to delegitimize purely judicial interpretations and to empower popular movements. The theater ratio (0.1) is low, as the contestation is genuine and not merely performative; the stakes are real. Accessibility collapse (0.4) is moderate, as judicial avenues still exist, but their finality is diminished. Resistance (0.7) is high, reflecting the ongoing and vigorous contestation from judicial supremacy advocates and minority rights groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of popular movements, this is a legitimate and necessary coordination mechanism for democratic self-governance. From the perspective of judicial supremacy advocates, it is an extractive snare that undermines the rule of law and endangers minority rights. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Popular political movements, legislative majorities, and anti-elitist claimants are beneficiaries, as this reading empowers their role in constitutional interpretation. Judicial supremacy advocates, minority rights advocates (who often rely on judicial protection), and those seeking constitutional finality are victims, as their preferred modes of constitutional stability and protection are undermined or made more precarious. Supreme Court justices, while still powerful, act as agenda-setters within a more contested interpretive environment.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_finality_vs_popular_will,
    'To what extent does popular constitutionalism undermine the finality and stability of constitutional meaning, and what are the long-term consequences for minority rights and the rule of law?',
    'Longitudinal empirical studies comparing constitutional stability and minority protections in systems with strong vs. weak judicial review, and historical analysis of periods of intense popular constitutional contestation.',
    'If it demonstrably leads to significant instability or erosion of minority rights, the ''extractiveness'' and ''suppression'' metrics for minority rights advocates would be higher, potentially shifting the classification towards a Snare for that seat. If it leads to more legitimate and adaptable constitutional outcomes without undue harm, the ''extractiveness'' would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_finality_vs_popular_will, empirical, 'The trade-off between democratic responsiveness and constitutional stability/minority protection.').

omega_variable(
    framing_of_interpretive_authority,
    'Is the ''popular constitutionalism'' reading a genuine alternative framework for constitutional interpretation, or is it primarily a political strategy to achieve desired outcomes by challenging judicial authority?',
    'Conceptual analysis of the philosophical coherence of popular constitutionalism as a theory of interpretation, independent of its political utility, and examination of its proponents'' consistency across different political contexts.',
    'If it is primarily a political strategy, the ''theater_ratio'' might be higher, as its stated coordination function (democratic legitimacy) could be seen as cover for power acquisition. If it is a coherent interpretive theory, the current metrics are appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_interpretive_authority, conceptual, 'Whether popular constitutionalism is a genuine interpretive theory or a political tactic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1960, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1960, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1960, 0.3).
narrative_ontology:measurement(us_c_be_t1980, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1960, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1960, 0.2).
narrative_ontology:measurement(us_c_su_t1980, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__living_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'us_constitution_interpretive' kernel. Each reading represents a different structural claim about how constitutional meaning is determined, leading to different beneficiaries, victims, and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
