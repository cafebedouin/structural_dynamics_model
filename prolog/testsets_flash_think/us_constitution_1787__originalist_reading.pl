% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__originalist_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: US Constitutional Originalist Interpretation
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the originalist reading of the US
 *   Constitution, asserting that its meaning was fixed at the time of
 *   ratification (1787) and is binding based on the framers' intent or
 *   original public meaning. This reading aims to provide stable, objective
 *   constitutional interpretation, but it faces significant contestation from
 *   alternative interpretive theories. The structural delta for this reading
 *   includes a narrow constraint set, legitimization of pre-1787 practices,
 *   exclusion of modern social rights claims from the constitutional
 *   boundary, and high epistemic demands on historical evidence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.65).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.8).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "US Constitutional Originalist Interpretation").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, '777fad2e-bef5-4566-bbf4-5821562c2f28').
narrative_ontology:cs_kernel_codification('777fad2e-bef5-4566-bbf4-5821562c2f28', fixed_text).
narrative_ontology:cs_authority_grounding('777fad2e-bef5-4566-bbf4-5821562c2f28', lineage).
narrative_ontology:cs_interpretation_layer_present('777fad2e-bef5-4566-bbf4-5821562c2f28').
narrative_ontology:cs_reading_relation('777fad2e-bef5-4566-bbf4-5821562c2f28', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_reading_relation('777fad2e-bef5-4566-bbf4-5821562c2f28', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('777fad2e-bef5-4566-bbf4-5821562c2f28', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('777fad2e-bef5-4566-bbf4-5821562c2f28', constitutional_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('777fad2e-bef5-4566-bbf4-5821562c2f28', foundational, framers_intent_binding).
narrative_ontology:cs_axiom_status(framers_intent_binding, holdable).
narrative_ontology:cs_axiom_grounding('777fad2e-bef5-4566-bbf4-5821562c2f28', framers_intent_binding, conventional).
narrative_ontology:cs_reference_frame('777fad2e-bef5-4566-bbf4-5821562c2f28', framers_original_public_meaning).
narrative_ontology:cs_drift_state('777fad2e-bef5-4566-bbf4-5821562c2f28', contemporary_legal_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('777fad2e-bef5-4566-bbf4-5821562c2f28', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_judges).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, conservative_legal_scholars).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, political_actors_aligned_with_1787_intent).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, advocates_for_evolving_rights).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, living_constitutionalist_judges).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, citizens_seeking_modern_rights).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, judicial_restraint_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, constitutional_stability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges who adhere to originalist principles, interpreting the Constitution based on the framers' intent or original public meaning. They benefit from a clear, albeit historically demanding, interpretive methodology that limits judicial discretion.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, identity_locked, national).

% Academics and legal theorists who develop and promote originalist theories. They benefit from the intellectual coherence and political traction of originalism, which provides a framework for their scholarship and advocacy.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, conservative_legal_scholars, beneficiary,
    organized, biographical, constrained, national).

% Organizations and individuals who seek to expand constitutional rights or apply the Constitution to modern social issues not contemplated in 1787. Their claims are often constrained or rejected by originalist interpretations, forcing them to pursue legislative or amendment routes.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, advocates_for_evolving_rights, payer,
    organized, generational, constrained, national).

% Judges who believe the Constitution's meaning evolves with society. Their interpretive methodology is directly challenged and often delegitimized by originalist arguments, leading to judicial conflict and public debate over the proper role of the judiciary.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, living_constitutionalist_judges, payer,
    institutional, generational, identity_locked, national).

% Individuals whose personal liberties or social protections are not explicitly recognized or protected by a strict 1787 interpretation. They bear the direct costs of originalist rulings that deny or limit their claims, with limited recourse outside of political action.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, citizens_seeking_modern_rights, payer,
    powerless, immediate, trapped, local).

% Political parties or movements whose policy goals align with a conservative, historically-rooted interpretation of the Constitution. Originalism provides a legal justification for their agenda and a check on opposing political forces.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, political_actors_aligned_with_1787_intent, beneficiary,
    powerful, biographical, mobile, national).

% The historical authors of the Constitution, whose collective intent or public meaning is the subject of originalist inquiry. They are not active agents but their historical positions are central to the constraint's operation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, framers_of_1787_constitution, excluded,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(us_constitution_1787__originalist_reading, framers_of_1787_constitution).

% The written document itself, which serves as the kernel for all constitutional interpretation. It is a non-agent entity, but its stability and authority are central to the constraint.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, constitutional_text, observer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(us_constitution_1787__originalist_reading, constitutional_text).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, objective method for interpreting the US Constitution, aiming to limit judicial discretion and ensure fidelity to the original compact, thereby coordinating judicial behavior around a fixed meaning.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary judges and societal values to the historical framers' intent or original public meaning; it also transfers the primary burden of constitutional change to the formal amendment process.
% ABSENT_VOICES: The framers themselves (who cannot speak to modern issues), future generations (whose evolving values are constrained by past intent), and those historically excluded from the original compact (e.g., women, enslaved people, indigenous populations) who would advocate for a more inclusive or evolving interpretation.
% DISAPPEARANCE_RATIONALE: If originalism as a dominant interpretive method vanished overnight, judicial interpretation would become more fluid and less constrained by historical intent. This would likely lead to different legal outcomes, a shift in the balance of power between branches, and a reorganization of legal scholarship and advocacy around alternative interpretive theories.
% FOUNDING_PROBLEM: To prevent judicial overreach and ensure the Constitution's meaning remains stable and democratically accountable, rather than being subject to the subjective preferences or evolving values of individual judges.
% FOUNDING_PROBLEM_CORROBORATION: Originalist legal scholars and conservative political movements attest that the problem of judicial activism is still live and that originalism is the necessary solution. Critics, including living constitutionalists and legal realists, argue that the founding problem is either substantially solved or that originalism itself creates new problems of democratic deficit and anachronism by binding modern society to outdated norms. This contestation is evident in legislative hearings, academic debates, and judicial opinions from outside the benefiting parties.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__originalist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because this reading imposes significant costs on those seeking to adapt the Constitution to modern contexts or expand rights not explicitly recognized in 1787. Suppression is very high (0.8) as originalism actively seeks to delegitimize and suppress alternative interpretive methods, particularly living constitutionalism, by framing them as illegitimate judicial activism. Theater ratio is moderate (0.4) reflecting the extensive, often performative, historical research and argumentation used to justify contemporary legal outcomes under the guise of historical fidelity. Accessibility collapse is high (0.75) for alternative interpretive paths, as originalism aims to close off judicial discretion. Resistance is also high (0.7) due to ongoing, vigorous opposition from other legal theories and political movements.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist perspective, this constraint is a neutral, objective 'rope' that coordinates judicial behavior and preserves democratic legitimacy. From the perspective of those whose rights or interpretive flexibility are constrained, it operates as a 'snare' or 'tangled rope,' extracting from modern society by binding it to historical norms that may no longer be just or relevant. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges and conservative legal scholars are primary beneficiaries, as this framework provides them with a principled basis for judicial decision-making and intellectual work. Political actors whose agendas align with 1787 interpretations also benefit from the legal justification originalism provides. Conversely, advocates for evolving rights, living constitutionalist judges, and citizens seeking modern rights are targets, as their claims and interpretive methods are constrained or rejected. The framers and the constitutional text are analytical observers, their historical roles being the subject of interpretation rather than active participation.
 *
 * MANDATROPHY ANALYSIS:
 *   Originalism aims to prevent mandatrophy by ensuring the Constitution's original mandate remains live and uncorrupted by evolving interpretations. However, critics argue that by rigidly adhering to an 18th-century mandate, originalism itself risks creating a form of mandatrophy where the Constitution's function for a modern society atrophies, even if its original purpose is theatrically maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_ascertainment_ambiguity,
    'Is it truly possible to ascertain a singular, coherent ''original intent'' or ''original public meaning'' of a collective body like the framers from centuries ago, given historical context and linguistic drift?',
    'Further historical and linguistic scholarship, or a shift in legal philosophy regarding the epistemic limits of historical inquiry. If a singular intent is demonstrably unrecoverable, the basis for originalism weakens.',
    'If original intent is found to be irreducibly ambiguous, the constraint''s legitimacy as an objective interpretive method would diminish, potentially reclassifying it closer to a ''snare'' or ''piton'' if its persistence relies on a fictionalized historical claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_ascertainment_ambiguity, empirical, 'The epistemic challenge of recovering historical intent.').

omega_variable(
    democratic_legitimacy_paradox,
    'Does binding modern society to 18th-century intent, without significant contemporary input, enhance or diminish democratic legitimacy?',
    'Empirical studies on public perception of judicial legitimacy under originalist vs. non-originalist courts, or a conceptual re-evaluation of the sources of democratic authority in a constitutional republic.',
    'If originalism is perceived to diminish democratic legitimacy by disenfranchising contemporary values, its ''rope'' claim would be undermined, pushing it towards a more extractive classification from the perspective of the citizenry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_paradox, conceptual, 'The tension between historical fidelity and contemporary democratic self-governance.').

omega_variable(
    framing_under_determination_originalism,
    'Is originalism the only defensible framing for constitutional interpretation, or do other coherent framings (living constitutionalism, positivism) produce different classifications that are equally valid?',
    'A meta-analysis of legal philosophical arguments and their practical consequences across different jurisdictions. The choice of framing is often a conceptual and preference-based decision.',
    'If alternative framings are recognized as equally coherent and valid, the ''rope'' classification of originalism might be seen as merely one perspective, and its suppressive aspects (against other framings) would be highlighted, potentially shifting its classification towards a ''tangled_rope'' or ''snare'' from an ''excluded'' or ''payer'' seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination_originalism, conceptual, 'Alternative coherent framings of constitutional interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_1787__originalist_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_1787__originalist_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_1787__originalist_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_1787__originalist_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_1787__originalist_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(us_c_tr_t2020, us_constitution_1787__originalist_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_1787__originalist_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(us_c_be_t1980, us_constitution_1787__originalist_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_1787__originalist_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_1787__originalist_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_1787__originalist_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(us_c_be_t2020, us_constitution_1787__originalist_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_1787__originalist_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(us_c_su_t1980, us_constitution_1787__originalist_reading, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_1787__originalist_reading, suppression_requirement, 1990, 0.74).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_1787__originalist_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_1787__originalist_reading, suppression_requirement, 2010, 0.79).
narrative_ontology:measurement(us_c_su_t2020, us_constitution_1787__originalist_reading, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, identity_coordination).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the US Constitution (us_constitution_1787 kernel). Each reading represents a different structural claim about constitutional meaning and has its own epsilon value and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
