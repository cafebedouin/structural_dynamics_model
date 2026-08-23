% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__formal_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__formal_equality_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: fourteenth_amendment_equal_protection__formal_equality_reading
 *   human_readable: Formal Equality Reading of the Fourteenth Amendment Equal Protection Clause
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The formal equality reading of the Fourteenth Amendment's Equal
 *   Protection Clause holds that the Constitution prohibits virtually all
 *   explicit state racial classifications, subject only to strict scrutiny.
 *   This reading emerged in the late 19th century (Plessy dissent), was
 *   eclipsed by separate-but-equal, reclaimed in Brown v. Board, and hardened
 *   into a near-categorical anti-classification rule from Bakke through
 *   Students for Fair Admissions. The reading claims Mountain status — formal
 *   equality as a neutral, timeless constitutional principle. But it operates
 *   with substantial suppression (blocking race-conscious remedies), declares
 *   identifiable beneficiaries (formal equality advocates) and victims (those
 *   seeking race-conscious remedies), and requires active judicial
 *   enforcement. This is a False Summit Mountain candidate: a constraint
 *   presented as natural law that structurally benefits a recognizable
 *   ideological coalition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.35).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.78).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, mountain).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "Formal Equality Reading of the Fourteenth Amendment Equal Protection Clause").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).
domain_priors:emerges_naturally(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, 'efffb78b-d93d-4286-a97e-e154c94d4438').
narrative_ontology:cs_kernel_codification('efffb78b-d93d-4286-a97e-e154c94d4438', fixed_text).
narrative_ontology:cs_authority_grounding('efffb78b-d93d-4286-a97e-e154c94d4438', lineage).
narrative_ontology:cs_interpretation_layer_present('efffb78b-d93d-4286-a97e-e154c94d4438').
narrative_ontology:cs_reading_relation('efffb78b-d93d-4286-a97e-e154c94d4438', fourteenth_amendment_equal_protection__anti_caste_reading, forecloses).
narrative_ontology:cs_axiom('efffb78b-d93d-4286-a97e-e154c94d4438', foundational, race_neutrality_as_sole_equal_protection_value).
narrative_ontology:cs_axiom_status(race_neutrality_as_sole_equal_protection_value, holdable).
narrative_ontology:cs_axiom_grounding('efffb78b-d93d-4286-a97e-e154c94d4438', race_neutrality_as_sole_equal_protection_value, deontological).
narrative_ontology:cs_axiom('efffb78b-d93d-4286-a97e-e154c94d4438', secondary, strict_scrutiny_for_all_racial_classifications).
narrative_ontology:cs_axiom_status(strict_scrutiny_for_all_racial_classifications, holdable).
narrative_ontology:cs_axiom_grounding('efffb78b-d93d-4286-a97e-e154c94d4438', strict_scrutiny_for_all_racial_classifications, conventional).
narrative_ontology:cs_reference_frame('efffb78b-d93d-4286-a97e-e154c94d4438', formal_equality_framework).
narrative_ontology:cs_drift_state('efffb78b-d93d-4286-a97e-e154c94d4438', contemporary_strict_scrutiny_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('efffb78b-d93d-4286-a97e-e154c94d4438', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, formal_equality_advocates).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_constitutionalists).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, opponents_of_race_conscious_remedies).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, racial_minorities_seeking_remedies).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, affirmative_action_beneficiaries).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, anti_caste_advocates).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, state_corrective_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, state_governments_seeking_to_remedy).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, formal_equality_principle).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_constitutionalism).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, strict_scrutiny_doctrine).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, race_neutrality_as_constitutional_baseline).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A coalition of justices and legal theorists who articulate and enforce the formal equality reading through strict scrutiny doctrine. They set the interpretive agenda, decide which classifications trigger heightened review, and determine whether state justifications are 'compelling.' Their authority derives from institutional position and the precedential force of their opinions.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, supreme_court_formalist_bloc, agenda_setter,
    institutional, generational, analytical, national).

% Legal organizations, scholars, and litigators who advance the formal equality reading in courts and public discourse. They benefit from a constitutional framework that treats race-conscious remedies as presumptively illegitimate, which aligns with their ideological commitments and litigation strategy. They can shift to other constitutional arguments if this reading loses dominance.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, formal_equality_advocates, beneficiary,
    organized, biographical, mobile, national).

% Communities and individuals who would benefit from race-conscious remedial programs (affirmative action, voting rights enforcement, disparate impact liability, school integration). They bear the cost of the constraint's suppression of those tools. Their exit from the constraint's effects is constrained — they cannot opt out of the legal regime, and political alternatives are blocked by the same doctrinal structure.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, racial_minorities_seeking_remedies, payer,
    moderate, biographical, constrained, national).

% Students, workers, and contractors who directly receive opportunities through race-conscious programs. When those programs are struck down or narrowed under strict scrutiny, they lose concrete benefits with no individual remedy. Their exit options are minimal — they cannot individually challenge the constitutional doctrine, and the political process is the very arena the doctrine polices.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, affirmative_action_beneficiaries, payer,
    powerless, immediate, trapped, national).

% Civil rights organizations, critical race theorists, and dissenting justices who read the Equal Protection Clause as requiring dismantling of hierarchy. Their reading is structurally excluded from controlling precedent when the formal equality reading dominates — they can dissent, file amicus briefs, and organize politically, but the constraint's logic treats their core premise (anti-subordination) as constitutionally irrelevant or illegitimate.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, anti_caste_advocates, excluded,
    organized, generational, constrained, national).

% State and local governments that wish to enact race-conscious remedial policies (contracting set-asides, school assignment plans, voting rights compliance). They are constrained by the doctrine they must enforce — the same federal courts that review their laws also define the limits of their remedial power. They retain some agenda-setting capacity through race-neutral alternatives, but the constraint narrows their toolkit.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, state_governments_seeking_to_remedy, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__formal_equality_reading, state_governments_seeking_to_remedy, agenda_setter).

% Constitutional scholars across the ideological spectrum who analyze, critique, and historicize the formal equality reading. They do not directly collect or pay under the constraint, but their work shapes the intellectual environment in which the constraint operates and evolves.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, legal_academy_observers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a neutral baseline for state action by prohibiting explicit racial sorting, coordinating expectations around a 'colorblind' constitutional norm that purports to treat all citizens identically regardless of race.
% TRANSFER_FUNCTION: Transfers remedial authority away from race-conscious state programs toward race-neutral alternatives; constrains the state's corrective capacity by treating structural inequality as pre-constitutional background rather than a constitutional concern; moves the burden of addressing disparity from targeted remedies to generalized policy.
% ABSENT_VOICES: Communities experiencing persistent structural inequality who would argue that formal neutrality perpetuates hierarchy; the anti-caste reading's proponents, whose constitutional vision is treated as illegitimate within the formal equality framework; future generations who inherit the unresolved hierarchy.
% DISAPPEARANCE_RATIONALE: If the formal equality reading vanished overnight, strict scrutiny would cease to apply to all racial classifications; states would regain broad authority to enact race-conscious remedial programs; affirmative action, voting rights preclearance, school integration mandates, and disparate impact liability would expand dramatically; the constitutional law of race would reorganize around anti-subordination rather than anti-classification.
% FOUNDING_PROBLEM: The post-Civil War need to prevent Southern states from reimposing racial caste through explicit racial legislation (Black Codes) while preserving federal power to enforce civil rights.
% FOUNDING_PROBLEM_CORROBORATION: The historical record of Black Codes and the 39th Congress's debates corroborate the founding problem of explicit racial caste legislation. However, the Reconstruction Congress also enacted race-conscious remedies (Freedmen's Bureau, Civil Rights Act of 1866), which anti-caste scholars cite as evidence the founding problem included hierarchy dismantling, not merely formal neutrality. The formal equality reading's proponents emphasize the anti-classification language; anti-caste scholars emphasize the remedial practice. No consensus exists outside the benefited parties.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__formal_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__formal_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__formal_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, ExtMetricName, E),
    domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fourteenth_amendment_equal_protection__formal_equality_reading),
    narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) — the constraint does not directly extract resources but blocks remedial channels, which is a form of opportunity extraction. Suppression is high (0.78) — strict scrutiny operates as a near-categorical bar; few race-conscious programs survive. Theater is low (0.22) — the constraint is genuinely enforced, not performative. Accessibility collapse is high (0.82) — race-conscious alternatives are doctrinally foreclosed. Resistance is high (0.71) — sustained scholarly, political, and judicial dissent from the anti-caste reading. The measurement series tracks the reading's evolution from Reconstruction through the modern strict scrutiny regime on a shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the formalist bloc's seat, the constraint is a Mountain — a neutral principle derived from constitutional text and history. From the racial minorities seat, it is a Snare — a doctrine that uses neutrality language to block the only tools that address structural hierarchy. From the state governments' seat, it is a Tangled Rope — it coordinates a baseline of non-discrimination but extracts remedial flexibility. The engine computes this divergence; the authored claim (mountain) does not resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court formalist bloc sits at the beneficiary end (d ~ 0.1) — they wield the constraint as institutional authority. Formal equality advocates are beneficiaries (d ~ 0.2) — the doctrine validates their constitutional vision. Racial minorities seeking remedies and affirmative action beneficiaries are targets (d ~ 0.85-0.95) — they bear the remedial foreclosure with constrained or trapped exit. Anti-caste advocates are excluded (d ~ 0.7) — their constitutional reading is structurally marginalized. State governments are dual-positioned: as remedial actors they are payers (d ~ 0.6); as enforcers of the constraint they are agenda-setters. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing explicit racial caste legislation) is largely solved — no state openly enacts Black Codes today. But the constraint persists and has expanded to block remedial classifications that the Reconstruction Congress itself enacted. The mandate has atrophied from anti-caste to anti-classification; the constraint now serves the ideological interests of its beneficiaries rather than its original function. This is mandatrophy: the arrangement persists because its beneficiaries control the interpretive apparatus, not because the founding problem remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_equality_naturalness,
    'Is the formal equality reading a genuine neutral principle of constitutional law, or a constructed doctrine that serves the ideological interests of its beneficiaries by blocking race-conscious remedies?',
    'Historical analysis of Reconstruction-era understanding; doctrinal genealogy of strict scrutiny; empirical study of which groups benefit from the doctrine''s operation versus its stated purpose.',
    'If constructed, the Mountain claim fails and False Summit Mountain signature triggers reclassification to tangled_rope (coordination of non-discrimination baseline + asymmetric extraction of remedial capacity). If genuine, Mountain certification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_equality_naturalness, conceptual, 'Whether formal equality is natural law or ideological construction.').

omega_variable(
    strict_scrutiny_neutrality,
    'Is strict scrutiny genuinely neutral as to which racial classifications it burdens, or does it disproportionately burden remedial classifications while permitting ''benign'' classifications that entrench advantage?',
    'Empirical survey of strict scrutiny outcomes: survival rates for remedial vs. non-remedial classifications; analysis of which government interests count as ''compelling'' in practice.',
    'If strict scrutiny is not neutral, the constraint''s coordination function is compromised and its extraction is asymmetric — supporting tangled_rope or snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_scrutiny_neutrality, empirical, 'Whether the enforcement mechanism operates symmetrically.').

omega_variable(
    committer_structure_formal_equality,
    'This constraint is one reading (formal_equality_reading) of the contested kernel fourteenth_amendment_equal_protection. The sibling reading (anti_caste_reading) would place state corrective action in the beneficiary set and treat structural inequality as the constitutional concern. Where is the structural disagreement located?',
    'Compare the two readings'' axiom sets: formal equality holds ''race_neutrality_as_sole_equal_protection_value'' (deontological); anti-caste holds ''anti_subordination_as_equal_protection_mandate'' (deontological). The disagreement is at the foundational axiom level — what the Equal Protection Clause *is for*.',
    'If the kernel permits both readings, the formal equality reading''s Mountain claim is underdetermined — the kernel itself does not dictate which reading is correct. This supports False Summit Mountain detection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_formal_equality, conceptual, 'Committer frame: this reading''s relationship to the contested kernel and its sibling.').

omega_variable(
    state_corrective_capacity_as_victim,
    'Should ''state_corrective_capacity'' be treated as a victim/payer — an institutional actor whose remedial authority is extracted by the constraint — or is the state merely the enforcement vehicle with no independent stake?',
    'Analyze whether state governments actively seek to enact race-conscious remedies and are blocked by the doctrine, versus merely complying passively. Historical record of state voluntary affirmative action programs and voting rights compliance efforts.',
    'If states are active remedial actors constrained by the doctrine, they are genuine payers — expanding the victim set and strengthening the asymmetric extraction case. If passive, the victim set narrows to only directly affected individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_corrective_capacity_as_victim, empirical, 'Whether state governments are victims of the constraint''s foreclosure of remedial tools.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 1868, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feqprot_tr_t1868, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1868, 0.1).
narrative_ontology:measurement(feqprot_tr_t1896, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1896, 0.45).
narrative_ontology:measurement(feqprot_tr_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1954, 0.25).
narrative_ontology:measurement(feqprot_tr_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1978, 0.22).
narrative_ontology:measurement(feqprot_tr_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(feqprot_tr_t2003, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2003, 0.22).
narrative_ontology:measurement(feqprot_tr_t2013, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2013, 0.22).
narrative_ontology:measurement(feqprot_tr_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2023, 0.22).

% Extraction over time
narrative_ontology:measurement(feqprot_be_t1868, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1868, 0.15).
narrative_ontology:measurement(feqprot_be_t1896, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1896, 0.25).
narrative_ontology:measurement(feqprot_be_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1954, 0.3).
narrative_ontology:measurement(feqprot_be_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1978, 0.35).
narrative_ontology:measurement(feqprot_be_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(feqprot_be_t2003, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2003, 0.35).
narrative_ontology:measurement(feqprot_be_t2013, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2013, 0.35).
narrative_ontology:measurement(feqprot_be_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2023, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(feqprot_su_t1868, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1868, 0.3).
narrative_ontology:measurement(feqprot_su_t1896, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1896, 0.85).
narrative_ontology:measurement(feqprot_su_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1954, 0.7).
narrative_ontology:measurement(feqprot_su_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1978, 0.78).
narrative_ontology:measurement(feqprot_su_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1995, 0.78).
narrative_ontology:measurement(feqprot_su_t2003, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2003, 0.78).
narrative_ontology:measurement(feqprot_su_t2013, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2013, 0.78).
narrative_ontology:measurement(feqprot_su_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2023, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fourteenth_amendment_equal_protection__formal_equality_reading, 0.1).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection__anti_caste_reading).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, strict_scrutiny_doctrine).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, affirmative_action_jurisprudence).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, voting_rights_act_preclearance).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, disparate_impact_liability).

% DUAL FORMULATION NOTE:
% This constraint and anti_caste_reading form a constraint family decomposing the 'Equal Protection' label. Formal equality reading: ε≈0.35, Mountain claim, beneficiaries=formal_equality_advocates, victims=remedy_seekers. Anti-caste reading: would have higher ε for race-conscious remedies blocked, different beneficiary/victim structure. Linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__formal_equality_reading, institutional, 0.15).
constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__formal_equality_reading, organized, 0.2).
constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__formal_equality_reading, moderate, 0.8).
constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__formal_equality_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
