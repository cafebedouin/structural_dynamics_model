% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions (1949): Absolute Humanitarian Minimums
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'humanitarian ceiling' reading of the 1949
 *   Geneva Conventions, which asserts that the Conventions establish
 *   absolute, non-reciprocal humanitarian minimums that constrain state
 *   violence regardless of adversary compliance or security rationales. This
 *   reading emphasizes expansive protections for civilians, detainees, and
 *   even irregular combatants, placing an asymmetric burden on state
 *   militaries. The claimed type is 'rope' reflecting its ideal function as a
 *   coordination mechanism for humane conduct, but the metrics reflect the
 *   high extraction and suppression required to maintain these absolute
 *   limits against state interests.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.8).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.9).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions (1949): Absolute Humanitarian Minimums").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, '3116e042-ec0d-42bc-8315-055a990741d6').
narrative_ontology:cs_kernel_codification('3116e042-ec0d-42bc-8315-055a990741d6', fixed_text).
narrative_ontology:cs_authority_grounding('3116e042-ec0d-42bc-8315-055a990741d6', lineage).
narrative_ontology:cs_interpretation_layer_present('3116e042-ec0d-42bc-8315-055a990741d6').
narrative_ontology:cs_reading_relation('3116e042-ec0d-42bc-8315-055a990741d6', geneva_conventions_1949__conditional_reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('3116e042-ec0d-42bc-8315-055a990741d6', geneva_conventions_1949__security_maximization_reading, forecloses).
narrative_ontology:cs_axiom('3116e042-ec0d-42bc-8315-055a990741d6', foundational, humanitarian_imperative_absolute).
narrative_ontology:cs_axiom_status(humanitarian_imperative_absolute, holdable).
narrative_ontology:cs_axiom_grounding('3116e042-ec0d-42bc-8315-055a990741d6', humanitarian_imperative_absolute, deontological).
narrative_ontology:cs_axiom('3116e042-ec0d-42bc-8315-055a990741d6', foundational, non_reciprocity_principle).
narrative_ontology:cs_axiom_status(non_reciprocity_principle, holdable).
narrative_ontology:cs_axiom_grounding('3116e042-ec0d-42bc-8315-055a990741d6', non_reciprocity_principle, conventional).
narrative_ontology:cs_reference_frame('3116e042-ec0d-42bc-8315-055a990741d6', post_wwii_humanitarian_consensus).
narrative_ontology:cs_drift_state('3116e042-ec0d-42bc-8315-055a990741d6', contemporary_asymmetric_conflict_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3116e042-ec0d-42bc-8315-055a990741d6', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, civilians).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, detainees).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, political_leaders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the primary burden of adhering to strict rules of engagement, targeting, and treatment of combatants and civilians, even when facing non-state actors or adversaries who do not reciprocate. This often conflicts with perceived tactical advantages or security imperatives.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries, payer,
    institutional, biographical, constrained, global).

% Are constrained in their strategic and tactical decisions during armed conflict by the absolute prohibitions of the Conventions, limiting options for achieving military objectives through means deemed inhumane or disproportionate.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, political_leaders, payer,
    institutional, biographical, constrained, national).

% Receive extensive protections from direct attack, indiscriminate targeting, and collective punishment, regardless of the nature of the conflict or the behavior of combatants. Their safety is prioritized as an absolute minimum.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, civilians, beneficiary,
    powerless, immediate, trapped, local).

% Are guaranteed humane treatment, access to medical care, and due process, with prohibitions against torture, cruel treatment, and degrading conditions, irrespective of their status as prisoners of war or other categories.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, detainees, beneficiary,
    powerless, immediate, trapped, local).

% Even without full Prisoner of War status, they retain basic humanitarian protections against torture, summary execution, and inhumane treatment, ensuring a floor of dignity in conflict.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants, beneficiary,
    powerless, immediate, trapped, local).

% Investigates and prosecutes individuals for war crimes, crimes against humanity, and genocide, acting as a key enforcement mechanism for the Conventions' absolute prohibitions. Its legitimacy rests on upholding these minimums.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, international_criminal_court, agenda_setter,
    institutional, generational, analytical, global).

% Acts as the guardian and interpreter of International Humanitarian Law, monitoring compliance, visiting detainees, and providing humanitarian aid. It advocates for the absolute and non-reciprocal application of the Conventions.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, international_committee_of_the_red_cross, agenda_setter,
    institutional, generational, analytical, global).

% Monitor and report on violations of the Conventions, advocating for their strict and absolute adherence by all parties to a conflict. They exert moral and political pressure on states and international bodies.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, human_rights_advocates, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes universal, non-negotiable standards for humane conduct in armed conflict, aiming to prevent a 'race to the bottom' in violence and ensure a minimum level of human dignity is preserved even amidst hostilities.
% TRANSFER_FUNCTION: Transfers the right to inflict unlimited violence from states to a constrained framework of absolute prohibitions; transfers protection and rights to vulnerable populations (civilians, detainees, wounded) from the discretion of combatants to international law.
% ABSENT_VOICES: Victims of atrocities who cannot speak for themselves, future generations who would inherit a world without these foundational norms, and those who would suffer if the 'humanitarian ceiling' were to collapse.
% DISAPPEARANCE_RATIONALE: If the Geneva Conventions' absolute humanitarian minimums vanished overnight, state violence would escalate dramatically, civilian protections would erode, and the very concept of 'humane' warfare would collapse. This would lead to widespread suffering, instability, and a fundamental shift in the ethics of conflict, reorganizing international relations around unchecked power.
% FOUNDING_PROBLEM: The unfettered brutality and widespread atrocities against civilians and prisoners of war witnessed during World War II, which demonstrated the catastrophic consequences of a lack of universal, binding humanitarian limits on warfare.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations (e.g., Amnesty International, Human Rights Watch), historical records, and UN reports consistently corroborate the ongoing relevance of the founding problem, as violations persist in contemporary conflicts, underscoring the continued need for these minimums.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.87 at end) because this reading imposes significant costs on states by limiting their military options and requiring adherence even when tactically disadvantageous. Suppression is very high (0.93) as it actively seeks to suppress any deviation from these absolute minimums. Theater ratio is low (0.14) because this reading demands genuine, not merely performative, compliance. Resistance is high (0.75) due to the constant tension between military necessity and humanitarian imperatives, leading to frequent challenges and violations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state militaries and political leaders, this constraint can feel highly extractive and suppressive, limiting their ability to achieve security objectives. From the perspective of humanitarian organizations and vulnerable populations, it is a vital, life-saving coordination mechanism. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries and political leaders are structural targets (payers) as they bear the costs of adherence and are constrained in their actions. Civilians, detainees, and irregular combatants are clear beneficiaries, receiving protections regardless of their status or the conflict's nature. International bodies like the ICC and ICRC act as agenda-setters and enforcers, upholding these minimums.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_compliance_genuineness,
    'To what extent do states genuinely internalize and adhere to these absolute minimums, versus merely performing compliance for international legitimacy?',
    'Empirical analysis of state practice in conflict zones, independent monitoring reports, and judicial findings of war crimes. If violations are systematic and unpunished, it suggests performative compliance.',
    'If compliance is largely performative, the effective extractiveness from states is lower than measured, and the theater_ratio is higher, potentially reclassifying towards a Piton or Snare for the victims of non-compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_compliance_genuineness, empirical, 'Assessing the sincerity of state adherence to IHL.').

omega_variable(
    enforceability_against_non_state_actors,
    'Is the ''humanitarian ceiling'' reading equally enforceable against non-state armed groups, or does its asymmetric burden on states create a legitimacy deficit?',
    'Analysis of international legal frameworks for non-state actors, and empirical studies of compliance by such groups. If enforcement is systematically weaker, it highlights a structural asymmetry.',
    'If enforcement is significantly weaker against non-state actors, the perceived extractiveness from state militaries increases, and the constraint''s overall stability may be compromised, potentially shifting its classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforceability_against_non_state_actors, conceptual, 'The challenge of applying absolute IHL minimums to non-state armed groups.').

omega_variable(
    absolute_vs_proportionality_tension,
    'How is the absolute nature of humanitarian minimums reconciled with the principle of proportionality in military operations, which allows for some civilian harm if proportionate to military advantage?',
    'Detailed legal analysis of case law and military manuals, and expert consensus on the interpretation of ''absolute'' prohibitions versus ''proportionality'' in specific contexts. If proportionality consistently overrides absolute prohibitions, the ''ceiling'' is not truly absolute.',
    'If proportionality is consistently interpreted to permit actions that would otherwise violate ''absolute'' minimums, the effective extractiveness from state militaries is lower, and the constraint''s claimed ''absolute'' nature is weakened, potentially shifting its classification towards a more conditional type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolute_vs_proportionality_tension, conceptual, 'The conceptual tension between absolute prohibitions and proportionality in IHL.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 1949, 2019).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1949, 0.05).
narrative_ontology:measurement(gene_tr_t1960, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1960, 0.07).
narrative_ontology:measurement(gene_tr_t1970, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(gene_tr_t1980, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1980, 0.09).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(gene_tr_t2000, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2010, 0.13).
narrative_ontology:measurement(gene_tr_t2019, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2019, 0.14).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1949, 0.75).
narrative_ontology:measurement(gene_be_t1960, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1960, 0.78).
narrative_ontology:measurement(gene_be_t1970, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1970, 0.8).
narrative_ontology:measurement(gene_be_t1980, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1980, 0.82).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1990, 0.83).
narrative_ontology:measurement(gene_be_t2000, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2000, 0.85).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2010, 0.86).
narrative_ontology:measurement(gene_be_t2019, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2019, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1949, 0.85).
narrative_ontology:measurement(gene_su_t1960, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1960, 0.87).
narrative_ontology:measurement(gene_su_t1970, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1970, 0.88).
narrative_ontology:measurement(gene_su_t1980, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1980, 0.89).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1990, 0.9).
narrative_ontology:measurement(gene_su_t2000, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2000, 0.91).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2010, 0.92).
narrative_ontology:measurement(gene_su_t2019, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2019, 0.93).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__security_maximization_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, international_criminal_law_framework).

% DUAL FORMULATION NOTE:
% This is one of three readings of the Geneva Conventions (1949) kernel. This 'humanitarian ceiling' reading emphasizes absolute, non-reciprocal humanitarian minimums, distinct from readings focused on reciprocity or security maximization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
