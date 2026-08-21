% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__treaty_primacy_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Indigenous Treaty Primacy in Secession Legitimacy
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint asserts that Indigenous treaty rights fundamentally
 *   predate and supersede both federal and provincial authority, making any
 *   secession claim illegitimate without the explicit consent of Indigenous
 *   treaty holders. It functions as a protective boundary for Indigenous
 *   sovereignty within the broader context of federalism and potential
 *   provincial secession. This story instantiates the
 *   'treaty_primacy_reading' of the 'secession_legitimacy_boundary' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.15).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.8).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, mountain).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Indigenous Treaty Primacy in Secession Legitimacy").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).
domain_priors:emerges_naturally(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, '08a76b06-7aff-4b61-8b14-a41325167dbf').
narrative_ontology:cs_kernel_codification('08a76b06-7aff-4b61-8b14-a41325167dbf', formalized).
narrative_ontology:cs_authority_grounding('08a76b06-7aff-4b61-8b14-a41325167dbf', lineage).
narrative_ontology:cs_interpretation_layer_present('08a76b06-7aff-4b61-8b14-a41325167dbf').
narrative_ontology:cs_reading_relation('08a76b06-7aff-4b61-8b14-a41325167dbf', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('08a76b06-7aff-4b61-8b14-a41325167dbf', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('08a76b06-7aff-4b61-8b14-a41325167dbf', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('08a76b06-7aff-4b61-8b14-a41325167dbf', foundational, indigenous_sovereignty_precedes_crown_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_precedes_crown_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('08a76b06-7aff-4b61-8b14-a41325167dbf', indigenous_sovereignty_precedes_crown_sovereignty, deontological).
narrative_ontology:cs_axiom('08a76b06-7aff-4b61-8b14-a41325167dbf', foundational, treaty_as_nation_to_nation_agreement).
narrative_ontology:cs_axiom_status(treaty_as_nation_to_nation_agreement, holdable).
narrative_ontology:cs_axiom_grounding('08a76b06-7aff-4b61-8b14-a41325167dbf', treaty_as_nation_to_nation_agreement, conventional).
narrative_ontology:cs_reference_frame('08a76b06-7aff-4b61-8b14-a41325167dbf', pre_existing_indigenous_sovereignty_framework).
narrative_ontology:cs_drift_state('08a76b06-7aff-4b61-8b14-a41325167dbf', contemporary_post_colonial_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('08a76b06-7aff-4b61-8b14-a41325167dbf', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, secessionist_provincial_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, settler_population).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__treaty_primacy_reading, pre_existing_rights_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the original inhabitants and signatories of treaties, they assert inherent and pre-existing rights that predate colonial claims. Their consent is deemed essential for any legitimate alteration of territorial or political arrangements, including secession. Their identity is deeply tied to their land and treaty relationships.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders, agenda_setter,
    organized, generational, identity_locked, regional).

% These governments seek to assert the right of self-determination for their provincial populations, often based on popular sovereignty. Under this constraint, they bear the cost of needing Indigenous consent, which complicates or invalidates unilateral secession efforts. Their options are to negotiate, litigate, or proceed illegitimately.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, secessionist_provincial_governments, payer,
    institutional, biographical, constrained, national).

% As a treaty partner, the federal government has a fiduciary duty to uphold treaty rights. This constraint limits its ability to unilaterally approve or deny secession without considering Indigenous consent, placing it in a complex legal and political position between provincial and Indigenous claims.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_government, agenda_setter,
    institutional, generational, constrained, national).

% These entities (e.g., UN bodies, human rights organizations) monitor and comment on the legitimacy of secession processes, often referencing international law on Indigenous rights. Their analysis can lend moral and political weight to the treaty primacy argument, influencing global opinion and diplomatic pressure.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, international_observers, observer,
    analytical, biographical, analytical, global).

% The non-Indigenous population within a secessionist province would be directly affected by the legitimacy (or lack thereof) of any separation. They indirectly bear the costs of political instability, legal uncertainty, and potential conflict arising from a secession that lacks Indigenous consent.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, settler_population, payer,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a legitimate framework for political transitions, including secession, that respects and integrates pre-existing Indigenous sovereignty and treaty obligations, preventing unilateral actions that would undermine fundamental rights.
% TRANSFER_FUNCTION: Transfers the ultimate authority to unilaterally determine secession from federal or provincial governments to a shared decision-making process that includes the consent of Indigenous treaty holders, thereby reallocating political power and territorial control.
% ABSENT_VOICES: Secessionist movements and political factions that prioritize a narrow interpretation of popular sovereignty within provincial boundaries, often ignoring or downplaying pre-existing Indigenous rights and treaty obligations. They would argue for the primacy of a provincial referendum.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, unilateral secession attempts would likely proceed without Indigenous consent, leading to severe legal challenges, political instability, potential civil unrest, and a fundamental erosion of Indigenous rights and land claims, destabilizing the entire federal structure.
% FOUNDING_PROBLEM: The historical and ongoing assertion of Indigenous sovereignty and self-determination in the face of colonial claims to absolute state sovereignty, particularly regarding land, resource rights, and the legitimacy of territorial boundaries established without Indigenous consent.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous legal scholars, international human rights bodies, and historical records consistently corroborate the ongoing nature of the problem of reconciling asserted state sovereignty with pre-existing Indigenous rights. This is attested from outside the immediate benefiting parties.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, ExtMetricName, E),
    domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(secession_legitimacy_boundary__treaty_primacy_reading),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's base extractiveness is low (0.15) because, when upheld, its primary function is to *prevent* extraction from Indigenous peoples, not to perform it. Its suppression (0.8) is high because it actively suppresses unilateral secession attempts that ignore treaty obligations. Resistance (0.7) is substantial, reflecting ongoing political and legal challenges from secessionist movements. Accessibility collapse (0.9) is high for unilateral alternatives. Theater ratio (0.1) is low as the constraint is actively contested and defended, not theatrically maintained.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Indigenous treaty holders, this constraint is a fundamental assertion of inherent rights and a necessary condition for legitimate governance. From the perspective of secessionist provincial governments, it is an obstacle to their self-determination claims, often viewed as an external imposition on their democratic will. The federal government navigates these competing claims, often acknowledging the constraint in principle but struggling with its practical implications.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous treaty holders are the primary beneficiaries (d near 0.0) as the constraint protects their sovereignty and rights. Secessionist provincial governments are the primary targets (d near 1.0) as they bear the cost of needing consent and face invalidation of unilateral claims. The federal government is a complex actor, acting as a constrained agenda-setter, benefiting from stability but also bound by treaty obligations.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents the mislabeling of Indigenous treaty rights as mere political demands or obstacles to 'progress.' By asserting their foundational and superseding nature, it highlights that the mandate for respecting these rights is live and essential, countering narratives that might otherwise dismiss them as outdated or secondary to provincial self-determination. The 'live' status of the founding problem directly challenges any claim of mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_legal_construct,
    'Is the primacy of Indigenous treaty rights a fundamental, unchangeable truth (a Mountain of inherent sovereignty) or a legal/political construct that requires active defense and interpretation?',
    'Continued legal challenges and international recognition of Indigenous self-determination, alongside shifts in state practice. If the principle holds even when actively challenged, it leans towards inherent truth; if it erodes without constant defense, it leans towards construct.',
    'If a genuine Mountain, its classification is stable regardless of political will. If a construct, its persistence and type are more vulnerable to shifts in power and legal interpretation, potentially reclassifying it as a Tangled Rope or Snare if actively undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_legal_construct, conceptual, 'Ambiguity between inherent Indigenous sovereignty and its legal codification.').

omega_variable(
    enforcement_capacity_and_political_will,
    'What is the actual capacity of Indigenous treaty holders to enforce this constraint against a determined provincial or federal government, and how does political will influence this capacity?',
    'Analysis of outcomes in specific legal and political disputes where Indigenous consent is challenged. Measuring the gap between legal victories and practical implementation.',
    'If enforcement capacity is low despite legal recognition, the constraint''s effective suppression is lower, and its extractiveness (if violated) is higher, potentially shifting its classification towards a Snare from the Indigenous perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_and_political_will, empirical, 'The practical enforceability of treaty primacy against state power.').

omega_variable(
    framing_under_determination_secession_legitimacy,
    'Does framing secession legitimacy primarily through ''treaty primacy'' (this reading) or ''constitutional impossibility'' (a sibling reading) produce different classifications for the underlying kernel?',
    'Compare the computed classifications of ''secession_legitimacy_boundary__treaty_primacy_reading'' and ''secession_legitimacy_boundary__constitutional_impossibility_reading''. If they diverge significantly, the framing choice is consequential.',
    'If the classifications diverge, the choice of framing is a critical conceptual decision, highlighting that the ''secession_legitimacy_boundary'' kernel is under-determined by its natural language label and requires explicit reading-indexed analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination_secession_legitimacy, conceptual, 'Conceptual omega for framing under-determination of the secession legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(sece_tr_t50, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(sece_be_t50, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(sece_su_t50, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 50, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__treaty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, resource_extraction_permitting).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, federal_provincial_power_sharing).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__grievance_threshold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'secession_legitimacy_boundary' kernel. Each reading presents a different structural claim regarding the conditions for legitimate secession, with differing ε values and stakeholder dynamics. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
