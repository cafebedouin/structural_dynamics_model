% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: Originalist Reading of US Constitutional Meaning
 *   domain: legal/political/interpretive
 *
 * SUMMARY:
 *   This constraint represents the originalist reading of the US
 *   Constitution, which asserts that the Constitution's meaning was fixed at
 *   the time of its ratification and should be interpreted according to the
 *   framers' intent or the original public meaning of its text. This reading
 *   aims to provide interpretive stability and limit judicial discretion, but
 *   it generates significant contestation regarding its application to modern
 *   issues and its impact on rights not explicitly recognized at the
 *   founding. The metrics reflect its function as a coordinating legal
 *   methodology that also imposes costs on certain groups and requires active
 *   defense against alternative interpretive approaches.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.45).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.65).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "Originalist Reading of US Constitutional Meaning").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "legal/political/interpretive").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, '61c7c640-fc7b-4365-b553-b97d7809436c').
narrative_ontology:cs_kernel_codification('61c7c640-fc7b-4365-b553-b97d7809436c', fixed_text).
narrative_ontology:cs_authority_grounding('61c7c640-fc7b-4365-b553-b97d7809436c', lineage).
narrative_ontology:cs_interpretation_layer_present('61c7c640-fc7b-4365-b553-b97d7809436c').
narrative_ontology:cs_reading_relation('61c7c640-fc7b-4365-b553-b97d7809436c', us_constitution_interpretive__living_constitution_reading, forecloses).
narrative_ontology:cs_reading_relation('61c7c640-fc7b-4365-b553-b97d7809436c', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('61c7c640-fc7b-4365-b553-b97d7809436c', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('61c7c640-fc7b-4365-b553-b97d7809436c', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('61c7c640-fc7b-4365-b553-b97d7809436c', foundational, judicial_role_limited_to_original_meaning).
narrative_ontology:cs_axiom_status(judicial_role_limited_to_original_meaning, holdable).
narrative_ontology:cs_axiom_grounding('61c7c640-fc7b-4365-b553-b97d7809436c', judicial_role_limited_to_original_meaning, deontological).
narrative_ontology:cs_reference_frame('61c7c640-fc7b-4365-b553-b97d7809436c', ratification_era_understanding).
narrative_ontology:cs_drift_state('61c7c640-fc7b-4365-b553-b97d7809436c', contemporary_legal_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('61c7c640-fc7b-4365-b553-b97d7809436c', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, original_public_meaning_proponents).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, living_constitutionalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Constitution based on fidelity to the framers' intent or original public meaning, applying this methodology to cases and setting legal precedent. Their authority is derived from their judicial office, but their interpretive method is self-imposed.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the originalist reading's tendency to limit federal power and preserve state autonomy, aligning with their political and legal philosophy. They actively support the appointment of originalist judges.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    organized, generational, mobile, national).

% Advocate for the originalist methodology as the correct way to interpret the Constitution, seeing it as a bulwark against judicial activism and a guarantor of stable law. They benefit from its adoption as the dominant interpretive framework.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, original_public_meaning_proponents, beneficiary,
    organized, generational, mobile, national).

% Bear the cost of having their claimed rights (e.g., privacy, certain equality claims) denied or limited because these rights are not explicitly found or clearly implied in the original understanding of the Constitution. Their legal avenues for redress are significantly narrowed.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Find their efforts to expand federal regulatory power (e.g., environmental protection, economic regulation) limited by a narrow, originalist reading of Congress's enumerated powers and the Commerce Clause. They must justify actions strictly within a historically constrained framework.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    institutional, biographical, constrained, national).

% Proponents of an evolving constitutional meaning, whose interpretive methodology is largely rejected or marginalized within courts dominated by originalist thought. Their arguments for adapting the Constitution to contemporary societal values are often dismissed.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, living_constitutionalists, excluded,
    institutional, generational, constrained, national).

% Analyze and critique originalist methodology, its theoretical underpinnings, its application in case law, and its effects on law and society. They contribute to the ongoing academic debate but do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for constitutional interpretation, aiming to limit judicial discretion and ensure fidelity to the founding document, thereby coordinating legal actors around a common interpretive method.
% TRANSFER_FUNCTION: Transfers interpretive authority from evolving societal norms or judicial discretion to historical texts and intentions, thereby shifting power dynamics between federal and state governments, and between different groups of rights claimants. It also transfers legitimacy from contemporary democratic processes to historical consensus.
% ABSENT_VOICES: Proponents of popular constitutionalism, who argue for direct democratic input into constitutional meaning, are largely excluded from the judicial interpretive debate dominated by originalism and living constitutionalism. Their arguments for a more democratically responsive Constitution are not given standing in the courts.
% DISAPPEARANCE_RATIONALE: If the originalist interpretive method vanished overnight, the interpretive landscape would shift dramatically. Judicial decisions would likely reflect a more expansive view of federal power and unenumerated rights, and the balance between branches of government and between federal and state authority would reorganize around a different interpretive philosophy.
% FOUNDING_PROBLEM: To prevent judicial overreach, ensure democratic accountability by tying interpretation to the people's original consent, and maintain a stable constitutional order by resisting transient political pressures.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and conservative legal organizations attest to the ongoing problem of judicial activism and the need for interpretive stability. Critics (e.g., living constitutionalists, some political scientists) argue that the founding problem is either overstated, or that originalism itself creates new forms of judicial overreach by imposing anachronistic views, or that it fails to address contemporary challenges.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__originalist_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).
:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45 at interval end) because while it aims to limit government overreach, it can be highly extractive for groups whose rights or interests are not aligned with the original understanding. Suppression is high (0.65) as it actively suppresses alternative interpretive methodologies in judicial practice. Theater ratio is moderate-low (0.25) as the methodology is a genuine attempt at legal interpretation, though some applications may involve performative aspects to maintain consistency with originalist principles. The increasing trends in extractiveness and suppression reflect the growing dominance and politicization of originalism over the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist judges and advocates, this constraint is a legitimate and necessary framework for constitutional governance, ensuring fidelity to the founding document. From the perspective of unenumerated rights claimants or federal regulatory agencies, it is an extractive and suppressive force that limits their ability to address contemporary societal needs or secure modern rights. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges and advocates are beneficiaries, as the constraint aligns with their interpretive philosophy and empowers their legal arguments. Federalism advocates also benefit from its tendency to limit federal power. Unenumerated rights claimants and federal regulatory expansion advocates are targets, as their claims are constrained by the originalist framework. Living constitutionalists are excluded, as their entire interpretive approach is deemed illegitimate by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_ambiguity,
    'Is ''original public meaning'' or ''framers'' intent'' a sufficiently determinate concept to provide stable, non-discretionary interpretation, or does it contain inherent ambiguities that allow for judicial discretion?',
    'Empirical analysis of judicial decisions claiming originalist fidelity: if outcomes vary widely on similar facts, it suggests interpretive discretion within the methodology. Conceptual analysis of historical sources and linguistic theory.',
    'If highly ambiguous, the constraint''s claimed coordination function (interpretive stability) is undermined, and its effective extractiveness may be higher due to unacknowledged judicial discretion. If determinate, its legitimacy as a stable framework is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_ambiguity, conceptual, 'Determinacy of originalist interpretive methods.').

omega_variable(
    judicial_power_locus_shift,
    'Does originalism genuinely limit judicial power, or does it merely shift the locus of judicial discretion from contemporary values to historical reconstruction, potentially creating new forms of judicial activism?',
    'Comparative analysis of judicial outcomes and reasoning under originalist vs. non-originalist regimes, focusing on the scope and nature of judicial intervention. Examination of the historical research required for originalist opinions.',
    'If it merely shifts discretion, the constraint''s claimed benefit (limiting judicial overreach) is undermined, and its effective suppression of alternative legal arguments may be higher than acknowledged. If it genuinely limits, its coordination function is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_power_locus_shift, empirical, 'Whether originalism limits or merely redirects judicial power.').

omega_variable(
    inter_reading_contestation,
    'To what extent is the persistence of the originalist reading dependent on its inherent structural merits versus the political power of its proponents in judicial appointments and legal discourse?',
    'Analysis of judicial appointment patterns, funding for legal advocacy groups, and public opinion shifts regarding constitutional interpretation. Counterfactual analysis of judicial outcomes under different political compositions.',
    'If persistence is primarily political, the constraint''s effective suppression of rival readings is higher, and its claimed legitimacy as a neutral interpretive method is weakened. If structural merits are dominant, its rope-like coordination function is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inter_reading_contestation, preference, 'Political vs. structural persistence of originalism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_interpretive__originalist_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_interpretive__originalist_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_interpretive__originalist_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_interpretive__originalist_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_interpretive__originalist_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(us_c_tr_t2020, us_constitution_interpretive__originalist_reading, theater_ratio, 2020, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_interpretive__originalist_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(us_c_be_t1980, us_constitution_interpretive__originalist_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_interpretive__originalist_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_interpretive__originalist_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_interpretive__originalist_reading, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement(us_c_be_t2020, us_constitution_interpretive__originalist_reading, base_extractiveness, 2020, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_interpretive__originalist_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(us_c_su_t1980, us_constitution_interpretive__originalist_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_interpretive__originalist_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_interpretive__originalist_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_interpretive__originalist_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(us_c_su_t2020, us_constitution_interpretive__originalist_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'US Constitution interpretive' kernel. Its ε value differs significantly from its sibling readings due to its specific interpretive methodology and resulting beneficiary/victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
