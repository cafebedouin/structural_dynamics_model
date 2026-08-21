% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__mandatory_interpretive_discretion, []).

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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: Balfour Mandate: Mandatory Interpretive Discretion
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint describes the British Mandatory power's interpretive
 *   discretion over the Balfour Declaration and League of Nations Mandate for
 *   Palestine. This reading focuses on how the British ability to adjudicate
 *   between competing interpretations without external review, and to shift
 *   policy (e.g., land regimes, White Papers), constituted an operational
 *   constraint system. This discretion created strategic uncertainty and
 *   path-dependent lock-in for both Arab and Zionist communities, while
 *   benefiting British colonial administrators through policy flexibility and
 *   divide-and-rule tactics. The constraint is claimed as a Snare due to its
 *   high extraction and suppression, despite the British framing it as a
 *   necessary coordination function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.65).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.75).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.65).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "Balfour Mandate: Mandatory Interpretive Discretion").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, '6c9c7740-3487-416c-b352-6bd3eb2a53a1').
narrative_ontology:cs_kernel_codification('6c9c7740-3487-416c-b352-6bd3eb2a53a1', formalized).
narrative_ontology:cs_authority_grounding('6c9c7740-3487-416c-b352-6bd3eb2a53a1', extraction).
narrative_ontology:cs_interpretation_layer_present('6c9c7740-3487-416c-b352-6bd3eb2a53a1').
narrative_ontology:cs_reading_relation('6c9c7740-3487-416c-b352-6bd3eb2a53a1', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('6c9c7740-3487-416c-b352-6bd3eb2a53a1', balfour_mandate_instruments__dual_obligation_indigenous_rights, coexists_with).
narrative_ontology:cs_axiom('6c9c7740-3487-416c-b352-6bd3eb2a53a1', foundational, unilateral_interpretive_sovereignty).
narrative_ontology:cs_axiom_status(unilateral_interpretive_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('6c9c7740-3487-416c-b352-6bd3eb2a53a1', unilateral_interpretive_sovereignty, conventional).
narrative_ontology:cs_reference_frame('6c9c7740-3487-416c-b352-6bd3eb2a53a1', unfettered_mandatory_discretion).
narrative_ontology:cs_drift_state('6c9c7740-3487-416c-b352-6bd3eb2a53a1', end_of_mandate_1948, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('6c9c7740-3487-416c-b352-6bd3eb2a53a1', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, palestinian_arab_community).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_jewish_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate authority to interpret the Mandate instruments, issuing White Papers and land ordinances that shift policy without external review. Benefits from policy flexibility, allowing for divide-and-rule tactics and maintaining control over both communities by preventing either from achieving a definitive legal status.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators, agenda_setter,
    institutional, generational, arbitrage, global).

% Subject to British policy shifts regarding land sales, immigration, and political representation. Unable to appeal to a fixed interpretation of the Mandate or international arbitration, leading to strategic uncertainty and erosion of land rights. Experiences suppression through administrative decrees and military force.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, palestinian_arab_community, payer,
    powerless, generational, trapped, regional).

% Seeks to establish a Jewish National Home, but faces constant uncertainty due to British interpretive discretion. Policy shifts (e.g., White Paper of 1939 restricting immigration and land sales) undermine their long-term goals. While organized, their power is constrained by the Mandatory's ultimate authority.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_jewish_community, payer,
    organized, generational, constrained, regional).

% Nominally oversees the Mandate system but lacks enforcement power over British interpretive decisions. Receives reports and hears petitions but cannot compel changes to British policy, effectively legitimizing British discretion through inaction.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, league_of_nations_mandates_commission, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for British administration of Palestine, ostensibly to prepare the territory for self-governance while balancing competing claims. The discretion allows for 'adaptive' governance in a complex, contested environment.
% TRANSFER_FUNCTION: Transfers political and legal authority over the territory and its inhabitants to the British Mandatory power, enabling the British to extract strategic geopolitical benefits and maintain control through interpretive ambiguity.
% ABSENT_VOICES: An independent international arbitration body with binding authority would challenge British interpretive discretion, forcing adherence to fixed legal principles rather than shifting policy. The indigenous Palestinian population's right to self-determination was largely absent from the Mandate's framing.
% DISAPPEARANCE_RATIONALE: If British interpretive discretion vanished, the legal and political landscape of Palestine would immediately reorganize. Both Arab and Zionist communities would seek to establish fixed legal claims based on their preferred readings of the Mandate, likely leading to intensified conflict or immediate international arbitration to define the territory's future.
% FOUNDING_PROBLEM: The problem of administering former Ottoman territories after WWI, specifically Palestine, with competing promises made to Arab and Jewish communities, and the need to establish a transitional governance structure.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international legal scholars widely corroborate that the founding problem of 'balancing' competing claims under a discretionary mandate was inherently unstable and ultimately failed, leading to the 1948 conflict. The discretion itself became a tool for maintaining British power rather than resolving the underlying tensions.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(balfour_mandate_instruments__mandatory_interpretive_discretion, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because British discretion allowed for policies that extracted land and political agency from one community to benefit the other, or to maintain British control, without a fixed legal basis. Suppression is high because British authority was enforced through administrative decrees and military presence, preventing either community from appealing to a higher, fixed legal standard. The theater ratio is moderate, as the 'balancing act' narrative often masked the strategic use of ambiguity to maintain British power. The extractiveness and suppression fluctuated with periods of increased Arab or Zionist resistance and subsequent British policy shifts.
 *
 * PERSPECTIVAL GAP:
 *   From the British perspective, their interpretive discretion was a necessary, if difficult, coordination function to manage an intractable conflict. From the perspective of both Arab and Zionist communities, it was an arbitrary and extractive mechanism that prevented resolution and served British imperial interests. The engine's classification as a Snare reflects the latter perspective, diverging from the British 'Rope' claim.
 *
 * DIRECTIONALITY LOGIC:
 *   British colonial administrators are the primary beneficiaries, gaining policy flexibility and maintaining control (low d). Both the Palestinian Arab and Zionist Jewish communities are victims, subjected to shifting policies and unable to appeal to fixed interpretations (high d). The League of Nations Mandates Commission, while an observer, effectively legitimized British discretion through its lack of enforcement power.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to prepare Palestine for self-governance. However, British interpretive discretion, rather than facilitating this, became a mechanism for prolonged control and extraction. The founding problem of balancing competing claims became 'dead' as discretion perpetuated conflict, leading to a Snare where the original coordination function atrophied into pure extraction. The classification prevents mislabeling this as a genuine coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_as_coordination_vs_extraction,
    'Was British interpretive discretion a genuine, albeit flawed, attempt at coordination in an intractable conflict, or primarily a tool for colonial extraction and control?',
    'Analysis of internal British colonial office documents revealing strategic intent behind policy shifts, and comparative studies of other Mandate administrations'' approaches to similar conflicts.',
    'If primarily coordination, the constraint might lean towards a Tangled Rope; if primarily extraction, its Snare classification is reinforced. This impacts the assessment of the British administrators'' directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_as_coordination_vs_extraction, conceptual, 'Ambiguity of discretion''s primary function: coordination or extraction.').

omega_variable(
    counterfactual_fixed_interpretation,
    'What would have been the outcome if the Mandate instruments had been subject to a fixed, externally reviewable interpretation from the outset?',
    'Historical counterfactual analysis, comparing outcomes in other territories with more rigidly defined international mandates or arbitration mechanisms.',
    'If a fixed interpretation would have led to earlier resolution or less conflict, it reinforces the extractive nature of discretion. If it would have led to immediate, unmanageable conflict, it might lend credence to the ''coordination'' aspect of discretion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_fixed_interpretation, empirical, 'Impact of a counterfactual fixed interpretation on conflict resolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(balf_tr_t1928, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1928, 0.15).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1936, 0.25).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1948, 0.2).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(balf_be_t1928, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1928, 0.6).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1936, 0.68).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1948, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(balf_su_t1928, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1928, 0.65).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1936, 0.78).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1948, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
