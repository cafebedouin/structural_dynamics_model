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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: British Mandatory Interpretive Discretion over Palestine Mandate
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint describes the British Mandatory Power's unchecked
 *   authority to interpret the terms of the Palestine Mandate, effectively
 *   making its interpretive discretion the operational constraint system.
 *   This reading highlights how the British leveraged ambiguity to maintain
 *   control and adapt policy, often at the expense of both Arab and Jewish
 *   communities who lacked recourse to a fixed textual meaning or external
 *   arbitration. The constraint is claimed as a snare because the
 *   coordination story (preparing for self-governance) was cover for an
 *   extractive system that benefited the administrators by maintaining
 *   strategic flexibility and control.
 *
 * KEY AGENTS:
 *   - british_colonial_administrators: Agenda setter (institutional/arbitrage) — benefits from policy flexibility and control.
 *   - arab_community_palestine: Payer (organized/trapped) — bears costs of policy shifts, lacks interpretive recourse.
 *   - jewish_community_palestine: Payer (organized/constrained) — bears costs of policy shifts, lacks interpretive recourse.
 *   - league_of_nations: Observer (institutional/analytical) — nominal oversight, limited practical power.
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
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "British Mandatory Interpretive Discretion over Palestine Mandate").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, 'a9b9681a-2846-4e79-86d7-718f3138be0b').
narrative_ontology:cs_kernel_codification('a9b9681a-2846-4e79-86d7-718f3138be0b', formalized).
narrative_ontology:cs_authority_grounding('a9b9681a-2846-4e79-86d7-718f3138be0b', extraction).
narrative_ontology:cs_interpretation_layer_present('a9b9681a-2846-4e79-86d7-718f3138be0b').
narrative_ontology:cs_reading_relation('a9b9681a-2846-4e79-86d7-718f3138be0b', balfour_mandate_instruments__jewish_national_home_primacy, influences).
narrative_ontology:cs_reading_relation('a9b9681a-2846-4e79-86d7-718f3138be0b', balfour_mandate_instruments__dual_obligation_indigenous_rights, influences).
narrative_ontology:cs_axiom('a9b9681a-2846-4e79-86d7-718f3138be0b', foundational, unilateral_interpretive_sovereignty).
narrative_ontology:cs_axiom_status(unilateral_interpretive_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('a9b9681a-2846-4e79-86d7-718f3138be0b', unilateral_interpretive_sovereignty, conventional).
narrative_ontology:cs_axiom('a9b9681a-2846-4e79-86d7-718f3138be0b', secondary, strategic_ambiguity_as_governance_tool).
narrative_ontology:cs_axiom_status(strategic_ambiguity_as_governance_tool, holdable).
narrative_ontology:cs_axiom_grounding('a9b9681a-2846-4e79-86d7-718f3138be0b', strategic_ambiguity_as_governance_tool, instrumental).
narrative_ontology:cs_reference_frame('a9b9681a-2846-4e79-86d7-718f3138be0b', unfettered_mandatory_discretion).
narrative_ontology:cs_drift_state('a9b9681a-2846-4e79-86d7-718f3138be0b', post_unscop_report_1947, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a9b9681a-2846-4e79-86d7-718f3138be0b', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_community_palestine).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_community_palestine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Mandate for Palestine, holding ultimate authority to interpret its terms and implement policies. Benefits from the flexibility and strategic ambiguity, allowing for adaptation to geopolitical shifts and maintaining control through divide-and-rule tactics. Their discretion is the core of the constraint.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators, agenda_setter,
    institutional, generational, arbitrage, global).

% Subject to British policy shifts regarding land, immigration, and political representation, often to their detriment. They are unable to appeal to a fixed interpretation of the Mandate or external arbitration, leading to strategic uncertainty and loss of control over their future. Their resistance is met with British enforcement.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_community_palestine, payer,
    organized, generational, trapped, local).

% Also subject to British policy shifts, which at times favor their national home aspirations and at other times restrict them. While benefiting from British protection and facilitation of immigration, they are ultimately dependent on British discretion, leading to uncertainty and frustration when policies diverge from their maximalist interpretation of the Mandate. Their ability to appeal is limited by the same interpretive authority.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_community_palestine, payer,
    organized, generational, constrained, local).

% The nominal oversight body for the Mandate, but with limited practical power to challenge British interpretive authority or policy implementation. Its role is largely symbolic, providing a veneer of international legitimacy without effective external review.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, league_of_nations, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for British administration of Palestine, ostensibly to prepare the territory for self-governance while facilitating the establishment of a Jewish national home, balancing competing claims under a single authority.
% TRANSFER_FUNCTION: Transfers interpretive authority over the Mandate's terms to the British Mandatory Power, allowing it to allocate land, immigration quotas, and political rights, primarily from the indigenous Arab population to the Jewish immigrant population, and from both communities to British administrative control.
% ABSENT_VOICES: An independent international tribunal with binding arbitration power would object to the lack of external review and the unchecked interpretive discretion, arguing for a fixed, rights-based interpretation of the Mandate. The indigenous Palestinian population's right to self-determination, as understood by international law, was largely absent from the British interpretive framework.
% DISAPPEARANCE_RATIONALE: If British interpretive discretion vanished, the legal and political landscape of Palestine would immediately reorganize. The competing claims of the Arab and Jewish communities would no longer be mediated by a single, flexible authority, likely leading to direct conflict or a new international arbitration framework. The entire state-formation process would have taken a different path.
% FOUNDING_PROBLEM: To manage the post-Ottoman disposition of Palestine, reconcile the Balfour Declaration's promise of a Jewish national home with the existing Arab population's rights, and prepare the territory for self-governance under international supervision.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem of reconciling competing claims under a temporary mandate is dead, as the Mandate ended in 1948 and the conflict persists. Historians and international legal scholars, outside the original British beneficiaries, corroborate that the interpretive discretion ultimately failed to resolve the underlying tensions and instead exacerbated them, leading to the current contested status of the territory.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(balfour_mandate_instruments__mandatory_interpretive_discretion, 'none', 1).

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
 *   Extractiveness is high (0.65) because British discretion allowed for policies that systematically favored British strategic interests and, at times, one community over the other, creating an asymmetric transfer of resources and rights. Suppression is also high (0.75) as both communities were denied effective means to challenge British interpretations or policies, with resistance often met by force. Theater ratio is moderate (0.20) as the stated goal of preparing for self-governance became increasingly performative as British interests dominated. The temporal measurements show a rise in extractiveness and suppression as the Mandate progressed and British control became more entrenched, peaking around WWII when strategic interests were paramount.
 *
 * PERSPECTIVAL GAP:
 *   British colonial administrators experienced this as a necessary, if challenging, coordination mechanism to manage a complex geopolitical situation. For both the Arab and Jewish communities, it was experienced as an arbitrary and extractive system that denied them agency and self-determination. The engine's per-seat classification should reflect this divergence, with the British seat computing as a beneficiary of a 'rope' (from their perspective) and the communities as victims of a 'snare'.
 *
 * DIRECTIONALITY LOGIC:
 *   British colonial administrators are the primary beneficiaries (d=0.0-0.1) as their interpretive discretion allowed them to maintain control and pursue strategic interests. Both the Arab and Jewish communities are targets (d=0.8-0.9) as they bore the costs of policy shifts and lacked effective exit or appeal options. The League of Nations is an analytical observer (d=0.5) with no direct stake in the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preparing for self-governance) atrophied as British interpretive discretion became an end in itself, serving colonial interests rather than the stated goals. The classification as a snare prevents mislabeling this as a genuine coordination mechanism, highlighting the coercive and extractive nature of unchecked interpretive authority. The 'dead' status of the founding problem, coupled with the 'world_rearranges' verdict, signals a zombie constraint where the original purpose is gone but the structure persists, extracting value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_discretion_vs_fixed_text,
    'To what extent was British interpretive discretion a necessary function of managing an ambiguous mandate, versus a deliberate strategy to avoid accountability to a fixed textual meaning?',
    'Analysis of internal British policy documents and diplomatic correspondence: if documents reveal explicit strategies to maintain ambiguity for control, it supports the latter. If they show genuine attempts to reconcile irreconcilable texts, it supports the former.',
    'If deliberate strategy, the extractiveness and suppression metrics are more firmly rooted in intentional design; if necessary function, the constraint might lean more towards a tangled rope where coordination costs are high due to inherent ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_discretion_vs_fixed_text, empirical, 'Ambiguity of interpretive discretion''s purpose.').

omega_variable(
    mandate_as_kernel_reading,
    'Is this constraint a genuine reading of the Balfour Mandate instruments, or does it represent a structural feature of colonial administration that merely used the Mandate as a pretext?',
    'Comparative analysis with other British mandates (e.g., Iraq, Transjordan): if similar interpretive discretion and outcomes are observed across different mandate texts, it suggests a broader colonial structural feature. If unique to Palestine, it points to a specific reading of this kernel.',
    'If a broader colonial structural feature, the constraint''s scope and persistence are larger than this specific kernel suggests; if unique, its classification is more tightly bound to the Mandate''s specific terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_as_kernel_reading, conceptual, 'This constraint is one reading of the ''balfour_mandate_instruments'' kernel, specifically ''mandatory_interpretive_discretion''. Sibling readings include ''jewish_national_home_primacy'' and ''dual_obligation_indigenous_rights''. This reading emphasizes the British power to adjudicate without external review, which structurally influences both sibling readings by defining the terms of their contestation. A shift to either sibling reading would fundamentally alter the power dynamics and beneficiary/victim structure, as the interpretive authority would be constrained by either a primacy of Jewish national home development or indigenous rights protection, rather than British discretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(balf_tr_t1925, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1925, 0.12).
narrative_ontology:measurement(balf_tr_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1930, 0.15).
narrative_ontology:measurement(balf_tr_t1935, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1935, 0.18).
narrative_ontology:measurement(balf_tr_t1940, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1940, 0.22).
narrative_ontology:measurement(balf_tr_t1945, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1948, 0.2).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(balf_be_t1925, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1925, 0.6).
narrative_ontology:measurement(balf_be_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1930, 0.65).
narrative_ontology:measurement(balf_be_t1935, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1935, 0.68).
narrative_ontology:measurement(balf_be_t1940, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1940, 0.7).
narrative_ontology:measurement(balf_be_t1945, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1945, 0.68).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1948, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1920, 0.65).
narrative_ontology:measurement(balf_su_t1925, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1925, 0.68).
narrative_ontology:measurement(balf_su_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1930, 0.72).
narrative_ontology:measurement(balf_su_t1935, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1935, 0.75).
narrative_ontology:measurement(balf_su_t1940, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1940, 0.8).
narrative_ontology:measurement(balf_su_t1945, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1945, 0.78).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1948, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
