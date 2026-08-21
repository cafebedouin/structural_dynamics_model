% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__security_necessity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Territorial Legitimacy via Security Necessity (1967 borders + strategic depth)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'security necessity' reading of
 *   territorial legitimacy in the Israeli-Palestinian conflict. It frames
 *   Israeli control over territories beyond the 1967 borders (e.g., West
 *   Bank, Golan Heights) as legitimate due to ongoing security threats and
 *   the need for strategic depth. Palestinian sovereignty is seen as
 *   conditional on demilitarization, and Israeli settlements are justified as
 *   a security presence. This reading is distinct from those based on
 *   international partition or indigenous continuity, focusing instead on a
 *   pragmatic, defensive territorial control. The constraint is classified as
 *   a Tangled Rope due to its genuine coordination function (Israeli
 *   security) intertwined with asymmetric extraction (Palestinian territorial
 *   and political rights) requiring active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.85).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.92).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Territorial Legitimacy via Security Necessity (1967 borders + strategic depth)").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, 'd2a86e4a-4669-4d28-8e1e-a546afa74f97').
narrative_ontology:cs_kernel_codification('d2a86e4a-4669-4d28-8e1e-a546afa74f97', formalized).
narrative_ontology:cs_authority_grounding('d2a86e4a-4669-4d28-8e1e-a546afa74f97', extraction).
narrative_ontology:cs_interpretation_layer_present('d2a86e4a-4669-4d28-8e1e-a546afa74f97').
narrative_ontology:cs_reading_relation('d2a86e4a-4669-4d28-8e1e-a546afa74f97', territorial_legitimacy__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2a86e4a-4669-4d28-8e1e-a546afa74f97', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('d2a86e4a-4669-4d28-8e1e-a546afa74f97', foundational, territorial_depth_as_security_imperative).
narrative_ontology:cs_axiom_status(territorial_depth_as_security_imperative, holdable).
narrative_ontology:cs_axiom_grounding('d2a86e4a-4669-4d28-8e1e-a546afa74f97', territorial_depth_as_security_imperative, empirically_contingent).
narrative_ontology:cs_axiom('d2a86e4a-4669-4d28-8e1e-a546afa74f97', foundational, demilitarization_as_sovereignty_precondition).
narrative_ontology:cs_axiom_status(demilitarization_as_sovereignty_precondition, holdable).
narrative_ontology:cs_axiom_grounding('d2a86e4a-4669-4d28-8e1e-a546afa74f97', demilitarization_as_sovereignty_precondition, instrumental).
narrative_ontology:cs_reference_frame('d2a86e4a-4669-4d28-8e1e-a546afa74f97', post_1967_defensive_posture).
narrative_ontology:cs_drift_state('d2a86e4a-4669-4d28-8e1e-a546afa74f97', contemporary_geopolitical_context, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d2a86e4a-4669-4d28-8e1e-a546afa74f97', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, state_of_israel).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_settlers).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_population).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts control over territories beyond 1967 borders (West Bank, Golan Heights) as essential for national security, citing historical attacks and ongoing threats. Administers these territories, including settlement expansion, and enforces demilitarization on Palestinian areas. Benefits from strategic depth and resource control.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, state_of_israel, agenda_setter,
    institutional, generational, constrained, national).

% Lives under military occupation and administrative control in the West Bank and Gaza, with restricted movement, land access, and resource use. Bears the direct costs of territorial control, including displacement, settlement expansion, and limited self-governance. Exit options are severely limited by borders and identity.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_population, payer,
    powerless, generational, trapped, regional).

% Exercises limited civil authority in fragmented areas of the West Bank, conditional on security cooperation with Israel. Its legitimacy and function are constrained by Israeli security imperatives, making it a payer of the security necessity framework while nominally representing the Palestinian population. Exit means collapse of self-governance structures.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_authority, payer,
    moderate, biographical, identity_locked, regional).

% Reside in communities established in the West Bank and Golan Heights, often with state support and protection, justified as part of the security presence. Benefit from subsidized housing, infrastructure, and a territorial claim that expands Israel's effective control. Their presence is integral to the security necessity argument.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_settlers, beneficiary,
    organized, generational, mobile, local).

% Observes and often critiques the application of this security necessity reading, particularly regarding international law on occupation and self-determination. Its actions (diplomacy, aid, sanctions) can influence the constraint but do not directly set its terms.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the defense of Israel by establishing a territorial buffer and maintaining military control over strategic high ground and border areas, preventing hostile forces from threatening Israeli population centers.
% TRANSFER_FUNCTION: Transfers territorial control, resource access, and security guarantees from the Palestinian population to the State of Israel and Israeli settlers, in exchange for perceived national security.
% ABSENT_VOICES: Palestinian refugees and diaspora communities, whose right of return is foreclosed by this reading's territorial claims, are excluded from the direct negotiation of these terms. Their voices would emphasize historical displacement and self-determination.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, Israel would lose its claimed security buffer, potentially leading to immediate military re-engagement or a rapid re-drawing of borders. The Palestinian population would likely assert full sovereignty over the West Bank and Gaza, leading to a fundamental rearrangement of the regional political and security landscape.
% FOUNDING_PROBLEM: The existential threat to Israel from surrounding hostile states and non-state actors, particularly after the 1967 Six-Day War, necessitating territorial control for defensive depth.
% FOUNDING_PROBLEM_CORROBORATION: The State of Israel and its security establishment consistently attest that the founding problem remains live, citing ongoing regional instability and threats from various groups. This is corroborated by historical military conflicts and current geopolitical analyses from independent defense strategists, though the extent of territorial necessity is contested by international legal scholars.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because this reading legitimizes significant territorial and resource control from the Palestinian population. Suppression is very high (0.92) as the constraint's persistence relies heavily on military occupation, administrative control, and the active suppression of Palestinian resistance and political aspirations. Theater ratio is moderate (0.4) because while genuine security concerns exist, a substantial portion of the enforcement and territorial expansion serves to consolidate control and expand settlements, which are not solely defensive in nature. The metrics reflect a system where security is achieved at a high cost to the governed population, maintained through active coercion.
 *
 * PERSPECTIVAL GAP:
 *   From the Israeli security perspective, this constraint is a necessary Rope, ensuring survival. From the Palestinian perspective, it is a Snare, entrenching occupation and denying self-determination. The engine's classification as Tangled Rope reflects the hybrid nature: a genuine security coordination problem for one party, achieved through substantial extraction from another, requiring active enforcement to maintain.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel and Israeli settlers are clear beneficiaries, gaining security, land, and resources. The Palestinian population and Palestinian Authority are targets, bearing the costs of occupation, restricted sovereignty, and limited self-determination. The international community acts as an observer, often critical but not directly subject to the constraint's extraction or coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (Israeli security) is still considered 'live' by its primary beneficiaries, preventing a full Mandatrophy resolution. However, the rising extractiveness and theater ratio over time suggest a drift where the 'security necessity' justification increasingly covers territorial expansion and control beyond immediate defensive needs. The classification as Tangled Rope, rather than a pure Snare, acknowledges the initial and ongoing security imperative, while highlighting the asymmetric extraction that has become integral to its operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_vs_expansion_boundary,
    'At what point does ''security necessity'' transition into territorial expansion or political control, and how can this boundary be empirically determined?',
    'Independent military and geopolitical analysis comparing claimed security needs with actual territorial acquisitions and settlement patterns, alongside international legal interpretations of defensive vs. offensive control.',
    'If the boundary is crossed, the ''security necessity'' justification becomes a cover for extraction, shifting the constraint closer to a Snare. If the necessity is consistently demonstrated, it reinforces the coordination aspect of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_vs_expansion_boundary, empirical, 'Distinguishing genuine security needs from territorial expansion under the guise of security.').

omega_variable(
    demilitarization_feasibility,
    'Is full Palestinian demilitarization a genuinely achievable and verifiable condition for sovereignty, or is it a perpetually moving goalpost designed to prevent full sovereignty?',
    'Analysis of historical demilitarization agreements and their implementation, alongside expert assessment of verification technologies and international guarantees for a demilitarized state.',
    'If demilitarization is feasible and verifiable, the condition for Palestinian sovereignty is a legitimate coordination mechanism. If it''s a moving goalpost, it reinforces the extractive nature of the constraint, as a condition that can never be met maintains the status quo of control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demilitarization_feasibility, empirical, 'Assessing the practical and political feasibility of Palestinian demilitarization as a condition for sovereignty.').

omega_variable(
    legitimacy_framing_contest,
    'Is the ''security necessity'' framing a primary, self-sufficient basis for territorial legitimacy, or is it a secondary justification layered over other (contested) claims?',
    'Comparative analysis of legal and political discourse, examining the historical evolution of arguments for territorial control and the relative prominence of security vs. historical/religious claims.',
    'If primary, the constraint''s internal logic is robust within its own framework. If secondary, its legitimacy is parasitic on the underlying claims, and its stability is vulnerable to challenges to those deeper claims, potentially revealing it as a more fragile Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_framing_contest, conceptual, 'The foundational status of security necessity as a legitimizing principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__security_necessity_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(terr_tr_t1980, territorial_legitimacy__security_necessity_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__security_necessity_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy__security_necessity_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(terr_tr_t2015, territorial_legitimacy__security_necessity_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__security_necessity_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(terr_be_t1980, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1993, 0.75).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(terr_be_t2015, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2015, 0.83).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(terr_su_t1980, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1993, 0.85).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2005, 0.89).
narrative_ontology:measurement(terr_su_t2015, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2015, 0.91).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__security_necessity_reading, 0.1).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__indigenous_continuity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, israeli_settlement_expansion).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'territorial_legitimacy' kernel. It focuses on security necessity, influencing and coexisting with readings based on partition and indigenous continuity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
