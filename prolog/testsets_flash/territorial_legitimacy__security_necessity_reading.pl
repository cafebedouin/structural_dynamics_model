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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Territorial Legitimacy via Security Necessity (1967 Borders + Strategic Depth)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint represents a specific reading of territorial legitimacy,
 *   asserting that control over territories (e.g., West Bank, Golan Heights)
 *   beyond 1948 borders is legitimate due to security necessity and the need
 *   for strategic depth. It posits that Palestinian sovereignty is
 *   conditional on demilitarization and views settlements as a legitimate
 *   security presence. This reading is actively enforced and contested,
 *   leading to high extractiveness and suppression for the Palestinian
 *   population.
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
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Territorial Legitimacy via Security Necessity (1967 Borders + Strategic Depth)").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, '9aa10766-e09e-46a9-ab04-f0ffdfdefcad').
narrative_ontology:cs_kernel_codification('9aa10766-e09e-46a9-ab04-f0ffdfdefcad', implicit).
narrative_ontology:cs_authority_grounding('9aa10766-e09e-46a9-ab04-f0ffdfdefcad', extraction).
narrative_ontology:cs_interpretation_layer_present('9aa10766-e09e-46a9-ab04-f0ffdfdefcad').
narrative_ontology:cs_reading_relation('9aa10766-e09e-46a9-ab04-f0ffdfdefcad', territorial_legitimacy__partition_reading, influences).
narrative_ontology:cs_reading_relation('9aa10766-e09e-46a9-ab04-f0ffdfdefcad', territorial_legitimacy__indigenous_continuity_reading, forecloses).
narrative_ontology:cs_axiom('9aa10766-e09e-46a9-ab04-f0ffdfdefcad', foundational, security_trumps_pre_existing_borders).
narrative_ontology:cs_axiom_status(security_trumps_pre_existing_borders, holdable).
narrative_ontology:cs_axiom_grounding('9aa10766-e09e-46a9-ab04-f0ffdfdefcad', security_trumps_pre_existing_borders, instrumental).
narrative_ontology:cs_axiom('9aa10766-e09e-46a9-ab04-f0ffdfdefcad', foundational, strategic_depth_is_existential).
narrative_ontology:cs_axiom_status(strategic_depth_is_existential, holdable).
narrative_ontology:cs_axiom_grounding('9aa10766-e09e-46a9-ab04-f0ffdfdefcad', strategic_depth_is_existential, empirically_contingent).
narrative_ontology:cs_reference_frame('9aa10766-e09e-46a9-ab04-f0ffdfdefcad', post_1967_defensive_posture).
narrative_ontology:cs_drift_state('9aa10766-e09e-46a9-ab04-f0ffdfdefcad', contemporary_settlement_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9aa10766-e09e-46a9-ab04-f0ffdfdefcad', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, state_of_israel).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_settlers).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_population).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_authority).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, international_law_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and enforces control over territories based on security necessity, maintaining military presence and administrative structures. Benefits from strategic depth and perceived security, but faces international condemnation and ongoing resistance.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, state_of_israel, agenda_setter,
    institutional, generational, constrained, national).

% Lives under military occupation and administrative control, experiencing restrictions on movement, land confiscation, and limited self-determination. Bears the direct costs of the security necessity claims.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_population, payer,
    powerless, generational, trapped, local).

% Reside in settlements in contested territories, benefiting from state protection, infrastructure, and often subsidized living. Their presence is justified by the security necessity reading, but they face security risks and international legal challenges.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_settlers, beneficiary,
    organized, generational, constrained, local).

% Exercises limited self-governance in fragmented areas, but its sovereignty is conditional and constrained by Israeli security control. Bears the political and administrative costs of the occupation.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_authority, payer,
    moderate, biographical, constrained, regional).

% Monitor and critique the application of international law, arguing that occupation and settlement activities violate established norms. They seek to influence international opinion and policy, but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_law_advocates, observer,
    organized, civilizational, analytical, global).

% Passes resolutions condemning occupation and settlement activities, but lacks the enforcement mechanisms to compel compliance. Its authority is challenged by the security necessity reading, which prioritizes national security over international legal consensus.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, united_nations, excluded,
    institutional, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__security_necessity_reading, state_of_israel).
narrative_ontology:fixing_cost_class(territorial_legitimacy__security_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the defense of the State of Israel by maintaining strategic depth and preventing hostile forces from establishing positions close to its population centers.
% TRANSFER_FUNCTION: Transfers land, resources, and sovereignty from the Palestinian population to the State of Israel, in exchange for perceived security and strategic advantage.
% ABSENT_VOICES: The voices of the Palestinian diaspora and refugees, who would assert their right of return and full self-determination, are largely excluded from the immediate political discourse and decision-making processes that shape this constraint.
% DISAPPEARANCE_RATIONALE: If the security necessity claim and its enforcement vanished overnight, the territorial control would collapse, leading to a rapid reorganization of borders, a surge in Palestinian self-determination efforts, and a fundamental shift in regional power dynamics. The current arrangements are entirely dependent on this constraint.
% FOUNDING_PROBLEM: The existential threat to the State of Israel from hostile neighboring states and non-state actors, particularly after the 1967 Six-Day War, necessitating a defensive buffer and strategic depth.
% FOUNDING_PROBLEM_CORROBORATION: The State of Israel and its security establishment attest that the founding problem remains live, citing ongoing threats. However, the Palestinian Authority, international legal bodies, and many independent analysts argue that the problem has evolved or is used as a pretext for expansion, with the original security needs now largely met or superseded by political objectives. Corroboration from outside the benefiting parties is largely absent for the 'live' status of the original problem.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).

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
 *   The high extractiveness (0.85) reflects the significant resources and land extracted from the Palestinian population, justified by security claims. Suppression (0.92) is extremely high due to the active military and administrative enforcement required to maintain control and suppress resistance. The theater ratio (0.4) indicates that while genuine security concerns exist, a substantial portion of the justification and enforcement serves to maintain territorial control and expansion rather than purely defensive needs. Accessibility collapse is moderate (0.7) as alternatives for Palestinians (e.g., independent statehood, full self-determination) are severely constrained but not entirely eliminated from discourse. Resistance is high (0.8) due to ongoing conflict and international opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the State of Israel and Israeli settlers, this constraint is framed as a necessary defense mechanism (a 'rope' or even a 'mountain' of geopolitical reality). From the perspective of the Palestinian population and international law advocates, it is a clear 'snare' of occupation and extraction. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel and Israeli settlers are clear beneficiaries (d near 0.0) as they gain security, strategic depth, and territorial control. The Palestinian population and Palestinian Authority are direct victims (d near 1.0), bearing the costs of occupation, restricted movement, and loss of sovereignty. International law advocates are also victims (d near 0.8) as their principles are violated, though they do not suffer direct material extraction in the same way as the Palestinian population.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a prime candidate for mandatrophy. While initially framed as a temporary security measure post-1967, its persistence and expansion suggest that the 'mandate' of security necessity has either outlived its original function or has been reinterpreted to justify ongoing territorial control. The high and increasing extractiveness and suppression, coupled with a rising theater ratio, indicate that the constraint's function has drifted from pure defense to a more extractive and performative maintenance of control. The 'contested' status of the founding problem further supports this, as external corroboration for the 'live' status of the founding problem is weak.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine security necessity, or a justification for territorial expansion?',
    'Independent security assessments by neutral parties, historical analysis of security threats pre- and post-1967, and evaluation of alternative security arrangements (e.g., demilitarized zones, international guarantees).',
    'If primarily a justification for expansion, the constraint''s extractiveness and suppression are higher, and its claimed type shifts from a ''defensive rope'' (as claimed by some proponents) to a ''snare''. If genuinely security-driven, the extractiveness might be seen as a necessary cost of survival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'Distinguishing genuine security necessity from expansionist claims within the ''territorial_legitimacy'' kernel.').

omega_variable(
    sibling_reading_impact_partition,
    'How would adopting the ''partition_reading'' of territorial legitimacy alter the structural properties of this ''security_necessity_reading''?',
    'Analysis of legal frameworks and political outcomes if UN Resolution 181 (1948 borders) were universally adopted as the basis for legitimacy.',
    'The ''partition_reading'' would directly challenge the legitimacy of control over territories beyond 1948 borders, increasing the ''security_necessity_reading''s'' suppression and resistance metrics as it would require greater enforcement to maintain its claims against a widely accepted alternative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_partition, conceptual, 'Impact of the ''partition_reading'' on the ''security_necessity_reading''.').

omega_variable(
    sibling_reading_impact_indigenous_continuity,
    'How would adopting the ''indigenous_continuity_reading'' of territorial legitimacy alter the structural properties of this ''security_necessity_reading''?',
    'Analysis of legal frameworks and political outcomes if indigenous land claims and anti-colonial self-determination were universally adopted as the basis for legitimacy.',
    'The ''indigenous_continuity_reading'' would fundamentally delegitimize the ''security_necessity_reading''s'' claims to any territory based on post-1948 or post-1967 control, leading to a dramatic increase in resistance and a collapse of the ''security_necessity_reading''s'' claimed legitimacy, effectively reclassifying it as a pure ''snare'' or ''piton'' maintained solely by force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_indigenous_continuity, conceptual, 'Impact of the ''indigenous_continuity_reading'' on the ''security_necessity_reading''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy__security_necessity_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(terr_tr_t10, territorial_legitimacy__security_necessity_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(terr_tr_t20, territorial_legitimacy__security_necessity_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(terr_tr_t30, territorial_legitimacy__security_necessity_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy__security_necessity_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(terr_be_t10, territorial_legitimacy__security_necessity_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(terr_be_t20, territorial_legitimacy__security_necessity_reading, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(terr_be_t30, territorial_legitimacy__security_necessity_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy__security_necessity_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(terr_su_t10, territorial_legitimacy__security_necessity_reading, suppression_requirement, 10, 0.88).
narrative_ontology:measurement(terr_su_t20, territorial_legitimacy__security_necessity_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement(terr_su_t30, territorial_legitimacy__security_necessity_reading, suppression_requirement, 30, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'territorial_legitimacy' kernel, focusing on security necessity. It is distinct from the 'partition_reading' and 'indigenous_continuity_reading', which offer alternative bases for legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
