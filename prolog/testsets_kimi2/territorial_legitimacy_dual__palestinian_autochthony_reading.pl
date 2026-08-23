% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Palestinian Autochthony Legitimacy Reading
 *   domain: political theory/international relations/territorial sovereignty
 *
 * SUMMARY:
 *   The Palestinian autochthony reading grounds territorial legitimacy in
 *   continuous habitation prior to 1948, the ongoing injustice of the Nakba
 *   and displacement, and the non-negotiable right of return. It functions as
 *   a commitment system that coordinates Palestinian political identity and
 *   claims across diaspora and occupation, while structurally contesting
 *   Israeli state legitimacy and maintaining refugee populations in a state
 *   of political suspension. The reading is one of three structurally
 *   distinct readings of the territorial_legitimacy_dual kernel; it is
 *   presented here as a clean, epsilon-invariant constraint without internal
 *   contest description.
 *
 * KEY AGENTS:
 *   - palestinian_refugees: Primary target (powerless/identity_locked/global) â bear the direct costs of protracted displacement and statelessness
 *   - palestinian_political_institutions: Primary beneficiary/agenda_setter (institutional/constrained/national) â administer the legitimacy framework and collect international legitimacy
 *   - israeli_jewish_citizens: Secondary target (institutional/constrained/national) â bear security and legitimacy costs of the unresolved contest
 *   - host_countries: Excluded seat (institutional/constrained/regional) â bear hosting costs without voice in the legitimacy framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.76).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.72).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Autochthony Legitimacy Reading").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political theory/international relations/territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, '5708b123-7183-4105-a1b8-9c9b597c5185').
narrative_ontology:cs_kernel_codification('5708b123-7183-4105-a1b8-9c9b597c5185', distributed).
narrative_ontology:cs_authority_grounding('5708b123-7183-4105-a1b8-9c9b597c5185', lineage).
narrative_ontology:cs_interpretation_layer_present('5708b123-7183-4105-a1b8-9c9b597c5185').
narrative_ontology:cs_reading_relation('5708b123-7183-4105-a1b8-9c9b597c5185', territorial_legitimacy_dual__zionist_refuge_reading, forecloses).
narrative_ontology:cs_reading_relation('5708b123-7183-4105-a1b8-9c9b597c5185', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('5708b123-7183-4105-a1b8-9c9b597c5185', foundational, autochthony_as_legitimacy_ground).
narrative_ontology:cs_axiom_status(autochthony_as_legitimacy_ground, holdable).
narrative_ontology:cs_axiom_grounding('5708b123-7183-4105-a1b8-9c9b597c5185', autochthony_as_legitimacy_ground, deontological).
narrative_ontology:cs_axiom('5708b123-7183-4105-a1b8-9c9b597c5185', foundational, right_of_return_non_negotiable).
narrative_ontology:cs_axiom_status(right_of_return_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('5708b123-7183-4105-a1b8-9c9b597c5185', right_of_return_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('5708b123-7183-4105-a1b8-9c9b597c5185', historical_palestine_continuous_presence).
narrative_ontology:cs_drift_state('5708b123-7183-4105-a1b8-9c9b597c5185', contemporary_post_oslo_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('5708b123-7183-4105-a1b8-9c9b597c5185', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_political_institutions).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_jewish_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in refugee camps or as legally precarious minorities across the diaspora. Their political identity and international legal status are organized around the right of return; naturalization or local integration is ideologically suppressed as abandonment of the homeland. They bear the direct material and biographical costs of protracted displacement and statelessness.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees, payer,
    powerless, generational, identity_locked, global).

% Maintain the autochthony narrative and right of return as core political doctrine. They represent Palestinian claims internationally, administer refugee registration, and derive institutional legitimacy, foreign aid, and political purpose from perpetuating the framework. Abandoning these principles would delegitimize their own authority.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_political_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_political_institutions, beneficiary).

% Their state's legitimacy is structurally contested by the autochthony reading, which frames their collective presence as the fruit of displacement. They bear the military, security, and existential costs of an unresolved territorial legitimacy contest that treats their citizenship as provisional or illegitimate.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_jewish_citizens, payer,
    institutional, generational, constrained, national).

% Bear the demographic, fiscal, and political costs of hosting refugee populations for decades. Their interests in stability and refugee integration are structurally excluded from the autochthony framework, which prioritizes return over local settlement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, host_countries, excluded,
    institutional, generational, constrained, regional).

% Observe and mediate through UN resolutions and agencies. They provide the institutional architecture that registers refugee status and the right of return, but do not themselves bear the constraint's direct costs or legitimacy claims.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, international_diplomatic_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_political_institutions).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__palestinian_autochthony_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Palestinian political identity and collective claims across a globally dispersed refugee population and occupied territories by grounding legitimacy in shared historical continuity, displacement trauma, and territorial attachment.
% TRANSFER_FUNCTION: Moves international political attention, diplomatic resources, and institutional legitimacy toward Palestinian political institutions, while the direct costs of displacement and statelessness remain borne by refugees and the security costs of contested legitimacy by Israeli Jewish citizens.
% ABSENT_VOICES: Host-country governments and populations who bear the costs of protracted refugee presence; Palestinian refugees who favor local integration or naturalization over return; Israeli Jewish citizens whose own historical trauma and legitimacy claims are structurally excluded from the framework's moral accounting.
% DISAPPEARANCE_RATIONALE: The constraint organizes the political identity, international legal status, and diplomatic strategy of millions. Its disappearance would restructure refugee aspirations, host-country obligations, and the terms of Israeli-Palestinian negotiation.
% FOUNDING_PROBLEM: The dispersal of the Palestinian population in 1948 and the absence of a sovereign state to represent their interests, requiring a normative framework to maintain collective political existence and territorial claims across diaspora and occupation.
% FOUNDING_PROBLEM_CORROBORATION: Host-country governments and some refugee advocates attest that the founding problem has evolved into a protracted displacement crisis requiring integration or resettlement, not exclusively return; Israeli and some international analysts attest that the framework perpetuates the problem rather than solving it. Palestinian institutions corroborate the founding problem from within the beneficiary set.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.76, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.76) because the constraint perpetuates refugee statelessness as a political resource and denies legitimacy to an existing state. Suppression (0.72) is high because alternatives (local integration, two-state finality) are actively suppressed by the framework's normative structure. Theater ratio (0.60) is elevated: while the historical trauma is genuine, the institutional maintenance of the claim has outpaced practical realization, producing performative steadfastness (sumud) that substitutes for territorial recovery. Accessibility collapse is very high (0.80) because once the autochthony frame is adopted, alternatives collapse conceptually â naturalization becomes betrayal, partition becomes deprivation. Resistance is moderate (0.55) because Israeli state power resists the claim, and pragmatic Palestinian and host-country voices push for alternatives, but the identity-locked structure dampens internal dissent.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seat (Palestinian political institutions) experiences the constraint as legitimate coordination of a dispossessed people; the payer seats (refugees, Israeli Jewish citizens) experience it as ongoing extraction â the former through perpetuated statelessness, the latter through delegitimization and security burden. Host countries experience it as an imposed externality they cannot exit. The engine computes this divergence from structural data: identical global events read as liberation versus existential threat depending on seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian political institutions sit near the beneficiary end: they derive legitimacy, funding, and purpose from the framework. Palestinian refugees sit near the target end despite being the nominal subjects of the claim: their lives are suspended by the framework's operation, and their exit into integration is structurally blocked by identity-locking. Israeli Jewish citizens are explicit targets: the framework's core operation is to contest their state's legitimacy. Host countries are excluded from the directionality computation because they are not parties to the legitimacy claim, though they bear costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) by acknowledging the genuine coordination problem it solves: maintaining collective political identity and claims across a globally dispersed population lacking a state. It prevents mislabeling as pure coordination (rope) by acknowledging the asymmetric extraction: refugees pay through protracted displacement, and Israeli Jews pay through contested legitimacy. The mandate has not fully atrophied (not a piton) because the coordination function remains live, but the theater ratio indicates growing proxy-goal substitution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autochthony_naturalization_ambiguity,
    'Does the autochthony claim function as a constructed political argument dressed in natural-law garb, or as a genuinely emergent historical entitlement?',
    'Comparative analysis with other indigenous-land-return movements and their political construction versus self-organization.',
    'If purely constructed, the constraint''s extraction is politically contingent and mutable; if naturalized, it approaches mountain-like immunity to reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autochthony_naturalization_ambiguity, conceptual, 'Whether autochthony is a constructed False Summit or genuine historical entitlement').

omega_variable(
    refugee_integration_suppression,
    'Is the suppression of refugee integration structural (host-country exclusion laws) or internalized (identity-locked refusal of naturalization)?',
    'Post-exit trajectory analysis: if refugees who naturalize retain the autochthony framework or abandon it.',
    'If internalized, effective suppression exceeds structural measure; the constraint travels with the agent after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_integration_suppression, empirical, 'Structural vs internalized suppression mechanism for refugees').

omega_variable(
    coordination_extraction_separation,
    'Can the coordination function (maintaining Palestinian collective identity across dispersion) be separated from the extraction function (perpetuating refugee statelessness for political leverage)?',
    'Counterfactual analysis of identity-maintenance mechanisms that do not depend on return as exclusive remedy.',
    'If inseparable, measured extraction is partly the necessary cost of coordination; if separable, the excess is pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separation, conceptual, 'Whether coordination and extraction are structurally separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(terr_tr_t15, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(terr_tr_t30, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(terr_tr_t45, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement(terr_tr_t60, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(terr_tr_t76, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 76, 0.6).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(terr_be_t15, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(terr_be_t30, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(terr_be_t45, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 45, 0.72).
narrative_ontology:measurement(terr_be_t60, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 60, 0.74).
narrative_ontology:measurement(terr_be_t76, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 76, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(terr_su_t15, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(terr_su_t30, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(terr_su_t45, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 45, 0.7).
narrative_ontology:measurement(terr_su_t60, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(terr_su_t76, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 76, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the territorial_legitimacy_dual family. The kernel conflates distinct structurally separate claims: Zionist legitimacy grounded in persecution/divine promise/UN partition, Palestinian legitimacy grounded in autochthony/trauma/return, and a two-state compromise reading. Each has different epsilon values and victim/beneficiary structures. Decomposed per the Îµ-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
