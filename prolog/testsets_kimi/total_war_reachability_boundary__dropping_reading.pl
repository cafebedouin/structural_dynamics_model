% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__dropping_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Total War Reachability Boundary â Dropping Reading
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint instantiates the dropping_reading of the
 *   total_war_reachability_boundary kernel. It holds that total war between
 *   great powers has dropped in probability since the mid-20th century but
 *   remains structurally reachable; deterrence operates as a coordination
 *   equilibrium (a rope, not a mountain) that must be actively maintained.
 *   The constraint is experienced asymmetrically: nuclear weapon states and
 *   their defense establishments gain strategic stability and extended
 *   deterrence credibility, while civilian populations under nuclear threat
 *   bear the existential risk of failure. The claim that deterrence is a
 *   coordination game rather than an inevitable structural feature frames the
 *   arrangement as solveable collective action with defection risk, while the
 *   tangled_rope classification captures the simultaneous coordination
 *   function and asymmetric risk imposition.
 *
 * KEY AGENTS:
 *   - Nuclear weapon states (institutional/constrained): administer deterrence posture, gain credibility and strategic stability
 *   - Civilian populations (powerless/trapped): bear existential risk under nuclear arsenals with no individual exit
 *   - Defense establishments (organized/identity_locked): gain mission, budget, and legitimacy from deterrence requirements
 *   - Non-nuclear weapon states (moderate/constrained): excluded from deterrence planning but subject to extended deterrence and potential fallout
 *   - Strategic studies community (moderate/analytical): interprets and legitimizes the equilibrium
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.62).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.58).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Total War Reachability Boundary â Dropping Reading").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, '4e4d0044-f44c-402e-a3f3-865c90e668d4').
narrative_ontology:cs_kernel_codification('4e4d0044-f44c-402e-a3f3-865c90e668d4', distributed).
narrative_ontology:cs_authority_grounding('4e4d0044-f44c-402e-a3f3-865c90e668d4', expertise).
narrative_ontology:cs_interpretation_layer_present('4e4d0044-f44c-402e-a3f3-865c90e668d4').
narrative_ontology:cs_reading_relation('4e4d0044-f44c-402e-a3f3-865c90e668d4', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e4d0044-f44c-402e-a3f3-865c90e668d4', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('4e4d0044-f44c-402e-a3f3-865c90e668d4', foundational, deterrence_as_coordination_equilibrium).
narrative_ontology:cs_axiom_status(deterrence_as_coordination_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('4e4d0044-f44c-402e-a3f3-865c90e668d4', deterrence_as_coordination_equilibrium, empirically_contingent).
narrative_ontology:cs_axiom('4e4d0044-f44c-402e-a3f3-865c90e668d4', foundational, persistent_reachability_despite_probability_drop).
narrative_ontology:cs_axiom_status(persistent_reachability_despite_probability_drop, holdable).
narrative_ontology:cs_axiom_grounding('4e4d0044-f44c-402e-a3f3-865c90e668d4', persistent_reachability_despite_probability_drop, empirically_contingent).
narrative_ontology:cs_reference_frame('4e4d0044-f44c-402e-a3f3-865c90e668d4', bipolar_deterrence_stability).
narrative_ontology:cs_drift_state('4e4d0044-f44c-402e-a3f3-865c90e668d4', multipolar_technological_disruption, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4e4d0044-f44c-402e-a3f3-865c90e668d4', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, defense_establishments).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, civilian_populations).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__dropping_reading, deterrence_equilibrium_theory).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__dropping_reading, mutual_assured_destruction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer nuclear postures, command-and-control systems, and extended deterrence alliances. They gain strategic stability and geopolitical credibility from the equilibrium. Disarmament is theoretically possible but strategically constrained by relative-gain fears and alliance commitments.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states, beneficiary).

% Live under the existential risk of nuclear use without individual or collective exit from the deterrence relationship. They bear the costs of the equilibriumâpotential annihilationâwhile having no seat in the strategic forums that set nuclear posture.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, civilian_populations, payer,
    powerless, civilizational, trapped, global).

% Derive mission, budget, and professional legitimacy from the maintenance of deterrence capabilities. Their organizational identity is fused with the nuclear mission; exit from the constraint would mean institutional dissolution or radical mission redefinition.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, defense_establishments, beneficiary,
    organized, biographical, identity_locked, global).

% Are directly affected by extended deterrence and potential fallout but are structurally excluded from nuclear planning forums. Their security is governed by commitments made without their participation, and their alternatives are constrained by alliance dependence or isolation.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, non_nuclear_weapon_states, excluded,
    moderate, generational, constrained, global).

% Would object to the risk-transfer architecture on humanitarian grounds but are kept out of strategic planning processes. Their proposals are institutionally marginalized by the nuclear states and the defense establishments that set posture.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, disarmament_advocates, excluded,
    moderate, generational, constrained, global).

% Analyzes and models the deterrence equilibrium, producing the interpretive framework through which the constraint is understood. They neither collect deterrence credibility nor bear the existential risk directly.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, strategic_studies_community, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__dropping_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates nuclear-armed states to avoid total war by making the cost of defection exceed the benefit of aggression, solving the commitment problem of restraint in an anarchic international system.
% TRANSFER_FUNCTION: Moves existential security risk from nuclear weapon states and defense establishments (who gain strategic stability and deterrence credibility) to civilian populations (who bear the risk of annihilation if the equilibrium fails).
% ABSENT_VOICES: Civilian populations under nuclear threat have no direct representation in deterrence planning; disarmament advocates and non-nuclear weapon states are structurally excluded from the strategic forums where nuclear posture is set, despite being directly affected by failure.
% DISAPPEARANCE_RATIONALE: If the total war reachability boundary dissolvedâif total war became genuinely impossible or irrelevantâthe entire architecture of nuclear strategy, extended deterrence alliances, defense budgeting, and great-power bargaining would reorganize. States would abandon counterforce postures, alliance commitments would shift, and the defense establishments' mission would collapse.
% FOUNDING_PROBLEM: How to prevent total war between nuclear-armed powers in an anarchic international system where no superior authority can guarantee restraint or punish defection.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear weapon states and defense establishments attest the problem remains live, citing rivalries and proliferation. Non-nuclear states, disarmament advocates, and independent security scholars outside the benefiting parties attest that the problem is partially solved by the nuclear revolution itself and that the arrangement persists as much for status and budgetary reasons as for genuine security; the humanitarian initiative and the Treaty on the Prohibition of Nuclear Weapons represent external corroboration of a shifted-problem reading.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial existential risk transferred to civilian populations to secure state-level strategic stability; it is not maximal because the coordination function (avoiding total war) is genuine. Suppression (0.58) captures the active maintenance required: nuclear modernization, alliance discipline, and exclusion of disarmament alternatives from strategic forums. Theater_ratio (0.48) has risen over the interval as signaling and posture became partially decoupled from actual warfighting needs, especially in the post-Cold War period. Accessibility_collapse (0.45) is moderate: general nuclear disarmament remains conceptually available but is institutionally marginalized. Resistance (0.40) reflects sustained disarmament advocacy and the Treaty on the Prohibition of Nuclear Weapons, which the nuclear states actively resist.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear weapon state seat, the constraint is a hard-won coordination equilibrium that prevents total war; from the civilian population seat, it is an involuntary risk transfer that extracts existential security. The defense establishment experiences it as professional identity and mission, while non-nuclear states experience exclusion from the rule-setting that governs their security. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states are declared beneficiaries (low d) because the constraint subsidizes their strategic position; civilian populations are declared victims (high d) because the constraint extracts existential security from them. Defense establishments are secondary beneficiaries whose identity-locked exit reinforces low directionality. Non-nuclear states and disarmament advocates sit between: they do not pay the direct cost of hosting arsenals but are excluded from the deterrence conversation that shapes their security environment.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by separating the coordination function (preventing total war through mutual deterrence) from the extraction mechanism (imposing existential risk on non-consenting populations). A pure rope reading would miss the asymmetric risk distribution; a pure snare reading would miss the genuine collective-action problem solved by the equilibrium. The tangled_rope classification captures both: the coordination is real, but so is the coercion required to maintain it and the victimization of populations who had no seat at its founding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status,
    'Does the dropping_reading''s coordination framing survive contact with multipolar technological disruption, or does it collapse into contingent_reachability?',
    'Track whether emerging technologies (hypersonics, AI-enabled command and control) are absorbed into the existing deterrence framework or fracture the equilibrium assumption.',
    'If technology fractures the equilibrium, the dropping_reading''s rope classification weakens and the constraint shifts toward contingent_reachability or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_status, conceptual, 'Whether the dropping reading''s equilibrium framing endures technological change').

omega_variable(
    coordination_extraction_boundary,
    'Is the deterrence equilibrium genuinely separable from the risk it imposes on civilians, or is the coordination function structurally fused to population targeting?',
    'Historical case analysis: can counterforce postures be maintained without targeting population centers, and does arms-control history show separability?',
    'If fused, the coordination story is cover for a structural snare; if separable, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether deterrence coordination is separable from population risk transfer').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (alliance discipline, arsenal modernization, institutional exclusion) or internalized (populations accepting nuclear risk as normal, fatalism about disarmament)?',
    'Post-crisis suppression trajectory: if resistance to nuclear posture collapses after a near-miss without structural change, suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measureâpopulations carry the constraint with them even when external enforcement weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 0, 63).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twrbd_tr_t0, total_war_reachability_boundary__dropping_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(twrbd_tr_t12, total_war_reachability_boundary__dropping_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(twrbd_tr_t25, total_war_reachability_boundary__dropping_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(twrbd_tr_t37, total_war_reachability_boundary__dropping_reading, theater_ratio, 37, 0.4).
narrative_ontology:measurement(twrbd_tr_t50, total_war_reachability_boundary__dropping_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(twrbd_tr_t63, total_war_reachability_boundary__dropping_reading, theater_ratio, 63, 0.48).

% Extraction over time
narrative_ontology:measurement(twrbd_be_t0, total_war_reachability_boundary__dropping_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(twrbd_be_t12, total_war_reachability_boundary__dropping_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(twrbd_be_t25, total_war_reachability_boundary__dropping_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(twrbd_be_t37, total_war_reachability_boundary__dropping_reading, base_extractiveness, 37, 0.45).
narrative_ontology:measurement(twrbd_be_t50, total_war_reachability_boundary__dropping_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(twrbd_be_t63, total_war_reachability_boundary__dropping_reading, base_extractiveness, 63, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(twrbd_su_t0, total_war_reachability_boundary__dropping_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(twrbd_su_t12, total_war_reachability_boundary__dropping_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(twrbd_su_t25, total_war_reachability_boundary__dropping_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(twrbd_su_t37, total_war_reachability_boundary__dropping_reading, suppression_requirement, 37, 0.35).
narrative_ontology:measurement(twrbd_su_t50, total_war_reachability_boundary__dropping_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement(twrbd_su_t63, total_war_reachability_boundary__dropping_reading, suppression_requirement, 63, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the total_war_reachability_boundary kernel. The dropping_reading holds that total war probability has dropped but reachability persists as a coordination equilibrium. Sibling readings differ on whether total war left the feasible set entirely (contraction_reading) or whether reachability is technology-contingent (contingent_reachability_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
