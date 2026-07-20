% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: Nuclear Winnability Rhetorical Contraction
 *   domain: strategic/nuclear deterrence/international relations
 *
 * SUMMARY:
 *   In the nuclear age, the concept of 'winnability' underwent a dual-layer
 *   contraction: public and scholarly discourse treats nuclear war as
 *   categorically unthinkable and unwinnable, producing a powerful rhetorical
 *   taboo, while classified operational planning continues to treat limited
 *   nuclear victory as constrained-but-reachable. This structure benefits
 *   strategic planners by granting them operational flexibility and
 *   insulation from democratic accountability, while imposing costs on
 *   legislative overseers and publics who cannot scrutinize the plans made in
 *   their name. This constraint instantiates the rhetorical_contraction
 *   reading of the war_winnability_post_1945 kernel.
 *
 * KEY AGENTS:
 *   - Strategic planners: agenda_setter/beneficiary (institutional/global/constrained) â administer the taboo and classified planning, capture operational flexibility
 *   - Legislative overseers: payer (institutional/national/constrained) â bear the cost of lost oversight
 *   - General public: payer/excluded (powerless/national/trapped) â bear existential risk and discursive exclusion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.71).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.78).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.71).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.64).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "Nuclear Winnability Rhetorical Contraction").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic/nuclear deterrence/international relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, 'abc29f5c-6111-45df-b862-1e5c8e691711').
narrative_ontology:cs_kernel_codification('abc29f5c-6111-45df-b862-1e5c8e691711', distributed).
narrative_ontology:cs_authority_grounding('abc29f5c-6111-45df-b862-1e5c8e691711', extraction).
narrative_ontology:cs_interpretation_layer_present('abc29f5c-6111-45df-b862-1e5c8e691711').
narrative_ontology:cs_reading_relation('abc29f5c-6111-45df-b862-1e5c8e691711', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('abc29f5c-6111-45df-b862-1e5c8e691711', war_winnability_post_1945__countervailing_thinkable, influences).
narrative_ontology:cs_axiom('abc29f5c-6111-45df-b862-1e5c8e691711', foundational, operational_planning_survives_rhetorical_taboo).
narrative_ontology:cs_axiom_status(operational_planning_survives_rhetorical_taboo, holdable).
narrative_ontology:cs_axiom_grounding('abc29f5c-6111-45df-b862-1e5c8e691711', operational_planning_survives_rhetorical_taboo, empirically_contingent).
narrative_ontology:cs_axiom('abc29f5c-6111-45df-b862-1e5c8e691711', secondary, strategic_secrecy_as_legitimate_governance).
narrative_ontology:cs_axiom_status(strategic_secrecy_as_legitimate_governance, holdable).
narrative_ontology:cs_axiom_grounding('abc29f5c-6111-45df-b862-1e5c8e691711', strategic_secrecy_as_legitimate_governance, conventional).
narrative_ontology:cs_reference_frame('abc29f5c-6111-45df-b862-1e5c8e691711', dual_layer_deterrence_governance).
narrative_ontology:cs_drift_state('abc29f5c-6111-45df-b862-1e5c8e691711', contemporary_strategic_competition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('abc29f5c-6111-45df-b862-1e5c8e691711', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, legislative_overseers).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer classified nuclear war plans and force posture. They enforce the rhetorical taboo on winnability in public discourse while continuing to treat limited nuclear victory as operationally constrained-but-reachable in secret planning. They gain operational flexibility and insulation from democratic accountability because the taboo shields their work from scrutiny. Exit from this apparatus is possible but carries high professional and identity costs.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_planners, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, strategic_planners, beneficiary).

% Constitutionally responsible for oversight of military and nuclear policy, but structurally blocked by classification regimes and the discursive taboo that makes serious public inquiry into winnability politically radioactive. They bear the cost of diminished accountability and the risk of policies they cannot fully audit or challenge.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, legislative_overseers, payer,
    institutional, biographical, constrained, national).

% Live under the existential risk of nuclear policies they did not choose and cannot influence, while being systematically excluded from the classified discourse that determines those policies. The rhetorical taboo prevents them from even articulating the question of winnability in public forums, effectively trapping them in a deterrence architecture they cannot exit.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, general_public, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, general_public, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__rhetorical_contraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes nuclear deterrence by establishing a shared transnational discursive norm that makes nuclear use publicly illegitimate and unthinkable, reducing the risk of accidental escalation and lowering the salience of first-use options in political discourse.
% TRANSFER_FUNCTION: Moves operational planning autonomy and freedom from public scrutiny to the classified strategic apparatus; moves the costs of opacity, democratic deficit, and existential risk to legislative overseers and publics who cannot scrutinize the plans made in their name.
% ABSENT_VOICES: Anti-nuclear activists, unclassified scholars who question the taboo, and elected representatives without security clearances are excluded from the planning conversation. Strategists who argue openly that nuclear war is winnable face professional ostracism and discursive penalties.
% DISAPPEARANCE_RATIONALE: If the rhetorical taboo vanished while operational planning persisted in secret, deterrence stability would erode as nuclear use became thinkable in public discourse, or democratic accountability would increase as the gap between rhetoric and operations closed. Either way, the current civil-military arrangement depends on the dual-layer structure.
% FOUNDING_PROBLEM: The advent of nuclear weapons and early Cold War crises created a need to prevent nuclear war while simultaneously preparing for it; unconstrained public discussion of winnability appeared to accelerate arms races and lower the threshold for first use during moments of superpower tension.
% FOUNDING_PROBLEM_CORROBORATION: Retired strategists and arms-control scholars attest the problem persists as a genuine coordination challenge; democratic-theory scholars and transparency advocates attest the problem has mutated into a justification for secrecy, corroborating the shifted-function reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.71, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71) is high because the constraint systematically extracts democratic accountability and public scrutiny from the nuclear planning process, transferring autonomy to a classified apparatus. Suppression (0.78) is higher because the taboo's persistence depends on active discursive enforcement: classification, career penalties for transgression, and the delegitimation of winnability talk. Theater ratio (0.64) reflects the large and growing gap between the performative public taboo and the continuing operational reality of strike planning. Accessibility collapse (0.58) captures the partial closure of alternatives: while isolated whistleblowers and scholars occasionally breach the taboo, open democratic deliberation on nuclear strategy remains structurally blocked. Resistance (0.35) is moderate: arms-control advocates and occasional leaks challenge the opacity, but the national-security consensus broadly enforces the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the planner's seat, the arrangement is necessary coordination: secrecy prevents destabilizing leaks and preserves credible deterrence. From the overseer and public seats, the same structure reads as active extraction of democratic control. The engine computes this divergence from the structural dataâbeneficiary declarations, trapped exit, and institutional power asymmetryâwithout the claim adjudicating which seat is 'correct'.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic planners are the primary beneficiaries: they collect operational freedom and avoidance of scrutiny (low directionality). Legislative overseers and the general public are the targets: they pay in lost accountability and unacknowledged risk (high directionality). The dual-positioned nature of the publicâboth payer and excludedâamplifies their effective extraction because they cannot exit the nuclear state or the discourse that traps them.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not a piton because the function has not atrophied: operational planning is vigorous and the taboo is actively enforced, not merely theatrically maintained. It is not a snare because the coordination function (deterrence stability through shared unthinkability) is genuine and not merely cover. The Tangled Rope classification captures the coexistence of real coordination and asymmetric extraction. Mandatrophy would only apply if the founding problem (preventing nuclear war through stable deterrence) were dead, but it remains contested; the structure persists because both the coordination and extraction functions are live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_rhetoric_gap,
    'Does the persistence of operational winnability planning beneath a rhetorical taboo represent a necessary secrecy function for deterrence stability, or an extractive evasion of democratic accountability?',
    'Comparative analysis of nuclear command-and-control transparency across allied states; evaluation of whether declassification of historical plans reveals stabilization or embarrassment.',
    'If secrecy is necessary for stability, the constraint is closer to a rope or scaffold; if accountability erosion is the primary effect, it confirms tangled_rope or snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_rhetoric_gap, conceptual, 'Whether the operational-rhetorical gap serves coordination or extraction').

omega_variable(
    taboo_internalization,
    'Is the winnability taboo enforced primarily through structural barriers (classification, institutional incentives) or through internalized belief among strategists and publics?',
    'Discourse analysis of strategic-community speech acts over time; exit interviews with former planners; observation of whether taboo persists after structural barriers are removed.',
    'If internalized, suppression is higher than structural measures suggest and the constraint''s persistence is more robust; if purely structural, reform is cheaper and resistance more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_internalization, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    kernel_reading_identity,
    'This constraint is the rhetorical_contraction reading of kernel war_winnability_post_1945. How would the structural classification change if the deterrence_unthinkable reading were adopted instead?',
    'Evaluate whether the operational planning layer would be read as residual irrationality (piton) rather than active extraction, and whether the beneficiary-victim structure would dissolve.',
    'Under deterrence_unthinkable, the operational layer would likely be classified as atrophied or irrational; under rhetorical_contraction, it is structurally central. This determines whether the constraint is tangled_rope or piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committer framing ambiguity between kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0, 0.1).
narrative_ontology:measurement(war__tr_t17, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 17, 0.3).
narrative_ontology:measurement(war__tr_t30, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 30, 0.45).
narrative_ontology:measurement(war__tr_t38, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 38, 0.55).
narrative_ontology:measurement(war__tr_t46, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 46, 0.6).
narrative_ontology:measurement(war__tr_t57, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 57, 0.62).
narrative_ontology:measurement(war__tr_t77, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 77, 0.63).
narrative_ontology:measurement(war__tr_t80, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 80, 0.64).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(war__be_t17, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 17, 0.45).
narrative_ontology:measurement(war__be_t30, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(war__be_t38, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 38, 0.65).
narrative_ontology:measurement(war__be_t46, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 46, 0.68).
narrative_ontology:measurement(war__be_t57, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 57, 0.7).
narrative_ontology:measurement(war__be_t77, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 77, 0.71).
narrative_ontology:measurement(war__be_t80, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 80, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(war__su_t17, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 17, 0.5).
narrative_ontology:measurement(war__su_t30, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(war__su_t38, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 38, 0.72).
narrative_ontology:measurement(war__su_t46, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 46, 0.75).
narrative_ontology:measurement(war__su_t57, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 57, 0.77).
narrative_ontology:measurement(war__su_t77, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 77, 0.78).
narrative_ontology:measurement(war__su_t80, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 80, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, countervailing_thinkable).

% DUAL FORMULATION NOTE:
% The war_winnability_post_1945 kernel decomposes into three structurally distinct constraints. This reading (rhetorical_contraction) treats the dual-layer gap as primary; the deterrence_unthinkable reading treats the taboo as reflecting objective strategic reality; the countervailing_thinkable reading treats operational reachability as primary. They form a constraint family linked by shared historical referent but divergent epsilon and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
