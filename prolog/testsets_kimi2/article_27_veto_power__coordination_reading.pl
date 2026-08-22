% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__coordination_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: P5 Veto as Great-Power War Prevention (Coordination Reading)
 *   domain: international_relations/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the coordination reading of the
 *   article_27_veto_power kernel. Under this reading, the P5 veto in the UN
 *   Security Council is a necessary coordination mechanism that solved the
 *   collective-action problem of great-power concert after the League of
 *   Nations collapsed. By ensuring that no binding resolution can compel a
 *   nuclear-armed great power into unwanted military confrontation, the veto
 *   keeps the five most powerful states inside the institutional framework
 *   rather than driving them to withdraw or fight. All states are
 *   structurally beneficiaries because the systemic stability of the
 *   post-1945 order depends on avoiding great-power war. This reading is
 *   contested by the oligopoly reading (extraction) and the sovereignty
 *   reading (consent instantiation).
 *
 * KEY AGENTS:
 *   - p5_nuclear_states: Primary agenda-setter (institutional/global) â wields the veto to preserve autonomous military decision-making.
 *   - non_p5_member_states: Systemic beneficiary (moderate/national) â accepts constrained Council action in exchange for great-power war avoidance.
 *   - un_secretariat: Analytical observer (institutional/global) â administers Council procedure without veto power.
 *   - global_civil_society: Excluded voice (organized/global) â advocates for humanitarian action but lacks standing in the veto structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.22).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.25).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "P5 Veto as Great-Power War Prevention (Coordination Reading)").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, '49c05a3f-c661-4425-8983-9ece5b2a8dc4').
narrative_ontology:cs_kernel_codification('49c05a3f-c661-4425-8983-9ece5b2a8dc4', formalized).
narrative_ontology:cs_authority_grounding('49c05a3f-c661-4425-8983-9ece5b2a8dc4', lineage).
narrative_ontology:cs_interpretation_layer_present('49c05a3f-c661-4425-8983-9ece5b2a8dc4').
narrative_ontology:cs_reading_relation('49c05a3f-c661-4425-8983-9ece5b2a8dc4', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('49c05a3f-c661-4425-8983-9ece5b2a8dc4', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('49c05a3f-c661-4425-8983-9ece5b2a8dc4', foundational, unanimity_prevents_nuclear_escalation).
narrative_ontology:cs_axiom_status(unanimity_prevents_nuclear_escalation, holdable).
narrative_ontology:cs_axiom_grounding('49c05a3f-c661-4425-8983-9ece5b2a8dc4', unanimity_prevents_nuclear_escalation, empirically_contingent).
narrative_ontology:cs_axiom('49c05a3f-c661-4425-8983-9ece5b2a8dc4', foundational, hegemonic_consent_required_for_collective_security).
narrative_ontology:cs_axiom_status(hegemonic_consent_required_for_collective_security, holdable).
narrative_ontology:cs_axiom_grounding('49c05a3f-c661-4425-8983-9ece5b2a8dc4', hegemonic_consent_required_for_collective_security, instrumental).
narrative_ontology:cs_reference_frame('49c05a3f-c661-4425-8983-9ece5b2a8dc4', san_francisco_charter_concert).
narrative_ontology:cs_drift_state('49c05a3f-c661-4425-8983-9ece5b2a8dc4', contemporary_multipolar_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('49c05a3f-c661-4425-8983-9ece5b2a8dc4', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, p5_nuclear_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, non_p5_member_states).
narrative_ontology:constraint_vindicates(article_27_veto_power__coordination_reading, collective_security_with_unanimity).
narrative_ontology:constraint_vindicates(article_27_veto_power__coordination_reading, great_power_concert_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wield the Article 27 veto to block Security Council resolutions that would compel them into unwanted military confrontation or enforcement action. They are the procedural gatekeepers of binding Chapter VII decisions; their continued participation in the UN system is conditional on this autonomy.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, p5_nuclear_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__coordination_reading, p5_nuclear_states, beneficiary).

% Receive the non-excludable public good of systemic stability and reduced great-power war risk. They accept the constraint that Council action requires P5 unanimity, which frequently blocks intervention in regional crises, in exchange for the existential benefit of avoiding nuclear confrontation among the great powers.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, non_p5_member_states, beneficiary,
    moderate, generational, constrained, national).

% Administers Security Council procedure, convenes meetings, and drafts reports under Article 99, but cannot override or circumvent a veto. Occupies an analytical seat that documents the gap between Charter purposes and political outcomes without possessing enforcement autonomy.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, un_secretariat, observer,
    institutional, generational, analytical, global).

% Advocates for humanitarian intervention and accountability for mass atrocities, but has no standing in the state-centric veto structure. Their exclusion is constitutive of the arrangement: the veto privileges great-power consent over civilian protection claims.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, global_civil_society, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains a universal collective security system by solving the commitment problem that destroyed the League of Nations: great powers will not remain inside an institution that can bind them to military action against their will. The veto preserves P5 buy-in, making the UN a forum for concert rather than a trigger for defection or war.
% TRANSFER_FUNCTION: No material transfer between specific agents. The arrangement transfers the risk of nuclear escalation away from the state system by preserving each P5 state's sovereign control over military deployment. The diffuse cost of inactionâunresolved regional conflicts where the Council is paralyzedâis transferred to the general international community and to populations under threat.
% ABSENT_VOICES: Populations in humanitarian crises blocked by veto (e.g., Syria, Gaza, Ukraine) are structurally absent from the veto bargain. The ACT group and mid-rank reform advocates are present in UN corridors but lack formal agenda-setting power over Charter amendment, which requires P5 consent.
% DISAPPEARANCE_RATIONALE: Without the veto, the Council could issue binding resolutions against P5 interests, collapsing the great-power consensus that underpins the UN's security function. P5 states would likely withdraw or ignore the body, reverting to balance-of-power rivalry and raising nuclear confrontation risk. The post-1945 architecture of institutionalized great-power concert would unravel.
% FOUNDING_PROBLEM: How to construct a collective security organization that binds states enough to deter aggression without collapsing when great powers disagree, as the League of Nations did after being ignored by Japan, Italy, and Germany.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the Dumbarton Oaks and San Francisco conferences corroborate the founding intent. Structural-realist scholars and mid-rank powers at the 1945 conference (e.g., Australia, Canada) attested that a council without great-power unanimity would be ignored; contemporary IR scholars outside the P5 continue to argue that the problem of great-power war persists under multipolarity.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__coordination_reading, 0.22, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__coordination_reading_tests).
:- end_tests(article_27_veto_power__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the veto's primary effect is procedural delay and selective inaction, not resource transfer or rent extraction; the coordination benefit (avoided nuclear confrontation) is treated by this reading as outweighing the costs of blocked humanitarian action. Suppression is low-moderate (0.25) because alternatives to the veto (Charter amendment, Uniting for Peace, ad hoc coalitions) persist and are periodically used, though the procedural bar itself is hard to remove. Theater ratio is low (0.16) because the veto is functionally load-bearing in this reading, not performative. Accessibility collapse is moderate (0.55): alternatives to unanimity-based great-power security governance exist in theory but have not proven viable at planetary scale since 1945. Resistance is low-moderate (0.25) because while reform rhetoric is constant, few states withdraw from the UN or mount sustained structural challenge to the veto itself.
 *
 * PERSPECTIVAL GAP:
 *   From the P5 seat, the veto is necessary institutional autonomy; from the non-P5 seat, it is the price of systemic stability. From the excluded humanitarian seat, the same procedural bar reads as lethal obstruction of collective security. The engine will compute divergent per-seat classifications: the agenda-setter/beneficiary seats trend toward Rope, while excluded seats experiencing veto-induced inaction on mass atrocities may compute higher extraction. This divergence is the signal the framework is designed to capture, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Both P5 and non-P5 states are declared beneficiaries in this reading because the coordination output (great-power war avoidance) is a non-excludable public good. The P5 states additionally hold agenda-setting power, which gives them low directionality toward the constraint (they subsidize themselves with autonomy). Non-P5 states have moderate power and constrained exit; their directionality is also low because they benefit from systemic stability. No victim class is declared because the reading treats the cost of inaction as diffuse and outweighed. Global civil society is excluded rather than victimized in the structural accounting because the reading treats their humanitarian claims as secondary to the existential risk of nuclear war.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as Rope rather than Snare or Tangled Rope prevents the mandatrophy error of treating all P5 privilege as extraction. The coordination reading insists that a genuine collective-action problem â sustaining great-power consent to a universal security system â is being solved. If the veto were pure extraction (oligopoly reading), we would expect concentrated gains accruing to the P5 and identifiable victims bearing structured costs; this reading asserts the gains are diffuse stability and the costs are diffuse inaction. The classification is falsifiable: if empirical analysis shows the veto is redundant to nuclear deterrence or that P5 states capture concentrated authority rents from it, the coordination reading collapses toward Piton or Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the P5 veto best understood as a coordination mechanism for systemic stability, or as an oligopolistic extraction structure that uses Charter immutability to entrench power?',
    'Comparative historical analysis of counterfactual UN structures without the veto (e.g., League of Nations experience) versus institutional-evolution metrics of reform blockage and authority-rent concentration.',
    'If oligopolistic extraction dominates, the constraint reclassifies toward Tangled Rope or Snare; if coordination dominates, it remains Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Core ambiguity between coordination and extraction readings of the veto kernel.').

omega_variable(
    nuclear_deterrence_redundancy,
    'Does mutual nuclear deterrence independently prevent great-power war, rendering the veto redundant coordination theater?',
    'Statistical and case-study analysis of nuclear-era crises (Cuban Missile Crisis, Berlin, Kashmir) to isolate whether institutional veto constraints or deterrence dynamics primarily prevented escalation.',
    'If redundancy is high, the veto''s coordination value collapses toward Piton (theater without function); if complementary, the coordination claim strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_deterrence_redundancy, empirical, 'Whether nuclear deterrence makes the veto functionally redundant.').

omega_variable(
    reform_feasibility,
    'Would Charter amendment to modify the veto be technically feasible and adopted if the coordination function were genuinely obsolete?',
    'Observing amendment attempts (e.g., Ezulwini Consensus) and P5 resistance patterns; if coordination were the true function, sunset or reform would be possible when the strategic problem changes.',
    'If reform is blocked despite changed strategic context, the persistence mechanism is extraction or inertia, not coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_feasibility, empirical, 'Whether veto reform blockage indicates extraction rather than coordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 0, 79).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a27_coord_tr_t0, article_27_veto_power__coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(a27_coord_tr_t20, article_27_veto_power__coordination_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(a27_coord_tr_t40, article_27_veto_power__coordination_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(a27_coord_tr_t60, article_27_veto_power__coordination_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement(a27_coord_tr_t79, article_27_veto_power__coordination_reading, theater_ratio, 79, 0.16).

% Extraction over time
narrative_ontology:measurement(a27_coord_be_t0, article_27_veto_power__coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(a27_coord_be_t20, article_27_veto_power__coordination_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(a27_coord_be_t40, article_27_veto_power__coordination_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(a27_coord_be_t60, article_27_veto_power__coordination_reading, base_extractiveness, 60, 0.2).
narrative_ontology:measurement(a27_coord_be_t79, article_27_veto_power__coordination_reading, base_extractiveness, 79, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article_27_veto_power__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__oligopoly_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the article_27_veto_power kernel. The natural-language label 'P5 veto' conflates structurally distinct claims: a coordination function (this file), an authority oligopoly (oligopoly_reading), and a sovereignty instantiation (sovereignty_reading). Each has distinct epsilon, stakeholders, and classification. They form a constraint family linked by shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
