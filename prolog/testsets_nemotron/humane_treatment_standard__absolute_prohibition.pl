% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Absolute Prohibition of Torture and Degrading Treatment under Common Article 3
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   Common Article 3 of the Geneva Conventions establishes that persons
 *   taking no active part in hostilities 'shall in all circumstances be
 *   treated humanely' and prohibits 'violence to life and person, in
 *   particular murder of all kinds, mutilation, cruel treatment and torture'
 *   as well as 'outrages upon personal dignity, in particular humiliating and
 *   degrading treatment.' This absolute prohibition reading holds that these
 *   standards are non-derogable peremptory norms (jus cogens) from which no
 *   security exception, military necessity, or public emergency can permit
 *   departure. The constraint operates as a mountain: it presents as a fixed
 *   structural feature of the international legal order, with near-total
 *   accessibility collapse (alternatives are logically excluded by the norm's
 *   peremptory character) and negligible resistance from the legal framework
 *   itself. The low extractiveness (0.15) reflects the constraint's operation
 *   as a protective barrier rather than an extractive mechanism — the
 *   'extraction' measured is the cost to states of forgoing interrogation
 *   methods they might prefer, assessed from the reading's own lights.
 *
 * KEY AGENTS:
 *   - detainees_in_non_international_armed_conflict: Primary beneficiary (protected class) — bears no extraction, receives the constraint's protection
 *   - state_interrogation_apparatus: Primary target (constrained actor) — bears the cost of forgone methods, exit options constrained by legal obligation
 *   - international_humanitarian_law_institutions: Agenda setter / guardian — administers the norm through monitoring, reporting, and judicial mechanisms
 *   - non_state_armed_groups: Bound party — formally bound by CA3 but with highly variable compliance incentives
 *   - human_rights_advocacy_networks: Observer / enforcement catalyst — mobilizes the norm's protective function through documentation and litigation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.15).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.05).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.15).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, mountain).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Absolute Prohibition of Torture and Degrading Treatment under Common Article 3").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/state_security/human_rights").

domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, '2faa0cae-0dc7-4595-839f-6bd919e99f39').
narrative_ontology:cs_kernel_codification('2faa0cae-0dc7-4595-839f-6bd919e99f39', fixed_text).
narrative_ontology:cs_authority_grounding('2faa0cae-0dc7-4595-839f-6bd919e99f39', lineage).
narrative_ontology:cs_interpretation_layer_present('2faa0cae-0dc7-4595-839f-6bd919e99f39').
narrative_ontology:cs_reading_relation('2faa0cae-0dc7-4595-839f-6bd919e99f39', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('2faa0cae-0dc7-4595-839f-6bd919e99f39', humane_treatment_standard__proportionality_balancing, coexists_with).
narrative_ontology:cs_axiom('2faa0cae-0dc7-4595-839f-6bd919e99f39', foundational, torture_categorically_prohibited_under_all_circumstances).
narrative_ontology:cs_axiom_status(torture_categorically_prohibited_under_all_circumstances, holdable).
narrative_ontology:cs_axiom_grounding('2faa0cae-0dc7-4595-839f-6bd919e99f39', torture_categorically_prohibited_under_all_circumstances, deontological).
narrative_ontology:cs_axiom('2faa0cae-0dc7-4595-839f-6bd919e99f39', foundational, humane_treatment_is_non_derogable_peremptory_norm).
narrative_ontology:cs_axiom_status(humane_treatment_is_non_derogable_peremptory_norm, holdable).
narrative_ontology:cs_axiom_grounding('2faa0cae-0dc7-4595-839f-6bd919e99f39', humane_treatment_is_non_derogable_peremptory_norm, deontological).
narrative_ontology:cs_reference_frame('2faa0cae-0dc7-4595-839f-6bd919e99f39', common_article_3_as_peremptory_floor).
narrative_ontology:cs_drift_state('2faa0cae-0dc7-4595-839f-6bd919e99f39', post_9_11_enhanced_interrogation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2faa0cae-0dc7-4595-839f-6bd919e99f39', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detainees_in_non_international_armed_conflict).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, civilian_populations_in_conflict_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, state_interrogation_apparatus).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, non_state_armed_groups).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, non_derogability_of_humane_treatment).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, absolute_prohibition_of_torture).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, human_dignity_as_peremptory_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons detained in non-international armed conflicts who receive the absolute protection of Common Article 3. They cannot exit the protected status — it attaches to their legal personhood in the conflict. The constraint operates as a shield against torture, cruel treatment, and outrages upon personal dignity. They bear no costs and collect no rents; they are the constraint's raison d'être.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detainees_in_non_international_armed_conflict, beneficiary,
    powerless, biographical, identity_locked, global).

% Civilian populations in conflict zones who benefit indirectly from the norm's general civilizing effect and directly when they become detainees. The constraint's existence shapes state behavior even before detention occurs. Like detainees, they cannot exit the protection — it is a status conferred by the legal order.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, civilian_populations_in_conflict_zones, beneficiary,
    powerless, biographical, identity_locked, global).

% State intelligence, military, and law enforcement agencies that conduct interrogations. They bear the opportunity cost of forgone interrogation methods (coercive techniques, enhanced interrogation) that the absolute prohibition forbids. Their exit is constrained: they remain bound by treaty obligation and jus cogens; withdrawal from the Geneva Conventions is legally contested and politically costly. The constraint extracts from them the option space of 'effective but illegal' methods.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, state_interrogation_apparatus, payer,
    institutional, generational, constrained, global).

% The ICRC, UN human rights treaty bodies, international criminal tribunals, and domestic courts that administer, monitor, and adjudicate the norm. They set the interpretive agenda, define the threshold of 'degrading treatment,' and enforce through reporting, litigation, and prosecution. They collect institutional legitimacy and operational mandate from the constraint's authority but do not extract rents from detainees or states.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_humanitarian_law_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Organized non-state armed groups formally bound by Common Article 3. They bear the same prohibition on torture and degrading treatment but often lack the institutional capacity to implement compliant detention practices. Their exit is trapped: they cannot opt out of CA3 applicability (it binds all parties to NIAC), but they frequently lack the resources to comply, creating a compliance gap that is not extractive in origin but structural.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, non_state_armed_groups, payer,
    organized, biographical, trapped, global).

% NGOs, journalists, legal clinics, and activist networks that document violations, litigate cases, and mobilize the norm's protective function. They neither bear costs nor collect rents from the constraint's operation; they use it as a legal and moral tool. Their analytical exit means they can engage or disengage from the framework without structural penalty.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, human_rights_advocacy_networks, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__absolute_prohibition, diffuse).
narrative_ontology:fixing_cost_class(humane_treatment_standard__absolute_prohibition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the minimum humanitarian baseline in non-international armed conflicts: establishes a universal floor of humane treatment that all parties must observe regardless of the conflict's character, the detainee's status, or security imperatives. Solves the coordination problem of 'what treatment is never permissible' across asymmetric parties with divergent incentives.
% TRANSFER_FUNCTION: Transfers the option to use coercive interrogation methods from state and non-state interrogators to the protected persons — the constraint removes certain methods from the available option set and vests the corresponding protection in the detainee. No material resource moves; the transfer is of legal permission and physical security.
% ABSENT_VOICES: Victims of torture in conflicts where the norm is violated but not adjudicated — their voices are absent because the violation occurs in secrecy, and they may be dead, disappeared, or silenced. Also absent: states that have not ratified the Geneva Conventions (though all 196 states have), and future generations who will inherit the norm's strength or erosion.
% DISAPPEARANCE_RATIONALE: If the absolute prohibition vanished overnight, states and non-state actors would immediately expand interrogation method sets; legal protections for detainees would collapse to domestic law only (highly variable); the jus cogens status of the prohibition would be lost, removing the highest legal barrier to torture. The world of detention practices would rearrange fundamentally.
% FOUNDING_PROBLEM: The founding problem was the absence of any legal restraint on treatment of persons in non-international armed conflicts — the 1949 Geneva Conventions' first effort to extend minimum humanitarian protections beyond international wars, where previously only the laws of war applied to states, leaving civil war detainees wholly unprotected.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC's 2016 updated Commentary on Common Article 3 (authored outside beneficiary states) attests that non-international armed conflicts have proliferated since 1949 and the protection need is greater, not lesser. The UN Special Rapporteur on Torture's mandate reports (independent of detainee advocacy) consistently document ongoing protection gaps. No credible source attests the founding problem is dead.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__absolute_prohibition, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, ExtMetricName, E),
    domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The absolute prohibition reading claims mountain status: emerges_naturally=true (as jus cogens), accessibility_collapse=0.95 (no legal alternative permits torture), resistance=0.02 (the norm itself meets almost no resistance within the legal order; resistance appears in enforcement, not the norm). Extractiveness=0.15 is non-zero because the constraint imposes opportunity costs on states that would prefer broader interrogation latitude — but this is assessed from the reading's own lights (the standing arrangement under contest is 'states may use any effective interrogation method'; the reading imposes the prohibition). Suppression=0.05 is the norm's internal coerciveness (it binds legally), not enforcement suppression. Theater_ratio=0.08 reflects occasional performative invocations by states that violate in practice.
 *
 * PERSPECTIVAL GAP:
 *   From the detainee/protected-person seat, the constraint is pure protection (d≈0, χ≈0). From the state interrogation apparatus seat, it is a binding restriction with opportunity costs (d≈0.7-0.8, χ scaled up). The engine computes this divergence from the structural data: beneficiaries declared (detainees, civilian populations), no victims declared, identity_locked exit for detainees (they cannot exit the protected status), arbitrage exit for institutional guardians. The claimed_type=mountain and metrics are authored independently — the engine will compute per-seat types from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: detainees_in_non_international_armed_conflict and civilian_populations_in_conflict_zones — these groups receive the constraint's protection without bearing its operational costs. Their exit_options=identity_locked (protected status is constitutive of their legal personhood in conflict). No victims declared — the constraint does not extract from any group for another's benefit. State interrogation apparatus bears opportunity costs but is not a 'victim' in the extractive sense; the cost is the price of legal compliance. International humanitarian law institutions are agenda_setters with analytical exit. The directionality derivation chain reads: beneficiaries → low d → low/negative χ; constrained state actors → higher d → χ scaled by scope (global) and power (institutional).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting non-combatants in non-international armed conflicts from arbitrary violence) remains live — such conflicts persist and the protection need is unchanged. The mandate has not atrophied; the constraint's protective function is continuously exercised. No mandatrophy resolution is declared. The constraint's persistence is functional, not inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the absolute prohibition reading of Common Article 3 a genuine natural law / peremptory norm, or a constructed legal constraint that benefits identifiable agents (detainees, human rights institutions)?',
    'Comparative analysis of state practice and opinio juris across all parties to the Geneva Conventions; tracking whether any state formally claims a security exception while remaining a party.',
    'If constructed, FSM may trigger reclassification to tangled_rope; if genuine peremptory norm, mountain classification stands. The omega documents the irreducible ambiguity the FSM candidate requires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Natural-law vs. constructed status of the absolute prohibition reading').

omega_variable(
    enforcement_gap_vs_norm_status,
    'Does widespread non-compliance in practice (torture by state parties) affect the constraint''s classification, or is the mountain classification about the norm''s structural status independent of enforcement?',
    'Distinguish between the constraint as a normative claim (mountain) and the enforcement regime as a separate constraint (likely snare or tangled_rope). The engine classifies the normative constraint; non-compliance is a separate measurement.',
    'Clarifies that extractiveness/suppression metrics here describe the normative constraint''s operation, not the enforcement gap. Prevents misclassification due to conflation of norm and practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_gap_vs_norm_status, conceptual, 'Separation of normative constraint from enforcement reality').

omega_variable(
    committer_structure_framing,
    'Is this constraint a reading of the ''humane_treatment_standard'' kernel, and if so, how does it relate to the sibling readings ''contextual_necessity'' and ''proportionality_balancing''?',
    'This constraint instantiates the ''absolute_prohibition'' reading of kernel ''humane_treatment_standard''. The reading forecloses ''contextual_necessity'' (mutually exclusive core premises) and coexists with ''proportionality_balancing'' (different parties hold both). The disagreement is located on whether any security exception can legally authorize crossing the torture threshold.',
    'Routes committer structure through omega rather than inventing fields. Documents the kernel/reading relationship and structural deltas for sibling comparison.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_framing, conceptual, 'Kernel/reading structure and sibling relations for humane_treatment_standard').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(humane_treatment_absolute_prohibition_tr_t1949, humane_treatment_standard__absolute_prohibition, theater_ratio, 1949, 0.02).
narrative_ontology:measurement(humane_treatment_absolute_prohibition_tr_t1977, humane_treatment_standard__absolute_prohibition, theater_ratio, 1977, 0.03).
narrative_ontology:measurement(humane_treatment_absolute_prohibition_tr_t2001, humane_treatment_standard__absolute_prohibition, theater_ratio, 2001, 0.06).
narrative_ontology:measurement(humane_treatment_absolute_prohibition_tr_t2006, humane_treatment_standard__absolute_prohibition, theater_ratio, 2006, 0.08).
narrative_ontology:measurement(humane_treatment_absolute_prohibition_tr_t2014, humane_treatment_standard__absolute_prohibition, theater_ratio, 2014, 0.07).
narrative_ontology:measurement(humane_treatment_absolute_prohibition_tr_t2024, humane_treatment_standard__absolute_prohibition, theater_ratio, 2024, 0.08).

% Extraction over time
narrative_ontology:measurement(humane_treatment_absolute_prohibition_be_t1949, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1949, 0.05).
narrative_ontology:measurement(humane_treatment_absolute_prohibition_be_t1977, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1977, 0.08).
narrative_ontology:measurement(humane_treatment_absolute_prohibition_be_t2001, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2001, 0.12).
narrative_ontology:measurement(humane_treatment_absolute_prohibition_be_t2006, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2006, 0.15).
narrative_ontology:measurement(humane_treatment_absolute_prohibition_be_t2014, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2014, 0.14).
narrative_ontology:measurement(humane_treatment_absolute_prohibition_be_t2024, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(humane_treatment_absolute_prohibition_su_t1949, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1949, 0.02).
narrative_ontology:measurement(humane_treatment_absolute_prohibition_su_t1977, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1977, 0.03).
narrative_ontology:measurement(humane_treatment_absolute_prohibition_su_t2001, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2001, 0.05).
narrative_ontology:measurement(humane_treatment_absolute_prohibition_su_t2006, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2006, 0.05).
narrative_ontology:measurement(humane_treatment_absolute_prohibition_su_t2014, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2014, 0.05).
narrative_ontology:measurement(humane_treatment_absolute_prohibition_su_t2024, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__absolute_prohibition, 0.1).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__proportionality_balancing).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, non_refoulement_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, fair_trial_guarantees_in_conflict).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, prohibition_of_enforced_disappearance).

% DUAL FORMULATION NOTE:
% This constraint is one member of the humane_treatment_standard constraint family. The kernel 'humane_treatment_standard' decomposes into three structurally distinct readings with different ε values: absolute_prohibition (this story, ε≈0.15), contextual_necessity (ε≈0.45, extraction via security exception), proportionality_balancing (ε≈0.35, extraction via calibrated discretion). They are linked via network.affects_constraints. The absolute_prohibition reading forecloses contextual_necessity (mutually exclusive premises) and coexists with proportionality_balancing (different institutional actors hold each).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
