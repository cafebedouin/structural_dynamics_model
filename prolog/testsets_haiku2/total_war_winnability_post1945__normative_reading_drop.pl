% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: Total War Normative Prohibition (Post-1945 Charter/IHL Reading)
 *   domain: international_relations/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the NORMATIVE READING of the total war
 *   winnability question: the claim that total war remains physically
 *   possible but became normatively illegitimate through the UN Charter's
 *   Article 2(4) (prohibition on force except in self-defense) and the
 *   development of humanitarian law (Geneva Conventions, Genocide Convention,
 *   ICC statute). Under this reading, the constraint operates as a rope-class
 *   coordination mechanism: states coordinate on treating total war as
 *   illegitimate, in exchange for reciprocal restraint. This reading is
 *   contested. The structural_contraction_reading argues that nuclear weapons
 *   physically removed total war from the reachable space, making the
 *   normative prohibition redundant. The strategic_culture_drift reading
 *   argues that total war dropped from elite discourse through ideational
 *   shift in strategic culture, not through legal covenant. This constraint
 *   models the normative reading's own structural account: the prohibition is
 *   a legal-institutional commitment that solves a coordination problem and
 *   persists because compliance is in the interests of status-quo powers. The
 *   authored metrics reflect that the constraint is substantially effective
 *   (extractiveness stays low, around 0.31; suppression is moderate, around
 *   0.58 because enforcement requires institutional vigilance rather than
 *   coercion of the masses) and increasingly routinized over the interval
 *   (theater_ratio plateaus around 0.22, indicating the constraint has moved
 *   from explicit legal assertion to taken-for-granted institutional
 *   practice).
 *
 * KEY AGENTS:
 *   - global_civilian_populations: primary beneficiaries; protected from total-war targeting
 *   - status_quo_powers (P5, NATO, G7): agenda-setters; maintain and enforce the prohibition through treaty, adjudication, and normative consensus
 *   - revisionist_powers: constrained payers; subject to the prohibition's limits on available strategy
 *   - international_humanitarian_institutions (ICC, ICTY, ICRC): secondary agenda-setters; sustain the norm through legal enforcement
 *   - rogue_state_or_genocidal_regime: excluded; would pursue total war if able but lacks power to overturn the prohibition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.31).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.58).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.31).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Total War Normative Prohibition (Post-1945 Charter/IHL Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international_relations/commitment_systems").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__normative_reading_drop).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, '07b76587-8dc4-401b-8996-3823145ac785').
narrative_ontology:cs_kernel_codification('07b76587-8dc4-401b-8996-3823145ac785', formalized).
narrative_ontology:cs_authority_grounding('07b76587-8dc4-401b-8996-3823145ac785', extraction).
narrative_ontology:cs_interpretation_layer_present('07b76587-8dc4-401b-8996-3823145ac785').
narrative_ontology:cs_reading_relation('07b76587-8dc4-401b-8996-3823145ac785', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('07b76587-8dc4-401b-8996-3823145ac785', total_war_winnability_post1945__strategic_culture_drift, influences).
narrative_ontology:cs_axiom('07b76587-8dc4-401b-8996-3823145ac785', foundational, total_war_normatively_prohibited_by_charter_and_ihl).
narrative_ontology:cs_axiom_status(total_war_normatively_prohibited_by_charter_and_ihl, holdable).
narrative_ontology:cs_axiom_grounding('07b76587-8dc4-401b-8996-3823145ac785', total_war_normatively_prohibited_by_charter_and_ihl, conventional).
narrative_ontology:cs_axiom('07b76587-8dc4-401b-8996-3823145ac785', secondary, prohibition_persistence_requires_institutional_enforcement).
narrative_ontology:cs_axiom_status(prohibition_persistence_requires_institutional_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('07b76587-8dc4-401b-8996-3823145ac785', prohibition_persistence_requires_institutional_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('07b76587-8dc4-401b-8996-3823145ac785', charter_humanitarian_law_supremacy).
narrative_ontology:cs_created_at('07b76587-8dc4-401b-8996-3823145ac785', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, state_system_stability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, emerging_military_powers).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_powers).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, emerging_military_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected from total war targeting by the prohibition's normative force and humanitarian law codification. Cannot enforce the prohibition themselves; their protection depends on state adherence and international enforcement. If the prohibition lapses, civilians lose legal recourse and face renewed vulnerability to unlimited warfare.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    powerless, biographical, trapped, universal).

% Established major powers (permanent UN Security Council members, NATO, G7) maintain and enforce the prohibition through treaty adherence, humanitarian law adjudication (ICC, tribunals), and normative consensus-building. They benefit from a rule-based order in which total war is delegitimized because they have least to gain from unrestricted warfare and most to lose from systemic collapse.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, status_quo_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% Powers seeking territorial or geopolitical revision face constraints on the means available to them: total war — the classical option for decisive victory — is normatively prohibited and brings international response (sanctions, legal prosecution, coalition military response). This constrains their strategic menu but does not eliminate it (conventional limited war, asymmetric approaches, coercive diplomacy remain available).
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_powers, payer,
    powerful, biographical, constrained, global).

% Regional powers building military capacity face the same normative-legal prohibition when they reach great-power status. They benefit from the prohibition's civilian protection but pay the cost of accepting limits on their strategic options. Their path to regional dominance must navigate the constraint rather than bypass it.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, emerging_military_powers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, emerging_military_powers, beneficiary).

% ICC, ICTY, ICTR, UN human rights bodies, ICRC interpret and enforce the prohibition through legal mechanisms. They sustain the constraint's normative weight by prosecuting violations and legitimizing the rule through adjudication. Their authority depends on state compliance; their enforcement depends on state will.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_humanitarian_institutions, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, international_humanitarian_institutions, observer).

% A regime willing to commit atrocities (genocide, war crimes against civilians) faces the prohibition's legal and normative weight; leaders can be prosecuted post-conflict. They are excluded from the consensus that sustains the prohibition — they view it as an obstacle to their strategic goals — but lack the power to overturn it unilaterally. They often proceed anyway, incurring international response.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, rogue_state_or_genocidal_regime, excluded,
    moderate, immediate, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__normative_reading_drop, diffuse).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__normative_reading_drop, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared legal and normative framework prohibiting warfare that targets or relies on destruction of civilian populations and productive capacity as a means of victory. Solves the commitment problem: states need to coordinate on rules that make escalation to total war unthinkable, because unrestricted total war is mutually devastating and unstable.
% TRANSFER_FUNCTION: Transfers the strategic option of total war from the available means set (for revisionist powers and potential great-power competitors) to the illegitimate means set. The 'cost' paid is the forfeiture of the classical strategy of conquest-through-annihilation; the 'transfer' is acceptance of limits on military objectives and methods in exchange for reciprocal limits from other states.
% ABSENT_VOICES: Defeated powers from the mid-20th century (Imperial Japan, Nazi Germany, Italian fascism) would have objected to the prohibition at its founding — it explicitly criminalized the total-war strategies they pursued and ruled them beyond legitimacy. Military theorists who view total war as a necessary option for existential defense (a small but persistent school) remain marginalized from the consensus. Future belligerents who perceive total war as their only viable strategy would contest the prohibition but lack the institutional power to overturn it at the time of their confrontation.
% DISAPPEARANCE_RATIONALE: If the prohibition and its humanitarian-law enforcement framework vanished, great-power competition would immediately face the prospect of total-war strategies re-entering the strategic menu: unrestricted bombing of civilian infrastructure, deliberate starvation, population transfer, and other means of breaking an adversary's will to fight would become tactically available again. The prohibition's disappearance would likely trigger rapid renewed militarization, preemptive buildup, and strategic instability as powers prepared for unrestricted conflict. The 1945 bifurcation (limited war as the international norm, total war as illegitimate) would reverse.
% FOUNDING_PROBLEM: Post-WWII recognition that total war had become existentially destabilizing: in the nuclear age, any major-power conflict carried the risk of escalation to mutual annihilation. The UN Charter and the Geneva Conventions codified a shared normative ceiling to prevent that escalation from becoming inevitable.
% FOUNDING_PROBLEM_CORROBORATION: Military strategists, international lawyers, and conflict scholars outside the benefiting parties (academic analyses, military doctrine reviews, strategic studies journals) consistently attest that the founding problem — the risk of total-war escalation in great-power competition — remains present. The prohibition's normative weight is frequently tested (in hybrid warfare, drone targeting, cyber operations) and requires continuous institutional maintenance. Independent testimony from diplomats and security experts from non-status-quo powers confirms the constraint is felt as a limit on their options, supporting its live persistence.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__normative_reading_drop, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.18) and drifts slightly upward to 0.31 over 80 years — this trajectory reflects the constraint's maturation from an explicit legal prohibition with low institutional cost to an embedded normative principle with growing institutional overhead. The rise is modest because the constraint does not extract value from its beneficiaries (civilians); rather, it constrains the strategic options of revisionist powers. Suppression is moderate (0.58) and stable because the prohibition's enforcement relies on institutional surveillance and legal adjudication, not on coercion of the masses. Theater ratio rises from 0.08 to 0.22 over time, indicating increasing performative elements (humanitarian rhetoric in military doctrine, compliance theater, normative signaling) alongside the constraint's real enforcement. This is consistent with a constraint that has achieved widespread acceptance: the performative layer grows as compliance becomes routinized. Accessibility_collapse is high (0.72) because once a state understands the prohibition's legal and normative weight, alternatives to total war (limited war, coercive diplomacy, sanctions, asymmetric approaches) are the only available menu — total war exits the thinkable space. Resistance is moderate (0.44) because the constraint faces ongoing pressure from actors who view it as an obstacle to their goals (revisionist powers, genocide-prone regimes) but these actors lack the institutional power to overturn it; the constraint holds because status-quo powers benefit from maintaining it.
 *
 * PERSPECTIVAL GAP:
 *   The normative reading instantiates different structural positions for different seats. Status-quo powers compute the constraint as genuine coordination (a shared legal framework that reduces mutual risk of escalation) and have low d (beneficiaries of order). Revisionist powers compute it as extraction (a limit on their strategic options imposed by a powerful coalition) and have high d (targets of the constraint). Global civilians have intermediate d (they benefit from the protection but cannot enforce it themselves, and the constraint's persistence depends on institutional will rather than their own agency). The engine computes these per-seat classifications from the structural data; the normative reading itself does not adjudicate them. This reading's claim is that the constraint is rope-class (coordination) precisely because all parties have an interest in reciprocal restraint, even if revisionist powers would prefer to be unconstrained. The claim/metric gap is intentional: the constraint is CLAIMED as rope (genuine coordination) while the authored suppression score (0.58) reflects that maintaining the prohibition requires active institutional enforcement, not mere mutual preference.
 *
 * DIRECTIONALITY LOGIC:
 *   Status-quo powers (agenda-setters, institutional power) experience the constraint as coordination they benefit from maintaining — low directionality (d ~0.15). Revisionist powers (powerful but constrained in their strategic options) experience high directionality (d ~0.75) because the constraint limits their reachable strategy space without protecting them in reciprocal fashion (the constraint is asymmetric: it limits total war for everyone, but revisionist powers have more to gain from total war than status-quo powers). Global civilians have near-zero directionality toward the constraint's operation (they benefit passively from its existence, but play no role in maintaining it) — d ~0.05. Emerging military powers have intermediate directionality (d ~0.50) because they both benefit from the constraint's civilian protections and constrain themselves as they rise. No directionality overrides are needed; the structural data (beneficiary/victim declarations + exit options) drive the derivation correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is live in its founding problem: total war remains strategically reachable but is treated as illegitimate, and the prohibition is actively maintained through international institutions. It is not a mandatrophy case (where the founding problem is dead but the constraint persists through inertia). However, the constraint's persistence depends on status-quo powers' willingness to enforce it. If a great-power war erupted between two nuclear-armed states, the prohibition would face its most severe test: would either belligerent escalate to total war, and would the international community enforce consequences? The constraint's classification as rope (rather than mountain or snare) rests on the normative reading's claim that the prohibition solves a genuine coordination problem. If the structural_contraction_reading were true (nuclear weapons made total war impossible regardless of norms), the constraint would be either mountain (if nuclear impossibility is treated as a natural law) or piton (if the prohibition persists through institutional theater despite being structurally unnecessary). This constraint does not attempt to reconcile the readings; it instantiates only the normative reading and routes the contest to omega variables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_vs_structural_causation,
    'Does the prohibition on total war persist because of its normative-legal force and institutional enforcement, or because nuclear weapons made total war structurally impossible regardless of norms?',
    'Counterfactual analysis: if nuclear weapons were removed and deterrence stabilized at conventional-only competition, would total war re-enter great-power strategic planning? Historical comparison with pre-1945 baseline and strategic doctrine analysis.',
    'If normative force is primary, the constraint is rope (genuine coordination). If structural impossibility is primary, the constraint is mountain or piton (depending on whether the prohibition is functional or theatrical). This distinction is the core uncertainty between the normative and structural readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(normative_vs_structural_causation, conceptual, 'Whether the prohibition''s persistence is causal or epiphenomenal to structural military change.').

omega_variable(
    ideational_vs_institutional_mechanism,
    'Does the prohibition persist because of strategic culture''s normative shift (ideas about what war is legitimate), or because of international institutions'' enforcement (law, tribunals, sanctions)?',
    'Examination of variance in commitment across different strategic cultures and regions; analysis of enforcement mechanisms and compliance patterns; case studies of violations and international response.',
    'If ideational shift is primary, the constraint is more fragile (dependent on continued cultural consensus) and might be better modeled as cultural norm-based rather than institutional-rope. If institutional enforcement is primary, the constraint''s persistence depends on status-quo power will, making it a rope that requires active maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideational_vs_institutional_mechanism, empirical, 'Whether the mechanism of the prohibition is cultural consensus or institutional coercion.').

omega_variable(
    revisionist_power_exit_path,
    'To what extent do revisionist powers experience the prohibition as a genuine constraint on their strategy, versus a rule they can selectively violate if they accept international consequences?',
    'Strategic doctrine analysis of rising powers and failed states; examination of violations (genocidal campaigns, indiscriminate bombing) and the international response to them; case studies of states choosing limited war over total war.',
    'If the constraint is experienced as genuine (exit is truly closed), the measured suppression is accurate. If the constraint is experienced as violable-with-consequences, the exit_options for revisionist powers are better modeled as ''constrained'' rather than ''trapped,'' and the directionality calculation shifts slightly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revisionist_power_exit_path, empirical, 'Whether the prohibition is experienced as a hard constraint or as a rule with negotiable costs.').

omega_variable(
    kernel_reading_scope,
    'Does the normative reading accurately distinguish itself from the structural and cultural readings, or do all three readings describe overlapping mechanisms in the same constraint?',
    'Detailed structural analysis of each reading''s claimed mechanism; examination of whether the three readings are alternative causal stories about the same phenomenon, or whether they describe genuinely different constraints (different ε, different beneficiary/victim structure).',
    'If the readings are genuinely distinct constraints, they should be decomposed into separate stories per the ε-invariance principle. If they are alternative causal stories about the same constraint, the normative reading''s ε-invariant claim is the framing choice that distinguishes this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Whether the kernel admits three distinct constraint structures or three causal framings of one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0, 0.08).
narrative_ontology:measurement(tota_tr_t10, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 10, 0.11).
narrative_ontology:measurement(tota_tr_t20, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 20, 0.15).
narrative_ontology:measurement(tota_tr_t35, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 35, 0.19).
narrative_ontology:measurement(tota_tr_t50, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 50, 0.21).
narrative_ontology:measurement(tota_tr_t65, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 65, 0.22).
narrative_ontology:measurement(tota_tr_t80, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 80, 0.22).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(tota_be_t10, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(tota_be_t20, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(tota_be_t35, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 35, 0.29).
narrative_ontology:measurement(tota_be_t50, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 50, 0.3).
narrative_ontology:measurement(tota_be_t65, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 65, 0.31).
narrative_ontology:measurement(tota_be_t80, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 80, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(tota_su_t10, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(tota_su_t20, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(tota_su_t35, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 35, 0.57).
narrative_ontology:measurement(tota_su_t50, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(tota_su_t65, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 65, 0.58).
narrative_ontology:measurement(tota_su_t80, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 80, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__normative_reading_drop, 0.12).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945__structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945__strategic_culture_drift).

% DUAL FORMULATION NOTE:
% The total_war_winnability_post1945 kernel admits three structurally distinct readings, each instantiating a different constraint type and ε value. The normative_reading_drop (this story) models total war as normatively prohibited through legal-institutional coordination (rope). The structural_contraction_reading models total war as physically impossible due to nuclear weapons (mountain or snare, depending on enforcement). The strategic_culture_drift reading models total war as dropped from elite discourse via ideational shift (piton or rope, depending on mechanism). These three stories are linked as constraint family members; each describes a different causal mechanism for the same phenomenon (total war's disappearance from contemporary great-power strategy). The network edges record dependency: the normative reading's rope-coordination claim influences both the structural and cultural readings' classification choices. All three readings share the same referent (total war's actual status in post-1945 strategic thinking) but instantiate different ε values and structural hypotheses about causation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
