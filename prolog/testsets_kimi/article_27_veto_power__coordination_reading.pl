% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   The UN Charter Article 27(3) grants the five permanent members of the
 *   Security Council a veto over substantive resolutions, including Chapter
 *   VII enforcement actions. This constraint story instantiates the
 *   coordination reading: the veto functions as a necessary unanimity gate
 *   preventing collective-security decisions from compelling a nuclear-armed
 *   great power into military confrontation it rejects, thereby avoiding
 *   systemic war. In this reading, all UN members are net beneficiaries of
 *   the stability produced, and the constraint is classified as Rope. This is
 *   one reading of the contested article_27_veto_power kernel; sibling
 *   readings (oligopoly_reading, sovereignty_reading) are modeled as separate
 *   constraints.
 *
 * KEY AGENTS:
 *   - p5_nuclear_states: Agenda-setters (powerful/constrained) â wield the veto to preserve strategic autonomy
 *   - un_member_states: Beneficiaries (organized/constrained) â receive systemic stability, pay procedural inequality
 *   - non_permanent_sc_members: Secondary beneficiaries (moderate/constrained) â accept institutional asymmetry for order
 *   - populations_in_blocked_mandates: Excluded (powerless/trapped) â structurally absent from the procedural gate
 *   - international_legal_scholars: Observers (analytical) â assess Charter consistency and systemic effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.28).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.25).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "P5 Veto as Great-Power War Prevention (Coordination Reading)").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, 'ac537e33-e648-4446-a1c5-b942cb241aa1').
narrative_ontology:cs_kernel_codification('ac537e33-e648-4446-a1c5-b942cb241aa1', formalized).
narrative_ontology:cs_authority_grounding('ac537e33-e648-4446-a1c5-b942cb241aa1', lineage).
narrative_ontology:cs_interpretation_layer_present('ac537e33-e648-4446-a1c5-b942cb241aa1').
narrative_ontology:cs_reading_relation('ac537e33-e648-4446-a1c5-b942cb241aa1', article_27_veto_power__oligopoly_reading, influences).
narrative_ontology:cs_reading_relation('ac537e33-e648-4446-a1c5-b942cb241aa1', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('ac537e33-e648-4446-a1c5-b942cb241aa1', foundational, unanimity_gate_prevents_nuclear_escalation).
narrative_ontology:cs_axiom_status(unanimity_gate_prevents_nuclear_escalation, holdable).
narrative_ontology:cs_axiom_grounding('ac537e33-e648-4446-a1c5-b942cb241aa1', unanimity_gate_prevents_nuclear_escalation, instrumental).
narrative_ontology:cs_axiom('ac537e33-e648-4446-a1c5-b942cb241aa1', foundational, systemic_stability_overrides_procedural_equality).
narrative_ontology:cs_axiom_status(systemic_stability_overrides_procedural_equality, holdable).
narrative_ontology:cs_axiom_grounding('ac537e33-e648-4446-a1c5-b942cb241aa1', systemic_stability_overrides_procedural_equality, instrumental).
narrative_ontology:cs_reference_frame('ac537e33-e648-4446-a1c5-b942cb241aa1', unanimity_collective_security_framework).
narrative_ontology:cs_drift_state('ac537e33-e648-4446-a1c5-b942cb241aa1', contemporary_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ac537e33-e648-4446-a1c5-b942cb241aa1', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, p5_nuclear_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, un_member_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, non_permanent_sc_members).
narrative_ontology:constraint_vindicates(article_27_veto_power__coordination_reading, unanimity_collective_security_theory).
narrative_ontology:constraint_vindicates(article_27_veto_power__coordination_reading, nuclear_taboo_reinforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wield the Article 27 veto to block Security Council enforcement resolutions they reject, preserving strategic autonomy and preventing binding commitments to military confrontation. Their exit from the UN system is technically possible but would sacrifice institutional leverage and normative standing.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, p5_nuclear_states, agenda_setter,
    powerful, generational, constrained, global).

% Receive systemic stability from the reduced probability of great-power war that the veto facilitates; accept that Chapter VII enforcement requires P5 unanimity, which limits procedural equality but avoids nuclear escalation.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, un_member_states, beneficiary,
    organized, generational, constrained, global).

% Gain temporary procedural voice and regional representation on the Council while accepting that substantive enforcement decisions remain contingent on P5 consensus; they benefit from order but do not control its terms.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, non_permanent_sc_members, beneficiary,
    moderate, biographical, constrained, global).

% Populations facing atrocities or aggression shielded by a P5 veto would demand enforcement action if present in the Council chamber; they are structurally excluded from the veto's procedural logic.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, populations_in_blocked_mandates, excluded,
    powerless, immediate, trapped, local).

% Analyze whether the veto stabilizes the Charter system or undermines its legal coherence; their assessments influence reform discourse but carry no decision-making authority.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(article_27_veto_power__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents collective-action failure among nuclear-armed great powers by ensuring that no Chapter VII enforcement resolution can bind a P5 state against its will, thereby removing the existential incentive to defect from the UN system through war or institutional exit.
% TRANSFER_FUNCTION: Transfers final decision authority on enforcement from the numerical majority of the Security Council and General Assembly to the unanimous consent of the five permanent members.
% ABSENT_VOICES: Populations facing atrocities or aggression shielded by a P5 veto, and majorities of the General Assembly favoring automatic enforcement or veto abolition, are structurally excluded from the Council's procedural gate.
% DISAPPEARANCE_RATIONALE: Without the veto, P5 states facing unfavorable Chapter VII resolutions would face a choice between submitting to coercive enforcement or exiting the UN system, raising acute escalation risk; the institutional architecture of collective security would destabilize as great powers built alternative alliance structures outside the Charter.
% FOUNDING_PROBLEM: How to sustain a universal collective-security organization when five members possess nuclear arsenals and global power projection, such that majority coercion of any one risks systemic war and organizational collapse.
% FOUNDING_PROBLEM_CORROBORATION: Post-WWII diplomatic historians and strategic stability scholars outside the P5 attest the founding problem was genuine and structurally acute; revisionist scholars and Global South diplomats contest that the bipolar nuclear risk structure has evolved such that the veto now serves national interest more than systemic survival.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__coordination_reading, 0.28, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low-moderate (0.28) because the veto's primary effect is procedural delay and selective inaction rather than resource transfer; suppression is moderate-low (0.25) because the constraint blocks majority-rule alternatives but does so openly as a designed feature of the Charter. Theater is low (0.12) because veto use remains functionally consequential even when criticized. Accessibility collapse is high (0.70) because once the nuclear-security logic is accepted, majority-voting alternatives collapse as feasibleâthey would trigger P5 exit or systemic rupture. Resistance is low (0.20) because most states rhetorically accept the veto's persistence as a necessary condition for great-power participation.
 *
 * PERSPECTIVAL GAP:
 *   The P5 agenda-setter seat experiences the constraint as autonomy-preserving coordination that prevents binding escalation; non-P5 states experience it as institutional inequality that purchases systemic stability. The engine computes divergent per-seat classifications from this structural asymmetry: the P5 seat derives low directionality (subsidy toward autonomy), while non-P5 seats sit nearer symmetric or mild-target positions depending on their exit options and vulnerability to blocked enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 nuclear states are structural beneficiaries of the autonomy subsidy the veto provides (low d). UN member states are beneficiaries of systemic stability but bear the cost of procedural inequality (moderate d). No victim class is declared in this reading because nuclear great-power war would harm all states more than the inequality cost; the engine will derive no high-d victim seats, consistent with the rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring the coordination function (preventing collective-action failure among nuclear powers) to be structurally independent of extraction. Because the veto is not actively enforced against alternativesâit is a procedural gate written into the Charterâand no agent captures the procedural inefficiency as concentrated rent, the constraint resists mandatrophy into snare or piton. Should P5 states begin using the veto primarily to shield allies from non-existential sanctions rather than to avoid direct confrontation, the coordination function would atrophy and the constraint would drift toward tangled_rope or oligopoly_reading territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nuclear_deterrence_redundancy,
    'Does nuclear deterrence (MAD) already prevent great-power war, making the veto institutionally redundant as a coordination mechanism?',
    'Counterfactual analysis of Cold War near-miss crises to determine whether the veto specifically prevented escalation that deterrence alone would not have.',
    'If redundant, the veto''s coordination claim collapses and extraction (institutional oligopoly) dominates the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_deterrence_redundancy, empirical, 'Whether MAD renders the veto coordinatively redundant').

omega_variable(
    collective_security_alternative_feasibility,
    'Could weighted voting, regional representation expansion, or supermajority rules achieve comparable systemic stability without a unanimity gate?',
    'Comparative institutional modeling and historical case studies of limited collective-security experiments.',
    'If viable alternatives exist, the veto is not a strict coordination necessity and its epsilon should rise toward tangled_rope territory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_security_alternative_feasibility, conceptual, 'Whether alternative voting rules could substitute for the veto').

omega_variable(
    oligopoly_entanglement,
    'Does the coordination reading''s widespread acceptance provide the legitimacy cover that enables the oligopoly reading''s extraction to persist?',
    'Discourse-tracing of veto justifications in SC debates and GA reform negotiations to measure functional-cover usage.',
    'If entangled, this constraint may compute as tangled_rope rather than rope despite the coordination reading''s low authored epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oligopoly_entanglement, conceptual, 'Whether coordination legitimacy masks oligopoly extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a27_coord_tr_t0, article_27_veto_power__coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(a27_coord_tr_t15, article_27_veto_power__coordination_reading, theater_ratio, 15, 0.06).
narrative_ontology:measurement(a27_coord_tr_t30, article_27_veto_power__coordination_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(a27_coord_tr_t45, article_27_veto_power__coordination_reading, theater_ratio, 45, 0.09).
narrative_ontology:measurement(a27_coord_tr_t60, article_27_veto_power__coordination_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(a27_coord_tr_t75, article_27_veto_power__coordination_reading, theater_ratio, 75, 0.12).

% Extraction over time
narrative_ontology:measurement(a27_coord_be_t0, article_27_veto_power__coordination_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(a27_coord_be_t15, article_27_veto_power__coordination_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(a27_coord_be_t30, article_27_veto_power__coordination_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement(a27_coord_be_t45, article_27_veto_power__coordination_reading, base_extractiveness, 45, 0.24).
narrative_ontology:measurement(a27_coord_be_t60, article_27_veto_power__coordination_reading, base_extractiveness, 60, 0.26).
narrative_ontology:measurement(a27_coord_be_t75, article_27_veto_power__coordination_reading, base_extractiveness, 75, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(a27_coord_su_t0, article_27_veto_power__coordination_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(a27_coord_su_t15, article_27_veto_power__coordination_reading, suppression_requirement, 15, 0.18).
narrative_ontology:measurement(a27_coord_su_t30, article_27_veto_power__coordination_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement(a27_coord_su_t45, article_27_veto_power__coordination_reading, suppression_requirement, 45, 0.22).
narrative_ontology:measurement(a27_coord_su_t60, article_27_veto_power__coordination_reading, suppression_requirement, 60, 0.24).
narrative_ontology:measurement(a27_coord_su_t75, article_27_veto_power__coordination_reading, suppression_requirement, 75, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, oligopoly_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, sovereignty_reading).

% DUAL FORMULATION NOTE:
% The article_27_veto_power kernel decomposes into three structurally distinct constraints: coordination_reading (rope, low epsilon, systemic stability function), oligopoly_reading (tangled_rope/snare, high epsilon, authority rent extraction), and sovereignty_reading (rope/mountain depending on framing, consent principle instantiation). Each reading carries a stable epsilon invariant to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
