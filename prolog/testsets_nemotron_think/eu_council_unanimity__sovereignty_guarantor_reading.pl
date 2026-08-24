% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__sovereignty_guarantor_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: EU Council Unanimity as Sovereignty Guarantor
 *   domain: political/institutional/international
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereignty_guarantor_reading of
 *   the eu_council_unanimity kernel. It treats unanimity as a foundational
 *   protection — a Mountain-type constraint — where each member state's veto
 *   is a legitimate exercise of sovereign right, not an extraction tool. The
 *   beneficiary set includes all member states (small states
 *   disproportionately, but large states equally in principle). Coordination
 *   costs (moderate ε ≈ 0.35) arise from the genuine difficulty of securing
 *   27-way consent on sensitive matters; these are the cost of the
 *   coordination function, not extractive overhead. No systematic extraction
 *   occurs: veto use does not transfer resources to the vetoing state. The
 *   constraint emerges from the logic of voluntary association among
 *   sovereigns — it is the structural condition that makes the union
 *   voluntary rather than coercive.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.35).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.1).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, mountain).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity as Sovereignty Guarantor").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "political/institutional/international").

domain_priors:emerges_naturally(eu_council_unanimity__sovereignty_guarantor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, '2a90750a-672f-4580-b4e8-2d4191654e20').
narrative_ontology:cs_kernel_codification('2a90750a-672f-4580-b4e8-2d4191654e20', formalized).
narrative_ontology:cs_authority_grounding('2a90750a-672f-4580-b4e8-2d4191654e20', lineage).
narrative_ontology:cs_interpretation_layer_present('2a90750a-672f-4580-b4e8-2d4191654e20').
narrative_ontology:cs_reading_relation('2a90750a-672f-4580-b4e8-2d4191654e20', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a90750a-672f-4580-b4e8-2d4191654e20', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('2a90750a-672f-4580-b4e8-2d4191654e20', foundational, unanimity_as_sovereignty_right).
narrative_ontology:cs_axiom_status(unanimity_as_sovereignty_right, holdable).
narrative_ontology:cs_axiom_grounding('2a90750a-672f-4580-b4e8-2d4191654e20', unanimity_as_sovereignty_right, deontological).
narrative_ontology:cs_axiom('2a90750a-672f-4580-b4e8-2d4191654e20', foundational, veto_as_legitimate_defense).
narrative_ontology:cs_axiom_status(veto_as_legitimate_defense, holdable).
narrative_ontology:cs_axiom_grounding('2a90750a-672f-4580-b4e8-2d4191654e20', veto_as_legitimate_defense, deontological).
narrative_ontology:cs_reference_frame('2a90750a-672f-4580-b4e8-2d4191654e20', treaty_sovereignty_framework).
narrative_ontology:cs_drift_state('2a90750a-672f-4580-b4e8-2d4191654e20', contemporary_integration_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2a90750a-672f-4580-b4e8-2d4191654e20', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, all_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, citizens_of_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).
narrative_ontology:constraint_vindicates(eu_council_unanimity__sovereignty_guarantor_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(eu_council_unanimity__sovereignty_guarantor_reading, consent_principle).
narrative_ontology:constraint_vindicates(eu_council_unanimity__sovereignty_guarantor_reading, sovereign_equality_of_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on unanimity to prevent their core sovereign choices from being overridden by qualified majority voting. Their veto is not a bargaining chip but a structural guarantee that accession treaties and constitutional identity remain under their control. Exit from the unanimity protection would mean accepting permanent minority status in a union where they cannot block sovereignty-implicating decisions.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states, beneficiary,
    moderate, generational, constrained, continental).

% Also protected by the unanimity rule — their sovereignty cannot be overridden either — but they bear higher coordination costs because their policy initiatives requiring unanimity must secure consent from all 27 members. They could exit the constraint by pushing for treaty change to QMV, but doing so would undermine their own sovereignty guarantee. They pay in legislative friction what small states pay in structural vulnerability.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, beneficiary,
    powerful, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, payer).

% Proposes legislation and manages policy areas where unanimity applies (taxation, foreign policy, accession, treaty change). The Commission experiences unanimity as a procedural constraint that slows agenda-setting but legitimizes its proposals by forcing broad consent. It does not collect rents from the rule; it administers the process.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, european_commission, agenda_setter,
    institutional, biographical, analytical, continental).

% The decision-making body where unanimity votes occur. Heads of state or government exercise the veto directly. They are both the authors and the subjects of the constraint — each leader holds a veto and is subject to others' vetoes. The constraint constitutes their collective authority.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, european_council, agenda_setter,
    institutional, generational, analytical, continental).

% Ultimate principals of national sovereignty. Unanimity ensures that no EU decision implicating constitutional identity, tax sovereignty, or foreign policy can bind their polity without their national government's consent — which in democratic systems requires parliamentary ratification or referendum. They do not directly exercise the veto but are the constituency it protects.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, citizens_of_member_states, beneficiary,
    organized, biographical, constrained, national).

% Accession requires unanimous consent of all current members. They have no voice in the unanimity rule that governs their entry, yet the rule determines whether and on what terms they join. They would object to vetoes blocking enlargement for reasons unrelated to accession criteria, but they are structurally absent from the decision.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, candidate_states, excluded,
    powerless, biographical, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__sovereignty_guarantor_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__sovereignty_guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the foundational coordination problem of a voluntary union of sovereign states: how to take collective action without permitting a majority to coerce a minority on matters implicating sovereignty. The unanimity rule makes each state's consent a necessary condition for such action, converting potential coercion into negotiated agreement.
% TRANSFER_FUNCTION: No systematic transfer occurs. The veto right is not a resource extracted from others — it is a structural position each state holds by virtue of sovereign equality. Coordination costs (time, diplomatic effort, policy delay) are borne symmetrically by all participants when unanimity is required; they are the cost of the coordination function, not a transfer to a beneficiary.
% ABSENT_VOICES: Candidate states seeking accession (structurally excluded from the veto that decides their entry); regions or peoples within member states whose sovereignty claims are not recognized by the state-centric framework; future generations who inherit the constitutional structure without having consented to it.
% DISAPPEARANCE_RATIONALE: If unanimity disappeared overnight and were replaced by qualified majority voting on sovereignty-implicating matters, small states would lose their structural guarantee against majoritarian override. Tax policy, foreign policy, constitutional amendments, and accession decisions could be imposed against a member state's will. The EU would transform from a union of sovereign equals into a majoritarian federation — a fundamental rearrangement of the constitutional order.
% FOUNDING_PROBLEM: The founding problem was creating a European union that could act collectively while preserving the sovereign equality of its member states — preventing the re-emergence of hegemony that had caused continental wars. Unanimity was the mechanism that made voluntary cooperation among sovereigns possible: no state would join a union where it could be outvoted on its core sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The Treaty on European Union (Article 4(2) TEU) explicitly recognizes national identity as inherent to state sovereignty. Constitutional courts of member states (German BVerfG, French Conseil Constitutionnel, Polish TK) have ruled that EU competences cannot override core state sovereignty without national consent. Small state diplomatic positions (Benelux, Nordic, Baltic) consistently treat unanimity as non-negotiable. These are corroborations from outside the 'beneficiary' framing — they come from treaty text, constitutional jurisprudence, and state practice, not only from states that currently benefit.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, ExtMetricName, E),
    domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(eu_council_unanimity__sovereignty_guarantor_reading),
    narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because coordination costs are real — securing unanimous consent across 27 diverse polities takes time, diplomatic capital, and policy compromise. But these costs are symmetric and inherent to the coordination problem, not asymmetric extraction. Suppression is minimal (0.1) because the constraint does not coerce — it prevents coercion. Theater ratio is low (0.1) because the veto is genuinely exercised as a sovereignty right, not performatively. Accessibility collapse is near-total (0.9) because once the sovereign equality principle is accepted, no alternative arrangement can satisfy it without unanimous consent. Resistance is near-zero (0.1) because the constraint is the constitutional foundation, not an imposition.
 *
 * PERSPECTIVAL GAP:
 *   From the small state seat: unanimity is the only structural guarantee against permanent minority status — experienced as Mountain (rights-exercise). From the large state seat: unanimity is a coordination cost they pay for their own sovereignty guarantee — experienced as Rope (symmetric coordination). From the Commission seat: unanimity is procedural friction — experienced as low-extraction coordination. The engine computes these seat divergences from the structural data; the authored claim (Mountain) reflects the constraint's foundational character from the system-design perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   All member states are beneficiaries (d ≈ 0.0–0.2) because the constraint protects each state's sovereignty symmetrically. Small states have constrained exit (cannot credibly threaten exit without losing EU benefits) but this does not make them targets — their constrained exit reflects the value they place on the union, not extraction by the constraint. Large states have mobile exit (could push treaty change) but choose not to because they also value the sovereignty guarantee. The Commission and Council are agenda_setters with analytical exit — they administer the rule but are not subject to its extraction. Candidate states are excluded (trapped) — they bear the constraint's effects without voice, but this is a boundary condition of the kernel, not an extraction dynamic within it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sovereign equality in collective action) remains live — QMV expansion into new policy areas creates recurring sovereignty disputes (tax, foreign policy, rule of law). The constraint has not atrophied; its domain has been contested but not supplanted. Mandatrophy is not resolved because the problem it solves persists. The sovereignty_guarantor_reading treats every veto as legitimate defense of that live problem; the veto_trap_reading treats some vetoes as extraction exploiting the problem's persistence. The classification prevents mislabeling coordination as extraction by anchoring the veto right in sovereign equality doctrine, not in bargaining outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does this reading''s structural characterization of the unanimity constraint relate to the sibling readings of the same kernel?',
    'Structural comparison of the three readings'' beneficiary/victim sets, extractiveness referents, and coordination/transfer functions. The kernel_id eu_council_unanimity instantiates three distinct constraints with different ε and type classifications.',
    'If the sibling readings are confirmed as structurally distinct constraints (per ε-invariance), the kernel is a family of linked constraints not a single ambiguous claim. This reading''s Mountain classification stands or falls on its own structural data, not on the kernel label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment-system framing: this reading is one of three distinct constraints instantiated from the eu_council_unanimity kernel.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the unanimity rule a genuine natural law of sovereign association (Mountain) or a constructed treaty provision that could be revised (Rope/Scaffold)?',
    'Compare with other voluntary unions of sovereigns (historical confederations, treaty organizations) to test whether unanimity emerges necessarily from sovereign equality or is a contingent design choice.',
    'If constructed, the Mountain claim is a false summit — FSM would reclassify to tangled_rope (coordination + asymmetric extraction where large states bear coordination costs). If natural law, Mountain certification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'False summit mountain candidate: Mountain claim with declared beneficiaries requires omega documenting natural-law vs. constructed ambiguity.').

omega_variable(
    coordination_cost_as_extraction,
    'Do the coordination costs of unanimity (delay, policy gridlock, lowest-common-denominator outcomes) constitute a form of extraction borne disproportionately by large states or integration-seeking actors?',
    'Measure policy output quality and speed in unanimity vs. QMV areas; assess whether costs are symmetrically distributed or systematically loaded on specific states.',
    'If coordination costs are systematically asymmetric, the constraint is at least tangled_rope (genuine coordination + asymmetric cost burden). If symmetric, Mountain/Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_as_extraction, empirical, 'Whether moderate ε reflects symmetric coordination cost or asymmetric extraction disguised as coordination cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_council_unanimity_sgr_tr_t1993, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 1993, 0.05).
narrative_ontology:measurement(eu_council_unanimity_sgr_tr_t1998, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 1998, 0.07).
narrative_ontology:measurement(eu_council_unanimity_sgr_tr_t2004, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2004, 0.08).
narrative_ontology:measurement(eu_council_unanimity_sgr_tr_t2009, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2009, 0.09).
narrative_ontology:measurement(eu_council_unanimity_sgr_tr_t2014, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(eu_council_unanimity_sgr_tr_t2020, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(eu_council_unanimity_sgr_tr_t2024, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(eu_council_unanimity_sgr_be_t1993, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 1993, 0.25).
narrative_ontology:measurement(eu_council_unanimity_sgr_be_t1998, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 1998, 0.28).
narrative_ontology:measurement(eu_council_unanimity_sgr_be_t2004, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2004, 0.32).
narrative_ontology:measurement(eu_council_unanimity_sgr_be_t2009, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2009, 0.33).
narrative_ontology:measurement(eu_council_unanimity_sgr_be_t2014, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2014, 0.34).
narrative_ontology:measurement(eu_council_unanimity_sgr_be_t2020, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2020, 0.35).
narrative_ontology:measurement(eu_council_unanimity_sgr_be_t2024, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(eu_council_unanimity_sgr_su_t1993, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 1993, 0.05).
narrative_ontology:measurement(eu_council_unanimity_sgr_su_t1998, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 1998, 0.07).
narrative_ontology:measurement(eu_council_unanimity_sgr_su_t2004, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2004, 0.08).
narrative_ontology:measurement(eu_council_unanimity_sgr_su_t2009, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2009, 0.09).
narrative_ontology:measurement(eu_council_unanimity_sgr_su_t2014, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2014, 0.1).
narrative_ontology:measurement(eu_council_unanimity_sgr_su_t2020, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2020, 0.1).
narrative_ontology:measurement(eu_council_unanimity_sgr_su_t2024, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__sovereignty_guarantor_reading, 0.08).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form the eu_council_unanimity constraint family. Each reading instantiates a distinct constraint with different ε, beneficiary/victim structure, and claimed_type. The sovereignty_guarantor_reading claims Mountain (ε=0.35 coordination cost); veto_trap_reading claims Snare/Tangled Rope (extraction via credible blocking threats); diplomatic_capital_reading claims Rope/Scaffold (consensus-building with sunset via treaty evolution). All three link to each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
