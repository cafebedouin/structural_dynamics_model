% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__veto_trap_reading, []).

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
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity Rule as Minoritarian Veto Trap
 *   domain: institutional/political/international
 *
 * SUMMARY:
 *   The EU Council's unanimity rule requires all member states to consent to
 *   decisions in most policy domains. This reading frames unanimity not as a
 *   sovereignty guarantee but as a structural trap enabling minoritarian
 *   extraction: a single state's credible veto threat forces the majority
 *   coalition to pay concessions, accept diluted policy, or grant budgetary
 *   carve-outs to avoid being blocked. The blocking state captures value from
 *   the constraint without bearing the coordination costs; the majority bears
 *   costs. The measurement series tracks rising extractiveness and
 *   suppression over a 20-year interval as blocking capacity
 *   institutionalizes and blocking states learn to use leverage more
 *   aggressively.
 *
 * KEY AGENTS:
 *   - blocking_minority_state: holds veto, extracts via credible threat leverage
 *   - coalition_majority_states: prefer collective action, forced to concede
 *   - supranational_institution_agenda: weakened by diluted outcomes
 *   - european_integration_advocates: excluded from negotiation, lose policy battles
 *   - treaty_framers: designed sovereignty guarantee that became extraction trap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.78).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.62).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, snare).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity Rule as Minoritarian Veto Trap").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "institutional/political/international").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, '5a4d29e0-7755-447d-82c2-c7f3956bdd25').
narrative_ontology:cs_kernel_codification('5a4d29e0-7755-447d-82c2-c7f3956bdd25', formalized).
narrative_ontology:cs_authority_grounding('5a4d29e0-7755-447d-82c2-c7f3956bdd25', lineage).
narrative_ontology:cs_interpretation_layer_present('5a4d29e0-7755-447d-82c2-c7f3956bdd25').
narrative_ontology:cs_reading_relation('5a4d29e0-7755-447d-82c2-c7f3956bdd25', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a4d29e0-7755-447d-82c2-c7f3956bdd25', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('5a4d29e0-7755-447d-82c2-c7f3956bdd25', foundational, veto_threat_enables_minoritarian_extraction).
narrative_ontology:cs_axiom_status(veto_threat_enables_minoritarian_extraction, holdable).
narrative_ontology:cs_axiom_grounding('5a4d29e0-7755-447d-82c2-c7f3956bdd25', veto_threat_enables_minoritarian_extraction, empirically_contingent).
narrative_ontology:cs_axiom('5a4d29e0-7755-447d-82c2-c7f3956bdd25', secondary, blocking_leverage_systematically_transfers_value_from_majority).
narrative_ontology:cs_axiom_status(blocking_leverage_systematically_transfers_value_from_majority, holdable).
narrative_ontology:cs_axiom_grounding('5a4d29e0-7755-447d-82c2-c7f3956bdd25', blocking_leverage_systematically_transfers_value_from_majority, empirically_contingent).
narrative_ontology:cs_reference_frame('5a4d29e0-7755-447d-82c2-c7f3956bdd25', treaty_unanimity_rule_as_blocking_mechanism).
narrative_ontology:cs_drift_state('5a4d29e0-7755-447d-82c2-c7f3956bdd25', contemporary_blocking_escalation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5a4d29e0-7755-447d-82c2-c7f3956bdd25', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, coalition_majority_states).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, supranational_institution_agenda).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds veto power over any Council decision. Uses credible blocking threats to extract concessions, opt-outs, budgetary carve-outs, or policy exceptions that benefit itself at the expense of the majority coalition's preferred outcome. The veto is structurally costless to threaten because membership and participation are mandatory; exit is not an option. Gains flow directly from the negotiation leverage the unanimity rule confers.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, blocking_minority_state, agenda_setter,
    powerful, generational, trapped, continental).

% Prefer collective action on a policy domain (climate, fiscal coordination, foreign policy). Face the systematic cost of buying off blocking states with concessions, diluted policy, or financial transfers to secure the unanimity required to act. Their leverage is asymmetric: they need the blocker; the blocker only needs to threaten. Cannot force action without unanimity; cannot exit the union without abandoning sunk institutional stakes.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, coalition_majority_states, payer,
    powerful, generational, constrained, continental).

% The Commission, Parliament, and Secretariat advance integration agendas that depend on Council decisions. Unanimity forces them into coalition management and bloc-building, often resulting in weakened, compromised outcomes that do not match their institutional momentum. They cannot ignore the blocking state and have no formal authority to override the unanimity requirement.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, supranational_institution_agenda, payer,
    moderate, generational, constrained, continental).

% Civil society, transnational movements, and academic networks advocate for deeper integration. They are not seated at the Council table; blocking states use the veto precisely to prevent the policy outcomes these advocates seek. Their absence from negotiation is structural — the unanimity rule does not require their consent.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, european_integration_advocates, excluded,
    organized, generational, constrained, continental).

% Designed the unanimity rule at successive treaty negotiations (Rome, Maastricht, Lisbon). Each generation of framers believed the rule guaranteed small-state voice and sovereignty protection; few treated it as a veto trap. The rule's design is archived in treaty text; its effects are measured in lived experience.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, treaty_framers, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__veto_trap_reading, blocking_minority_state).
narrative_ontology:fixing_cost_class(eu_council_unanimity__veto_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally ensures all member states consent to collective decisions affecting sovereignty and budgets, providing veto holders assurance their interests will not be overridden by majority preference.
% TRANSFER_FUNCTION: Transfers negotiating leverage to blocking states and concessions from the majority coalition to the blocker. The constraint moves policy dilution, budgetary carve-outs, and opt-outs from the coalition majority to states with credible blocking capacity.
% ABSENT_VOICES: Transnational advocates for integration and smaller states without veto leverage are structurally absent from the negotiation. Their absence matters: unanimity is often invoked to protect small states, but small states without a credible blocking threat are exposed to majority will precisely as the rule is designed; only blockers benefit.
% DISAPPEARANCE_RATIONALE: If unanimity vanished overnight and were replaced by qualified majority voting, the Council would move decisively on blocked agendas (climate, fiscal union, foreign policy coordination). Blocking states would lose their systematic leverage; majority coalitions would shift to supermajority thresholds without concessions. The institutional equilibrium would reorganize entirely.
% FOUNDING_PROBLEM: Treaty framers sought a rule that would prevent dominant coalitions from marginalizing small states and overriding sovereignty concerns. The rule was intended to protect weaker members and enforce iterative consensus-building.
% FOUNDING_PROBLEM_CORROBORATION: Treaty text and official rhetoric from founding states frame unanimity as a sovereignty safeguard. Blocking states (Hungary, Poland, Cyprus in recent crises) invoke the founding problem when deploying veto. Majority-coalition states and supranational advocates attest the rule has transformed into a mechanism for extractive blocking and agenda hijacking; legal scholars and political economists working outside the benefiting parties document the veto-trap dynamic and its costs.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__veto_trap_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__veto_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_council_unanimity__veto_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.55→0.78 over 20 years) because the veto mechanism systematically transfers concessions from majority to blocker without any reciprocal obligation. Blocking is costless (no exit, no penalty for threatening) and credible (institutional rules enforce it). Suppression is moderate-to-high (0.48→0.62) because the majority coalition can technically still act, but only by accepting the blocker's terms — their preference set is constrained by the veto threat, not by explicit coercion, but the effect is suppression of majority will. Theater ratio rises (0.25→0.41) because as blocking becomes routine, procedural theater around 'consensus-building' and 'sensitive sovereignty issues' grows — the formal narrative of protecting minorities conceals the actual dynamic of minoritarian extraction. Accessibility collapse is low (0.48) because alternatives technically exist (qualified majority, treaty reform, enhanced cooperation among coalitions), but the institutional friction cost of switching is prohibitively high, making the stated alternative inaccessible in practice.
 *
 * PERSPECTIVAL GAP:
 *   The blocking state and the majority coalition compute different types from identical structural data. The blocker sees unanimity as enforcement of its veto power (an institutional asset it controls). The majority sees it as structural coercion (their preferred action is blocked unless they pay). The supranational institution experiences it as agenda capture (its policy initiatives diluted by blocking). The engine computes these divergences from the power atoms, exit options, and beneficiary/victim declarations without reconciling them — the gap IS what the per-seat classification measures.
 *
 * DIRECTIONALITY LOGIC:
 *   The blocking minority state is a beneficiary in the raw sense of capturing transfers (concessions, carve-outs, opt-outs), but crucially it is also the agenda-setter (it enforces the veto rule and extracts through strategic use). Its directionality is asymmetrically toward the beneficiary end (d near 0.0–0.2) because it collects without running the coordination machinery; it only threatens. The majority coalition states are payers — they bear the cost of diluted policy and concessions, with moderate leverage but constrained exit. Their d sits near the target end (d near 0.7–0.9). The supranational institution is also a payer (weakened agenda), but its power is moderate and its exit options are analytical only (it cannot leave the EU). The treaty framers and integration advocates are observers; the framers designed the rule (power=institutional but time_horizon=civilizational, exit_options=analytical), and the advocates lack a seat.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandate/function mismatch precisely. The founding mandate was 'protect small states from majoritarian coercion.' The function became 'enable minoritarian states to extract value from majority coalitions through credible blocking.' The original mandate is dead (small states without veto leverage are unprotected; large states with veto leverage capture disproportionate value). The constraint persists not to serve the founding mandate but because benefiting states defend it and the cost of treaty reform is prohibitive. This is a textbook mandatrophy case — the reading declares the mandate obsolete (status=dead) while the constraint persists (world_rearranges if removed). The snare classification captures this: the rule was sold as coordination (protecting minorities) but operates as pure extraction (minoritarian blocking leverages transfers from the majority).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_threat_cost_incidence,
    'Does the blocking state bear any real cost for deploying veto threats, or is the threat costless by institutional design?',
    'Trace outcomes of member-state veto episodes: do blocking states face retaliation, isolation, or reciprocal blocking? Or does the rule enforce costless blocking without consequence?',
    'If veto is truly costless, the constraint is pure snare (asymmetric extraction with no cost to the extractor). If blocking triggers retaliation or reciprocal blocking, the constraint approximates tangled_rope (coordinated with enforcement costs on both sides).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_threat_cost_incidence, empirical, 'Whether veto threats carry institutional or political penalties.').

omega_variable(
    mandate_vs_function_distance,
    'Is unanimity''s function (blocking leverage enabling minoritarian extraction) structurally separable from its stated mandate (protecting small-state sovereignty)?',
    'Counterfactual: would small states without veto leverage (non-strategic members) still have defensible sovereignty under qualified majority voting? Does the mandate actually require unanimity, or would alternative voting thresholds achieve the same protective function?',
    'If separable, the mandate is dead (small-state protection is decoupled from unanimity; blocking leverage is a side effect, not the mechanism). If inseparable, unanimity is genuinely the only sovereignty protection available and the extraction reading is over-stated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_vs_function_distance, conceptual, 'Whether unanimity''s stated mandate and its extraction mechanism are the same function or different.').

omega_variable(
    kernel_reading_coalescence,
    'Is each reading of the unanimity kernel (veto_trap, sovereignty_guarantor, diplomatic_capital) held by a distinct institutional party, or do the same parties shift between readings strategically?',
    'Discourse analysis of member-state rhetorical positioning: when does a state invoke ''sovereignty protection'' (sovereignty_guarantor) vs. ''extraction opportunity'' (veto_trap) vs. ''consensus legitimacy'' (diplomatic_capital)? Do readings track member position (blocking vs. blocked) or are they genuinely different normative commitments?',
    'If readings track position strategically, unanimity is structurally contested (different seats have irreconcilable interests in the rule). If readings are normative commitments independent of position, unanimity may be genuinely ambiguous (legitimately framable multiple ways). The composition of these determines whether treaty reform is a straightforward value dispute or a deeper institutional contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coalescence, empirical, 'Whether the three kernel readings represent stable commitments or strategic positioning.').

omega_variable(
    small_state_protection_without_unanimity,
    'Can smaller member states'' sovereignty interests be protected against majoritarian harm via voting thresholds other than unanimity (e.g., supermajority, blocking minorities, national veto in defined domains)?',
    'Comparative institutional analysis of other supranational systems (UN Security Council permanent veto, WTO consensus, IMF voting thresholds) and their outcomes for weaker members.',
    'If yes, unanimity is not the necessary mechanism for small-state protection, undermining the sovereignty_guarantor_reading and supporting the veto_trap_reading. If no, unanimity becomes a genuine (though crude) tool for protecting minorities, and the extraction reading must account for the coordination benefit it provides.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_state_protection_without_unanimity, conceptual, 'Whether unanimity is necessary or merely sufficient for small-state protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__veto_trap_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(eu_c_tr_t3, eu_council_unanimity__veto_trap_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement(eu_c_tr_t6, eu_council_unanimity__veto_trap_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(eu_c_tr_t10, eu_council_unanimity__veto_trap_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(eu_c_tr_t15, eu_council_unanimity__veto_trap_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__veto_trap_reading, theater_ratio, 20, 0.41).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__veto_trap_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(eu_c_be_t3, eu_council_unanimity__veto_trap_reading, base_extractiveness, 3, 0.61).
narrative_ontology:measurement(eu_c_be_t6, eu_council_unanimity__veto_trap_reading, base_extractiveness, 6, 0.66).
narrative_ontology:measurement(eu_c_be_t10, eu_council_unanimity__veto_trap_reading, base_extractiveness, 10, 0.73).
narrative_ontology:measurement(eu_c_be_t15, eu_council_unanimity__veto_trap_reading, base_extractiveness, 15, 0.76).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__veto_trap_reading, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__veto_trap_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(eu_c_su_t3, eu_council_unanimity__veto_trap_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(eu_c_su_t6, eu_council_unanimity__veto_trap_reading, suppression_requirement, 6, 0.56).
narrative_ontology:measurement(eu_c_su_t10, eu_council_unanimity__veto_trap_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement(eu_c_su_t15, eu_council_unanimity__veto_trap_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(eu_c_su_t20, eu_council_unanimity__veto_trap_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__veto_trap_reading, 0.12).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__diplomatic_capital_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, qualified_majority_voting_alternative).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, enhanced_cooperation_opt_out_mechanism).

% DUAL FORMULATION NOTE:
% The EU Council unanimity rule generates three structurally distinct constraints depending on the reading: (1) veto_trap_reading (this file) — unanimity as minoritarian extraction via credible blocking threats, high extractiveness, snare classification; (2) sovereignty_guarantor_reading — unanimity as protection against majoritarian coercion, lower extractiveness, rope/coordination classification; (3) diplomatic_capital_reading — unanimity as requirement for iterative consensus-building and policy legitimacy, mixed extractiveness. Each reading interprets the same kernel (the unanimity rule) differently and generates a different ε value. They are linked by affects_constraints. The choice of reading is not observable-dependent (same rule, same observable outcomes) but reading-dependent (which aspect of the rule is focal: blocking power, sovereignty protection, or consensus requirement). The veto_trap_reading focuses on blocking leverage and its extraction effects; the other readings emphasize the coordination or protective functions. Do not merge these into one constraint with a 'measurement basis' parameter — they are structurally distinct constraints with different stakeholder asymmetries and different types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
