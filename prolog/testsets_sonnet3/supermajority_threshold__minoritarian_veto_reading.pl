% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Constitutional Amendment Supermajority Threshold as Minoritarian Veto
 *   domain: constitutional/political_economy
 *
 * SUMMARY:
 *   This story instantiates the minoritarian-veto reading of the
 *   supermajority-threshold kernel: the same counting rule that
 *   consensus-safeguard readers describe as protecting deep democratic
 *   consensus is here read as a mechanism that freezes a historical
 *   distributional settlement into a standing, self-renewing veto. Under this
 *   reading, the threshold's justification (testing durable consensus) is a
 *   cover story; the actual operation is that a minority bloc, whose blocking
 *   weight derives from population or territorial arithmetic fixed at a
 *   founding moment, can indefinitely refuse ratification of reforms that
 *   command sustained numerical majorities. The rising extraction and
 *   suppression series model how each failed ratification cycle further
 *   consolidates the blocking coalition's position — the coalition learns to
 *   organize precisely to the threshold, and enforcement of the counting rule
 *   (certification procedures, judicial deference to the ratification math)
 *   hardens over successive amendment attempts. This is one of three linked
 *   readings of the same kernel; the consensus_safeguard_reading and
 *   adaptive_gradient_reading are separate constraint stories with their own
 *   ε and stakeholder structure — this file does not average across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.71).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.68).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Constitutional Amendment Supermajority Threshold as Minoritarian Veto").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional/political_economy").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, '0b2d7325-235b-43db-ab3f-0224c22069f1').
narrative_ontology:cs_kernel_codification('0b2d7325-235b-43db-ab3f-0224c22069f1', fixed_text).
narrative_ontology:cs_authority_grounding('0b2d7325-235b-43db-ab3f-0224c22069f1', extraction).
narrative_ontology:cs_interpretation_layer_present('0b2d7325-235b-43db-ab3f-0224c22069f1').
narrative_ontology:cs_reading_relation('0b2d7325-235b-43db-ab3f-0224c22069f1', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b2d7325-235b-43db-ab3f-0224c22069f1', supermajority_threshold__adaptive_gradient_reading, influences).
narrative_ontology:cs_axiom('0b2d7325-235b-43db-ab3f-0224c22069f1', foundational, counting_rule_disconnected_from_present_consensus).
narrative_ontology:cs_axiom_status(counting_rule_disconnected_from_present_consensus, holdable).
narrative_ontology:cs_axiom_grounding('0b2d7325-235b-43db-ab3f-0224c22069f1', counting_rule_disconnected_from_present_consensus, empirically_contingent).
narrative_ontology:cs_axiom('0b2d7325-235b-43db-ab3f-0224c22069f1', foundational, historical_privilege_illegitimately_perpetuated_by_procedure).
narrative_ontology:cs_axiom_status(historical_privilege_illegitimately_perpetuated_by_procedure, holdable).
narrative_ontology:cs_axiom_grounding('0b2d7325-235b-43db-ab3f-0224c22069f1', historical_privilege_illegitimately_perpetuated_by_procedure, deontological).
narrative_ontology:cs_reference_frame('0b2d7325-235b-43db-ab3f-0224c22069f1', founding_era_distributional_bargain).
narrative_ontology:cs_drift_state('0b2d7325-235b-43db-ab3f-0224c22069f1', contemporary_national_majority_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0b2d7325-235b-43db-ab3f-0224c22069f1', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_rural_state_coalitions).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_incumbent_elites).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, malapportioned_legislative_blocs).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_national_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, underrepresented_urban_populations).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, reform_movements_blocked_by_veto).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls enough legislative seats, disproportionate to population, to block any amendment requiring supermajority ratification. Uses the threshold not to test consensus but to hold a permanent veto over any change that would dilute its own disproportionate weight. Faces no exit cost because the threshold protects the bloc's structural position indefinitely.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, malapportioned_legislative_blocs, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__minoritarian_veto_reading, malapportioned_legislative_blocs, beneficiary).

% Benefits from the current constitutional settlement — property arrangements, electoral maps, institutional privileges — and finances the political infrastructure that keeps the blocking coalition mobilized at ratification time. Can relocate assets or influence if the settlement changes, but has no incentive to let it.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, status_quo_incumbent_elites, beneficiary,
    powerful, generational, mobile, national).

% A minority of the national population organized at the state or subunit level, holding blocking power under the threshold's counting rule regardless of population share. Ratification math means their assent (or refusal) is worth several multiples of an equivalent urban vote; they exercise this leverage to withhold consent from amendments that would reduce it.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, entrenched_rural_state_coalitions, beneficiary,
    organized, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__minoritarian_veto_reading, entrenched_rural_state_coalitions, agenda_setter).

% Repeatedly assembles numerical majorities in favor of specific reforms — voting rights expansion, apportionment correction, structural rebalancing — that fail ratification solely because the threshold requires assent from blocs representing a fraction of the population. Has no exit from the jurisdiction and no lawful path to amendment other than the very process controlled by the blocking minority.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_national_majorities, payer,
    organized, biographical, trapped, national).

% Concentrated in jurisdictions whose votes count for less in the ratification arithmetic than dispersed rural votes of equal number. Bears the cost of unrealized reforms — representation, resource allocation, rights protections — that would pass under simple-majority rules but fail under the threshold, indefinitely, with no individual recourse.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, underrepresented_urban_populations, payer,
    powerless, biographical, trapped, national).

% Builds sustained multi-decade coalitions to enact structural change, only to see ratification fail below the threshold each cycle. Cannot appeal to any body above the amendment process itself; each failed cycle resets the clock while the blocking coalition's position does not erode.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, reform_movements_blocked_by_veto, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__minoritarian_veto_reading, reform_movements_blocked_by_veto, excluded).

% Trace the threshold's origin to a historical settlement between unequal founding parties and track how its counting rule has, over successive amendment cycles, correlated with which reforms succeed and which permanently fail — providing the evidentiary record for whether the blocking pattern is structural rather than incidental.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__minoritarian_veto_reading, malapportioned_legislative_blocs).
narrative_ontology:fixing_cost_class(supermajority_threshold__minoritarian_veto_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None functions genuinely under this reading: the nominal coordination story — that supermajority assent proves durable consensus — is read here as cover for a counting rule that converts a historical population or territorial imbalance into a standing veto disconnected from any live consensus-testing function.
% TRANSFER_FUNCTION: Moves decision-making power from the numerical majority to a fixed minority bloc whose blocking weight derives from historical apportionment rather than present-day population share; the transfer is political control over constitutional change itself, indefinitely renewed each ratification cycle.
% ABSENT_VOICES: Underrepresented urban populations and successive reform coalitions are structurally present as voters but their votes are systematically discounted by the counting rule; they object at every ratification attempt and are heard, but never counted at parity — their objection is procedurally legible and substantively powerless.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold were replaced overnight with simple-majority ratification, the reforms that have failed repeatedly for decades — apportionment correction, voting rights expansion, structural rebalancing — would very likely pass, and the blocking coalitions' leverage over the amendment process would collapse entirely; the entrenched blocs would lose the specific asset (veto power disproportionate to population) that the threshold currently guarantees them.
% FOUNDING_PROBLEM: The threshold was set at a founding moment to secure the assent of parties who held disproportionate power at the time and would not have joined the constitutional settlement without a guarantee that later majorities could not simply outvote their interests.
% FOUNDING_PROBLEM_CORROBORATION: Independent constitutional historians and comparative-institutions scholars outside the entrenched blocs attest that the population and territorial imbalances the threshold was calibrated to protect no longer track any live distributive concern of the parties it originally shielded; the beneficiary blocs themselves assert the founding rationale is still live, but this attestation comes only from the parties who currently hold the veto.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__minoritarian_veto_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71) and suppression (0.68) are authored high because, on this reading, the threshold's operation is measured by its actual output: decades of numerically-majoritarian reform proposals failing ratification while the blocking bloc's structural position never erodes. Theater ratio (0.42) reflects that some genuine deliberative process still occurs at each ratification attempt even though, on this reading, the deliberation cannot alter the outcome the counting rule predetermines. Accessibility collapse (0.6) and resistance (0.74) are authored to reflect that formal alternative paths (further amendment attempts, litigation, convention calls) nominally exist but have never succeeded, while resistance from blocked majorities is high and organized every cycle.
 *
 * PERSPECTIVAL GAP:
 *   From the blocking bloc's seat, the threshold looks like principled caution against transient majoritarian passion — exactly the consensus_safeguard framing. From the blocked-majority seat, the identical structure looks like a permanent minority veto dressed in deliberative language. The engine computes these as different seat-level classifications from the same structural data; this story authors the reading in which the veto function dominates and the safeguard story is understood as legitimating cover.
 *
 * DIRECTIONALITY LOGIC:
 *   Malapportioned legislative blocs and entrenched rural state coalitions are the structural beneficiaries: their d sits near the full-beneficiary end because the counting rule directly converts their disproportionate representation into leverage they did not have to earn through numbers. Status quo incumbent elites benefit indirectly by financing and mobilizing the blocking coalition. Contemporary national majorities, underrepresented urban populations, and reform movements are targets: trapped or constrained exit options combined with victim declarations push their d toward the full-target end — they cannot leave the jurisdiction and cannot out-organize a counting rule that discounts their votes by construction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored dead: the specific distributional imbalance the threshold was calibrated to protect at the founding moment has, on this reading, dissolved or shifted, while the counting rule that protected it persists unchanged. This is the classic mandatrophy signature — mandate outlived function, structure retained. Classifying this as a snare under the veto reading (rather than treating it as an unresolved rope) prevents the coordination story from perpetually excusing an arrangement whose founding rationale independent historians report as no longer live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_vs_safeguard_framing_indeterminacy,
    'Is the supermajority counting rule, as actually operated, functioning as a genuine test of durable cross-population consensus, or as a mechanism that converts a fixed historical distributional advantage into a standing veto disconnected from present consensus formation?',
    'Compare ratification outcomes against independent measures of contemporaneous public consensus (repeated cross-sectional polling, referendum results where available) over multiple amendment cycles; if ratification failure rates track blocking-bloc composition rather than measured public consensus shifts, the veto reading is empirically favored over the safeguard reading.',
    'If the veto reading is empirically vindicated, this story''s snare classification and high ε are structurally supported; if the safeguard reading is vindicated instead, this reading''s claimed_type should be treated as the delegitimating minority position rather than the dominant structural account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_vs_safeguard_framing_indeterminacy, conceptual, 'Whether the threshold''s operation is best read as consensus-testing or as fixed-minority veto; the two sibling readings instantiate opposite answers.').

omega_variable(
    historical_privilege_persistence,
    'Does the population/territorial imbalance that originally justified extra weight for the blocking bloc still track any live distributive or protective concern, or has it become pure inertial advantage?',
    'Demographic and economic analysis comparing the founding-era rationale (protecting a genuinely vulnerable minority interest) against the present composition and resources of the blocking bloc.',
    'If the imbalance no longer tracks a live protective concern, the founding_problem_status of dead is corroborated and the snare classification strengthens; if a live concern persists, the tangled_rope or scaffold framing becomes more defensible even within this reading''s own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_privilege_persistence, empirical, 'Whether the historical rationale for extra weight has decayed into pure entrenchment.').

omega_variable(
    reform_movement_coalition_durability,
    'Could blocked majorities, organized as a coalition of reform movements and underrepresented populations, ever assemble enough concentrated power to overcome the threshold through means other than the amendment process itself (e.g., sustained electoral realignment, judicial reinterpretation)?',
    'Track multi-decade case studies of successful versus failed structural reform campaigns operating under comparable supermajority thresholds in other jurisdictions.',
    'If such coalitions have historically succeeded elsewhere, the trapped exit_options authored for contemporary_national_majorities may overstate the closure; if they have not, the trapped characterization is corroborated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_movement_coalition_durability, empirical, 'Whether blocked-majority coalitions have any realistic non-amendment path to overcome threshold entrenchment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(supe_tr_t50, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(supe_tr_t60, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(supe_be_t50, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(supe_be_t60, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 60, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(supe_su_t30, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(supe_su_t50, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 50, 0.66).
narrative_ontology:measurement(supe_su_t60, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the supermajority_threshold kernel. consensus_safeguard_reading models the identical counting rule as a rope (genuine consensus-testing function, no identifiable victims); adaptive_gradient_reading models it as a scaffold-like tool whose legitimacy depends on empirical calibration to consensus-formation rates. This story (minoritarian_veto_reading) models it as a snare with named beneficiaries and victims. All three share the same textual kernel and structural facts about the counting rule; they differ in which reading of beneficiary/victim structure and founding-problem status is authored as true. Contamination propagation: if empirical evidence corroborates the veto reading over multiple cycles, downstream confidence in the safeguard reading's legitimacy should erode; this is the mechanism the affects_constraints edge is meant to expose to the network layer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
