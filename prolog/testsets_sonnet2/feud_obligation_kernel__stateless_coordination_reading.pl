% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__stateless_coordination_reading, []).

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
 *   constraint_id: feud_obligation_kernel__stateless_coordination_reading
 *   human_readable: Blood-Feud Obligation as Stateless Justice Coordination
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This story authors the stateless-coordination reading of the
 *   feud-obligation kernel: from within this reading's own lights, blood-feud
 *   obligation is a rational, low-overhead equilibrium that substitutes for
 *   absent state capacity, using the credible threat of proportionate
 *   retaliation (with wergild as a negotiated off-ramp) to deter aggression
 *   and resolve disputes. This is a deliberately narrow reading — it does not
 *   describe the Christianized-pacification reading (which treats the same
 *   practice as usurping divinely delegated authority over violence) or the
 *   extraction-cycle reading (which treats it as a destructive rent-cycle
 *   that depletes productive capacity and blocks consolidation). Those are
 *   separate constraints with their own ε values, linked here via
 *   network.affects_constraints. Under this reading's own lights, extraction
 *   is low (0.32): the primary flow is deterrence and redress among
 *   consenting kin-network participants, not rent extraction from a captive
 *   population, and wergild as a genuine coexisting alternative keeps
 *   accessibility collapse only moderate (0.4).
 *
 * KEY AGENTS:
 *   - kin_group_members: organized/constrained — dual beneficiary/administrator of the coordination norm
 *   - feud_participants: moderate/constrained — direct parties in an active cycle, wergild remains available
 *   - prospective_victims_of_would_be_aggressors: powerless/constrained — beneficiaries of ambient deterrence
 *   - feud_defectors: powerless/trapped — bear honor loss and expulsion cost for non-participation
 *   - kinless_outsiders: powerless/trapped — structurally undefended by a kin-indexed mechanism
 *   - local_chieftains_and_elders: moderate/mobile — mediate settlements without commanding enforcement
 *   - comparative_legal_historians: analytical/analytical — external corroborating observers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.32).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.28).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligation as Stateless Justice Coordination").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/medieval_history/comparative_political_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, '4d4bbe5b-8bf5-497d-a744-6ed54635013b').
narrative_ontology:cs_kernel_codification('4d4bbe5b-8bf5-497d-a744-6ed54635013b', distributed).
narrative_ontology:cs_authority_grounding('4d4bbe5b-8bf5-497d-a744-6ed54635013b', practice).
narrative_ontology:cs_interpretation_layer_present('4d4bbe5b-8bf5-497d-a744-6ed54635013b').
narrative_ontology:cs_reading_relation('4d4bbe5b-8bf5-497d-a744-6ed54635013b', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d4bbe5b-8bf5-497d-a744-6ed54635013b', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('4d4bbe5b-8bf5-497d-a744-6ed54635013b', foundational, self_enforcement_substitutes_for_absent_state_capacity).
narrative_ontology:cs_axiom_status(self_enforcement_substitutes_for_absent_state_capacity, holdable).
narrative_ontology:cs_axiom_grounding('4d4bbe5b-8bf5-497d-a744-6ed54635013b', self_enforcement_substitutes_for_absent_state_capacity, instrumental).
narrative_ontology:cs_axiom('4d4bbe5b-8bf5-497d-a744-6ed54635013b', secondary, wergild_availability_negates_pure_coercion_reading).
narrative_ontology:cs_axiom_status(wergild_availability_negates_pure_coercion_reading, holdable).
narrative_ontology:cs_axiom_grounding('4d4bbe5b-8bf5-497d-a744-6ed54635013b', wergild_availability_negates_pure_coercion_reading, empirically_contingent).
narrative_ontology:cs_reference_frame('4d4bbe5b-8bf5-497d-a744-6ed54635013b', kin_based_reciprocal_deterrence_equilibrium).
narrative_ontology:cs_drift_state('4d4bbe5b-8bf5-497d-a744-6ed54635013b', early_state_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4d4bbe5b-8bf5-497d-a744-6ed54635013b', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, kin_group_members).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, feud_participants).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, prospective_victims_of_would_be_aggressors).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, feud_defectors).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, kinless_outsiders).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, reciprocal_deterrence_theory).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, self_enforcing_norm_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Belongs to a kin network that both protects its members through the credible threat of collective retaliation and requires its members to participate in that retaliation when called upon. Receives protection and standing in exchange for the obligation to avenge or compensate; the arrangement is administered collectively by the kin group itself, with no external enforcer needed.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kin_group_members, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__stateless_coordination_reading, kin_group_members, agenda_setter).

% Directly involved in an active feud cycle — either seeking redress for a killing or injury to kin, or negotiating wergild as an alternative. Gains vindication, restored honor, and a demonstrated deterrent reputation that discourages future aggression against the kin group. Wergild remains an available off-ramp at every stage; nothing structurally blocks settlement.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_participants, beneficiary,
    moderate, biographical, constrained, local).

% Never directly enters a feud but benefits from the deterrent shadow the system casts — potential aggressors calculate the cost of retaliation before acting. In a landscape with no police or courts, this ambient deterrence is the primary source of personal safety.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, prospective_victims_of_would_be_aggressors, beneficiary,
    powerless, generational, constrained, local).

% A kin-group member who refuses to participate in a called retaliation or refuses to accept wergild in settlement of a claim against them. Loses honor standing, faces exclusion from the mutual-protection network, and in severe cases is expelled from kinship — losing the very protection the system exists to provide. Cannot simply walk away from the obligation without incurring this cost.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_defectors, payer,
    powerless, biographical, trapped, local).

% Travelers, orphans, foreigners, or the estranged who lack a kin group able to mount credible retaliation on their behalf. Because deterrence in this system runs entirely through kinship, an injury to a kinless person carries little cost to the aggressor — they are structurally undefended by the very mechanism that protects everyone else.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kinless_outsiders, payer,
    powerless, biographical, trapped, local).

% Mediate wergild negotiations, interpret customary norms of proportional retaliation, and sometimes broker settlements between feuding kin groups. They administer the informal rules without commanding an enforcement apparatus of their own; their authority rests entirely on the parties' willingness to defer to a recognized mediator.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, local_chieftains_and_elders, agenda_setter,
    moderate, generational, mobile, local).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__stateless_coordination_reading, local_chieftains_and_elders, observer).

% Study feud systems across stateless and weak-state societies to evaluate whether they function as genuine self-enforcing equilibria providing order, or whether the coordination story understates their costs and coercive character.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__stateless_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__stateless_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In the absence of a centralized enforcer capable of investigating wrongs and compelling compensation, kin-based feud obligation creates a credible, decentralized deterrence and dispute-resolution mechanism: the expectation of proportionate retaliation (or negotiated wergild) makes aggression costly and gives injured parties a path to redress without requiring a state.
% TRANSFER_FUNCTION: Moves the cost of deterrence and dispute resolution from a hypothetical central authority (which does not exist in this setting) onto kin groups themselves, who bear the risk, labor, and occasional lethal cost of enforcement collectively. Compensation (wergild) or violence moves between the aggressor's kin group and the victim's kin group.
% ABSENT_VOICES: Kinless outsiders and marginal dependents (unmarried women without male kin advocates, the very poor, foreigners) have no seat in the negotiation of norms and bear the full cost of being outside the deterrence umbrella; they would argue for a universal, kin-independent enforcement mechanism, but no such mechanism exists in this setting for them to appeal to.
% DISAPPEARANCE_RATIONALE: If feud obligation vanished overnight with no substitute enforcement mechanism, the deterrence function it provides would disappear with it — aggression against persons and property would become cost-free absent a replacement authority, and kin groups would either revert to ad hoc violence or rapidly reconstruct an equivalent norm. The wergild institutions that coexist with feud would have to absorb the entire dispute-resolution load without the underlying threat that currently makes negotiated settlement attractive to both sides.
% FOUNDING_PROBLEM: In stateless or weak-state societies, no external authority exists with the investigative capacity, monopoly on legitimate force, or willingness to punish wrongs against individuals; without some mechanism, aggression against a person or their kin carries no reliable cost.
% FOUNDING_PROBLEM_CORROBORATION: Comparative legal historians studying Icelandic sagas, early Germanic law codes, and contemporary stateless pastoral societies (e.g., Somali xeer, highland New Guinea) attest that feud and its wergild alternative functioned as the primary order-maintaining mechanism where state capacity was genuinely absent — this corroboration comes from scholarship outside the kin groups themselves, not from the feuding parties' own account of their honor system.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__stateless_coordination_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__stateless_coordination_reading_tests).
:- end_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.32) because, under this reading, the primary structural flow is reciprocal deterrence and negotiated redress among kin-network members who are net beneficiaries of the arrangement — not a one-directional transfer to a narrow elite. Suppression is authored low-moderate (0.28) because wergild persists as a live, uncoerced alternative at every stage of a feud cycle; the mechanism does not need to foreclose alternatives to function, which is precisely what distinguishes this reading from the extraction-cycle reading's account of the same practice. Resistance (0.35) and accessibility_collapse (0.4) are moderate rather than low because defection carries a real cost (honor loss, kinship expulsion) and kinless outsiders are structurally excluded from the deterrence umbrella — these are the reading's own acknowledged victim class, not smoothed over.
 *
 * DIRECTIONALITY LOGIC:
 *   Kin-group members, feud participants, and prospective beneficiaries of ambient deterrence sit near the beneficiary end of directionality: the arrangement is one they built, administer, and collect protection/redress from. Feud defectors and kinless outsiders sit near the target end: defectors are punished by the very mechanism meant to protect kin, and kinless outsiders receive none of the deterrent shadow because the mechanism is kinship-indexed rather than universal. This is the structural delta this reading commits to: participants (not the population at large) are the beneficiary set, and defectors/outsiders (not a ruling elite) are the victim set — a materially different beneficiary/victim structure than either sibling reading would author.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — absence of centralized enforcement capacity — is genuinely contested as either live or dead depending on the historical moment being examined: in a stateless pastoral society it remains fully live; in a weak but consolidating early state it may be transitioning toward dead as royal or ecclesiastical courts begin to substitute. This reading does not resolve that transition; it describes the mechanism's operation while the founding problem remains at least partially live, which is why founding_problem_status is authored as contested rather than flatly live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary_across_readings,
    'Does the feud system''s low measured extraction reflect a genuinely low-overhead equilibrium, or does the stateless-coordination reading undercount productive capacity destroyed by feud cycles that the extraction_cycle_reading captures directly?',
    'Comparative quantitative study of agricultural output, population stability, and capital accumulation in feud-active versus feud-suppressed periods within the same society, controlling for external shocks.',
    'If productive-capacity destruction is substantial and systemic rather than episodic, this reading''s low ε may understate the practice''s true cost even by its own coordination-function logic, strengthening the case for treating the two readings as empirically adjudicable rather than purely perspectival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary_across_readings, empirical, 'Whether feud''s low extraction score survives scrutiny against extraction-cycle evidence of productive loss.').

omega_variable(
    kin_indexed_exclusion_severity,
    'How severe and how common was the structural exclusion of kinless outsiders from feud-based deterrence, across the range of societies this reading generalizes over?',
    'Cross-cultural survey of feud societies (Icelandic, Albanian kanun, Somali xeer, highland Papua New Guinea) measuring rates of violence against kinless individuals relative to kin-embedded individuals.',
    'If exclusion of kinless outsiders was severe and widespread, the victim set for this reading should be weighted more heavily, which would push the computed classification toward tangled_rope even within this reading''s own framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kin_indexed_exclusion_severity, empirical, 'How large and consistent the kinless-outsider victim class actually was.').

omega_variable(
    kernel_framing_choice,
    'Is the coordination-function framing (deterrence economics) the most defensible lens for this practice, or does the christianized_pacification_reading''s legitimacy-authority framing better capture what contemporaries themselves believed was at stake?',
    'Textual analysis of contemporaneous sources (saga literature, canon law commentary, royal capitularies) to determine which framing dominates period self-understanding versus modern functionalist reconstruction.',
    'If period actors overwhelmingly framed feud in terms of divine/legitimate-authority questions rather than efficiency questions, this reading''s functionalist framing may be an anachronistic overlay — worth flagging even though it does not change this story''s own internally consistent ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the efficiency/coordination framing is a modern analytical import rather than the practice''s own self-understanding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(feud_tr_t60, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement(feud_tr_t80, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 20, 0.29).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(feud_be_t60, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 60, 0.31).
narrative_ontology:measurement(feud_be_t80, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 80, 0.31).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 100, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(feud_obligation_kernel__stateless_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__stateless_coordination_reading, 0.12).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the feud_obligation_kernel. stateless_coordination_reading (this file) authors low ε and a participant/defector beneficiary-victim structure. extraction_cycle_reading authors high ε with a productive-population victim set (destructive rent cycle). christianized_pacification_reading authors the practice as a legitimacy violation against a divinely-delegated authority structure rather than an efficiency claim at all. All three share the same underlying practice but are structurally distinct constraints per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
