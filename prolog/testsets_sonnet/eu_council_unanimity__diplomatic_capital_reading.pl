% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__diplomatic_capital_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__diplomatic_capital_reading, []).

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
 *   constraint_id: eu_council_unanimity__diplomatic_capital_reading
 *   human_readable: EU Council Unanimity as Consensus-Building / Legitimacy Mechanism
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This story instantiates the diplomatic-capital reading of the EU Council
 *   unanimity kernel: unanimity as a consensus-forcing coordination device
 *   whose iterative-negotiation cost purchases policy legitimacy and
 *   durability. Under this reading there is no fixed extractive
 *   beneficiary/victim pair — the coordination cost (time, flexibility) is
 *   broadly shared and the payoff (reduced downstream defection, broader
 *   compliance, genuine buy-in from smaller states) is broadly shared as
 *   well. This is a distinct constraint from the
 *   sovereignty_guarantor_reading (which treats unanimity as a
 *   rights-protection mechanism against majoritarian coercion) and the
 *   veto_trap_reading (which treats the same formal rule as a structural
 *   vulnerability enabling minoritarian extraction via blocking threats). All
 *   three share a kernel — the formal unanimity requirement in Council
 *   decision-making — but instantiate structurally different constraints with
 *   different epsilon values, different beneficiary/victim structures, and
 *   different persistence logics. This story's epsilon is low and stable
 *   because, under this reading, the negotiation cost is a coordination
 *   expense that a legitimacy-maximizing framework treats as necessary
 *   overhead, not extraction.
 *
 * KEY AGENTS:
 *   - member_states_collectively: primary beneficiary of durable, consented outcomes
 *   - council_negotiators: agenda-setters whose professional value depends on producing genuine compromise
 *   - small_member_states: structural beneficiaries who gain leverage they would lack under majority rule
 *   - large_member_states: bear most of the negotiation time cost, receive durability in return
 *   - eu_citizens: analytical observers experiencing downstream policy stability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.22).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.18).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity as Consensus-Building / Legitimacy Mechanism").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "institutional_design/international_relations/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, '18cab0ea-d2ba-4d7c-968a-26af05f03acb').
narrative_ontology:cs_kernel_codification('18cab0ea-d2ba-4d7c-968a-26af05f03acb', formalized).
narrative_ontology:cs_authority_grounding('18cab0ea-d2ba-4d7c-968a-26af05f03acb', practice).
narrative_ontology:cs_interpretation_layer_present('18cab0ea-d2ba-4d7c-968a-26af05f03acb').
narrative_ontology:cs_reading_relation('18cab0ea-d2ba-4d7c-968a-26af05f03acb', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('18cab0ea-d2ba-4d7c-968a-26af05f03acb', eu_council_unanimity__veto_trap_reading, influences).
narrative_ontology:cs_axiom('18cab0ea-d2ba-4d7c-968a-26af05f03acb', foundational, costly_consensus_produces_durable_legitimacy).
narrative_ontology:cs_axiom_status(costly_consensus_produces_durable_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('18cab0ea-d2ba-4d7c-968a-26af05f03acb', costly_consensus_produces_durable_legitimacy, instrumental).
narrative_ontology:cs_axiom('18cab0ea-d2ba-4d7c-968a-26af05f03acb', secondary, iterative_negotiation_is_coordination_not_extraction).
narrative_ontology:cs_axiom_status(iterative_negotiation_is_coordination_not_extraction, holdable).
narrative_ontology:cs_axiom_grounding('18cab0ea-d2ba-4d7c-968a-26af05f03acb', iterative_negotiation_is_coordination_not_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('18cab0ea-d2ba-4d7c-968a-26af05f03acb', consensus_seeking_intergovernmental_bargaining).
narrative_ontology:cs_drift_state('18cab0ea-d2ba-4d7c-968a-26af05f03acb', post_enlargement_contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('18cab0ea-d2ba-4d7c-968a-26af05f03acb', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, member_states_collectively).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, council_negotiators).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, small_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, large_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, large_member_states).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, durable_consensus_outperforms_imposed_majority_rule).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in extended negotiation rounds on treaty-level and sensitive-policy decisions. Each state's assent is required, which forces drafters to accommodate objections before a vote is even called. The resulting policy carries the imprimatur of every government having actively signed on, which reduces the likelihood any one state quietly sabotages implementation later.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, member_states_collectively, beneficiary,
    organized, generational, constrained, continental).

% Chair and broker the iterative bargaining rounds, shuttle compromise text between capitals, and build the package deals that let every delegation claim a win. Their professional currency is the durable compromise; a unanimity requirement that produces resilient outcomes is what makes their diplomatic labor valuable rather than performative.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, council_negotiators, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, council_negotiators, beneficiary).

% Would be structurally outvoted under simple majority or population-weighted schemes on many sensitive files. The unanimity requirement guarantees a seat at the table and a genuine say in the final text, converting formal equality into substantive negotiating leverage they would not otherwise carry.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, small_member_states, beneficiary,
    moderate, biographical, constrained, national).

% Bear most of the time cost of iterative negotiation and must sometimes soften preferred positions to secure the assent of smaller delegations. In exchange, they get policy outcomes far less likely to be litigated, defected from, or reversed at implementation than a majority-imposed rule would be — the delay is priced against durability.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, large_member_states, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, large_member_states, beneficiary).

% Experience the downstream effect of policies that took longer to negotiate but are less frequently reversed or unevenly enforced. They have no direct seat in Council negotiation but benefit from the reduced volatility of unanimously-agreed frameworks compared to contested majority impositions.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_citizens, observer,
    moderate, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Forces every affected government to actively assent before a collective decision binds it, which surfaces objections early, compels package-deal compromise, and produces outcomes with broad ex-ante buy-in rather than ex-post grudging compliance.
% TRANSFER_FUNCTION: Moves negotiating time and short-term flexibility from all parties (especially large states, who could otherwise move faster) into durability and legitimacy that accrues to the resulting policy and, derivatively, to every state that must live under it.
% ABSENT_VOICES: Sub-national regions and civil society groups within member states have no direct seat in Council bargaining; their preferences are filtered entirely through their national government's negotiating position, so intra-state minority views can be lost even as inter-state consensus is achieved.
% DISAPPEARANCE_RATIONALE: If unanimity were replaced overnight by simple majority voting on the same set of decisions, some observers argue the EU would move faster with little durability loss (world_unchanged in practice); others argue several member states would face policy imposed without their consent, triggering non-compliance, treaty renegotiation demands, or exit pressure (world_rearranges). The diplomatic-capital reading holds the latter risk is real but that the mechanism's principal value — durability through buy-in — would be lost either way, which is why this reading treats the question as genuinely contested rather than settled in either direction.
% FOUNDING_PROBLEM: Early European integration required sovereign states with no supranational enforcement tradition to bind themselves jointly; without a mechanism guaranteeing each state real influence over outcomes that touched core national interests, governments would have had no incentive to enter or remain in the arrangement at all.
% FOUNDING_PROBLEM_CORROBORATION: Independent comparative-institutions scholarship (studies contrasting EU unanimity-negotiated outcomes with QMV-imposed outcomes on compliance and reversal rates) and testimony from national parliaments ratifying treaties attest that the consent-seeking function remains operative, not merely a legacy formality — this corroboration source sits outside the Council negotiators who directly benefit from the mechanism's continuation.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, contested).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__diplomatic_capital_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__diplomatic_capital_reading_tests).
:- end_tests(eu_council_unanimity__diplomatic_capital_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) and rising only marginally over the interval, reflecting the reading's claim that negotiation cost is coordination overhead rather than rent extraction — there is no identifiable party siphoning value through the mechanism under this reading. Suppression is low (0.18): no party is coerced into assent by anything beyond the ordinary pressure of wanting a functioning collective agreement, and any state retains formal exit (leaving the negotiation, invoking reservations, or in the extreme case withdrawing from the Union). Theater ratio is low and only slowly rising (0.10 to 0.15), reflecting that the negotiation labor is substantially real rather than performative, though some ritualization of package-deal choreography has crept in over successive enlargement rounds.
 *
 * DIRECTIONALITY LOGIC:
 *   No single party is positioned as a clean beneficiary or victim under this reading — the coordination cost (negotiation time, compromise) is paid by all Council participants in proportion to how much they would otherwise prefer to move fast, and the legitimacy payoff (durability, buy-in, reduced defection) accrues to all participants in proportion to how much they value the outcome sticking. Small member states are named as structural beneficiaries because unanimity converts their formal equality into real leverage they would not hold under population-weighted majority rule; large states are named as payers because they absorb the largest share of the time and flexibility cost, but they are also beneficiaries of durability, which the secondary_role field reflects.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in the following sense: because the founding problem (securing genuine sovereign consent to bind states with no supranational enforcement backstop) remains live per corroboration from outside the Council's own negotiators, the diplomatic-capital function has not obviously atrophied into pure ritual, even as theater_ratio creeps upward with enlargement. The engine's per-seat computation is expected to classify this reading as rope or near-rope from most seats precisely because no seat here is authored as a trapped, high-suppression target — that is the structural signature this reading commits to, distinct from the veto_trap_reading's authored victim structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    durability_causal_attribution,
    'Is the empirically observed lower reversal/defection rate of unanimously-agreed EU policies actually caused by the unanimity requirement''s consensus-forcing function, or is it a selection effect — i.e., only policies that would have been durable anyway are the ones capable of clearing the unanimity bar in the first place?',
    'Comparative institutional study matching policy domains where the EU shifted from unanimity to QMV over time (e.g. certain single-market provisions), tracking reversal and compliance rates before and after the procedural shift, controlling for policy content.',
    'If durability is a selection effect rather than a causal consequence of the negotiation process, the diplomatic-capital reading''s central legitimacy claim weakens substantially and the constraint looks more like the veto_trap_reading''s account of the same formal rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(durability_causal_attribution, empirical, 'Whether unanimity causes durability or merely selects for policies that would have been durable regardless.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the choice among the diplomatic_capital, sovereignty_guarantor, and veto_trap readings itself resolvable by evidence, or does the same formal unanimity rule genuinely support all three readings simultaneously depending on which policy file, era, and coalition configuration is examined?',
    'Case-level coding of individual unanimity votes across policy areas and decades, classifying each instance by whether the outcome pattern matches consensus-building (broad buy-in, low reversal), sovereignty-protection (blocked action that infringed core national competence), or veto-trap (narrow minoritarian holdout extracting side payments).',
    'If case-level evidence shows the three patterns cluster by policy domain or era rather than being uniformly present, that would argue for narrower-scoped kernel decomposition (e.g. distinguishing treaty-amendment unanimity from CFSP unanimity) rather than three fully general competing readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the three sibling readings are genuinely co-extensive with the same kernel or should be scoped to different policy domains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(eu_c_tr_t5, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(eu_c_tr_t10, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(eu_c_tr_t15, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 15, 0.13).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(eu_c_tr_t25, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 25, 0.15).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(eu_c_be_t5, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 5, 0.19).
narrative_ontology:measurement(eu_c_be_t10, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(eu_c_be_t15, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 15, 0.21).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 20, 0.21).
narrative_ontology:measurement(eu_c_be_t25, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 25, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(eu_council_unanimity__diplomatic_capital_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__diplomatic_capital_reading, 0.12).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__veto_trap_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the eu_council_unanimity kernel. diplomatic_capital_reading (this file) claims low epsilon with no fixed victim structure; sovereignty_guarantor_reading claims the same formal rule protects sovereign consent as an intrinsic value; veto_trap_reading claims the same formal rule enables minoritarian extraction with named victims among blocked majorities. Each carries its own epsilon and classification; none averages over the others. Decomposition follows the ε-invariance principle: the natural-language label 'EU unanimity requirement' conflates three structurally distinct claims about the same procedural kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
