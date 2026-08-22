% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__diplomatic_capital_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: EU Council Unanimity — Consensus-Building and Legitimacy Reading
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This story generates the diplomatic-capital reading of the EU Council
 *   unanimity kernel: unanimity as a coordination device whose costly,
 *   iterative negotiation process manufactures durable legitimacy. On this
 *   reading there is no fixed victim class — the negotiation burden is
 *   distributed and reciprocal, and the payoff (higher compliance, lower
 *   post-decision defection, reduced domestic backlash for outvoted-feeling
 *   capitals) accrues broadly to all participating states and their publics.
 *   This is one of three readings of the same kernel; the
 *   sovereignty_guarantor_reading treats unanimity as a consent floor
 *   protecting sovereignty rather than a legitimacy-manufacturing negotiation
 *   cost, and the veto_trap_reading treats the identical voting rule as a
 *   structural vulnerability that lets a single minoritarian state extract
 *   concessions through credible blocking threats. Each reading is authored
 *   as its own constraint with its own epsilon; this file does not average
 *   across them.
 *
 * KEY AGENTS:
 *   - member_states_collectively: primary beneficiary of the coordination function — durable, co-authored outcomes
 *   - small_member_states: structural beneficiary — formal parity with larger states on gated matters
 *   - large_member_states: pay in negotiation time and concessions but receive durability in exchange
 *   - council_presidency_rotation_holders: agenda-setter who brokers the iterative process and benefits reputationally from successful consensus
 *   - domestic_publics_of_member_states: downstream beneficiary of legitimacy — receive co-authored rather than imposed policy
 *   - european_commission: observer/excluded — proposes but has no unanimity vote itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.18).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.15).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity — Consensus-Building and Legitimacy Reading").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "institutional_design/international_relations/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, '3a811197-e31e-4986-b354-cfdcf9aef472').
narrative_ontology:cs_kernel_codification('3a811197-e31e-4986-b354-cfdcf9aef472', formalized).
narrative_ontology:cs_authority_grounding('3a811197-e31e-4986-b354-cfdcf9aef472', practice).
narrative_ontology:cs_interpretation_layer_present('3a811197-e31e-4986-b354-cfdcf9aef472').
narrative_ontology:cs_reading_relation('3a811197-e31e-4986-b354-cfdcf9aef472', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a811197-e31e-4986-b354-cfdcf9aef472', eu_council_unanimity__veto_trap_reading, influences).
narrative_ontology:cs_axiom('3a811197-e31e-4986-b354-cfdcf9aef472', foundational, negotiated_buy_in_yields_durable_compliance).
narrative_ontology:cs_axiom_status(negotiated_buy_in_yields_durable_compliance, holdable).
narrative_ontology:cs_axiom_grounding('3a811197-e31e-4986-b354-cfdcf9aef472', negotiated_buy_in_yields_durable_compliance, empirically_contingent).
narrative_ontology:cs_axiom('3a811197-e31e-4986-b354-cfdcf9aef472', secondary, coordination_cost_is_legitimacy_investment_not_extraction).
narrative_ontology:cs_axiom_status(coordination_cost_is_legitimacy_investment_not_extraction, holdable).
narrative_ontology:cs_axiom_grounding('3a811197-e31e-4986-b354-cfdcf9aef472', coordination_cost_is_legitimacy_investment_not_extraction, instrumental).
narrative_ontology:cs_reference_frame('3a811197-e31e-4986-b354-cfdcf9aef472', founding_consent_bargain).
narrative_ontology:cs_drift_state('3a811197-e31e-4986-b354-cfdcf9aef472', post_enlargement_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3a811197-e31e-4986-b354-cfdcf9aef472', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, member_states_collectively).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, small_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, council_presidency_rotation_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, large_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, domestic_publics_of_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, large_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in a negotiation process where every state's assent is required for unanimity-gated decisions (foreign policy, treaty change, taxation, enlargement). The requirement forces sustained bargaining until a text every capital can defend domestically emerges. States can walk from a specific proposal but not from the negotiating table itself if they want continued membership in the arrangement.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, member_states_collectively, beneficiary,
    institutional, generational, constrained, continental).

% Gain a seat at the table equal in formal weight to the largest members on unanimity-gated matters. Without unanimity, their preferences would be structurally outvoted by population-weighted majorities. The requirement is what lets them extract concessions and shape final text rather than simply absorb outcomes.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, small_member_states, beneficiary,
    moderate, generational, constrained, continental).

% Bring more resources to negotiation but must still secure the assent of smaller states to move unanimity-gated proposals forward. They pay in negotiation time and concessions, but receive in exchange decisions that are far less likely to be challenged or defected from once made, because every capital co-authored the outcome.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, large_member_states, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, large_member_states, payer).

% The rotating presidency manages the iterative negotiation process — sequencing proposals, brokering compromise texts, running the informal consultations that let unanimity actually be reached. It has strong incentive to demonstrate a successful consensus outcome during its term and benefits reputationally from brokered unanimity.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, council_presidency_rotation_holders, agenda_setter,
    institutional, biographical, constrained, continental).

% Proposes policy and can advocate for outcomes but does not hold a unanimity vote itself. It observes the negotiation and sometimes redrafts proposals to find the unanimity-compatible version, but has no formal voice in the veto structure — it works within whatever the member states will unanimously accept.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, european_commission, observer,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, european_commission, excluded).

% Receive policy outcomes that their own government has explicitly endorsed rather than had imposed by a majority of other states. This is the legitimacy payoff of the reading: domestic audiences can be told their government secured a good deal rather than absorbed a defeat, which reduces domestic backlash and downstream noncompliance.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, domestic_publics_of_member_states, beneficiary,
    powerless, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unanimity forces every unanimity-gated proposal through iterative rounds of negotiation until a text exists that no single state finds intolerable enough to block — this produces genuinely durable buy-in rather than a majority-imposed settlement that a losing minority might quietly undermine or defect from later.
% TRANSFER_FUNCTION: The arrangement moves negotiating time and drafting effort from all parties into a shared final text; it does not primarily move money or resources between states but redistributes influence over final wording toward whichever state's assent is most costly to secure.
% ABSENT_VOICES: Sub-national regions and pan-European civil society groups without direct representation at the Council table have no formal voice in the unanimity negotiation, though they are affected by its outcomes; this reading treats their absence as a separate representation question, not evidence against the coordination account.
% DISAPPEARANCE_RATIONALE: If unanimity were replaced by simple majority voting on these matters overnight, negotiation rounds would shorten dramatically, outvoted states would face domestically unpopular impositions rather than co-authored outcomes, and the durability advantage this reading identifies — lower rates of post-decision defection and non-implementation — would need to be re-earned through some other legitimacy mechanism.
% FOUNDING_PROBLEM: Early European integration needed a mechanism that let sovereign states pool decision-making on sensitive matters without any state fearing it would be permanently outvoted by a shifting majority of others — unanimity was built to make joining collective decisions safe enough that reluctant states would participate at all.
% FOUNDING_PROBLEM_CORROBORATION: Independent political science research on EU decision durability (comparing implementation and defection rates on unanimity-gated versus QMV-gated measures) corroborates that unanimously-reached decisions show higher compliance; this evidence comes from academic analysts outside the Council and outside any single member state's foreign ministry, not solely from the states that benefit from the slower process.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__diplomatic_capital_reading, 0.18, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored low (0.18) because on this reading the primary cost — negotiation time and diplomatic effort — is a shared coordination cost with a legitimacy payoff, not a rent extracted by one party from another. Suppression is low (0.15): no state is coerced into assent; the mechanism's entire function is that assent must be given, not compelled. Theater ratio starts low and drifts mildly upward (0.12 to 0.20) reflecting a modest increase over time in performative aspects of Council negotiation (pre-negotiated 'unanimity theater' where outcomes are substantially settled in informal channels before formal votes), but this remains well below the threshold that would suggest metric substitution has displaced the real coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   No stakeholder is authored with a high-d target profile in this reading, consistent with the expected structural delta: there is no fixed beneficiary/victim asymmetry. Large member states carry a dual beneficiary/payer role because they bear more of the negotiation cost in absolute terms (more issues on which their preferences must be reconciled with more counterparties) while still receiving the durability payoff. Small member states are pure beneficiaries because the formal-parity effect dominates their structural position. The european_commission sits outside the beneficiary/victim frame entirely — it is a process participant without a vote, hence observer/excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored live, not dead: the underlying problem (making pooled decision-making safe for reluctant sovereign states) persists as EU membership has expanded and preference heterogeneity has grown, if anything strengthening rather than weakening the case for a consent-based mechanism. This blocks a mandatrophy read on this particular reading — the coordination function has not visibly atrophied into pure performance, distinguishing this reading sharply from a hypothetical piton reading where unanimity would be authored as vestigial ritual with no live founding problem behind it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_payoff_vs_extraction_cover,
    'Is the iterative negotiation process genuinely producing durable buy-in (the diplomatic_capital_reading''s claim), or is the ''consensus-building'' framing cover for the same voting rule''s exploitation by minoritarian blocking threats (the veto_trap_reading''s claim about the identical structural mechanism)?',
    'Comparative case analysis: do unanimity-gated decisions on this reading''s own account show markedly lower post-decision defection AND markedly lower incidence of the final text being visibly shaped by a single small state''s threatened veto disproportionate to its stake? If durability holds but concession-extraction by blocking states is also pervasive, the readings may both be partially correct about different subsets of unanimity-gated decisions.',
    'If empirical analysis shows the durability effect is small or absent while extraction-via-veto-threat is pervasive and case-identifiable, this reading''s low epsilon would be undermined and the veto_trap_reading''s account would better fit the observed voting record for a substantial share of cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_payoff_vs_extraction_cover, empirical, 'Whether the coordination/legitimacy account or the extraction/veto-threat account better fits the observed record of unanimity-gated Council decisions.').

omega_variable(
    which_reading_is_the_modal_case,
    'Even granting that both durable-buy-in cases and veto-extraction cases occur under the same formal rule, which is the modal (typical) case across the actual population of unanimity-gated Council decisions since founding?',
    'A systematic dataset of unanimity-gated decisions coded for (a) presence of a credible minority blocking threat and (b) subsequent implementation/defection rates would allow estimating the relative frequency of each reading''s characteristic pattern.',
    'If diplomatic-capital-reading cases (broad buy-in, low defection) are the clear modal pattern, this reading''s low epsilon claim is well-supported as the typical operation of the mechanism, with veto-trap cases as a documented but minority pathology. If veto-trap cases are equally or more frequent, the low-epsilon claim describes an idealized rather than typical case.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_modal_case, empirical, 'Relative frequency of the coordination-legitimacy pattern versus the veto-extraction pattern across the actual decision record.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(eu_c_tr_t8, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(eu_c_tr_t16, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(eu_c_tr_t24, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(eu_c_tr_t32, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 32, 0.19).
narrative_ontology:measurement(eu_c_tr_t40, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(eu_c_be_t8, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 8, 0.15).
narrative_ontology:measurement(eu_c_be_t16, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement(eu_c_be_t24, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(eu_c_be_t32, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 32, 0.18).
narrative_ontology:measurement(eu_c_be_t40, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 40, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(eu_council_unanimity__diplomatic_capital_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__diplomatic_capital_reading, 0.12).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__veto_trap_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the eu_council_unanimity kernel. The identical formal voting rule (Article-level unanimity requirement in specified Council decision domains) is read here as a coordination-cost/legitimacy-payoff mechanism with low epsilon (0.18) and no fixed beneficiary/victim asymmetry. The veto_trap_reading reads the same rule as enabling minoritarian extraction via credible blocking threats and should be authored with substantially higher epsilon and an explicit victim class (states whose preferences are overridden or whose policy is delayed/watered down by a blocking minority). The sovereignty_guarantor_reading reads the same rule as a deontological consent floor, largely orthogonal to epsilon-scoring on consequentialist grounds. All three share the same kernel_id and must be linked via affects_constraints in each file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
