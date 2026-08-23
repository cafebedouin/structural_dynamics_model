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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: eu_council_unanimity__diplomatic_capital_reading
 *   human_readable: EU Council Unanimity â Diplomatic Capital Reading
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This is the diplomatic_capital_reading of the eu_council_unanimity
 *   kernel. It treats Council unanimity not as a veto trap or sovereignty
 *   shield, but as a coordination investment: member states spend time and
 *   diplomatic capital up front to produce decisions that all parties own,
 *   yielding higher compliance and legitimacy than majority imposition. The
 *   kernel is contested â sibling readings frame the same procedure as
 *   sovereignty_guarantor (protecting small states against majoritarian
 *   coercion) or veto_trap (enabling minoritarian extraction). This story
 *   instantiates only the diplomatic-capital reading, with low Îµ reflecting
 *   friction costs rather than extraction.
 *
 * KEY AGENTS:
 *   - council_member_states (institutional/constrained): symmetric beneficiaries of legitimacy and durability, equally possessing veto power.
 *   - european_commission (institutional/analytical): observer evaluating transaction costs and integration effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.22).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.25).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity â Diplomatic Capital Reading").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "institutional_design/international_relations/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, 'f030244e-cd48-4f3a-8257-56ef09171e9c').
narrative_ontology:cs_kernel_codification('f030244e-cd48-4f3a-8257-56ef09171e9c', formalized).
narrative_ontology:cs_authority_grounding('f030244e-cd48-4f3a-8257-56ef09171e9c', lineage).
narrative_ontology:cs_interpretation_layer_present('f030244e-cd48-4f3a-8257-56ef09171e9c').
narrative_ontology:cs_reading_relation('f030244e-cd48-4f3a-8257-56ef09171e9c', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('f030244e-cd48-4f3a-8257-56ef09171e9c', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_axiom('f030244e-cd48-4f3a-8257-56ef09171e9c', foundational, iterative_negotiation_produces_legitimacy).
narrative_ontology:cs_axiom_status(iterative_negotiation_produces_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f030244e-cd48-4f3a-8257-56ef09171e9c', iterative_negotiation_produces_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('f030244e-cd48-4f3a-8257-56ef09171e9c', foundational, unanimity_preferable_to_qmv_for_core_policies).
narrative_ontology:cs_axiom_status(unanimity_preferable_to_qmv_for_core_policies, holdable).
narrative_ontology:cs_axiom_grounding('f030244e-cd48-4f3a-8257-56ef09171e9c', unanimity_preferable_to_qmv_for_core_policies, instrumental).
narrative_ontology:cs_reference_frame('f030244e-cd48-4f3a-8257-56ef09171e9c', consensual_intergovernmental_bargaining).
narrative_ontology:cs_drift_state('f030244e-cd48-4f3a-8257-56ef09171e9c', post_enlargement_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f030244e-cd48-4f3a-8257-56ef09171e9c', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, council_member_states).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, legitimacy_through_consent).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, unanimity_durability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collective of EU member states bound by the Council unanimity procedure. Each state possesses an equal veto and invests diplomatic time in iterative negotiation. In return they receive policy buy-in, sovereignty reassurance, and durable agreements with lower downstream defection risk. Exit from the procedure is effectively tied to EU membership, which is politically and economically costly.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, council_member_states, beneficiary,
    institutional, generational, constrained, continental).

% Proposes legislation and monitors implementation but does not vote in the Council. Observes the unanimity process from an institutional vantage point, assessing whether negotiated outcomes align with the integration acquis and evaluating the transaction costs of consensus relative to majority alternatives.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, european_commission, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__diplomatic_capital_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__diplomatic_capital_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Secures collective action among sovereign states with divergent interests by forcing iterative negotiation until all parties consent, thereby internalizing objections before adoption.
% TRANSFER_FUNCTION: Transfers diplomatic time and negotiation effort from member states into the bargaining process; transfers legitimacy and implementation durability to the resulting policy.
% ABSENT_VOICES: EU citizens, candidate countries, and affected third parties are not at the bargaining table; their interests may be diluted or traded away in closed-door consensus packages.
% DISAPPEARANCE_RATIONALE: If unanimity vanished overnight in sensitive domains, the Council would shift to QMV or similar majoritarian rules. Bargaining would speed up but lose the procedural guarantee that every state's core interests are accommodated; the resulting policies would face higher non-compliance and political contestation risks.
% FOUNDING_PROBLEM: How to secure collective action and compliance among sovereign states with highly divergent interests without triggering exit or systematic non-compliance.
% FOUNDING_PROBLEM_CORROBORATION: Academic scholars in international relations and EU studies attest that the problem of securing sovereign-state compliance remains live in taxation, foreign policy, and enlargement. Independent treaty histories and comparative federalism research corroborate the continuing relevance of the consensus problem outside the beneficiary states' own assertions.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__diplomatic_capital_reading, 0.22, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.22) because the 'cost' of unanimity is mutual negotiation friction, not a unilateral transfer. Suppression is low (0.25) because the rule does not silence dissent but rather requires its resolution; alternatives like QMV are procedurally unavailable for covered decisions, but this is a collectively adopted procedure, not an imposed coercion. Theater is low (0.15): the negotiation is substantively functional. Accessibility collapse (0.35) reflects that QMV alternatives are procedurally blocked in unanimity domains, yet remain visible and politically discussed. Resistance (0.30) captures intermittent complaint about decisional paralysis, counterbalanced by general acceptance of the legitimacy premium.
 *
 * PERSPECTIVAL GAP:
 *   The divergence between this reading and the veto_trap reading is kernel-level, not metric-level. Under diplomatic_capital, all member states are near-symmetric beneficiaries (d â 0.5) investing coordination cost for collective gain. Under veto_trap, specific states or coalitions become agenda_setters wielding credible blocking threats to extract side-payments, producing asymmetric extraction. The engine will compute different per-seat classifications for the same procedural rule depending on which reading's structural data (beneficiary/victim declarations) is loaded.
 *
 * DIRECTIONALITY LOGIC:
 *   Council member states are declared beneficiaries with constrained exit, yielding directionality near the symmetric center. No victim group is declared because this reading asserts no fixed extraction from any state to another; the veto is equally distributed. The European Commission is an analytical observer with no directional stake in the extraction formula.
 *
 * MANDATROPHY ANALYSIS:
 *   Unanimity was built to solve the compliance problem among sovereigns. In this reading it remains live for sensitive domains (tax, foreign policy, enlargement) because the underlying coordination problem â securing willing implementation from states with divergent interests â has not disappeared. Mandatrophy is therefore not declared resolved. The classification as rope rather than piton is supported by the low theater ratio and the absence of performative maintenance: states genuinely bargain, package deals, and adjust texts rather than theatrically re-enacting a solved consensus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_delta,
    'How would the classification of EU Council unanimity change if the veto_trap reading or sovereignty_guarantor reading were adopted as the operative framing?',
    'Comparative domain analysis across policy areas (taxation, foreign policy, enlargement) to test whether unanimity operates as coordination, sovereignty protection, or extraction in each.',
    'Adopting the veto_trap reading would raise Îµ and shift classification toward tangled_rope or snare by introducing asymmetric victimhood; adopting the sovereignty_guarantor reading would keep Îµ low but make the beneficiary structure asymmetric (small states as primary beneficiaries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Structural ambiguity between coordination, sovereignty protection, and extraction readings of the same kernel.').

omega_variable(
    unanimity_durability_empirical,
    'Do decisions taken by unanimity in the EU Council exhibit measurably higher compliance and lower defection rates than those taken by QMV?',
    'Quantitative compliance studies comparing transposition timelines, infringement proceedings, and national court challenges for unanimity-adopted versus QMV-adopted directives.',
    'If durability is not empirically supported, the legitimacy payoff central to this reading is undermined and Îµ should rise to reflect pure friction without compensating benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimity_durability_empirical, empirical, 'Empirical test of the legitimacy-durability claim central to this reading.').

omega_variable(
    negotiation_cost_legitimacy_tradeoff,
    'At what point do the transaction costs of unanimity negotiation exceed the legitimacy benefits, and does the post-enlargement EU currently sit above or below that threshold?',
    'Economic analysis of legislative delay costs combined with elite and public legitimacy surveys across member states.',
    'If costs exceed benefits, the constraint becomes a net extraction mechanism even under this reading, pushing classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(negotiation_cost_legitimacy_tradeoff, conceptual, 'Cost-benefit threshold ambiguity for unanimity as coordination investment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_unanimity_dipcap_tr_t0, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(eu_unanimity_dipcap_tr_t10, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(eu_unanimity_dipcap_tr_t20, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(eu_unanimity_dipcap_tr_t30, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(eu_unanimity_dipcap_tr_t40, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(eu_unanimity_dipcap_tr_t50, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(eu_unanimity_dipcap_tr_t60, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 60, 0.15).

% Extraction over time
narrative_ontology:measurement(eu_unanimity_dipcap_be_t0, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(eu_unanimity_dipcap_be_t10, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 10, 0.21).
narrative_ontology:measurement(eu_unanimity_dipcap_be_t20, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(eu_unanimity_dipcap_be_t30, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 30, 0.23).
narrative_ontology:measurement(eu_unanimity_dipcap_be_t40, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 40, 0.24).
narrative_ontology:measurement(eu_unanimity_dipcap_be_t50, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 50, 0.23).
narrative_ontology:measurement(eu_unanimity_dipcap_be_t60, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 60, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(eu_council_unanimity__diplomatic_capital_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__diplomatic_capital_reading, 0.1).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__veto_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the eu_council_unanimity kernel. The diplomatic_capital_reading interprets the same procedural rule as coordination investment, while sibling readings interpret it as sovereignty protection or extraction trap. They are linked as a constraint family because they share the same natural-language label but instantiate structurally distinct claims with different Îµ values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
