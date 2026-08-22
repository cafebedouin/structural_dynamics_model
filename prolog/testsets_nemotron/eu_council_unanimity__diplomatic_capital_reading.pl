% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__diplomatic_capital_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: EU Council Unanimity Requirement (Diplomatic Capital Reading)
 *   domain: political/institutional
 *
 * SUMMARY:
 *   The EU Council's unanimity requirement is read here as a
 *   consensus-building mechanism: by requiring all member states to consent,
 *   it forces iterative negotiation that produces buy-in and reduces
 *   downstream defection. The coordination cost (prolonged negotiation,
 *   side-payments, issue linkage) is the price of legitimacy; the payoff is
 *   decisions that hold without enforcement because all parties invested in
 *   them. This reading claims low ε (0.18) because the constraint's operation
 *   generates net coordination value — unanimous decisions are empirically
 *   more durable than QMV impositions. No fixed beneficiary/victim structure:
 *   all member states bear negotiation costs and all receive the legitimacy
 *   dividend. The beneficiaries declared (member_states_collective,
 *   eu_institutions) reflect the coordination function, not extraction
 *   targets. This is one of three readings of the contested
 *   eu_council_unanimity kernel; the other readings
 *   (sovereignty_guarantor_reading, veto_trap_reading) instantiate
 *   structurally distinct constraints with different ε, beneficiary
 *   structures, and classifications.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.18).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.15).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity Requirement (Diplomatic Capital Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "political/institutional").

domain_priors:requires_active_enforcement(eu_council_unanimity__diplomatic_capital_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, '462d78e8-5231-4030-8ba5-91df5dd915ad').
narrative_ontology:cs_kernel_codification('462d78e8-5231-4030-8ba5-91df5dd915ad', formalized).
narrative_ontology:cs_authority_grounding('462d78e8-5231-4030-8ba5-91df5dd915ad', lineage).
narrative_ontology:cs_interpretation_layer_present('462d78e8-5231-4030-8ba5-91df5dd915ad').
narrative_ontology:cs_reading_relation('462d78e8-5231-4030-8ba5-91df5dd915ad', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('462d78e8-5231-4030-8ba5-91df5dd915ad', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_axiom('462d78e8-5231-4030-8ba5-91df5dd915ad', foundational, consensus_generates_legitimacy).
narrative_ontology:cs_axiom_status(consensus_generates_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('462d78e8-5231-4030-8ba5-91df5dd915ad', consensus_generates_legitimacy, conventional).
narrative_ontology:cs_axiom('462d78e8-5231-4030-8ba5-91df5dd915ad', foundational, iterative_negotiation_reduces_defection).
narrative_ontology:cs_axiom_status(iterative_negotiation_reduces_defection, holdable).
narrative_ontology:cs_axiom_grounding('462d78e8-5231-4030-8ba5-91df5dd915ad', iterative_negotiation_reduces_defection, empirically_contingent).
narrative_ontology:cs_reference_frame('462d78e8-5231-4030-8ba5-91df5dd915ad', founding_treaty_consensus_order).
narrative_ontology:cs_drift_state('462d78e8-5231-4030-8ba5-91df5dd915ad', post_enlargement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('462d78e8-5231-4030-8ba5-91df5dd915ad', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, member_states_collective).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, small_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, large_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, member_states_collective).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, large_member_states).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, consensus_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, iterative_negotiation_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All 27 member states participate in unanimous decision-making. Each bears the negotiation cost (diplomatic time, side-payments, issue linkage) and each receives the legitimacy dividend (decisions that hold without enforcement because all consented). Exit is constrained: leaving the unanimity rule requires treaty change (which itself requires unanimity), but enhanced cooperation and opt-outs provide partial exits for coalitions of the willing.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, member_states_collective, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, member_states_collective, payer).

% The Commission and Council presidency gain legitimate, durable decisions without bearing the full bilateral negotiation costs that member states carry. They administer the process and benefit from the downstream compliance that unanimity-produced decisions enjoy. Their exit is analytical — they observe and administer the constraint but are not subject to it in the same way.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_institutions, beneficiary,
    institutional, generational, analytical, continental).

% Smaller states disproportionately benefit from the unanimity rule's equalizing effect: their consent carries the same weight as large states, giving them leverage in negotiations that QMV would diminish. They bear negotiation costs but gain protection against majoritarian dominance. This seat appears in the veto_trap_reading as a potential extractor; here it is a coordination beneficiary.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, small_member_states, beneficiary,
    moderate, biographical, constrained, continental).

% Larger states bear higher absolute negotiation costs (more complex domestic coordination, greater side-payment capacity expected) but also gain the legitimacy dividend for decisions affecting their core interests. They have more credible exit threats (enhanced cooperation, bilateral deals outside EU framework) which gives them mobile exit options. In the veto_trap_reading, this seat is the primary target of extraction.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, large_member_states, payer,
    powerful, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, large_member_states, beneficiary).

% Groups of member states that want to move faster than unanimity allows. They are structurally excluded from the unanimity constraint's direct operation (they use the Treaty's enhanced cooperation clause instead) but their existence is shaped by the constraint's friction. They would object to unanimity's scope if present in the Council room.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, enhanced_cooperation_coalitions, excluded,
    organized, biographical, mobile, continental).

% European Parliament, civil society, and citizens experience unanimity indirectly: as democratic deficit (decisions made behind closed doors), as policy delay, and as legitimacy of outcomes. They are not formal parties to the Council's unanimity rule but their legitimacy judgments affect the constraint's long-term sustainability.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, ep_citizens_civil_society, observer,
    organized, biographical, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produces legitimate, durable collective decisions among sovereign equals by requiring iterative negotiation until all consent — the negotiation process itself generates the buy-in that makes decisions self-enforcing.
% TRANSFER_FUNCTION: Moves diplomatic capital, time, and side-payments among member states during negotiation; the net transfer is near-zero across the collective (each state pays and receives in different negotiations), but the coordination cost is real and the legitimacy dividend is collective.
% ABSENT_VOICES: Citizens and subnational regions who experience policy outcomes but have no seat at the Council table; future generations bound by unanimous treaty commitments; third states affected by EU external action decided by unanimity. Enhanced cooperation coalitions are structurally excluded from the unanimity process by design.
% DISAPPEARANCE_RATIONALE: If unanimity vanished overnight, the Council would default to QMV for all policy areas. Decisions would be faster but less legitimate — implementation fidelity would drop in sensitive areas (tax, foreign policy, social policy), defection and non-compliance would rise, and the EU's claim to act as a union of sovereign equals would be structurally damaged. The institutional architecture would reorganize around coalition politics rather than consensus.
% FOUNDING_PROBLEM: The founding problem was designing a decision rule for a union of sovereign states that prevents majoritarian coercion while enabling collective action — each state must be able to protect its vital interests, but the union must be able to decide.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Treaty texts (Rome, Maastricht, Lisbon) and the drafting history recorded in the CVCE archives. Member states collectively corroborate the problem is live (they defend unanimity in sensitive domains). The European Parliament and federalist civil society organizations attest the problem is substantially solved by the EU's evolved legal order and that unanimity now functions as veto_trap. Independent political science literature (e.g., König & Bräuninger, Tsebelis) corroborates that the coordination cost has risen with enlargement while the protective rationale remains contested.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__diplomatic_capital_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction is low (0.18) because the primary operation is coordination: the cost of negotiation is the mechanism producing the benefit of legitimacy, not a transfer to a distinct beneficiary. Suppression is low (0.15) because alternatives (QMV, enhanced cooperation) exist and are used; unanimity applies only to treaty-defined sensitive areas. Theater ratio is moderate (0.22) because some negotiation is performative (signaling resolve, domestic audience management) but the core iterative process is functional. Accessibility collapse is moderate (0.35) because enhanced cooperation and differentiated integration provide exits for coalitions of the willing. Resistance is low (0.25) because member states generally defend unanimity in their domains of sensitivity. The claimed type is rope — genuine coordination with net beneficiary structure.
 *
 * PERSPECTIVAL GAP:
 *   From the member state seat, unanimity is a right and a cost — the right to block unwanted coercion, the cost of negotiating everything. From the EU institutions seat, it is a constraint on agenda-setting speed but a source of decision legitimacy. From the veto_trap_reading seat (which sees the same rule), the structure is extractive: small states or single-issue blockers capture side-payments. The engine will compute different per-seat types from the different structural declarations across the three readings. This reading's symmetric d-values produce rope classifications; the other readings' asymmetric structures will not.
 *
 * DIRECTIONALITY LOGIC:
 *   All member states are structurally symmetric under this reading: each bears negotiation costs (time, diplomatic capital, side-payments) and each receives the legitimacy dividend (durable decisions, reduced defection). The directionality derivation from beneficiary/victim declarations yields d ≈ 0.5 for all member state seats — symmetric cost/benefit. The eu_institutions seat (Commission, Council presidency) is a slight net beneficiary (d ≈ 0.3) because it gains legitimate decisions without bearing the full negotiation cost. No victim seats declared because this reading posits no asymmetric extraction. The sovereignty_guarantor_reading and veto_trap_reading declare different beneficiary/victim structures and will compute different directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing majoritarian coercion among sovereign equals) is live but contested. The unanimity rule was designed when the EU was smaller and policy scope narrower; as membership expanded and competences deepened, the coordination cost has risen. This reading acknowledges the rising cost (reflected in the upward extractiveness trajectory) but argues the legitimacy payoff scales with scope — decisions affecting more states need more buy-in. The mandatrophy risk is that the coordination function atrophies into veto_trap dynamics in high-stakes domains. The classification as rope (not tangled_rope or snare) reflects the judgment that the coordination function remains genuine and symmetric across most policy areas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_diplomatic_capital,
    'Is this constraint a distinct reading of the contested eu_council_unanimity kernel, and how does its ε and beneficiary structure differ from sibling readings?',
    'Compare the ε, beneficiary/victim declarations, and structural dynamics across all three declared readings of the eu_council_unanimity kernel (diplomatic_capital_reading, sovereignty_guarantor_reading, veto_trap_reading). The kernel contest is resolved by demonstrating that each reading instantiates a different constraint with its own stable ε and classification.',
    'If the diplomatic capital reading''s low ε and coordination function are structurally distinct from the sovereignty_guarantor_reading''s protective function and the veto_trap_reading''s extractive structure, then the kernel decomposes into three constraint stories linked by network.affects_constraints. If they collapse to the same ε and dynamics, they are one constraint and the kernel framing was mistaken.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_diplomatic_capital, conceptual, 'Kernel decomposition: this reading vs. sovereignty_guarantor_reading vs. veto_trap_reading').

omega_variable(
    legitimacy_payoff_measurability,
    'Can the downstream defection-reduction payoff from unanimous consensus be empirically distinguished from the status quo bias that unanimity also entrenches?',
    'Longitudinal comparison of implementation fidelity and defection rates for unanimous vs. QMV decisions in the same policy domains, controlling for issue salience and distributional conflict.',
    'If the legitimacy payoff is real and measurable, the coordination function is genuine and ε remains low. If the payoff is indistinguishable from status quo entrenchment, the constraint may be a false summit (mountain claim masking extraction) or a piton (coordination function atrophied).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_payoff_measurability, empirical, 'Whether the claimed legitimacy payoff is empirically separable from status quo bias').

omega_variable(
    negotiation_cost_vs_extraction,
    'At what point do the transaction costs of iterative negotiation under unanimity become extractive overhead rather than coordination cost?',
    'Measure the ratio of negotiation time/resources to policy substance across issue areas; identify the threshold where marginal negotiation cost exceeds marginal legitimacy gain.',
    'If negotiation costs are systematically extractive in certain domains (e.g., enlargement, tax, foreign policy), the constraint may be a tangled_rope in those domains — coordination function present but asymmetric extraction layered on. This reading would then need domain-specific decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiation_cost_vs_extraction, empirical, 'Threshold where coordination cost becomes extractive overhead').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(eu_c_tr_t10, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(eu_c_tr_t30, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 30, 0.22).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(eu_c_be_t10, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(eu_c_be_t30, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 30, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(eu_c_su_t10, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(eu_c_su_t20, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement(eu_c_su_t30, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 30, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__diplomatic_capital_reading, 0.08).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__veto_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the eu_council_unanimity kernel. The diplomatic_capital_reading emphasizes coordination function and legitimacy payoff (rope, ε≈0.18). The sovereignty_guarantor_reading emphasizes protective function against majoritarianism (likely mountain or rope, different ε). The veto_trap_reading emphasizes extractive minoritarian blocking (likely snare/tangled_rope, higher ε). All three share the same formal rule but instantiate different constraints because their ε, beneficiary/victim structures, and operational dynamics differ structurally. This decomposition follows the ε-invariance principle: the label 'unanimity' conflates distinct claims; the framework models them as separate stories linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
