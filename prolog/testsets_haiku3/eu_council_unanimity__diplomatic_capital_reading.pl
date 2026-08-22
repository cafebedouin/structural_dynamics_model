% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__diplomatic_capital_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: EU Council Unanimity as Diplomatic Capital Coordination
 *   domain: institutional_design/political_economy
 *
 * SUMMARY:
 *   The EU Council's unanimity requirement is a rule that every member state
 *   must agree to a decision before it becomes binding. The
 *   diplomatic_capital_reading frames unanimity as a coordination mechanism
 *   that forces iterative negotiation and strengthens the legitimacy of
 *   outcomes. Under this reading, the constraint solves a genuine
 *   coordination problem (how to make binding collective decisions while
 *   preserving state consent) at the cost of slowed decision-making and
 *   concentrated negotiation burdens on larger states. The referent
 *   constraint is the standing unanimity rule itself, assessed by this
 *   reading's own lights: how well it achieves consensus-building and
 *   legitimacy at the cost of negotiation friction. This is one of three
 *   structurally distinct readings of the same kernel (the EU's commitment to
 *   collective decision-making with state consent); the other readings —
 *   sovereignty_guarantor_reading and veto_trap_reading — instantiate
 *   different constraints with different ε values and structural stories.
 *
 * KEY AGENTS:
 *   - Smaller member states: leverage from blocking power, gain participatory legitimacy and input into policy
 *   - Larger member states: pay negotiation costs and make substantive concessions to secure unanimity
 *   - European Commission: agenda-setter and consensus-engineer, mediates between state preferences
 *   - European Parliament: excluded from unanimity gate in most decisions, sidelined in intergovernmental domains
 *   - Civil society coalitions: observers, input through member-state preferences, not at the table
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.38).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.12).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity as Diplomatic Capital Coordination").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "institutional_design/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__diplomatic_capital_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, '265a9b2c-9add-4aec-8450-fd4277382899').
narrative_ontology:cs_kernel_codification('265a9b2c-9add-4aec-8450-fd4277382899', formalized).
narrative_ontology:cs_authority_grounding('265a9b2c-9add-4aec-8450-fd4277382899', lineage).
narrative_ontology:cs_interpretation_layer_present('265a9b2c-9add-4aec-8450-fd4277382899').
narrative_ontology:cs_reading_relation('265a9b2c-9add-4aec-8450-fd4277382899', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('265a9b2c-9add-4aec-8450-fd4277382899', eu_council_unanimity__veto_trap_reading, influences).
narrative_ontology:cs_axiom('265a9b2c-9add-4aec-8450-fd4277382899', foundational, consensus_legitimacy_principle).
narrative_ontology:cs_axiom_status(consensus_legitimacy_principle, holdable).
narrative_ontology:cs_axiom_grounding('265a9b2c-9add-4aec-8450-fd4277382899', consensus_legitimacy_principle, instrumental).
narrative_ontology:cs_axiom('265a9b2c-9add-4aec-8450-fd4277382899', foundational, iterative_negotiation_function).
narrative_ontology:cs_axiom_status(iterative_negotiation_function, holdable).
narrative_ontology:cs_axiom_grounding('265a9b2c-9add-4aec-8450-fd4277382899', iterative_negotiation_function, empirically_contingent).
narrative_ontology:cs_reference_frame('265a9b2c-9add-4aec-8450-fd4277382899', consensus_based_union).
narrative_ontology:cs_drift_state('265a9b2c-9add-4aec-8450-fd4277382899', enlarged_union_post_2004, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('265a9b2c-9add-4aec-8450-fd4277382899', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, smaller_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, coalition_builders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, larger_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under unanimity, a smaller state's veto power creates leverage for negotiation. A state representing 2% of EU population can block decisions, forcing larger states to address its concerns. This translates into disproportionate voice in agenda-setting and negotiation outcomes — not from superior power but from coordination rules that valorize consensus. The benefit is participatory legitimacy: the state's voice is heard, its consent sought, its buy-in secured.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, smaller_member_states, beneficiary,
    moderate, generational, constrained, global).

% Bear the cost of iterative negotiation and concession-making required to secure unanimity. France cannot unilaterally impose agricultural policy or tax harmonization; it must bargain with Luxembourg, Cyprus, and Malta. This consumes diplomatic capital, delays decisions, and often requires substantive compromise away from the large state's preferred position. The payoff (durable, legitimate policy) is real but intangible and deferred; the cost is immediate and concrete.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, larger_member_states, payer,
    powerful, generational, constrained, global).

% Proposes legislation and mediates negotiation. Under unanimity, the Commission's role as honest broker is strengthened — it must craft proposals that can pass the unanimity filter, which means incorporating diverse state preferences into the initial proposal. The Commission becomes a consensus-engineering body rather than merely an executor of a strong majority.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, european_commission, agenda_setter,
    institutional, generational, analytical, global).

% In areas where Council votes unanimously, the Parliament has been historically sidelined. Parliamentary supermajority is not the deciding gate; Council unanimity is. The Parliament's transnational constituency (representing individual citizens rather than states) is excluded from the unanimity logic. Its voice enters through pressure campaigns and co-decision procedures, but the unanimity gate privileges state coordination over transnational representation.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, european_parliament, excluded,
    organized, generational, constrained, global).

% See unanimity as opening or closing depending on alignment with member-state preferences. When a small state's veto blocks progress on a cause (e.g., climate, labor rights), unanimity is read as obstructive. When unanimity forces slower, more consultative policymaking that incorporates civil-society input into the negotiation, it is read as legitimacy-enhancing. The constraint's effect is mediated through member-state positioning.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, civil_society_coalitions, observer,
    organized, biographical, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__diplomatic_capital_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__diplomatic_capital_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unanimity forces member states to negotiate iteratively toward consensus, embedding state preferences into policy from the start and building buy-in before implementation. This produces decisions that are durable because they reflect input from all parties, reducing post-passage defection and non-compliance.
% TRANSFER_FUNCTION: Transfers diplomatic capital and negotiation costs to larger/more powerful states (who must compromise to secure unanimity) and transfers legitimacy/durability to the resulting policy (which carries the signature of all member states, not just a majority).
% ABSENT_VOICES: The European Parliament, representing transnational constituencies of individual citizens rather than state governments, is excluded from the unanimity gate in most Council decisions. Supranational civil society and advocacy networks lobby member states but do not sit at the unanimity table. Future member states and non-member-state stakeholders (third countries, non-EU citizens affected by EU policy) have no seat.
% DISAPPEARANCE_RATIONALE: If unanimity disappeared and the Council switched to qualified majority voting (QMV) on all decisions, decision-making would accelerate, smaller states would lose blocking leverage, and larger states would gain agenda-setting power. Policies would pass faster but with less buy-in from excluded states — compliance costs would likely rise (more non-implementation, more legal challenge), and the EU's legitimacy in smaller states would decline. The political equilibrium of the Union would reorganize.
% FOUNDING_PROBLEM: After the European Union's founding as a consensus-based club of sovereign states, the core problem unanimity addressed was: How can a supranational authority (the Council/Commission) make binding decisions while preserving each state's veto power over measures that implicate its sovereignty? Unanimity was the answer: every state must consent.
% FOUNDING_PROBLEM_CORROBORATION: Treaty signatories (member-state governments) historically attested that sovereignty protection was live and necessary. Contemporary scholars of European integration (Pollack, Majone, Scharpf — outside the beneficiary set) argue the founding problem is partly dead (supranational institutions have accumulated authority and legitimacy independent of unanimity) and partly persists in specific domains (taxation, foreign policy, asylum) where state consent remains politically essential. Smaller member states attest the problem remains live in practice.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__diplomatic_capital_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.38) because while negotiation is costly to larger states and slows decision-making, the payoff — durable, legitimate policy with state buy-in — is real and substantial. The constraint does not extract in the snare sense (no fixed victimhood, no alternative suppressed); it instead distributes the cost of legitimacy-building. Suppression is low (0.12) because the rule is transparent, openly debated, and no state is coerced into concealing its position — negotiation is visible and iterative. Theater is low-to-moderate (0.22) because while some performances of consensus occur (states declare support they do not fully hold to reach agreement), the bulk of the constraint's operation is functional: real negotiation, real concessions, real buy-in. The measurement trajectory is stable across 31 years, with extractiveness clustering around 0.37–0.39 and a slight rise in theater_ratio from 2000 to 2014 (as the Union expanded and negotiation became more stylized and staged), then moderate decline as the constraint's limitations became more openly contested. Theater peaked during the period of high enlargement costs (mid-2000s) when the Commission worked harder to construct artificial consensus narratives.
 *
 * PERSPECTIVAL GAP:
 *   The gap between larger and smaller state seats is substantial. France or Germany reads unanimity as a costly constraint on its agenda-setting capacity; Luxembourg or Malta reads it as a guarantee of voice and leverage. These are not disagreements about what the rule does but different structural positions within it. The Commission reads it as an opportunity to engineer consensus; member states read it as a constraint on their autonomy. The engine will compute different effective extractiveness for each seat from the same structural data — larger states experience higher χ (the negotiation cost is borne by them) while smaller states experience lower or inverted χ (the veto leverage benefits them). This divergence is not a defect in the story; it is exactly what the classification system measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is NOT simple beneficiary/victim because the constraint has a coordination function that produces real payoffs distributed across multiple seats. Smaller member states sit near the beneficiary end (d ≈ 0.25–0.35) because unanimity gives them leverage they would not have under QMV; their veto power is a structural resource. Larger member states sit near the middle-to-upper end (d ≈ 0.55–0.65) because they bear real negotiation costs and make substantive concessions, but they also benefit from the durability and legitimacy of consensus outcomes. Neither is fully a target or fully a beneficiary — the constraint's operation moves both closer to d = 0.5 than to the poles. The Commission sits near the beneficiary end (d ≈ 0.15–0.25) because its role as honest broker and consensus-engineer is strengthened; it does not bear the state-level costs. The Parliament is not seated in the unanimity gate at all — it is excluded, not beneficiary or victim of this specific constraint (though it bears the downstream effect: reduced co-decision power in intergovernmental domains).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to preserve state sovereignty while enabling binding collective decisions) was live and pressing at the EU's founding. By 2024 it is contested: the problem is partly dead (supranational institutions have accumulated legitimacy and authority; most decisions do not implicate core sovereignty) and partly persists (in taxation, foreign policy, asylum, core EU powers — states still insist on veto rights). The constraint persists partly because the founding problem persists and partly because the constraint has become institutionalized as the normal way the Union operates. If the founding problem is dead but the constraint persists, that signals mandatrophy — the rule outliving its rationale. The measurement data does not show this; extractiveness is stable (no accumulation), and theater is moderate (no rise into pure performance). This suggests the constraint is not yet a piton: it still solves a live (if contested) problem and is actively maintained for reasons beyond inertia. The reading's own framework preserves the mandate — consensus-building remains valuable even if sovereignty protection is partly obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_durability_empirical_test,
    'Do policies passed under unanimity actually have lower defection and non-compliance rates than policies passed under QMV, holding policy domain constant?',
    'Comparative analysis of implementation rates and legal challenges to unanimously-passed directives vs. QMV-passed directives in comparable domains; interview data from member states about perceived legitimacy and compliance motivation.',
    'If unanimity produces measurably higher compliance and lower defection, the legitimacy payoff is empirically real and the rope classification holds. If compliance rates do not differ, the constraint extracts negotiation costs without the offsetting legitimacy benefit — reclassifying as tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_durability_empirical_test, empirical, 'Whether consensus-building actually produces durable, lower-defection policy outcomes.').

omega_variable(
    reading_foreclosure_test,
    'Is the diplomatic_capital_reading logically compatible with the sovereignty_guarantor_reading in a single framework, or do they foreclose each other?',
    'Examine whether a single member state can coherently hold both readings: (1) unanimity exists to build consensus and strengthen legitimacy; (2) unanimity exists to preserve each state''s veto right as a sovereignty protection. Can both be true of the same rule in the same framework?',
    'If the readings foreclose each other, the relation is forecloses (one reading''s core premise contradicts the other''s). If a state can hold both (legitimacy AND sovereignty preservation as joint rationales), the relation is coexists_with (different parties emphasize different functions). This determines the cs_structure.reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether the diplomatic and sovereignty readings are logically compatible or mutually exclusive.').

omega_variable(
    veto_weaponization_ambiguity,
    'At what point does legitimate veto-exercise (blocking a measure to force negotiation) become minority extraction (blocking to extort concessions unrelated to the measure)?',
    'Case study analysis of specific Council vetoes: did the blocking state use the veto to improve its position on the substance of the disputed measure, or to extract gains on unrelated issues? What evidence distinguishes negotiation leverage from minority predation?',
    'If vetoes are systematically used for substantive negotiation (forcing reconsideration of the blocked measure itself), the diplomatic_capital reading holds. If vetoes are systematically used for side-deals and unrelated concessions, the veto_trap reading (the sibling constraint) becomes the better classification. This omega names where the readings diverge operationally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_weaponization_ambiguity, empirical, 'Whether veto-exercise is negotiation leverage within the coordination function or minority extraction outside it.').

omega_variable(
    reading_vs_kernel_disambiguation,
    'Is this constraint best understood as one reading of a contested kernel (the EU''s commitment to state consent in binding decisions), or as a distinct constraint independent of reading choice?',
    'If the rule is the same rule regardless of reading (unanimity in the Treaties, regardless of interpretation), then it is one kernel with multiple readings. If the rule itself changes when the reading changes (e.g., unanimity-as-consensus vs. unanimity-as-sovereignty are operationalized differently), then the readings instantiate different constraints.',
    'If readings do not change the rule''s operation, this story remains one reading of a kernel; if they do, the three readings should be separate non-related stories. The kernel frame assumes the rule persists and readings interpret it; the constraint frame assumes different interpretations produce different constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_kernel_disambiguation, conceptual, 'Whether the kernel reading frame (one rule, multiple interpretations) is appropriate for EU unanimity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t1993, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 1993, 0.15).
narrative_ontology:measurement_basis(eu_c_tr_t1993, observed).
narrative_ontology:measurement(eu_c_tr_t2000, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement_basis(eu_c_tr_t2000, observed).
narrative_ontology:measurement(eu_c_tr_t2007, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 2007, 0.2).
narrative_ontology:measurement_basis(eu_c_tr_t2007, observed).
narrative_ontology:measurement(eu_c_tr_t2014, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 2014, 0.24).
narrative_ontology:measurement_basis(eu_c_tr_t2014, observed).
narrative_ontology:measurement(eu_c_tr_t2020, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 2020, 0.23).
narrative_ontology:measurement_basis(eu_c_tr_t2020, observed).
narrative_ontology:measurement(eu_c_tr_t2024, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(eu_c_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t1993, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 1993, 0.32).
narrative_ontology:measurement_basis(eu_c_be_t1993, observed).
narrative_ontology:measurement(eu_c_be_t2000, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 2000, 0.36).
narrative_ontology:measurement_basis(eu_c_be_t2000, observed).
narrative_ontology:measurement(eu_c_be_t2007, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 2007, 0.39).
narrative_ontology:measurement_basis(eu_c_be_t2007, observed).
narrative_ontology:measurement(eu_c_be_t2014, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 2014, 0.38).
narrative_ontology:measurement_basis(eu_c_be_t2014, observed).
narrative_ontology:measurement(eu_c_be_t2020, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 2020, 0.39).
narrative_ontology:measurement_basis(eu_c_be_t2020, observed).
narrative_ontology:measurement(eu_c_be_t2024, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement_basis(eu_c_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t1993, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 1993, 0.08).
narrative_ontology:measurement_basis(eu_c_su_t1993, observed).
narrative_ontology:measurement(eu_c_su_t2000, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 2000, 0.09).
narrative_ontology:measurement_basis(eu_c_su_t2000, observed).
narrative_ontology:measurement(eu_c_su_t2007, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 2007, 0.11).
narrative_ontology:measurement_basis(eu_c_su_t2007, observed).
narrative_ontology:measurement(eu_c_su_t2014, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 2014, 0.12).
narrative_ontology:measurement_basis(eu_c_su_t2014, observed).
narrative_ontology:measurement(eu_c_su_t2020, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 2020, 0.13).
narrative_ontology:measurement_basis(eu_c_su_t2020, observed).
narrative_ontology:measurement(eu_c_su_t2024, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 2024, 0.12).
narrative_ontology:measurement_basis(eu_c_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__diplomatic_capital_reading, 0.18).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__veto_trap_reading).

% DUAL FORMULATION NOTE:
% The EU Council unanimity rule is a single kernel (the EU's commitment to collective decision-making with state consent, codified in the treaties) read through three structurally distinct framings: diplomatic_capital_reading (this story, ε=0.38, rope, coordination with legitimacy payoff), sovereignty_guarantor_reading (ε near zero, mountain, foundational protection), veto_trap_reading (ε=0.68, snare, minority extraction vulnerability). Each reading instantiates a different constraint with different beneficiary/victim structures and classifications. The network links them as a constraint family; the three stories are siblings related through the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
