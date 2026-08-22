% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: EU Council Unanimity Requirement as Sovereignty Guarantor
 *   domain: institutional_design/international_relations
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_guarantor_reading of the
 *   eu_council_unanimity kernel: the requirement that certain EU Council
 *   decisions (treaty amendment, taxation, common foreign and security
 *   policy, enlargement) require unanimous consent of all member states, read
 *   as a legitimate structural protection against majoritarian coercion of
 *   sovereign states. Under this reading, no state extracts from another
 *   through the veto — each state's blocking capacity is a rights-exercise
 *   defending its own sovereign prerogative, and the beneficiary set is
 *   universal across all member states regardless of size. Coordination costs
 *   are real (unanimity slows decisions, requires extended negotiation) but
 *   these costs are the price of genuine consent-based governance, not
 *   evidence of extraction. This is one of three readings of the same kernel;
 *   the veto_trap_reading treats the identical rule as enabling minoritarian
 *   extraction via credible blocking threats, and the
 *   diplomatic_capital_reading treats it as a consensus-forcing mechanism
 *   that builds policy legitimacy through iteration. Each reading is a
 *   separate constraint story with its own ε and stakeholder structure,
 *   linked via network.affects_constraints; this file does not adjudicate
 *   between them.
 *
 * KEY AGENTS:
 *   - small_member_states: primary beneficiary (moderate/constrained) — relies on veto parity
 *   - medium_member_states: beneficiary (moderate/constrained) — uses veto as background insurance
 *   - large_member_states: beneficiary and cost-bearer (powerful/constrained) — absorbs more absolute coordination cost but holds same right
 *   - commission_and_qualified_majority_advocates: excluded (institutional/constrained) — favors narrowing the rule's scope
 *   - eu_citizens_of_all_member_states: indirect beneficiary (powerless/constrained) — protected via national government's veto channel
 *   - constitutional_law_scholars: analytical observer — compares to federal minority-protection structures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.28).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.22).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity Requirement as Sovereignty Guarantor").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "institutional_design/international_relations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, '81498b03-ad31-4004-b046-58da9243070c').
narrative_ontology:cs_kernel_codification('81498b03-ad31-4004-b046-58da9243070c', formalized).
narrative_ontology:cs_authority_grounding('81498b03-ad31-4004-b046-58da9243070c', lineage).
narrative_ontology:cs_interpretation_layer_present('81498b03-ad31-4004-b046-58da9243070c').
narrative_ontology:cs_reading_relation('81498b03-ad31-4004-b046-58da9243070c', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('81498b03-ad31-4004-b046-58da9243070c', eu_council_unanimity__diplomatic_capital_reading, influences).
narrative_ontology:cs_axiom('81498b03-ad31-4004-b046-58da9243070c', foundational, sovereignty_consent_is_inviolable_absent_unanimity).
narrative_ontology:cs_axiom_status(sovereignty_consent_is_inviolable_absent_unanimity, holdable).
narrative_ontology:cs_axiom_grounding('81498b03-ad31-4004-b046-58da9243070c', sovereignty_consent_is_inviolable_absent_unanimity, deontological).
narrative_ontology:cs_axiom('81498b03-ad31-4004-b046-58da9243070c', foundational, veto_exercise_is_rights_defense_not_extraction).
narrative_ontology:cs_axiom_status(veto_exercise_is_rights_defense_not_extraction, holdable).
narrative_ontology:cs_axiom_grounding('81498b03-ad31-4004-b046-58da9243070c', veto_exercise_is_rights_defense_not_extraction, conventional).
narrative_ontology:cs_reference_frame('81498b03-ad31-4004-b046-58da9243070c', treaty_based_sovereign_consent_framework).
narrative_ontology:cs_drift_state('81498b03-ad31-4004-b046-58da9243070c', post_lisbon_qmv_expansion_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('81498b03-ad31-4004-b046-58da9243070c', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, medium_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens_of_all_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a veto on unanimity-required matters (treaty change, taxation, foreign policy, enlargement) equal in formal weight to the largest members. Rely on this parity to prevent measures that would bind them to fiscal, security, or legal obligations they did not agree to. Exit from the Union itself remains available (Article 50) but exit from any single vote is not a live option — voice, not exit, is the mechanism this reading protects.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states, beneficiary,
    moderate, generational, constrained, continental).

% Use the veto less frequently than small states but retain it as a background guarantee when core sovereignty interests (tax harmonization, defense integration) are at stake. Their bargaining weight in qualified-majority contexts is higher than small states', so unanimity mainly matters to them as insurance against being outvoted on existential issues.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, medium_member_states, beneficiary,
    moderate, generational, constrained, continental).

% Absorb the coordination cost of unanimity — proposals they favor can be slowed or reshaped by any single holdout — but also hold the same veto right and use it themselves on matters touching their own sovereignty (tax policy, treaty change). They bear more of the aggregate cost of consensus-seeking in absolute terms because they propose more initiatives, but the guarantee runs symmetrically to them as well.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, payer).

% Would prefer more decisions move to qualified-majority voting to accelerate integration; they are not party to the unanimity rule's design and cannot unilaterally change it since treaty revision itself typically requires unanimity. Their objection is to the mechanism's scope, not this reading's account of what unanimity is for.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, commission_and_qualified_majority_advocates, excluded,
    institutional, generational, constrained, continental).

% Benefit indirectly: the unanimity requirement means their national government cannot be bound on core sovereignty matters (taxation, treaty change, foreign policy commitments with military implications) without that government's explicit consent, preserving a channel of democratic accountability through national elections and national government positions at Council.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens_of_all_member_states, beneficiary,
    powerless, generational, constrained, continental).

% Study the unanimity requirement as an instance of consent-based supranational governance design, comparing it to federal structures with entrenched minority protections (US Senate filibuster history, federal constitutional amendment thresholds).
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unanimity solves the problem of binding sovereign states to collective decisions that touch the core of what sovereignty means (taxation, treaty change, defense, enlargement) without any state being coerced by a majority of others into obligations it never agreed to bear.
% TRANSFER_FUNCTION: No systematic transfer occurs under this reading: the rule does not move resources or power from one party to another. It preserves each state's pre-existing sovereign authority against being overridden, rather than redistributing anything between states.
% ABSENT_VOICES: Sub-national regions and EU citizens acting outside national government channels have no direct voice in the unanimity mechanism itself — their sovereignty-protection interest is represented only derivatively through their national government's veto, which this reading treats as adequate representation rather than exclusion.
% DISAPPEARANCE_RATIONALE: If unanimity disappeared on sovereignty-implicating matters, smaller and medium states would lose their principal formal guarantee against being outvoted on taxation, treaty change, defense integration, or enlargement by a coalition of larger states; national parliaments' ratification leverage over EU treaty change would be structurally weakened, and national governments would face pressure to accept obligations negotiated without their consent.
% FOUNDING_PROBLEM: The founding member states and successive accession states needed a mechanism ensuring that joining or deepening the Union would never expose them to having core sovereign prerogatives — taxation, foreign policy, treaty content itself — altered against their will by a majority coalition of other members.
% FOUNDING_PROBLEM_CORROBORATION: National constitutional courts (notably Germany's Bundesverfassungsgericht in its EU-integration jurisprudence) independently corroborate that unanimity on treaty-level and sovereignty-implicating matters is treated as a live constitutional safeguard, not a vestigial formality — this attestation comes from judicial bodies outside the Council and outside any state's executive, i.e. outside the set of parties who benefit from the veto's continued existence.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).
:- end_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.28) and rises only marginally over three decades: this reading holds that coordination costs (delayed decisions, negotiation overhead) are real but do not constitute extraction because no party's position is used to extract concessions beyond defending its own sovereignty interest. Suppression is low (0.22) because no state is coerced into voting a particular way — the rule's entire function is to prevent coercion. Theater ratio is very low and flat (0.08-0.12) because the veto is genuinely exercised, not merely performed; accession states have repeatedly invoked or credibly threatened it on live matters (taxation harmonization proposals, CFSP positions, enlargement terms), evidencing real function rather than ritual. Accessibility collapse is moderate (0.35) — states retain the alternative of not joining measures requiring unanimity, or of exiting the Union entirely, so alternatives are not fully foreclosed the way a mountain's would be. Resistance is moderate (0.30): active resistance to the unanimity rule itself comes mainly from supranational integration advocates who want it narrowed, not from states party to the guarantee.
 *
 * PERSPECTIVAL GAP:
 *   The sovereignty_guarantor_reading predicts modest seat divergence: large states may experience more FRICTION (their initiatives are slowed more often, in raw count) but should not compute as targets of extraction, since the friction is the symmetric cost of a right they also hold. Any seat divergence toward a target-type reading for a large state would be a signal that the veto_trap_reading, not this one, is structurally operative for that specific episode — a question this file does not resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, every member state — regardless of power atom — sits closer to the beneficiary end of directionality, because the veto right runs symmetrically: large states benefit exactly as small states do when their own sovereignty interest is at stake, even though large states bear more absolute coordination cost as frequent initiators. There are no victims in this reading; the empty victims array reflects the reading's own account, not an omission. Citizens are beneficiaries at one remove, protected through their national government's veto rather than directly.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the founding problem (protecting sovereign consent from majoritarian override) as still live, not merely as an inherited justification for what has become extraction — the corroboration from national constitutional courts outside the benefiting executive branches supports treating the mandate as unresolved-but-real rather than a zombie mandate propped up by inertia. Classifying this reading as rope (rather than tangled_rope or snare) prevents mislabeling a genuine, still-functioning consent mechanism as pure extraction merely because it is slow and occasionally frustrates integration advocates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_use_pattern_ambiguity,
    'Is the empirical pattern of Council veto invocation over the study period consistent with rights-defense (blocking measures that genuinely implicate the vetoing state''s sovereignty) or with strategic extraction (blocking unrelated measures to extract side-payments)?',
    'Case-by-case analysis of documented veto episodes (e.g., taxation directives, enlargement negotiations, CFSP sanctions votes) coding whether the blocked measure had a direct sovereignty nexus to the vetoing state''s stated objection, versus episodes where the linkage was pretextual and a side-payment or unrelated concession resolved the block.',
    'If a substantial share of veto episodes show pretextual sovereignty framing masking extraction demands, this reading''s account of the beneficiary set as universal and victim set as empty would not hold for those episodes, and the veto_trap_reading would be the operative structural account for that subset of cases rather than this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_use_pattern_ambiguity, empirical, 'Whether documented veto invocations match rights-defense or extraction patterns.').

omega_variable(
    sovereignty_scope_ambiguity,
    'Is the set of matters currently requiring unanimity (taxation, CFSP, treaty change, enlargement, some social policy) a principled, stable definition of ''sovereignty-implicating,'' or a contested and shifting boundary that itself reflects power dynamics among member states?',
    'Track proposals to move specific policy areas from unanimity to qualified-majority voting (e.g., repeated Commission proposals to move tax and CFSP matters to QMV) and analyze which states resist scope-narrowing and why.',
    'If the scope of ''sovereignty-implicating'' matters has been strategically expanded or defended by particular states to protect specific interests rather than a stable principled category, this reading''s naturalized account of what unanimity protects would need revision — some scope decisions might themselves be better read through the veto_trap or diplomatic_capital lens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_scope_ambiguity, conceptual, 'Whether the unanimity-requiring policy scope is a principled or contested/power-reflecting boundary.').

omega_variable(
    kernel_reading_selection,
    'Given that the same unanimity rule and the same historical episodes can be read as sovereignty_guarantor, veto_trap, or diplomatic_capital, what determines which reading a given observer or institution adopts, and is that selection itself a strategic act?',
    'Survey which institutional actors (Commission, Parliament, small vs. large member states, accession candidates) publicly favor which reading, and whether reading choice correlates with structural position (beneficiaries of the status quo favor sovereignty_guarantor; integration advocates favor veto_trap).',
    'If reading selection strongly correlates with structural position rather than independent analysis of veto-use patterns, this suggests the kernel itself is genuinely underdetermined by the evidence and the three readings are not resolvable by better data but reflect an irreducible framing contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Whether reading selection correlates with structural position, indicating irreducible framing contest rather than resolvable ambiguity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t1993, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 1993, 0.08).
narrative_ontology:measurement(eu_c_tr_t1999, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 1999, 0.09).
narrative_ontology:measurement(eu_c_tr_t2004, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2004, 0.1).
narrative_ontology:measurement(eu_c_tr_t2009, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2009, 0.1).
narrative_ontology:measurement(eu_c_tr_t2016, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2016, 0.11).
narrative_ontology:measurement(eu_c_tr_t2024, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t1993, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 1993, 0.22).
narrative_ontology:measurement(eu_c_be_t1999, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 1999, 0.24).
narrative_ontology:measurement(eu_c_be_t2004, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2004, 0.26).
narrative_ontology:measurement(eu_c_be_t2009, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2009, 0.26).
narrative_ontology:measurement(eu_c_be_t2016, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2016, 0.27).
narrative_ontology:measurement(eu_c_be_t2024, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2024, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(eu_council_unanimity__sovereignty_guarantor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__sovereignty_guarantor_reading, 0.12).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language concept 'EU Council unanimity requirement.' Each reading of the eu_council_unanimity kernel instantiates a structurally distinct constraint with its own ε: sovereignty_guarantor_reading (this file, ε=0.28, rope, universal beneficiary set, empty victim set), veto_trap_reading (expected higher ε, snare or tangled_rope, concentrated beneficiary = holdout state, victim set = blocked coalition), and diplomatic_capital_reading (expected moderate ε, rope or scaffold, beneficiary = policy legitimacy / all negotiating parties). The three files share the underlying rule text but diverge on beneficiary/victim structure and extraction because they model different structural claims about what the veto does, per the ε-invariance principle — this is not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
