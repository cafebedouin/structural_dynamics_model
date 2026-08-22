% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: EU Council Unanimity as Sovereignty Protection (Rights-Exercise Reading)
 *   domain: institutional/political/international relations
 *
 * SUMMARY:
 *   In the European Union's Council of Ministers, major categories of
 *   decision-making require unanimity: foreign policy, taxation,
 *   constitutional matters, and key aspects of social and labor policy. This
 *   constraint is ONE READING of the unanimity rule. Under the
 *   sovereignty-guarantor reading, unanimity is not a source of extraction or
 *   veto-trap leverage, but a foundational protection of state agency within
 *   the supranational order. Small member states benefit structurally: their
 *   veto power is legitimate rights-exercise, not coercive leverage. The
 *   constraint's extractiveness is moderate (0.38) because coordination costs
 *   are real—consensus-building is slower than majority rule—but these costs
 *   are the price of preserving sovereignty, not the mark of a snare.
 *   Suppression is minimal (0.12) because the constraint operates through
 *   rightful authority (each state consents), not coercion. Theater is
 *   low-to-moderate (0.18 at end) because the sovereignty function is
 *   generally regarded as legitimate, though recent instances of member-state
 *   blocking (Hungary on Ukraine aid, Poland on LGBTQ policies) have invited
 *   the counter-reading (veto_trap_reading) that reframes the same blocking
 *   as minoritarian obstruction. This reading and the veto_trap reading
 *   coexist: they are live positions held by different institutional actors
 *   and different member states.
 *
 * KEY AGENTS:
 *   - Small member states (Czech Republic, Ireland, Luxembourg, Slovenia, Malta, Cyprus): structural beneficiaries of unanimity; their veto protects them against majoritarian overruling on sovereignty-touching issues.
 *   - Large member states (France, Germany, Italy, Spain): both beneficiaries (they also hold veto power) and payers (consensus-building slows their legislative agenda).
 *   - European Commission: institutional administrator of the unanimity rule; no direct extraction or benefit.
 *   - European Parliament: structurally excluded from many unanimity-required decisions; would argue for majoritarian democratic rule.
 *   - Supranational constituencies and transnational movements: excluded from the veto right; would argue unanimity empowers states to block policies majorities of Union citizens support.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.38).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.12).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity as Sovereignty Protection (Rights-Exercise Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "institutional/political/international relations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, 'd72650f9-5e8e-4bbe-a4ed-9f7ecf3b8f26').
narrative_ontology:cs_kernel_codification('d72650f9-5e8e-4bbe-a4ed-9f7ecf3b8f26', formalized).
narrative_ontology:cs_authority_grounding('d72650f9-5e8e-4bbe-a4ed-9f7ecf3b8f26', lineage).
narrative_ontology:cs_interpretation_layer_present('d72650f9-5e8e-4bbe-a4ed-9f7ecf3b8f26').
narrative_ontology:cs_reading_relation('d72650f9-5e8e-4bbe-a4ed-9f7ecf3b8f26', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('d72650f9-5e8e-4bbe-a4ed-9f7ecf3b8f26', eu_council_unanimity__diplomatic_capital_reading, influences).
narrative_ontology:cs_axiom('d72650f9-5e8e-4bbe-a4ed-9f7ecf3b8f26', foundational, state_consent_foundational_legitimacy).
narrative_ontology:cs_axiom_status(state_consent_foundational_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d72650f9-5e8e-4bbe-a4ed-9f7ecf3b8f26', state_consent_foundational_legitimacy, deontological).
narrative_ontology:cs_axiom('d72650f9-5e8e-4bbe-a4ed-9f7ecf3b8f26', secondary, sovereignty_protection_requires_veto).
narrative_ontology:cs_axiom_status(sovereignty_protection_requires_veto, holdable).
narrative_ontology:cs_axiom_grounding('d72650f9-5e8e-4bbe-a4ed-9f7ecf3b8f26', sovereignty_protection_requires_veto, instrumental).
narrative_ontology:cs_reference_frame('d72650f9-5e8e-4bbe-a4ed-9f7ecf3b8f26', state_sovereign_union_framework).
narrative_ontology:cs_drift_state('d72650f9-5e8e-4bbe-a4ed-9f7ecf3b8f26', post_lisbon_treaty_federalism_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d72650f9-5e8e-4bbe-a4ed-9f7ecf3b8f26', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, sovereignty_preserving_coalitions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with populations under 5 million whose structural interest in EU decisions runs counter to likely majority coalitions (smaller economies in energy, agricultural, or financial regulation contexts). Unanimity is their structural protection against being outvoted by the Franco-German axis or majority coalitions. Their veto is a rights-exercise, not extraction: when they block, they are defending their sovereignty interests, not extracting concessions through coercion. Exit is constrained by economic integration and geographic proximity, but they choose to remain because unanimity makes membership tenable.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states, beneficiary,
    moderate, generational, constrained, regional).

% France, Germany, Italy, Spain benefit from EU coordination on trade and defense but bear a coordination cost: they cannot pass legislation favored by 85% of the Union because one small state can block. They are payers insofar as unanimity slows their agenda; they are beneficiaries insofar as they also hold veto power and use it defensively (when German environmental standards threaten Polish coal interests, Germany pays the coordination cost of consensus-building). Mobility is real: they could contemplate opt-out or tiered integration, but remain committed to the Union framework.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, payer,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, beneficiary).

% Drafts and proposes legislation, mediates member state negotiation, and administers the unanimity rule in practice. Does not directly benefit or pay: it is the institutional apparatus implementing the sovereign consent requirement. Its power is structural (it initiates), not extractive (it does not collect from the constraint).
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, european_commission, agenda_setter,
    institutional, generational, analytical, regional).

% Has no veto in many Council decisions (especially in foreign policy, taxation, social affairs). Would argue for majority voting to amplify democratic representation; is systematically excluded from this constraint's core decision space. Their position would be that unanimity empowers states to overrule democratic majorities on issues with Union-wide legitimacy.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, european_parliament, excluded,
    institutional, generational, trapped, regional).

% Transnational constituencies (environmental movements, digital-rights advocates, labor unions) that would benefit from majority-rule Union-wide standards but are blocked by small states acting unilaterally. They are excluded from the veto right because they are not state actors; they would argue that unanimity subordinates transnational interests to parochial state sovereignty claims.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, supranational_federalists, excluded,
    organized, generational, trapped, regional).

% Studies the constraint's operation across time, examining what gets blocked, by whom, under what pressure. Neutral analytic seat.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, analytical_observer, observer,
    analytical, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solving the coordination problem of preserving sovereign state agency within a supranational union: member states retain the right to refuse participation in actions they judge incompatible with their constitutional or vital interests. This prevents the Union from becoming a majoritarian superstate that subordinates minorities to majority rule on matters touching statehood itself.
% TRANSFER_FUNCTION: The constraint does not move a material transfer. It transfers a right: the right to withhold consent. In this reading, small states receive the affirmation of their sovereignty status and the power to defend it; large states incur the cost of negotiating consensus rather than imposing majority rule. No party extracts rents; all parties exercise their rightful authority.
% ABSENT_VOICES: Supranational constituencies (transnational movements, citizen coalitions formed across borders around shared policy interests) would argue that unanimity empowers states to block policies that majorities of Union citizens support. They are excluded because the veto belongs to state governments, not to transnational democratic majorities. The constraint reflects a state-centric legitimacy framework, not a transnational democratic one.
% DISAPPEARANCE_RATIONALE: If unanimity disappeared and simple or qualified majority voting replaced it across all domains, small member states would immediately lose their structural protection against outmaneuvering on policies touching sovereignty (military, taxation, labor standards, energy). Some would likely exit the Union or demand opt-out provisions; the remaining architecture would be a majoritarian superstate rather than a voluntary union of equals. The entire political equilibrium turns on this rule.
% FOUNDING_PROBLEM: How to create a supranational coordination mechanism (necessary for trade, security, environmental problems that cross borders) without subordinating smaller polities to majoritarian rule and converting the union into a new form of domination by powerful states. The founding insight: if states consent to be bound only by rules they accept, they remain sovereign agents rather than subjects of a superstate.
% FOUNDING_PROBLEM_CORROBORATION: EU founding treaties (Rome Treaties, Single European Act preambles) explicitly frame unanimity as protecting the constitutional autonomy of member states. Constitutional courts in several small member states (Czech Republic, Poland, Hungary) have grounded their review of EU acts in the principle that unanimity and member state veto are foundational to the sovereign consent that legitimates Union authority. Larger states acknowledge the founding problem implicitly in their negotiating practice: they never try to eliminate unanimity globally, only to carve out domains for qualified majority voting — they assume the sovereignty-protection function is legitimate.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.38) rather than low because unanimity imposes real coordination costs: building consensus across 27 member states with conflicting interests is slower and more expensive than majority voting. However, the reading treats these costs as the legitimate price of sovereignty protection, not as extraction imposed by an agent on targets. Small states are not targets paying extraction to large states; rather, all states are rights-holders exercising their rightful authority to withhold consent. The measurement series is relatively flat (ranging 0.35–0.40) because the constraint's core function—protecting sovereignty—has been stable across the interval, though recent political polarization (Hungary's blocking on Ukraine, Poland's stance on judicial independence) has created pressure toward the veto_trap reading interpretation. Suppression is minimal (0.12) because the constraint operates through formal authority (each state has equal veto right) rather than coercion. The modest rise in theater_ratio toward 2020 (peaking at 0.22) reflects increased rhetorical defense of unanimity in response to proposals for qualified majority voting in foreign policy (Lisbon Treaty aftermath, Brexit, migration crisis): more of the constraint's operation became performative justification rather than background rule. The dip in 2024 reflects the recent invocation of emergency procedures (Article 50 procedures, simplified in response to Russian invasion of Ukraine) that briefly allowed some decisions without unanimity, deflating the theater temporarily.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute to different types from each other. From the small-state seat, the constraint is a rope: it solves the coordination problem of preserving sovereignty within a supranational union, and all parties are willing participants in this solution (they agreed to it in the founding treaties and have not departed). From the large-state seat, the constraint may compute closer to tangled_rope or piton: large states pay a coordination cost for a benefit (sovereignty protection) they also enjoy, but when their legislative agenda is blocked by small-state veto, they experience the constraint as extractive (they are paying without directly receiving). The European Parliament seat would compute toward snare (majorities of Union citizens are blocked by minority states), while the supranational constituencies seat does not compute at all because those actors are excluded from the decision-making frame. The engine's per-seat computation captures these divergences; this story authors the structural data from which those computations follow.
 *
 * DIRECTIONALITY LOGIC:
 *   Small member states are the primary beneficiaries: unanimity is their structural protection, and exercising the veto is rights-exercise, not extraction. Directionality for small states is low (~0.15), reflecting that they collect the benefit of veto power without being extracted from. Large states sit near symmetric (d ~0.45): they incur the coordination cost of consensus-building when they want to advance legislation, but they also benefit from their own veto power when their interests diverge from the majority, and they benefit from the Union framework itself (which unanimity helps preserve by preventing majoritarian domination). The European Commission and Parliament are institutional actors; their directionality is analytical. Supranational constituencies are excluded stakeholders: they would be targets of the constraint (their transnational preferences are blocked) if they were in the decision-making frame, but because they are not state actors, the constraint does not directly extract from them—it structurally privileges state-level preferences over transnational ones. The secondary_role assignments (large_member_states holding both payer and beneficiary) reflect that these actors experience dual position: they pay when their agenda is blocked (payer), and benefit when their sovereignty is protected (beneficiary).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by anchoring the constraint's justification to an ongoing, live problem: the need to preserve member state sovereignty within a supranational order. The founding mandate is not obsolete; rather, it is contested. Different institutional frames (constitutional courts defending state autonomy, federalist movements pushing for transnational democracy, intergovernmental conferences negotiating EU architecture) read the founding problem differently. The sovereignty reading holds that the problem remains live and pressing: states retain sufficient preference-divergence on sovereignty-touching issues (energy, labor, taxation, foreign policy) that a majority could impose its will to the detriment of minorities, and unanimity is the structural answer. The veto_trap reading holds the founding problem is dead in one sense (member states are not imperiled by majoritarian overruling in the classical sense) but the constraint persists as a vehicle for extractive blocking. The diplomatic_capital reading holds the founding problem is partly obsolete as a sovereignty question but the constraint has evolved a new function: it forces consensus-building that improves policy legitimacy. None of these readings can be resolved by empirical fact alone—the disagreement is about what legitimates the constraint, not whether it functions as described. Mandatrophy in this case would be if all three readings converged on 'the founding problem is dead but the constraint persists for institutional inertia,' which is not yet the case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    blocking_as_rights_vs_extraction,
    'When a small state blocks a Council decision through its veto, is this exercise of a legitimate sovereign right, or is it coercive extraction (leveraging veto power to force concessions)?',
    'Post-blocking analysis: does the blocking state receive concessions beyond what consensus-building would naturally produce? If Hungary blocks a decision and receives significant side payments or carve-outs, that signals extraction (veto_trap reading). If blocking is sustained despite pressure and without side payment (exercising the right to say no), that signals rights-exercise (sovereignty reading).',
    'If blocking is predominantly rights-exercise, the constraint is rope (coordination with minimal extraction). If blocking regularly involves extractive side-payment leveraging, the constraint is tangled_rope or snare. The ambiguity is located in what blocking means structurally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blocking_as_rights_vs_extraction, empirical, 'Whether blocking is legitimate sovereignty defense or coercive leverage.').

omega_variable(
    small_state_benefit_reality,
    'Do small member states actually benefit from unanimity, or are they passive beneficiaries whose protection exists only because large states choose not to override them?',
    'Counterfactual analysis: in a majority-voting system, how would small states'' interests fare? Evidence from policy domains that have already shifted to qualified majority voting (internal market decisions) vs. unanimity domains (foreign policy, taxation) shows the concrete difference.',
    'If small states would be substantially outvoted in a majority system and preserve better outcomes under unanimity, the benefit is real and structural (rope reading reinforced). If they would achieve similar outcomes through coalition-building and negotiating power (size-independent), then unanimity''s protection is less substantial, and the constraint may be less beneficial than the sovereignty reading claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_state_benefit_reality, empirical, 'Whether unanimity provides structural protection small states would lose under majority rule.').

omega_variable(
    sibling_reading_coexistence,
    'Can the sovereignty-guarantor reading and the veto-trap reading coexist in the same institutional framework, or do they necessarily foreclose each other?',
    'Institutional design analysis: can unanimity be structured to preserve sovereignty protection while reducing extractive veto-leverage (e.g., through threshold mechanisms, transparency rules, or side-payment restrictions)? If yes, the readings coexist and can both be true (different actors leverage the same rule differently). If the only way to eliminate veto-trap behavior is to eliminate veto power entirely, the readings foreclose each other.',
    'If they coexist, both sibling readings have ''coexists_with'' relation to this reading. If they foreclose, one or more has ''forecloses'' relation. This omega resolves the reading_relations structure in cs_structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether the sovereignty and trap readings can coexist or necessarily foreclose.').

omega_variable(
    commission_actor_status,
    'Is the European Commission a neutral administrator of the unanimity rule, or does it benefit structurally from unanimity by maintaining its agenda-setting power and mediating role?',
    'Institutional analysis: if unanimity were replaced by qualified majority voting, would the Commission''s power increase (because it could work with 55% of member states) or decrease (because the European Parliament gains power)? What are the Commission''s actions in debates about voting rules?',
    'If the Commission benefits, it should be listed as beneficiary, not as neutral agenda_setter. If it is neutral, the stakeholder assignment stands. This affects the structural reading of who coordinates and who collects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commission_actor_status, empirical, 'Whether the Commission is a beneficiary or a neutral administrator.').

omega_variable(
    transnational_democratic_legitimacy,
    'Does unanimity at the state level represent legitimate democratic consent, or does it subordinate transnational democratic majorities (the majority of EU citizens) to state veto power?',
    'Legitimacy comparison: polling data on citizen support for EU-wide policies blocked by small-state veto (e.g., climate policy, digital regulation) shows whether unanimity protects minorities or suppresses transnational majorities. This is not empirically resolvable—it depends on what democracy means (state-centric or transnational).',
    'If democracy means state-sovereign consent, unanimity is a protection. If democracy means transnational majority rule, unanimity is a suppression. This omega documents the reading-frame ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transnational_democratic_legitimacy, preference, 'State-sovereign vs. transnational-democratic frame for legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t1993, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(eu_c_tr_t2000, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(eu_c_tr_t2008, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(eu_c_tr_t2016, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2016, 0.18).
narrative_ontology:measurement(eu_c_tr_t2020, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(eu_c_tr_t2024, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t1993, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 1993, 0.35).
narrative_ontology:measurement(eu_c_be_t2000, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2000, 0.36).
narrative_ontology:measurement(eu_c_be_t2008, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2008, 0.37).
narrative_ontology:measurement(eu_c_be_t2016, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2016, 0.39).
narrative_ontology:measurement(eu_c_be_t2020, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(eu_c_be_t2024, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t1993, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 1993, 0.08).
narrative_ontology:measurement(eu_c_su_t2000, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2000, 0.09).
narrative_ontology:measurement(eu_c_su_t2008, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2008, 0.1).
narrative_ontology:measurement(eu_c_su_t2016, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2016, 0.12).
narrative_ontology:measurement(eu_c_su_t2020, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2020, 0.15).
narrative_ontology:measurement(eu_c_su_t2024, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2024, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__sovereignty_guarantor_reading, 0.2).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__diplomatic_capital_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_qualified_majority_voting__secondary_framework).

% DUAL FORMULATION NOTE:
% The EU Council unanimity rule is a contested kernel with three distinct readings: sovereignty_guarantor_reading (this story) frames unanimity as legitimate state-sovereign protection; veto_trap_reading frames it as enabling minoritarian extraction; diplomatic_capital_reading frames it as consensus-forcing legitimacy mechanism. Each reading shares the same formal rule (unanimity requirement) but interprets its justification and beneficiary structure differently. The three readings coexist as live institutional framings held by different institutional actors. They are not empirical alternatives (whether unanimity exists is not contested); they are normative/conceptual alternatives (what unanimity justifies and who benefits from it). Each reading has its own constraint story with independent ε, stakeholder structure, and beneficiary/victim set. They are linked via network.affects_constraints to enable contamination analysis: if one reading's political coalition weakens, the others shift toward dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
