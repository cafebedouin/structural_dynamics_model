% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: EU Council Unanimity as Sovereignty Guarantor
 *   domain: institutional_design/international_relations
 *
 * SUMMARY:
 *   The EU Council unanimity requirement mandates that all member states
 *   consent to decisions in domains designated as sovereignty-sensitive
 *   (foreign policy, tax coordination, certain constitutional amendments).
 *   Under the sovereignty-guarantor reading, unanimity is a foundational
 *   protection: it prevents majoritarian override of states' core interests
 *   and enables small states to exercise legitimate veto authority. Veto use
 *   is rights-exercise, not extraction. The constraint is claimed as Rope
 *   (genuine coordination solving a real union-formation problem) while
 *   measured metrics show moderate extractiveness and low suppression — the
 *   gap is intentional and reflects the reading's core claim that blocking,
 *   while costly to large states, is not inherently extractive but rather a
 *   structural protection.
 *
 * KEY AGENTS:
 *   - Small states: moderate power, protected by veto authority, beneficiaries of the unanimity rule
 *   - Large states: powerful, experience unanimity as coordination cost and loss of majoritarian leverage, payers
 *   - Integration maximalists: organized advocates for deeper union, constrained by unanimity blocking capacity, payers
 *   - Supranational institutions: institutional agenda-setters interpreting the scope of sovereignty-implicating proposals
 *   - European Parliament: excluded from unanimity-protected domains despite representing direct constituencies
 *   - Third states: external observers experiencing EU response-capacity constrained by unanimity requirements
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
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity as Sovereignty Guarantor").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "institutional_design/international_relations").

domain_priors:requires_active_enforcement(eu_council_unanimity__sovereignty_guarantor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, '6196e0f2-a1f6-45fa-a1da-9ae2442176cf').
narrative_ontology:cs_kernel_codification('6196e0f2-a1f6-45fa-a1da-9ae2442176cf', formalized).
narrative_ontology:cs_authority_grounding('6196e0f2-a1f6-45fa-a1da-9ae2442176cf', extraction).
narrative_ontology:cs_interpretation_layer_present('6196e0f2-a1f6-45fa-a1da-9ae2442176cf').
narrative_ontology:cs_reading_relation('6196e0f2-a1f6-45fa-a1da-9ae2442176cf', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('6196e0f2-a1f6-45fa-a1da-9ae2442176cf', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('6196e0f2-a1f6-45fa-a1da-9ae2442176cf', foundational, state_consent_sovereign_decisions).
narrative_ontology:cs_axiom_status(state_consent_sovereign_decisions, holdable).
narrative_ontology:cs_axiom_grounding('6196e0f2-a1f6-45fa-a1da-9ae2442176cf', state_consent_sovereign_decisions, deontological).
narrative_ontology:cs_axiom('6196e0f2-a1f6-45fa-a1da-9ae2442176cf', foundational, veto_as_rights_exercise_not_extraction).
narrative_ontology:cs_axiom_status(veto_as_rights_exercise_not_extraction, holdable).
narrative_ontology:cs_axiom_grounding('6196e0f2-a1f6-45fa-a1da-9ae2442176cf', veto_as_rights_exercise_not_extraction, deontological).
narrative_ontology:cs_reference_frame('6196e0f2-a1f6-45fa-a1da-9ae2442176cf', sovereign_union_of_equal_states).
narrative_ontology:cs_drift_state('6196e0f2-a1f6-45fa-a1da-9ae2442176cf', post_lisbon_treaty_geopolitical_pressure, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6196e0f2-a1f6-45fa-a1da-9ae2442176cf', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, sovereignty_principle).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, large_states).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, integration_maximalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected by unanimity requirement from being overridden on matters touching national sovereignty. Can block proposals they deem existentially threatening without needing coalition support or superior resources. Their veto capacity is proportional to their legal standing, not their economic or military power. Exit from the framework forecloses participation in collective benefits but preserves unilateral sovereignty.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_states, beneficiary,
    moderate, generational, constrained, continental).

% Coordinated action on matters requiring unanimity is slower and more difficult to execute than majoritarian decision-making would allow. Must negotiate with small states as formal equals despite asymmetric resources. Can be blocked by single small-state veto on initiatives that advance large-state interests. Cannot override or override-proof decisions through aggregated power.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, large_states, payer,
    powerful, generational, constrained, continental).

% Advocacy parties favoring deeper political union and centralized decision-making. Unanimity is experienced as a structural veto against federalizing agendas. Can frame small-state blocking as obstructionism or nationalist holdout rather than legitimate sovereignty defense. Their exit is reframing or institutional capture, not organizational departure.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, integration_maximalists, payer,
    organized, biographical, mobile, continental).

% Administers the unanimity rule as formal requirement and interprets its application to specific proposals. Mediates between blocking states and coordinating majority, manages the framing of proposals (whether they 'implicate sovereignty' and therefore require unanimity vs. fall under qualified majority procedures). Their interpretation shapes the constraint's practical scope.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, supranational_institutions, agenda_setter,
    institutional, generational, analytical, continental).

% Represents directly-elected constituencies across all member states but has no formal vote in the unanimity-protected domains (foreign policy, tax coordination). Would advocate for majority voting in these areas to amplify its institutional power. Their voice is systematically absent from the decisional structure that unanimity protects.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, european_parliament, excluded,
    organized, biographical, constrained, continental).

% External observers of EU decision-making experiencing its pace and predictability as determined by unanimity requirements. Can observe that single small-state blocking power constrains EU responses to geopolitical challenges. Their position is analytical; they experience outcomes but do not participate in decisions.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, third_states, observer,
    powerful, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__sovereignty_guarantor_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__sovereignty_guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unanimous consent requirement coordinates sovereign states into a collective framework while protecting each state's capacity to withhold acceptance from arrangements it judges incompatible with its fundamental interests. Solves the problem of aggregating independent political communities into a functioning union without subordinating any to majoritarian override on existentially sensitive matters.
% TRANSFER_FUNCTION: Moves decision-making authority from individual states (pre-union) to a collective body, with a gating condition: the transfer is valid only when unanimous. Small states transfer authority to the collective on most economic and regulatory matters; they retain veto authority on matters the rule classifies as sovereignty-implicating. Large states accept this asymmetry as the structural price of union with autonomous partners.
% ABSENT_VOICES: Directly-elected constituency representatives (European Parliament) have no formal vote in unanimity-protected domains, despite representing millions of citizens. Minority parties and movements within member states advocating for majoritarian decision-making are structurally excluded from the negotiating table; only state representatives have standing. Citizens experiencing constraint as blocking obstructionism rather than sovereignty defense are not seated.
% DISAPPEARANCE_RATIONALE: If unanimity disappeared and qualified-majority voting became the standard for all Council decisions, small states would immediately lose their blocking capacity on sovereignty-sensitive matters. The pace of EU decision-making would accelerate on foreign policy and tax coordination; small-state veto threats would evaporate; the internal negotiating structure would reorganize around large-state coalitions capable of forming working majorities. Small-state political alignment strategies would shift radically.
% FOUNDING_PROBLEM: How to construct a political union of independent sovereign states without subordinating any state to the will of others on matters of existential national concern. The founding problem is structural: unanimous consent is the mechanism that permits union-formation while preserving veto authority over the substantive scope of that union.
% FOUNDING_PROBLEM_CORROBORATION: Treaty texts and founding conventions (Schuman Declaration, Treaty of Rome preamble) attest unanimity as a deliberate protection against majoritarian override. Small-state negotiators in founding and accession documents explicitly condition membership on unanimity in sovereignty-sensitive domains. This corroboration comes from the founding-era parties themselves and from subsequent accession negotiations where each new small state negotiated unanimity guarantees as a condition of membership.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Under this reading, extractiveness is moderate (0.38) rather than low or high. Extractiveness is non-zero because large states incur real coordination costs: decision-making is slower, unanimous agreement requires iterative negotiation, and small-state blocking power does constrain large-state initiatives. However, extractiveness is not high because the measured extraction does not accrue to a concentrated beneficiary; small states exercise veto authority as a legitimate right, not as a tool for collecting systematic transfer. The beneficiary is the sovereignty principle itself, vindicated through each state's consent requirement. Suppression is low (0.12) because the rule is formally transparent and codified; small states face no hidden coercive pressure beyond the institutional cost of maintaining unanimity. Theater ratio is low (0.18) because the rule's enforcement is straightforward — proposals either achieve unanimity or fail — rather than performative. Accessibility collapse is moderate (0.65) because alternatives to the unanimity-protected framework do exist (withdrawal, majoritarian voting in non-protected domains, variable geometry), but the cost of exit is high for small states that depend on the union's benefits.
 *
 * PERSPECTIVAL GAP:
 *   Payer seats (large states, integration maximalists) should compute as experiencing higher extractiveness and classification pressure toward snare or tangled-rope; beneficiary seats (small states) should compute toward rope or mountain. The supranational agenda-setter seat should compute toward a hybrid: it benefits from the clarity and legitimacy the unanimity rule provides, but is constrained by it. European Parliament observers should compute the rule as a suppressive external constraint limiting their institutional power, even though they are not formal payers. The engine computes per-seat types from the structural data; this reading predicts large-state seats will show snare characteristics (extraction + lack of alternatives) while small-state seats will show rope characteristics (coordination + symmetric benefit).
 *
 * DIRECTIONALITY LOGIC:
 *   Small states sit at the beneficiary end of directionality (d near 0.0–0.3): they receive protection from majoritarian override and can exercise blocking authority proportional to their legal standing, not their material power. Large states sit at the payer end (d near 0.6–0.8): they bear the cost of slower decision-making and must negotiate concessions to achieve unanimity. Integration maximalists sit near the payer end (d ~0.65) because they experience unanimity as a structural block on their preferred federalizing agenda. Supranational institutions sit near symmetric (d ~0.45–0.50): they both administer the constraint and experience it as a constraint on their capacity for autonomous institutional evolution. European Parliament sits outside the formal directionality calculation because it holds no seat in the decision-making body; its exclusion is structural.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting small-state sovereignty in a union context) remains substantively live, though its practical urgency shifts with EU enlargement and geopolitical pressure toward faster decision-making. Mandatrophy is not present under this reading: the rule has not outlived its function. Mandatrophy would emerge if small states ceased to value the protection (because they no longer feared majoritarian override) while large states continued to bear the coordination cost. This has not occurred; small states continue to mobilize unanimity protection when they perceive substantive threats. The measured theater ratio (0.18, stable across the interval) shows the rule is not performing identity maintenance in the absence of function — blocking proposals are issued and defended, not merely gestured at.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_implication_boundary,
    'What propositions legitimately count as ''implicating sovereignty'' and therefore falling under unanimity protection vs. mere sectoral policy requiring qualified majority?',
    'Institutional case law and explicit doctrinal clarification from supranational courts and treaty bodies; comparative analysis of how proposals have been classified across time and negotiating contexts.',
    'If the boundary shifts or becomes contested, the protection afforded by unanimity erodes implicitly without formal amendment. A broad reading of ''sovereignty-implicating'' strengthens small-state protection; a narrow reading reduces the scope of veto authority while preserving its formal existence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_implication_boundary, conceptual, 'The interpretive boundary determining which proposals fall under unanimity protection.').

omega_variable(
    veto_threat_vs_veto_use,
    'Is the measured extractiveness capturing veto threats (credible blocking postures used to extract concessions) or genuine rights-exercise (blocking that is exercised when fundamental interests are at stake)?',
    'Temporal analysis of blocking behavior: if veto threats precede compromise and blocking is infrequent when threats fail, the constraint operates as negotiating leverage; if blocking is exercised without prior threat and is driven by stated principled objection, it operates as rights-defense.',
    'High veto-threat behavior relative to actual blocking would shift the classification toward snare (minorities extracting rents through credible blocking); low threat-to-use ratio supports the sovereignty-defense framing. This omega documents the reading-internal variation between threat-as-extraction and veto-as-right.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_threat_vs_veto_use, empirical, 'Whether measured extractiveness reflects strategic threat behavior or principled rights-exercise.').

omega_variable(
    reading_contest_foreclosure,
    'Does the sovereign-defense reading logically foreclose the veto-trap reading, or do both remain coherent descriptions of the same institutional structure?',
    'Structural analysis: if both readings can accommodate the same observed blocking behavior and negotiating patterns, they coexist; if one reading''s core claim directly contradicts the other''s such that no institutional framework could hold both premises, the stronger reading forecloses the weaker.',
    'Coexistence strengthens the kernel-contest frame: both readings are live positions and the corpus should track which reading is ascendant in specific contexts. Foreclosure would mean one reading''s adoption eliminates the other as a live institutional claim. This omega documents the kernel structure itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Whether the sovereignty-guarantor and veto-trap readings are logically independent or mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unanimity_sovereign_guar_tr_t0, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(unanimity_sovereign_guar_tr_t5, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(unanimity_sovereign_guar_tr_t10, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(unanimity_sovereign_guar_tr_t15, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(unanimity_sovereign_guar_tr_t20, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(unanimity_sovereign_guar_tr_t25, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(unanimity_sovereign_guar_tr_t30, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 30, 0.18).

% Extraction over time
narrative_ontology:measurement(unanimity_sovereign_guar_be_t0, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(unanimity_sovereign_guar_be_t5, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(unanimity_sovereign_guar_be_t10, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(unanimity_sovereign_guar_be_t15, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement(unanimity_sovereign_guar_be_t20, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(unanimity_sovereign_guar_be_t25, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(unanimity_sovereign_guar_be_t30, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(unanimity_sovereign_guar_su_t0, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(unanimity_sovereign_guar_su_t5, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 5, 0.09).
narrative_ontology:measurement(unanimity_sovereign_guar_su_t10, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(unanimity_sovereign_guar_su_t15, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 15, 0.11).
narrative_ontology:measurement(unanimity_sovereign_guar_su_t20, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement(unanimity_sovereign_guar_su_t25, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 25, 0.12).
narrative_ontology:measurement(unanimity_sovereign_guar_su_t30, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 30, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__sovereignty_guarantor_reading, 0.12).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% The EU_COUNCIL_UNANIMITY kernel decomposes into three structurally distinct constraint stories, each a reading of the same formal rule. The sovereignty-guarantor reading (this story) frames veto use as legitimate rights-exercise protecting state-level autonomy. The veto-trap reading frames the same rule as enabling minority extraction through credible blocking threats. The diplomatic-capital reading frames it as forcing consensus-building and policy legitimacy. These are not three measurements of one constraint — they are three distinct constraints instantiated by three readings of a contested kernel. Network edges link them as a family; per-seat engine classifications will diverge across the readings, revealing which reading's framing is structural and which are cover narratives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_council_unanimity__sovereignty_guarantor_reading, moderate, 0.15).
constraint_indexing:directionality_override(eu_council_unanimity__sovereignty_guarantor_reading, powerful, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
