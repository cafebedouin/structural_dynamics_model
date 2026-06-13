% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: RBIO Hegemonic Norm Complex: Formal Revisability / Practical Rigidity
 *   domain: international_relations/political_economy
 *
 * SUMMARY:
 *   The Responsibility to Protect (R2P), sovereignty norms, non-intervention
 *   principles, and international humanitarian law form a interconnected
 *   institutional and legal complex (RBIO) that claims universal application
 *   but exhibits selective enforcement. From the hegemonic-extraction
 *   reading: the norms are formally revisable (any state can propose
 *   amendments) but practically un-amendable (P5 veto prevents constraints on
 *   their power); enforcement is selective (authorized against non-aligned
 *   states, not against allies); and economic conditionality imposes
 *   structural adjustment on Global South borrowers without meaningful
 *   consent. This reading instantiates the constraint as tangled rope: there
 *   is a genuine coordination function (interstate procedure, humanitarian
 *   standards, collective legitimacy) layered beneath asymmetric extraction
 *   (selective enforcement, veto power, conditionality coercion). The
 *   practical rigidity (formal revisability, actual un-amendability) and
 *   enforcement selectivity (revealing extractive intent) are the signature
 *   features. This is one reading of a contested kernel; the
 *   liberal_institutional_reading and sovereignty_maximalist_reading are
 *   separate constraint stories with different ε values and
 *   beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - permanent_five_states: agenda-setters; control veto and interpretation; trapped in the system (cannot exit without losing power)
 *   - us_european_capital: beneficiaries; gain from enforcement selectivity and conditionality; arbitrage-grade exit (can ignore unfavorable rulings)
 *   - global_south_states: payers; subject to selective enforcement and constrained by veto; moderate power, constrained exit
 *   - structural_adjustment_populations: payers; bearers of conditionality costs; identity-locked to their geography and citizenship
 *   - non_aligned_states: excluded from agenda-setting; would demand binding non-intervention and majority-vote reforms
 *   - imf_world_bank: agenda-setters and beneficiaries; administer conditionality; shielded by technocratic neutrality
 *   - human_rights_ngos: observers; provide corroboration from outside benefiting parties; split between liberal and critical readings
 *   - academic_international_law: observers; theoretical and doctrinal corroboration; competing interpretive traditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.79).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.71).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "RBIO Hegemonic Norm Complex: Formal Revisability / Practical Rigidity").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international_relations/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, 'cef7b61c-1e25-4073-a58e-d5c538a69ae4').
narrative_ontology:cs_kernel_codification('cef7b61c-1e25-4073-a58e-d5c538a69ae4', fixed_text).
narrative_ontology:cs_authority_grounding('cef7b61c-1e25-4073-a58e-d5c538a69ae4', extraction).
narrative_ontology:cs_interpretation_layer_present('cef7b61c-1e25-4073-a58e-d5c538a69ae4').
narrative_ontology:cs_reading_relation('cef7b61c-1e25-4073-a58e-d5c538a69ae4', rbio_practice_norm_complex__liberal_institutional_reading, influences).
narrative_ontology:cs_reading_relation('cef7b61c-1e25-4073-a58e-d5c538a69ae4', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('cef7b61c-1e25-4073-a58e-d5c538a69ae4', foundational, p5_veto_forecloses_amendment).
narrative_ontology:cs_axiom_status(p5_veto_forecloses_amendment, holdable).
narrative_ontology:cs_axiom_grounding('cef7b61c-1e25-4073-a58e-d5c538a69ae4', p5_veto_forecloses_amendment, empirically_contingent).
narrative_ontology:cs_axiom('cef7b61c-1e25-4073-a58e-d5c538a69ae4', foundational, enforcement_selectivity_reveals_power).
narrative_ontology:cs_axiom_status(enforcement_selectivity_reveals_power, holdable).
narrative_ontology:cs_axiom_grounding('cef7b61c-1e25-4073-a58e-d5c538a69ae4', enforcement_selectivity_reveals_power, empirically_contingent).
narrative_ontology:cs_reference_frame('cef7b61c-1e25-4073-a58e-d5c538a69ae4', universal_consent_based_revisable_norms).
narrative_ontology:cs_drift_state('cef7b61c-1e25-4073-a58e-d5c538a69ae4', contemporary_enforcement_selectivity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cef7b61c-1e25-4073-a58e-d5c538a69ae4', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, permanent_five_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, structural_adjustment_populations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__hegemonic_extraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.79) is high because: (1) selective enforcement concentrates benefits on P5/allied states and distributes costs on Global South; (2) conditionality extracts rents beyond marginal cost of lending; (3) the system's formal revisability is practically negated by veto lock-in, so amendment is structurally unavailable to those bearing costs. Suppression (0.71) is substantial because: (1) veto power actively prevents amendment; (2) enforcement selectivity is maintained through deliberate institutional design (Security Council procedure, IMF governance, debt-restructuring authority); (3) alternatives (regional courts, non-aligned institution-building, exit from capital markets) are materially costly, making them suppressed rather than freely available. Theater ratio (0.58) is moderately high and RISING (0.38 → 0.58 over the interval) because: (1) the norms' original coordination function (preventing power anarchy, establishing procedure) has been largely displaced by performance (ceremonial reaffirmation) and power politics (enforcement follows geopolitical interest, not norm-based criteria); (2) humanitarian justifications increasingly appear disconnected from enforcement patterns. Accessibility collapse (0.42) is MODERATE and deliberately not high because: (1) alternatives exist for Global South states (regional institutions, bilateral agreements, exit — costly but not impossible); (2) for P5 states, there is no collapse (arbitrage-grade exit available); (3) the constraint persists not because alternatives are invisible but because exit is costly for most parties and blocked entirely for P5. Resistance (0.68) is substantial because: (1) Global South states actively propose amendments and challenge enforcement selectivity (General Assembly debates, regional court development, BRICS alternatives); (2) social movements contest conditionality and sovereignty violations; (3) critical scholarship contests the system's legitimacy. The metrics reflect a constraint that is genuinely extractive but not perfectly suppressed — it faces active, organized resistance that has not yet succeeded in dismantling or reforming it. The measurement trajectory shows extraction and theater ratio rising over the interval (0.62 → 0.79, 0.38 → 0.58), consistent with a constraint whose extractive intent becomes increasingly visible and whose coordination function increasingly hollowed by enforcement selectivity.
 *
 * PERSPECTIVAL GAP:
 *   The P5 and liberal institutionalists compute the constraint as rope (genuine coordination, capacity-limited enforcement): RBIO norms solve a real coordination problem; selective enforcement reflects unequal resources and institutional capacity, not intentional extraction; the system is revisable through proper procedure (though slow). Global South states and critical scholars compute it as snare (pure extraction under the cover of norms): veto lock-in makes amendment impossible; enforcement selectivity reveals power politics; conditionality is coerced. This reading (hegemonic extraction) computes it as tangled rope: BOTH coordination and extraction are real; the coordination function is genuine but layered beneath asymmetric extraction via veto power and enforcement selectivity. The engine's per-seat computation will produce this divergence: the P5 seat will compute lower extractiveness (from the beneficiary position with arbitrage exit); the Global South seat will compute higher extractiveness (from the constrained-payer position with suppressed alternatives); the observer seat will compute the authored metrics as structurally true.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality toward the constraint (d = how much it extracts from each agent): P5 states (d ≈ 0.0–0.1, full beneficiaries, veto power, arbitrage exit), U.S./European capital (d ≈ 0.1–0.2, beneficiary, institutional power, arbitrage exit), IMF/World Bank (d ≈ 0.2–0.3, mixed agenda-setter/beneficiary, institutional capture, trapped but with rents), Global South states (d ≈ 0.6–0.8, payers, moderate power, constrained exit), structural-adjustment populations (d ≈ 0.9–1.0, near-complete targets, powerless, identity-locked). The beneficiary/victim declarations drive this directionality pattern: beneficiaries are the P5 and U.S./European capital; victims are Global South states and structural-adjustment populations. Suppression is NOT scaled by directionality — it is a raw structural property (veto lock-in, enforcement selectivity); the engine scales extractiveness upward for targets (high d) and downward for beneficiaries (low d), but suppression remains 0.71 regardless of seat.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (founding problem dead but constraint persists) is CONTESTED. The P5 reads the founding problem as still live: 'Without RBIO norms we return to power anarchy.' Global South states read it as dead but captured: 'Norms exist, but selective enforcement means power politics continues unchanged.' This reading (hegemonic) splits the difference: the founding problem (need for procedure, norms, collective authority) is substantially solved in form but functionally atrophied in practice. The constraint persists not because the founding problem is live but because it redistributes power and resources in favor of the P5 — it has been captured by the very actors it was meant to constrain. The theater-ratio rise (0.38 → 0.58) suggests the coordination function is increasingly performative, supporting the mandatrophy reading. However, this is contested: liberal institutionalists argue capacity constraints are the issue, not functional death; sovereignty maximalists argue RBIO norms never solved the founding problem (power anarchy persists in disguise). The constraint qualifies for `mandatrophy_resolved: false` (it shows signs of functional atrophy but this is contested; the rise in theater ratio is diagnostic evidence, not settled fact).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_rigidity_vs_legitimacy,
    'Is the P5 veto a legitimate protection against tyranny-of-majority overreach, or a structural lock-in that makes the system un-amendable and therefore forfeits legitimacy?',
    'Empirical: track whether any amendment substantively constraining P5 power has succeeded since 1945 (answer: none). Normative: philosophical debate on consent-based authority and veto power is irresolvable without prior commitment to a theory of legitimacy.',
    'If veto is legitimate, the constraint is rope (hard to amend but stable by design). If veto forfeits legitimacy by making amendment impossible, the constraint is tangled rope masquerading as rope (formal revisability, practical extraction). This reading instantiates the second horn.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_rigidity_vs_legitimacy, conceptual, 'Whether P5 veto forecloses amendment power and thereby undermines the system''s legitimacy claim.').

omega_variable(
    selective_enforcement_mechanism,
    'Is selective enforcement a capacity problem (Global North has more resources to enforce against near-enemies) or an extractive mechanism (enforcement is deliberately selective to concentrate benefits)?',
    'Historical pattern analysis: do P5 states enforce RBIO norms against their own allies and interests, or do enforcement patterns track P5 geopolitical advantage? Archival evidence on internal deliberation; comparative case studies of symmetric/asymmetric enforcement.',
    'Capacity problem: extractiveness is lower, suppression reflects resource asymmetry, not intent. Extractive mechanism: extractiveness is high, suppression is active, intentional enforcement selectivity is core to the constraint''s function. This reading assumes the extractive mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_enforcement_mechanism, empirical, 'Whether enforcement selectivity is incidental capacity asymmetry or deliberate extraction design.').

omega_variable(
    conditionality_coercion_boundary,
    'Is IMF/World Bank conditionality a legitimate price for borrowing (lender''s right to terms) or coerced contract (creditor''s leverage over desperate states)?',
    'Counterfactual: can Global South states obtain comparable financing elsewhere on substantially better terms? What happens to their policy autonomy if they exit the system entirely (isolation vs. alternative-financing network access)? Qualitative: do borrowing states report conditionality as negotiated or as imposed?',
    'Legitimate terms: extractiveness is lower (transfer is compensation for risk and service). Coerced contract: extractiveness is high (transfer extracts beyond marginal cost; suppression is active because exit is costly and unavailable). This reading classifies conditionality as coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_coercion_boundary, empirical, 'Whether IMF/World Bank conditionality is negotiated lending terms or coercive extraction.').

omega_variable(
    humanitarian_intervention_framing,
    'Are humanitarian exceptions to non-interference legitimate (states have duties to prevent atrocities; intervention is authorized when consent fails) or pretexts for regime change (humanitarian framing legitimizes interventions that serve geopolitical/economic interests)?',
    'Pattern analysis: do humanitarian authorizations correlate with P5/allied interests (Libya, Syria no-fly zones vs. Rwanda, Yemen inaction)? Documentary evidence on internal deliberation in Security Council debates. Comparative: do the same humanitarian criteria trigger intervention when the target is an ally?',
    'Legitimate exception: extractiveness is lower (intervention serves a real coordination function — preventing atrocities). Pretext: extractiveness is high, suppression is active (humanitarian framing legitimizes selective enforcement that serves P5 interests). This reading reads humanitarian intervention as pretext.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_intervention_framing, empirical, 'Whether humanitarian intervention is a legitimate exception or a cover story for selective enforcement.').

omega_variable(
    theater_ratio_acceleration,
    'Is the rising theater ratio (0.38 → 0.58 over the interval) a sign that the norms'' original coordination function has been largely displaced by performance and enforcement selectivity?',
    'Content analysis of Security Council debates and UN documents: measure the ratio of invocations of RBIO norms that mobilize enforcement action vs. invocations that are ceremonial (reaffirming principles without action). Track whether enforcement actions increasingly cite non-RBIO justifications (geopolitical threat, great-power interest) rather than norm-based justifications.',
    'Rising theater ratio suggests the constraint''s original coordination function has atrophied, leaving mostly performance. This is consistent with the hegemonic-extraction reading: the norms persist ceremonially while power determines outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_acceleration, empirical, 'Whether rising theater ratio indicates functional atrophy of RBIO coordination.').

omega_variable(
    committer_reading_alternative,
    'This story is one reading of the contested RBIO kernel. The liberal_institutional_reading reads the same norms as universal, revisable through legitimate procedure, and enforcement selectivity as a capacity problem. The sovereignty_maximalist_reading reads RBIO norms as inherently illegitimate when they override state consent and non-interference, and humanitarian exceptions as regime-change pretexts. Which reading is structurally correct?',
    'This is not resolvable by data alone: the readings differ on what legitimacy means (P5 intent / universal procedure / sovereign will) and on which capacity constraint is binding (enforcement resources / veto authority / state autonomy). The readings coexist as live positions held by different institutional actors (P5 + liberal scholars vs. Global South states + critical scholars vs. sovereignty-first states). Empirical evidence (enforcement patterns, conditionality effects, institutional design history) can inform the debate but cannot resolve it.',
    'The three readings instantiate different constraint types: liberal reading → rope (authentic coordination, capacity-limited enforcement); sovereignty reading → snare (humanitarian framing as pure extraction); hegemonic reading (this one) → tangled rope (real coordination function + asymmetric extraction via selective enforcement and veto lock-in). The engine computes metrics for this reading; the three readings together form a constraint family linked by network.affects_constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_alternative, conceptual, 'This constraint is one reading of a contested kernel; the structural delta is resolved by which reading''s framing is adopted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(rbio_tr_t5, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(rbio_tr_t10, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(rbio_tr_t15, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(rbio_tr_t25, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 25, 0.56).
narrative_ontology:measurement(rbio_tr_t35, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 35, 0.58).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(rbio_be_t5, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 5, 0.66).
narrative_ontology:measurement(rbio_be_t10, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(rbio_be_t15, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 15, 0.74).
narrative_ontology:measurement(rbio_be_t25, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement(rbio_be_t35, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 35, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(rbio_su_t5, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(rbio_su_t10, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(rbio_su_t15, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(rbio_su_t25, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 25, 0.69).
narrative_ontology:measurement(rbio_su_t35, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 35, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__hegemonic_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.18).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% The RBIO practice norm complex decomposes into three structurally distinct readings of the same contested kernel. The hegemonic_extraction_reading (this story) interprets selective enforcement and veto lock-in as evidence of extractive intent, yielding high ε (0.79) and classification as tangled_rope. The liberal_institutional_reading interprets the same norms as universal and revisable, treating enforcement selectivity as a capacity problem, yielding lower ε and classification as rope. The sovereignty_maximalist_reading interprets RBIO norms as inherently delegitimating state consent, treating humanitarian exceptions as pretexts, yielding different beneficiary/victim structures and possible snare classification. The three readings are linked by network edges: hegemonic→liberal (influences: the extraction reading creates pressure on the liberal reading to account for systematic enforcement patterns); hegemonic→sovereignty (coexists_with: both challenge the P5 legitimacy claim, but from different premises). All three stories share the same kernel (the RBIO formal commitment) but instantiate it via different axioms and reference frames per their distinct committer positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__hegemonic_extraction_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
