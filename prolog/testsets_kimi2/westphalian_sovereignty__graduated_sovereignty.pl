% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty (Capacity-Spectrum Reading)
 *   domain: international law / political philosophy / global governance
 *
 * SUMMARY:
 *   The Westphalian sovereignty kernel has multiple structurally distinct
 *   readings. This constraint instantiates the graduated_sovereignty reading:
 *   the claim that sovereignty is not a binary equal status but a spectrum
 *   determined by state capacity and governance legitimacy. Under this
 *   reading, external actors gain formal discretion to classify states as
 *   failed, fragile, rogue, or transitioning â which determines access to
 *   recognition, aid, intervention, and institutional voice. Weak states and
 *   targeted populations bear the costs of this classification, losing
 *   autonomy and facing intervention. The reading is structurally distinct
 *   from absolute_sovereignty (which it forecloses) and
 *   conditional_sovereignty (with which it coexists).
 *
 * KEY AGENTS:
 *   - Great powers (agenda_setter / institutional / global): Define classification criteria and reserve intervention discretion.
 *   - International financial institutions (agenda_setter / institutional / global): Convert capacity ratings into conditional lending and policy leverage.
 *   - Weak states (payer / powerless / national): Subjected to classification and denied full autonomy.
 *   - Targeted populations (payer / powerless / national): Bear intervention costs and are excluded from classification processes.
 *   - Human rights advocacy networks (beneficiary / organized / global): Benefit from expanded external scrutiny mandate.
 *   - Global South diplomatic coalitions (observer / organized / global): Resist the framework as neo-colonial but lack veto power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.62).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.72).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.62).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty (Capacity-Spectrum Reading)").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international law / political philosophy / global governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, '70467f6e-80a9-41eb-9ca0-0f38c590d19e').
narrative_ontology:cs_kernel_codification('70467f6e-80a9-41eb-9ca0-0f38c590d19e', formalized).
narrative_ontology:cs_authority_grounding('70467f6e-80a9-41eb-9ca0-0f38c590d19e', extraction).
narrative_ontology:cs_interpretation_layer_present('70467f6e-80a9-41eb-9ca0-0f38c590d19e').
narrative_ontology:cs_reading_relation('70467f6e-80a9-41eb-9ca0-0f38c590d19e', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('70467f6e-80a9-41eb-9ca0-0f38c590d19e', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('70467f6e-80a9-41eb-9ca0-0f38c590d19e', foundational, sovereignty_scales_with_capacity).
narrative_ontology:cs_axiom_status(sovereignty_scales_with_capacity, holdable).
narrative_ontology:cs_axiom_grounding('70467f6e-80a9-41eb-9ca0-0f38c590d19e', sovereignty_scales_with_capacity, empirically_contingent).
narrative_ontology:cs_axiom('70467f6e-80a9-41eb-9ca0-0f38c590d19e', foundational, external_capacity_assessment_legitimate).
narrative_ontology:cs_axiom_status(external_capacity_assessment_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('70467f6e-80a9-41eb-9ca0-0f38c590d19e', external_capacity_assessment_legitimate, conventional).
narrative_ontology:cs_reference_frame('70467f6e-80a9-41eb-9ca0-0f38c590d19e', differential_sovereign_capacity).
narrative_ontology:cs_drift_state('70467f6e-80a9-41eb-9ca0-0f38c590d19e', contemporary_multipolar_resistance, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('70467f6e-80a9-41eb-9ca0-0f38c590d19e', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, great_powers).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, human_rights_advocacy_networks).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, targeted_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and administer the classification criteria that place states on the sovereignty spectrum. Reserve unilateral and multilateral discretion to label states as failed, fragile, rogue, or transitioning, and use these labels to authorize intervention, condition recognition, or withhold institutional access. Are structurally immune to reciprocal classification because the frameworks are designed by and for their own interests.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, great_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% Rate states on governance, capacity, and fragility indices; condition lending and debt relief on compliance with externally defined benchmarks. Convert sovereignty downgrades into policy leverage, effectively transferring decision rights over domestic resource allocation from recipient governments to institutional staff in headquarters.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions, beneficiary).

% Subjected to recurring external assessments of their capacity and legitimacy. When classified low on the sovereignty spectrum, they lose procedural equality in international forums, face suspended recognition or conditional engagement, and are denied the full autonomy that stronger states enjoy. Cannot opt out of the classification regime without exiting the international order itself.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, weak_states, payer,
    powerless, generational, trapped, national).

% Live under governments that have been classified as illegitimate or incapable by external assessors. Bear the direct costs of intervention, state-building mandates, and structural adjustment, while excluded from the international processes that classify their polities and determine their governance.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, targeted_populations, payer,
    powerless, biographical, trapped, national).

% Leverage the graduated sovereignty framework to open doctrinal and institutional space for external scrutiny of domestic rights practices. Benefit from the normative shift that treats sovereignty as contingent on performance, which expands their mandate and access.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, human_rights_advocacy_networks, beneficiary,
    organized, biographical, mobile, global).

% Organize diplomatic resistance to graduated sovereignty frameworks, framing them as neo-colonial devices that reproduce imperial hierarchy under technocratic cover. Demand sovereign equality but lack institutional veto over the classification regimes designed by great powers and embedded in international institutions.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, global_south_diplomatic_coalitions, observer,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__graduated_sovereignty, great_powers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Calibrates international engagement to observed state capacity, allegedly preventing both dangerous neglect of collapsed states and inappropriate interference in functional ones by matching intervention intensity to local conditions.
% TRANSFER_FUNCTION: Moves sovereignty-defining authority from domestic institutions and populations to external assessors â great powers, international financial institutions, and selected expert bodies â transferring discretion over recognition, intervention, and resource allocation.
% ABSENT_VOICES: Post-colonial and anti-imperial scholars are marginalized in the institutional design of classification regimes; absolute-sovereignty advocates have been procedurally excluded from post-1990 international legal discourse; weak-state populations are formally consulted but structurally overruled by external assessment mechanisms.
% DISAPPEARANCE_RATIONALE: If the spectrum framework vanished, weak states would reclaim uncontested territorial authority, international financial institutions would lose governance-conditionality leverage, humanitarian intervention would require alternative legal frames, and the architecture of liberal international order would reorganize around either absolute or conditional sovereignty readings.
% FOUNDING_PROBLEM: Post-Cold War collapse of bipolar stability produced failed states, humanitarian crises, and transnational security threats that the absolute-sovereignty framework appeared unable to address; the graduated framework was built to manage disorder and capacity gaps without recourse to formal colonialism.
% FOUNDING_PROBLEM_CORROBORATION: Great powers and international financial institutions attest the problem is still live, citing ongoing fragility and conflict. Weak states, the Non-Aligned Movement successor coalitions, and critical international legal scholars attest the problem was manufactured to preserve hierarchical intervention rights; their testimony from outside the beneficiary set supports the shifted-function reading.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__graduated_sovereignty, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) reflects the substantial transfer of sovereignty-defining authority from domestic actors to external assessors. Suppression (0.72) is high because the constraint persists through institutionalized classification regimes, conditional financing, and the exclusion of absolute-sovereignty alternatives from post-1990 discourse. Theater ratio (0.48) is moderate-high: capacity assessments and state-building metrics perform genuine information-gathering but increasingly serve to legitimize predetermined hierarchies. Accessibility collapse (0.70) is high because once the spectrum framework is accepted, absolute sovereign equality becomes practically unthinkable in mainstream international legal discourse. Resistance (0.58) reflects persistent Global South diplomatic opposition and occasional normative backlash. The measurement series share one time grid (0â30) to prevent temporal misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   From the great-power and IFI seats, graduated sovereignty is necessary global governance â a response to real capacity gaps that absolute sovereignty cannot handle. From the weak-state and targeted-population seats, the same structure is a reclassification device that strips procedural equality and authorizes intervention. The engine computes this divergence from the structural data; the authored claim (snare) does not adjudicate the seat-level experience but reflects the structural assessment that extraction and victimization are the dominant functions.
 *
 * DIRECTIONALITY LOGIC:
 *   Great powers and international financial institutions are structural beneficiaries: the constraint subsidizes their discretion and leverage, yielding low directionality. Human rights advocacy networks are secondary beneficiaries (low-to-moderate directionality) who collect normative authority from the framework without administering it. Weak states and targeted populations are the targets: they pay through lost autonomy, intervention, and conditionality, with minimal exit options yielding high directionality. The engine will compute high effective extraction for weak states and targeted populations, low or negative effective extraction for the intervening coalition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring explicit victim identification and high suppression. A pure coordination story (state-building as scaffold or rope) would lack concentrated victims and would not require active enforcement against states seeking equal standing. Here, weak states resist classification, great powers enforce it, and the theater ratio indicates that a growing share of activity is performative maintenance of hierarchy rather than genuine capacity development. The founding problem (post-Cold War disorder) is contested and likely dead as a live justification, while the arrangement persists â a mandatrophy signal that the engine can cross-check against the computed piton/theater path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the graduated_sovereignty reading of the westphalian_sovereignty kernel. Does the authored structural data capture the distinct epsilon and stakeholder geometry of this reading, or does it conflate siblings?',
    'Comparison against independently authored absolute_sovereignty and conditional_sovereignty constraints in the same family; divergence in epsilon, victim sets, and directionality profiles confirms successful decomposition.',
    'If conflated, the corpus loses the cross-reading falsification data the kernel design is meant to produce; if clean, the family enables observer-relative classification analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Commitment frame verification for kernel reading isolation').

omega_variable(
    capacity_assessment_instrumentality,
    'Are state-capacity and governance-legitimacy assessments genuinely empirical measurements, or are they selectively deployed to authorize intervention in geopolitically weak targets while excusing comparable failures in powerful states?',
    'Cross-national longitudinal audit of classification outcomes: compare the rate of downgrade for weak states versus great powers across matched capacity/legitimacy indicators.',
    'If assessments are systematically biased by power, the constraint''s empirically_contingent axiom collapses and the extraction is revealed as politically instrumental, strengthening the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_assessment_instrumentality, empirical, 'Whether capacity metrics are neutral instruments or power-laden devices').

omega_variable(
    intervention_benefit_ambiguity,
    'Do populations in classified weak states experience graduated sovereignty as protective coordination (humanitarian delivery, stability provision) or as extractive victimization (loss of self-determination, external resource extraction)?',
    'Post-intervention welfare and autonomy outcome studies disaggregated by population segment, compared against counterfactual non-intervention trajectories.',
    'If populations are net beneficiaries, the victim set shrinks and the constraint edges toward tangled_rope; if net victims, the snare classification hardens and targeted_populations directionality moves toward full target.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_benefit_ambiguity, empirical, 'Ambiguity about whether intervention under graduated sovereignty helps or harms subjected populations').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the compliance of weak states with graduated sovereignty frameworks driven by structural coercion (dependence on aid, recognition, and market access) or by internalized acceptance of their own lesser sovereignty?',
    'Observe diplomatic behavior of weak states in contexts where structural dependence is attenuated (e.g., multipolar alignment options, commodity booms); if resistance rises when exit options improve, suppression is structural.',
    'If internalized, effective suppression exceeds the structural measure and weak-state directionality sits closer to full target than the institutional data suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in weak-state compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(westphalian_graduated_tr_t0, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0, 0.2).
narrative_ontology:measurement(westphalian_graduated_tr_t5, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 5, 0.26).
narrative_ontology:measurement(westphalian_graduated_tr_t10, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 10, 0.32).
narrative_ontology:measurement(westphalian_graduated_tr_t15, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 15, 0.38).
narrative_ontology:measurement(westphalian_graduated_tr_t20, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 20, 0.43).
narrative_ontology:measurement(westphalian_graduated_tr_t25, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 25, 0.46).
narrative_ontology:measurement(westphalian_graduated_tr_t30, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(westphalian_graduated_be_t0, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(westphalian_graduated_be_t5, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(westphalian_graduated_be_t10, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(westphalian_graduated_be_t15, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(westphalian_graduated_be_t20, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(westphalian_graduated_be_t25, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(westphalian_graduated_be_t30, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(westphalian_graduated_su_t0, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(westphalian_graduated_su_t5, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(westphalian_graduated_su_t10, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(westphalian_graduated_su_t15, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(westphalian_graduated_su_t20, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(westphalian_graduated_su_t25, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(westphalian_graduated_su_t30, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, conditional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the westphalian_sovereignty kernel, which decomposes into three structurally distinct claims per the epsilon-invariance principle: absolute_sovereignty (sovereignty as unconditional equal authority, negligible extraction), conditional_sovereignty (sovereignty as responsibility, moderate extraction), and graduated_sovereignty (sovereignty as capacity spectrum, high extraction). The epsilon values, victim sets, and stakeholder geometries differ across the family. Network links prevent orphan stories and enable contamination propagation analysis across the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
