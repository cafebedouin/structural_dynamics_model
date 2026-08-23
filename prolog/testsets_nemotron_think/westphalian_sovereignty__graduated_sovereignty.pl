% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty: State Capacity and Legitimacy as Intervention Threshold
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   The graduated sovereignty reading claims that state sovereignty is not a
 *   binary status but a spectrum calibrated by administrative capacity and
 *   governance legitimacy. States that fall below externally defined
 *   thresholds become legitimate targets for intervention — military,
 *   administrative, or financial. The framework originated in the R2P
 *   doctrine and World Bank governance indices but has expanded to justify
 *   open-ended tutelage of post-conflict states, conditionality regimes for
 *   aid-dependent states, and selective pressure on geopolitical rivals. The
 *   coordination story (calibrated response to state failure) is the cover;
 *   the operational reality is a classification system controlled by great
 *   powers and international institutions that extracts decision-making
 *   authority and resources from weak states. The constraint is a snare: high
 *   extraction, active enforcement (sanctions, conditionality, intervention),
 *   identifiable victims (weak and post-conflict states), and suppressed
 *   alternatives (sovereign equality, non-interference, regional solutions).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.65).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.72).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.65).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty: State Capacity and Legitimacy as Intervention Threshold").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, '1d597ffb-4be6-411a-ad0e-245de5a76581').
narrative_ontology:cs_kernel_codification('1d597ffb-4be6-411a-ad0e-245de5a76581', distributed).
narrative_ontology:cs_authority_grounding('1d597ffb-4be6-411a-ad0e-245de5a76581', extraction).
narrative_ontology:cs_interpretation_layer_present('1d597ffb-4be6-411a-ad0e-245de5a76581').
narrative_ontology:cs_reading_relation('1d597ffb-4be6-411a-ad0e-245de5a76581', westphalian_sovereignty__absolute_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('1d597ffb-4be6-411a-ad0e-245de5a76581', westphalian_sovereignty__conditional_sovereignty, influences).
narrative_ontology:cs_axiom('1d597ffb-4be6-411a-ad0e-245de5a76581', foundational, sovereignty_is_graduated_by_capacity_and_legitimacy).
narrative_ontology:cs_axiom_status(sovereignty_is_graduated_by_capacity_and_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('1d597ffb-4be6-411a-ad0e-245de5a76581', sovereignty_is_graduated_by_capacity_and_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('1d597ffb-4be6-411a-ad0e-245de5a76581', foundational, external_assessment_authority_is_legitimate).
narrative_ontology:cs_axiom_status(external_assessment_authority_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('1d597ffb-4be6-411a-ad0e-245de5a76581', external_assessment_authority_is_legitimate, conventional).
narrative_ontology:cs_axiom('1d597ffb-4be6-411a-ad0e-245de5a76581', secondary, intervention_calibration_tracks_state_performance).
narrative_ontology:cs_axiom_status(intervention_calibration_tracks_state_performance, holdable).
narrative_ontology:cs_axiom_grounding('1d597ffb-4be6-411a-ad0e-245de5a76581', intervention_calibration_tracks_state_performance, instrumental).
narrative_ontology:cs_reference_frame('1d597ffb-4be6-411a-ad0e-245de5a76581', post_westphalian_responsibility_framework).
narrative_ontology:cs_drift_state('1d597ffb-4be6-411a-ad0e-245de5a76581', contemporary_intervention_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1d597ffb-4be6-411a-ad0e-245de5a76581', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, great_power_interveners).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_institutions).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, regional_hegemonies).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, post_conflict_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, states_with_low_administrative_capacity).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, states_contested_by_external_powers).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__graduated_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__graduated_sovereignty, state_failure_as_threat_to_international_order).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__graduated_sovereignty, legitimacy_performance_linkage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and apply the capacity/legitimacy thresholds that trigger intervention. They author the assessment frameworks (World Bank governance indicators, Fragile States Index, R2P criteria) and control the military, financial, and diplomatic means to enforce reclassification. They gain strategic access, resource concessions, and geopolitical leverage from interventions justified by the graduated framework.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, great_power_interveners, agenda_setter,
    institutional, generational, arbitrage, global).

% UN agencies, World Bank, IMF, and regional bodies gain expanded mandates, funding streams, and operational authority when states are reclassified as lacking capacity or legitimacy. Their bureaucracies grow around assessment, monitoring, and reconstruction missions. They are not the primary extractors but capture institutional rents from the arrangement.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_institutions, beneficiary,
    organized, generational, mobile, global).

% Regional powers (e.g., Russia in post-Soviet space, US in Western Hemisphere, AU in Africa) use the graduated sovereignty language to legitimize interventions in their near abroad. They adopt the global framework's vocabulary while applying it selectively to neighbors. They gain sphere-of-influence recognition and reduced external scrutiny of their own interventions.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, regional_hegemonies, beneficiary,
    powerful, biographical, constrained, regional).

% States with low administrative capacity, weak revenue bases, or contested territorial control face continuous assessment against externally defined thresholds. They lose control over natural resources, border security, and domestic policy when reclassified. They cannot exit the assessment regime — the indices are produced by external bodies and applied regardless of consent. Resistance is met with sanctions, aid conditionality, or military intervention.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, weak_states, payer,
    powerless, biographical, trapped, national).

% States emerging from civil war or regime collapse are automatically placed in the lowest sovereignty tier. Their reconstruction is managed by international administrations that control budgets, rewrite laws, and vet political candidates. The graduated framework makes this tutelage indefinite — 'capacity building' benchmarks shift as progress is made. They bear the costs of foreign administration while having no voice in the criteria.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, post_conflict_states, payer,
    powerless, biographical, trapped, national).

% Functioning but poor states that score low on governance indicators face perpetual conditionality: aid, debt relief, and market access tied to 'capacity building' programs designed by external agencies. They can sometimes negotiate terms or play donors against each other, but the framework's logic — that low scores justify external direction — is non-negotiable. Their sovereignty is de facto leased.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, states_with_low_administrative_capacity, payer,
    moderate, biographical, constrained, national).

% States targeted by great powers for strategic reasons find their legitimacy assessed through the graduated lens. Election monitoring, human rights reports, and corruption indices become instruments of pressure. They have some diplomatic agency (UN voting, non-aligned movement) but the assessment infrastructure is controlled by their adversaries. Exit means accepting subordinate alliance status.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, states_contested_by_external_powers, payer,
    moderate, biographical, constrained, national).

% Civilians in states subjected to graduated-sovereignty interventions experience both protection (from atrocities) and displacement (by intervention). They have no standing in the classification process, no vote on intervention thresholds, and no remedy when interventions cause harm. The framework claims to act in their name while structurally excluding their agency.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, affected_populations_in_target_states, excluded,
    powerless, immediate, trapped, local).

% Scholars of international law, IR theorists, and normative philosophers who evaluate the framework's coherence, track its empirical outcomes, and debate its legitimacy. They do not collect rents or bear extraction but shape the intellectual environment in which the constraint operates.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement claims to coordinate the international community's response to state failure and mass atrocity by providing a graduated scale: the less capacity and legitimacy a state demonstrates, the more legitimate external intervention becomes. It purports to solve the binary dilemma of 'intervene everywhere' vs. 'intervene nowhere' by calibrating response to assessed need.
% TRANSFER_FUNCTION: Moves decision-making authority over domestic affairs (security, resource allocation, political process, legal reform) from target states to external interveners and international institutions. Moves financial resources from target state treasuries and natural resource revenues to international administration budgets and contractor networks. Moves legitimacy capital from sovereign equality norms to performance-based legitimacy norms.
% ABSENT_VOICES: The populations of target states — who experience both the harms of state failure and the harms of intervention — have no standing in the bodies that define capacity thresholds, design assessment indices, or authorize interventions. Civil society organizations in the Global South that critique the framework are consulted performatively but hold no veto. The 'beneficiaries' of protection are structurally excluded from the coordination mechanism claimed to serve them.
% DISAPPEARANCE_RATIONALE: If the graduated sovereignty framework vanished overnight, the legal and normative architecture justifying conditional interventions (R2P mandates, World Bank conditionality, IMF structural benchmarks, EU accession criteria, AU intervention mandates) would lose its unifying logic. Interventions would revert to ad hoc great-power politics or require new UNSC authorization for each case. The assessment industry (Fragile States Index, Worldwide Governance Indicators, CPIA ratings) would lose its policy anchor. Target states would regain formal sovereign equality but lose the 'protection' the framework promises.
% FOUNDING_PROBLEM: The international community lacked a coherent framework for responding to state collapse and mass atrocity after the Cold War. The binary sovereignty/non-sovereignty distinction failed in Somalia, Rwanda, Bosnia — where states existed formally but exercised no authority, or committed atrocities behind sovereign shields. Graduated sovereignty was built to calibrate international response to the degree of state failure.
% FOUNDING_PROBLEM_CORROBORATION: The International Commission on Intervention and State Sovereignty (ICISS 2001) and the 2005 World Summit Outcome Document attest the problem was real and the R2P framework was the response. Critics from the Global South (Non-Aligned Movement statements, African Union's Ezulwini Consensus, TWAIL scholarship) attest the framework has been weaponized for regime change and resource extraction. Independent evaluations (OECD DAC fragility assessments, UN Peacebuilding Commission reviews) document that 'capacity building' correlates with prolonged foreign administration, not sovereign recovery.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__graduated_sovereignty, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.65) because the framework transfers substantial authority and resources from target states to interveners on a continuous basis, not episodically. Suppression is high (0.72) because the assessment infrastructure (indices, monitoring missions, conditionality) is actively maintained and expanded, and target states cannot opt out of being scored. Theater ratio is moderate-high (0.48) because genuine capacity-building occurs but an increasing share of activity serves to justify continued intervention rather than achieve exit. Accessibility collapse is moderate (0.55) because alternative frameworks (sovereign equality, regional security architectures) exist but are marginalized in practice. Resistance is significant (0.68) from target states, Global South coalitions, and critical scholars, but has not altered the framework's core operation.
 *
 * PERSPECTIVAL GAP:
 *   From the intervener seat, the framework appears as necessary coordination — a rational response to the problem of state failure. From the target state seats, it appears as neo-colonial extraction — the same metrics that justify intervention also ensure its perpetuity. From the excluded population seat, it appears as double victimization — failed by their state, then subjected to unaccountable foreign administration. The engine computes this divergence from the structural data; the authored claim (snare) reflects the target-state and excluded-population reading as structurally dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   Great power interveners and international institutions are structural beneficiaries (d near 0.0): they control the classification criteria, collect institutional rents, and face no risk of being classified. Regional hegemonies are secondary beneficiaries (d ~ 0.2): they gain legitimized spheres of influence. Weak states, post-conflict states, and low-capacity states are full targets (d near 1.0): they bear the full extraction, have trapped exit, and face identity-lock through the 'failing state' label. Contested states are constrained targets (d ~ 0.7): they have some diplomatic agency but the assessment infrastructure is adversary-controlled. Affected populations are excluded (no d — they are not agents in the classification game). Analytical observers are analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (calibrating response to state failure) remains live in the sense that state collapse and atrocity persist. But the graduated framework has become a self-justifying machine: the more it intervenes, the more 'fragility' it detects, the more intervention it warrants. The mandate has atrophied into a permanent emergency architecture. The classification prevents mislabeling this as coordination because the coordination function (temporary stabilization) is structurally subordinate to the extraction function (permanent authority transfer) — evidenced by rising theater ratio, zero graduation cases, and the framework's expansion beyond its original atrocity-prevention scope to routine governance conditionality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the graduated sovereignty reading a distinct constraint from conditional_sovereignty, or a rhetorical extension of the same R2P framework?',
    'Trace the institutional genealogy: if the assessment indices (CPIA, WGI, FSI) and intervention mandates (peacebuilding commissions, transition administrations) were built for graduated sovereignty specifically, not just R2P, the readings are structurally distinct. Compare the intervention trigger lists: atrocity-only vs. governance-threshold.',
    'If distinct, graduated_sovereignty carries its own ε (high) and victim structure (weak states). If not, the extraction attributed here belongs to conditional_sovereignty, and this story should be merged or the sibling relation changed to influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether graduated sovereignty is a structurally independent reading or a rhetorical stretch of conditional sovereignty.').

omega_variable(
    coordination_extraction_boundary,
    'Is there a genuine coordination function (preventing atrocities, filling governance vacuums) that is structurally separable from the extraction function (authority transfer, resource capture)?',
    'Examine cases where intervention occurred without great-power strategic interest (e.g., Sierra Leone 2000, Timor-Leste 1999, Liberia 2003). If outcomes show sustainable sovereign recovery without permanent external authority, coordination is separable. If all cases show indefinite tutelage, the coordination story is cover.',
    'If separable, the constraint is a tangled rope (coordination + extraction). If inseparable, it is a pure snare. Current evidence (zero graduated-to-full-sovereignty transitions, rising theater ratio) points to snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable or the coordination is cover.').

omega_variable(
    suppression_mechanism_internalized,
    'Do target states internalize the graduated sovereignty framework such that they self-police to avoid reclassification, making suppression partially internalized rather than purely structural?',
    'Track policy adoption in aid-dependent states: do they adopt governance reforms voluntarily to improve index scores, or only under conditionality? Measure the correlation between index improvement and genuine institutional capacity vs. performative compliance. Post-exit trajectories: do states that ''graduate'' maintain reforms without external pressure?',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint persists even if external enforcement relaxes. This would increase the snare classification confidence and explain the zero-graduation pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression operates through internalized compliance in addition to external enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t2001, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(west_tr_t2005, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(west_tr_t2011, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2011, 0.45).
narrative_ontology:measurement(west_tr_t2015, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2015, 0.48).
narrative_ontology:measurement(west_tr_t2020, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2020, 0.49).
narrative_ontology:measurement(west_tr_t2024, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(west_be_t2001, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2001, 0.35).
narrative_ontology:measurement(west_be_t2005, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(west_be_t2011, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2011, 0.58).
narrative_ontology:measurement(west_be_t2015, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(west_be_t2020, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(west_be_t2024, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t2001, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2001, 0.45).
narrative_ontology:measurement(west_su_t2005, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(west_su_t2011, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2011, 0.68).
narrative_ontology:measurement(west_su_t2015, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(west_su_t2020, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(west_su_t2024, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__conditional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint, absolute_sovereignty, and conditional_sovereignty form the westphalian_sovereignty kernel family. They share the kernel (state authority over territory) but instantiate different constraints with different ε, beneficiaries, and victims. Graduated sovereignty has the highest ε (0.65) and the most extractive victim structure. Absolute sovereignty has near-zero ε (mountain-like). Conditional sovereignty sits between (tangled rope: genuine atrocity-prevention coordination + selective extraction). The ε-invariance principle requires separate stories because the 'sovereignty' label covers structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
