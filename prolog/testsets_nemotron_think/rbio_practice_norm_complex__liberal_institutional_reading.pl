% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: Rules-Based International Order — Liberal Institutional Reading
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the liberal_institutional_reading of
 *   the RBIO practice norm complex: the claim that rules-based international
 *   order norms are universal, consent-based, and revisable through
 *   legitimate multilateral processes, with enforcement selectivity a
 *   capacity problem rather than a legitimacy defect. The reading presents
 *   RBIO as a genuine coordination mechanism (collective security, human
 *   rights, economic governance) but the structural delta reveals
 *   beneficiaries (intervening states, contractors, IO staff) and victims
 *   (targeted states, sanctioned civilians). The claimed_type is tangled_rope
 *   — the author's structural judgment that genuine coordination coexists
 *   with asymmetric extraction — while the reading itself would claim rope.
 *   Metrics reflect the descriptive reality: moderate-high extraction (0.58)
 *   from sanctions and conditionality, high suppression (0.72) from UNSC veto
 *   gatekeeping and enforcement machinery, rising theater (0.38) as
 *   humanitarian rhetoric increasingly decorates geopolitical intervention.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.58).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.72).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "Rules-Based International Order — Liberal Institutional Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, '3e109e7b-0a5b-4e8e-9b53-5931e98b64ba').
narrative_ontology:cs_kernel_codification('3e109e7b-0a5b-4e8e-9b53-5931e98b64ba', formalized).
narrative_ontology:cs_authority_grounding('3e109e7b-0a5b-4e8e-9b53-5931e98b64ba', lineage).
narrative_ontology:cs_interpretation_layer_present('3e109e7b-0a5b-4e8e-9b53-5931e98b64ba').
narrative_ontology:cs_reading_relation('3e109e7b-0a5b-4e8e-9b53-5931e98b64ba', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e109e7b-0a5b-4e8e-9b53-5931e98b64ba', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('3e109e7b-0a5b-4e8e-9b53-5931e98b64ba', foundational, universal_norms_legitimate_multilateral_revision).
narrative_ontology:cs_axiom_status(universal_norms_legitimate_multilateral_revision, holdable).
narrative_ontology:cs_axiom_grounding('3e109e7b-0a5b-4e8e-9b53-5931e98b64ba', universal_norms_legitimate_multilateral_revision, conventional).
narrative_ontology:cs_axiom('3e109e7b-0a5b-4e8e-9b53-5931e98b64ba', foundational, enforcement_selectivity_capacity_not_legitimacy).
narrative_ontology:cs_axiom_status(enforcement_selectivity_capacity_not_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3e109e7b-0a5b-4e8e-9b53-5931e98b64ba', enforcement_selectivity_capacity_not_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('3e109e7b-0a5b-4e8e-9b53-5931e98b64ba', un_charter_collective_security).
narrative_ontology:cs_drift_state('3e109e7b-0a5b-4e8e-9b53-5931e98b64ba', post_cold_war_unipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3e109e7b-0a5b-4e8e-9b53-5931e98b64ba', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, security_contractors).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, international_orgs_staff).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__liberal_institutional_reading, universal_human_rights).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__liberal_institutional_reading, collective_security_doctrine).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__liberal_institutional_reading, multilateralism_norm).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__liberal_institutional_reading, sovereignty_as_responsibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% P5 and allied states that authorize or lead interventions, set sanctions regimes, and control international financial architecture. They frame actions as upholding universal norms while gaining strategic access, contractor revenue, and geopolitical leverage. Exit from the constraint is trivial — they write the rules.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, beneficiary).

% Private military firms, reconstruction contractors, and compliance consultancies that capture revenue streams from intervention and sanctions enforcement. Their business model depends on the RBIO enforcement machinery remaining active and expanding.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, security_contractors, beneficiary,
    organized, biographical, mobile, global).

% UN Secretariat, ICC, World Bank, IMF, WTO staff and associated epistemic communities. Their careers, budgets, and institutional mandates expand with each new enforcement mechanism. They authenticate the constraint's legitimacy while materially benefiting from its growth.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_orgs_staff, beneficiary,
    organized, biographical, mobile, global).

% States subjected to sanctions, intervention, or conditionality. They bear sovereignty costs, economic isolation, and regime destabilization. Exit options are limited — leaving the dollar system or UN framework is technically possible but politically and economically prohibitive. They contest the constraint's legitimacy but cannot avoid its application.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states, payer,
    moderate, generational, constrained, national).

% Populations in sanctioned or intervened states who suffer humanitarian degradation (healthcare collapse, malnutrition, displacement) regardless of their government's conduct. No individual exit from the constraint; collective exit requires regime change — which the constraint often aims to produce.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions, payer,
    powerless, biographical, trapped, local).

% Global South states that neither lead interventions nor are primary targets but are bound by conditionality and norms they did not equally author. They argue for reform in UNGA and NAM but lack veto power or enforcement capacity. Their dissent is recorded but not decisive.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, non_aligned_states, excluded,
    moderate, generational, constrained, regional).

% Human rights NGOs, humanitarian organizations, academic experts, and legal advocates. They monitor compliance, document violations, and legitimize the constraint through advocacy. Some are funded by intervening states; others maintain independence. Their analytical seat sees the full structure but their influence is agenda-setting, not determinative.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_civil_society, observer,
    organized, biographical, mobile, global).

% The formal authorization body whose P5 veto structure makes enforcement selective by design. It administers the constraint's legitimacy while its permanent members are the primary intervening states. The Council's deadlock is read by this reading as capacity failure, not structural illegitimacy.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, un_security_council, agenda_setter,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, consent-based framework for collective security, human rights protection, and economic governance that replaces great-power war with legalized dispute settlement and authorized enforcement.
% TRANSFER_FUNCTION: Moves sovereignty costs, economic resources, and humanitarian burdens from targeted states and their civilian populations to intervening states and their contractors through sanctions, conditionality, and military intervention authorized by multilateral processes.
% ABSENT_VOICES: Populations in targeted states who experience sanctions and intervention as collective punishment; Global South states that experience conditionality as neo-colonial; future generations who inherit the precedent of selective enforcement. They are not in the room when UNSC authorizes or IMF conditions.
% DISAPPEARANCE_RATIONALE: If RBIO norms and their enforcement machinery vanished overnight, the legal framework for collective security, human rights accountability, and multilateral trade would collapse. Great-power competition would revert to unmediated coercion; no legitimate basis for intervention or sanctions would remain; the UN system would lose its operational core.
% FOUNDING_PROBLEM: The post-1945 need to prevent great-power war, establish universal human rights, and create a legalized alternative to unilateral coercion — grounded in the UN Charter, UDHR, and Bretton Woods institutions.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the UN Charter preamble, UDHR drafting history, and contemporary international lawyers (e.g., Kranzbach, Koskenniemi) and historians (e.g., Mazower, Moyn) who document both the genuine coordination achievement and the persistent gap between universal aspiration and selective application — sources outside the intervening-state beneficiary set.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that sanctions regimes and conditionality transfer real resources from targeted populations to intervening-state economies and contractor networks, but the coordination function (nuclear non-proliferation, trade dispute settlement, some human rights gains) is non-zero and valued by multiple parties. Suppression (0.72) is high because the constraint's persistence depends on active enforcement: UNSC authorization gates, SWIFT exclusion, ICC jurisdiction, military intervention — alternatives are not merely discouraged but structurally blocked. Theater ratio (0.38) captures the growing gap between humanitarian/legal rhetoric and geopolitical practice, especially post-1990. Accessibility collapse (0.55) is moderate: regional alternatives (AU, ASEAN, CELAC) exist but lack enforcement parity. Resistance (0.65) is substantial: targeted states build parallel financial infrastructure, non-aligned states contest in UNGA, civil society documents double standards.
 *
 * PERSPECTIVAL GAP:
 *   The intervening_states/agenda_setter seat experiences the constraint as coordination it built and maintains (low effective extraction, high legitimacy). The targeted_states/civilian_populations payer seats experience it as enforced extraction with no exit (high effective extraction, low legitimacy). The international_civil_society observer seat sees both coordination gains and extraction costs, producing a contested assessment. The engine computes this divergence from the structural data — the authored claim (tangled_rope) reflects the author's judgment that the constraint is structurally hybrid, not that any single seat recognizes it as such.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states and their contractors are structural beneficiaries: they set the agenda, collect revenue, and face trivial exit (d near 0.0). Targeted states are payers with constrained exit — they can resist but cannot avoid the constraint's application (d near 0.8). Civilian populations under sanctions are trapped payers with zero exit (d near 1.0). International org staff are beneficiaries with mobile exit (d low). Non-aligned states are excluded from agenda-setting but bound by outcomes (d moderate-high). UNSC as an institution is both agenda_setter and the enforcement gate — its P5 members are intervening states, so its directionality mirrors theirs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (preventing great-power war, universalizing human rights) remains partially live — nuclear war has been avoided, some norms have universalized — but the enforcement machinery has expanded beyond the founding mandate into regime change, expansive conditionality, and contractor economies. The reading's insistence that selectivity is a capacity problem (not legitimacy) performs mandatrophy resolution: it reinterprets the gap between universal claim and selective practice as implementation failure rather than structural feature, preserving the constraint's legitimacy while its extraction profile grows. This is a classic mandatrophy pattern — the mandate has outlived its original function but the constraint persists by redefining the problem it solves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selectivity_capacity_vs_legitimacy,
    'Is enforcement selectivity (P5 veto shielding allies, targeting adversaries) a genuine capacity limitation of multilateral institutions, or does it reveal that the constraint''s legitimacy depends on the extraction it enables?',
    'Counterfactual analysis: if P5 veto were reformed and enforcement became universal, would intervening states sustain the same enforcement intensity against their own allies? Historical test: compare enforcement against non-P5 vs. P5-aligned violators of same norms.',
    'If selectivity is structural (legitimacy depends on extraction), the constraint is a snare or tangled_rope with high extraction. If selectivity is contingent capacity failure, the constraint could evolve toward rope with lower extraction under reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selectivity_capacity_vs_legitimacy, conceptual, 'Core dispute between liberal institutional and hegemonic extraction readings — whether the gap between universal claim and selective practice is bug or feature.').

omega_variable(
    humanitarian_intervention_regime_change_boundary,
    'Is the humanitarian intervention doctrine (R2P, UNSC 1973-style authorization) structurally separable from regime-change practice, or does the doctrine function as a permission structure for extraction?',
    'Case comparison: interventions authorized for civilian protection that did vs. did not escalate to regime change (Libya 2011 vs. Kosovo 1999 vs. Syria non-intervention). Measure post-intervention political-economic capture by intervening-state contractors.',
    'If inseparable, the coordination function (civilian protection) is a cover for extraction (regime change + contractor capture) — higher extractiveness. If separable, part of measured extraction is the genuine cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_intervention_regime_change_boundary, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable at the enforcement level.').

omega_variable(
    p5_veto_reform_feasibility,
    'Is the UN Charter''s P5 veto structure practically amendable (per this reading''s revisability claim), or does institutional path-dependency make revision impossible (per hegemonic reading)?',
    'Historical analysis of Charter amendment attempts (1963, 1965, 1990s reform proposals) and current UNGA/Intergovernmental Negotiations process. Assess whether any P5 member has ever accepted veto dilution on enforcement matters.',
    'If revision is practically impossible, the reading''s claim of ''legitimate multilateral revisability'' is a legitimating fiction — the constraint is more extractive than claimed. If revision is feasible, the reading''s capacity-problem framing gains credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(p5_veto_reform_feasibility, empirical, 'Whether the constraint''s self-proclaimed revisability is operational or ceremonial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 0, 79).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_liberal_tr_t0, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(rbio_liberal_tr_t15, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(rbio_liberal_tr_t30, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(rbio_liberal_tr_t45, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 45, 0.33).
narrative_ontology:measurement(rbio_liberal_tr_t60, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement(rbio_liberal_tr_t79, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 79, 0.38).

% Extraction over time
narrative_ontology:measurement(rbio_liberal_be_t0, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(rbio_liberal_be_t15, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(rbio_liberal_be_t30, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(rbio_liberal_be_t45, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 45, 0.55).
narrative_ontology:measurement(rbio_liberal_be_t60, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(rbio_liberal_be_t79, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 79, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(rbio_liberal_su_t0, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(rbio_liberal_su_t15, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(rbio_liberal_su_t30, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(rbio_liberal_su_t45, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 45, 0.68).
narrative_ontology:measurement(rbio_liberal_su_t60, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(rbio_liberal_su_t79, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 79, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__liberal_institutional_reading, 0.1).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the RBIO practice norm complex into three readings with distinct ε values and beneficiary/victim structures. The liberal institutional reading (this story) claims universal revisability and capacity-limited selectivity (ε=0.58). The hegemonic extraction reading claims frozen hegemony and structural selectivity (ε≈0.75). The sovereignty maximalist reading claims sovereignty protection as sole legitimacy (ε≈0.3 for its beneficiaries, but high suppression for interveners). They are linked because the upstream liberal institutional claim is often cited as evidence for the downstream hegemonic extraction critique, and the sovereignty reading reacts to both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__liberal_institutional_reading, institutional, 0.1).
constraint_indexing:directionality_override(rbio_practice_norm_complex__liberal_institutional_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
