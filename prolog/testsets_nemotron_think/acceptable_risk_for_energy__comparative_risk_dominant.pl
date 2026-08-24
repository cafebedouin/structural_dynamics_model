% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__comparative_risk_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Comparative Risk Framing for Nuclear Acceptability
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'comparative_risk_dominant'
 *   reading of the 'acceptable_risk_for_energy' kernel. It holds that nuclear
 *   energy's risk is legitimated ONLY by comparison to fossil fuel
 *   alternatives — there is no absolute safety threshold nuclear must meet,
 *   only a relative one: it must be less harmful than the coal/gas it
 *   displaces. This reading gained dominance after Chernobyl (1986) as
 *   climate urgency grew, and now structures IPCC pathways, IAEA safety
 *   standards, and national licensing. The constraint extracts from
 *   nuclear-adjacent communities, uranium miners, and future generations to
 *   benefit climate-vulnerable populations and energy consumers. It is a
 *   Tangled Rope: genuine coordination function (climate mitigation) fused
 *   with asymmetric extraction (localized sacrifice for global benefit).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.58).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.52).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative Risk Framing for Nuclear Acceptability").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, '3f5c28ab-ea59-484d-976f-ef403c436a7d').
narrative_ontology:cs_kernel_codification('3f5c28ab-ea59-484d-976f-ef403c436a7d', distributed).
narrative_ontology:cs_authority_grounding('3f5c28ab-ea59-484d-976f-ef403c436a7d', expertise).
narrative_ontology:cs_interpretation_layer_present('3f5c28ab-ea59-484d-976f-ef403c436a7d').
narrative_ontology:cs_reading_relation('3f5c28ab-ea59-484d-976f-ef403c436a7d', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('3f5c28ab-ea59-484d-976f-ef403c436a7d', acceptable_risk_for_energy__expected_value_dominant, influences).
narrative_ontology:cs_axiom('3f5c28ab-ea59-484d-976f-ef403c436a7d', foundational, climate_urgency_overrides_absolute_thresholds).
narrative_ontology:cs_axiom_status(climate_urgency_overrides_absolute_thresholds, holdable).
narrative_ontology:cs_axiom_grounding('3f5c28ab-ea59-484d-976f-ef403c436a7d', climate_urgency_overrides_absolute_thresholds, instrumental).
narrative_ontology:cs_axiom('3f5c28ab-ea59-484d-976f-ef403c436a7d', foundational, intergenerational_waste_acceptable_relative_to_climate_catastrophe).
narrative_ontology:cs_axiom_status(intergenerational_waste_acceptable_relative_to_climate_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('3f5c28ab-ea59-484d-976f-ef403c436a7d', intergenerational_waste_acceptable_relative_to_climate_catastrophe, instrumental).
narrative_ontology:cs_reference_frame('3f5c28ab-ea59-484d-976f-ef403c436a7d', post_chernobyl_climate_deadlock).
narrative_ontology:cs_drift_state('3f5c28ab-ea59-484d-976f-ef403c436a7d', renewables_dominance_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3f5c28ab-ea59-484d-976f-ef403c436a7d', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, energy_consumers).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry_operators).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_adjacent_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, future_generations_waste_burden).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, uranium_mining_communities).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__comparative_risk_dominant, climate_mitigation_imperative).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__comparative_risk_dominant, energy_security_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face existential climate impacts (sea level rise, extreme heat, agricultural collapse) that nuclear energy helps mitigate by displacing fossil fuels. They gain avoided catastrophe but have no voice in nuclear siting decisions and bear no direct nuclear risk.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, beneficiary,
    powerless, biographical, trapped, global).

% Receive reliable, low-carbon baseload power from nuclear plants. Benefit from comparative risk framing that keeps nuclear in the energy mix, preventing price spikes and blackouts. Exit options limited by grid infrastructure and housing markets.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, energy_consumers, beneficiary,
    moderate, immediate, constrained, national).

% Directly profit from nuclear plant operation and new builds enabled by comparative risk acceptance. Shape regulatory frameworks through lobbying and technical expertise. Can relocate capital across jurisdictions; exit is arbitrage-grade.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry_operators, agenda_setter).

% Bear routine emissions risk, emergency planning burdens, property value impacts, and psychological stress from living near nuclear facilities. Identity-locked: community identity, generational housing, and local economies fuse with the plant's presence; exit means abandoning home, heritage, and social fabric.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_adjacent_communities, payer,
    moderate, biographical, identity_locked, local).

% Inherit high-level radioactive waste requiring active management for 100,000+ years. No consent possible, no exit, no voice in decisions that create their burden. Comparative framing discounts this intergenerational transfer by treating waste as 'manageable' relative to climate catastrophe.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, future_generations_waste_burden, payer,
    powerless, civilizational, trapped, global).

% Bear environmental contamination, health impacts, and cultural disruption from uranium extraction (often Indigenous lands). Comparative framing renders their sacrifice invisible by focusing only on reactor-site risk vs. climate benefit. Trapped by economic dependence and contaminated land.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, uranium_mining_communities, payer,
    powerless, generational, trapped, regional).

% Set licensing criteria, safety standards, and waste policy. Comparative risk framing becomes embedded in regulatory guidance (e.g., NRC's 'adequate protection' standard, IAEA safety standards). They administer the constraint but are structurally captive to the comparative framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, energy_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Argue that renewables+storage make nuclear unnecessary, so the comparative frame is a false dichotomy. Excluded from core risk-assessment bodies (IAEA, ICRP, national nuclear regulators) where comparative framing is institutionalized. Their objection is that the victim set is manufactured by excluding the no-nuclear pathway.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, renewable_energy_advocates, excluded,
    organized, biographical, constrained, global).

% Provide the carbon budget and warming trajectory data that make the comparative frame numerically coherent. See the full structure: climate risk is real and urgent; nuclear risk is real and localized; the framing chooses which risks count and for whom.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_science_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global decarbonization by legitimizing nuclear as a necessary bridge technology: aligns investment, regulation, and public acceptance around a shared 'lesser evil' narrative that overcomes nuclear stigma and enables climate action.
% TRANSFER_FUNCTION: Transfers localized radiological risk, intergenerational waste stewardship, and extractive mining burdens from the global climate-beneficiary pool onto specific communities (plant neighbors, mining regions, future generations) in exchange for avoided carbon emissions.
% ABSENT_VOICES: Future generations (cannot speak), uranium mining communities (structurally excluded from reactor-centric risk frameworks), and renewable-pathway advocates (excluded from nuclear governance bodies). The comparative frame only counts voices that accept its premises.
% DISAPPEARANCE_RATIONALE: If the comparative risk frame vanished, nuclear licensing would revert to absolute safety standards (ALARA, dose limits), likely stranding assets and slowing decarbonization. Climate-vulnerable populations lose a mitigation wedge; nuclear communities gain veto power; waste policy shifts to strict isolation requirements. The energy transition pathway reorganizes.
% FOUNDING_PROBLEM: Post-Chernobyl nuclear stigma and fossil fuel lock-in created a deadlock: climate action required rapid decarbonization, but nuclear was politically frozen. The comparative frame was built to break this deadlock by reframing nuclear risk as acceptable relative to the certain catastrophe of unchecked warming.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGIII (2022) and IEA Net Zero by 2050 (2021) attest nuclear's role in modeled pathways — but both note renewables+storage now dominate least-cost scenarios. Nuclear industry and some governments attest the framing remains necessary; renewable advocates and waste safety experts attest the founding deadlock is resolved by technology change, making the frame obsolete.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the constraint transfers substantial, non-consensual burdens (waste, routine risk, mining harm) onto identifiable victim groups while beneficiaries are diffuse and powerful. Suppression (0.52) is moderate: alternatives (renewables+storage) are not banned but are structurally disadvantaged by the framing that defines 'baseload' and 'reliability' in nuclear-favorable terms. Theater ratio (0.42) captures the gap between safety theater (deterministic safety analysis, design basis accidents) and the real risk governance (probabilistic risk assessment that excludes tail risks, waste policy that kicks the can forward). Accessibility collapse (0.61) is significant: once you accept the comparative frame, absolute safety thresholds and intergenerational equity arguments become 'unreasonable' — alternatives collapse into the frame. Resistance (0.55) is sustained but fragmented across local opposition, waste policy fights, and renewable advocacy.
 *
 * PERSPECTIVAL GAP:
 *   From the climate-vulnerable seat, this is a Rope: genuine coordination solving an existential collective action problem. From the nuclear-adjacent community seat, it is a Snare: extraction dressed as necessity, with exit blocked by identity and economics. From the future-generations seat, it is a Mountain of injustice: an absolute intergenerational transfer framed as relative. The engine computes these divergences from the structural data; the claimed_type (tangled_rope) captures the coordinator's view, not the victims'.
 *
 * DIRECTIONALITY LOGIC:
 *   Climate-vulnerable populations are structural beneficiaries (d ~ 0.15) — they gain avoided catastrophe with zero nuclear risk exposure. Energy consumers are near-symmetric beneficiaries (d ~ 0.4) — real benefit, diffuse indirect cost. Nuclear operators are strong beneficiaries (d ~ 0.1) — they capture profits and shape rules. Nuclear-adjacent communities are identity-locked targets (d ~ 0.85) — bear concentrated risk, cannot exit without identity rupture. Future generations are trapped targets (d ~ 0.95) — zero consent, zero exit, maximal time horizon. Uranium mining communities are trapped targets (d ~ 0.9) — contaminated land, economic dependence, no voice in reactor-level decisions. Regulators are analytical/institutional (d ~ 0.5) — they administer the frame but are captive to it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-Chernobyl nuclear stigma blocking climate action) is contested: renewable cost declines since 2010 arguably resolved the deadlock, but the constraint persists because nuclear industry, regulators, and some governments benefit from its continuation. Mandatrophy is unresolved — the arrangement outlives its original justification but captures new beneficiaries (nuclear new-build supply chains, national energy security narratives). The theater_ratio rise (0.25→0.52) tracks this: safety performance increasingly substitutes for the original climate coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the comparative_risk_dominant reading a distinct constraint from its sibling readings, or a measurement perspective on a single constraint?',
    'Apply the ε-invariance test: if catastrophic_tail_dominant and expected_value_dominant yield structurally different ε values, beneficiary/victim sets, and suppression mechanisms for the same nuclear licensing decisions, they are separate constraints. The kernel ''acceptable_risk_for_energy'' is a contested label covering multiple constraints.',
    'If distinct constraints, each must be authored separately with its own ε, stakeholders, and classification. Linking via network.affects_constraints captures the family structure. If a single constraint, the framework must model observable-dependent classification — which it rejects by design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the kernel''s declared readings are ε-invariant distinct constraints').

omega_variable(
    renewables_substitution_completeness,
    'Do renewables+storage now fully substitute for nuclear''s grid role, making the comparative frame''s ''necessary bridge'' premise empirically false?',
    'Grid modeling at high renewable penetration (80%+): if reliability, cost, and land-use metrics show nuclear adds no marginal value, the coordination function is vestigial and extraction becomes pure.',
    'If substitution is complete, the constraint degrades from Tangled Rope to Snare (coordination cover for extraction). If incomplete, Tangled Rope holds but with shifting coordination/extraction balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewables_substitution_completeness, empirical, 'Whether the coordination function remains genuine or has become vestigial').

omega_variable(
    intergenerational_discounting_mechanism,
    'How does the comparative frame structurally discount intergenerational waste burdens — is it explicit discounting, framing exclusion, or temporal myopia?',
    'Trace regulatory discourse (NRC, IAEA, national waste policies): does waste appear as a quantified cost in comparative assessments, or is it categorized as ''managed'' and excluded from the risk comparison?',
    'If explicit discounting, the extraction is acknowledged and debated. If framing exclusion, the constraint operates by making victims invisible — a stronger Snare signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_discounting_mechanism, conceptual, 'Mechanism by which future generations'' waste burden is excluded from the comparative calculus').

omega_variable(
    mining_community_invisibility,
    'Why are uranium mining communities (disproportionately Indigenous) excluded from the victim set in reactor-centric comparative risk assessments?',
    'Analyze the system boundary of standard nuclear risk assessments (PSA Level 3, IAEA Safety Standards): do they include front-end fuel cycle impacts, or only reactor-site risk?',
    'If system boundary exclusion is structural, the constraint''s extraction is larger than measured — a hidden Snare layer beneath the Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mining_community_invisibility, empirical, 'Whether front-end fuel cycle victims are structurally excluded from the comparative frame').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 1986, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acceptable_risk_comparative_tr_t1986, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1986, 0.25).
narrative_ontology:measurement(acceptable_risk_comparative_tr_t1995, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(acceptable_risk_comparative_tr_t2005, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(acceptable_risk_comparative_tr_t2015, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(acceptable_risk_comparative_tr_t2025, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2025, 0.42).
narrative_ontology:measurement(acceptable_risk_comparative_tr_t2035, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2035, 0.48).
narrative_ontology:measurement(acceptable_risk_comparative_tr_t2050, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 2050, 0.52).

% Extraction over time
narrative_ontology:measurement(acceptable_risk_comparative_be_t1986, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1986, 0.35).
narrative_ontology:measurement(acceptable_risk_comparative_be_t1995, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(acceptable_risk_comparative_be_t2005, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2005, 0.45).
narrative_ontology:measurement(acceptable_risk_comparative_be_t2015, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(acceptable_risk_comparative_be_t2025, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement(acceptable_risk_comparative_be_t2035, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2035, 0.62).
narrative_ontology:measurement(acceptable_risk_comparative_be_t2050, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 2050, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(acceptable_risk_comparative_su_t1986, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1986, 0.4).
narrative_ontology:measurement(acceptable_risk_comparative_su_t1995, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement(acceptable_risk_comparative_su_t2005, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(acceptable_risk_comparative_su_t2015, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(acceptable_risk_comparative_su_t2025, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2025, 0.52).
narrative_ontology:measurement(acceptable_risk_comparative_su_t2035, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2035, 0.55).
narrative_ontology:measurement(acceptable_risk_comparative_su_t2050, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 2050, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__comparative_risk_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_waste_governance__intergenerational_transfer).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, climate_mitigation_pathways__nuclear_wedge).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, energy_justice__siting_burden_distribution).

% DUAL FORMULATION NOTE:
% This constraint is one member of the 'acceptable_risk_for_energy' kernel family. The catastrophic_tail_dominant reading (constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant) treats tail risks as absolute vetoes, producing a Mountain or Snare classification depending on enforcement. The expected_value_dominant reading (constraint_id: acceptable_risk_for_energy__expected_value_dominant) produces a Rope or Tangled Rope with different victim weighting. All three share the kernel but instantiate different constraints with different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__comparative_risk_dominant, institutional, 0.15).
constraint_indexing:directionality_override(acceptable_risk_for_energy__comparative_risk_dominant, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
