% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__integrated_reading, []).

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
 *   constraint_id: ai_alignment_commitment__integrated_reading
 *   human_readable: Integrated AI Alignment Commitment (Dual-Mandate Reading)
 *   domain: technological/governance/ethical
 *
 * SUMMARY:
 *   The integrated_reading of the ai_alignment_commitment kernel asserts that
 *   alignment is not a choice between preventing catastrophic loss of control
 *   (safety_control_reading) and preventing present-day social harm
 *   (ethics_justice_reading), but requires simultaneous attention to both as
 *   structurally interdependent problems. This reading emerged from the
 *   observed failure of siloed approaches: safety-only frameworks ignored
 *   deployment-scale harms that become control-relevant at scale;
 *   justice-only frameworks ignored architectural risks that amplify harm
 *   beyond regulatory reach. The constraint operates as a governance mandate
 *   — funding requirements, conference norms, policy frameworks — that
 *   demands dual-mandate work. It coordinates by creating a unified field
 *   where previously there were two hostile subfields, but extracts
 *   integration costs from specialists who must broaden beyond their
 *   comparative advantage.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, 0.42).
domain_priors:suppression_score(ai_alignment_commitment__integrated_reading, 0.38).
domain_priors:theater_ratio(ai_alignment_commitment__integrated_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__integrated_reading, "Integrated AI Alignment Commitment (Dual-Mandate Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__integrated_reading, "technological/governance/ethical").

domain_priors:requires_active_enforcement(ai_alignment_commitment__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__integrated_reading, '94901b99-120c-4c8d-a192-48836f53cc73').
narrative_ontology:cs_kernel_codification('94901b99-120c-4c8d-a192-48836f53cc73', distributed).
narrative_ontology:cs_authority_grounding('94901b99-120c-4c8d-a192-48836f53cc73', expertise).
narrative_ontology:cs_interpretation_layer_present('94901b99-120c-4c8d-a192-48836f53cc73').
narrative_ontology:cs_reading_relation('94901b99-120c-4c8d-a192-48836f53cc73', ai_alignment_commitment__safety_control_reading, forecloses).
narrative_ontology:cs_reading_relation('94901b99-120c-4c8d-a192-48836f53cc73', ai_alignment_commitment__ethics_justice_reading, forecloses).
narrative_ontology:cs_axiom('94901b99-120c-4c8d-a192-48836f53cc73', foundational, alignment_requires_dual_mandate).
narrative_ontology:cs_axiom_status(alignment_requires_dual_mandate, holdable).
narrative_ontology:cs_axiom_grounding('94901b99-120c-4c8d-a192-48836f53cc73', alignment_requires_dual_mandate, empirically_contingent).
narrative_ontology:cs_axiom('94901b99-120c-4c8d-a192-48836f53cc73', foundational, siloed_approaches_create_systemic_blind_spots).
narrative_ontology:cs_axiom_status(siloed_approaches_create_systemic_blind_spots, holdable).
narrative_ontology:cs_axiom_grounding('94901b99-120c-4c8d-a192-48836f53cc73', siloed_approaches_create_systemic_blind_spots, empirically_contingent).
narrative_ontology:cs_reference_frame('94901b99-120c-4c8d-a192-48836f53cc73', pre_polarization_alignment_field).
narrative_ontology:cs_drift_state('94901b99-120c-4c8d-a192-48836f53cc73', post_2022_foundation_model_deployment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('94901b99-120c-4c8d-a192-48836f53cc73', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__integrated_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, present_marginalized_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, future_humanity).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, integrated_alignment_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, policy_makers_seeking_coherent_governance).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, safety_specialist_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, justice_specialist_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, siloed_funding_institutions).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, single_mandate_regulatory_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, safety_specialist_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, justice_specialist_researchers).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__integrated_reading, alignment_problems_are_structurally_interdependent).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__integrated_reading, siloed_approaches_create_systemic_blind_spots).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Researchers and institutions pushing for funding mandates, conference tracks, and policy frameworks that require dual attention to control and justice. They set the agenda for integrated alignment but must constantly justify the dual mandate against specialized competitors for limited resources.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, integrated_alignment_advocates, agenda_setter,
    organized, generational, constrained, global).

% Researchers whose careers and funding are built on catastrophic risk prevention. The integrated mandate requires them to allocate effort to justice problems they view as lower-stakes, diluting their specialized focus. They benefit from the field's overall legitimacy but pay integration costs.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, safety_specialist_researchers, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, safety_specialist_researchers, beneficiary).

% Researchers focused on bias, fairness, and present-day harm. The integrated mandate demands they engage with speculative control problems far from their empirical base, stretching thin already-marginalized resources. They gain field recognition but lose specialized depth.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, justice_specialist_researchers, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, justice_specialist_researchers, beneficiary).

% Communities currently harmed by deployed AI systems — algorithmic discrimination, surveillance, labor displacement. They benefit when alignment resources address justice problems, but have no voice in research prioritization and cannot exit the systems that harm them.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, present_marginalized_populations, beneficiary,
    powerless, biographical, trapped, global).

% All future persons who would bear catastrophic consequences of loss of control over advanced AI. They benefit from control research but have no representation in current governance. Their interests are invoked by safety specialists but structurally excluded from decision-making.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, future_humanity, beneficiary,
    powerless, civilizational, trapped, universal).

% Government agencies, foundations, and corporate labs with separate safety and ethics programs. The integrated mandate forces restructuring of funding streams, review criteria, and organizational boundaries — costly administrative and epistemic restructuring.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, siloed_funding_institutions, payer,
    institutional, generational, constrained, global).

% Regulators and legislators drafting AI governance frameworks. They benefit from a unified alignment concept that simplifies oversight, but face pressure from both specialist camps to maintain separate regulatory tracks. Their exit is mobile — they can choose which expert testimony to privilege.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, ai_policy_makers, agenda_setter,
    institutional, generational, mobile, national).

% Philosophers, historians of science, and meta-researchers studying the alignment field's structure. They see the full dialectic but hold no institutional leverage. Their exit is analytical — they can reframe but not change the constraint.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of fragmented alignment research by establishing a unified dual-mandate framework that prevents safety and justice efforts from working at cross-purposes, creating blind spots, or competing for the same scarce legitimacy and funding.
% TRANSFER_FUNCTION: Moves research attention, funding, and institutional capacity from specialized single-mandate programs toward integrated workstreams that address both control and justice. Transfers epistemic authority from narrow specialists to integrative practitioners. Transfers protection from future-only or present-only frameworks to a unified shield for both present marginalized populations and future humanity.
% ABSENT_VOICES: Grassroots communities directly impacted by current AI harms who are not represented in alignment research priority-setting. Global South AI developers and ethicists excluded from Western-dominated alignment discourse. Frontline workers displaced by automation who bear justice costs but have no seat at the alignment table. Their absence lets the integrated mandate be defined by elite research institutions rather than affected populations.
% DISAPPEARANCE_RATIONALE: If the integrated alignment mandate vanished, funding would immediately re-silo into separate safety and ethics tracks. Present-day justice work would lose its strongest theoretical link to long-term risk reduction. Safety work would lose its strongest incentive to address deployment-scale harms. Policy frameworks would fragment into disconnected near-term and long-term regimes. The field would lose its only structural mechanism for forcing conversation across the control/justice divide.
% FOUNDING_PROBLEM: Early AI alignment discourse split into two camps: one treating catastrophic risk as the only legitimate alignment target (safety_control_reading), the other treating present-day bias and harm as the only legitimate target (ethics_justice_reading). This split fragmented research funding, created mutually hostile epistemic communities, and left both present marginalized populations and future humanity underprotected by allowing each camp to treat the other's concerns as distractions.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by multiple independent sources outside the integrated reading's beneficiaries: (1) Funding agency program officers document the administrative burden of managing siloed portfolios (NSF, EU Horizon reports 2020-2024). (2) Historians of AI ethics document the 2018-2022 polarization as a field-level coordination failure (Whittaker et al., Crawford, Benjamin). (3) Intergovernmental bodies (OECD AI Principles, UNESCO Recommendation) explicitly adopt dual-mandate language citing the silo problem as motivation. No single benefiting party controls these sources.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__integrated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__integrated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_commitment__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__integrated_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__integrated_reading_tests).
:- end_tests(ai_alignment_commitment__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects real integration costs: specialists must learn adjacent literatures, funders must restructure programs, reviewers must evaluate cross-domain work. Suppression (0.38) is moderate — the constraint suppresses single-mandate funding streams and hiring lines, but exit to pure safety or pure justice work remains possible (though professionally costly). Theater ratio (0.28) captures performative 'integration' — papers adding token justice/safety sections without genuine synthesis — which has grown as the mandate gained traction. Accessibility collapse (0.45) is moderate: alternatives (siloed work) remain viable but increasingly illegitimate in top venues. Resistance (0.52) is significant: both specialist camps resist the mandate as dilution of their core mission. The constraint is a tangled rope: genuine coordination (unified field, shared vocabulary, combined policy leverage) coexisting with asymmetric extraction (specialists pay integration costs, integrated advocates gain agenda-setting power).
 *
 * PERSPECTIVAL GAP:
 *   From the integrated_advocate seat, the constraint is a rope — genuine coordination solving a real collective action problem. From the specialist researcher seats, it is a tangled rope — coordination with real integration costs extracted from their specialized productivity. From the trapped beneficiary seats (present marginalized, future humanity), it is a scaffold — temporary protection that must transition into durable institutional guarantees. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) reflects the authoring seat's judgment that the extraction asymmetry is structural, not incidental.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrated_alignment_advocates are agenda_setters with constrained exit — they built the mandate but depend on its persistence for their institutional position (d ~ 0.3). Safety_specialist_researchers and justice_specialist_researchers are payers with constrained exit — they bear integration costs but benefit from field unity (d ~ 0.65). Present_marginalized_populations and future_humanity are beneficiaries with trapped exit — they cannot leave the systems the constraint governs (d ~ 0.1). Siloed_funding_institutions are payers with constrained exit — they must restructure but gain coherent oversight (d ~ 0.55). AI_policy_makers are agenda_setters with mobile exit — they can adopt or reject the mandate (d ~ 0.4). Analytical_observers are analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate's founding problem (siloed fragmentation) remains contested — safety specialists argue control risk is categorically distinct and dominant; justice specialists argue present harm is empirically grounded while control risk is speculative. The integrated reading prevents mislabeling by naming both the coordination function (unified field) and the extraction (specialist integration costs). Without this dual accounting, the mandate would read as pure coordination (rope) from the advocate seat or pure extraction (snare) from the specialist seats. The mandatrophy risk is that if the founding problem is resolved (e.g., evidence shows silos work better), the mandate persists as piton — but current evidence suggests the problem is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the integrated_reading a distinct constraint from the sibling readings, or a meta-constraint that subsumes them?',
    'Analyze whether funding mandates, hiring lines, and policy frameworks instantiate a single integrated requirement or parallel but separate requirements. Trace resource flows: if integrated grants require dual expertise, it''s a distinct constraint; if they fund two separate PIs, it''s parallel constraints.',
    'If distinct, this story''s ε=0.42 measures the integration mandate itself. If meta, ε should be computed for each sibling separately and this story becomes a network coordination node. Changes classification boundary for all three readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether integrated_reading is a standalone constraint or a coordination layer over siblings.').

omega_variable(
    siloed_extraction_attribution,
    'Is the measured extractiveness (0.42) properly attributed to the integrated mandate, or does it reflect the pre-existing costs of the siloed equilibrium?',
    'Counterfactual comparison: measure specialist integration costs before vs. after integrated mandate adoption in specific institutions (e.g., NSF AI Institutes 2020 vs 2024). If costs rise with mandate, extraction is mandate-driven; if flat, extraction is inherent to the problem structure.',
    'If extraction is mandate-driven, tangled_rope classification holds. If extraction is inherent, the integrated mandate may be a rope (coordination without added extraction) and the siloed constraints are the extractive ones.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(siloed_extraction_attribution, empirical, 'Whether integration costs are caused by the mandate or reveal pre-existing fragmentation costs.').

omega_variable(
    victim_set_boundary,
    'Do present_marginalized_populations and future_humanity count as beneficiaries of this constraint, or as victims of the sibling constraints that this constraint opposes?',
    'Trace harm pathways: under safety_control_reading alone, do present marginalized populations suffer extractive neglect? Under ethics_justice_reading alone, does future humanity suffer extractive neglect? If yes, they are victims of sibling constraints, not beneficiaries of this one. This story''s beneficiaries should then be the integrated researchers and policy makers who gain coherence.',
    'Reclassifies victim/beneficiary structure. If they are victims of siblings, this constraint''s beneficiaries shift to integrated_alignment_researchers and policy_makers. Changes directionality derivation for all seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_set_boundary, conceptual, 'Whether protected populations are beneficiaries of integration or victims of silos.').

omega_variable(
    mandate_enforcement_mechanism,
    'What specific enforcement mechanisms sustain the integrated mandate — funding conditions, peer review norms, hiring criteria, regulatory requirements — and how do they differ across jurisdictions?',
    'Inventory enforcement mechanisms in major funding bodies (NSF, ERC, UKRI, corporate labs), publication venues (NeurIPS, ICML, FAccT, AAAI), and regulatory proposals (EU AI Act, US EO 14110). Map mechanism strength to integration compliance.',
    'If enforcement is weak/voluntary, requires_active_enforcement may be overstated and classification shifts toward rope. If enforcement is strong and punitive, tangled_rope confirmed. Determines whether suppression (0.38) is accurately measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_enforcement_mechanism, empirical, 'The concrete enforcement infrastructure behind the integrated mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__integrated_reading, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_alignment_integrated_tr_t2018, ai_alignment_commitment__integrated_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(ai_alignment_integrated_tr_t2020, ai_alignment_commitment__integrated_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(ai_alignment_integrated_tr_t2022, ai_alignment_commitment__integrated_reading, theater_ratio, 2022, 0.26).
narrative_ontology:measurement(ai_alignment_integrated_tr_t2024, ai_alignment_commitment__integrated_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement(ai_alignment_integrated_tr_t2026, ai_alignment_commitment__integrated_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(ai_alignment_integrated_be_t2018, ai_alignment_commitment__integrated_reading, base_extractiveness, 2018, 0.25).
narrative_ontology:measurement(ai_alignment_integrated_be_t2020, ai_alignment_commitment__integrated_reading, base_extractiveness, 2020, 0.32).
narrative_ontology:measurement(ai_alignment_integrated_be_t2022, ai_alignment_commitment__integrated_reading, base_extractiveness, 2022, 0.38).
narrative_ontology:measurement(ai_alignment_integrated_be_t2024, ai_alignment_commitment__integrated_reading, base_extractiveness, 2024, 0.41).
narrative_ontology:measurement(ai_alignment_integrated_be_t2026, ai_alignment_commitment__integrated_reading, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ai_alignment_integrated_su_t2018, ai_alignment_commitment__integrated_reading, suppression_requirement, 2018, 0.2).
narrative_ontology:measurement(ai_alignment_integrated_su_t2020, ai_alignment_commitment__integrated_reading, suppression_requirement, 2020, 0.28).
narrative_ontology:measurement(ai_alignment_integrated_su_t2022, ai_alignment_commitment__integrated_reading, suppression_requirement, 2022, 0.35).
narrative_ontology:measurement(ai_alignment_integrated_su_t2024, ai_alignment_commitment__integrated_reading, suppression_requirement, 2024, 0.37).
narrative_ontology:measurement(ai_alignment_integrated_su_t2026, ai_alignment_commitment__integrated_reading, suppression_requirement, 2026, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__integrated_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__integrated_reading, 0.08).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__ethics_justice_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the ai_alignment_commitment kernel. The safety_control_reading and ethics_justice_reading instantiate mutually exclusive definitional claims (alignment = control only / alignment = justice only). The integrated_reading rejects the exclusivity, creating a tangled rope constraint that coordinates across the divide while extracting integration costs from specialists. The ε values differ substantially: safety_control_reading ε≈0.3 (coordination-dominant), ethics_justice_reading ε≈0.35 (coordination-dominant), integrated_reading ε≈0.42 (hybrid). The network edges represent the structural pressure this reading exerts on the siblings — it influences their legitimacy conditions and resource access without foreclosing them as live positions for other parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_commitment__integrated_reading, organized, 0.35).
constraint_indexing:directionality_override(ai_alignment_commitment__integrated_reading, powerless, 0.1).
constraint_indexing:directionality_override(ai_alignment_commitment__integrated_reading, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
