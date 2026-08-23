% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__equity_reading, []).

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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: CBDR-Interpreted NDC Equity Reading
 *   domain: international_law/climate_governance
 *
 * SUMMARY:
 *   The Paris Agreement's Article 4 establishes Nationally Determined
 *   Contributions (NDCs) as the mitigation architecture. This constraint
 *   story captures the equity_reading: NDCs must be interpreted through the
 *   Common But Differentiated Responsibilities and Respective Capabilities
 *   (CBDR-RC) principle, requiring structural distinctions between developed
 *   and developing country obligations. This reading holds that developed
 *   countries bear binding absolute mitigation targets and finance
 *   obligations (Articles 4.4, 9), while developing countries' NDCs are
 *   conditional on support and reflect equity-based differentiation. The
 *   constraint coordinates universal participation through asymmetric
 *   burden-sharing, but extracts disproportionate costs from developed
 *   country taxpayers and high-emission industries while granting policy
 *   space and veto power to equity coalitions. The claimed type is
 *   tangled_rope — genuine coordination function (universal treaty
 *   participation) fused with asymmetric extraction (finance transfers,
 *   differentiated stringency).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.55).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.45).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "CBDR-Interpreted NDC Equity Reading").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international_law/climate_governance").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, '162424f2-be6d-4861-a7ac-ce456328edf0').
narrative_ontology:cs_kernel_codification('162424f2-be6d-4861-a7ac-ce456328edf0', formalized).
narrative_ontology:cs_authority_grounding('162424f2-be6d-4861-a7ac-ce456328edf0', lineage).
narrative_ontology:cs_interpretation_layer_present('162424f2-be6d-4861-a7ac-ce456328edf0').
narrative_ontology:cs_reading_relation('162424f2-be6d-4861-a7ac-ce456328edf0', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('162424f2-be6d-4861-a7ac-ce456328edf0', paris_article_4_ndc__supranational_reading, influences).
narrative_ontology:cs_axiom('162424f2-be6d-4861-a7ac-ce456328edf0', foundational, historical_responsibility_entails_differentiated_obligations).
narrative_ontology:cs_axiom_status(historical_responsibility_entails_differentiated_obligations, holdable).
narrative_ontology:cs_axiom_grounding('162424f2-be6d-4861-a7ac-ce456328edf0', historical_responsibility_entails_differentiated_obligations, deontological).
narrative_ontology:cs_axiom('162424f2-be6d-4861-a7ac-ce456328edf0', foundational, capability_differentiation_justifies_policy_space).
narrative_ontology:cs_axiom_status(capability_differentiation_justifies_policy_space, holdable).
narrative_ontology:cs_axiom_grounding('162424f2-be6d-4861-a7ac-ce456328edf0', capability_differentiation_justifies_policy_space, deontological).
narrative_ontology:cs_reference_frame('162424f2-be6d-4861-a7ac-ce456328edf0', unfccc_cbdr_principle).
narrative_ontology:cs_drift_state('162424f2-be6d-4861-a7ac-ce456328edf0', paris_agreement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('162424f2-be6d-4861-a7ac-ce456328edf0', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, developing_country_parties).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, equity_coalitions).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, climate_finance_institutions).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, small_island_developing_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_country_parties).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, high_emission_industries_developed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developing_country_parties).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, common_but_differentiated_responsibilities).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, equity_principle_in_climate_law).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, historical_responsibility_entails_differentiated_obligations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear binding mitigation obligations and climate finance transfer commitments under Article 9. Their NDCs are expected to reflect highest possible ambition. Exit from the treaty framework carries diplomatic and reputational costs; domestic political resistance to finance transfers creates internal pressure.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_country_parties, payer,
    institutional, generational, constrained, global).

% Retain policy space for development priorities; NDCs are conditional on finance, technology transfer, and capacity building. They contribute mitigation efforts but on differentiated terms. Exit is constrained by development finance dependencies and diplomatic positioning within G77+China.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developing_country_parties, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, developing_country_parties, payer).

% Coalitions like G77+China, LMDC, AOSIS, and African Group wield collective veto power over supranational enforcement mechanisms. They negotiate as blocs to preserve CBDR interpretation. Their influence derives from consensus rules in the COP/CMA; exit means fragmentation and loss of blocking power.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, equity_coalitions, beneficiary,
    organized, generational, constrained, global).

% Institutions like the Green Climate Fund, Global Environment Facility, and multilateral development banks administer finance flows mandated by the equity reading. They shape eligibility criteria, allocation rules, and reporting standards. They benefit from mandate expansion and operational budgets; exit options include pivoting to other development finance streams.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, climate_finance_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, climate_finance_institutions, beneficiary).

% Fossil fuel, heavy industry, and transport sectors in developed countries face transition costs driven by stringent NDCs. They lobby for weaker domestic implementation and against finance transfers. Exit options include carbon leakage, asset stranding, or political capture of regulatory processes.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, high_emission_industries_developed, payer,
    organized, biographical, constrained, national).

% Existentially vulnerable to climate impacts; depend on the equity reading's temperature goal (1.5°C) and loss-and-damage provisions. They have no meaningful exit — their territory and sovereignty are at stake. Their leverage is moral authority and coalition-building within AOSIS.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, small_island_developing_states, beneficiary,
    powerless, civilizational, trapped, global).

% NGOs, research institutions, and watchdog networks monitor NDC ambition, finance delivery, and transparency reporting. They provide independent assessment but hold no formal decision power. Their exit is analytical — they can shift focus to other governance domains.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, civil_society_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates global mitigation effort across countries according to historical responsibility and capability, enabling universal participation in a single treaty framework that would otherwise fracture along North-South lines.
% TRANSFER_FUNCTION: Moves financial resources, technology, and capacity-building support from developed to developing countries; allocates policy space to developing countries for development-first pathways; assigns binding mitigation burden to developed countries.
% ABSENT_VOICES: Future generations who inherit the carbon budget; non-party states (e.g., non-UNFCCC members); fossil fuel exporting developing countries whose interests diverge from the G77+China consensus; subnational actors in developed countries bearing localized transition costs.
% DISAPPEARANCE_RATIONALE: Without the CBDR interpretation, the Paris Agreement's participation architecture collapses: developing countries would not have joined without differentiated obligations; the global stocktake and ratchet mechanism lose their equity anchor; finance flows lose their legal basis; the treaty reverts to a Kyoto-style bifurcation that failed to achieve universal participation.
% FOUNDING_PROBLEM: How to achieve universal participation in global climate mitigation while respecting the historical responsibility of industrialized nations for accumulated emissions and the development imperatives of the Global South.
% FOUNDING_PROBLEM_CORROBORATION: UNFCCC negotiating records (1992-2015) document the North-South deadlock that CBDR resolved. G77+China statements consistently attest the problem persists. Independent legal scholarship (e.g., Rajamani, Brunnée, Streck) and IPCC WGIII assessments corroborate that capability and responsibility differentials remain structurally significant. Developed country governments contest the status, arguing evolving capabilities have shifted the problem.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__equity_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__equity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__equity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.55) reflects moderate but asymmetric extraction: developed countries transfer ~$100B/year (pledged, not fully delivered) plus domestic mitigation costs; developing countries receive finance and policy space. Suppression (0.45) is moderate — the enhanced transparency framework and global stocktake create peer pressure and naming-and-shaming, but no hard enforcement sanctions. Theater ratio (0.35) captures performative NDC submissions that signal ambition without domestic implementation, and finance accounting creative practices. Accessibility collapse (0.4) is moderate — alternative governance forms (climate clubs, carbon markets, unilateral measures) exist but the treaty framework dominates. Resistance (0.5) reflects developed country pushback on finance delivery and developing country resistance to enhanced transparency requirements.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat types: from the developed country payer seat, the constraint appears as a snare (high extraction, active enforcement, constrained exit). From the developing country beneficiary seat, it appears as a rope (genuine coordination, net benefit). From the equity coalition seat, it appears as a scaffold (transitional equity mechanism with sunset logic tied to capability convergence). The agenda-setter (finance institutions) sees a rope with institutional capture. The observer sees the structural tension. This divergence is the measurement — the engine computes it from the authored structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed country parties are primary payers (d ~0.75): they bear binding mitigation costs and finance obligations with constrained exit (treaty withdrawal is costly). Developing country parties are net beneficiaries (d ~0.25): they receive finance, technology, and policy space, though they pay in mitigation effort. Equity coalitions are structural beneficiaries (d ~0.15): they gain veto power over supranational enforcement. Climate finance institutions are agenda-setters with arbitrage exit — they administer flows but can pivot. High-emission industries in developed countries are payers with constrained exit (capital lock-in). Small island states are trapped beneficiaries (existential stakes, no exit). Civil society observers sit at analytical d=0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (universal participation with equity) remains contested, not dead. The mandate has not atrophied — the equity reading is actively defended by G77+China and AOSIS in every COP/CMA. However, practice drift is substantial: developed countries treat finance as voluntary, push for uniform transparency that erodes differentiation, and advocate for 'ratchet' mechanisms that collapse CBDR into a single ambition trajectory. The constraint persists because no alternative architecture achieves universal participation, but its operational form drifts toward the supranational_reading. This is not mandatrophy (dead mandate persisting) but mandate contestation with active drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cbdr_coordination_vs_extraction,
    'Is the CBDR interpretation a genuine coordination mechanism enabling universal participation, or a cover story for extracting transfers from developed countries while shielding developing country emissions?',
    'Counterfactual analysis: would developing countries have joined a non-differentiated treaty? Historical record of Kyoto non-participation vs. Paris universal participation. Economic modeling of mitigation costs with/without finance transfers.',
    'If genuine coordination, the asymmetric extraction is the price of universal participation (tangled_rope). If cover story, the constraint is a snare with coordination theater. Determines whether the equity reading''s extraction is structurally necessary or parasitically layered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cbdr_coordination_vs_extraction, conceptual, 'Whether CBDR differentiation is a necessary coordination device or an extractive cover.').

omega_variable(
    finance_delivery_gap,
    'Does the $100B/year finance commitment (and post-2025 goal) represent a real transfer obligation or a rhetorical pledge with no enforcement?',
    'OECD climate finance delivery reports tracked against UNFCCC standing committee assessments; independent verification of additionality, concessionality, and grant-equivalent valuation.',
    'If finance delivery is real and additional, extraction from developed countries is substantive (tangled_rope/snare). If largely re-labeled ODA, the equity reading''s transfer function is theatrical — extraction is lower but so is coordination credibility. Affects epsilon and theater_ratio trajectories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(finance_delivery_gap, empirical, 'Whether promised climate finance materializes as real resource transfer.').

omega_variable(
    equity_coalition_veto_substance,
    'Do equity coalitions (G77+China, LMDC, AOSIS) hold genuine structural veto power over supranational enforcement, or is their influence rhetorical within a consensus process that ultimately bends to major power preferences?',
    'Process-tracing of COP/CMA decisions on transparency framework, global stocktake modalities, and Article 6 rules: identify instances where equity coalitions blocked or substantively modified outcomes opposed by developed countries.',
    'If veto is real, the equity reading constrains supranational_reading''s enforcement trajectory (influences relation). If rhetorical, the equity reading is a side-payment to secure participation while supranational enforcement advances (forecloses relation in practice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_coalition_veto_substance, empirical, 'Whether equity coalition veto power is structural or performative.').

omega_variable(
    kernel_reading_relations,
    'How do the three readings of the paris_article_4_ndc kernel structurally relate: does the equity_reading foreclose, coexist with, or influence the sovereigntist_reading and supranational_reading?',
    'Legal-political analysis of whether a single party''s framework can simultaneously hold CBDR differentiation and national sovereignty primacy (coexists_with), or whether CBDR logically contradicts voluntary self-determination (forecloses). Track whether equity_reading''s finance and differentiation demands create downstream pressure on supranational_reading''s uniform ratchet (influences).',
    'Determines the kernel''s internal dynamics: three-way coexistence = fragmented regime; equity forecloses sovereigntist = North-South polarization; equity influences supranational = contested evolution toward hybrid regime.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relations between the three kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_tr_t0, paris_article_4_ndc__equity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_tr_t4, paris_article_4_ndc__equity_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_tr_t8, paris_article_4_ndc__equity_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_tr_t12, paris_article_4_ndc__equity_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_tr_t16, paris_article_4_ndc__equity_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_tr_t20, paris_article_4_ndc__equity_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_be_t0, paris_article_4_ndc__equity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_be_t4, paris_article_4_ndc__equity_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_be_t8, paris_article_4_ndc__equity_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_be_t12, paris_article_4_ndc__equity_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_be_t16, paris_article_4_ndc__equity_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_be_t20, paris_article_4_ndc__equity_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_su_t0, paris_article_4_ndc__equity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_su_t4, paris_article_4_ndc__equity_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_su_t8, paris_article_4_ndc__equity_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_su_t12, paris_article_4_ndc__equity_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_su_t16, paris_article_4_ndc__equity_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(paris_article_4_ndc__equity_reading_su_t20, paris_article_4_ndc__equity_reading, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__equity_reading, 0.1).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_9_finance).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_13_transparency).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_14_global_stocktake).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_6_cooperative_approaches).

% DUAL FORMULATION NOTE:
% This constraint (equity_reading) is one of three readings of the paris_article_4_ndc kernel. The sovereigntist_reading (voluntary pledges, national sovereignty) and supranational_reading (binding ratchet, international accountability) form a constraint family linked by network.affects_constraints. The equity_reading has moderate epsilon with asymmetric distribution; sovereigntist_reading has low epsilon (voluntary) but high suppression for non-participants; supranational_reading has high epsilon (binding enforcement) with symmetric distribution. All three share the kernel but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__equity_reading, institutional, 0.75).
constraint_indexing:directionality_override(paris_article_4_ndc__equity_reading, organized, 0.25).
constraint_indexing:directionality_override(paris_article_4_ndc__equity_reading, powerless, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
