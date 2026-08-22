% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__resource_sovereignty_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__resource_sovereignty_primacy, []).

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
 *   constraint_id: provincial_sovereignty_boundary__resource_sovereignty_primacy
 *   human_readable: Provincial Resource Sovereignty Primacy (s.92A CA 1982)
 *   domain: political/federalism/resource_governance
 *
 * SUMMARY:
 *   Section 92A of the Constitution Act, 1982 entrenched provincial ownership
 *   and management of natural resources. The resource_sovereignty_primacy
 *   reading treats this as grounding absolute provincial sovereignty over
 *   resources — resource control equals territorial sovereignty; federal
 *   climate and fiscal policy in the resource space is illegitimate
 *   extraction; unilateral provincial exit from federal schemes is a
 *   constitutional right. This reading is advanced by resource-rich provinces
 *   (Alberta, Saskatchewan, Newfoundland) and their aligned industries. It
 *   operates as a tangled rope: it coordinates resource development within
 *   provincial boundaries (genuine coordination function) while extracting
 *   regulatory authority and fiscal capacity from federal and interprovincial
 *   claimants (asymmetric extraction). Active enforcement is required —
 *   provinces litigate, legislate (e.g., Alberta Sovereignty Act), and use
 *   regulatory permitting to maintain the boundary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.62).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.58).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.62).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Provincial Resource Sovereignty Primacy (s.92A CA 1982)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, '77aa4bc5-caea-4d6c-99a2-0a0c32e6d279').
narrative_ontology:cs_kernel_codification('77aa4bc5-caea-4d6c-99a2-0a0c32e6d279', formalized).
narrative_ontology:cs_authority_grounding('77aa4bc5-caea-4d6c-99a2-0a0c32e6d279', extraction).
narrative_ontology:cs_interpretation_layer_present('77aa4bc5-caea-4d6c-99a2-0a0c32e6d279').
narrative_ontology:cs_reading_relation('77aa4bc5-caea-4d6c-99a2-0a0c32e6d279', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_reading_relation('77aa4bc5-caea-4d6c-99a2-0a0c32e6d279', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_axiom('77aa4bc5-caea-4d6c-99a2-0a0c32e6d279', foundational, resource_jurisdiction_equals_full_sovereignty).
narrative_ontology:cs_axiom_status(resource_jurisdiction_equals_full_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('77aa4bc5-caea-4d6c-99a2-0a0c32e6d279', resource_jurisdiction_equals_full_sovereignty, conventional).
narrative_ontology:cs_axiom('77aa4bc5-caea-4d6c-99a2-0a0c32e6d279', foundational, federal_climate_policy_illegitimate_in_resource_space).
narrative_ontology:cs_axiom_status(federal_climate_policy_illegitimate_in_resource_space, holdable).
narrative_ontology:cs_axiom_grounding('77aa4bc5-caea-4d6c-99a2-0a0c32e6d279', federal_climate_policy_illegitimate_in_resource_space, instrumental).
narrative_ontology:cs_axiom('77aa4bc5-caea-4d6c-99a2-0a0c32e6d279', secondary, unilateral_provincial_exit_constitutional_right).
narrative_ontology:cs_axiom_status(unilateral_provincial_exit_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('77aa4bc5-caea-4d6c-99a2-0a0c32e6d279', unilateral_provincial_exit_constitutional_right, conventional).
narrative_ontology:cs_reference_frame('77aa4bc5-caea-4d6c-99a2-0a0c32e6d279', s92a_entrenchment_1982).
narrative_ontology:cs_drift_state('77aa4bc5-caea-4d6c-99a2-0a0c32e6d279', post_pan_canadian_framework_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('77aa4bc5-caea-4d6c-99a2-0a0c32e6d279', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_crowns).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_extraction_incumbents).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_treasuries).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_climate_policy_agents).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, interprovincial_trade_beneficiaries).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_resource_rights_holders).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, national_fiscal_stabilization_beneficiaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_extraction_incumbents).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_ownership_natural_resources_s92a).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_control_equals_territorial_sovereignty).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, unilateral_exit_constitutional_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer Crown resource rights, grant tenures, collect royalties, and set regulatory terms. They are the operational face of the province's sovereignty claim. They control the licensing machinery that federal policy must work through. Their institutional mandate is to maximize provincial benefit from resources; federal intrusion is read as ultra vires.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_crowns, agenda_setter,
    institutional, generational, arbitrage, regional).

% Receive direct royalty and tax revenue from resource development. This revenue funds provincial services without federal transfers, creating fiscal independence that underwrites the sovereignty claim. They benefit from the constraint's extraction of resource rents from federal and interprovincial claims.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_treasuries, beneficiary,
    institutional, biographical, mobile, regional).

% Hold long-term leases and regulatory relationships with provincial Crowns. They benefit from stable, predictable provincial jurisdiction and resist federal layering (carbon pricing, impact assessment). They pay royalties to provinces but treat the provincial monopoly as a single-window advantage over fragmented federal regulation. Exit means stranding assets.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_extraction_incumbents, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_extraction_incumbents, payer).

% Administer national carbon pricing, clean fuel regulations, and emissions caps. Their policies are structurally constrained by provincial refusal to cede resource jurisdiction. They bear the cost of policy fragmentation, legal challenges, and negotiated equivalency agreements. Their exit is constitutional amendment — politically prohibitive.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_climate_policy_agents, payer,
    institutional, generational, constrained, national).

% Firms and consumers who would benefit from a seamless internal market in energy, electricity, and processed resources. They pay the friction costs of 13 distinct regulatory regimes, transmission barriers, and province-first procurement. Their exit is lobbying for federal paramountcy or interprovincial agreements — slow, uncertain, opposed by beneficiaries.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, interprovincial_trade_beneficiaries, payer,
    organized, biographical, constrained, national).

% Hold Aboriginal title and treaty rights to resources that the provincial sovereignty claim treats as provincial Crown property. They are structurally excluded from the s.92A settlement — their consent was neither sought nor required. Their exit is litigation (slow, expensive) or nation-to-nation negotiation (requires federal counterparty the constraint weakens).
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_resource_rights_holders, excluded,
    organized, generational, identity_locked, regional).

% Regions and populations that rely on federal equalization and fiscal transfers funded partly by resource revenues that provincial sovereignty keeps local. They bear the cost of fiscal divergence — richer resource provinces decouple from national risk-sharing. Their exit is political pressure for transfer reform, opposed by beneficiary provinces.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, national_fiscal_stabilization_beneficiaries, payer,
    moderate, biographical, constrained, national).

% Interpret s.92A, the division of powers, and the scope of provincial resource jurisdiction. They map the doctrinal boundary but do not collect or pay the constraint's rents. Their readings structure the litigation that tests the constraint's reach.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_scholars_courts, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates resource development within provincial boundaries: single-window tenure, unified environmental assessment, province-wide royalty regimes, and infrastructure planning. Solves the problem of fragmented authority over spatially concentrated resources.
% TRANSFER_FUNCTION: Moves resource rents (royalties, taxes, economic rent) from federal/interprovincial/indigenous claimants to provincial treasuries and provincial Crown corporations. Moves regulatory authority from federal climate/fiscal agencies to provincial resource ministries. Moves decision-making veto from nationalmajoritarian institutions to provincial executives.
% ABSENT_VOICES: Indigenous nations whose title and treaty rights predate and ground the provincial claim but were excluded from s.92A's enactment. Future generations who bear climate externalities from uncoordinated extraction. Atlantic and territorial populations who depend on equalization funded by resource revenues the constraint keeps provincial.
% DISAPPEARANCE_RATIONALE: If provincial resource sovereignty vanished overnight, federal climate policy would apply uniformly, interprovincial energy trade would integrate, equalization would recalibrate to include resource revenues, and Indigenous consent would become a jurisdictional prerequisite — the entire political economy of Canadian federation would reorganize.
% FOUNDING_PROBLEM: The 1980-82 constitutional negotiations: provinces (especially Alberta, Saskatchewan, Newfoundland) demanded explicit constitutional protection of resource jurisdiction after the National Energy Program demonstrated federal capacity to override provincial resource decisions. s.92A was the compromise that entrenched provincial ownership and management.
% FOUNDING_PROBLEM_CORROBORATION: Provincial governments attest the NEP trauma proves the founding problem is live — federal overreach remains the threat. Federal government and academic federalists attest the NEP was a specific historical episode, not a permanent structural risk, and that climate change creates a new founding problem (coordinated decarbonization) the constraint obstructs. Indigenous legal orders attest the founding problem never included their consent.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the constraint's diversion of fiscal and regulatory capacity from federal climate policy, national market integration, and Indigenous jurisdiction to provincial treasuries and Crowns. Suppression (0.58) reflects active legal and regulatory enforcement to maintain the boundary — reference cases, constitutional challenges, permitting walls. Theater (0.42) has risen as the coordination justification (single-window management) becomes proportionally smaller than the extraction function (blocking carbon pricing, equalization inclusion, Indigenous consent). Accessibility collapse (0.35) is moderate: alternatives (federal paramountcy, interprovincial agreements, Indigenous co-management) exist but are structurally blocked. Resistance (0.48) is significant from federal, Indigenous, and interprovincial actors but has not shifted the structural boundary.
 *
 * PERSPECTIVAL GAP:
 *   From the provincial agenda-setter seat, the constraint is a rope — genuine coordination of resource development within a clear jurisdiction. From federal climate and Indigenous excluded seats, it is a snare — extraction of authority and consent backed by constitutional entrenchment. From resource incumbent seats, it is a tangled rope — they get coordination value but pay monopoly royalties and face regulatory capture risk. The engine computes this divergence from the declared structural positions; the authored claim (tangled_rope) represents the generating model's structural assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   Provincial resource Crowns and treasuries are structural beneficiaries (d near 0.0-0.2): they collect rents and set rules. Resource incumbents are dual-positioned: beneficiaries of stable jurisdiction, payers of royalties (net d ~0.3-0.4). Federal climate agents, interprovincial trade beneficiaries, and equalization beneficiaries are structural payers (d 0.7-0.9): they bear fragmentation costs and lost policy capacity. Indigenous rights holders are excluded and identity-locked (d ~0.95): the constraint defines their title as subordinate to provincial Crown ownership; exit requires constitutional recognition they are structurally denied. Constitutional scholars are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal overreach a la NEP) is contested as live vs. historical. The constraint now operates substantially beyond the NEP trauma: it blocks climate coordination that did not exist in 1982, excludes Indigenous jurisdiction that was invisible in 1982, and enables fiscal decoupling that undermines the federal bargain. The coordination function (single-window resource management) persists but the extraction function (blocking federal climate policy, equalization inclusion, Indigenous consent) has grown. This is textbook mandatrophy: the mandate (prevent NEP-style overreach) has atrophied relative to the constraint's current extraction profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the provincial single-window resource management function structurally separable from the extraction of federal climate authority and Indigenous consent?',
    'Counterfactual: if provinces voluntarily harmonized carbon pricing and Indigenous co-management while retaining tenure/royalty administration, would coordination survive? Evidence from Quebec''s cap-and-trade linkage and BC''s revenue-sharing agreements tests separability.',
    'If separable, the constraint''s extraction is gratuitous — the coordination function could persist without the sovereignty claim. If inseparable, the extraction is the price of coordination (the constraint is a genuine tangled rope). If coordination is a cover, the constraint is a snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable or whether the sovereignty claim is the load-bearing wall for both.').

omega_variable(
    indigenous_consent_as_extraction_or_coordination,
    'Does the exclusion of Indigenous jurisdiction from the provincial sovereignty claim serve a coordination function (clear decision rules) or is it pure extraction (denying consent to capture rents)?',
    'Compare outcomes in jurisdictions with Indigenous co-management (e.g., James Bay, modern treaties) vs. unilateral provincial management: if co-management produces better coordination (less conflict, more stable investment), exclusion is extractive.',
    'If exclusion is extractive, the constraint''s victim set expands and its suppression score understates the internalized coercion on Indigenous nations. If exclusion coordinates, the constraint''s coordination function includes boundary maintenance against competing sovereignty claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_consent_as_extraction_or_coordination, empirical, 'Whether Indigenous exclusion is a coordination feature or an extraction mechanism.').

omega_variable(
    committer_frame_ambiguity,
    'Does the resource_sovereignty_primacy reading foreclose the compact_federalism reading, or do they coexist as distinct provincial strategies?',
    'Track provincial government behavior: do the same provinces advance both readings in different contexts (primacy in court, compact in intergovernmental negotiation)? If a single government advances both, they coexist. If primacy advocates explicitly reject compact theory, forecloses.',
    'If forecloses, the kernel has a genuine logical split — the engine''s cs_axiom_contradiction will detect it. If coexists, the kernel''s readings are strategic framings, not distinct commitments, and the constraint family maps political positioning more than constitutional structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Structural relationship between this reading and its compact_federalism sibling — foreclosure vs. strategic coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 1982, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_sov_bdry_prim_tr_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1982, 0.15).
narrative_ontology:measurement(prov_sov_bdry_prim_tr_t1990, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(prov_sov_bdry_prim_tr_t2000, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(prov_sov_bdry_prim_tr_t2010, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2010, 0.32).
narrative_ontology:measurement(prov_sov_bdry_prim_tr_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(prov_sov_bdry_prim_tr_t2020, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(prov_sov_bdry_prim_tr_t2025, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(prov_sov_bdry_prim_be_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1982, 0.25).
narrative_ontology:measurement(prov_sov_bdry_prim_be_t1990, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(prov_sov_bdry_prim_be_t2000, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(prov_sov_bdry_prim_be_t2010, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(prov_sov_bdry_prim_be_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(prov_sov_bdry_prim_be_t2020, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2020, 0.59).
narrative_ontology:measurement(prov_sov_bdry_prim_be_t2025, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(prov_sov_bdry_prim_su_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1982, 0.2).
narrative_ontology:measurement(prov_sov_bdry_prim_su_t1990, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(prov_sov_bdry_prim_su_t2000, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(prov_sov_bdry_prim_su_t2010, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(prov_sov_bdry_prim_su_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(prov_sov_bdry_prim_su_t2020, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(prov_sov_bdry_prim_su_t2025, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_allocation).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.15).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_carbon_pricing_backstop).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, interprovincial_electricity_transmission).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, equalization_formula_resource_revenue).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_resource_consent_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint (resource_sovereignty_primacy) is one of three readings of the provincial_sovereignty_boundary kernel. It differs from compact_federalism (which grounds sovereignty in the original compact, not s.92A specifically) and constitutional_subordination (which denies the sovereignty claim). The three readings share the same kernel but instantiate different constraints with different ε, different victim/beneficiary structures, and different type classifications. This reading's ε (0.62) is substantially higher than compact_federalism's expected ε (~0.35, coordination-heavy) and constitutional_subordination's expected ε (~0.15, federal supremacy). The network edges capture downstream constraints whose operation this reading structurally conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_sovereignty_boundary__resource_sovereignty_primacy, institutional, 0.15).
constraint_indexing:directionality_override(provincial_sovereignty_boundary__resource_sovereignty_primacy, organized, 0.75).
constraint_indexing:directionality_override(provincial_sovereignty_boundary__resource_sovereignty_primacy, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
