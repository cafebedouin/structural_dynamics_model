% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__regulatory_takings_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine: Value Diminution as Compensable Taking
 *   domain: constitutional/legal
 *
 * SUMMARY:
 *   The regulatory takings reading of the Takings Clause expands Fifth
 *   Amendment protection beyond direct physical appropriation to include
 *   regulations that diminish property value 'too far.' A landowner subject
 *   to a wetland preservation order, endangered species protection mandate,
 *   or zoning restriction that reduces their land's economic value can seek
 *   compensation under this reading by arguing the regulation constitutes a
 *   'taking' of their property right to economic use. This reading introduces
 *   an ad hoc balancing test (the Penn Central factors) that creates
 *   uncertainty in regulatory space but provides powerful protection against
 *   non-physical extraction of property value. The constraint creates a
 *   coordination problem: property owners gain predictability (regulations
 *   diminishing value trigger compensation), but regulatory agencies face
 *   fiscal and political barriers to aggressive environmental or public
 *   health measures. The reading is one of three structurally distinct
 *   interpretations of the takings clause kernel, each with different victim
 *   sets and extraction profiles.
 *
 * KEY AGENTS:
 *   - property_owners_with_diminished_value: Beneficiaries under this reading; their constraint is the duty to prove 'too much' diminution
 *   - regulatory_agencies: Institutional payers; bear compensation obligations and face reduced regulatory capacity
 *   - environmental_conservation_mandates: Non-agent policy objective; constrained by compensation liability
 *   - supreme_court: Agenda-setter; administers the Penn Central balancing test and determines what counts as a taking
 *   - future_regulatory_beneficiaries: Structurally excluded; have no standing in takings disputes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.68).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.52).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine: Value Diminution as Compensable Taking").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional/legal").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, 'ec412e6b-89d1-4aa0-a7f2-1f726fac9de2').
narrative_ontology:cs_kernel_codification('ec412e6b-89d1-4aa0-a7f2-1f726fac9de2', fixed_text).
narrative_ontology:cs_authority_grounding('ec412e6b-89d1-4aa0-a7f2-1f726fac9de2', lineage).
narrative_ontology:cs_interpretation_layer_present('ec412e6b-89d1-4aa0-a7f2-1f726fac9de2').
narrative_ontology:cs_reading_relation('ec412e6b-89d1-4aa0-a7f2-1f726fac9de2', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec412e6b-89d1-4aa0-a7f2-1f726fac9de2', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('ec412e6b-89d1-4aa0-a7f2-1f726fac9de2', foundational, value_diminution_compensable).
narrative_ontology:cs_axiom_status(value_diminution_compensable, holdable).
narrative_ontology:cs_axiom_grounding('ec412e6b-89d1-4aa0-a7f2-1f726fac9de2', value_diminution_compensable, deontological).
narrative_ontology:cs_axiom('ec412e6b-89d1-4aa0-a7f2-1f726fac9de2', foundational, regulatory_scope_requires_balancing).
narrative_ontology:cs_axiom_status(regulatory_scope_requires_balancing, holdable).
narrative_ontology:cs_axiom_grounding('ec412e6b-89d1-4aa0-a7f2-1f726fac9de2', regulatory_scope_requires_balancing, conventional).
narrative_ontology:cs_reference_frame('ec412e6b-89d1-4aa0-a7f2-1f726fac9de2', takings_clause_property_protection).
narrative_ontology:cs_drift_state('ec412e6b-89d1-4aa0-a7f2-1f726fac9de2', contemporary_environmental_regulatory_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ec412e6b-89d1-4aa0-a7f2-1f726fac9de2', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_owners_with_diminished_value).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, constitutional_property_protection_jurisprudence).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, regulatory_agencies).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, environmental_conservation_mandates).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, public_health_regulators).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, land_use_planning_authorities).
narrative_ontology:constraint_vindicates(takings_clause_boundary__regulatory_takings_reading, constitutional_property_rights_doctrine).
narrative_ontology:constraint_vindicates(takings_clause_boundary__regulatory_takings_reading, takings_clause_original_meaning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Landowners and real estate developers whose property values decline substantially due to new environmental regulations, zoning restrictions, or conservation mandates. Under this reading, they gain the right to seek compensation when regulations reduce their property's economic value 'too far.' Their leverage lies in litigation capacity and political organization; their constraint is that quantifying 'too much' diminution triggers uncertainty in regulatory enforcement.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_owners_with_diminished_value, beneficiary,
    powerful, biographical, constrained, national).

% Environmental Protection Agency, state environmental departments, zoning boards, conservation authorities. Bear the cost of potential compensation claims whenever regulations significantly reduce property values. Must conduct pre-regulatory takings analysis and may face retroactive claims. Their constraint is the duty to compensate; their exit option (lighter regulation) conflicts with their public mandate.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, regulatory_agencies, payer,
    institutional, generational, constrained, national).

% The set of environmental protection goals (wetland preservation, endangered species habitat, climate mitigation, air/water quality). These are constrained by the takings doctrine: regulations that would most effectively achieve these mandates may trigger compensation obligations, creating a fiscal drag on environmental policy. Listed as non-agent because it is a policy objective, not a decision-making entity.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, environmental_conservation_mandates, payer,
    moderate, civilizational, constrained, national).
narrative_ontology:stakeholder_non_agent(takings_clause_boundary__regulatory_takings_reading, environmental_conservation_mandates).

% Health departments, occupational safety agencies. When health regulations reduce property values (e.g., restrictions on industrial use in residential areas, lead abatement mandates), they face takings claims. Their regulatory tools are constrained by the compensation requirement.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, public_health_regulators, payer,
    institutional, generational, constrained, national).

% City planners and zoning boards that restrict development to preserve community character, open space, or prevent sprawl. Broad takings exposure means every restrictive zoning decision risks compensation litigation, raising the fiscal and political cost of planning restrictions.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, land_use_planning_authorities, payer,
    institutional, generational, constrained, local).

% Communities and individuals who would benefit from aggressive environmental or public health regulation but are not represented in takings litigation. They lack legal standing to defend regulations against takings claims and are structurally absent from the compensation calculus.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, future_regulatory_claimants, excluded,
    powerless, generational, trapped, national).

% Tenants and lower-income residents whose housing costs rise when regulations increase property owner costs (passed through as higher rents) or when regulations reduce housing supply by limiting development. They bear the cost of compensation obligations without having standing to participate in takings disputes.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, lower_income_renters, excluded,
    powerless, biographical, trapped, national).

% Sets the takings doctrine's boundaries through decisions (Penn Central balancing, Lucas categorical rule, regulatory takings frame). Administers the interpretive process by which property diminution claims are evaluated. Controls which harms count as takings.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Law professors and legal scholars who produce competing constitutional theories. Generate the framings under which different readings of the takings clause appear coherent. Provide the epistemic legitimacy for rival readings.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, constitutional_law_academia, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__regulatory_takings_reading, property_owners_with_diminished_value).
narrative_ontology:fixing_cost_class(takings_clause_boundary__regulatory_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a predictable, judicially enforceable boundary for property rights: signals to landowners that regulations diminishing value beyond a certain threshold trigger compensation, thus stabilizing investment expectations and reducing uncertainty about the regulatory environment's taking of property without due process.
% TRANSFER_FUNCTION: Moves fiscal obligations from property owners to regulatory agencies (and ultimately taxpayers) whenever regulations reduce property value substantially. The constraint transfers the risk of value loss from the regulated property owner to the regulatory entity.
% ABSENT_VOICES: Future beneficiaries of regulation (communities, ecosystems, future generations) are structurally excluded: they have no legal standing in takings cases and receive no compensation when regulations fail to materialize due to takings liability. Lower-income renters and non-property holders affected by reduced development or higher rents are similarly absent from the doctrine's accounting.
% DISAPPEARANCE_RATIONALE: If the regulatory takings reading disappeared and only physical appropriation triggered compensation, regulatory agencies could implement broad environmental and public health restrictions without fiscal barriers. Property values would adjust to reflect the regulatory constraint as a permanent feature, not a compensable harm. The real estate and development industries would reorganize around tighter regulation; environmental policy would accelerate; compensation costs would collapse.
% FOUNDING_PROBLEM: Property owners need protection against government appropriation of their land or severe diminution of its value without due process or compensation. The Fifth Amendment's Takings Clause was drafted to prevent government from acting as a unilateral taker.
% FOUNDING_PROBLEM_CORROBORATION: Property rights advocates and real estate interests attest the protection is live and essential, citing expanding regulatory scope. Environmental advocates, public health officials, and regulatory scholars attest the founding problem has shifted: modern takings doctrine protects property interests against legitimate public regulation, not against appropriation. Comparative constitutional analysis (Canada, Australia, European jurisdictions with narrower takings doctrines) shows the problem is contestable — property protection can be achieved without a broad regulatory takings reading.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__regulatory_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__regulatory_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the regulatory takings reading transfers substantial fiscal obligations from property owners to the public sector and regulatory agencies. The transfer is asymmetric: property owners gain compensation protection while environmental and public health mandates lose enforcement capacity. Suppression is moderate (0.52) because the doctrine must be actively enforced through litigation to remain credible; regulatory agencies suppress broader measures to avoid takings claims. Theater ratio rises from 0.25 to 0.41 over the interval, indicating that enforcement activities increasingly focus on the balancing test's performative application rather than substantive property protection — agencies spend resources on takings analysis documents that influence few final regulatory decisions. The measurements are shared on one grid: every metric is authored at every examined time point. The plateau at t=30 reflects the doctrine's stabilization after major Supreme Court precedents established the Penn Central framework; extractiveness and suppression remain elevated rather than declining because the doctrine persists through litigation rather than normative acceptance. Resistance remains high (0.72) because environmental and regulatory advocates actively oppose the doctrine through litigation, legislation, and academic critique.
 *
 * PERSPECTIVAL GAP:
 *   The property owner and regulatory agency seats should compute very differently. From the owner's position, the constraint provides protection against arbitrary diminution of value — a coordination benefit clarifying what regulations they must accept versus what triggers compensation. From the regulatory agency's seat, the same structure operates as an extractive constraint: they must compensate for regulatory choices that serve public goods (wetland preservation, species protection) that benefit uncompensated future generations. The engine computes this divergence from structural data — the property owner has constrained exit (they own the land) and gains compensation certainty; the regulatory agency has constrained exit (they must regulate) and bears fiscal costs. The widening theater ratio reflects increasing reliance on balancing-test procedure rather than substantive coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners benefiting from compensation protection have directionality near 0.3 (beneficiary position: the constraint subsidizes their investment expectations by guaranteeing compensation if regulation diminishes value). Regulatory agencies and environmental mandates have directionality near 0.8 (target positions: they bear the fiscal and capacity costs of the compensation obligation). The Supreme Court as agenda-setter is approximately symmetric (d ≈ 0.5) — it benefits from the legitimacy the doctrine provides but must manage the feedback from property owners and environmental advocates. Future regulatory beneficiaries, excluded from the constraint's accounting, cannot be assigned directionality within the framework because they have no declared relationship to it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's classification as tangled_rope requires both genuine coordination (protecting property rights against arbitrary taking) and asymmetric extraction (fiscal transfer from regulators to property owners). The mandatrophy question is whether the founding coordination function — preventing government from acting as a unilateral taker — has survived. Environmental advocates argue the function has died: the regulatory takings reading protects property from legitimate regulation, not from appropriation. Property rights advocates argue it is still live: expanding regulation requires a compensation mechanism to remain legitimate. The doctrine persists through litigation and constitutional theory rather than through participant acceptance, suggesting theaters-ratio rise. The constrained exit of both property owners (they own regulated land) and regulatory agencies (they must regulate) prevents resolution through renegotiation. If the founding problem (arbitrary taking without due process) is indeed dead while the compensation obligation persists, this would indicate inertial piton dynamics; however, the active resistance (0.72) and ongoing litigation suggest the constraint is still contested enough to remain tangled_rope rather than devolving to piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    value_diminution_threshold_ambiguity,
    'What degree of property value diminution constitutes ''too far'' and triggers takings compensation? Is there a coherent threshold or does it depend entirely on ad hoc balancing?',
    'Empirical analysis of Penn Central balancing outcomes: do courts apply consistent threshold logic, or does the outcome turn on case-specific facts that resist generalization? Comparative jurisprudence examining how other property regimes handle value diminution.',
    'If a coherent threshold emerges, property owners gain predictability (extractiveness drops, suppression may rise as regulation becomes clearer). If balancing is genuinely ad hoc, regulatory agencies face irreducible uncertainty and extractiveness of the compensation obligation increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(value_diminution_threshold_ambiguity, empirical, 'Whether the regulatory takings doctrine operates through predictable threshold or irreducible case-by-case balancing').

omega_variable(
    coordination_vs_extraction_reading_boundary,
    'Is the regulatory takings reading best understood as protecting property owners from arbitrary state action (coordination function), or does it primarily extract from public regulation to benefit property owners (extraction function)? Can both framings coexist?',
    'Genealogical analysis: trace how the doctrine was adopted (genuine protection against arbitrary taking vs. judicial expansion protecting concentrated interests). Structural analysis: examine whether the doctrine prevents arbitrary taking or primarily protects established property interests against regulation serving new public goods (environmental protection, climate mitigation).',
    'If coordination function is primary, the tangled_rope classification holds (both coordination and extraction). If extraction dominates, reclassify toward snare. If the doctrine represents a shift in what counts as ''arbitrary'' (incorporating environmental values), the classification may change across readings of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_extraction_reading_boundary, conceptual, 'Whether the regulatory takings reading coordinates property protection or extracts from environmental/public health regulation').

omega_variable(
    takings_reading_sibling_foreclosure,
    'Do the three readings of the takings_clause_boundary kernel (physical_appropriation, categorical, regulatory) logically foreclose each other, or do they coexist as live positions within a single constitutional framework?',
    'Constitutional theory analysis: examine whether the readings are incompatible core premises or different interpretations of a shared constitutional commitment. Institutional analysis: observe that multiple readings coexist within the Court''s composition and justify themselves through different constitutional methodologies.',
    'If readings foreclose each other, they are distinct constraints, not sibling readings. If they coexist, the kernel context correctly identifies them as alternative live positions within constitutional interpretation. This affects how the corpus models the takings clause: as a settled rule or as an ongoing interpretive contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(takings_reading_sibling_foreclosure, conceptual, 'Logical and institutional status of sibling readings within the takings clause kernel').

omega_variable(
    regulatory_takings_temporal_shift,
    'Has the regulatory takings doctrine''s intensity and breadth changed over time? Do the measurements reflect genuine escalation (rising extractiveness, theater ratio) or stable application of settled doctrine?',
    'Empirical analysis of takings litigation outcomes: frequency of successful claims, size of compensation awarded, breadth of regulations challenged. Temporal comparison of Supreme Court doctrine statements and lower court applications.',
    'If rising metrics reflect genuine escalation, the doctrine is externalizing increasing costs to regulatory capacity (snare-adjacent dynamics). If stable, the plateau in measurements (t=30 onward) indicates the doctrine reached equilibrium. The theater-ratio rise suggests increasing reliance on procedural balancing rather than substantive coordination — if this trend continues, piton dynamics may emerge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_takings_temporal_shift, empirical, 'Whether regulatory takings doctrine intensity is escalating, stable, or declining over the period examined').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(taki_tr_t0, observed).
narrative_ontology:measurement(taki_tr_t5, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(taki_tr_t5, observed).
narrative_ontology:measurement(taki_tr_t10, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(taki_tr_t10, observed).
narrative_ontology:measurement(taki_tr_t15, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(taki_tr_t15, observed).
narrative_ontology:measurement(taki_tr_t20, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(taki_tr_t20, observed).
narrative_ontology:measurement(taki_tr_t25, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(taki_tr_t25, observed).
narrative_ontology:measurement(taki_tr_t30, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(taki_tr_t30, observed).
narrative_ontology:measurement(taki_tr_t40, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(taki_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(taki_be_t0, observed).
narrative_ontology:measurement(taki_be_t5, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement_basis(taki_be_t5, observed).
narrative_ontology:measurement(taki_be_t10, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(taki_be_t10, observed).
narrative_ontology:measurement(taki_be_t15, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(taki_be_t15, observed).
narrative_ontology:measurement(taki_be_t20, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(taki_be_t20, observed).
narrative_ontology:measurement(taki_be_t25, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(taki_be_t25, observed).
narrative_ontology:measurement(taki_be_t30, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(taki_be_t30, observed).
narrative_ontology:measurement(taki_be_t40, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(taki_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(taki_su_t0, observed).
narrative_ontology:measurement(taki_su_t5, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(taki_su_t5, observed).
narrative_ontology:measurement(taki_su_t10, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(taki_su_t10, observed).
narrative_ontology:measurement(taki_su_t15, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 15, 0.46).
narrative_ontology:measurement_basis(taki_su_t15, observed).
narrative_ontology:measurement(taki_su_t20, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(taki_su_t20, observed).
narrative_ontology:measurement(taki_su_t25, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(taki_su_t25, observed).
narrative_ontology:measurement(taki_su_t30, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(taki_su_t30, observed).
narrative_ontology:measurement(taki_su_t40, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(taki_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__regulatory_takings_reading, 0.18).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__categorical_takings_reading).

% DUAL FORMULATION NOTE:
% The takings_clause_boundary kernel instantiates three structurally distinct constraint stories, each representing a different reading of the Fifth Amendment's Takings Clause. The regulatory_takings_reading (this story) expands compensation obligations to non-physical value diminution via ad hoc balancing. The physical_appropriation_reading restricts compensation to direct seizure. The categorical_takings_reading uses categorical rules for permanent occupation and total value elimination. All three coexist as live interpretive positions held by different justices, scholars, and lower courts. Each reading produces different victim sets, beneficiary structures, and extraction profiles. The regulatory_takings_reading is downstream of both siblings in the sense that it builds on the takings clause's legitimacy and extends its scope; however, it does not logically foreclose either sibling — all three remain plausible readings of the shared constitutional text. Link all three stories via this network field to enable the corpus to track the constraint family's contested nature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(takings_clause_boundary__regulatory_takings_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
