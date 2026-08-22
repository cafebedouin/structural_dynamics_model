% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__grievance_threshold_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__grievance_threshold_reading
 *   human_readable: Secession Legitimacy via Grievance Threshold
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint instantiates the 'grievance threshold' reading of the
 *   secession legitimacy kernel: a province's right to secede becomes
 *   legitimate when federal actions cross an objective threshold of
 *   structural injustice (systematic resource extraction, legislative
 *   override, demographic marginalization), regardless of constitutional text
 *   that prohibits unilateral secession. The reading treats the federation as
 *   a conditional contract, not a perpetual suicide pact. It is advanced by
 *   Quebec sovereigntists, some Western Canadian alienation movements, and
 *   international legal scholars of remedial secession. The constraint is
 *   transitional (scaffold) because its justification is the remedy for a
 *   specific breach; once the breach is remedied (by negotiation, reform, or
 *   separation), the threshold claim's work is done.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.15).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.45).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, scaffold).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Secession Legitimacy via Grievance Threshold").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:has_sunset_clause(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, 'bb0510f6-70a0-426e-b7ff-69a151a75a0b').
narrative_ontology:cs_kernel_codification('bb0510f6-70a0-426e-b7ff-69a151a75a0b', distributed).
narrative_ontology:cs_authority_grounding('bb0510f6-70a0-426e-b7ff-69a151a75a0b', extraction).
narrative_ontology:cs_interpretation_layer_present('bb0510f6-70a0-426e-b7ff-69a151a75a0b').
narrative_ontology:cs_reading_relation('bb0510f6-70a0-426e-b7ff-69a151a75a0b', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('bb0510f6-70a0-426e-b7ff-69a151a75a0b', secession_legitimacy_boundary__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb0510f6-70a0-426e-b7ff-69a151a75a0b', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('bb0510f6-70a0-426e-b7ff-69a151a75a0b', foundational, federal_breach_triggers_remedial_exit).
narrative_ontology:cs_axiom_status(federal_breach_triggers_remedial_exit, holdable).
narrative_ontology:cs_axiom_grounding('bb0510f6-70a0-426e-b7ff-69a151a75a0b', federal_breach_triggers_remedial_exit, deontological).
narrative_ontology:cs_axiom('bb0510f6-70a0-426e-b7ff-69a151a75a0b', foundational, threshold_burden_of_proof_on_claimant).
narrative_ontology:cs_axiom_status(threshold_burden_of_proof_on_claimant, holdable).
narrative_ontology:cs_axiom_grounding('bb0510f6-70a0-426e-b7ff-69a151a75a0b', threshold_burden_of_proof_on_claimant, empirically_contingent).
narrative_ontology:cs_reference_frame('bb0510f6-70a0-426e-b7ff-69a151a75a0b', conditional_federal_contract).
narrative_ontology:cs_drift_state('bb0510f6-70a0-426e-b7ff-69a151a75a0b', post_clarity_act_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bb0510f6-70a0-426e-b7ff-69a151a75a0b', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, grieved_province_population).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, regional_elites).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, federal_state_institutions).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, interprovincial_transfer_recipients).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, constitutional_order_stakeholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Population of a province experiencing sustained federal resource extraction, legislative override of provincial jurisdiction, and demographic marginalization. They claim the federation no longer serves their interests and that secession is a remedial right. Their exit is constrained by economic integration, family ties, and the federal government's refusal to recognize a unilateral path.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, grieved_province_population, beneficiary,
    organized, generational, constrained, regional).

% Political and economic leaders within the aggrieved province who articulate the grievance threshold, mobilize referendums, and negotiate with the federal state. They benefit from enhanced regional autonomy and resource control if the threshold is recognized, but also bear the risk of federal retaliation. Their personal exit options (capital, professional networks) are more mobile than the population they represent.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, regional_elites, agenda_setter,
    powerful, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__grievance_threshold_reading, regional_elites, beneficiary).

% The federal executive, legislature, and judiciary that bear the costs of secession threats: constitutional instability, economic uncertainty, and the need to concede asymmetric powers or transfers to maintain unity. They are trapped in the sense that the federation's existence depends on managing this constraint; they cannot 'exit' the federation they embody.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_state_institutions, payer,
    institutional, generational, trapped, national).

% Provinces and populations that receive net fiscal transfers from the federation. They bear the cost of concessionary federalism (enhanced transfers, opt-outs) granted to the aggrieved province to prevent secession. Their exit is constrained because they are bound by the same fiscal union; they cannot opt out of the transfer system without dissolving the federation.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, interprovincial_transfer_recipients, payer,
    organized, generational, constrained, national).

% Legal scholars, federalist civil society, and institutions that view the constitutional order as a collective asset. They experience the grievance-threshold claim as a structural threat to the rule of law and the permanence of the union. Their identity is fused with the constitutional project; exit from this identity would mean abandoning their professional and civic self-conception.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, constitutional_order_stakeholders, payer,
    moderate, civilizational, identity_locked, national).

% Indigenous nations whose treaty relationships are with the Crown (federal), not the province. They are excluded from provincial secession deliberations despite having prior sovereignty and treaty rights that would be unilaterally altered by provincial exit. Their exit is trapped because their territorial and legal standing is bound to the federal treaty relationship.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, indigenous_treaty_nations, excluded,
    organized, generational, trapped, regional).

% The judicial branch that adjudicates the legal parameters of the grievance threshold (e.g., clarity of question, clear majority, duty to negotiate). They observe the structural tension but do not bear its costs or collect its benefits directly; their role is to interpret the kernel's legal instantiation.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, supreme_court_observers, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, law-governed pathway for a province to exit the federation when the federal partner has persistently violated the terms of the union, preventing violent rupture and legitimating negotiated separation.
% TRANSFER_FUNCTION: Transfers sovereignty over territory, resources, and population from the federal state to the seceding province, conditional on meeting an objective threshold of demonstrated federal injustice. The transfer moves fiscal flows, regulatory authority, and international legal personality.
% ABSENT_VOICES: Indigenous treaty nations whose prior sovereignty and federal treaty relationships are structurally excluded from provincial secession decisions; future generations in both the seceding and remaining provinces who inherit the consequences without voice; federal public servants whose institutional continuity is disrupted but who have no seat at the negotiation.
% DISAPPEARANCE_RATIONALE: If the grievance-threshold reading vanished overnight, the federation would lose its only law-governed remedial exit valve. Provinces with deep grievances would face a binary choice: accept permanent subordination or pursue extra-constitutional rupture. The federal state would lose the pressure valve that forces concessionary negotiation. The constitutional order would become more brittle, not more stable.
% FOUNDING_PROBLEM: The federation was built on an implicit bargain: provincial autonomy in exchange for shared sovereignty. When the federal partner systematically extracts resources, overrides provincial jurisdiction, and marginalizes a province's demographic weight, the bargain is broken. The founding problem is how to recognize that breach without dissolving the entire constitutional order into anarchy.
% FOUNDING_PROBLEM_CORROBORATION: The 1998 Supreme Court Reference on Quebec Secession (paragraphs 126-154) articulates the remedial right to secession as a corollary of the underlying constitutional principles (federalism, democracy, constitutionalism, rule of law, minority rights) — an external judicial corroboration. Federalist scholars (e.g., Peter Russell, 'Constitutional Odyssey') acknowledge the theoretical coherence of remedial secession while contesting its operationalization. The aggrieved province's own political leadership (e.g., the 1995 referendum question, the 2000 Clarity Act response) self-attests the grievance, but no disinterested international body has validated the specific threshold claim.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).
:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.15) because the constraint is not an active extraction mechanism but a conditional claim — it extracts nothing until the threshold is crossed and acted upon. Suppression is moderate (0.45) because the federal state actively resists the threshold's operationalization (Clarity Act, reference cases, refusal to negotiate pre-emptively) but does not use violence. Theater ratio (0.25) reflects that much of the federal 'dialogue' is performative — the Clarity Act creates procedural hurdles without engaging the substantive grievance. Accessibility collapse (0.6) is moderate: alternatives (asymmetric federalism, renewed federation) exist but narrow as grievances accumulate. Resistance (0.4) is significant but not existential: federal institutions resist the claim's legitimacy but engage its procedural instantiation.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (grieved province) experience this as a rope/scaffold — a genuine coordination mechanism for remedial justice. The payer seats (federal institutions, transfer recipients) experience it as a snare/tangled rope — an extraction threat backed by the mobilization capacity of a regional population. The victim seat (constitutional order) experiences it as a piton — a degrading constraint that persists theatrically because no one can afford to fix or remove it. The engine computes these divergences from the declared power, exit_options, and beneficiary/victim structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The grieved province population and regional elites are beneficiaries (d ≈ 0.2) — they gain a potential exit right. Regional elites have higher mobility (mobile exit) than the population (constrained). Federal state institutions are payers (d ≈ 0.8) — they bear the structural cost of the threat and the concessions. Interprovincial transfer recipients are payers (d ≈ 0.7) — they subsidize the concessions. Constitutional order stakeholders are victims (d ≈ 0.9, identity_locked) — their self-conception is bound to the union's permanence. Indigenous treaty nations are excluded (trapped) — their prior rights are structurally invisible to the provincial-federal binary. The Supreme Court is analytical (d = 0.5) — it adjudicates without collecting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (providing a lawful exit for structural injustice) remains live because the founding problem (federal breach of the federal bargain) is contested — the federal state denies the breach, the province asserts it. The constraint has not atrophied into a piton because the grievance is actively mobilized (referendums, political platforms, intergovernmental conflict). It has not become a snare because the threshold requires objective demonstration, not mere assertion. The sunset clause is the negotiation mandate: once the threshold is met, the duty to negotiate is the transitional mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_objectivity,
    'Can the ''threshold of structural injustice'' be operationalized as an objective standard, or does it inevitably collapse into the claimant''s subjective grievance?',
    'Comparative analysis of secession precedents (Kosovo, South Sudan, Quebec, Scotland, Catalonia) to identify whether international practice converges on measurable criteria (systematic discrimination, resource extraction ratios, legislative override frequency) or remains indeterminate.',
    'If objective, the constraint is a genuine scaffold with a determinate trigger; if subjective, it becomes a snare — a permanent threat that the federal state must appease without ever knowing what suffices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_objectivity, conceptual, 'Whether the grievance threshold is a determinate standard or an open-ended claim.').

omega_variable(
    federal_breach_measurement,
    'What counts as ''federal action crossing the threshold'' — and who measures it?',
    'The Supreme Court''s Reference attempted this (clear question, clear majority, duty to negotiate) but left the substantive threshold undefined. A future reference case or negotiated framework could specify metrics (e.g., fiscal extraction > X% of provincial GDP, federal legislative override of exclusive provincial jurisdiction > Y instances per decade).',
    'Without a measurement protocol, the constraint''s extractiveness is effectively higher (the federal state must over-concede to avoid the risk) and its suppression is higher (the federal state must police the boundary more aggressively).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_breach_measurement, empirical, 'The missing measurement protocol for the threshold condition.').

omega_variable(
    indigenous_exclusion_structural,
    'Is the exclusion of Indigenous treaty nations from the grievance-threshold calculus a contingent omission or a structural feature of the reading?',
    'Analyze whether any variant of the remedial secession doctrine in international law or Canadian practice incorporates prior treaty holders as necessary parties to the secession decision.',
    'If structural, the reading is a snare for Indigenous nations regardless of the threshold — their rights are violated by the constraint''s very architecture. If contingent, a corrected reading could include treaty nations as veto-holders or necessary negotiators.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_exclusion_structural, conceptual, 'Whether Indigenous exclusion is built into the grievance-threshold logic or is a remediable gap.').

omega_variable(
    committer_kernel_reading,
    'How does this reading''s structural relationship to the secession_legitimacy_boundary kernel differ from its siblings?',
    'Map the kernel''s four declared readings and their structural deltas: constitutional_impossibility (extraction=0, victim=none), grievance_threshold (extraction=conditional, victim=threshold-dependent), popular_sovereignty (extraction=democratic_will, victim=minorities), treaty_primacy (extraction=treaty_rights, victim=any_secession_without_consent). The engine should compute per-reading classifications from these deltas.',
    'If the kernel framework is valid, each reading should compute to a different constraint type from the same structural engine — demonstrating that the kernel is not one constraint but a family of structurally distinct constraints linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading, conceptual, 'Commitment-system framing: this constraint as one reading of a contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 1982, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t1982, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(sece_tr_t1990, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(sece_tr_t1995, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(sece_tr_t2000, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(sece_tr_t2010, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(sece_tr_t2020, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 2020, 0.24).
narrative_ontology:measurement(sece_tr_t2026, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 2026, 0.25).

% Extraction over time
narrative_ontology:measurement(sece_be_t1982, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 1982, 0.08).
narrative_ontology:measurement(sece_be_t1990, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 1990, 0.1).
narrative_ontology:measurement(sece_be_t1995, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 1995, 0.12).
narrative_ontology:measurement(sece_be_t2000, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(sece_be_t2010, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 2010, 0.13).
narrative_ontology:measurement(sece_be_t2020, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 2020, 0.14).
narrative_ontology:measurement(sece_be_t2026, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 2026, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t1982, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 1982, 0.25).
narrative_ontology:measurement(sece_su_t1990, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(sece_su_t1995, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(sece_su_t2000, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(sece_su_t2010, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 2010, 0.44).
narrative_ontology:measurement(sece_su_t2020, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 2020, 0.43).
narrative_ontology:measurement(sece_su_t2026, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__grievance_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__grievance_threshold_reading, 0.1).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__treaty_primacy_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, federal_clarity_act_enforcement).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, interprovincial_equalization_transfers).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, indigenous_treaty_implementation).

% DUAL FORMULATION NOTE:
% This constraint and its three siblings form the secession_legitimacy_boundary constraint family. All four share the kernel_id but instantiate different constraints with different ε, different beneficiary/victim structures, and different claimed types. The grievance_threshold_reading is downstream of the constitutional_impossibility_reading (which establishes the baseline prohibition) and upstream of the federal_clarity_act_enforcement (which operationalizes the federal response). The treaty_primacy_reading is a structural constraint on all three: no secession reading is legitimate without treaty holder consent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
