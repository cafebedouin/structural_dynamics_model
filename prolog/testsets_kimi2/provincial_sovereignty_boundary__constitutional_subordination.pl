% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__constitutional_subordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__constitutional_subordination, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: provincial_sovereignty_boundary__constitutional_subordination
 *   human_readable: Provincial Sovereignty Boundary â Constitutional Subordination Reading
 *   domain: political/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the constitutional_subordination
 *   reading of the provincial_sovereignty_boundary kernel in Canadian
 *   federalism. Under this reading, provinces are constitutionally
 *   subordinate creatures of the federal constitutional order, possessing no
 *   inherent or residual sovereignty, and any exit from the federation
 *   requires federal consent under the constitutional amendment formula. The
 *   reading vindicates federal authority over equalization, climate policy,
 *   and national standards, while treating separatist claims as
 *   constitutional nullities. It is contested by compact federalism
 *   (provinces as sovereign parties to a compact) and resource sovereignty
 *   primacy (provincial resource ownership as territorial sovereignty). The
 *   authored metrics describe an actively enforced constitutional arrangement
 *   that coordinates a continental federation while asymmetrically extracting
 *   jurisdictional autonomy from provincial governments, particularly
 *   resource-rich provinces whose effective control is overridden by federal
 *   climate and fiscal policy.
 *
 * KEY AGENTS:
 *   - federal_crown: Primary beneficiary/agenda_setter (institutional/constrained) â holds veto over provincial exit and claims legitimacy for federal paramountcy
 *   - provincial_governments: Primary target (organized/constrained) â bear the loss of inherent sovereignty and jurisdictional autonomy
 *   - resource_rich_provinces: Intensified target (organized/identity_locked) â face federal override of resource and climate jurisdiction; identity fused with resource stewardship within Canada
 *   - separatist_movements: Excluded voice (organized/trapped) â assert inherent sovereignty but are constitutional nullities under this reading
 *   - constitutional_courts: Analytical observer (institutional/analytical) â interpret and enforce the constitutional subordination doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.76).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.84).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.43).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.76).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.43).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Provincial Sovereignty Boundary â Constitutional Subordination Reading").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, 'c582338b-979e-450f-ac24-2726923cc185').
narrative_ontology:cs_kernel_codification('c582338b-979e-450f-ac24-2726923cc185', formalized).
narrative_ontology:cs_authority_grounding('c582338b-979e-450f-ac24-2726923cc185', lineage).
narrative_ontology:cs_interpretation_layer_present('c582338b-979e-450f-ac24-2726923cc185').
narrative_ontology:cs_reading_relation('c582338b-979e-450f-ac24-2726923cc185', provincial_sovereignty_boundary__compact_federalism, forecloses).
narrative_ontology:cs_reading_relation('c582338b-979e-450f-ac24-2726923cc185', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('c582338b-979e-450f-ac24-2726923cc185', foundational, no_inherent_provincial_sovereignty).
narrative_ontology:cs_axiom_status(no_inherent_provincial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('c582338b-979e-450f-ac24-2726923cc185', no_inherent_provincial_sovereignty, conventional).
narrative_ontology:cs_axiom('c582338b-979e-450f-ac24-2726923cc185', foundational, federal_consent_required_for_provincial_exit).
narrative_ontology:cs_axiom_status(federal_consent_required_for_provincial_exit, holdable).
narrative_ontology:cs_axiom_grounding('c582338b-979e-450f-ac24-2726923cc185', federal_consent_required_for_provincial_exit, conventional).
narrative_ontology:cs_reference_frame('c582338b-979e-450f-ac24-2726923cc185', federal_supremacy_framework).
narrative_ontology:cs_drift_state('c582338b-979e-450f-ac24-2726923cc185', contemporary_fiscal_climate_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c582338b-979e-450f-ac24-2726923cc185', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_crown).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, provincial_governments).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the constitutional framework that subordinates provinces to federal authority; holds the veto over provincial exit through the constitutional amendment formula and claims legitimacy for federal paramountcy over equalization, climate policy, and national standards.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_crown, agenda_setter,
    institutional, generational, constrained, national).

% Exercise delegated jurisdiction under the federal constitution but lack inherent sovereignty; their autonomy is overridden by federal spending power and paramountcy; exit from the federation is legally barred without federal consent.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, provincial_governments, payer,
    organized, generational, constrained, national).

% Hold provincial resource ownership under s.92A yet face federal climate and equalization policy that overrides effective control; political identity is fused with resource stewardship within Canada, making sovereignty claims psychologically costly even where constitutionally futile.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces, payer,
    organized, generational, identity_locked, regional).

% Assert inherent provincial sovereignty and seek exit via referendum or unilateral declaration; treated as constitutional nullities under the federal-subordination reading and excluded from legitimate constitutional negotiation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements, excluded,
    organized, generational, trapped, regional).

% Interpret the constitutional division of powers and the amendment formula; their rulings on provincial sovereignty reference the federal constitutional framework and the peace, order and good government doctrine, reinforcing subordination.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__constitutional_subordination, federal_crown).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__constitutional_subordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds together a geographically vast, economically and linguistically diverse federation by establishing a supreme federal legal order, preventing provincial balkanization, and enabling national standards for trade, currency, defense, and inter-regional redistribution.
% TRANSFER_FUNCTION: Transfers jurisdictional autonomy, resource rent control, and unilateral exit capacity from provincial governments to federal institutions, in exchange for federal transfer payments, shared services, and the security of the federation.
% ABSENT_VOICES: Separatist movements (especially Quebec sovereignty and western separatist currents) and some Indigenous nations asserting inherent sovereignty are structurally excluded; they would reject the premise that provinces are mere creatures of federal law, but are treated as constitutional nullities. Resource-rich provincial electorates that deny federal climate authority are also partially excluded from constitutional recognition.
% DISAPPEARANCE_RATIONALE: If the constitutional subordination doctrine vanished, provinces would assert inherent or residual sovereignty, the amendment formula and federal veto over exit would lose legitimacy, and the federation would face dissolution or radical reconstitution as a looser confederation or multiple states. Federal equalization and climate mandates would immediately lack constitutional footing.
% FOUNDING_PROBLEM: Preventing the fragmentation of British North America into competing jurisdictions, ensuring a unified economic and military space, and managing deep linguistic and cultural divisions after 1867.
% FOUNDING_PROBLEM_CORROBORATION: Federal Crown and judicial decisions (Reference re Secession of Quebec) attest the subordination reading as necessary for national unity. Provincial governments, sovereigntist movements, and constitutional historians advancing compact theory attest the problem is misstated and the federation was a negotiated compact, not a unilateral federal creation. Independent constitutional scholars outside both camps are divided; the judicial record strongly favors subordination, while historical and political-science scholarship increasingly emphasizes the negotiated and conditional origins of confederation.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__constitutional_subordination, 0.76, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.76) is high because the constraint strips provinces of sovereignty and exit autonomy, transferring jurisdictional rents to federal institutions. Suppression (0.84) is higher because the arrangement persists through constitutional entrenchment, judicial enforcement, and the explicit requirement of federal consent for exit, which actively suppresses provincial secession and nullifies competing sovereignty claims. Theater ratio (0.43) reflects the growing performative quality of federal-provincial consultations and cooperative federalism rituals that mask underlying subordination. Accessibility collapse (0.72) is high: once the constitutional framework is accepted, legal alternatives to federal subordination (unilateral exit, nullification) collapse because they are ruled unconstitutional. Resistance (0.62) is moderate-to-high: provincial governments, especially resource-rich and nationalist ones, actively resist through litigation, political mobilization, and policy non-compliance, but remain structurally constrained.
 *
 * PERSPECTIVAL GAP:
 *   The federal crown experiences this constraint as necessary coordination â the doctrine that prevents balkanization and secures national standards. Provincial governments, especially those with resource or nationalist identities, experience the same structure as extraction of their democratic and jurisdictional autonomy. The engine computes this divergence from the structural asymmetry in beneficiary/victim declarations and exit options: the federal crown has agenda-setting power and collects authority, while provinces face identity-locked or constrained exit and bear the sovereignty loss.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal_crown is the structural beneficiary (low directionality, authority accrues to it). Provincial_governments and resource_rich_provinces are structural targets (high directionality, sovereignty is extracted from them). Resource_rich_provinces sit nearer the full-target end than general provincial governments because their political identity is fused with resource control, making federal climate and equalization override a deeper extraction. Separatist_movements are excluded entirely â their voice is not coordinated but suppressed. Constitutional_courts are observers with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled_rope classification, this arrangement could be misread as pure coordination (a rope holding the federation together) or pure extraction (a snare of federal imperialism). The tangled_rope classification is warranted because the constraint genuinely coordinates a continental polity â providing currency union, internal trade, and national defense â while simultaneously and through the same structure extracting jurisdictional autonomy from provinces. The coordination and extraction are not separable: the federal veto over exit is the enforcement mechanism for both the coordination benefit and the extraction cost. Mandatrophy would occur if the coordination function (federation) were used to justify extraction long after the coordination need became obsolete; current measurements show extraction rising, suggesting potential future mandatrophy, but not yet resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_origin_compact_vs_statute,
    'Was the Canadian federation founded as a negotiated compact among sovereign colonies or as a unilateral federal statute?',
    'Comparative historical analysis of the 1864 Quebec Conference, Colonial Office records, and the 1982 patriation negotiations.',
    'If compact theory is historically accurate, the constitutional_subordination reading is a later judicial construction and the constraint''s naturalness collapses, supporting reclassification toward snare or tangled_rope with higher theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_origin_compact_vs_statute, empirical, 'Ambiguity over whether confederation was a compact or a federal statute.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is provincial compliance driven by constitutional enforcement (courts, federal spending power) or by internalized Canadian federal identity?',
    'Post-devolution or post-referendum behavior: if provinces continue to accept federal paramountcy even when fiscal levers are removed, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, conceptual, 'Structural versus internalized suppression mechanism.').

omega_variable(
    resource_sovereignty_absolute_or_delegated,
    'Does provincial ownership of natural resources under s.92A Constitution Act 1982 constitute absolute sovereignty or merely delegated authority subordinate to federal climate and trade powers?',
    'Judicial review of federal carbon pricing and environmental assessment laws; provincial refusals and federal responses.',
    'If absolute, resource_sovereignty_primacy reading strengthens and this reading''s extraction metric rises; if delegated, this reading''s authority is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_sovereignty_absolute_or_delegated, conceptual, 'Whether provincial resource ownership is absolute or delegated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psb_cs_tr_t0, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0, 0.2).
narrative_ontology:measurement(psb_cs_tr_t5, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 5, 0.22).
narrative_ontology:measurement(psb_cs_tr_t10, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 10, 0.25).
narrative_ontology:measurement(psb_cs_tr_t15, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 15, 0.28).
narrative_ontology:measurement(psb_cs_tr_t20, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 20, 0.31).
narrative_ontology:measurement(psb_cs_tr_t25, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 25, 0.34).
narrative_ontology:measurement(psb_cs_tr_t30, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 30, 0.37).
narrative_ontology:measurement(psb_cs_tr_t35, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 35, 0.4).
narrative_ontology:measurement(psb_cs_tr_t40, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 40, 0.43).

% Extraction over time
narrative_ontology:measurement(psb_cs_be_t0, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(psb_cs_be_t5, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(psb_cs_be_t10, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(psb_cs_be_t15, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(psb_cs_be_t20, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(psb_cs_be_t25, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(psb_cs_be_t30, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(psb_cs_be_t35, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 35, 0.73).
narrative_ontology:measurement(psb_cs_be_t40, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 40, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(psb_cs_su_t0, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(psb_cs_su_t5, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(psb_cs_su_t10, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(psb_cs_su_t15, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(psb_cs_su_t20, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(psb_cs_su_t25, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 25, 0.74).
narrative_ontology:measurement(psb_cs_su_t30, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(psb_cs_su_t35, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 35, 0.81).
narrative_ontology:measurement(psb_cs_su_t40, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 40, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, compact_federalism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, resource_sovereignty_primacy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the provincial_sovereignty_boundary kernel. Its siblings instantiate compact_federalism and resource_sovereignty_primacy readings. The epsilon values differ across the family because each reading makes a structurally distinct claim about the locus of sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
