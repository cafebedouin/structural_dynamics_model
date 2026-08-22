% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__constitutional_impossibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__constitutional_impossibility_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__constitutional_impossibility_reading
 *   human_readable: Constitutional Prohibition on Unilateral Secession
 *   domain: political/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint instantiates the constitutional_impossibility_reading of
 *   the secession_legitimacy_boundary kernel. It treats the constitutional
 *   text as establishing an absolute prohibition on unilateral secession,
 *   permitting exit only through the constitutional amendment process.
 *   Federal territorial integrity is framed as a foundational constitutional
 *   value, and all alternative exit legitimations â popular sovereignty,
 *   grievance thresholds, and treaty primacy â are categorically rejected.
 *   From this reading's own perspective, there is no victim set because
 *   provincial separatist claims are themselves constitutionally
 *   illegitimate; the constraint is presented as legitimate constitutional
 *   architecture rather than extraction. Structurally, however, the
 *   arrangement coerces the continued membership of unwilling regions and
 *   retains federal control over their resources.
 *
 * KEY AGENTS:
 *   - federal_union_authority: Primary agenda_setter (institutional/identity_locked) â enforces territorial integrity and captures resource jurisdiction.
 *   - constitutional_court: Secondary agenda_setter (institutional/analytical) â legitimizes prohibition through interpretation.
 *   - interprovincial_coalition: Beneficiary (organized/constrained) â receives fiscal and territorial stability from continued union.
 *   - separatist_regional_movements: Primary payer (organized/trapped) â bears denied sovereignty and extraconstitutional status.
 *   - resource_exporting_provinces: Secondary payer (moderate/constrained) â bears retained resource extraction and limited fiscal autonomy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.68).
domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.75).
domain_priors:theater_ratio(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__constitutional_impossibility_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__constitutional_impossibility_reading, "Constitutional Prohibition on Unilateral Secession").
narrative_ontology:topic_domain(secession_legitimacy_boundary__constitutional_impossibility_reading, "political/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__constitutional_impossibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__constitutional_impossibility_reading, '64eaf2ad-41ac-4995-8d9e-d8a839ff3e25').
narrative_ontology:cs_kernel_codification('64eaf2ad-41ac-4995-8d9e-d8a839ff3e25', fixed_text).
narrative_ontology:cs_authority_grounding('64eaf2ad-41ac-4995-8d9e-d8a839ff3e25', lineage).
narrative_ontology:cs_interpretation_layer_present('64eaf2ad-41ac-4995-8d9e-d8a839ff3e25').
narrative_ontology:cs_reading_relation('64eaf2ad-41ac-4995-8d9e-d8a839ff3e25', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('64eaf2ad-41ac-4995-8d9e-d8a839ff3e25', secession_legitimacy_boundary__grievance_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('64eaf2ad-41ac-4995-8d9e-d8a839ff3e25', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('64eaf2ad-41ac-4995-8d9e-d8a839ff3e25', foundational, constitutional_amendment_sole_legitimate_exit_path).
narrative_ontology:cs_axiom_status(constitutional_amendment_sole_legitimate_exit_path, holdable).
narrative_ontology:cs_axiom_grounding('64eaf2ad-41ac-4995-8d9e-d8a839ff3e25', constitutional_amendment_sole_legitimate_exit_path, conventional).
narrative_ontology:cs_axiom('64eaf2ad-41ac-4995-8d9e-d8a839ff3e25', foundational, federal_territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(federal_territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('64eaf2ad-41ac-4995-8d9e-d8a839ff3e25', federal_territorial_integrity_absolute, conventional).
narrative_ontology:cs_reference_frame('64eaf2ad-41ac-4995-8d9e-d8a839ff3e25', perpetual_union_constitutional_framework).
narrative_ontology:cs_drift_state('64eaf2ad-41ac-4995-8d9e-d8a839ff3e25', contemporary_secessionist_resurgence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('64eaf2ad-41ac-4995-8d9e-d8a839ff3e25', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_union_authority).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, interprovincial_coalition).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, separatist_regional_movements).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_exporting_provinces).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, perpetual_union_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the constitutional prohibition on unilateral secession through federal courts, fiscal transfers, and coercive power. Benefits from territorial integrity, unified resource jurisdiction, and tax authority over all constituent regions. Its political identity is fused with the preservation of the union.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_union_authority, agenda_setter,
    institutional, generational, identity_locked, national).

% Adjudicates reference questions and challenges concerning the constitutional permissibility of secession. Its decisions activate or deactivate enforcement machinery and legitimize the prohibition through constitutional interpretation.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_court, agenda_setter,
    institutional, generational, analytical, national).

% Provinces and territories that benefit from continued federal union, common market access, and fiscal equalization flows. They support the constitutional prohibition because it preserves the economic and territorial union from which they derive net benefit.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, interprovincial_coalition, beneficiary,
    organized, generational, constrained, national).

% Political movements seeking independent statehood or unilateral exit from the federation. Bear the cost of the constitutional prohibition: their preferred political status is categorically illegal, their referendum mandates are invalidated, and their resource wealth remains subject to federal redistribution.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, separatist_regional_movements, payer,
    organized, biographical, trapped, regional).

% Regions with significant natural resource deposits whose fiscal capacity is governed by federal-provincial agreements. The prohibition on unilateral secession prevents them from exiting the federation to retain full resource rents and autonomous regulatory control.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_exporting_provinces, payer,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_union_authority).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__constitutional_impossibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents territorial fragmentation of the federation, preserving common defense, currency union, internal trade, and interprovincial transfer mechanisms that would collapse if regions could exit unilaterally without negotiated terms.
% TRANSFER_FUNCTION: Moves sovereignty, territorial jurisdiction, and resource rents from regional separatist movements and resource-exporting provinces to the federal authority and the remaining interprovincial coalition.
% ABSENT_VOICES: Indigenous nations whose treaty territories may be bisected by provincial secession borders; sub-provincial minority communities within separatist regions who oppose exit but are subsumed by the regional majority; international actors who recognize self-determination norms but are excluded from domestic constitutional interpretation.
% DISAPPEARANCE_RATIONALE: If the constitutional prohibition vanished, regions with separatist majorities would likely declare independence, the federal resource base and territorial integrity would fragment, and the interprovincial economic and defense order would require immediate fundamental renegotiation.
% FOUNDING_PROBLEM: How to bind geographically dispersed, economically unequal, and culturally distinct regions into a durable political union capable of common defense and shared resource governance without permitting constant exit threats that would prevent collective investment and risk balkanization.
% FOUNDING_PROBLEM_CORROBORATION: Federal constitutional historians and central government officials attest the necessity of union preservation. Regional separatist historians and legal pluralists attest the founding problem was resolved by suppressing legitimate self-determination claims rather than by genuine consent; independent comparative federalism scholars note that federations with explicit secession clauses (e.g., Ethiopia, historical USSR constitutional theory) functioned as viable states, corroborating that absolute prohibition is not structurally necessary for union durability.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__constitutional_impossibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__constitutional_impossibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__constitutional_impossibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.68) because the constraint enables the federal authority to retain control over resource-rich territories and fiscal flows against the expressed will of regional majorities. Suppression is high (0.75) because the prohibition is backed by judicial invalidation of referenda, fiscal penalties, and historically by latent or manifest coercive capacity. Theater ratio is moderate-low (0.25) because while there is genuine constitutional argumentation, a significant portion of federal legal and political activity performs territorial control rather than disinterested constitutional interpretation. Accessibility collapse is high (0.70) because once the constitutional framework is accepted, unilateral exit appears legally impossible and alternatives are foreclosed in the legal imagination. Resistance is moderate (0.55) because separatist movements continue to mobilize, hold provincial office, and contest the constraint through electoral and legal channels. The temporal series show rising extraction and enforcement ratchet from t=0 to t=40 as separatist challenges emerged and were suppressed.
 *
 * PERSPECTIVAL GAP:
 *   The federal authority and interprovincial coalition experience the constraint as necessary constitutional order preserving collective goods (defense, currency, trade, resource pooling). Separatist movements and resource-exporting provinces experience it as forcible retention within an unwanted union and as a barrier to autonomous resource control. The constitutional court experiences it as a legal interpretive duty. The engine computes this divergence from the structural data: identical constitutional text generates opposite seat classifications depending on beneficiary versus payer position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal_union_authority and interprovincial_coalition are structural beneficiaries (low directionality: the constraint subsidizes their territorial and resource control). Separatist_regional_movements and resource_exporting_provinces are structural payers (high directionality: the constraint extracts their sovereignty and resource autonomy). The constitutional_court sits near symmetric but leans beneficiary as the administrator of the constraint whose institutional function depends on the constitutional order's continuity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by separating the genuine coordination function (federal union provides defense, currency, trade infrastructure) from the extraction function (preventing resource-rich regions from exiting and taking their resources). The R5 genealogy shows the founding problem of fragmentation risk is contested â comparative evidence demonstrates that federations with explicit secession procedures (e.g., Ethiopia, historical USSR theory) maintained viable coordination â suggesting the absolute prohibition may have outlived its coordination justification and now persists to capture resource rents. The mandatrophy_resolved flag is not set because the arrangement persists despite contested live necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_coordination_motive,
    'Is the prohibition on unilateral secession primarily motivated by genuine coordination needs (defense, currency, trade) or by the extraction of resource rents from unwilling regions?',
    'Comparative analysis of federations with and without secession clauses, measuring union stability and fiscal flows; natural experiment from constitutional moments where secession was permitted or negotiated.',
    'If primarily coordination, the extraction metric overstates the constraint''s cost and the classification may shift toward rope. If primarily extraction, the tangled_rope classification is reinforced and the coordination story is cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_motive, empirical, 'Whether the constraint''s core motive is coordination or resource extraction').

omega_variable(
    consent_vs_coercion_origin,
    'Was the original federation formed by genuine regional consent or by colonial, imperial, or coercive imposition, and does this origin affect whether the prohibition constitutes legitimate coordination?',
    'Historical archival research and comparative state-formation studies assessing the voluntariness of original accession.',
    'If origin was coercive, the constraint''s legitimacy as coordination collapses and the classification tilts toward snare. If origin was genuinely consensual, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_coercion_origin, empirical, 'Whether federal origin was consensual or coercive').

omega_variable(
    reading_scope_ambiguity,
    'Does the constitutional impossibility reading logically foreclose indigenous treaty-based exit claims, or do treaty rights occupy a separate normative layer that coexists with the constitutional amendment requirement?',
    'Jurisprudential analysis of whether constitutional courts have treated treaty rights as superseding, parallel to, or subordinate to the federal constitutional architecture.',
    'If treaty rights are foreclosed, the reading is more absolutist than its framing suggests. If they coexist, the reading''s scope is narrower than absolute federal authority and may permit plural exit pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_scope_ambiguity, conceptual, 'Ambiguity in the scope of constitutional absolutism relative to treaty rights').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__constitutional_impossibility_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(sece_tr_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 40, 0.25).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(sece_be_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(sece_su_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 32, 0.73).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, grievance_threshold_reading).

% DUAL FORMULATION NOTE:
% This constraint is the constitutional_impossibility_reading of the secession_legitimacy_boundary kernel. The kernel decomposes into four structurally distinct readings with different epsilon values, beneficiary/victim structures, and authority groundings. Each reading must be evaluated as a separate constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
