% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__compact_federalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__compact_federalism, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: provincial_sovereignty_boundary__compact_federalism
 *   human_readable: Provincial Sovereignty Boundary (Compact Federalism Reading)
 *   domain: political/federalism/resource_governance
 *
 * SUMMARY:
 *   The provincial sovereignty boundary under the compact federalism reading
 *   treats Canadian Confederation as a negotiated treaty among pre-existing
 *   sovereign colonies rather than a unilateral act of British imperial
 *   legislation. Under this reading, provinces retain all powers not
 *   expressly delegated to the federal government, including residual
 *   sovereignty and a right to negotiate exit under duress. The constraint
 *   coordinates a geographically and economically diverse federation but
 *   extracts asymmetrically: resource-rich provinces retain resource rents
 *   and veto federal climate and fiscal policy, while the federal government
 *   faces constrained authority and have-not provinces face uncertain
 *   equalization. Indigenous nations are structurally excluded from the
 *   compact. The constraint is actively enforced through constitutional
 *   litigation, intergovernmental negotiation, and political threat of
 *   non-compliance or secession.
 *
 * KEY AGENTS:
 *   - province_governments: Primary beneficiary (institutional/constrained) â retain residual sovereignty and jurisdictional autonomy
 *   - resource_rich_provinces: Concentrated beneficiary (powerful/constrained) â control natural resources and resist federal climate regulation
 *   - federal_government: Agenda-setter and payer (institutional/constrained) â administers federation but faces provincial vetoes and conditional authority
 *   - have_not_provinces: Primary payer (moderate/constrained) â rely on negotiable equalization with reduced bargaining power
 *   - indigenous_nations: Excluded voice (organized/trapped) â prior sovereignty ignored by the provincial-federal binary
 *   - apex_court: Analytical observer (institutional/analytical) â adjudicates federalism disputes without fully endorsing compact theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.64).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.45).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.64).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Provincial Sovereignty Boundary (Compact Federalism Reading)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, '791113f6-9934-44b2-a619-d28238bbc01c').
narrative_ontology:cs_kernel_codification('791113f6-9934-44b2-a619-d28238bbc01c', fixed_text).
narrative_ontology:cs_authority_grounding('791113f6-9934-44b2-a619-d28238bbc01c', lineage).
narrative_ontology:cs_interpretation_layer_present('791113f6-9934-44b2-a619-d28238bbc01c').
narrative_ontology:cs_reading_relation('791113f6-9934-44b2-a619-d28238bbc01c', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('791113f6-9934-44b2-a619-d28238bbc01c', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('791113f6-9934-44b2-a619-d28238bbc01c', foundational, residual_sovereignty_retention).
narrative_ontology:cs_axiom_status(residual_sovereignty_retention, holdable).
narrative_ontology:cs_axiom_grounding('791113f6-9934-44b2-a619-d28238bbc01c', residual_sovereignty_retention, conventional).
narrative_ontology:cs_axiom('791113f6-9934-44b2-a619-d28238bbc01c', foundational, exit_by_negotiation_not_permission).
narrative_ontology:cs_axiom_status(exit_by_negotiation_not_permission, holdable).
narrative_ontology:cs_axiom_grounding('791113f6-9934-44b2-a619-d28238bbc01c', exit_by_negotiation_not_permission, conventional).
narrative_ontology:cs_reference_frame('791113f6-9934-44b2-a619-d28238bbc01c', confederation_compact_sovereignty).
narrative_ontology:cs_drift_state('791113f6-9934-44b2-a619-d28238bbc01c', contemporary_federalism_conflicts, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('791113f6-9934-44b2-a619-d28238bbc01c', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, province_governments).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, resource_rich_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, have_not_provinces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain residual sovereignty under the compact reading; control education, health, natural resources, and local institutions. Bargain with the federal government over equalization and climate policy. Exit from the federation is structurally possible only through negotiation under conditions of political duress, not by unilateral declaration.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, province_governments, beneficiary,
    institutional, generational, constrained, regional).

% Operates national taxation, defence, and interprovincial trade but faces provincial resistance and constitutional challenge when legislating in areas of provincial jurisdiction. Under this reading its authority is conditional on provincial consent; it bears the political and fiscal cost of renegotiating transfers and accommodating provincial vetoes.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, federal_government, payer).

% Control substantial oil, gas, and mineral revenues under provincial ownership of resources. Use the compact frame to resist federal carbon pricing and climate regulation, framing resources as exclusively provincial jurisdiction. Benefit from retained rents and policy autonomy.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, resource_rich_provinces, beneficiary,
    powerful, biographical, constrained, regional).

% Rely on federal equalization transfers to maintain comparable public services. Under the compact reading equalization is negotiable rather than guaranteed; their transfer levels depend on the fiscal capacity and political goodwill of resource-rich provinces and the federal government.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, have_not_provinces, payer,
    moderate, biographical, constrained, regional).

% Hold prior and continuing sovereignty over territories and resources that the provincial-federal compact allocates between settler governments. Excluded from the 1867 compact and its subsequent interpretations; their claims erode the binary sovereignty boundary the constraint presupposes.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, indigenous_nations, excluded,
    organized, generational, trapped, national).

% Adjudicates federalism disputes including references on provincial autonomy, federal power, and secession. Its interpretations partially constitute the enforceability of the provincial sovereignty boundary, though it has avoided fully endorsing compact theory.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, apex_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__compact_federalism, province_governments).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__compact_federalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a multi-provincial federation by distributing residual sovereignty to provinces, requiring mutual consent for constitutional amendment, and establishing a bilateral negotiation framework for fiscal transfers and overlapping jurisdiction.
% TRANSFER_FUNCTION: Transfers control over natural resources, education, health, and property rights to provincial governments; transfers uncertainty over equalization entitlements to have-not provinces; transfers conditional legitimacy and bounded agenda-setting capacity to the federal government.
% ABSENT_VOICES: Indigenous nations, whose prior sovereignty is erased by the provincial-federal binary; national-policy constituencies seeking uniform environmental or social standards across the federation; federal centralists who read Confederation as creating a supreme central government.
% DISAPPEARANCE_RATIONALE: If the provincial sovereignty boundary under compact federalism vanished overnight, federal authority would likely expand into provincial domains, equalization would become a federal program rather than a negotiation, resource-rich provinces would face direct federal regulation, and the federation would shift toward constitutional subordination or dissolution into separate sovereignties.
% FOUNDING_PROBLEM: How to unite geographically separated British colonies with distinct economies, legal systems, and cultural identities into a single self-governing polity without submerging local autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Federal historians and the federal government attest the problem was solved by a centrally drafted imperial statute. Provincial governments and compact historians attest it was solved by a negotiated treaty among sovereign colonies. Indigenous historians and legal scholars attest the problem was solved without Indigenous participation, rendering the compact illegitimate from their perspective. Corroboration is split, with no consensus outside the benefiting parties.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__compact_federalism, 0.64, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__compact_federalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__compact_federalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.64 at interval end) is substantial because the compact reading enables provinces to block or extract concessions on federal climate, fiscal, and regulatory policy, converting jurisdictional boundaries into bargaining leverage. Theater ratio (0.40) is moderate-to-high: sovereignty rhetoric and intergovernmental drama often exceed the functional need for policy coordination, especially around resource and climate disputes. Suppression (0.45) is moderate â alternatives (federal supremacy, unilateral secession) are legally and politically difficult but not fully suppressed; they remain live positions. Accessibility collapse (0.35) is moderate: alternatives are visible (centralization, independence) but structurally costly. Resistance (0.55) is moderate-to-high: federal centralists and have-not provinces actively contest the compact reading in courts and fiscal negotiations.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (provinces) experience the constraint as a legitimate constitutional safeguard of local autonomy and cultural survival. The payer seats (federal government, have-not provinces) experience it as a barrier to national coordination and redistribution. The engine computes this divergence from the structural role declarations and exit asymmetries: provinces have institutional power and constrained but real exit (negotiation), while have-not provinces are similarly constrained but lack resource leverage, creating a same-level lateral asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Province_governments and resource_rich_provinces are structural beneficiaries: the constraint assigns them sovereignty, resource control, and a veto position in federal negotiations (low directionality â damped extraction). Federal_government and have_not_provinces are structural payers: the federal government faces constrained authority, and have-not provinces face uncertain transfers (high directionality â amplified extraction). Indigenous_nations are excluded from the beneficiary/payer calculus entirely because the constraint presupposes their sovereignty is subsumed within provincial or federal title; their exclusion is the constraint's blind spot.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents misreading the constraint as either a pure rope (ignoring the extraction of federal authority and have-not provincial security) or a pure snare (ignoring the genuine coordination function of a multi-national federation). It also prevents piton misclassification: the constraint is not merely theatrical maintenance of an obsolete structure; it continues to materially distribute resource rents and jurisdictional authority. The founding problem â uniting diverse colonies â remains contested but not dead, as the federation still requires coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compact_historical_accuracy,
    'Was the British North America Act 1867 a negotiated compact among sovereign colonies or a unilateral imperial statute imposed by the British Parliament?',
    'Archival and historiographical review of the Quebec Resolutions, colonial legislative debates, and British parliamentary records to determine the degree of colonial sovereign capacity at the moment of union.',
    'If the compact theory lacks historical support, the residual sovereignty claim weakens and the constraint shifts toward constitutional subordination; if supported, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compact_historical_accuracy, empirical, 'Historical accuracy of the compact theory of Confederation').

omega_variable(
    indigenous_exclusion_from_compact,
    'Does the provincial sovereignty boundary rendered by the compact reading systematically exclude Indigenous nations whose sovereignty predates both provincial and federal claims?',
    'Constitutional reconciliation processes, treaty revival, and judicial recognition of Indigenous jurisdiction that would reframe the binary federalism model as trilateral or multilateral.',
    'If Indigenous sovereignty is structurally incompatible with the compact reading, the constraint''s legitimacy is undermined and its coordination function may collapse into a contested scaffold or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_exclusion_from_compact, conceptual, 'Indigenous sovereignty exclusion from the provincial-federal compact').

omega_variable(
    equalization_negotiability,
    'Is equalization a constitutionally guaranteed right of have-not provinces or a politically negotiable transfer under provincial consent?',
    'Supreme Court of Canada reference on equalization formula entrenchment, or constitutional text amendment clarifying s.36.',
    'If equalization is guaranteed, have-not provinces are less victimized by the compact reading; if purely negotiable, asymmetric extraction from them is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equalization_negotiability, empirical, 'Legal status of equalization transfers under compact federalism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t0, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prov_tr_t8, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 8, 0.23).
narrative_ontology:measurement(prov_tr_t16, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 16, 0.27).
narrative_ontology:measurement(prov_tr_t24, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 24, 0.31).
narrative_ontology:measurement(prov_tr_t32, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 32, 0.35).
narrative_ontology:measurement(prov_tr_t40, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(prov_be_t0, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(prov_be_t8, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(prov_be_t16, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(prov_be_t24, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(prov_be_t32, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 32, 0.59).
narrative_ontology:measurement(prov_be_t40, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 40, 0.64).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(provincial_sovereignty_boundary__compact_federalism, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, resource_allocation).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, resource_sovereignty_primacy).

% DUAL FORMULATION NOTE:
% The provincial_sovereignty_boundary kernel decomposes into three structurally distinct readings: compact_federalism (conditional federal authority, negotiated exit), constitutional_subordination (federal supremacy, no residual sovereignty), and resource_sovereignty_primacy (absolute resource-based sovereignty). Each reading has a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
