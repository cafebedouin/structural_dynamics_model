% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__resource_sovereignty_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: provincial_sovereignty_boundary__resource_sovereignty_primacy
 *   human_readable: Provincial Resource Sovereignty (s.92A Reading)
 *   domain: political_economy/federalism/constitutional_law
 *
 * SUMMARY:
 *   Section 92A of the Constitution Act 1982 grants provinces exclusive
 *   jurisdiction over natural resources within their borders and the right to
 *   make laws regarding resource exploitation and export. This constraint
 *   story instantiates the resource_sovereignty_primacy READING of the
 *   provincial_sovereignty_boundary kernel — the reading that s.92A grounds
 *   absolute provincial sovereignty over resources, and that federal climate
 *   policy, interprovincial coordination rules, and fiscal instruments that
 *   constrain resource extraction therefore constitute illegitimate federal
 *   extraction of provincial sovereignty. This is a contested constitutional
 *   claim: the compact_federalism reading emphasizes residual provincial
 *   power without claiming absoluteness; the constitutional_subordination
 *   reading asserts that provinces are creatures of the federal constitution
 *   and s.92A is a enumerated power, not a sovereignty grant. This story
 *   models ONLY the resource_sovereignty_primacy reading as a coherent
 *   constraint with its own ε-invariance, beneficiary structure, and
 *   persistence mechanism. Sibling readings are OTHER constraints, linked via
 *   network edges.
 *
 * KEY AGENTS:
 *   - provincial_governments: institutional agenda-setters (identity-locked to sovereignty framing); assert s.92A as absolute right
 *   - resource_extraction_industries: powerful beneficiaries (arbitrage-mobile); benefit from provincial defiance of federal climate rules
 *   - federal_government: institutional payer (constrained exit); loses climate enforcement authority and fiscal leverage
 *   - other_provinces: institutional payers (identity-locked, constrained); face regulatory races to the bottom and fragmentation
 *   - environmental_advocates: organized but excluded (constrained exit); blocked from federal enforcement channels by sovereignty assertion
 *   - international_climate bodies: institutional but excluded (trapped); unable to enforce federal commitments against provincial defection
 *   - constitutional_courts: analytical observers; their interpretation of s.92A scope determines enforceability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.68).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.72).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.68).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Provincial Resource Sovereignty (s.92A Reading)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political_economy/federalism/constitutional_law").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, '3d304913-b908-4657-aad1-31eb83751b41').
narrative_ontology:cs_kernel_codification('3d304913-b908-4657-aad1-31eb83751b41', fixed_text).
narrative_ontology:cs_authority_grounding('3d304913-b908-4657-aad1-31eb83751b41', lineage).
narrative_ontology:cs_interpretation_layer_present('3d304913-b908-4657-aad1-31eb83751b41').
narrative_ontology:cs_reading_relation('3d304913-b908-4657-aad1-31eb83751b41', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('3d304913-b908-4657-aad1-31eb83751b41', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_axiom('3d304913-b908-4657-aad1-31eb83751b41', foundational, resource_ownership_is_sovereignty).
narrative_ontology:cs_axiom_status(resource_ownership_is_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('3d304913-b908-4657-aad1-31eb83751b41', resource_ownership_is_sovereignty, deontological).
narrative_ontology:cs_axiom('3d304913-b908-4657-aad1-31eb83751b41', foundational, provincial_resource_authority_is_absolute).
narrative_ontology:cs_axiom_status(provincial_resource_authority_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('3d304913-b908-4657-aad1-31eb83751b41', provincial_resource_authority_is_absolute, conventional).
narrative_ontology:cs_reference_frame('3d304913-b908-4657-aad1-31eb83751b41', provincial_ownership_as_constitutional_right).
narrative_ontology:cs_drift_state('3d304913-b908-4657-aad1-31eb83751b41', contemporary_climate_intensification_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3d304913-b908-4657-aad1-31eb83751b41', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_governments).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_extraction_industries).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_environmental_enforcement).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, interprovincial_coordination_capacity).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, climate_policy_implementation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, other_provinces).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_autonomy_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_jurisdiction_as_sovereignty).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, compact_theory_residual_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce the sovereignty reading through litigation (challenging federal climate rules, defending provincial permitting), legislation (asserting provincial revenue rights, resisting federal fiscal conditions), and regulatory action (granting resource permits, setting provincial environmental standards as floor rather than ceiling). Their institutional identity is constituted through claims of territorial sovereignty; they cannot exit this reading without dissolving the legitimacy claim that grounds their authority. They assert that s.92A places resource control entirely in provincial hands and that federal climate policy therefore violates the constitution.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_governments, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from provincial invocation of s.92A to resist federal climate and environmental regulation. Capture provincial regulatory bodies through lobbying and campaign finance; fund litigation defending provincial sovereignty. Can arbitrage between permissive provincial regimes and federal pressure by threatening relocation. Maintain influence at the federal level simultaneously to create regulatory uncertainty that favors delay and fragmentation. Directly benefit from extraction rents (resource revenue, deferred climate costs).
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_extraction_industries, beneficiary,
    powerful, biographical, arbitrage, national).

% Bears extraction costs: cannot implement coherent climate policy (provinces block federal carbon pricing and emissions standards through constitutional claims); loses fiscal leverage (provinces claim exclusive resource revenue rights and resist federal equalization and climate-contingent funding); faces litigation costs defending federal environmental authority; sits unable to enforce international climate commitments where provincial defection is visible to other nations. Constrained by constitutional text and current judicial interpretation: unilateral federal override is a constitutional violation under the resource_sovereignty_primacy reading.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, payer,
    institutional, generational, constrained, national).

% When one province invokes absolute resource sovereignty, it constrains others' ability to set higher environmental or labor standards (regulatory races to the bottom); blocks interprovincial resource-sharing agreements that federal coordination could enforce (fragmented energy markets, water disputes); and fragments carbon pricing and emissions-control regimes. A province that prefers federal coordination finds its own sovereignty claim invoked against it; exit from the sovereignty frame is constitutionally unavailable (they cannot reject s.92A without surrendering their own resource jurisdiction).
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, other_provinces, payer,
    institutional, generational, constrained, national).

% Have no standing in resource sovereignty disputes; they cannot hold resource title and cannot directly invoke s.92A. If environmental regulation is reframed as illegitimate federal extraction of provincial rights, advocates lose their primary federal enforcement mechanism. They are structurally outside the sovereignty conversation even though the constraint's operation directly forecloses their policy channels. They operate through litigation as intervenors and through federal legislative lobbying, but their voices are excluded from the core sovereignty dispute.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, environmental_advocates, excluded,
    organized, generational, constrained, global).

% Canada commits to climate targets (Paris Agreement, net-zero pledges) at the federal level, but provincial invocation of absolute resource sovereignty fragments implementation. International bodies cannot enforce targets against sub-state provincial actors; they are trapped holding a federal government accountable for powers it cannot exercise. They must negotiate with federal Canada while watching provincial defection make federal commitments impossible to meet.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, international_climate_bodies, excluded,
    institutional, civilizational, trapped, global).

% Interpret s.92A and adjacent federal powers (peace/order/good government, criminal law, trade and commerce). Their rulings on whether resource sovereignty is absolute or limited by federal climate/trade authority determine the constraint's enforceability. Different judicial compositions and changing jurisprudence show the constraint is not naturally determined but judicially constructed. Current mixed jurisprudence (some rulings affirm provincial absolute rights, others recognize federal climate authority) leaves the boundary contestable.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_courts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_governments).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__resource_sovereignty_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates resource development authority and revenue within a federal system: the coordination problem is whether resource decisions are provincial (leaving extraction to local choice and local revenue capture) or federal (enabling systematic climate and trade policy). This reading asserts the coordination solution is absolute provincial authority: resource jurisdiction IS provincial sovereignty, and federal coordination is illegitimate.
% TRANSFER_FUNCTION: Moves enforcement authority and fiscal revenue from federal climate/environmental/trade regulation to provincial resource control; moves regulatory deference from federal standard-setting to provincial permitting. Also moves litigation cost burden (federal actors must defend climate authority in court; provinces gain free legal challenge to federal rules). Extracts federal coordination capacity into provincial revenue protection and sovereignty assertion.
% ABSENT_VOICES: Environmental advocates and climate-dependent sectors (agriculture, renewable energy, fisheries harmed by carbon lock-in) have no seat at the sovereignty table. They cannot invoke s.92A and thus are excluded from the negotiation even though the constraint's operation directly blocks their policy channels. International climate parties are similarly excluded — they negotiate federal commitments but cannot compel provincial compliance.
% DISAPPEARANCE_RATIONALE: If this reading of s.92A as grounding absolute provincial sovereignty disappeared — if courts ruled that resource jurisdiction is limited by federal climate/trade/interprovincial coordination authority — the federal environmental enforcement apparatus would reset entirely. Carbon pricing would apply uniformly across provinces, pipeline and extraction permitting would have a federal safety valve, interprovincial resource disputes would have clear federal adjudication, and international climate commitments would become implementable through federal law. Provincial governments would lose their veto over federal climate rules. The resource and energy economy would reorganize around a federal-provincial regulatory floor rather than provincial-maximum fragments.
% FOUNDING_PROBLEM: Confederation created constitutional ambiguity: did provincial resource ownership (a fact of colonial and early dominion governance) survive under a federal constitution with enumerated federal powers over trade, commerce, and criminal law? s.92A was added in 1982 (patriation and constitutional reform) partly to assert that the answer is unambiguously 'yes — provinces own resources absolutely' and partly as a political concession to Alberta during energy disputes over federal control of oil prices and export policy.
% FOUNDING_PROBLEM_CORROBORATION: Provincial governments assert the founding problem is live: federal climate and environmental overreach requires continued assertion of s.92A rights to protect provincial autonomy. Federal government and federal courts assert the founding problem is substantially solved; the constitution's text and judicial precedent already clarify federal and provincial powers. Environmental economists and climate scientists assert the founding problem has been INVERTED: the original ambiguity (whether provinces own resources) is solved, but the constraint now creates a NEW problem (fragmented climate governance that prevents emission reductions). Constitutional historians and archivists document that s.92A's drafting was driven by Alberta's political pressure during the National Energy Program dispute (1980-1985), not by a demonstrated governance gap or ambiguity in jurisprudence — corroborating that the constraint's persistence is primarily political and industry-captured rather than problem-responsive.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'none', 1).

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
 *   Extractiveness rises from 0.48 to 0.68 over the interval as federal climate policy intensifies and provincial invocation of s.92A becomes the primary defense against that intensification. The extraction is not captured as direct revenue transfer (as with the platform commission example) but as enforcement authority transfer: provinces extract the right to block federal environmental/climate rules by claiming sovereignty. Suppression is high (0.72 endpoint) and rising because the constraint's persistence depends on actively suppressing federal enforcement mechanisms — not through overt coercion but through constitutional litigation and regulatory defiance that courts must adjudicate. Federal climate enforcement is suppressed not by denying its legitimacy in principle but by making it legally contestable at each application. Theater rises from 0.25 to 0.41 because sovereignty assertion increasingly serves to defend resource extraction interests (the original coordination function — allocating resource authority — is stable, but performative assertion of absolute sovereignty to resist climate rules grows). The measurement series share one time grid: every metric is authored at t=0, 5, 10, 15, 20, 25, 30, 40 with basis marked (observed through t=20, projected beyond).
 *
 * PERSPECTIVAL GAP:
 *   From a provincial government seat, the constraint is genuine coordination: allocating resource authority between levels of government, protecting provincial autonomy against centralizing federal overreach. The extraction reading appears to the provincial seat as illegitimate reframing by federal actors and environmental advocates who want to strip provincial rights. From the federal seat, the same structure is extraction: provinces invoke sovereignty to block environmental regulation, fragmenting climate policy, and extract compliance costs (federal rules must be litigated, implemented patchily, or abandoned). From an environmental advocate seat (excluded), the constraint operates as pure suppression of federal enforcement channels — the sovereignty frame makes climate regulation impossible regardless of its merits. The engine should compute these as divergent classifications from the same structural data: federal/environmental seats should compute the constraint as more extractive (higher d toward target end); provincial seats should compute it as coordination (lower d toward beneficiary end). This divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality overrides are authored. The structural derivation should produce this pattern: provincial governments are beneficiaries (low d, ~0.1-0.2) given their control-gaining exit-gaining role; federal government is a target (high d, ~0.75-0.85) given suppressed enforcement authority and loss of coordination capacity; other provinces sit awkwardly (moderate-high d, ~0.55-0.70) because they are payers in a coordination failure (fragmentation) but identity-locked to the beneficiary frame (they cannot reject sovereignty claims without surrendering their own authority claims). The constraint's enforceability rests on this d-divergence: it persists because the seats that benefit (provinces) have sufficient institutional power to enforce their reading through courts and legislatures.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint sits in a mandatrophy-resolved state: the founding problem was to clarify whether federal powers could override provincial resource decisions. s.92A was written to answer 'no, provinces have absolute resource rights.' But the founding problem has been solved — federal authority over commerce, trade, and criminal law is now constitutionally established, and s.92A's interaction with those powers is defined (though contested). What persists is not a coordination failure but a political dispute over the boundary. The provincial assertion of 'absolute sovereignty' is increasingly theatrical: it performs sovereignty (litigation, legislative defiance, regulatory grandstanding) while operating within constitutional constraints (courts do override some provincial rules, federal-provincial agreements do sometimes bind). The measurement data show theater_ratio rising from 0.25 to 0.41, indicating that a growing share of provincial 'sovereignty assertion' is performative maintenance of the reading rather than functional governance. This is the piton signature: the constraint persists by theatrical assertion, not because it solves a live problem. However, the constraint is classified as tangled_rope (not piton) because the extraction component is real and substantial — provinces do extract enforcement deference, and industries do benefit from fragmented federal climate policy — even though the coordination function has atrophied. Piton classification would require no concentrated beneficiary; here the beneficiary concentration is clear (provinces and resource industries collect extraction), so the constraint is tangled_rope with high theater_ratio and contested mandate status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_ownership_equals_sovereignty,
    'Does constitutional ownership of natural resources ENTAIL or GROUND territorial sovereignty, or is resource jurisdiction a mere enumerated power, subject to federal limits like any other provincial authority?',
    'Constitutional court ruling on whether s.92A creates an exception to federal supremacy or is simply one allocation of power within a federally supreme framework. Jurisprudential historical analysis of the drafting intent of s.92A: was it written to grant absolute provincial rights or to clarify that resource extraction is a provincial matter (subject to federal limits in trade, criminal law, etc.)?',
    'If resource ownership grounds sovereignty (this reading''s axiom), s.92A functions as a constitutional exit clause and provinces can unilaterally reject federal climate rules. If it is an enumerated power, federal climate law overrides provincial resource decisions when the federal domain is engaged, and s.92A becomes a coordination point, not a sovereignty grant. The constraint type could shift from tangled_rope to snare (pure federal extraction) or rope (genuine coordination) depending on the resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_ownership_equals_sovereignty, conceptual, 'Whether resource ownership constitutes territorial sovereignty or is an enumerated provincial power.').

omega_variable(
    federal_climate_authority_scope,
    'Does federal authority over trade/commerce, criminal law, and peace/order/good government extend to setting and enforcing climate/environmental standards that constrain resource extraction, or does s.92A carve out an exception?',
    'A series of constitutional court decisions on federal carbon pricing, interprovincial pipeline permitting, and environmental assessment authority. The pattern of rulings will clarify whether federal instruments are treated as invasions of provincial sovereignty or as legitimate federal exercises within assigned domains.',
    'If federal climate authority is recognized, the constraint shifts toward pure coordination or rope (provinces and federal government negotiate binding climate rules). If federal climate authority is blocked by s.92A invocation, the constraint remains as currently modeled (tangled_rope with high extraction and suppression). This is the live fight.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_climate_authority_scope, empirical, 'Whether federal climate/environmental authority is constitutionally available.').

omega_variable(
    provincial_exit_conditionality,
    'Is unilateral provincial exit from federal climate frameworks a constitutional RIGHT (as resource_sovereignty_primacy claims) or a political OPTION constrained by federal authority and equalization obligations?',
    'A federal province attempting to withdraw from a federal climate agreement (carbon pricing, emission standards) and being challenged in court. Does the court recognize an exit right grounded in s.92A, or does it impose federal-compliance requirements based on trade/commerce and fiscal authority?',
    'If exit is a right, provinces can credibly threaten withdrawal and extract concessions from federal climate policy. If exit is conditional, provinces have negotiating power but not veto power. The constraint''s enforceability and beneficiary structure both hinge on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provincial_exit_conditionality, empirical, 'Whether provincial exit from federal frameworks is a constitutional right or a conditional political option.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression of federal climate enforcement structural (federal rules are blocked by constitutional barriers) or internalized (federal officials and courts have adopted the sovereignty frame and self-suppress enforcement)?',
    'Institutional history: examine how federal and court actors shifted their behavior over the interval. Did they stop trying to enforce climate rules because they lost court cases (structural suppression), or did they preemptively refrain because they accepted the sovereignty reading (internalized)? Post-change behavior after a potential court reversal would show whether suppression persists.',
    'If structural, the constraint persists until a court clarification changes the rule. If internalized, the constraint persists through institutionalized acceptance of the sovereignty frame even after legal barriers shift. The remediation strategy and timescale differ significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of federal enforcement is structural barriers or internalized frame adoption.').

omega_variable(
    theater_ratio_acceleration,
    'Why does theater_ratio rise from 0.25 to 0.41 (growing divergence between performative sovereignty assertion and actual governance function) while base_extractiveness plateaus at 0.68 after t=20?',
    'Qualitative institutional analysis: track the composition of provincial ''sovereignty assertion'' activities over time. If more is litigation and media performance and less is actual resource governance innovation, theater is rising. Examine resource extraction volumes and regulatory outcomes: are extraction rates accelerating (functional governance) or stable (maintenance by assertion)?',
    'Rising theater with plateau extraction suggests the constraint is drifting toward piton (performative maintenance of inertial institutional form). If theater continues rising above 0.5, the constraint''s primary function may become theatrical rather than extractive, and the beneficiary concentration may diffuse (no one is capturing the extraction anymore; instead, everyone performs sovereignty). This would trigger reclassification pressure toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_acceleration, empirical, 'Why performative sovereignty assertion is accelerating while actual extraction stabilizes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(prov_tr_t0, observed).
narrative_ontology:measurement(prov_tr_t5, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 5, 0.29).
narrative_ontology:measurement_basis(prov_tr_t5, observed).
narrative_ontology:measurement(prov_tr_t10, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(prov_tr_t10, observed).
narrative_ontology:measurement(prov_tr_t15, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(prov_tr_t15, observed).
narrative_ontology:measurement(prov_tr_t20, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(prov_tr_t20, observed).
narrative_ontology:measurement(prov_tr_t25, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(prov_tr_t25, projected).
narrative_ontology:measurement(prov_tr_t30, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(prov_tr_t30, projected).
narrative_ontology:measurement(prov_tr_t40, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(prov_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(prov_be_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(prov_be_t0, observed).
narrative_ontology:measurement(prov_be_t5, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(prov_be_t5, observed).
narrative_ontology:measurement(prov_be_t10, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(prov_be_t10, observed).
narrative_ontology:measurement(prov_be_t15, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(prov_be_t15, observed).
narrative_ontology:measurement(prov_be_t20, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(prov_be_t20, observed).
narrative_ontology:measurement(prov_be_t25, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(prov_be_t25, projected).
narrative_ontology:measurement(prov_be_t30, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(prov_be_t30, projected).
narrative_ontology:measurement(prov_be_t40, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(prov_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(prov_su_t0, observed).
narrative_ontology:measurement(prov_su_t5, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(prov_su_t5, observed).
narrative_ontology:measurement(prov_su_t10, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(prov_su_t10, observed).
narrative_ontology:measurement(prov_su_t15, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(prov_su_t15, observed).
narrative_ontology:measurement(prov_su_t20, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(prov_su_t20, observed).
narrative_ontology:measurement(prov_su_t25, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(prov_su_t25, projected).
narrative_ontology:measurement(prov_su_t30, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(prov_su_t30, projected).
narrative_ontology:measurement(prov_su_t40, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(prov_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__resource_sovereignty_primacy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.12).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_climate_policy_implementation).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, interprovincial_coordination_capacity).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_extraction_permitting_authority).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary__constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary__compact_federalism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the provincial_sovereignty_boundary kernel. Sibling readings (constitutional_subordination and compact_federalism) are separate constraints with different ε values, beneficiary structures, and judicial interpretations. The three constraints form a constraint family linked through the kernel: they are not three perspectives on one constraint but three structurally distinct constraints grounded in competing interpretations of s.92A and federal-provincial authority. Each reading instantiates a different ε (resource_sovereignty_primacy: high extraction; subordination: potential low extraction if federal authority is recognized; compact: moderate extraction with negotiable boundaries). The family structure enables lifecycle analysis: as courts rule on specific disputes (carbon pricing, pipeline authority, interprovincial trade), the empirical support for each reading shifts, and the family's configuration changes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
