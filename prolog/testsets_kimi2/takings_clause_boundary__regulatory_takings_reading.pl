% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-27
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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine (Penn Central Reading)
 *   domain: constitutional/law/property
 *
 * SUMMARY:
 *   This constraint instantiates the regulatory takings reading of the Fifth
 *   Amendment Takings Clause: regulations that diminish economic value too
 *   far constitute compensable takings even without physical seizure. The
 *   reading expands the victim set beyond physical appropriation to include
 *   severe value diminution, introduces an ad hoc balancing test (Penn
 *   Central), and creates persistent uncertainty in regulatory space. As one
 *   reading of the contested takings_clause_boundary kernel, it competes with
 *   the physical_appropriation_reading and categorical_takings_reading. The
 *   authored metrics and claimed type are independent: the claim is
 *   tangled_rope because the doctrine simultaneously coordinates a genuine
 *   boundary between property and regulation while asymmetrically extracting
 *   from taxpayers and regulatory beneficiaries to compensate property
 *   owners.
 *
 * KEY AGENTS:
 *   - Property owners: Primary beneficiary (powerful/mobile) â receive compensation and veto leverage.
 *   - Taxpayers: Primary fiscal target (organized/constrained) â fund awards without direct voice.
 *   - Regulatory agencies: Structural target (institutional/constrained) â bear regulatory chill and litigation risk.
 *   - Regulatory beneficiaries: Diffuse target (powerless/constrained) â lose protective regulation due to chill.
 *   - Courts: Agenda-setter (institutional/analytical) â define and administer the balancing test.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.62).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.58).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine (Penn Central Reading)").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional/law/property").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, 'b0272e65-763d-475f-b862-2e28e34aa500').
narrative_ontology:cs_kernel_codification('b0272e65-763d-475f-b862-2e28e34aa500', formalized).
narrative_ontology:cs_authority_grounding('b0272e65-763d-475f-b862-2e28e34aa500', lineage).
narrative_ontology:cs_interpretation_layer_present('b0272e65-763d-475f-b862-2e28e34aa500').
narrative_ontology:cs_reading_relation('b0272e65-763d-475f-b862-2e28e34aa500', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0272e65-763d-475f-b862-2e28e34aa500', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('b0272e65-763d-475f-b862-2e28e34aa500', foundational, regulatory_value_diminution_is_compensable_taking).
narrative_ontology:cs_axiom_status(regulatory_value_diminution_is_compensable_taking, holdable).
narrative_ontology:cs_axiom_grounding('b0272e65-763d-475f-b862-2e28e34aa500', regulatory_value_diminution_is_compensable_taking, conventional).
narrative_ontology:cs_axiom('b0272e65-763d-475f-b862-2e28e34aa500', secondary, ad_hoc_balancing_legitimate_methodology).
narrative_ontology:cs_axiom_status(ad_hoc_balancing_legitimate_methodology, holdable).
narrative_ontology:cs_axiom_grounding('b0272e65-763d-475f-b862-2e28e34aa500', ad_hoc_balancing_legitimate_methodology, instrumental).
narrative_ontology:cs_reference_frame('b0272e65-763d-475f-b862-2e28e34aa500', comprehensive_property_value_protection).
narrative_ontology:cs_drift_state('b0272e65-763d-475f-b862-2e28e34aa500', contemporary_regulatory_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b0272e65-763d-475f-b862-2e28e34aa500', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_owners).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, taxpayers).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, regulatory_agencies).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, regulatory_beneficiaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold real and personal property subject to land-use, environmental, and public-health regulation. Can bring suit alleging that regulation has diminished value too far, potentially triggering compensation from the public fisc. Benefit from judicially enforced protection against uncompensated regulatory burdens.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_owners, beneficiary,
    powerful, generational, mobile, national).

% Fund compensation awards through general revenue. Do not choose specific regulatory takings payouts and receive no direct countervailing benefit from the constrained regulation. Exit is limited to legislative and political processes that have little granularity over individual compensation judgments.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Design and implement zoning, environmental, health, and safety regulations under legislative mandate. Must account for potential compensation liability when regulations diminish property value. Regulatory chill occurs when agencies avoid high-value regulations due to fiscal exposure and litigation risk.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, regulatory_agencies, payer,
    institutional, biographical, constrained, national).

% Adjudicate takings claims and define what too far means through the Penn Central ad hoc balancing test and subsequent refinements. Control the doctrinal boundary and can revise the standard in new decisions without legislative action.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Would benefit from environmental, health, and safety regulations that are chilled, weakened, or abandoned because agencies fear compensation liability. Bear the costs of under-regulation without direct standing in takings litigation and limited voice in setting the constitutional rule.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, regulatory_beneficiaries, payer,
    powerless, biographical, constrained, local).

% Analyze and critique the doctrinal coherence, historical pedigree, and distributive consequences of the regulatory takings test. Produce competing frameworks that influence judicial and legislative discourse without direct enforcement power.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__regulatory_takings_reading, property_owners).
narrative_ontology:fixing_cost_class(takings_clause_boundary__regulatory_takings_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a judicially enforceable boundary between legitimate police power regulation and compensable regulatory expropriation, preventing governments from externalizing the full cost of regulation onto discrete property owners.
% TRANSFER_FUNCTION: Moves the fiscal burden of regulation-induced value loss from property owners to taxpayers via compensation awards, and moves the political burden of regulatory design from agencies to courts via ad hoc balancing.
% ABSENT_VOICES: Environmental justice communities and future residents who would benefit from stringent land-use regulation are structurally underrepresented in takings litigation; their interests appear only indirectly through agency defense. Progressive property scholars who question the common-law ownership baseline are largely excluded from the doctrinal frame.
% DISAPPEARANCE_RATIONALE: Without the compensation requirement, agencies would face no fiscal liability for value-diminishing regulations, zoning and environmental law would expand without the Penn Central brake, and property owners would absorb regulatory costs directly; the political economy of land use would rearrange toward heavier regulation.
% FOUNDING_PROBLEM: Absolute government power to regulate property without compensation creates insecure property tenure and enables majoritarian factions to impose disproportionate burdens on discrete owners for public benefit.
% FOUNDING_PROBLEM_CORROBORATION: Property rights organizations and some originalist scholars attest the problem is live, citing regulatory burdens on small property owners. Regulatory agencies and progressive legal scholars attest the founding problem has been substantially co-opted by commercial developers seeking windfalls; independent empirical studies on regulatory chill and compensation incidence are mixed and contested.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__regulatory_takings_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because the doctrine channels public funds to private property holders and chills regulation that would benefit diffuse publics. Suppression (0.58) reflects the chilling effect on regulatory alternatives, not direct physical coercion. Theater ratio (0.45) captures the elaborate doctrinal performance of ad hoc balancingâfactors are named and weighted, but outcomes remain unpredictable. Accessibility collapse is moderate (0.40) because narrower readings (physical appropriation only) remain live doctrinal alternatives. Resistance is high (0.72) because agencies, progressive scholars, and environmental groups actively contest the doctrine. The measurement series tracks the doctrine's evolution from Penn Central (1978) through expansion (Lucas era), post-Lingle moderation, and contemporary stabilization.
 *
 * PERSPECTIVAL GAP:
 *   The property owner seat experiences the constraint as protective coordination against majoritarian overreach; the engine should compute a lower effective extraction there. The taxpayer, agency, and regulatory beneficiary seats experience the same constraint as asymmetric extraction that subsidizes property holders and disables necessary regulation; the engine should compute higher effective extraction for these targets. The court seat sits near symmetric: it administers the balancing test and derives institutional legitimacy from the doctrinal role, but does not directly collect or pay.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners are structural beneficiaries (low d) because the constraint channels compensation and veto power to them. Taxpayers and regulatory beneficiaries are structural targets (high d) because they bear fiscal and public-health costs without recourse. Regulatory agencies are targets (high d) because the constraint chills their statutory authority and exposes them to liability. Courts sit as agenda_setter with moderate d: they administer the test and are neither primary beneficiaries nor targets, though their institutional authority is partly tied to the doctrine's continuance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling because its founding problemâprotecting owners against uncompensated regulatory expropriationâremains live in some contexts, but the doctrine has drifted into an ad hoc balancing test that frequently serves commercial developers more than small owners. The coordination function (boundary setting between regulation and compensation) persists, while the extraction function (compensation windfalls and regulatory chill) has grown around it. This is the defining tangled rope signature: genuine coordination and asymmetric extraction coexisting in the same structure, maintained by active judicial enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'Is the Fifth Amendment Takings Clause properly read to protect only against physical appropriation, or also against non-physical regulatory diminution of value?',
    'Historical-originalist analysis of founding-era understanding, plus empirical study of regulatory chill and compensation incidence under competing readings.',
    'If the kernel is strictly physical, this reading is a misreading and its extraction is pure judicial invention; if the kernel encompasses value protection, the reading is legitimated as constitutional enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Whether the Takings Clause kernel encompasses non-physical value diminution').

omega_variable(
    balancing_test_uncertainty,
    'Does the ad hoc balancing test introduced by this reading create more extraction through uncertainty than it prevents through protection?',
    'Quantitative analysis of regulatory chill, agency legal expenditure, and settlement patterns under Penn Central versus categorical rules.',
    'If uncertainty costs exceed protection benefits, the reading leans toward snare; if protection dominates, it leans toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_uncertainty, empirical, 'Whether the ad hoc balancing test generates net extraction via uncertainty').

omega_variable(
    victim_set_boundary,
    'Does the expanded victim set under this reading capture genuine uncompensated losses, or does it subsidize existing property values at public expense?',
    'Economic incidence analysis of compensation awards by property type and income level, compared to regulatory benefits foregone.',
    'If compensation flows primarily to wealthy commercial developers while basic regulation is chilled, the extraction is regressive and asymmetric; if it protects small owners against disproportionate burdens, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary, empirical, 'Whether expanded compensation reaches deserving victims or captures rents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(taki_tr_t10, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(taki_tr_t20, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(taki_tr_t30, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 30, 0.5).
narrative_ontology:measurement(taki_tr_t40, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 40, 0.46).
narrative_ontology:measurement(taki_tr_t45, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 45, 0.45).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(taki_be_t10, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(taki_be_t20, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(taki_be_t30, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement(taki_be_t40, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(taki_be_t45, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 45, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(taki_su_t10, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(taki_su_t20, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(taki_su_t30, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(taki_su_t40, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(taki_su_t45, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 45, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the takings_clause_boundary kernel, decomposed per the epsilon-invariance principle because the physical_appropriation, categorical, and regulatory readings have structurally distinct epsilon values, victim sets, and enforcement requirements. Each reading is authored as a separate constraint story linked by cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
