% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine (Penn Central Ad Hoc Balancing)
 *   domain: constitutional_law/property_rights
 *
 * SUMMARY:
 *   This constraint instantiates the regulatory_takings_reading of the
 *   takings_clause_boundary kernel. The Fifth Amendment Takings Clause is
 *   read through Penn Central's ad hoc balancing framework: regulations that
 *   severely diminish economic value without physically appropriating
 *   property may constitute a taking requiring compensation. This reading
 *   expands the protected party set beyond physical seizure to include severe
 *   value diminution, introduces judicial balancing of public purpose against
 *   economic impact, and generates distinct beneficiary and victim structures
 *   compared to its sibling readings.
 *
 * KEY AGENTS:
 *   - property_owners: Primary beneficiaries (moderate/constrained) â invoke doctrine to shield property value from regulation.
 *   - commercial_developers: Concentrated beneficiaries (powerful/mobile) â strategically litigate to extract compensation or force regulatory concessions.
 *   - municipal_governments: Primary payers (institutional/constrained) â bear compensation liability and regulatory chill costs.
 *   - federal_judiciary: Agenda setter (institutional/analytical) â administers the balancing test and controls doctrinal boundaries.
 *   - community_residents: Excluded victims (powerless/constrained) â suffer externalities from regulatory chill but have no seat in the balancing.
 *   - taxpayers: Diffuse payers (moderate/constrained) â fund compensation awards without direct representation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.62).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.58).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine (Penn Central Ad Hoc Balancing)").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional_law/property_rights").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, '79b907b0-dce8-4902-899d-babd54a5a1cf').
narrative_ontology:cs_kernel_codification('79b907b0-dce8-4902-899d-babd54a5a1cf', fixed_text).
narrative_ontology:cs_authority_grounding('79b907b0-dce8-4902-899d-babd54a5a1cf', lineage).
narrative_ontology:cs_interpretation_layer_present('79b907b0-dce8-4902-899d-babd54a5a1cf').
narrative_ontology:cs_reading_relation('79b907b0-dce8-4902-899d-babd54a5a1cf', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('79b907b0-dce8-4902-899d-babd54a5a1cf', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('79b907b0-dce8-4902-899d-babd54a5a1cf', foundational, nonphysical_regulatory_diminution_may_constitute_taking).
narrative_ontology:cs_axiom_status(nonphysical_regulatory_diminution_may_constitute_taking, holdable).
narrative_ontology:cs_axiom_grounding('79b907b0-dce8-4902-899d-babd54a5a1cf', nonphysical_regulatory_diminution_may_constitute_taking, conventional).
narrative_ontology:cs_axiom('79b907b0-dce8-4902-899d-babd54a5a1cf', foundational, ad_hoc_case_by_case_balancing_required).
narrative_ontology:cs_axiom_status(ad_hoc_case_by_case_balancing_required, holdable).
narrative_ontology:cs_axiom_grounding('79b907b0-dce8-4902-899d-babd54a5a1cf', ad_hoc_case_by_case_balancing_required, conventional).
narrative_ontology:cs_reference_frame('79b907b0-dce8-4902-899d-babd54a5a1cf', constitutional_property_protection_framework).
narrative_ontology:cs_drift_state('79b907b0-dce8-4902-899d-babd54a5a1cf', post_penn_central_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('79b907b0-dce8-4902-899d-babd54a5a1cf', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_owners).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, commercial_developers).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, municipal_governments).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own real property subject to land-use and environmental regulations. Can invoke the regulatory takings doctrine to demand compensation when regulation severely diminishes economic value without physically appropriating the property. Their exit is limited by real estate illiquidity and the stickiness of local markets.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_owners, beneficiary,
    moderate, biographical, constrained, national).

% Invest in large-scale land development and rely on the doctrine to challenge zoning and environmental restrictions. Strategically use regulatory takings litigation to extract compensation awards or force regulatory concessions, capturing concentrated gains from the balancing test's uncertainty.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, commercial_developers, beneficiary,
    powerful, biographical, mobile, national).

% Enact zoning, environmental, and land-use regulations to manage growth and public welfare. Face direct exposure to compensation liability and litigation costs when regulations diminish property value, which chills aggressive regulation and diverts local budgets to private compensation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, municipal_governments, payer,
    institutional, generational, constrained, local).

% Interprets the Takings Clause through evolving precedent, administering the Penn Central balancing test to weigh the public purpose of regulation against its economic impact on property owners. Controls the doctrinal boundaries of what constitutes going too far.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Bear externalities such as pollution or overdevelopment when regulations are chilled by the threat of compensation liability. They are not parties to takings litigation and their interest in clean neighborhoods or stable land use is not directly weighed in the balancing test.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, community_residents, excluded,
    powerless, biographical, constrained, local).

% Fund compensation awards and litigation costs through municipal and state taxes. They do not choose whether to pay for takings judgments; the cost is diffuse but real, shifting public resources from services to private compensation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, taxpayers, payer,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__regulatory_takings_reading, commercial_developers).
narrative_ontology:fixing_cost_class(takings_clause_boundary__regulatory_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the government's interest in regulating land use and environmental harm against the property owner's interest in retaining viable economic use, preventing regulators from externalizing the full cost of public benefits onto individual property holders.
% TRANSFER_FUNCTION: Moves the cost of severe regulatory limitation from individual property owners to municipal governments and taxpayers via compensation awards, and moves uncertainty about regulatory validity from legislatures to courts and regulated parties.
% ABSENT_VOICES: Community residents who benefit from zoning and environmental rules are not parties to takings litigation; their interest in regulatory stability is absent from the Penn Central balancing. Taxpayers who fund compensation are also unrepresented in the case-by-case analysis.
% DISAPPEARANCE_RATIONALE: If the regulatory takings doctrine vanished, municipalities would reallocate risk internally without compensation exposure, property owners would lose a constitutional shield against value-diminishing regulation, and the relative power between local planning and private development would shift sharply toward unilateral regulatory authority.
% FOUNDING_PROBLEM: Government regulators could effectively confiscate property value through severe non-physical restrictions without triggering the Takings Clause's compensation requirement, treating the power to regulate as a means to achieve ends that would require eminent domain if pursued directly.
% FOUNDING_PROBLEM_CORROBORATION: Property-rights legal historians attest to the historical problem of regulatory circumvention outside the beneficiary set; municipal associations and progressive constitutional scholars dispute that non-physical diminution was ever the intended target of the Clause, arguing the doctrine itself is the pathology. The corroboration record is split, with no dominant external consensus.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.62) reflects that the doctrine systematically transfers public funds and regulatory autonomy to private property holders, with commercial developers capturing concentrated gains. Suppression (0.58) captures the chilling effect on regulatory innovation: agencies avoid bold land-use rules to evade liability. Theater ratio (0.30) is moderate because the balancing test performs real adjudicative work, though a portion of judicial activity rehearses formulas that mask policy discretion. Accessibility collapse (0.45) is incomplete because regulators retain alternative tools (permitting, incentives) but must navigate around compensation risk. Resistance (0.55) is substantial from municipal associations and progressive legal scholars who contest the doctrine's legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   A payer seat (municipal government) experiences the constraint as an externalized cost imposed by distant courts: it loses planning capacity and must tax residents to compensate developers. A beneficiary seat (commercial developer) experiences the same constraint as a legitimate constitutional protection against regulatory overreach. The engine computes this divergence from beneficiary/victim declarations and exit asymmetry; the authored claim does not resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners and commercial developers are structural beneficiaries: the constraint subsidizes their property expectations and lowers their cost of resisting regulation (low d). Municipal governments and taxpayers are structural targets: the constraint extracts fiscal resources and policy autonomy from them (high d). The federal judiciary sits near symmetric in directionality â it neither collects compensation nor pays it, but wields authoritative control over the test's application. Community residents are excluded from the derivation chain because their seat is not in the conversation.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine prevents mislabeling by preserving a genuine coordination function: without some check, regulators could indeed impose public burdens on individual property holders without collective cost-sharing. However, the metrics do not collapse to rope because the asymmetric extraction is real (taxpayers and municipalities pay; developers capture), active enforcement is required (courts must constantly police the boundary), and alternatives are suppressed (regulatory chill). The classification as tangled_rope captures both the real coordination and the real extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    takings_kernel_boundary,
    'Does the regulatory takings reading extend the physical-appropriation kernel continuously, or instantiate a distinct constraint with independent beneficiary and victim structures?',
    'Doctrinal genealogy tracing whether Penn Central balancing was implicit in the original public meaning or a later interpretive innovation; structural comparison of Îµ and victim/beneficiary profiles across the three sibling readings.',
    'If the reading is structurally discontinuous, its authority grounding shifts from lineage to extraction, raising the extraction floor and potentially reclassifying the judicial role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(takings_kernel_boundary, conceptual, 'Continuity between regulatory takings and physical appropriation readings').

omega_variable(
    regulatory_chill_empirical,
    'Does the doctrine actually suppress regulations that would otherwise be enacted, or is regulatory chill largely theoretical?',
    'Comparative regulatory output studies in jurisdictions with strong versus weak regulatory takings doctrines; qualitative agency interviews on litigation risk and permitting behavior.',
    'If chill is substantial and empirically verified, suppression is higher than structurally measured and the extraction profile strengthens; if chill is negligible, the constraint moves toward a purer coordination classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chill_empirical, empirical, 'Empirical magnitude of regulatory chill').

omega_variable(
    balancing_test_stability,
    'Is the ad hoc balancing test structurally stable enough to function as coordination, or does its indeterminacy convert it into an extraction mechanism for property owners?',
    'Longitudinal analysis of case outcomes: if results are unpredictable and correlate with litigant resources rather than rule structure, indeterminacy is extractive; if outcomes cluster around stable factors, the test functions as coordination.',
    'High indeterminacy correlated with resource advantages would support reclassification toward snare; stable factor clustering would support the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_stability, conceptual, 'Indeterminacy as coordination feature or extraction bug').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 0, 46).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reg_takings_tr_t0, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(reg_takings_tr_t9, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 9, 0.25).
narrative_ontology:measurement(reg_takings_tr_t18, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement(reg_takings_tr_t27, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 27, 0.35).
narrative_ontology:measurement(reg_takings_tr_t36, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 36, 0.33).
narrative_ontology:measurement(reg_takings_tr_t46, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 46, 0.3).

% Extraction over time
narrative_ontology:measurement(reg_takings_be_t0, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(reg_takings_be_t9, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 9, 0.48).
narrative_ontology:measurement(reg_takings_be_t18, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(reg_takings_be_t27, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 27, 0.61).
narrative_ontology:measurement(reg_takings_be_t36, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 36, 0.63).
narrative_ontology:measurement(reg_takings_be_t46, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 46, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(reg_takings_su_t0, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(reg_takings_su_t9, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 9, 0.52).
narrative_ontology:measurement(reg_takings_su_t18, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(reg_takings_su_t27, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 27, 0.63).
narrative_ontology:measurement(reg_takings_su_t36, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 36, 0.61).
narrative_ontology:measurement(reg_takings_su_t46, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 46, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__categorical_takings_reading).

% DUAL FORMULATION NOTE:
% This constraint is the regulatory_takings_reading of the takings_clause_boundary kernel, distinct from physical_appropriation_reading and categorical_takings_reading. The Takings Clause label conflates structurally distinct claims: one limited to physical seizure, one using categorical rules, and this one using ad hoc balancing for non-physical diminution. Each reading has different Îµ, beneficiary/victim structures, and failure modes, and is authored as a separate constraint linked by the family network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
