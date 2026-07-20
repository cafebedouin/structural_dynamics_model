% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__physical_appropriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__physical_appropriation_reading, []).

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
 *   constraint_id: takings_clause_boundary__physical_appropriation_reading
 *   human_readable: Takings Clause Physical Appropriation Reading
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   This constraint instantiates the physical appropriation reading of the
 *   Takings Clause kernel (kernel_id: takings_clause_boundary). It holds that
 *   the Fifth Amendment's Just Compensation Clause requires payment only when
 *   government directly seizes or permanently occupies private property;
 *   non-physical regulatory diminutions, however severe, are non-compensable
 *   background risks of property ownership. This reading creates a
 *   bright-line rule that preserves broad regulatory power for government
 *   while leaving property owners to bear regulatory costs. It is one of
 *   three competing readings; the categorical and regulatory takings readings
 *   are modeled as separate constraints in the constraint family.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Agenda-setter (institutional/analytical) â interprets and enforces the narrow compensation rule
 *   - state_local_governments: Primary beneficiary (institutional/constrained) â retains regulatory power without fiscal liability
 *   - regulated_property_owners: Primary payer (moderate/constrained) â bears regulatory losses without compensation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.62).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.66).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Takings Clause Physical Appropriation Reading").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, '0876d2cb-390a-4fed-80f7-9598caf2f523').
narrative_ontology:cs_kernel_codification('0876d2cb-390a-4fed-80f7-9598caf2f523', fixed_text).
narrative_ontology:cs_authority_grounding('0876d2cb-390a-4fed-80f7-9598caf2f523', lineage).
narrative_ontology:cs_interpretation_layer_present('0876d2cb-390a-4fed-80f7-9598caf2f523').
narrative_ontology:cs_reading_relation('0876d2cb-390a-4fed-80f7-9598caf2f523', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('0876d2cb-390a-4fed-80f7-9598caf2f523', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('0876d2cb-390a-4fed-80f7-9598caf2f523', foundational, physical_seizure_constitutional_core).
narrative_ontology:cs_axiom_status(physical_seizure_constitutional_core, holdable).
narrative_ontology:cs_axiom_grounding('0876d2cb-390a-4fed-80f7-9598caf2f523', physical_seizure_constitutional_core, empirically_contingent).
narrative_ontology:cs_axiom('0876d2cb-390a-4fed-80f7-9598caf2f523', foundational, regulatory_burden_background_risk).
narrative_ontology:cs_axiom_status(regulatory_burden_background_risk, holdable).
narrative_ontology:cs_axiom_grounding('0876d2cb-390a-4fed-80f7-9598caf2f523', regulatory_burden_background_risk, conventional).
narrative_ontology:cs_reference_frame('0876d2cb-390a-4fed-80f7-9598caf2f523', physical_appropriation_framework).
narrative_ontology:cs_drift_state('0876d2cb-390a-4fed-80f7-9598caf2f523', modern_regulatory_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0876d2cb-390a-4fed-80f7-9598caf2f523', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, state_local_governments).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, regulated_property_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Fifth Amendment to require compensation only for direct physical seizures or permanent physical occupations of private property; dismisses regulatory takings claims that lack physical appropriation; maintains the rule as a matter of constitutional text, original public meaning, and judicial administrability.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Exercise police power over zoning, land use, environmental protection, and public health without owing just compensation unless they physically seize or permanently occupy private property; retain broad fiscal and policy flexibility that would contract under broader takings readings.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, state_local_governments, beneficiary,
    institutional, generational, constrained, national).

% Own property subject to zoning downzoning, environmental restrictions, permit moratoria, and other non-physical regulations that diminish economic value; bear these losses as non-compensable background risks because their claims lack physical seizure or occupation; litigation exit is blocked by the physical appropriation rule.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, regulated_property_owners, payer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__physical_appropriation_reading, state_local_governments).
narrative_ontology:fixing_cost_class(takings_clause_boundary__physical_appropriation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, judicially administrable standard for separating compensable exercises of eminent domain from non-compensable police power regulations, reducing litigation uncertainty and preventing regulatory paralysis from the threat of pervasive compensation liability.
% TRANSFER_FUNCTION: Shifts the fiscal and economic burden of non-physical land-use and environmental regulation from the public fisc to private property owners; transfers regulatory flexibility and fiscal certainty to state and local governments.
% ABSENT_VOICES: Property owners suffering non-physical regulatory losses are formally present in court but their claims are systematically dismissed; the broader public that might prefer stricter regulatory accountability is not party to takings litigation. No party is structurally excluded from the legal conversation, though the doctrinal framework renders regulatory-takings arguments dead on arrival.
% DISAPPEARANCE_RATIONALE: If the physical appropriation limit vanished overnight, state and local governments would face immediate exposure for regulatory diminutions under Penn Central-style balancing; property owners would file a wave of new claims; judicial dockets would swell; and the fiscal and political calculus of zoning, environmental, and historic-preservation regulation would fundamentally change.
% FOUNDING_PROBLEM: The need for a determinate constitutional standard to distinguish compensable takings from non-compensable police power regulations, preventing government paralysis and giving courts a manageable rule.
% FOUNDING_PROBLEM_CORROBORATION: State and local government associations attest the bright-line rule is necessary for regulatory function. Independent legal historians and constitutional scholars outside the beneficiary set provide contested corroboration: some originalist scholars support the historical premise, while many property-law historians argue the 1791 understanding of 'taken' was broader or more ambiguous, undermining the claimed founding rationale.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__physical_appropriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__physical_appropriation_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__physical_appropriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__physical_appropriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the constraint systematically shifts the costs of public regulation to private property holders by denying compensation for non-physical losses. Suppression (0.66) is high because the rule actively forecloses regulatory takings claims in federal court, collapsing the alternative Penn Central balancing test for physical-appropriation-excluded claims. Theater ratio (0.38) reflects moderate performative maintenance: originalist and textualist arguments provide a methodological veneer that partially masks a policy choice to immunize regulation from fiscal accountability. Accessibility collapse (0.48) is moderateâonce the physical appropriation framework is accepted, regulatory takings alternatives become nearly inaccessible to litigants, though they persist in scholarly and dissenting discourse. Resistance (0.45) is moderate: property rights advocates and some jurists persistently challenge the narrow rule, but the doctrine remains entrenched.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary and state governments experience this constraint as a necessary bulwark against regulatory paralysis and fiscal uncertaintyâa coordination device that prevents every land-use decision from triggering constitutional litigation. Regulated property owners experience it as an asymmetric cost-shield: they bear the full burden of public regulation without the compensation that would flow under broader readings. The engine computes this divergence from the structural data (beneficiary/victim declarations and exit modulation) rather than from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   State and local governments are declared beneficiaries with constrained exit (bound by federal constitutional interpretation but structurally subsidized by the rule), yielding a low directionality value near the beneficiary pole. Regulated property owners are declared victims with constrained exit (litigation is their primary exit and the doctrine blocks it), yielding a high directionality value near the target pole. The federal judiciary sits near the analytical middle as agenda-setter with analytical exit, though its institutional interest in administrability pulls it slightly toward the beneficiary side.
 *
 * MANDATROPHY ANALYSIS:
 *   The bright-line coordination functionâproviding courts and regulators with a determinate standardâprevents mislabeling this constraint as a pure snare. Without the genuine administrability problem it solves, the constraint would be indistinguishable from raw government cost-shifting. However, the coordination function does not eliminate the asymmetric extraction; it merely means the extraction rides on a real structural necessity. Mandatrophy would occur if the coordination function were proven illusoryâif the physical appropriation line turned out to be no more administrable than Penn Central balancingâat which point the constraint would degrade toward snare or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_public_meaning_empirical,
    'Does the original public meaning of ''taken'' in 1791 exclusively encompass direct physical seizure and permanent occupation, excluding regulatory diminution?',
    'Corpus linguistics analysis of 1791 legal and lay usage; examination of Founding-era state compensation statutes and eminent domain practice.',
    'If ''taken'' included regulatory burdens, the physical appropriation reading rests on a false empirical premise and its authority grounding collapses toward conventional policy choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_empirical, empirical, 'Empirical basis of the physical appropriation axiom').

omega_variable(
    coordination_extraction_boundary,
    'Is the physical appropriation rule a genuinely necessary coordination mechanism for judicial administrability, or primarily a cost-shield for government regulation?',
    'Comparative analysis of jurisdictions with broader regulatory takings doctrines to measure regulatory output, litigation rates, and fiscal burden; evaluation of whether the bright-line rule reduces uncertainty or merely truncates legitimate claims.',
    'If the rule is necessary coordination, its extraction is in the tolerated cost zone; if it is primarily cost-shielding, reclassification toward snare is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Coordination function versus extraction function of the rule').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(taki_tr_t6, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(taki_tr_t12, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(taki_tr_t18, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 18, 0.32).
narrative_ontology:measurement(taki_tr_t24, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(taki_tr_t30, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(taki_be_t6, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(taki_be_t12, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(taki_be_t18, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(taki_be_t24, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(taki_be_t30, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(taki_su_t6, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(taki_su_t12, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(taki_su_t18, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement(taki_su_t24, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(taki_su_t30, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 30, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, categorical_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, regulatory_takings_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'takings_clause_boundary' conflates three structurally distinct interpretive constraints. Each reading produces a different epsilon, different beneficiary/victim structure, and different classification. They are modeled as a constraint family linked by network edges, not as a single constraint with measurement-dependent classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
