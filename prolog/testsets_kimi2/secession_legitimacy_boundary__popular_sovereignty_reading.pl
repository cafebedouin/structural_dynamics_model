% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Popular Sovereignty Reading of Secession Legitimacy Boundary
 *   domain: political/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint instantiates the popular sovereignty reading of the
 *   secession legitimacy boundary kernel. It holds that a democratic majority
 *   within a provincial unit possesses ultimate sovereignty and that a
 *   referendum result is self-legitimating, requiring no federal consent.
 *   This reading forecloses the constitutional impossibility reading
 *   (unilateral exit is constitutionally barred) and coexists with
 *   grievance-based readings while influencing treaty-based readings by
 *   subordinating indigenous territorial consent to provincial majority will.
 *   The constraint arranges political authority such that the provincial
 *   majority is both agenda-setter and primary beneficiary, while the federal
 *   government and provincial minorities bear the costs of territorial exit.
 *
 * KEY AGENTS:
 *   - provincial_majority: Primary beneficiary and agenda-setter (powerful/mobile) â drives referendum and claims sovereignty
 *   - federal_government: Primary target (institutional/constrained) â loses territory and authority
 *   - provincial_minorities: Secondary target (powerless/trapped) â bound by majority decision against their will
 *   - indigenous_treaty_holders: Excluded voice (organized/constrained) â territorial rights bypassed by provincial majority decision
 *   - constitutional_courts: Analytical observer (institutional/analytical) â authority subordinated to popular will
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.71).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.65).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Popular Sovereignty Reading of Secession Legitimacy Boundary").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, 'ba0155df-fbed-4db8-8368-25ac809ae168').
narrative_ontology:cs_kernel_codification('ba0155df-fbed-4db8-8368-25ac809ae168', formalized).
narrative_ontology:cs_authority_grounding('ba0155df-fbed-4db8-8368-25ac809ae168', practice).
narrative_ontology:cs_reading_relation('ba0155df-fbed-4db8-8368-25ac809ae168', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('ba0155df-fbed-4db8-8368-25ac809ae168', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('ba0155df-fbed-4db8-8368-25ac809ae168', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('ba0155df-fbed-4db8-8368-25ac809ae168', foundational, popular_sovereignty_supreme).
narrative_ontology:cs_axiom_status(popular_sovereignty_supreme, holdable).
narrative_ontology:cs_axiom_grounding('ba0155df-fbed-4db8-8368-25ac809ae168', popular_sovereignty_supreme, deontological).
narrative_ontology:cs_axiom('ba0155df-fbed-4db8-8368-25ac809ae168', foundational, referendum_as_sole_legitimator).
narrative_ontology:cs_axiom_status(referendum_as_sole_legitimator, holdable).
narrative_ontology:cs_axiom_grounding('ba0155df-fbed-4db8-8368-25ac809ae168', referendum_as_sole_legitimator, conventional).
narrative_ontology:cs_reference_frame('ba0155df-fbed-4db8-8368-25ac809ae168', popular_sovereignty_framework).
narrative_ontology:cs_drift_state('ba0155df-fbed-4db8-8368-25ac809ae168', contemporary_constitutional_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ba0155df-fbed-4db8-8368-25ac809ae168', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds democratic majority within provincial boundaries; sets the secession agenda via referendum and claims self-legitimating authority to declare independence without federal consent. Exits the federation if the referendum passes, acquiring sovereignty and territorial jurisdiction.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority, agenda_setter,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority, beneficiary).

% Loses territorial integrity, tax base, and constitutional supremacy over the province when the provincial majority unilaterally secedes. Must either accept the loss or bear the costs of political and potentially coercive resistance to prevent exit.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government, payer,
    institutional, generational, constrained, national).

% Residents of the province who oppose secession; their federal citizenship, minority protections, and economic affiliations are terminated against their individual will by the majority referendum result. No individual opt-out mechanism is provided.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minorities, payer,
    powerless, biographical, trapped, regional).

% Hold treaty rights to territory and resources within the provincial boundaries. Under the popular sovereignty reading their consent is not required for secession, though their lands and treaty relationships may be unilaterally reconfigured by the provincial majority decision.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders, excluded,
    organized, generational, constrained, regional).

% Adjudicate constitutional ambiguity on secession. Under this reading their authority to block or condition exit is subordinated to the direct democratic expression of provincial popular sovereignty.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a democratically legitimate mechanism for territorial exit when a population within a defined boundary no longer consents to federal membership; coordinates the transition from sub-unit to independent polity without requiring unanimous consent.
% TRANSFER_FUNCTION: Moves sovereignty, territorial jurisdiction, and fiscal control from the federal state to the provincial majority; simultaneously moves federal citizenship and minority protections away from provincial residents who dissent.
% ABSENT_VOICES: Indigenous treaty holders whose territorial rights predate provincial boundaries are not consulted; provincial minorities opposing secession have no veto; the federal electorate outside the province is excluded from the referendum franchise.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished, unilateral secession would lose its primary legitimacy mechanism; provincial exit would require federal negotiation or constitutional amendment; the territorial integrity of federal states would rest on centralized rather than popular-sovereignty foundations.
% FOUNDING_PROBLEM: How to legitimate political exit from a federation when the central government refuses to release a sub-unit, and when the sub-unit's population claims a distinct democratic will that deserves self-governing expression.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists of self-determination and comparative constitutional scholars attest the problem is live; federal governments outside the benefiting parties universally contest that the problem justifies unilateral exit, arguing instead for constitutional unity.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 0.71, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71) is high because the doctrine authorizes a unilateral transfer of sovereignty that extracts federal authority, fiscal base, and territorial control from the central state, and extracts federal citizenship and protections from dissenting residents. Suppression (0.65) reflects the active political enforcement and constitutional override required to make the referendum result stick against federal resistance and internal minority opposition. Theater ratio (0.40) captures the performative dimension of secession referenda that legitimate exit without guaranteeing the institutional capacity to execute sovereign statehood. Accessibility collapse (0.70) is high because once the referendum is won, alternatives to remaining in the federation collapse for the minority and the federal state. Resistance (0.75) is high because both federal governments and provincial minorities actively resist unilateral secession claims.
 *
 * PERSPECTIVAL GAP:
 *   The provincial majority seat experiences the constraint as liberation from federal extraction and democratic self-expression; the federal and minority seats experience it as coercive dissolution of constitutional order without their consent. The engine computes this divergence from the structural data â the authored claim of tangled rope does not adjudicate the perspectival dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   The provincial majority is the beneficiary and agenda-setter, exercising mobile exit from the federation, yielding a directionality near the beneficiary end. The federal government and provincial minorities are the targets, with constrained or trapped exit options, yielding directionality near the full-target end. Indigenous treaty holders are excluded rather than coordinated, losing territorial protections without gaining a seat at the decision.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy by preserving a genuine coordination function: democratic self-determination solves the practical and moral problem of how a population legitimately exits a polity it no longer accepts. However, the asymmetric extraction from minorities and the federal state prevents classification as pure rope. The coexistence of both coordination and extraction functions structurally requires tangled rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minority_coercion_ambiguity,
    'Does the popular sovereignty reading coerce provincial minorities, or merely reflect legitimate democratic collective choice?',
    'Comparative analysis of post-referendum minority rights in secessionist territories versus pre-referendum status, including exit options and citizenship guarantees.',
    'If minorities are systematically worse off and trapped, the reading functions as extraction from the minority; if rights are preserved and individual exit is available, it moves toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_coercion_ambiguity, conceptual, 'Whether the doctrine coerces minorities or expresses legitimate democracy').

omega_variable(
    federal_extraction_perception,
    'Is the popular sovereignty reading a response to genuine federal extraction, or does it construct extraction narratives to legitimate majority preference?',
    'Fiscal and policy flow analysis between the province and federal center pre-referendum, measuring net transfers and policy autonomy gaps.',
    'Documented genuine extraction would justify the reading as remedial; constructed extraction would shift the reading toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_extraction_perception, empirical, 'Whether the reading responds to real or perceived federal extraction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of federal authority and minority dissent structural (constitutional silence on secession) or active (political mobilization against dissenters)?',
    'Examination of pre- and post-referendum minority treatment, federal enforcement actions, and incidence of political violence or legal retaliation.',
    'Active suppression would raise the measured suppression; structural silence would lower it and shift classification emphasis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus active suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement(sece_tr_t32, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 32, 0.45).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(sece_be_t32, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(sece_su_t32, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 32, 0.65).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% The secession_legitimacy_boundary kernel decomposes into four structurally distinct readings. This reading (popular_sovereignty) asserts provincial majority will as the sole legitimating principle; the constitutional_impossibility reading asserts federal constitutional supremacy; the grievance_threshold reading grounds legitimacy in structural injustice; the treaty_primacy reading grounds legitimacy in pre-existing indigenous territorial rights. Each reading carries a distinct epsilon, stakeholder set, and directionality profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
