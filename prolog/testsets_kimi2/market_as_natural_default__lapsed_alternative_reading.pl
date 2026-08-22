% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_alternative_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market as Natural Default (Lapsed Alternative Reading)
 *   domain: political_economy/ideology/economic_history
 *
 * SUMMARY:
 *   This constraint story instantiates the 'lapsed_alternative_reading' of
 *   the kernel 'market_as_natural_default': the ideological and institutional
 *   dominance of market allocation is read not as actively defended by
 *   incumbent beneficiaries, but as a passive drift product of historical
 *   forgetting. The epistemic closure is low-extraction and minimally
 *   theatrical because no concentrated actor maintains it; it persists as a
 *   D3 (deferred decision) artifact where the cost of recovering alternatives
 *   exceeds the perceived benefit for any single agent. The claim is ropeâa
 *   coordination default on unmarked expectationsâwhile the metrics
 *   independently register low extraction and minimal suppression.
 *
 * KEY AGENTS:
 *   - Macroeconomic policymakers: Operate within the market-default frame without needing to defend it; moderate power, constrained exit via institutional path dependency
 *   - Diffuse firms and households: Bear the opportunity cost of foregone alternatives without experiencing the constraint as an imposition; powerless to moderate power, epistemically constrained
 *   - Economic historians and heterodox economists: Analytical observers who recover alternatives but lack institutional voice; analytical power, analytical exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.12).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.15).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, rope).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market as Natural Default (Lapsed Alternative Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology/economic_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, '67b8f04b-171e-482d-aa26-2391fd8d0839').
narrative_ontology:cs_kernel_codification('67b8f04b-171e-482d-aa26-2391fd8d0839', implicit).
narrative_ontology:cs_authority_grounding('67b8f04b-171e-482d-aa26-2391fd8d0839', diffuse_epistemic).
narrative_ontology:cs_reading_relation('67b8f04b-171e-482d-aa26-2391fd8d0839', market_as_natural_default__beneficiary_maintained_reading, forecloses).
narrative_ontology:cs_reading_relation('67b8f04b-171e-482d-aa26-2391fd8d0839', market_as_natural_default__hybrid_amnesia_reading, influences).
narrative_ontology:cs_axiom('67b8f04b-171e-482d-aa26-2391fd8d0839', foundational, no_active_beneficiary_necessary).
narrative_ontology:cs_axiom_status(no_active_beneficiary_necessary, holdable).
narrative_ontology:cs_axiom_grounding('67b8f04b-171e-482d-aa26-2391fd8d0839', no_active_beneficiary_necessary, empirically_contingent).
narrative_ontology:cs_axiom('67b8f04b-171e-482d-aa26-2391fd8d0839', secondary, alternatives_recoverable_by_inquiry).
narrative_ontology:cs_axiom_status(alternatives_recoverable_by_inquiry, holdable).
narrative_ontology:cs_axiom_grounding('67b8f04b-171e-482d-aa26-2391fd8d0839', alternatives_recoverable_by_inquiry, empirically_contingent).
narrative_ontology:cs_reference_frame('67b8f04b-171e-482d-aa26-2391fd8d0839', market_as_unmarked_default).
narrative_ontology:cs_drift_state('67b8f04b-171e-482d-aa26-2391fd8d0839', contemporary_political_economy, gap(stable, minor, false)).
narrative_ontology:cs_created_at('67b8f04b-171e-482d-aa26-2391fd8d0839', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a default institutional expectation that reduces the transaction costs of economic coordination; no party needs to negotiate or defend the market form because it is universally assumed as the unmarked background.
% TRANSFER_FUNCTION: Minimal direct transfer; opportunity cost of foregone alternative institutional arrangements is borne diffusely by all economic actors whose imagination and political demand are bounded by the remembered set.
% ABSENT_VOICES: Historians of economic thought, economic anthropologists, and advocates of alternative allocation mechanisms (cooperative, planned, gift-based) are present in academia but structurally peripheral to policy discourse; their recovery of alternatives does not reach the default decision frameworks of macroeconomic institutions.
% DISAPPEARANCE_RATIONALE: If the 'market as natural default' assumption dissolved and alternatives were actively remembered, policy design, firm organization, and household economic strategy would reorganize around a broader menu of institutional forms; the current arrangement depends on the epistemic closure.
% FOUNDING_PROBLEM: The collapse of earlier alternative economic orders (feudal, planned, cooperative) left a vacuum of institutional imagination; markets expanded as the residual, unmarked form not because they were selected but because the memory of alternatives faded.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and historians of political economy attest to the live presence of alternative institutional forms before the mid-20th century; no contemporary beneficiary group attests the founding problem as still live, consistent with this reading's denial of an identifiable beneficiary class.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.12, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).
:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint extracts only diffuse opportunity cost of foregone alternatives, not concentrated rents. Suppression is low (0.15) because the reading explicitly denies active closure; alternatives are recoverable via research. Theater ratio is low-moderate (0.18) because while market-naturalization rhetoric exists in textbooks and policy discourse, it is not performative maintenance by identifiable beneficiaries but passive reproduction of a settled default. Accessibility collapse is moderate (0.45) because once the historical record is consulted, alternatives become visible, but the initial state presents the market form as the only conceivable background. Resistance is low (0.10) because the default is hegemonic and not experienced as a constraint by those living within it. Measurements share a single time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of diffuse economic actors, the constraint is invisibleâa background assumption rather than an active imposition. From the analytical seat of the historian, the constraint is visible as a contingent closure that could be reopened. The engine will compute near-symmetric directionality for most agents because the costs and benefits of the default are both diffuse; there is no concentrated beneficiary or target.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim declarations are authored, consistent with the reading's claim of no identifiable class. Directionality reverts to the power atom's canonical fallback: diffuse agents at moderate power with constrained exit sit near symmetric. The absence of declared structural relationships means effective extraction is computed close to base extraction for all indices, which is low.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâcoordinating economic activity after the collapse of prior alternativesâis dead. The constraint persists not because it solves a live problem but because the memory of alternatives lapsed. However, because extraction is diffuse and low, and no agenda setter maintains it, the constraint does not compute as a snare or tangled rope. It is claimed as rope because the coordination functionâreduced deliberation cost from shared defaultâis still operationally active even if historically accidental. The piton alternative (atrophied function, theatrical inertia) is recorded as an omega variable rather than the claimed type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine rope (low-extraction coordination default) or a piton (atrophied institutional function maintained only by inertia)?',
    'Historical-institutional analysis tracing whether the ''market as default'' expectation still actively reduces coordination costs or merely persists as cognitive residue.',
    'If atrophied, reclassification to piton would raise theater_ratio threshold and flag the constraint as a target for institutional redesign; if genuine rope, the low extraction is the legitimate cost of expectation coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ontological status of the coordination function in a lapsed-memory default').

omega_variable(
    diffuse_extraction_or_none,
    'Does the absence of identifiable beneficiaries mean extraction is truly zero, or is the extraction so diffuse that it evades class detection?',
    'Agent-based modelling or survey research measuring welfare/opportunity-cost distribution across demographic and sectoral categories under alternative-institutional counterfactuals.',
    'If diffuse extraction is present and significant, the directionality derivation would shift toward mild target status for broad agent classes, potentially pushing the constraint toward tangled_rope or snare territory despite low per-capita extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diffuse_extraction_or_none, empirical, 'Whether diffuse opportunity costs constitute hidden extraction').

omega_variable(
    historical_recovery_efficacy,
    'Can historical research actually recover viable alternative institutional imaginaries, or has the epistemic closure become self-sealing through auxiliary ideological investments?',
    'Comparative policy experiments and deliberative-forum studies testing whether exposure to historical alternatives changes preference orderings or institutional design choices.',
    'If recovery is ineffective, accessibility_collapse is higher than measured and the constraint may be more deeply anchored than the lapsed-memory reading suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_recovery_efficacy, empirical, 'Whether alternatives are genuinely recoverable or only theoretically available').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lapsed_alt_tr_t0, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lapsed_alt_tr_t10, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(lapsed_alt_tr_t20, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(lapsed_alt_tr_t30, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(lapsed_alt_tr_t40, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(lapsed_alt_tr_t50, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 50, 0.18).

% Extraction over time
narrative_ontology:measurement(lapsed_alt_be_t0, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(lapsed_alt_be_t10, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 10, 0.11).
narrative_ontology:measurement(lapsed_alt_be_t20, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 20, 0.11).
narrative_ontology:measurement(lapsed_alt_be_t30, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(lapsed_alt_be_t40, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 40, 0.12).
narrative_ontology:measurement(lapsed_alt_be_t50, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 50, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(market_as_natural_default__lapsed_alternative_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'lapsed_alternative_reading' of the kernel 'market_as_natural_default', decomposed per the Îµ-invariance principle from the 'beneficiary_maintained_reading' and 'hybrid_amnesia_reading' because the referent, beneficiary structure, and Îµ values differ structurally across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
