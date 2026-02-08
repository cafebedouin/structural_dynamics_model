% ============================================================================
% CONSTRAINT STORY: risk_socialization_threshold
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_risk_socialization_threshold, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: risk_socialization_threshold
 * human_readable: The Asymmetric Liability Trap
 * domain: economic/political
 * * SUMMARY:
 * A scenario where a systemic entity (e.g., a "too big to fail" bank or a
 * state-backed utility) privatizes profits during stability but socializes
 * losses during a crisis. The threshold represents the point where the
 * entity's private risk is transferred to the public ledger. It functions
 * as a Rope for short-term economic coordination but becomes a massive
 * Snare for taxpayers who bear the unpriced downside.
 * * KEY AGENTS:
 * - Average Taxpayer: Subject (Powerless)
 * - Systemic Institution: Beneficiary (Institutional)
 * - Forensic Macroeconomist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) because the systemic "safety net" siphons public
% wealth to protect private equity, liquidating the subject's future surplus.
domain_priors:base_extractiveness(risk_socialization_threshold, 0.88).
domain_priors:suppression_score(risk_socialization_threshold, 0.73).
domain_priors:theater_ratio(risk_socialization_threshold, 0.85). % High theater: "Market stability" used as moral justification.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(risk_socialization_threshold, extractiveness, 0.88).
narrative_ontology:constraint_metric(risk_socialization_threshold, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(risk_socialization_threshold, theater_ratio, 0.85).

% Constraint self-claim: It claims to be a coordination mechanism for stability.
narrative_ontology:constraint_claim(risk_socialization_threshold, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(risk_socialization_threshold). % Required for Tangled Rope. Bailouts are enforced by law.

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(risk_socialization_threshold, systemic_institutions).
narrative_ontology:constraint_victim(risk_socialization_threshold, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the taxpayer, the threshold is a snare: they receive no upside
% but are legally bound to cover the systemic downside.
constraint_indexing:constraint_classification(risk_socialization_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the threshold as a Rope—the only way to coordinate
% capital at a scale that ensures systemic continuity.
constraint_indexing:constraint_classification(risk_socialization_threshold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature of coordination intent (Rope)
% entangled with extreme, systemic extraction (Snare).
constraint_indexing:constraint_classification(risk_socialization_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.85) > 0.70 triggers Piton: the "risk management" is
% a performative facade for a pure wealth transfer.
constraint_indexing:constraint_classification(risk_socialization_threshold, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(risk_socialization_threshold, TR), TR > 0.70.


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(risk_socialization_threshold_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(risk_socialization_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(risk_socialization_threshold, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless = snare,
    TypeInstitutional = rope.

test(analytical_classification_is_tangled_rope) :-
    % The canonical analytical view should resolve the conflict as a Tangled Rope.
    constraint_indexing:constraint_classification(risk_socialization_threshold, tangled_rope,
        context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global))).

test(piton_detection_on_high_theater) :-
    % An auditor focused on performativity should see a Piton.
    constraint_indexing:constraint_classification(risk_socialization_threshold, piton,
        context(agent_power(analytical), time_horizon(historical), exit_options(arbitrage), spatial_scope(universal))).

:- end_tests(risk_socialization_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects a "Mandatrophy" state where the
 * "coordination" benefit of systemic stability is essentially a pretext
 * for predatory resource siphoning. The suppression score (0.73) reflects
 * the legal and political impossibility for taxpayers to opt out.
 * The high theater ratio (0.85) is key, justifying the Piton classification
 * from an auditor's perspective; the public narrative of "stability" is
 * almost entirely performative compared to the raw function of wealth transfer.
 *
 * PERSPECTIVAL GAP:
 * The Average Taxpayer feels a Snare because they pay for a "safety net"
 * they do not control and cannot exit. The Systemic Institution sees a
 * Rope because it allows for high-risk coordination that would otherwise
 * be impossible under true market conditions.
 *
 * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The system resolves this by classifying the constraint
 * as a Tangled Rope from the canonical analytical perspective. This acknowledges
 * the genuine (if self-serving) coordination function for beneficiaries while
 * correctly identifying the asymmetric, coercive extraction imposed on victims.
 * The additional Piton classification highlights the decay of the public-facing
 * justification into pure theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_threshold_volatility,
    'Can the threshold be mathematically fixed, or is it a political variable (Snare vs Mountain)?',
    'Tracking the delta between institutional "reserve" levels and actual bailout amounts over multiple economic cycles.',
    'If fixed: Mountain of Math. If moving: Snare of Political Arbitrage.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(risk_socialization_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint likely began as a legitimate, low-extraction backstop and
% degraded over time through regulatory capture and moral hazard, accumulating
% both extraction and theatrical justification.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(rst_tr_t0, risk_socialization_threshold, theater_ratio, 0, 0.20).
narrative_ontology:measurement(rst_tr_t5, risk_socialization_threshold, theater_ratio, 5, 0.55).
narrative_ontology:measurement(rst_tr_t10, risk_socialization_threshold, theater_ratio, 10, 0.85).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(rst_ex_t0, risk_socialization_threshold, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(rst_ex_t5, risk_socialization_threshold, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(rst_ex_t10, risk_socialization_threshold, base_extractiveness, 10, 0.88).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The mechanism functions by reallocating public resources
% to cover private losses, which is a form of resource allocation.
narrative_ontology:coordination_type(risk_socialization_threshold, resource_allocation).

% Network relationships: This policy directly increases the public debt burden.
narrative_ontology:affects_constraint(risk_socialization_threshold, public_debt_burden).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */