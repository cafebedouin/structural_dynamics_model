% ============================================================================
% CONSTRAINT STORY: hp_liberalism
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_hp_liberalism, []).

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
 * * constraint_id: hp_liberalism
 * human_readable: The Potterverse Liberalism Constraint
 * domain: socio_political
 * * SUMMARY:
 * This constraint represents the "Mirror of Erised" effect where 1990s liberal
 * virtues (tolerance, non-violence, institutional trust), embodied by the Harry
 * Potter narrative, act as a coordination mechanism for one generation but are
 * perceived as an extractive or atrophied trap by a subsequent generation
 * facing different material conditions.
 * * KEY AGENTS:
 * - Millennial Believer: Subject (Powerless/Trapped in nostalgia)
 * - Corporate IP Holders: Beneficiary (Institutional/Mobile)
 * - The Zoomer Skeptic: Auditor (Analytical/Seeking Exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.52) due to corporate monetization of nostalgia despite
% waning socio-political utility for the youngest cohort.
domain_priors:base_extractiveness(hp_liberalism, 0.52). % Snare extraction >= 0.46
domain_priors:suppression_score(hp_liberalism, 0.45).   % Suppression of alternative modern mythologies.
domain_priors:theater_ratio(hp_liberalism, 0.78).       % High "theatrical" maintenance in spinoffs (Piton >= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(hp_liberalism, extractiveness, 0.52).
narrative_ontology:constraint_metric(hp_liberalism, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(hp_liberalism, theater_ratio, 0.78).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(hp_liberalism, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(hp_liberalism). % Required for Tangled Rope (maintaining "canon" moral guides).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(hp_liberalism, corporate_ip_holders).
narrative_ontology:constraint_victim(hp_liberalism, younger_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE MILLENNIAL "HP ADULT" (SNARE)
% Trapped by nostalgia, the high extraction from monetization is felt directly.
% χ = 0.52 * 1.5 (powerless) * 1.0 (national) = 0.78. This is a clear Snare.
constraint_indexing:constraint_classification(hp_liberalism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE IP BENEFICIARY / WARNER BROS (ROPE)
% Experiences negative extraction, viewing the system as pure coordination.
% χ = 0.52 * -0.2 (institutional) * 1.2 (global) = -0.12. This is a Rope.
constraint_indexing:constraint_classification(hp_liberalism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees the full structure: a coordination function with asymmetric extraction.
% χ = 0.52 * 1.15 (analytical) * 1.2 (global) = 0.7176. High extraction.
% With beneficiary, victim, and enforcement, this is a canonical Tangled Rope.
constraint_indexing:constraint_classification(hp_liberalism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ZOOMER SKEPTIC (PITON)
% Sees the constraint as a non-functional, inertial remnant of an affluent era.
% The high theater ratio (0.78) makes this a Piton from this perspective.
constraint_indexing:constraint_classification(hp_liberalism, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(national))) :-
    domain_priors:theater_ratio(hp_liberalism, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hp_liberalism_tests).

test(perspectival_gap) :-
    % Verify the gap between the powerless (Snare) and institutional (Rope).
    constraint_indexing:constraint_classification(hp_liberalism, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hp_liberalism, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(hp_liberalism, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(hp_liberalism, E),
    E >= 0.46. % Confirms it's a high-extraction constraint.

test(piton_classification) :-
    domain_priors:theater_ratio(hp_liberalism, TR),
    TR > 0.70,
    constraint_indexing:constraint_classification(hp_liberalism, piton, context(agent_power(analytical), _, _, _)).

:- end_tests(hp_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Perspectival Gap is stark. For the institutional beneficiary, the IP is a
 * pure coordination 'Rope' that generates revenue (negative extraction). For the
 * powerless subject trapped by nostalgia, the constant monetization and ideological
 * baggage make it a 'Snare'. The analytical observer, accounting for both the
 * genuine coordination function and the asymmetric extraction, classifies it as
 * a 'Tangled Rope'. Finally, another analytical view focused on the endless,
 * low-substance spinoffs and performative defenses of the canon correctly sees
 * a 'Piton'—a structure whose original function has atrophied into theater.
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The system prevents mislabeling this as a pure Snare by acknowledging its
 * 'Tangled Rope' status from the analytical perspective. While corporate entities
 * extract immense value (E=0.52), the narrative still provides a genuine (if
 * aging) coordination function for a large community, preventing the
 * classification from collapsing into a pure Snare across all indices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_hp_resilience,
    'Will the HBO adaptation successfully re-index the constraint as a Rope for a new generation?',
    'Market share and sentiment analysis of the under-18 demographic upon 2026 release.',
    'Success = Reversion to Tangled Rope with lower theater; Failure = Hardens into a pure Piton.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing. Interval represents 1997-2026.
narrative_ontology:interval(hp_liberalism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the constraint's drift from a coordination mechanism
% into an extractive, theatrical one.
% T=0 (1997), T=5 (~2011, post-movies), T=10 (~2026, spinoff era).

% Theater ratio over time (metric_substitution drift):
narrative_ontology:measurement(hp_liberalism_tr_t0, hp_liberalism, theater_ratio, 0, 0.10).
narrative_ontology:measurement(hp_liberalism_tr_t5, hp_liberalism, theater_ratio, 5, 0.40).
narrative_ontology:measurement(hp_liberalism_tr_t10, hp_liberalism, theater_ratio, 10, 0.78).

% Extraction over time (extraction_accumulation drift):
narrative_ontology:measurement(hp_liberalism_ex_t0, hp_liberalism, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(hp_liberalism_ex_t5, hp_liberalism, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(hp_liberalism_ex_t10, hp_liberalism, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint's primary function is to provide a shared moral/social vocabulary.
narrative_ontology:coordination_type(hp_liberalism, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */