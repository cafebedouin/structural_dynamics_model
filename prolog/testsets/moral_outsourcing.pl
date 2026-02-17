% ============================================================================
% CONSTRAINT STORY: moral_outsourcing
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_moral_outsourcing, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: moral_outsourcing
 * human_readable: The Ethical Externalization Loop
 * domain: social/economic/technological
 * * SUMMARY:
 * A scenario where a system or institution delegitimizes individual ethical
 * agency by automating decision-making or deferring responsibility to
 * algorithmic or bureaucratic frameworks. This functions as a "Rope" for
 * large-scale coordination and liability mitigation but acts as a "Snare"
 * for the subject, whose moral intuition is siphoned into a "follow-protocol"
 * mandate, liquidating their ability to act on localized context.
 * * KEY AGENTS:
 * - Protocol Adherent: Subject (Powerless)
 * - Institutional Architect: Beneficiary (Institutional)
 * - Ethical Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) because the system siphons the subject's
% primary moral agency to maintain the liability-free coordination
% of the institution.
domain_priors:base_extractiveness(moral_outsourcing, 0.84).
domain_priors:suppression_score(moral_outsourcing, 0.76).
domain_priors:theater_ratio(moral_outsourcing, 0.88). % Extreme theater: "Ethical AI" or "Compliance" branding.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(moral_outsourcing, extractiveness, 0.84).
narrative_ontology:constraint_metric(moral_outsourcing, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(moral_outsourcing, theater_ratio, 0.88).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(moral_outsourcing, tangled_rope).
narrative_ontology:human_readable(moral_outsourcing, "The Ethical Externalization Loop").

% Binary flags and structural properties for Tangled Rope classification
domain_priors:requires_active_enforcement(moral_outsourcing).
narrative_ontology:constraint_beneficiary(moral_outsourcing, institutional_architects).
narrative_ontology:constraint_victim(moral_outsourcing, protocol_adherents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the powerless agent, the outsourcing is a snare: they are legally
% and socially punished if they override the protocol, even when it is
% clearly harmful.
constraint_indexing:constraint_classification(moral_outsourcing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views outsourcing as a vital Rope—the only way to
% coordinate ethical standards across millions of transactions with
% near-zero variance.
constraint_indexing:constraint_classification(moral_outsourcing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.88) > 0.70 triggers Piton: the "ethical framework"
% is an inert spike of logic that protects the institution from
% liability rather than generating moral outcomes.
constraint_indexing:constraint_classification(moral_outsourcing, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(moral_outsourcing, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.84) and suppression (0.76) masking as coordination.
% The presence of beneficiaries, victims, and active enforcement confirms Tangled Rope.
constraint_indexing:constraint_classification(moral_outsourcing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moral_outsourcing_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(moral_outsourcing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moral_outsourcing, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classifications) :-
    % Verify that analytical observers detect both Piton and Tangled Rope aspects.
    constraint_indexing:constraint_classification(moral_outsourcing, piton, context(agent_power(analytical), time_horizon(historical), _, _)),
    constraint_indexing:constraint_classification(moral_outsourcing, tangled_rope, context(agent_power(analytical), time_horizon(civilizational), _, _)).

test(threshold_validation) :-
    % Ensure high extraction and theater ratios are correctly registered.
    narrative_ontology:constraint_metric(moral_outsourcing, extractiveness, E),
    narrative_ontology:constraint_metric(moral_outsourcing, theater_ratio, TR),
    E >= 0.46,
    TR >= 0.70.

:- end_tests(moral_outsourcing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects the liquidation of the subject's
 * moral agency, which is siphoned to create a liability shield for the
 * institution. The high suppression (0.76) represents the professional and
 * social cost of defying the protocol. The extreme theater ratio (0.88)
 * comes from the extensive "Ethical AI" or "Compliance" branding that
 * obscures the extractive function.
 *
 * * PERSPECTIVAL GAP:
 * The Protocol Adherent feels a Snare because they are forced to
 * suppress their conscience to keep their job. The Institutional
 * Architect sees a Rope because the protocol ensures global
 * consistency and eliminates individual "ethical noise," reducing liability.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction (0.84) indicates a state of Mandatrophy, where a
 * mandate's original purpose has been corrupted into a self-serving
 * extractive loop. This is resolved by the Tangled Rope classification.
 * A simpler analysis might label this a pure Snare, but that would ignore
 * the genuine (if now parasitic) coordination function that benefits the
 * institution. Tangled Rope correctly identifies the dual nature: it is
 * both a coordination mechanism AND an extractive one. The Piton
 * classification further refines this, showing that from another analytical
 * view, the claimed function has become so theatrical it is effectively inert.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_moral_outsourcing,
    'Can an automated system truly simulate moral responsibility, or is it always a Mountain of computational limits disguised as a Snare of policy?',
    'Tracking the frequency of "unforeseen harm" events in protocol-driven vs. human-discretion environments over a civilizational timescale.',
    'If protocols consistently reduce harm: a genuine Rope. If harm persists or mutates: a permanent Snare of Outsourcing.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(moral_outsourcing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint likely began with a lower extraction and theater ratio,
% representing a genuine attempt at coordination, but degraded over time as
% liability mitigation and control became the primary goals.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(mo_tr_t0, moral_outsourcing, theater_ratio, 0, 0.20).
narrative_ontology:measurement(mo_tr_t5, moral_outsourcing, theater_ratio, 5, 0.55).
narrative_ontology:measurement(mo_tr_t10, moral_outsourcing, theater_ratio, 10, 0.88).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(mo_ex_t0, moral_outsourcing, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(mo_ex_t5, moral_outsourcing, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(mo_ex_t10, moral_outsourcing, base_extractiveness, 10, 0.84).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint functions as a mechanism for enforcing ethical protocols,
% making it an enforcement_mechanism type.
narrative_ontology:coordination_type(moral_outsourcing, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */