% ============================================================================
% CONSTRAINT STORY: heglig_oil_field_control
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-25
% ============================================================================

:- module(constraint_heglig_oil_field_control, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: heglig_oil_field_control
 *   human_readable: Control of Heglig Oil Field as a Strategic War Asset
 *   domain: Geopolitical / Economic
 *
 * SUMMARY:
 *   In the context of the Sudanese civil war, the paramilitary Rapid Support
 *   Forces (RSF) have captured Heglig, the country's largest oil field.
 *   This constraint represents the seizure of a critical national resource,
 *   transforming it from a state asset into a source of funding for a non-state
 *   actor's war effort. The control is maintained by military force and
 *   enables direct extraction of value at the expense of the state and its citizens.
 *
 * KEY AGENTS (by structural relationship):
 *   - Sudanese Civilians: Primary target (powerless/trapped) — bear the ultimate cost of a prolonged war funded by diverted national wealth.
 *   - Sudanese Armed Forces (SAF): Inter-institutional target (institutional/constrained) — the state actor that has lost control of the asset and its revenue stream.
 *   - Rapid Support Forces (RSF): Primary beneficiary (organized/arbitrage) — captures the oil revenue to fund its military and political objectives.
 *   - Illicit Oil Traders: Secondary beneficiary (powerful/arbitrage) — gain access to oil, potentially at a discount, outside of formal state channels.
 *   - Analytical Observer: Analytical observer — sees the full structure as a coercive wealth transfer mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(heglig_oil_field_control, 0.85). % The primary purpose is revenue seizure.
domain_priors:suppression_score(heglig_oil_field_control, 0.95).   % Maintained by direct military force, suppressing all other claims.
domain_priors:theater_ratio(heglig_oil_field_control, 0.10).       % Little to no pretense; it is a raw military-economic action.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(heglig_oil_field_control, extractiveness, 0.85).
narrative_ontology:constraint_metric(heglig_oil_field_control, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(heglig_oil_field_control, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(heglig_oil_field_control, snare).

% --- Binary flags ---
domain_priors:requires_active_enforcement(heglig_oil_field_control). % Control depends entirely on military presence.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(heglig_oil_field_control, rsf_paramilitary).
narrative_ontology:constraint_beneficiary(heglig_oil_field_control, illicit_oil_traders).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(heglig_oil_field_control, sudanese_civilians).
narrative_ontology:constraint_victim(heglig_oil_field_control, sudanese_state_saf).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE SUDANESE CIVILIAN (PRIMARY TARGET)
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% The diversion of national wealth to fund a war is pure extraction from their perspective.
constraint_indexing:constraint_classification(heglig_oil_field_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE RAPID SUPPORT FORCES (PRIMARY BENEFICIARY)
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
% From their view, this is a coordination mechanism (a Rope) to solve the problem
% of funding their war effort. They experience no extraction, only benefit. The
% high base ε (0.85) is an objective property, but their perspectival χ is negative.
constraint_indexing:constraint_classification(heglig_oil_field_control, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SUDANESE ARMED FORCES (INTER-INSTITUTIONAL VICTIM)
% An institutional actor, but one that is targeted and constrained. The engine
% derives a high 'd' from their victim status and constrained exit, leading to high χ.
% They experience this not as a coordination failure, but as a direct, coercive seizure.
constraint_indexing:constraint_classification(heglig_oil_field_control, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% Default analytical context. Sees the high base extraction (ε=0.85), high suppression
% (S=0.95), and coercive nature, classifying it as a Snare.
% χ = ε × f(d) × σ(S) = 0.85 * 1.15 * 1.2 ≈ 1.173, which is well above the snare threshold.
constraint_indexing:constraint_classification(heglig_oil_field_control, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(heglig_oil_field_control_tests).

test(perspectival_gap_beneficiary_victim) :-
    % Verify the core perspectival gap between the RSF (beneficiary) and civilians (victim).
    constraint_indexing:constraint_classification(heglig_oil_field_control, TypeBeneficiary, context(agent_power(organized), _, _, _)),
    constraint_indexing:constraint_classification(heglig_oil_field_control, TypeVictim, context(agent_power(powerless), _, _, _)),
    assertion(TypeBeneficiary == rope),
    assertion(TypeVictim == snare),
    assertion(TypeBeneficiary \= TypeVictim).

test(inter_institutional_gap) :-
    % Verify the different classifications between the two main armed actors.
    constraint_indexing:constraint_classification(heglig_oil_field_control, TypeRSF, context(agent_power(organized), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(heglig_oil_field_control, TypeSAF, context(agent_power(institutional), _, exit_options(constrained), _)),
    assertion(TypeRSF == rope),
    assertion(TypeSAF == snare).

test(analytical_claim_matches) :-
    % Verify the analytical perspective matches the declared constraint claim.
    narrative_ontology:constraint_claim(heglig_oil_field_control, ClaimType),
    constraint_indexing:constraint_classification(heglig_oil_field_control, AnalyticalType, context(agent_power(analytical), _, _, _)),
    assertion(ClaimType == snare),
    assertion(ClaimType == AnalyticalType).

test(threshold_validation_snare) :-
    narrative_ontology:constraint_metric(heglig_oil_field_control, extractiveness, E),
    narrative_ontology:constraint_metric(heglig_oil_field_control, suppression_requirement, S),
    assertion(E >= 0.46),
    assertion(S >= 0.60).

:- end_tests(heglig_oil_field_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.85) and suppression (S=0.95) are set extremely high
 *   to reflect the nature of the constraint: a military seizure of a strategic economic
 *   asset. Its function is not coordination for a public good, but coercive revenue
 *   extraction for a private (factional) good. The theater ratio is minimal; this
 *   is an act of raw power, not bureaucratic performance. These metrics firmly place
 *   the constraint in the Snare category from any victim or analytical perspective.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The RSF, as the beneficiary, experiences the constraint as a
 *   Rope. It solves their coordination problem: "how to secure a revenue stream to
 *   prosecute the war." For them, the effective extraction (χ) is negative, as they
 *   are the net recipients of value. For the Sudanese state (SAF) and civilians, it
 *   is an unambiguous Snare, a mechanism of coercive wealth transfer that traps
 *   them in a prolonged conflict by funding one of its belligerents. This Rope/Snare
 *   duality is characteristic of constraints involving organized theft or predation.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The `rsf_paramilitary` and `illicit_oil_traders` directly profit. The engine assigns them a very low directionality `d` due to their beneficiary status and arbitrage/mobile exit options, resulting in a low or negative effective extraction `χ`.
 *   - Victims: The `sudanese_civilians` and `sudanese_state_saf` bear the costs. The civilians are `trapped`, while the SAF is `constrained`. The engine assigns them a very high `d`, resulting in a high `χ` that reflects the intense extraction they experience.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story is a prime example of an inter-institutional conflict. Both the RSF (`organized`)
 *   and the SAF (`institutional`) are powerful, structured actors. However, their relationship
 *   to the constraint is inverted. The RSF has `arbitrage` exit (it can sell the oil), making
 *   it the beneficiary. The SAF has `constrained` exit (it has lost control and must fight
 *   to regain it), making it the victim. This difference in exit options, combined with their
 *   roles as beneficiary/victim, is what the directionality engine uses to produce the
 *   correctly opposed Rope vs. Snare classifications, even for two powerful actors.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY]: The framework correctly avoids mislabeling this as a simple coordination problem.
 *   While the RSF is 'coordinating' oil extraction, the high ε (0.85) and suppression (0.95), and the explicit
 *   declaration of a victim group (`sudanese_state_saf`), force the analytical
 *   classification to be a Snare. It recognizes that coordination in the service of
 *   pure, coercive extraction is not a Rope but the defining feature of a functional Snare.
 *   The is_mandatrophy_resolved hook is satisfied because the high extraction is not ambiguous;
 *   it is the primary, observable function of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_heglig_oil_field_control,
    'Is the RSF''s control a stable, long-term extraction mechanism, or a temporary disruption that will collapse due to technical/political inability to export?',
    'Observation of sustained oil export volumes and documented revenue flows to the RSF over a 12-24 month period.',
    'If stable -> A durable Snare that entrenches the conflict and deepens state fragmentation. If it collapses -> A failed Snare attempt that degrades into a Piton (the claim of control without the function).',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
% The reporting engine reads narrative_ontology:omega_variable/3 with structure
% (ID, TypeClass, Description) where TypeClass is one of:
%   empirical   — resolvable by gathering more data
%   conceptual  — depends on definitional or theoretical framing
%   preference  — depends on value judgments or policy choices
% The /3 form is what the engine reads; /5 provides narrative context.
narrative_ontology:omega_variable(omega_heglig_oil_field_control, empirical, 'Stability and long-term viability of the RSF''s oil extraction and export capability.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(heglig_oil_field_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint is high-extraction (ε=0.85 > 0.46), so lifecycle data is required.
% The interval models the period of the RSF's campaign to seize strategic assets.
% At T=0, their control (and thus the extraction from this specific constraint) was zero.
% It ramped up quickly as their military campaign succeeded.

% Theater ratio over time (consistently low):
narrative_ontology:measurement(heglig_tr_t0, heglig_oil_field_control, theater_ratio, 0, 0.10).
narrative_ontology:measurement(heglig_tr_t5, heglig_oil_field_control, theater_ratio, 5, 0.10).
narrative_ontology:measurement(heglig_tr_t10, heglig_oil_field_control, theater_ratio, 10, 0.10).

% Extraction over time (rapid accumulation):
narrative_ontology:measurement(heglig_ex_t0, heglig_oil_field_control, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(heglig_ex_t5, heglig_oil_field_control, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(heglig_ex_t10, heglig_oil_field_control, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The RSF's internal activity is a form of resource allocation.
narrative_ontology:coordination_type(heglig_oil_field_control, resource_allocation).

% Network relationships: Control of this resource directly impacts humanitarian conditions.
narrative_ontology:affects_constraint(heglig_oil_field_control, sudan_humanitarian_access).


/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */