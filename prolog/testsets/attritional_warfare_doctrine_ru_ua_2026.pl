% ============================================================================
% CONSTRAINT STORY: attritional_warfare_doctrine_ru_ua_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_attritional_warfare_doctrine_ru_ua_2026, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: attritional_warfare_doctrine_ru_ua_2026
 *   human_readable: Russian Attritional Warfare Doctrine in Ukraine (2026)
 *   domain: political
 *
 * SUMMARY:
 *   This constraint models the Russian military's doctrine of attritional
 *   warfare, exemplified by "meat grinder" tactics using Storm-Z convict units
 *   in Ukraine as of early 2026. The doctrine accepts massive casualties and
 *   materiel expenditure to achieve incremental territorial gains, aiming to
 *   exhaust Ukrainian forces and Western support over a long time horizon.
 *   This system constrains the options of frontline soldiers, commanders on
 *   both sides, and international actors.
 *
 * KEY AGENTS (by structural relationship):
 *   - Frontline Conscripts & Convicts: Primary target (powerless/trapped) — bears the ultimate cost of extraction (their lives).
 *   - Russian General Staff: Primary beneficiary (institutional/arbitrage) — utilizes the doctrine as a tool to achieve strategic aims, leveraging manpower as an expendable resource.
 *   - Ukrainian High Command: Inter-institutional victim (institutional/constrained) — forced to engage within the terms of the attritional dynamic set by Russia, expending limited resources.
 *   - Western Defense Contractors: Secondary beneficiary (institutional/arbitrage) — the high burn rate of materiel creates sustained demand for their products.
 *   - Military Analysts: Analytical observer — sees the full structure, including the coordination function for one side and the extreme extraction for the other.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attritional_warfare_doctrine_ru_ua_2026, 0.75).
domain_priors:suppression_score(attritional_warfare_doctrine_ru_ua_2026, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(attritional_warfare_doctrine_ru_ua_2026, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attritional_warfare_doctrine_ru_ua_2026, extractiveness, 0.75).
narrative_ontology:constraint_metric(attritional_warfare_doctrine_ru_ua_2026, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(attritional_warfare_doctrine_ru_ua_2026, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(attritional_warfare_doctrine_ru_ua_2026, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(attritional_warfare_doctrine_ru_ua_2026). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(attritional_warfare_doctrine_ru_ua_2026, russian_general_staff).
narrative_ontology:constraint_beneficiary(attritional_warfare_doctrine_ru_ua_2026, western_defense_contractors).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(attritional_warfare_doctrine_ru_ua_2026, frontline_conscripts_and_convicts).
narrative_ontology:constraint_victim(attritional_warfare_doctrine_ru_ua_2026, ukrainian_high_command).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% The convict/conscript soldier on the front line.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The Russian General Staff planning the operations.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Sees the coordination and the extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% Perspective 4A: Ukrainian High Command (TANGLED ROPE)
% An institutional actor, but structurally a victim forced to operate within
% a system they did not choose.
% Engine derives d from: victim membership + constrained exit → d ≈ 0.7-0.8 → high χ
constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 4B: Western Defense Contractors (ROPE)
% An institutional beneficiary whose business model profits from the high
% rate of materiel consumption.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → low/negative χ
constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attritional_warfare_doctrine_ru_ua_2026_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the core perspectival gap between the soldier and the general.
    constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, TypeTarget, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget \= TypeBeneficiary.

test(inter_institutional_gap) :-
    % Verify the gap between the two institutional victims/beneficiaries.
    constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, TypeVictim, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeVictim \= TypeBeneficiary.

test(tangled_rope_structural_requirements_met) :-
    narrative_ontology:constraint_beneficiary(attritional_warfare_doctrine_ru_ua_2026, _),
    narrative_ontology:constraint_victim(attritional_warfare_doctrine_ru_ua_2026, _),
    domain_priors:requires_active_enforcement(attritional_warfare_doctrine_ru_ua_2026).

:- end_tests(attritional_warfare_doctrine_ru_ua_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): Extremely high, representing the massive waste
 *     of human lives and materiel for minimal tactical or strategic gain. This
 *     is the defining feature of "meat grinder" warfare.
 *   - Suppression (s=0.80): Very high. For frontline soldiers, alternatives like
 *     retreat or surrender are suppressed by barrier troops and command policy.
 *     For the Ukrainian command, the alternative of ceding territory without a
 *     fight is politically and strategically unacceptable.
 *   - Theater Ratio (t=0.15): Low. While propaganda is a factor, the core
 *     function of this doctrine is brutally kinetic and functional, not performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the Russian General Staff, the doctrine is a Rope: a
 *   tool that coordinates their primary resource (manpower) to achieve a
 *   strategic goal (exhaustion of the enemy). The immense cost is an accepted
 *   input. For the frontline soldier, the same system is a Snare: a trap with
 *   no exit that extracts their life for objectives they may not understand or
 *   support.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is critical here. The `constraint_beneficiary` and
 *   `constraint_victim` declarations map directly to the military hierarchy.
 *   - Beneficiaries (`russian_general_staff`, `western_defense_contractors`)
 *     are those who can leverage the system from a distance with `arbitrage`
 *     exit options, leading to low/negative effective extraction (χ).
 *   - Victims (`frontline_conscripts`, `ukrainian_high_command`) are those
 *     directly subjected to the doctrine's violence with `trapped` or
 *     `constrained` exit, leading to high χ.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model captures the asymmetric positions of institutional actors. The
 *   Russian and Ukrainian high commands are both 'institutional', but their
 *   relationship to the constraint is opposite. Russia is a beneficiary with
 *   agency (`arbitrage` exit). Ukraine is a victim forced to react (`constrained`
 *   exit). This difference in exit options, combined with their victim/beneficiary
 *   status, correctly derives a different directionality `d` and thus a
 *   different classification (Rope vs. Tangled Rope) for institutions of the
 *   same power level.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   This is a canonical Tangled Rope, preventing a simplistic analysis. Calling
 *   it a pure Snare would ignore that, for its architects, it has a genuine
 *   (if brutal) coordination function. Calling it a Rope would ignore the
 *   astronomical and asymmetric extraction. The Tangled Rope classification
 *   correctly identifies it as a system with both properties, which is key to
 *   understanding its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_attritional_warfare_doctrine,
    'Is the doctrine a calculated, effective strategy for a long war of exhaustion, or a symptom of institutional decay and an inability to conduct modern maneuver warfare?',
    'Access to internal Russian General Staff strategic assessments and planning documents from 2024-2026.',
    'If it is a calculated strategy, it remains a stable Tangled Rope. If it is a symptom of decay, its base_extractiveness is even higher than estimated and it may degrade into a Piton as Russian command structures lose coherence.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_attritional_warfare_doctrine, empirical, 'Whether the doctrine is a calculated strategy or a symptom of institutional decay, resolvable with internal planning documents.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(attritional_warfare_doctrine_ru_ua_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.75 > 0.46), so temporal data is required.
% The data models the doctrine's intensification as the war shifted from
% initial maneuver attempts (2022) to a more static, attritional phase.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(awd_tr_t0, attritional_warfare_doctrine_ru_ua_2026, theater_ratio, 0, 0.15).
narrative_ontology:measurement(awd_tr_t5, attritional_warfare_doctrine_ru_ua_2026, theater_ratio, 5, 0.15).
narrative_ontology:measurement(awd_tr_t10, attritional_warfare_doctrine_ru_ua_2026, theater_ratio, 10, 0.15).

% Extraction over time (extraction_accumulation drift):
narrative_ontology:measurement(awd_ex_t0, attritional_warfare_doctrine_ru_ua_2026, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(awd_ex_t5, attritional_warfare_doctrine_ru_ua_2026, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(awd_ex_t10, attritional_warfare_doctrine_ru_ua_2026, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The doctrine is a mechanism for allocating resources (lives, shells) to front sectors.
narrative_ontology:coordination_type(attritional_warfare_doctrine_ru_ua_2026, resource_allocation).

% Network relationships (structural influence edges)
% This doctrine directly drives the demand placed on Western supply chains.
narrative_ontology:affects_constraint(attritional_warfare_doctrine_ru_ua_2026, western_munitions_supply_chain).
narrative_ontology:affects_constraint(attritional_warfare_doctrine_ru_ua_2026, ukrainian_manpower_mobilization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The automatic derivation chain,
% using the declared beneficiary/victim groups and the specified exit_options
% for each perspective, accurately models the directionality of the system.
% The stark differences in exit options (trapped vs. constrained vs. arbitrage)
% are sufficient for the engine to compute appropriate d values.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */