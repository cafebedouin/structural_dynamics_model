% ============================================================================
% CONSTRAINT STORY: dk_us_alliance_espionage
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_dk_us_alliance_espionage, []).

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
 *   constraint_id: dk_us_alliance_espionage
 *   human_readable: "Implicit 'No Industrial Espionage' Norm within Western Alliances"
 *   domain: geopolitical
 *
 * SUMMARY:
 *   This constraint models the implicit norm within NATO that member states,
 *   particularly the hegemon (USA), will not conduct industrial espionage
 *   against allied nations (e.g., Denmark). The story is triggered by the
 *   Danish intelligence service (FE) officially reclassifying the USA as a
 *   threat due to persistent espionage targeting Danish defense industries.
 *   This reclassification reveals the breakdown of a core coordination
 *   function (trust) and exposes the asymmetric extraction occurring under
 *   the cover of the alliance.
 *
 * KEY AGENTS (by structural relationship):
 *   - Danish State & Defense Industry: Primary target (institutional/constrained) — bears the cost of stolen intellectual property and strategic disadvantage.
 *   - US Intelligence Agencies & Defense Contractors: Primary beneficiary (institutional/arbitrage) — benefits from illicitly acquired technology and market advantage.
 *   - Allied Whistleblower: Secondary target (powerless/trapped) — faces severe repercussions for exposing the norm violation.
 *   - Analytical Observer: Analytical observer — sees both the coordination function and the extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dk_us_alliance_espionage, 0.55).
domain_priors:suppression_score(dk_us_alliance_espionage, 0.75).   % Structural property (raw, unscaled). High due to the difficulty of exiting NATO or finding a comparable security guarantor.
domain_priors:theater_ratio(dk_us_alliance_espionage, 0.15).       % Piton detection (>= 0.70). The espionage is functional, not theatrical.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dk_us_alliance_espionage, extractiveness, 0.55).
narrative_ontology:constraint_metric(dk_us_alliance_espionage, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dk_us_alliance_espionage, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(dk_us_alliance_espionage, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(dk_us_alliance_espionage). % Both the espionage and counter-espionage require active effort.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint being violated?
narrative_ontology:constraint_beneficiary(dk_us_alliance_espionage, us_defense_industrial_base).
%
% Who bears disproportionate cost from the violation?
narrative_ontology:constraint_victim(dk_us_alliance_espionage, danish_defense_industry).
narrative_ontology:constraint_victim(dk_us_alliance_espionage, allied_european_defense_industries).
narrative_ontology:constraint_victim(dk_us_alliance_espionage, intelligence_whistleblowers).

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

% PERSPECTIVE 1: THE TARGET (DENMARK)
% As a victim with constrained exit options within the alliance, the derived
% directionality 'd' is high (d ≈ 0.9), leading to high effective extraction (χ).
% The coordination function appears broken, making the constraint feel like a Snare.
constraint_indexing:constraint_classification(dk_us_alliance_espionage, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE BENEFICIARY (USA)
% As the primary beneficiary with arbitrage exit (it sets the rules), the
% derived directionality 'd' is very low (d ≈ 0.05). This results in a negative
% effective extraction (χ), making the alliance norm classify as a pure Rope.
constraint_indexing:constraint_classification(dk_us_alliance_espionage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analyst sees both the genuine coordination function of the alliance AND
% the asymmetric extraction. This dual nature is the definition of a Tangled Rope.
% This is the basis for the constraint_claim.
constraint_indexing:constraint_classification(dk_us_alliance_espionage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE WHISTLEBLOWER (POWERLESS TARGET)
% A powerless individual within the system (e.g., an intelligence analyst) who
% attempts to expose the activity. They are trapped by secrecy laws and face
% severe personal and legal repercussions. For them, the entire apparatus of
% state secrecy that enables this unaccountable action is a pure Snare.
constraint_indexing:constraint_classification(dk_us_alliance_espionage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% This story is fundamentally inter-institutional. Perspectives 1 & 2 model
% the two primary institutional actors (Denmark, USA) experiencing the same
% constraint in fundamentally different ways due to their structural positions
% and exit options. We add another for other allies in a similar position.

% PERSPECTIVE 5: OTHER TARGETED ALLIES (e.g., GERMANY)
% These actors are in a similar structural position to Denmark: they are victims
% of the espionage but remain trapped by the same alliance dependencies.
constraint_indexing:constraint_classification(dk_us_alliance_espionage, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dk_us_alliance_espionage_tests).

test(perspectival_gap_institutional, [nondet]) :-
    % Verify the core perspectival gap between the two institutional actors.
    constraint_indexing:constraint_classification(dk_us_alliance_espionage, TypeTarget,
        context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(dk_us_alliance_espionage, TypeBeneficiary,
        context(agent_power(institutional), _, exit_options(arbitrage), _)),
    assertion(TypeTarget == snare),
    assertion(TypeBeneficiary == rope),
    assertion(TypeTarget \= TypeBeneficiary).

test(analytical_classification_is_tangled_rope) :-
    % Ensure the analytical perspective correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(dk_us_alliance_espionage, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % Verify that all three required structural predicates for a Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(dk_us_alliance_espionage, _),
    narrative_ontology:constraint_victim(dk_us_alliance_espionage, _),
    domain_priors:requires_active_enforcement(dk_us_alliance_espionage).

:- end_tests(dk_us_alliance_espionage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): Industrial espionage against a nation's
 *     high-tech defense sector represents significant value extraction. This
 *     score is high enough to trigger Snare/Tangled Rope classifications
 *     but not so high as to imply total economic predation.
 *   - Suppression (0.75): Extremely high. For a country like Denmark, there
 *     is no viable alternative to the US-led security architecture (NATO).
 *     Exit is practically impossible, meaning it must tolerate the behavior.
 *   - The combination of a coordination function (alliance) with high ε and
 *     high suppression makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the US (beneficiary, arbitrage exit), the norm
 *   is a Rope; the alliance provides massive benefits, and the espionage is a
 *   low-cost "bonus." For Denmark (victim, constrained exit), the trust is
 *   broken, and the extraction is damaging; it feels like a Snare where they
 *   are trapped and exploited by the entity meant to protect them. This gap
 *   between Rope and Snare is the source of the geopolitical friction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the structural roles. The `us_defense_industrial_base`
 *   is the clear beneficiary, receiving stolen IP. The `danish_defense_industry`
 *   is the clear victim, losing its competitive edge and security. The engine
 *   maps these roles and their disparate `exit_options` (`arbitrage` vs. `constrained`)
 *   to derive the different directionality values (`d`) that drive the
 *   perspectival gap. No overrides are needed because the derivation captures
 *   the dynamic correctly.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story is a prime example of inter-institutional constraint dynamics.
 *   Two institutional actors (USA, Denmark) are bound by the same rule-set
 *   (NATO norms), but their different power levels and exit options create
 *   vastly different realities. The US has `arbitrage` (it can selectively
 *   violate norms with few consequences), while Denmark is `constrained` (it
 *   cannot exit the relationship without catastrophic costs). This asymmetry
 *   allows the Rope (for the US) to become a Snare (for Denmark).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two key errors. A simplistic analysis might
 *   label the entire NATO alliance a "Snare" based on this behavior, which
 *   would be mandatrophy—it ignores the very real, substantial coordination
 *   benefits the alliance provides to its members. Conversely, ignoring the
 *   espionage and labeling the norm a pure "Rope" would be naive, whitewashing
 *   the real, asymmetric harm being done. The Tangled Rope classification
 *   correctly identifies the hybrid state: a valuable coordination mechanism
 *   that has been co-opted for coercive extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_dk_us_alliance_espionage,
    'What is the true economic and strategic impact of the US espionage on Denmark and other allies?',
    'Declassification of counter-intelligence assessments from multiple allied nations.',
    'If impact is low, ε should be reduced, potentially shifting the analytical view towards a degraded Rope. If high, ε should be increased, solidifying the Tangled Rope/Snare classification.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_dk_us_alliance_espionage, empirical, 'The true economic and strategic impact of the US espionage on Denmark and other allies.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(dk_us_alliance_espionage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has degraded over time. Initially, the norm was stronger
% (low extraction), but it has weakened as US strategic priorities shifted,
% leading to more aggressive intelligence gathering against allies.
% Base extractiveness is > 0.46, so this section is required.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(dk_us_tr_t0, dk_us_alliance_espionage, theater_ratio, 0, 0.10).
narrative_ontology:measurement(dk_us_tr_t5, dk_us_alliance_espionage, theater_ratio, 5, 0.12).
narrative_ontology:measurement(dk_us_tr_t10, dk_us_alliance_espionage, theater_ratio, 10, 0.15).

% Extraction over time (shows degradation of the norm):
narrative_ontology:measurement(dk_us_ex_t0, dk_us_alliance_espionage, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(dk_us_ex_t5, dk_us_alliance_espionage, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(dk_us_ex_t10, dk_us_alliance_espionage, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The norm is part of the trust infrastructure of the alliance.
narrative_ontology:coordination_type(dk_us_alliance_espionage, global_infrastructure).

% Network relationships: The degradation of this norm directly impacts other
% alliance-related constraints, such as technology sharing agreements and
% collective defense commitments.
narrative_ontology:affects_constraint(dk_us_alliance_espionage, nato_tech_sharing_framework).
narrative_ontology:affects_constraint(dk_us_alliance_espionage, transatlantic_data_sharing_agreements).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The standard derivation chain
% correctly distinguishes between the two institutional actors based on their
% declared beneficiary/victim status and their distinct exit_options
% ('arbitrage' vs. 'constrained'). This demonstrates the power of the v6.0
% directionality model to capture inter-institutional dynamics without manual
% intervention.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */