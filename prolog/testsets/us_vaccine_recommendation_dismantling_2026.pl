% ============================================================================
% CONSTRAINT STORY: us_vaccine_recommendation_dismantling_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_us_vaccine_recommendation_dismantling_2026, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_vaccine_recommendation_dismantling_2026
 *   human_readable: Dismantling of National Childhood Vaccine Recommendations
 *   domain: political/social
 *
 * SUMMARY:
 *   Based on a hypothetical 2026 scenario, a new presidential administration
 *   effectively dismantles the national framework for childhood vaccine
 *   recommendations (e.g., the ACIP schedule). This removes a centralized,
 *   evidence-based coordination mechanism, forcing states, school districts,
 *   and individual parents to navigate a fragmented and politicized
 *   information landscape. The constraint is not a new rule, but the removal
 *   of a public good, which externalizes immense public health and economic
 *   costs onto the general population.
 *
 * KEY AGENTS (by structural relationship):
 *   - Public Health Dependents: Primary target (powerless/trapped) — children,
 *     immunocompromised individuals, parents, and state/local public health
 *     agencies who bear the costs of resurgent preventable diseases.
 *   - Anti-Regulation Advocates: Primary beneficiary (institutional/arbitrage) —
 *     Political actors and movements that gain influence by dismantling
 *     federal institutions and promoting "medical freedom" narratives.
 *   - Federal Executive: Inter-institutional beneficiary (institutional/arbitrage)
 *     — The architects of the policy, able to externalize consequences to states.
 *   - State Public Health Agencies: Inter-institutional victim (institutional/constrained)
 *     — Tasked with managing outbreaks without federal support or coordination.
 *   - Analytical Observer: Sees the full structure of externalized costs and
 *     destruction of a coordination good.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_vaccine_recommendation_dismantling_2026, 0.75).
domain_priors:suppression_score(us_vaccine_recommendation_dismantling_2026, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_vaccine_recommendation_dismantling_2026, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_vaccine_recommendation_dismantling_2026, extractiveness, 0.75).
narrative_ontology:constraint_metric(us_vaccine_recommendation_dismantling_2026, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(us_vaccine_recommendation_dismantling_2026, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_vaccine_recommendation_dismantling_2026, snare).
narrative_ontology:human_readable(us_vaccine_recommendation_dismantling_2026, "Dismantling of National Childhood Vaccine Recommendations").
narrative_ontology:topic_domain(us_vaccine_recommendation_dismantling_2026, "political/social").

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_vaccine_recommendation_dismantling_2026). % Enforcement to prevent CDC etc. from issuing guidance.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_vaccine_recommendation_dismantling_2026, anti_regulation_advocates).
narrative_ontology:constraint_beneficiary(us_vaccine_recommendation_dismantling_2026, federal_executive_architects).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_vaccine_recommendation_dismantling_2026, public_health_dependents).
narrative_ontology:constraint_victim(us_vaccine_recommendation_dismantling_2026, state_public_health_agencies).
%
% Gate requirements: Snare requires victim.

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Public Health Dependents (parents, children). They are trapped in a higher-risk
% environment with suppressed access to reliable, coordinated information.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Anti-Regulation Advocates. They perceive the removal of federal oversight as a
% pure coordination good ("restoring freedom").
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% The observer sees that the policy does not create a new coordination good,
% but rather destroys an existing one, imposing massive, uncompensated costs
% on a vulnerable population. The structure is pure extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15. With ε=0.75, χ is very high.
constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% Perspective 4A: Federal Executive (Beneficiary with arbitrage exit)
% The policy architects benefit politically and can shift blame for negative
% outcomes to the states. Their exit option (arbitrage) and beneficiary status
% leads to a Rope classification from their viewpoint.
constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 4B: State Public Health Agencies (Victim with constrained exit)
% State agencies are institutional actors but are victims of this federal policy.
% They cannot easily exit the responsibility of managing disease outbreaks.
% Engine derives a higher d for them (victim + constrained). This correctly
% classifies the constraint as a Snare even from an institutional perspective,
% capturing the coercive nature of the cost-shifting.
constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_vaccine_recommendation_dismantling_2026_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, TypeBeneficiary, context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), _)),
    assertion(TypeTarget == snare),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(inter_institutional_gap) :-
    % Verify the gap between the two institutional perspectives.
    constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, TypeFedExec,
        context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, TypeStateHealth,
        context(agent_power(institutional), _, exit_options(constrained), _)),
    assertion(TypeFedExec == rope),
    assertion(TypeStateHealth == snare),
    TypeFedExec \= TypeStateHealth.

test(analytical_view_is_snare) :-
    constraint_indexing:constraint_classification(us_vaccine_recommendation_dismantling_2026, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == snare).

:- end_tests(us_vaccine_recommendation_dismantling_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): High. The extraction is not direct financial
 *     gain but the massive externalization of costs onto the public. This
 *     includes the direct cost of treating vaccine-preventable diseases,
 *     lost economic productivity from illness and caregiving, and the
 *     incalculable cost of disability and death. It represents a transfer of
 *     well-being and safety away from the public.
 *   - Suppression Score (s=0.80): High. The policy actively suppresses the
 *     primary alternative: a centralized, evidence-based, scientifically
 *     vetted public health recommendation system. It replaces a high-signal
 *     source with a low-signal, high-noise environment dominated by political
 *     and ideological interests.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. Beneficiaries (Anti-Regulation Advocates) frame the
 *   constraint as a Rope of "freedom" and "parental rights," a coordination
 *   mechanism to escape perceived federal overreach. For them, χ is negative.
 *   Victims (Public Health Dependents) experience it as a Snare of manufactured
 *   risk, trapping them in a more dangerous environment where the cost of
 *   individual and collective safety has been dramatically increased. For them,
 *   χ is extremely high.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations are key. 'Anti-regulation_advocates'
 *   benefit by achieving ideological goals and consolidating political power.
 *   'Public_health_dependents' (a diffuse group including children, parents,
 *   and immunocompromised individuals) bear the costs directly through health
 *   risks. State health agencies are also victims, inheriting an unfunded mandate
 *   to manage the fallout. This asymmetric cost/benefit structure is the
 *   hallmark of a Snare.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story highlights a classic inter-institutional coercion dynamic. The
 *   Federal Executive (institutional, beneficiary, arbitrage exit) can impose
 *   costs on State Public Health Agencies (institutional, victim, constrained
 *   exit) without bearing the consequences. The directionality engine correctly
 *   captures this by deriving a higher 'd' for the constrained state agencies,
 *   leading to a 'Snare' classification even though they are an institutional
 *   actor. This demonstrates how one institution can create a Snare for another.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies that destroying a coordination
 *   mechanism is not itself a valid coordination function. The analytical
 *   perspective classifies this as a Snare, cutting through the theatrical
 *   "Rope of freedom" rhetoric to see the underlying extractive structure. A
 *   less sophisticated system might mistakenly classify this as a Tangled
 *   Rope, but it lacks the required *genuine* coordination function. Its
 *   sole structural effect is extractive. [RESOLVED MANDATROPHY]
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_vaccine_dismantling_2026,
    'Will states and private institutions successfully create grassroots coordination systems (e.g., interstate compacts, insurance mandates) to replace the dismantled federal framework?',
    'Observing state-level legislative responses and public health outcomes over the 5-10 years post-policy.',
    'If YES, the Snare could be partially mitigated over time, though with higher transactional costs. If NO, the Snare becomes deeply entrenched, leading to sustained negative public health outcomes.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_vaccine_recommendation_dismantling_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required for high-extraction constraints (base_extractiveness > 0.46).
% Models the intensification of extraction as the real-world costs of the
% policy become apparent over time.

% Theater ratio over time: Initially high due to "freedom" rhetoric, then
% falls as real-world consequences undermine the narrative.
narrative_ontology:measurement(us_vaccine_rec_dismantle_tr_t0, us_vaccine_recommendation_dismantling_2026, theater_ratio, 0, 0.60).
narrative_ontology:measurement(us_vaccine_rec_dismantle_tr_t5, us_vaccine_recommendation_dismantling_2026, theater_ratio, 5, 0.50).
narrative_ontology:measurement(us_vaccine_rec_dismantle_tr_t10, us_vaccine_recommendation_dismantling_2026, theater_ratio, 10, 0.40).

% Extraction over time: Starts lower when the policy is just an abstraction,
% then rises sharply as disease outbreaks occur and the economic/health
% costs accumulate.
narrative_ontology:measurement(us_vaccine_rec_dismantle_ex_t0, us_vaccine_recommendation_dismantling_2026, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(us_vaccine_rec_dismantle_ex_t5, us_vaccine_recommendation_dismantling_2026, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(us_vaccine_rec_dismantle_ex_t10, us_vaccine_recommendation_dismantling_2026, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This constraint actively DESTROYS a coordination mechanism, so it does not
% have a coordination_type itself. Instead, it structurally affects the
% pre-existing coordination constraint.

% Network relationships (structural influence edges)
% This policy directly attacks and degrades the existing vaccine schedule constraint.
narrative_ontology:affects_constraint(us_vaccine_recommendation_dismantling_2026, us_acip_vaccine_schedule).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The standard derivation from
% beneficiary/victim declarations and exit options (trapped, constrained,
% arbitrage) accurately captures the structural relationships, including the
% critical inter-institutional dynamic between the federal and state levels.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */