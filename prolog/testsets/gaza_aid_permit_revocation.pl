% ============================================================================
% CONSTRAINT STORY: gaza_aid_permit_revocation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_gaza_aid_permit_revocation, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gaza_aid_permit_revocation
 *   human_readable: Revocation of Work Permits for Local Aid Workers in Gaza
 *   domain: political
 *
 * SUMMARY:
 *   The Israeli state, through its military and civil administration (COGAT),
 *   is revoking work permits for local Palestinian staff of international
 *   humanitarian organizations operating in Gaza. This action effectively
 *   halts or severely cripples the ability of these organizations to deliver
 *   aid, directly impacting the civilian population dependent on it. The
 *   stated justification is to exert pressure on Hamas, but the immediate
 *   effect is the denial of essential resources to a trapped population.
 *
 * KEY AGENTS (by structural relationship):
 *   - Palestinian Civilians in Gaza: Primary target (powerless/trapped) — bear the extraction of essential aid and services.
 *   - Local Palestinian Aid Workers: Secondary target (moderate/trapped) — bear the direct extraction of their livelihood and ability to work.
 *   - Israeli State & Military: Primary beneficiary (institutional/arbitrage) — benefits from the increased leverage and control gained by the policy.
 *   - International Aid Organizations & Donor Nations: Secondary victim (organized/constrained) — their operations are blocked, and their influence is suppressed.
 *   - Analytical Observer: Analytical observer — sees the full structure of coercion and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gaza_aid_permit_revocation, 0.85). % Extracts survival resources and leverage.
domain_priors:suppression_score(gaza_aid_permit_revocation, 0.90).   % Structural property (raw, unscaled). Alternatives are blockaded.
domain_priors:theater_ratio(gaza_aid_permit_revocation, 0.15).       % The action is highly functional to its coercive goal, not primarily theatrical.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gaza_aid_permit_revocation, extractiveness, 0.85).
narrative_ontology:constraint_metric(gaza_aid_permit_revocation, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(gaza_aid_permit_revocation, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(gaza_aid_permit_revocation, snare).

% --- Binary flags ---
domain_priors:requires_active_enforcement(gaza_aid_permit_revocation). % Enforced by COGAT and military.

% --- Emergence flag (required for mountain constraints) ---
% N/A for this constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(gaza_aid_permit_revocation, israeli_state_and_military).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(gaza_aid_permit_revocation, palestinian_civilians_in_gaza).
narrative_ontology:constraint_victim(gaza_aid_permit_revocation, international_aid_organizations).
narrative_ontology:constraint_victim(gaza_aid_permit_revocation, local_palestinian_aid_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% The Palestinian civilian population dependent on aid. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → very high χ
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The Israeli state enacting the policy. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% Default analytical context. Sees the high base extraction and coercion.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% International aid organizations and donor states like Norway. They are
% institutional actors but are victims of the policy with constrained options.
% Engine derives d from: victim membership + constrained exit -> d > 0.5 -> high χ
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gaza_aid_permit_revocation_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(gaza_aid_permit_revocation, TypeTarget, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(gaza_aid_permit_revocation, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget == snare,
    TypeBeneficiary == rope,
    TypeTarget \= TypeBeneficiary.

test(perspectival_gap_ngo_beneficiary) :-
    % Verify perspectival gap between constrained NGO and beneficiary.
    constraint_indexing:constraint_classification(gaza_aid_permit_revocation, TypeNGO, context(agent_power(organized), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(gaza_aid_permit_revocation, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeNGO == snare,
    TypeBeneficiary == rope,
    TypeNGO \= TypeBeneficiary.

test(analytical_claim_matches) :-
    % Verify the analytical view matches the overall constraint claim.
    narrative_ontology:constraint_claim(gaza_aid_permit_revocation, ClaimType),
    constraint_indexing:constraint_classification(gaza_aid_permit_revocation, AnalyticalType, context(agent_power(analytical), _, _, _)),
    ClaimType == AnalyticalType.

test(threshold_validation_snare) :-
    domain_priors:base_extractiveness(gaza_aid_permit_revocation, E),
    domain_priors:suppression_score(gaza_aid_permit_revocation, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(gaza_aid_permit_revocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.85): The constraint directly removes access to
 *     life-sustaining resources (food, water, medicine) from a population. This
 *     is one of the highest possible forms of extraction, converting human
 *     suffering into geopolitical leverage.
 *   - Suppression Score (0.90): The targeted population is under a blockade,
 *     meaning there are no viable alternative channels for receiving aid if the
 *     primary international organizations are incapacitated. The constraint
 *     functions by actively suppressing alternatives.
 *   - Theater Ratio (0.15): While there is a political narrative (pressuring
 *     Hamas), the primary effect of the constraint is its direct, functional
 *     impact on aid delivery. The action is coercive enforcement, not performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For the Palestinian civilians (powerless, trapped), the
 *   constraint is an existential threat with no exit, a textbook Snare. For the
 *   Israeli state (institutional, arbitrage), it is a policy instrument—a tool
 *   for coordinating state power to achieve a strategic objective. From this
 *   perspective, it functions as a Rope, coordinating military and civil
 *   actions. This difference in classification arises directly from their
 *   structural positions and the derived directionality (d).
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `israeli_state_and_military`. They design and enforce the
 *     constraint and are the sole beneficiaries of the leverage it creates.
 *     Combined with `arbitrage` exit, this yields a low `d` and negative `χ`.
 *   - Victims: `palestinian_civilians_in_gaza` and `local_palestinian_aid_workers`.
 *     They bear the full cost. Combined with `trapped` exit, this yields a
 *     high `d` and high `χ`.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   International aid organizations represent an interesting inter-institutional
 *   case. They are powerful, organized actors, but in this context, their agency
 *   is nullified. Their `exit_options(constrained)` reflects their inability to
 *   override the sovereign decision of the Israeli state on its controlled
 *   territory. Despite being 'institutional', they are structurally victims of
 *   this constraint, causing it to classify as a Snare from their perspective
 *   as well, albeit with a slightly lower effective extraction (χ) than for the
 *   trapped civilians.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   This is a clear case of a Snare. Any attempt to frame it as a coordination
 *   mechanism (Rope) fails because the coordination function is entirely
 *   internal to the enforcing agent. For those subjected to it, there is no
 *   coordination benefit, only asymmetric extraction. The system correctly
 *   identifies this by separating the beneficiary's view (Rope) from the
 *   victim's and analytical views (Snare), preventing the beneficiary's
 *   framing from defining the constraint's objective nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_gaza_aid_permit_revocation,
    'Is the primary strategic goal of the policy to pressure Hamas leadership, or to collectively punish the civilian population for leverage?',
    'Declassified internal policy memos, credible whistleblower testimony, or a change in policy direction after a verifiable change in Hamas behavior.',
    'If primarily targeting Hamas (Tangled Rope), it suggests a flawed but rational strategic logic. If primarily collective punishment (Snare), it implies a different and more severe moral and legal calculus.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(gaza_aid_permit_revocation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This policy represents an
% intensification of control over time. We model an increase in extraction
% and a decrease in theater as diplomatic justifications wear thin and
% direct enforcement becomes the norm.
% Required because base_extractiveness (0.85) > 0.46.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(gaza_aid_tr_t0, gaza_aid_permit_revocation, theater_ratio, 0, 0.30).
narrative_ontology:measurement(gaza_aid_tr_t5, gaza_aid_permit_revocation, theater_ratio, 5, 0.20).
narrative_ontology:measurement(gaza_aid_tr_t10, gaza_aid_permit_revocation, theater_ratio, 10, 0.15).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(gaza_aid_ex_t0, gaza_aid_permit_revocation, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(gaza_aid_ex_t5, gaza_aid_permit_revocation, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(gaza_aid_ex_t10, gaza_aid_permit_revocation, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This constraint functions as a mechanism of coercive enforcement.
narrative_ontology:coordination_type(gaza_aid_permit_revocation, enforcement_mechanism).

% Network relationships (structural influence edges)
% This constraint is likely downstream of broader blockade policies.
% affects_constraint(gaza_blockade, gaza_aid_permit_revocation).
% (Leaving commented as gaza_blockade is not yet defined in this context).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% power dynamics and directionality of the constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */