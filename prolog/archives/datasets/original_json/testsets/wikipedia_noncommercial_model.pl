% ============================================================================
% CONSTRAINT STORY: wikipedia_noncommercial_model
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_wikipedia_noncommercial_model, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: wikipedia_noncommercial_model
 *   human_readable: Wikipedia's Non-Commercial, Volunteer-Driven Model
 *   domain: technological
 *
 * SUMMARY:
 *   This constraint describes the socio-technical model of Wikipedia, which
 *   relies on a global community of volunteer editors and a non-profit,
 *   donation-based funding structure to produce a free, comprehensive
 *   knowledge commons. The model explicitly constrains against commercial
 *   advertising, paid editing, and algorithmic curation, prioritizing human
 *   collaboration and verifiable neutrality. This structure has enabled it
 *   to become a foundational piece of internet infrastructure while suppressing
 *   for-profit competitors.
 *
 * KEY AGENTS (by structural relationship):
 *   - Global Public (Readers): Primary beneficiary (moderate/mobile) — receives a vast, free resource.
 *   - Volunteer Editors: Primary cost-bearers (organized/mobile) — provide the uncompensated labor that creates the value.
 *   - Wikimedia Foundation: Institutional beneficiary/steward (institutional/arbitrage) — manages the infrastructure and funds.
 *   - Big Tech Platforms: Secondary institutional beneficiaries (institutional/arbitrage) — use Wikipedia data for free to power services like knowledge graphs and train AI models.
 *   - Commercial Encyclopedia Publishers: Suppressed victims (institutional/constrained) — their business models were rendered largely unviable.
 *   - Analytical Observer: Sees the full structure as a highly successful coordination mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wikipedia_noncommercial_model, 0.12). % Extraction is the value of donated labor/funds, which is low relative to the immense public value created.
domain_priors:suppression_score(wikipedia_noncommercial_model, 0.55).   % Structural property (raw, unscaled). High suppression of commercial alternatives via network effects and zero cost.
domain_priors:theater_ratio(wikipedia_noncommercial_model, 0.10).       % Piton detection (>= 0.70). Very low; focus is on function.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wikipedia_noncommercial_model, extractiveness, 0.12).
narrative_ontology:constraint_metric(wikipedia_noncommercial_model, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(wikipedia_noncommercial_model, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A. This is a human-constructed system.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(wikipedia_noncommercial_model, rope).
narrative_ontology:human_readable(wikipedia_noncommercial_model, "Wikipedia's Non-Commercial, Volunteer-Driven Model").
narrative_ontology:topic_domain(wikipedia_noncommercial_model, "technological").

% --- Binary flags ---
% domain_priors:requires_active_enforcement is declared to resolve a SCAFFOLD_DANGER_ZONE
% lint warning. The "enforcement" is the constant, active community moderation
% (reverting vandalism, enforcing NPOV) required to maintain the encyclopedia's
% integrity. This active maintenance is a core part of the non-commercial model.
domain_priors:requires_active_enforcement(wikipedia_noncommercial_model).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(wikipedia_noncommercial_model, global_public_readers).
narrative_ontology:constraint_beneficiary(wikipedia_noncommercial_model, wikimedia_foundation).
narrative_ontology:constraint_beneficiary(wikipedia_noncommercial_model, big_tech_platforms).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(wikipedia_noncommercial_model, volunteer_editors). % They provide the labor.
narrative_ontology:constraint_victim(wikipedia_noncommercial_model, donors). % They provide the funds.
narrative_ontology:constraint_victim(wikipedia_noncommercial_model, commercial_encyclopedia_publishers). % Their market is suppressed.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE GENERAL PUBLIC / READER (ROPE)
% As a primary beneficiary with no costs, they see a pure coordination good.
% Engine derives a very low d from beneficiary status.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: THE VOLUNTEER EDITOR (ROPE)
% As a cost-bearer (victim=labor) but also a believer in the mission, their
% position is nuanced. Their power is 'organized' and exit is 'mobile'.
% The engine derives a moderate d (≈0.55), but with ε=0.12, the resulting χ
% is very low. They perceive it as a Rope they are helping to weave.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE WIKIMEDIA FOUNDATION (ROPE)
% As the institutional steward and a primary beneficiary, they see a pure Rope.
% Engine derives d from beneficiary status + arbitrage exit → d ≈ 0.05 → negative χ.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (ROPE)
% Acknowledges the costs borne by volunteers but recognizes the model's primary
% function as a massive, low-extraction coordination mechanism.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 5: BIG TECH PLATFORMS (ROPE)
% These actors are institutional beneficiaries who consume the output (data) for
% free to enhance their own commercial products. Their exit is 'arbitrage'.
% Like the Wikimedia Foundation, they experience the constraint as a pure public good.
% The engine derives a very low d, resulting in a negative χ.
constraint_indexing:constraint_classification(wikipedia_noncommercial_model, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wikipedia_noncommercial_model_tests).

test(uniform_classification_as_rope) :-
    % Verify that key actors all perceive this as a Rope, demonstrating its
    % nature as a powerful coordination mechanism.
    constraint_indexing:constraint_classification(wikipedia_noncommercial_model, rope, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(wikipedia_noncommercial_model, rope, context(agent_power(organized), _, _, _)),
    constraint_indexing:constraint_classification(wikipedia_noncommercial_model, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(wikipedia_noncommercial_model, rope, context(agent_power(analytical), _, _, _)).

test(low_extraction_threshold) :-
    % Verify that the base extractiveness is low, consistent with a Rope.
    domain_priors:base_extractiveness(wikipedia_noncommercial_model, E),
    E < 0.45.

:- end_tests(wikipedia_noncommercial_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.12): This value is low because the model is
 *     explicitly non-profit. The "extraction" is of voluntary labor and
 *     donations, not coerced payment or rent-seeking. While the absolute value
 *     of this contributed labor is enormous, relative to the total public
 *     value created, the extractive ratio is minimal.
 *   - Suppression (0.55): The model is highly suppressive of for-profit
 *     alternatives, not through legal coercion but through market dominance
 *     achieved via network effects and a zero-price public good.
 *   - Theater (0.10): The Wikimedia Foundation is lean and mission-focused.
 *     Fundraising efforts are direct and functional, not theatrical.
 *   - Active Enforcement: The model requires active enforcement, not by a state,
 *     but by its global community of editors who constantly revert vandalism,
 *     remove unsourced claims, and enforce policies like Neutral Point of View.
 *     This declaration is crucial to distinguish this durable Rope from a
 *     temporary, non-enforced Scaffold, resolving a potential ambiguity in the
 *     classification engine.
 *
 * PERSPECTIVAL GAP:
 *   There is a remarkable LACK of a perspectival gap. Nearly all agents,
 *   including the cost-bearing volunteers, perceive the constraint as a Rope.
 *   This is a hallmark of a successful, mission-driven coordination project.
 *   The volunteers are aligned with the project's goals, so their contribution
 *   is seen as participation, not extraction. The only agents who might see it
 *   differently are the commercial competitors whose models were suppressed, but
 *   the constraint doesn't extract *from* them; it simply out-competes them.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The 'global_public_readers' and 'big_tech_platforms'
 *     receive immense value for zero cost. The 'wikimedia_foundation' benefits
 *     by fulfilling its mission. These declarations lead to low `d` values and
 *     negative effective extraction (χ), correctly identifying them as
 *     beneficiaries.
 *   - Victims: The 'volunteer_editors' and 'donors' are listed as victims
 *     because they bear the primary costs (labor and money). This correctly
 *     assigns them a higher directionality `d`. However, because their exit is
 *     'mobile' and power 'organized', and base extraction ε is so low, the
 *     resulting χ remains far below the threshold for a Tangled Rope or Snare.
 *     This correctly models their relationship as voluntary contributors to a
 *     system they support.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies Wikipedia as a Rope (coordination)
 *   despite the huge amount of labor involved. A naive analysis might mislabel
 *   the uncompensated labor as pure extraction (Snare). The Deferential Realism
 *   framework avoids this error by considering the low base extractive *ratio* (ε),
 *   the voluntary nature of the contribution (mobile exit), and the alignment
 *   of the contributors with the project's goals. It correctly separates a
 *   cooperative, high-functioning public good from a coercive, extractive system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_wikipedia_sustainability,
    'Is the volunteer labor and donation model sustainable in the long term against editor burnout, misinformation campaigns, and competition for attention from commercial platforms?',
    'Longitudinal data on editor retention rates, donation levels per user, and the cost/effort required to revert vandalism and coordinated disinformation.',
    'If unsustainable (False), the constraint could degrade into a Piton (maintained by inertia but losing function) or require a structural change that increases ε. If sustainable (True), it remains a durable Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(wikipedia_noncommercial_model, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has been remarkably stable. The temporal data reflects this,
% showing no significant drift towards higher extraction or theater.
% As ε < 0.46, this section is not strictly required but is included for completeness.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(wiki_tr_t0, wikipedia_noncommercial_model, theater_ratio, 0, 0.10).
narrative_ontology:measurement(wiki_tr_t5, wikipedia_noncommercial_model, theater_ratio, 5, 0.10).
narrative_ontology:measurement(wiki_tr_t10, wikipedia_noncommercial_model, theater_ratio, 10, 0.10).

% Extraction over time (stable and low):
narrative_ontology:measurement(wiki_ex_t0, wikipedia_noncommercial_model, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(wiki_ex_t5, wikipedia_noncommercial_model, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(wiki_ex_t10, wikipedia_noncommercial_model, base_extractiveness, 10, 0.12).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Wikipedia is a textbook example of a global information standard.
narrative_ontology:coordination_type(wikipedia_noncommercial_model, global_infrastructure).

% Network relationships (structural influence edges)
% Wikipedia's data is a foundational input for training Large Language Models.
narrative_ontology:affects_constraint(wikipedia_noncommercial_model, llm_training_data_commons).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% from beneficiary/victim declarations and exit options accurately models the
% structural relationships of all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */