% ============================================================================
% CONSTRAINT STORY: indonesia_penal_code_2023
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_indonesia_penal_code_2023, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: indonesia_penal_code_2023
 *   human_readable: Indonesia's 2023 Penal Code (KUHP) Criminalizing Personal Liberties
 *   domain: political/social
 *
 * SUMMARY:
 *   In 2023, Indonesia ratified a new penal code (KUHP) that criminalizes a
 *   range of personal activities, including extramarital sex, cohabitation by
 *   unmarried couples, and insulting the president or state institutions.
 *   The law, framed as a decolonization effort, creates a powerful tool for
 *   social control and suppression of dissent, impacting citizens, foreigners,
 *   and the business climate.
 *
 * KEY AGENTS (by structural relationship):
 *   - Unmarried individuals, journalists, and political dissidents: Primary targets (powerless/trapped) — bear the extraction of personal liberty and face risk of prosecution.
 *   - Conservative political and religious factions: Primary beneficiaries (institutional/arbitrage) — benefit from the enforcement of their social norms and the suppression of opposition.
 *   - Foreign tourism and investment sector: Inter-institutional victim (organized/mobile) — bears costs from reputational damage and operational risk.
 *   - Human rights organizations: Analytical observers — see the full structure of coordination for beneficiaries and extraction from victims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indonesia_penal_code_2023, 0.75).
domain_priors:suppression_score(indonesia_penal_code_2023, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(indonesia_penal_code_2023, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indonesia_penal_code_2023, extractiveness, 0.75).
narrative_ontology:constraint_metric(indonesia_penal_code_2023, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(indonesia_penal_code_2023, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(indonesia_penal_code_2023, tangled_rope).
narrative_ontology:human_readable(indonesia_penal_code_2023, "Indonesia's 2023 Penal Code (KUHP) Criminalizing Personal Liberties").
narrative_ontology:topic_domain(indonesia_penal_code_2023, "political/social").

% --- Binary flags ---
domain_priors:requires_active_enforcement(indonesia_penal_code_2023). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(indonesia_penal_code_2023, conservative_political_and_religious_factions).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(indonesia_penal_code_2023, unmarried_individuals).
narrative_ontology:constraint_victim(indonesia_penal_code_2023, political_dissidents_and_journalists).
narrative_ontology:constraint_victim(indonesia_penal_code_2023, foreign_tourism_and_investment_sector).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET

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
% An ordinary Indonesian citizen subject to the morality clauses.
% They are a declared victim with trapped exit. Engine derives d ≈ 0.95 -> f(d) ≈ 1.42.
% χ = 0.75 * 1.42 * 1.0 (national) ≈ 1.065. High extraction, high suppression.
constraint_indexing:constraint_classification(indonesia_penal_code_2023, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% A member of a conservative political party that championed the law.
% They are a declared beneficiary with arbitrage exit. Engine derives d ≈ 0.05 -> f(d) ≈ -0.12.
% χ = 0.75 * -0.12 * 1.0 (national) ≈ -0.09. Negative extraction -> a tool for control.
constraint_indexing:constraint_classification(indonesia_penal_code_2023, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% A human rights organization assessing the law's structure.
% Sees both the coordination function for beneficiaries and the severe asymmetric
% extraction from victims. Requires active enforcement. Canonical Tangled Rope.
% Engine derives d ≈ 0.72 -> f(d) ≈ 1.15. Global scope amplifies χ.
constraint_indexing:constraint_classification(indonesia_penal_code_2023, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% The foreign tourism sector, which must now navigate increased risk.
% As an organized victim with mobile exit, d is derived around 0.85 -> f(d) ≈ 1.15.
% χ = 0.75 * 1.15 * 1.0 (national) ≈ 0.86.
% Experiences the law as a highly extractive mechanism that disrupts its business model.
constraint_indexing:constraint_classification(indonesia_penal_code_2023, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indonesia_penal_code_2023_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the core perspectival gap.
    constraint_indexing:constraint_classification(indonesia_penal_code_2023, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indonesia_penal_code_2023, rope, context(agent_power(institutional), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    % Ensure the analytical view correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(indonesia_penal_code_2023, tangled_rope, context(agent_power(analytical), _, _, _)).

test(high_extraction_and_suppression_thresholds) :-
    % Verify the base metrics are in the Snare/Tangled Rope range.
    domain_priors:base_extractiveness(indonesia_penal_code_2023, E), E >= 0.46,
    domain_priors:suppression_score(indonesia_penal_code_2023, S), S >= 0.60.

test(tangled_rope_gate_requirements_met) :-
    % Verify all three conditions for Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(indonesia_penal_code_2023, _),
    narrative_ontology:constraint_victim(indonesia_penal_code_2023, _),
    domain_priors:requires_active_enforcement(indonesia_penal_code_2023).

:- end_tests(indonesia_penal_code_2023_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): The value being extracted is fundamental human liberty—freedom of speech, privacy, and association. The potential penalty (imprisonment) represents a near-total extraction of autonomy, justifying a high score.
 *   - Suppression (0.80): The law's explicit purpose is to suppress lifestyles and speech that deviate from a state-sanctioned norm, directly replacing a more liberal (albeit colonial-era) legal framework. The availability of alternatives is legally foreclosed.
 *   - Theater (0.20): While framed as an act of decolonization and assertion of national identity, the law has severe, real-world enforcement mechanisms. The theater is present but secondary to the functional extraction and suppression.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the conservative coalition (beneficiary), the law is a Rope that coordinates their political power and enforces a shared social vision, appearing as a pure good (negative extraction). For an individual citizen or a journalist (target), the same law is a Snare, a coercive tool that extracts their freedom with no corresponding benefit, only the threat of punishment. The law's function is perceived entirely differently depending on one's structural position relative to it.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: Conservative religious and nationalist factions gain direct political and social power. The law is their instrument. The system derives a low `d`, reflecting that the constraint works for them.
 *   - Victims: Unmarried citizens, LGBTQ+ individuals, political critics, and journalists directly bear the costs. Their freedom is the resource being extracted. The system derives a high `d` from their victim status and trapped/constrained exit options.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The tourism and foreign investment sectors are institutional victims. Unlike individual citizens, they are `organized` and `mobile`. However, the law still imposes significant costs (risk, reputational damage, difficulty attracting foreign talent), making it a Snare from their perspective. It extracts certainty and value from their operations, demonstrating that even powerful actors can be targets of extraction when they lack the specific institutional power to shape the rule-making process.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] Classifying this law as a simple Snare from all perspectives would be inaccurate, as it would miss its genuine coordination function for the ruling coalition. Classifying it as a Rope would be a gross misrepresentation of its impact on the populace. The analytical classification of Tangled Rope is essential. It correctly identifies the dual nature of the constraint: it is simultaneously a coordination mechanism for one group and a highly extractive mechanism for another, with its existence predicated on active state enforcement. This prevents the mislabeling of coercive social engineering as simple "coordination."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_indonesia_penal_code_2023,
    'Will the law be enforced broadly against the populace, or primarily used selectively to target political opponents and marginalized groups?',
    'Analysis of prosecution data over a 5-10 year period, cross-referenced with the political affiliations and social status of those charged.',
    'Broad enforcement confirms its function as a widespread Snare. Selective enforcement suggests its primary function is a Tangled Rope for political suppression, with the general threat serving as a secondary control mechanism.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing. Interval models the 3-year transition
% plus several years of full enforcement.
narrative_ontology:interval(indonesia_penal_code_2023, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.75 > 0.46), so temporal data is required.
% We model a slight increase in extraction and theater as the law becomes normalized
% and its enforcement apparatus becomes more entrenched.

% Theater ratio over time:
narrative_ontology:measurement(ipc23_tr_t0, indonesia_penal_code_2023, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ipc23_tr_t5, indonesia_penal_code_2023, theater_ratio, 5, 0.18).
narrative_ontology:measurement(ipc23_tr_t10, indonesia_penal_code_2023, theater_ratio, 10, 0.20).

% Extraction over time:
narrative_ontology:measurement(ipc23_ex_t0, indonesia_penal_code_2023, base_extractiveness, 0, 0.70).
narrative_ontology:measurement(ipc23_ex_t5, indonesia_penal_code_2023, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(ipc23_ex_t10, indonesia_penal_code_2023, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The law's function for its beneficiaries is to enforce a
% specific socio-political ideology.
narrative_ontology:coordination_type(indonesia_penal_code_2023, enforcement_mechanism).

% Network relationships: This law directly impacts the investment and tourism
% climate, which can be modeled as separate constraints.
narrative_ontology:affects_constraint(indonesia_penal_code_2023, indonesia_tourism_competitiveness).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% chain based on the declared beneficiary/victim groups and their respective
% exit options accurately models the structural relationships and produces
% the correct directionality (d) values for each perspective.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */