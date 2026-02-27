% ============================================================================
% CONSTRAINT STORY: average_is_over_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_average_is_over_2026, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: average_is_over_2026
 *   human_readable: The AI-Talent Barbell Economy
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The 'AI-Talent Barbell Economy' describes a structural shift where
 *   economic rewards are concentrated at two poles, hollowing out the middle.
 *   At one end are those who can leverage AI and complex systems
 *   ('AI-Leveraging Talent' and 'Capital Owners'). At the other are those
 *   with skills resistant to automation (e.g., high-touch manual trades). In
 *   the middle, the value of routine cognitive work plummets, creating a
 *   large class of economically precarious 'Mid-Skill Cognitive Workers'.
 *   This constraint is not a specific law but the emergent outcome of
 *   technological progress interacting with existing market structures.
 *
 * KEY AGENTS:
 *   - Mid-Skill Cognitive Workers: Primary victims (powerless/trapped) — their skills and economic security are being extracted.
 *   - AI-Leveraging Talent: Primary beneficiaries (powerful/arbitrage) — experience the system as a productivity-enhancing coordination tool.
 *   - Capital Owners: Primary beneficiaries (institutional/arbitrage) — own the AI systems and platforms, capturing the lion's share of productivity gains.
 *   - Policymakers: Institutional actors (institutional/constrained) — attempt to manage the transition with temporary support structures.
 *   - Legacy University System: Institutional actors (institutional/constrained) — their primary function is degrading, persisting through inertia.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(average_is_over_2026, 0.65).
domain_priors:suppression_score(average_is_over_2026, 0.75).
domain_priors:theater_ratio(average_is_over_2026, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(average_is_over_2026, extractiveness, 0.65).
narrative_ontology:constraint_metric(average_is_over_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(average_is_over_2026, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(average_is_over_2026, tangled_rope).
narrative_ontology:human_readable(average_is_over_2026, "The AI-Talent Barbell Economy").
narrative_ontology:topic_domain(average_is_over_2026, "economic/technological").

domain_priors:requires_active_enforcement(average_is_over_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(average_is_over_2026, ai_leveraging_talent).
narrative_ontology:constraint_beneficiary(average_is_over_2026, capital_owners).
narrative_ontology:constraint_victim(average_is_over_2026, mid_skill_cognitive_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MID-SKILL WORKER (SNARE) — From the perspective of an individual whose skills are being devalued by AI, the system is a trap. Their career capital is eroding, wages are stagnating, and the cost of retraining is high with uncertain outcomes. They are trapped within a national economy undergoing this shift. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.92. This is pure extraction.
constraint_indexing:constraint_classification(average_is_over_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE AI-LEVERAGING TALENT (ROPE) — For the top 1-5% of talent who can use AI as a force multiplier, the system is a pure coordination mechanism. It allows them to scale their abilities, find opportunities globally, and generate immense value. They experience no extraction, only subsidy. d≈0.15 (beneficiary+mobile), f(d)≈-0.01, σ=1.2 → χ≈-0.01. Net beneficiary.
constraint_indexing:constraint_classification(average_is_over_2026, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE) — The observer sees both the immense productivity and coordination gains (the Rope function) and the severe, asymmetric extraction from the middle class (the Snare function). The system requires active market enforcement to maintain this disequilibrium. This matches the claimed_type. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(average_is_over_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE POLICYMAKER (SCAFFOLD) — Governments view this economic shift as a temporary, turbulent transition. They implement policies like retraining programs, tax credits, and social safety nets as temporary scaffolds, intended to support the workforce until new, stable job categories emerge. The 'sunset clause' is the implicit belief that the market will eventually create a new equilibrium. This perspective acknowledges the problem but frames it as solvable and temporary.
constraint_indexing:constraint_classification(average_is_over_2026, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE LEGACY UNIVERSITY (PITON) — The traditional university system's function of credentialing for mid-skill cognitive jobs is being hollowed out. Yet, it persists due to institutional inertia, brand value, and its role as a social/cultural ritual. Its core economic function has degraded, while the performative aspects (campus life, alumni networks, signaling) remain. The high theater_ratio (0.75) of the overall constraint captures this perfectly, satisfying the piton gate.
constraint_indexing:constraint_classification(average_is_over_2026, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(average_is_over_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(average_is_over_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(average_is_over_2026, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(average_is_over_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(average_is_over_2026, TR),
    TR >= 0.70.

:- end_tests(average_is_over_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The system systematically transfers economic surplus from labor in the middle of the skill distribution to capital and labor at the top. Suppression (0.75): High. For an individual worker, opting out is not feasible; they must adapt or face marginalization. Alternatives at a systemic level (e.g., different economic models) are heavily suppressed by political and institutional inertia. Theater Ratio (0.75): High. This reflects the massive industry of performative 'upskilling' programs, 'future of work' consulting, and political rhetoric that often fails to deliver concrete pathways for displaced workers, alongside the credentialing rituals of legacy institutions that no longer map to economic reality.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For the beneficiary at the top, this is a golden age of progress and coordination (Rope). For the victim in the middle, it is a coercive trap that devalues their life's work (Snare). Policymakers see a temporary, manageable crisis (Scaffold), while legacy institutions experience their own functional decay (Piton). The analytical view (Tangled Rope) must hold both the coordination and extraction functions in tension to understand the full picture.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (AI Talent, Capital Owners) have arbitrage exit options, leading to a low or negative derived directionality (d), classifying the constraint as a Rope from their view. Victims (Mid-Skill Workers) are trapped, leading to a maximal d-value and a Snare classification. Institutional actors like governments and universities are constrained, leading to intermediate d-values, where the classification is then determined by other factors like the sunset clause (Scaffold) or high theater (Piton).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves a key mandatrophy: mistaking a technologically-driven structural transformation for either pure progress (a Rope) or pure exploitation (a Snare). The Deferential Realism framework shows it is both simultaneously. The analytical classification of Tangled Rope correctly identifies the dual nature of the system—it genuinely coordinates resources in a highly productive way while also creating a severe, asymmetric extraction from a specific, structurally-defined victim class. Ignoring either function leads to a dangerously incomplete model of reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    new_job_creation_rate,
    'Will new job categories, accessible to the median worker, emerge fast enough to absorb those displaced from the ''middle''?',
    'Longitudinal labor market analysis tracking job title creation/destruction and associated wage/skill data.',
    'If new jobs emerge, the constraint softens towards a Scaffold or Rope. If not, it hardens into a permanent Snare for a large segment of the population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(new_job_creation_rate, empirical, 'Rate of emergence for new, accessible job categories.').

omega_variable(
    policy_intervention_efficacy,
    'Can policy interventions like UBI, wealth taxes, or large-scale public retraining programs effectively counteract the market''s extractive tendencies?',
    'Comparative analysis of national policy experiments and their effect on income inequality and social mobility metrics.',
    'High efficacy reduces suppression and effective extraction, shifting the classification. Low efficacy confirms the Snare/Tangled Rope structure as dominant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_intervention_efficacy, preference, 'Efficacy of policy in counteracting market-driven extraction.').

omega_variable(
    talent_vs_access,
    'Is the ''top talent'' on one end of the barbell a reflection of innate ability, or a product of privileged access to elite education, networks, and capital?',
    'Sociological studies correlating ''AI-leveraging'' success with socioeconomic background, controlling for cognitive test scores.',
    'If primarily innate, the structure may resemble a Mountain (a law of cognitive physics). If primarily access, it confirms the structure is a socially constructed Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(talent_vs_access, conceptual, 'Whether the ''talent'' pole is innate ability or privileged access.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(average_is_over_2026, 2022, 2032).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aver_tr_t2022, average_is_over_2026, theater_ratio, 2022, 0.2).
narrative_ontology:measurement(aver_tr_t2027, average_is_over_2026, theater_ratio, 2027, 0.6).
narrative_ontology:measurement(aver_tr_t2032, average_is_over_2026, theater_ratio, 2032, 0.75).

% Extraction over time
narrative_ontology:measurement(aver_be_t2022, average_is_over_2026, base_extractiveness, 2022, 0.3).
narrative_ontology:measurement(aver_be_t2027, average_is_over_2026, base_extractiveness, 2027, 0.55).
narrative_ontology:measurement(aver_be_t2032, average_is_over_2026, base_extractiveness, 2032, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(average_is_over_2026, resource_allocation).
narrative_ontology:affects_constraint(average_is_over_2026, credentialism_as_gatekeeping).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
