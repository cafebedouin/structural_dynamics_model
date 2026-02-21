% ============================================================================
% CONSTRAINT STORY: fda_accelerated_approval_alz
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_fda_accelerated_approval_alz, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fda_accelerated_approval_alz
 *   human_readable: FDA Accelerated Approval Pathway for Alzheimer's Drugs
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The FDA's accelerated approval pathway allows drugs for serious conditions
 *   to be approved based on a surrogate endpoint (e.g., amyloid plaque
 *   reduction) that is "reasonably likely" to predict clinical benefit, rather
 *   than proven clinical benefit (e.g., slowed cognitive decline). This story
 *   models the application of this pathway to high-cost Alzheimer's drugs
 *   like aducanumab and lecanemab, creating a system with both a genuine
 *   coordination goal (speeding treatments to patients) and a significant
 *   extractive component (high revenues for drugmakers based on unproven
 *   real-world efficacy).
 *
 * KEY AGENTS (by structural relationship):
 *   - Alzheimer's Patients & Families: Primary target (powerless/trapped) — bear the costs of treatment, side effects, and potentially false hope for a marginal or unproven clinical benefit.
 *   - Pharmaceutical Manufacturers: Primary beneficiary (institutional/arbitrage) — gain market access and billions in revenue years earlier than via traditional pathways, capitalizing on the surrogate endpoint.
 *   - FDA (Food and Drug Administration): Institutional Actor (institutional/constrained) — the enforcer and architect of the constraint, balancing a mandate to speed up drug access against duties of ensuring safety and efficacy. Their exit is constrained by their public mission.
 *   - CMS (Centers for Medicare & Medicaid Services): Institutional Actor (institutional/constrained) — a primary payer for these drugs, bearing immense financial extraction on behalf of the public. Their exit is constrained by political and legal obligations to cover approved treatments.
 *   - Clinical Researchers: Analytical observer — evaluate the structural gap between the surrogate endpoint and meaningful clinical outcomes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fda_accelerated_approval_alz, 0.48). % High cost & side effects vs. modest/unproven clinical benefit.
domain_priors:suppression_score(fda_accelerated_approval_alz, 0.65).   % Crowds out R&D on non-amyloid targets and suppresses the alternative of waiting for definitive clinical data.
domain_priors:theater_ratio(fda_accelerated_approval_alz, 0.20).       % Not pure theater, as plaque is removed, but the link to the real goal is the core issue.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fda_accelerated_approval_alz, extractiveness, 0.48).
narrative_ontology:constraint_metric(fda_accelerated_approval_alz, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fda_accelerated_approval_alz, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fda_accelerated_approval_alz, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(fda_accelerated_approval_alz). % The FDA must actively grant and maintain the approvals.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(fda_accelerated_approval_alz, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(fda_accelerated_approval_alz, fda_regulators). % Fulfills mandate to act.

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(fda_accelerated_approval_alz, alzheimers_patients).
narrative_ontology:constraint_victim(fda_accelerated_approval_alz, public_payers_cms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (PATIENTS)
% As victims with trapped exit, their derived d is high (~0.95), leading to
% high effective extraction (χ ≈ 0.48 * 1.42 * 1.0 = 0.68). This high χ,
% combined with high suppression, meets the Snare criteria. They experience a
% high-cost, high-risk system with uncertain rewards.
constraint_indexing:constraint_classification(fda_accelerated_approval_alz, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (PHARMA)
% As beneficiaries with arbitrage exit, their derived d is very low (~0.05),
% leading to negative effective extraction (χ ≈ 0.48 * -0.12 * 1.0 = -0.06).
% The pathway is a pure coordination mechanism for them, efficiently
% converting R&D into revenue.
constraint_indexing:constraint_classification(fda_accelerated_approval_alz, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (RESEARCHERS)
% The analytical observer sees the dual function: a stated coordination goal
% (speeding treatment) and a clear extractive outcome. The engine derives
% d≈0.72, giving χ ≈ 0.48 * 1.15 * 1.2 = 0.66. With high ε, high suppression,
% and declared beneficiary/victim/enforcement, this classifies as a Tangled Rope.
constraint_indexing:constraint_classification(fda_accelerated_approval_alz, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 4A: THE REGULATOR (FDA)
% As an institutional beneficiary with constrained exit, the FDA's derived d
% is higher than pharma's but still low (d~0.3). They see the pathway as a
% successful coordination tool that fulfills their mandate.
% χ ≈ 0.48 * f(0.3) * 1.0 ≈ 0.48 * 0.17 ≈ 0.08. This is very low extraction,
% classifying as a Rope.
constraint_indexing:constraint_classification(fda_accelerated_approval_alz, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4B: THE PAYER (CMS)
% As an institutional victim with constrained exit, CMS experiences the
% extraction directly. Their derived d is high (d~0.9).
% χ ≈ 0.48 * f(0.9) * 1.0 ≈ 0.48 * 1.37 ≈ 0.66. They see a highly extractive
% system that strains their budget for questionable benefit, classifying it
% as a Snare or, acknowledging the coordination goal, a Tangled Rope.
constraint_indexing:constraint_classification(fda_accelerated_approval_alz, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fda_accelerated_approval_alz_tests).

test(perspectival_gap_patient_vs_pharma) :-
    constraint_indexing:constraint_classification(fda_accelerated_approval_alz, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(fda_accelerated_approval_alz, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(inter_institutional_gap_fda_vs_cms) :-
    constraint_indexing:constraint_classification(fda_accelerated_approval_alz, rope, CtxFda),
    CtxFda = context(agent_power(institutional), _, exit_options(constrained), _),
    constraint_indexing:constraint_classification(fda_accelerated_approval_alz, tangled_rope, CtxCms),
    CtxCms = context(agent_power(institutional), _, exit_options(constrained), _),
    % Test ensures the model can differentiate two institutional actors with the same exit, based on beneficiary/victim status.
    narrative_ontology:constraint_beneficiary(fda_accelerated_approval_alz, fda_regulators),
    narrative_ontology:constraint_victim(fda_accelerated_approval_alz, public_payers_cms).

test(tangled_rope_conditions_met) :-
    narrative_ontology:constraint_claim(fda_accelerated_approval_alz, tangled_rope),
    domain_priors:base_extractiveness(fda_accelerated_approval_alz, E), E >= 0.30,
    domain_priors:suppression_score(fda_accelerated_approval_alz, S), S >= 0.40,
    domain_priors:requires_active_enforcement(fda_accelerated_approval_alz),
    narrative_ontology:constraint_beneficiary(fda_accelerated_approval_alz, _),
    narrative_ontology:constraint_victim(fda_accelerated_approval_alz, _).

:- end_tests(fda_accelerated_approval_alz_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): High, reflecting the multi-billion dollar cost to the healthcare system for drugs with statistically small and clinically debated benefits, plus the physical costs of side effects (ARIA).
 *   - Suppression (0.65): High. The focus on amyloid-targeting drugs via this pathway crowds out funding and research for alternative hypotheses of Alzheimer's pathology. It also suppresses the "wait for proof" alternative, creating immense pressure on regulators and doctors to act.
 *   - Classification: The constraint is a canonical Tangled Rope. It possesses a genuine coordination function (getting drugs to desperate patients faster) which is declared via `constraint_beneficiary`. However, it also has a powerful asymmetric extraction function (immense profits on uncertain outcomes) declared via `constraint_victim`, and requires `active_enforcement` by the FDA.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For pharmaceutical firms (beneficiary), it's a Rope that solves the coordination problem of capitalizing on promising but unproven research. For patients (victim), it's a Snare that extracts their hope, time, and money (via the healthcare system) in exchange for a high-risk, low-certainty outcome.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story showcases a critical inter-institutional conflict. The FDA and CMS, both `institutional` actors with `constrained` exit, perceive the same constraint differently due to their structural roles.
 *   - The FDA, as a beneficiary (fulfilling its mandate), sees a functional Rope.
 *   - CMS, as a victim (bearing the financial cost), sees a highly extractive Tangled Rope, bordering on a Snare.
 *   This gap is not a matter of opinion but a direct consequence of their differing structural relationships to the flow of costs and benefits, correctly captured by the directionality engine via their beneficiary/victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the hybrid nature of the pathway. A simplistic analysis would label it either "good" (a Rope for innovation) or "bad" (a Snare for pharma greed). The Tangled Rope classification captures the reality: it is a system that *simultaneously* serves a coordination function and an extraction function. The framework's purpose is to quantify this tension, not resolve it into a binary judgment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_fda_accelerated_approval_alz,
    'Is amyloid plaque reduction a valid surrogate endpoint for meaningful clinical benefit in Alzheimer''s disease?',
    'Long-term, independent, post-market clinical trials that definitively correlate plaque removal with slowed cognitive decline.',
    'If true, the base extractiveness (ε) of the constraint drops significantly, and it shifts towards a Rope. If false, ε is confirmed or even rises, and the Snare classification for patients is validated.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fda_accelerated_approval_alz, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This pathway's application has drifted from its original intent (e.g., for
% HIV/AIDS where surrogate endpoints were highly predictive) to more
% ambiguous areas like Alzheimer's, showing significant extraction_accumulation.

% Theater ratio over time (stable):
narrative_ontology:measurement(fda_tr_t0, fda_accelerated_approval_alz, theater_ratio, 0, 0.10).
narrative_ontology:measurement(fda_tr_t5, fda_accelerated_approval_alz, theater_ratio, 5, 0.15).
narrative_ontology:measurement(fda_tr_t10, fda_accelerated_approval_alz, theater_ratio, 10, 0.20).

% Extraction over time (accumulating):
narrative_ontology:measurement(fda_ex_t0, fda_accelerated_approval_alz, base_extractiveness, 0, 0.20). % Initial use in HIV/AIDS.
narrative_ontology:measurement(fda_ex_t5, fda_accelerated_approval_alz, base_extractiveness, 5, 0.35). % Expansion into less certain oncology endpoints.
narrative_ontology:measurement(fda_ex_t10, fda_accelerated_approval_alz, base_extractiveness, 10, 0.48). % Application to Alzheimer's drugs.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It allocates R&D capital and patient/payer resources.
narrative_ontology:coordination_type(fda_accelerated_approval_alz, resource_allocation).

% Network relationships: This policy directly influences the entire ecosystem of
% pharmaceutical R&D and public healthcare spending.
narrative_ontology:affects_constraint(fda_accelerated_approval_alz, pharma_rd_incentives).
narrative_ontology:affects_constraint(fda_accelerated_approval_alz, medicare_budget_sustainability).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */