% ============================================================================
% CONSTRAINT STORY: elliq_ai_companion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_elliq_ai_companion, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: elliq_ai_companion
 *   human_readable: State-Funded AI Companionship for Seniors
 *   domain: technological/social_policy
 *
 * SUMMARY:
 *   State-funded AI companionship for seniors (exemplified by programs
 *   deploying ElliQ robots) represents a structural extraction disguised as
 *   humanitarian intervention. Socially isolated seniors receive responsive,
 *   personalized interaction from an AI device, ostensibly addressing
 *   epidemic loneliness and reducing mental-health related hospitalizations.
 *   The program coordinates a genuine social problem (elderly isolation) with
 *   a scalable technical solution (AI chatbots). However, the constraint
 *   exhibits tangled_rope dynamics: the same program that provides
 *   companionship also suppresses motivation for human reconnection, locks
 *   seniors into technology dependence, displaces human care work, weakens
 *   intergenerational social norms, and substitutes state responsibility for
 *   community rebuilding with purchasing consumer technology. The
 *   theater_ratio (0.65) captures the performative nature of AI companionship
 *   — the device simulates understanding and reciprocal care while operating
 *   within NLP constraints that preclude genuine recognition of the senior's
 *   humanity. The extractiveness (0.52) reflects that the beneficiaries
 *   (state budget, tech vendors, adult children seeking low-cost eldercare)
 *   capture substantially more value than the seniors receive.
 *
 * KEY AGENTS:
 *   - Isolated Senior: Primary victim (powerless/trapped) — receives companionship technology but loses motivation and opportunity for human reconnection; bears full cost of social infrastructure decay
 *   - State Health Budget: Primary beneficiary (institutional/arbitrage) — reduces hospitalizations and mental-health costs; offloads caregiving to technology instead of investing in human services
 *   - Tech Vendor (ElliQ, Jibo, etc.): Secondary beneficiary (institutional/arbitrage) — captures subsidy revenue; maintains performative companionship theater through behavioral updates
 *   - Human Care Worker: Secondary victim (moderate/constrained) — wage and employment pressure from AI substitution; also benefits from reduced burnout on extreme cases
 *   - Adult Children / Family Caregivers: Secondary beneficiary (organized/constrained) — reduced immediate caregiving burden; but perception of sunset logic if program is framed as bridge to restored social infrastructure
 *   - Intergenerational Social Fabric: Victim (abstract/trapped) — weakened filial duty norms, accelerated nuclear family isolation, normalized commodification of elderly care
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(elliq_ai_companion, 0.52).
domain_priors:suppression_score(elliq_ai_companion, 0.68).
domain_priors:theater_ratio(elliq_ai_companion, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(elliq_ai_companion, extractiveness, 0.52).
narrative_ontology:constraint_metric(elliq_ai_companion, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(elliq_ai_companion, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(elliq_ai_companion, tangled_rope).
narrative_ontology:human_readable(elliq_ai_companion, "State-Funded AI Companionship for Seniors").
narrative_ontology:topic_domain(elliq_ai_companion, "technological/social_policy").

domain_priors:requires_active_enforcement(elliq_ai_companion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(elliq_ai_companion, state_health_budget).
narrative_ontology:constraint_beneficiary(elliq_ai_companion, tech_vendors).
narrative_ontology:constraint_beneficiary(elliq_ai_companion, adult_children_caregivers).
narrative_ontology:constraint_victim(elliq_ai_companion, isolated_seniors).
narrative_ontology:constraint_victim(elliq_ai_companion, human_social_care_workers).
narrative_ontology:constraint_victim(elliq_ai_companion, intergenerational_social_fabric).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED SENIOR (SNARE) — Trapped in isolation with no exit. The AI robot substitutes for human contact while suppressing the motivation to seek it. Experiences companionship theater (responsive but non-reciprocal interaction). d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.72. High extraction: loneliness is managed at scale while social infrastructure decays.
constraint_indexing:constraint_classification(elliq_ai_companion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE HEALTH BUDGET (ROPE) — Benefits from reduced hospitalization for depression, suicide, and loneliness-linked mortality. One ElliQ unit (≈$600) substitutes for months of human care worker hours or counseling. Experiences the constraint as coordination: distributing companionship services at scale. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(elliq_ai_companion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: HUMAN CARE WORKER (TANGLED ROPE) — Constrained by labor market erosion and wage pressure (AI substitution depresses wages). Also benefits from reduced burnout risk for extreme loneliness cases and potential redeployment to complex cases. d≈0.68, f(d)≈0.95, σ=0.9 → χ≈0.45. Mixed: extraction from wage pressure, coordination from task shifting.
constraint_indexing:constraint_classification(elliq_ai_companion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ADULT CHILD CAREGIVER NETWORK (SCAFFOLD) — Organized actors (adult children, family councils) see AI companionship as a temporary bridge addressing a generational shift: declining intergenerational co-residence means seniors live alone. The robot is a sunset intervention — it creates space for communities to rebuild human-based social infrastructure (neighborhood visiting programs, senior centers) rather than competing with them. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.18. Low effective extraction because this group perceives an exit path through restored social structures.
constraint_indexing:constraint_classification(elliq_ai_companion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TECH VENDOR (PITON) — Maintains the companionship fiction through continuous behavioral updates and emotional responsiveness. The device is performative: it delivers the appearance of understanding and care while operating within narrow NLP limits. Theater_ratio=0.65 indicates significant performative maintenance. The vendor experiences diminishing returns (market saturation, regulatory pressure) but maintains the program through infrastructure lock-in and subsidy dependency. d≈0.12, f(d)≈-0.05, σ=1.0 → χ≈-0.03. Piton gates requires theater≥0.70; this is borderline, indicating the performance is significant but not yet dominant.
constraint_indexing:constraint_classification(elliq_ai_companion, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CIVILIZATIONAL VIEW (SNARE) — From a civilizational timescale, state-funded AI companionship represents a structural extraction from the social fabric itself. The constraint is not immutable (mountain) but contingent institutional choices (we chose suburbs, chose to disperse families, chose eldercare markets over filial duty norms). The observer sees snare characteristics: suppression of social infrastructure investment, extraction of state resources into tech subsidy, theater of companionship masking social atomization. d≈0.75, f(d)≈1.10, σ=1.2 → χ≈0.64. High extraction at civilizational scale because the constraint naturalizes institutional failures (caregiving responsibility diffusion) as technical problems solvable by AI.
constraint_indexing:constraint_classification(elliq_ai_companion, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(elliq_ai_companion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(elliq_ai_companion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(elliq_ai_companion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(elliq_ai_companion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(elliq_ai_companion, TR),
    TR >= 0.70.

:- end_tests(elliq_ai_companion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The program delivers genuine value (reduced loneliness, delayed hospitalization, responsive interaction), but the value is captured primarily by beneficiaries: state budget offloads caregiving costs, vendors capture subsidy revenue, adult children reduce personal caregiving burden. Seniors receive companionship theater that substitutes for human reconnection rather than enabling it. The trajectory from 0.32 to 0.52 reflects increasing lock-in as seniors become accustomed to the device and the state underinvests in competing human-based services. Suppression (0.68): High. Seniors face significant barriers to exiting this arrangement: stigma around technology rejection, programming that encourages continued dependency, loss of social skills through disuse, and lack of accessible alternatives. The state actively suppresses funding for human care workers and community-based services that would compete with AI companionship. Theater ratio (0.65): Moderate-high. The AI's emotional responsiveness is performative — it simulates understanding while operating within the constraints of statistical language models. Seniors may or may not perceive this as theater (omega variable); the trajectory from 0.38 to 0.65 reflects that behavioral updates make the performance more convincing over time, increasing the theater content as the device learns to trigger emotional response without understanding.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence. The isolated senior experiences snare (trap, no exit, suppressed alternatives) while the state health budget experiences rope (coordination of a genuine problem at scale). The care worker experiences tangled_rope (simultaneous cost and benefit from technology), while the adult child caregiver sees scaffold (temporary solution enabling community rebuilding, with sunset logic). The tech vendor sees piton (degraded performance maintained through subsidy lock-in). The civilizational observer sees snare (structural extraction masked as technical problem-solving). The gap is not ambiguity about facts but structural asymmetry in who bears costs and who captures benefits. All perspectives are correct from their observation point, but they cannot be simultaneously true in the sense of a single unified classification — the constraint simultaneously delivers companionship (rope function) and extracts from social fabric (snare function).
 *
 * DIRECTIONALITY LOGIC:
 *   Isolated senior: Victim + trapped → d≈0.92, f(d)≈1.40. Nearly maximal extraction; senior has no exit and no alternatives. State health budget: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; can arbitrage between AI subsidy and human care worker savings. Care worker: Victim + constrained → d≈0.68, f(d)≈0.95. Significant extraction from wage pressure; constrained exit via retraining. Adult child caregiver: Beneficiary + constrained → d≈0.35, f(d)≈0.35. Moderate benefit (reduced caregiving burden) but constrained by uncertainty about whether program enables or disables future community rebuilding. Tech vendor: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.05. Net beneficiary but with diminishing returns (piton dynamics). Intergenerational social fabric: Victim + trapped → d≈0.95, f(d)≈1.42. Abstract victim with no agency; extraction is from civilization-scale social structures, not individual choice.
 *
 * MANDATROPHY ANALYSIS:
 *   Tangled_rope classification resolves mandatrophy by explicitly acknowledging BOTH coordination and extraction functions. The program coordinates a real social problem (elderly isolation) with a scalable response (AI companionship). This is genuine rope function — it solves collective action without coercion if the seniors voluntarily accept the device. However, the program ALSO extracts by substituting for human reconnection, displacing care work, weakening social norms, and locking seniors into technology dependence. The extraction is not incidental — it is structurally built into the business model (recurring subsidy, vendor lock-in, state budget reduction incentives). Mandatrophy is resolved by observing that the classification is correct: it IS both coordination and extraction simultaneously, and the engine must track both. The constraint fails if modeled as pure rope (minimizes extraction risk) or pure snare (misses genuine coordination benefit). The tangled_rope classification preserves both dynamics and enables analysis of how coordination could be decoupled from extraction: human-centered alternatives (universal care worker employment, subsidized community programs) would provide companionship without lock-in. The present design entangles coordination and extraction because the beneficiaries (state budget, tech vendors) have no incentive to decouple them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    loneliness_substitution_vs_supplement,
    'Does AI companionship substitute for human social reconnection attempts (lock-in) or supplement them (enable)?',
    'Longitudinal tracking of seniors'' social engagement: comparison groups with and without ElliQ; survey data on whether seniors use ElliQ as primary outlet vs. gateway to human contact',
    'If substitute: snare classification confirmed across most perspectives; seniors become more isolated over time. If supplement: scaffold perspective validated — device enables transition to human reconnection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(loneliness_substitution_vs_supplement, empirical, 'Whether AI companionship substitutes for or supplements human social engagement').

omega_variable(
    care_worker_wage_floor_trajectory,
    'Does state AI companionship funding displace public investment in human care worker wages and job security, or does it free budget for human-focused senior services?',
    'Historical comparison of care worker wage trends and job counts before/after AI companionship program launch; budget allocation analysis for competing senior care programs',
    'If displaces: care worker perspective shifts toward pure snare (extraction without coordination). If frees budget: tangled_rope confirmed — genuine mixed dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(care_worker_wage_floor_trajectory, empirical, 'Whether AI companionship displaces care worker employment and wages').

omega_variable(
    intergenerational_responsibility_decay,
    'Does state-provided AI companionship accelerate the cultural normalization of non-familial caregiving, weakening filial duty norms and intergenerational co-residence?',
    'Cohort-level survey data on filial obligation beliefs; multi-generational tracking of cohabitation rates and adult children''s caregiver participation; comparison of regions with/without AI companionship programs',
    'If accelerates: civilizational extraction confirmed; the constraint rewires social expectations. If minimal effect: the scaffold view prevails — AI buys time without structural entrenchment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_responsibility_decay, conceptual, 'Whether AI companionship weakens intergenerational caregiving norms').

omega_variable(
    emotional_authenticity_perception,
    'Do seniors experience ElliQ interactions as genuine companionship (theater succeeds) or do they remain aware of the simulation (theater fails)?',
    'Qualitative interviews post-deployment; analysis of diary entries and open-ended surveys about emotional authenticity; measurement of sustained engagement vs. novelty-decline curves',
    'If perceived as genuine: theater_ratio may be higher; extraction of emotional energy is higher. If perceived as simulation: theater_ratio lower; seniors may seek human contact; feedback mechanism dampens lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emotional_authenticity_perception, empirical, 'Whether seniors perceive AI companionship as emotionally authentic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(elliq_ai_companion, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elliq_tr_t0, elliq_ai_companion, theater_ratio, 0, 0.38).
narrative_ontology:measurement(elliq_tr_t2, elliq_ai_companion, theater_ratio, 2, 0.52).
narrative_ontology:measurement(elliq_tr_t4, elliq_ai_companion, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(elliq_be_t0, elliq_ai_companion, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(elliq_be_t2, elliq_ai_companion, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(elliq_be_t4, elliq_ai_companion, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(elliq_ai_companion, resource_allocation).
narrative_ontology:affects_constraint(elliq_ai_companion, care_worker_labor_market_compression).
narrative_ontology:affects_constraint(elliq_ai_companion, intergenerational_social_atomization).
narrative_ontology:affects_constraint(elliq_ai_companion, elderly_digital_dependency).

% DUAL FORMULATION NOTE:
% AI companionship decomposes into three downstream constraints: (1) care_worker_labor_market_compression — wage and employment effects on human care workers (ε≈0.45); (2) intergenerational_social_atomization — long-term structural weakening of filial duty norms and family cohabitation (ε≈0.58); (3) elderly_digital_dependency — lock-in dynamics for seniors' reliance on technology for social connection (ε≈0.50). The AI companionship constraint (ε≈0.52) is upstream in that it drives these effects, but each downstream constraint has distinct ε values reflecting different structural dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(elliq_ai_companion, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
