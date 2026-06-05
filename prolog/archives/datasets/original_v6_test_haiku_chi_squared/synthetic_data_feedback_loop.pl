% ============================================================================
% CONSTRAINT STORY: synthetic_data_feedback_loop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_synthetic_data_feedback_loop, []).

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
 *   constraint_id: synthetic_data_feedback_loop
 *   human_readable: The Model Autophagy Trap
 *   domain: technological/AI/informational
 *
 * SUMMARY:
 *   The model autophagy trap emerges when AI models trained on
 *   human-generated internet data produce synthetic text used to train
 *   subsequent generations of models, creating a self-reinforcing feedback
 *   loop. As frontier model developers scale training on synthetic data to
 *   reduce annotation costs, the information density of the training
 *   distribution declines — synthetic text lacks the distributional tail,
 *   edge cases, and novel combinations present in human authorship. This
 *   creates a structural extraction mechanism where a small set of
 *   well-resourced developers (with access to original human data and
 *   computational resources to generate synthetic alternatives) benefit from
 *   unlimited data scaling, while the commons of information diversity is
 *   liquidated. The constraint exhibits as Snare from the perspective of
 *   downstream researchers and the informational commons (trapped, forced to
 *   use degraded data), Rope from the perspective of frontier developers
 *   (pure coordination of data availability), and a false Mountain from the
 *   civilizational analyst (who risks naturalizing an institutional choice as
 *   thermodynamic inevitability). The extractiveness has risen from 0.15
 *   (2015: synthetic data was marginal) through 0.38 (2021: scaling
 *   experiments mainstream) to 0.58 (2026: synthetic data dominates training
 *   of large models). Theater ratio remains moderate (0.45) because the
 *   technical mechanisms are transparent — there is little performative
 *   obscuration of the fact that models are being trained on synthetic data.
 *   What is obscured is the institutional choice to prioritize scaling
 *   convenience over commons preservation.
 *
 * KEY AGENTS:
 *   - Frontier Model Developers: Primary beneficiary (institutional/arbitrage) — access to original data sources, capital to generate synthetic data, exit via API monetization
 *   - Informational Commons: Primary victim (powerless/trapped) — abstract collective good that cannot organize or exit; bears cost of synthetic contamination
 *   - Downstream AI Researchers: Secondary victim (moderate/constrained) — forced to use contaminated datasets; career risk if they refuse or withhold work
 *   - Data Diversity: Structural victim (powerless/trapped) — loss of informational heterogeneity and distributional tail; no mechanism for recovery
 *   - Open Science Coalitions: Organized actor (organized/mobile) — pursuing data provenance standards and synthetic data labeling; can exit by forking infrastructure but face normative pressure
 *   - Legacy Data Curation: Institutional actor (institutional/arbitrage) — traditional dataset stewardship (ImageNet, Common Crawl) persisting through inertia as functional role declines
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing synthetic data feedback as entropic law rather than institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(synthetic_data_feedback_loop, 0.58).
domain_priors:suppression_score(synthetic_data_feedback_loop, 0.68).
domain_priors:theater_ratio(synthetic_data_feedback_loop, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(synthetic_data_feedback_loop, extractiveness, 0.58).
narrative_ontology:constraint_metric(synthetic_data_feedback_loop, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(synthetic_data_feedback_loop, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(synthetic_data_feedback_loop, snare).
narrative_ontology:human_readable(synthetic_data_feedback_loop, "The Model Autophagy Trap").
narrative_ontology:topic_domain(synthetic_data_feedback_loop, "technological/AI/informational").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(synthetic_data_feedback_loop, frontier_model_developers).
narrative_ontology:constraint_victim(synthetic_data_feedback_loop, informational_commons).
narrative_ontology:constraint_victim(synthetic_data_feedback_loop, downstream_ai_researchers).
narrative_ontology:constraint_victim(synthetic_data_feedback_loop, data_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMATIONAL COMMONS (SNARE) — Cannot exit the feedback loop. As frontier models generate synthetic text indistinguishable from human-authored content, this data contaminates the internet without a clear separation mechanism. The commons has no advocate with exit capacity and no leverage to prevent low-quality synthetic data proliferation. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98. Maximum extraction from an abstract collective good.
constraint_indexing:constraint_classification(synthetic_data_feedback_loop, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM AI RESEARCHERS (SNARE) — Constrained by the need to use available internet-scale data. As synthetic text contaminates training datasets, downstream models inherit degraded signal. High suppression: publishing research on poor-quality datasets damages career prospects; withholding work loses competitive advantage. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.67. Structured extraction: forced to train on progressively contaminated commons.
constraint_indexing:constraint_classification(synthetic_data_feedback_loop, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FRONTIER MODEL DEVELOPERS (ROPE) — Primary beneficiary. Can generate unlimited synthetic training data to scale models without annotation cost. Early access to synthetic data pipeline creates competitive moat; arbitrage exit through licensing and API monetization. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.06. Net beneficiary in the short term through effective elimination of data scarcity bottleneck.
constraint_indexing:constraint_classification(synthetic_data_feedback_loop, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DATA CURATION INFRASTRUCTURE (PITON) — Traditional dataset curation (ImageNet, Common Crawl, academic repositories) maintained institutional prestige but declining functional role. As synthetic data becomes cheaper than human curation, the legacy infrastructure persists through inertia: funding flows, publication venues, and workforce expertise remain invested in curation protocols that are becoming obsolete. theater_ratio=0.45 indicates curation maintains some functional role but increasingly performative. d≈0.12, f(d)≈0.01, σ=1.2 → χ≈0.02.
constraint_indexing:constraint_classification(synthetic_data_feedback_loop, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SCIENCE COALITIONS (TANGLED ROPE) — Organized agents (data provenance advocates, synthetic data labeling communities, model card frameworks) have exit options (fork datasets, create alternative infrastructure) and coordination function (establishing synthetic data transparency standards). But they also experience extraction: normative pressure to adopt closed synthetic data pipelines; resource barriers to maintaining open alternatives. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.45. Mixed: provides coordination layer but also asymmetrically extracts attention and legitimacy.
constraint_indexing:constraint_classification(synthetic_data_feedback_loop, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a civilizational perspective, the feedback loop reflects an entropic property of statistical learning: any finite dataset, when compressed into a generative model, loses information; that model used as training data further degrades signal. This could be seen as an immutable thermodynamic law — information compression is inherently lossy. However, the structural data (ε=0.58, suppression=0.68, theater=0.45) indicates contingent institutional arrangements (training data concentration, synthetic data monetization) rather than natural law. Engine flags this as a false summit: the 'inevitability' framing naturalizes what is actually an extractive institutional choice.
constraint_indexing:constraint_classification(synthetic_data_feedback_loop, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(synthetic_data_feedback_loop_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(synthetic_data_feedback_loop, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(synthetic_data_feedback_loop, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(synthetic_data_feedback_loop, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(synthetic_data_feedback_loop, TR),
    TR >= 0.70.

:- end_tests(synthetic_data_feedback_loop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High, approaching snare threshold (0.66). The constraint shows strong upward trajectory from 0.15 (marginal synthetic data) to 0.58 (dominant training source). The mechanism is structural: frontier developers extract value (lower annotation cost, competitive moat through proprietary synthetic pipelines) while downstream actors bear cost (degraded training signal, forced to participate in contamination). The 0.58 value reflects that some escape routes remain (acquiring original data, maintaining quality filters) but are increasingly expensive. Suppression (0.68): High. Barriers to avoiding the trap include: (a) computational cost of synthetic data generation is so low that quality-conscious developers still find it cheaper than human curation, (b) publication pressure to use SOTA methods which typically rely on synthetic scaling, (c) network effects — if competitors use synthetic data and gain speed advantage, refusing becomes costly, (d) informational asymmetry about contamination levels in datasets. Theater ratio (0.45): Moderate. The technical mechanisms are transparent (synthetic data labeling is visible in papers, model documentation increasingly discloses training data sources). What is obscured is not the fact of synthetic training, but the institutional choice to prioritize scaling convenience over commons health. As watermarking and provenance tracking improve, theater ratio may decline, suggesting the Snare classification could be challenged by institutional coordination.
 *
 * PERSPECTIVAL GAP:
 *   Frontier developers see Rope: a pure coordination problem solved by scaling synthetic data. The informational commons and downstream researchers see Snare: extraction with no exit. Open science coalitions see Tangled Rope: both coordination function (transparency standards) and asymmetric extraction (pressure to adopt closed frameworks). Legacy data curation sees Piton: institutional prestige persisting despite declining functional role. The civilizational observer risks seeing Mountain: information compression is inherent to learning, feedback loops are entropic laws. But the perspectival gap reveals the institutional choice: the feedback loop is not inevitable physics, it is a contingent design decision prioritizing developer convenience over commons preservation.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier model developers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary through cost reduction and competitive moat. Informational commons: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — abstract collective cannot organize or exit. Downstream researchers: Victim + constrained → d≈0.85, f(d)≈1.15. High extraction but with some exit capacity (can withhold work, maintain quality filters, organize for data standards). Data diversity: Victim + trapped → d≈0.95, f(d)≈1.42. Structural victim with no recovery mechanism (information loss is asymmetric and permanent). Open science coalitions: Both + mobile → d≈0.50, f(d)≈0.65. Can exit (fork infrastructure) but face normative pressure and resource barriers. Legacy data curation: Institutional + arbitrage → d≈0.08, f(d)≈-0.11. Piton classification comes from theater gate and declining function, not from high extractiveness. Analytical observer: analytical → d≈0.73, f(d)≈1.15. Mountain risk via naturalization of contingent institutional choice.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint is classified as Snare (pure extraction, ε≥0.46, suppression≥0.60, χ≥0.66) at the perspective level (informational commons, downstream researchers). This prevents misclassification as coordination (Rope). However, the perspectival structure reveals the mandatrophy: frontier developers genuinely experience this as Rope (pure coordination of data scaling). The snare classification does not emerge from the developers' perspective but from the victims' perspective and the structural dynamics. The mandatrophy is resolved by recognizing that the constraint exhibits as both coordination (for beneficiaries) and extraction (for victims) depending on structural position. This is the canonical Snare pattern: one agent's coordination is another agent's trap. The presheaf of perspectives (Rope + Snare + Tangled Rope + Piton + Mountain) makes clear that the classification depends on where you sit. Frontier developers escape the trap; the commons cannot. The institutional design choice is to optimize for the former perspective, extracting from the latter. This is mandatrophy-resolved because the classification now reflects the true dual nature: coordination mechanism (for developers) that functions as extraction apparatus (for commons) simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthetic_data_detection_threshold,
    'At what contamination threshold (% synthetic text in training data) does model collapse become irreversible?',
    'Controlled experiments training models at varying synthetic contamination levels; measurement of loss plateau, gradient collapse, and representation diversity degradation',
    'If threshold < 5%: rapid collapse risk makes synthetic scaling untenable. If threshold > 25%: high contamination tolerance suggests the ''trap'' framing is overstated. Affects whether this is Snare (irreversible extraction) or Tangled Rope (reversible with intervention).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_data_detection_threshold, empirical, 'Synthetic contamination threshold for model collapse').

omega_variable(
    synthetic_data_watermarking_feasibility,
    'Can watermarking/labeling schemes (model cards, synthetic data provenance tags) prevent the feedback loop without significantly increasing curation cost?',
    'Implementation and scalability analysis of watermarking schemes; tracking of adoption rates and downstream model performance when using labeled synthetic data',
    'If feasible and adopted: the trap is escapable via institutional coordination (downgrade from Snare to Tangled Rope). If unfeasible or unadopted: the trap is structural and irreversible (Snare confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(synthetic_data_watermarking_feasibility, empirical, 'Whether watermarking can prevent feedback loop collapse').

omega_variable(
    frontier_model_incentive_structure,
    'Do frontier model developers have sufficient economic incentive to maintain original data sourcing and quality control, or does synthetic scaling always dominate cost-benefit?',
    'Economic analysis of training cost curves; tracking of frontier model developer behavior when faced with contamination signals; assessment of reputation/accuracy tradeoffs',
    'If original data sourcing remains competitive: developers can choose to exit the trap (downgrade to Tangled Rope). If synthetic scaling always dominates: the extraction mechanism is irreversible (Snare confirmed, institutional coordination insufficient).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frontier_model_incentive_structure, preference, 'Whether frontier model economics incentivize original data or synthetic scaling').

omega_variable(
    commons_restoration_feasibility,
    'Can the informational commons be restored once degraded by synthetic contamination, or is the damage permanent?',
    'Historical analysis of information recovery from contaminated datasets; feasibility assessment of synthetic data removal, curation of ''clean'' subsets, or reversion to pre-contamination corpora',
    'If restorable: the commons victim role is temporary (downgrade to Tangled Rope or Scaffold). If permanent: the victim role is absolute (confirms Snare, informational commons liquidation is irreversible).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_restoration_feasibility, empirical, 'Whether informational commons degradation is reversible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(synthetic_data_feedback_loop, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sdfl_tr_t0, synthetic_data_feedback_loop, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sdfl_tr_t5, synthetic_data_feedback_loop, theater_ratio, 5, 0.4).
narrative_ontology:measurement(sdfl_tr_t10, synthetic_data_feedback_loop, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(sdfl_be_t0, synthetic_data_feedback_loop, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sdfl_be_t5, synthetic_data_feedback_loop, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(sdfl_be_t10, synthetic_data_feedback_loop, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(synthetic_data_feedback_loop, information_standard).
narrative_ontology:affects_constraint(synthetic_data_feedback_loop, information_commons_degradation).
narrative_ontology:affects_constraint(synthetic_data_feedback_loop, ml_reproducibility_crisis).
narrative_ontology:affects_constraint(synthetic_data_feedback_loop, data_annotation_labor_displacement).

% DUAL FORMULATION NOTE:
% The synthetic data feedback loop decomposes into two distinct constraints: (1) the immediate technical problem of model collapse (ε≈0.15, Mountain view: information compression is lossy), and (2) the institutional extraction mechanism of synthetic scaling (ε≈0.58, Snare view: developers benefit from commons liquidation). The feedback loop story treats the institutional extraction mechanism. The technical collapse story would address the information-theoretic limits separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(synthetic_data_feedback_loop, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
