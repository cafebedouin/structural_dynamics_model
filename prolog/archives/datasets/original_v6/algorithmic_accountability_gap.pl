% ============================================================================
% CONSTRAINT STORY: algorithmic_accountability_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_accountability_gap, []).

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
 *   constraint_id: algorithmic_accountability_gap
 *   human_readable: Algorithmic Accountability Gap
 *   domain: technology/governance/social
 *
 * SUMMARY:
 *   The algorithmic accountability gap represents a structural constraint in
 *   which corporations deploying algorithms at scale enjoy both opacity
 *   (concealed decision logic, proprietary training data) and asymmetric
 *   power (unilateral deployment, limited appeal mechanisms, regulatory
 *   evasion). Individuals and communities subject to algorithmic decisions
 *   face opaque systems they cannot exit, understand, or effectively
 *   challenge. This gap is not a technical inevitability — interpretability
 *   research demonstrates feasible transparency — but rather the result of
 *   institutional choices to preserve opacity as a competitive advantage and
 *   shield against liability. The constraint exhibits tangled rope structure:
 *   genuine coordination problems exist (communicating algorithm performance
 *   to stakeholders) alongside systematic extraction (opacity as a mechanism
 *   to avoid accountability). Theater has increased over time as corporations
 *   have adopted explainability reporting and fairness audits that perform
 *   accountability without achieving it. Regulatory frameworks (EU AI Act,
 *   algorithmic impact assessments) represent a sunset approach, building
 *   requirements that systematically reduce the opacity advantage. The
 *   constraint operates globally but encounters different institutional
 *   resistance in different jurisdictions, creating regulatory arbitrage
 *   risks.
 *
 * KEY AGENTS:
 *   - Individuals Subject to Algorithms: Primary victim (powerless/trapped) — credit decisions, hiring filters, content curation, predictive policing; no exit option, no transparency, no appeal mechanism
 *   - Affected Communities: Secondary victim (organized/constrained) — minorities subject to algorithmic bias in policing; communities in lending discrimination; can advocate for regulation but cannot unilaterally opt out
 *   - Algorithm-Deploying Corporations: Primary beneficiary (institutional/arbitrage) — preserve competitive advantage through opacity; capture market value from algorithmic optimization; can arbitrage between transparency demands and operational secrecy
 *   - Algorithmic Transparency Advocates: Secondary victim (moderate/constrained) — researchers, NGOs, regulators demanding explainability; constrained by corporate ownership of models and data; carry cost of transparency labor
 *   - Regulatory Agencies: Organized actors (organized/constrained) — building accountability frameworks (EU AI Act, impact assessments) with sunset logic; constrained by jurisdictional limits and corporate resistance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating technical opacity as natural law when it is partly institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_accountability_gap, 0.58).
domain_priors:suppression_score(algorithmic_accountability_gap, 0.65).
domain_priors:theater_ratio(algorithmic_accountability_gap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_accountability_gap, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_accountability_gap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(algorithmic_accountability_gap, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_accountability_gap, tangled_rope).
narrative_ontology:human_readable(algorithmic_accountability_gap, "Algorithmic Accountability Gap").
narrative_ontology:topic_domain(algorithmic_accountability_gap, "technology/governance/social").

domain_priors:requires_active_enforcement(algorithmic_accountability_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_accountability_gap, algorithm_deploying_corporations).
narrative_ontology:constraint_beneficiary(algorithmic_accountability_gap, platform_operators).
narrative_ontology:constraint_victim(algorithmic_accountability_gap, individuals_subject_to_algorithms).
narrative_ontology:constraint_victim(algorithmic_accountability_gap, affected_communities).
narrative_ontology:constraint_victim(algorithmic_accountability_gap, algorithmic_transparency_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGORITHM-SUBJECTED INDIVIDUAL (SNARE) — Individuals subject to algorithmic decisions (credit scoring, hiring filters, content curation, predictive policing) have no meaningful exit option. They cannot refuse participation in digital systems without severe material consequences. The algorithm's logic is opaque; outcomes cannot be appealed through transparent criteria. Maximum experienced extraction — powerless agents bear costs of algorithmic bias and opacity with no recourse.
constraint_indexing:constraint_classification(algorithmic_accountability_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AFFECTED COMMUNITY COALITION (TANGLED ROPE) — Communities disproportionately harmed by algorithmic bias (minorities in predictive policing, lower-income populations in lending algorithms) can organize and advocate for accountability. They have constrained exit options — they can demand regulation and transparency but cannot unilaterally opt out of systems. The constraint includes both coordination (shared interest in fair algorithms) and extraction (asymmetric burden of algorithmic harms). Organized power moderates experienced extraction but does not eliminate it.
constraint_indexing:constraint_classification(algorithmic_accountability_gap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Corporations deploying algorithms at scale experience the constraint as a coordination mechanism. They need to communicate algorithmic outcomes to users and regulators. Opacity creates legal liability and reputational risk. The accountability gap, from the corporate perspective, is a coordination problem solved through disclosure frameworks, audit mechanisms, and compliance theater. Net beneficiary — they can arbitrage between transparency demands and operational opacity.
constraint_indexing:constraint_classification(algorithmic_accountability_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY FRAMEWORK COALITION (SCAFFOLD) — EU AI Act, algorithmic impact assessments, and explainability mandates represent a sunset-clause approach: build transparency and accountability requirements that systematically phase out the opacity-based business model. The framework has a clear exit path — as regulations tighten, corporations must either comply (losing the opacity advantage) or exit jurisdictions (losing markets). Organized actors see this as a temporary coordination failure with structured resolution.
constraint_indexing:constraint_classification(algorithmic_accountability_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ALGORITHMIC EXPLAINABILITY THEATER (PITON) — Transparency reports, fairness audits, and explainability claims have become substantially performative. Corporations publish algorithmic accountability reports and conduct fairness reviews, but the core opacity persists: proprietary training data, model weights, and decision thresholds remain hidden. The accountability ritual (audits, reports, certifications) maintains the appearance of accountability while the extraction mechanism (opacity, unilateral deployment, absence of meaningful appeal) remains functional. Theater ratio high because the mechanism has atrophied — corporations maintain the ritual because it substitutes for actual transparency.
constraint_indexing:constraint_classification(algorithmic_accountability_gap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some opacity is inherent to machine learning: the decision logic of large neural networks is fundamentally opaque even to their creators (black-box functions). This perspective sees the accountability gap as an immutable property of current AI technology — a limit of mathematical tractability. However, the structural data contradicts the mountain classification: the opacity is partly technical (training complexity) and partly institutional (corporate secrecy choices). The engine will compute this as a false summit, revealing that 'technical inevitability' naturalizes what is partly a strategic choice.
constraint_indexing:constraint_classification(algorithmic_accountability_gap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_accountability_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_accountability_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_accountability_gap, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_accountability_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_accountability_gap, TR),
    TR >= 0.70.

:- end_tests(algorithmic_accountability_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Corporations extract significant value from opacity — competitive advantage, regulatory evasion, liability shield, behavioral lock-in through algorithmic design. The extractiveness is not as severe as a pure snare (0.72+) because alternative platforms exist and some transparency mechanisms function. However, the opacity is actively maintained and constitutes a clear extractive mechanism. Suppression (0.65): High. Barriers to exit and challenge include: (1) mandatory participation in digital systems (employment, financial access, government services); (2) opaque decision logic that prevents meaningful appeal; (3) proprietary data and models that prevent external verification; (4) asymmetric power — corporations define appeal terms; (5) publication bias — corporations suppress negative audits. Theater ratio (0.68): High and increasing. Transparency reports, fairness audits, algorithmic explainability claims have become substantially performative. Corporations publish accountability reports while maintaining core opacity. The ritual has increased from 0.35 to 0.68 over the interval as transparency demands have grown — corporations adopt theater to satisfy external pressure while preserving the extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The algorithm-deploying corporation sees a coordination problem (Rope) — they need to communicate performance and manage liability. The regulatory framework coalition sees a temporary coordination failure being systematically resolved (Scaffold) — sunset mechanisms are building transparency requirements. The explainability theater sees its own ritual as performative (Piton) — the corporation maintains the audit ritual because it substitutes for transparency. The affected community sees mixed coordination and extraction (Tangled Rope) — the system affects them asymmetrically but they can organize for change. The powerless individual sees pure extraction with no escape (Snare) — no exit, no understanding, no appeal. The analytical observer risks seeing technical inevitability (Mountain) — 'neural networks are black boxes' — but this naturalizes what is partly institutional choice. The gap is not perceptual confusion but genuine structural difference: the corporation and the individual occupy opposite positions in the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations directly determine directionality (d) and effective extraction (χ). Algorithm-deploying corporations are beneficiaries with arbitrage exit options (d ≈ 0.10, f(d) ≈ -0.05) — they experience the constraint as coordination at low effective extraction. Individuals subject to algorithms are victims with trapped exit options (d ≈ 0.95, f(d) ≈ 1.42) — they experience maximum effective extraction. Organized communities benefit from collective voice but remain constrained by structural barriers (d ≈ 0.60, f(d) ≈ 0.80) — moderate effective extraction. The regulatory coalition sees the constraint as temporary with sunset mechanisms (d ≈ 0.45, f(d) ≈ 0.50) — moderate extraction moderated by agency and visible exit paths. The scope modifier (global) amplifies extractiveness (σ(global) = 1.2) because algorithmic systems operate across jurisdictions, escape local regulation, and concentrate power at planetary scale.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: This constraint resolves the coordination-versus-extraction ambiguity through structural decomposition. The genuine coordination problem — communicating algorithm performance to stakeholders — is real and solvable. The tangled rope classification holds because the constraint simultaneously enables coordination (performance reporting, audit frameworks) and maintains asymmetric extraction (preserved opacity, unilateral deployment, limited appeals). The mandatrophy is resolved not by choosing between rope and snare but by recognizing both functions are present. The beneficiary's rope experience is their genuine perspective (communication and coordination). The victim's snare experience is equally genuine (opacity and entrapment). The scaffold perspective shows the structural resolution path: regulatory frameworks that systematically increase transparency requirements will reduce the extractive advantage until the constraint becomes pure coordination (Rope) or disappears entirely. The theater increase (0.35 to 0.68) is the diagnostic signal: as external pressure for accountability rises, corporations adopt more elaborate transparency performance to maintain the core extraction mechanism. When theater stabilizes at high levels (>0.70) and transparency mandates remain unenforced, the piton classification becomes appropriate — the accountability ritual becomes an inertial artifact of past enforcement, no longer functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_vs_strategic_opacity,
    'What portion of algorithmic opacity is due to fundamental technical limits of machine learning versus corporate strategic choice to preserve competitive advantage and avoid accountability?',
    'Comparative analysis of transparency in public algorithms (weather models, scientific computing) versus proprietary algorithms; longitudinal tracking of model interpretability improvements in published research vs deployed systems',
    'If opacity is primarily technical (>70%): mountain classification holds; accountability gap becomes generational-timescale problem. If opacity is primarily strategic (<40%): snare classification dominates; accountability gap is actively maintained and can be resolved through regulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_vs_strategic_opacity, empirical, 'Technical versus strategic roots of algorithmic opacity').

omega_variable(
    explainability_utility_paradox,
    'Do explainability mechanisms (LIME, SHAP, attention visualization) actually enable meaningful appeal and reversal of algorithmic decisions, or do they provide legitimacy theater while preserving unilateral corporate control?',
    'Audit of appeal success rates under explainability frameworks; user testing of whether explanations enable informed challenge of decisions; comparison of appeal outcomes before/after transparency mandates',
    'If explainability enables meaningful appeal: scaffold perspective confirmed, regulatory sunset is functional. If explainability is theater: piton classification holds, and transparency mandates are performing accountability rather than achieving it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(explainability_utility_paradox, empirical, 'Whether explainability mechanisms enable meaningful appeals').

omega_variable(
    algorithmic_bias_intersectionality,
    'Are algorithmic harms distributed equally across identity dimensions or do intersectional amplification effects create compound extraction for multiply-marginalized groups?',
    'Disaggregated analysis of algorithmic bias outcomes across intersectional categories; comparison of failure modes for single-axis versus multi-axis marginalization',
    'If harms are proportional: suppression value appropriate. If harms show amplification: suppression and extractiveness values both underestimate the constraint''s severity for affected communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_bias_intersectionality, empirical, 'Intersectional amplification of algorithmic harms').

omega_variable(
    regulatory_arbitrage_persistence,
    'As regulations tighten in some jurisdictions, will corporations migrate to less-regulated markets (regulatory arbitrage) or will global markets enforce accountability through competitive pressure?',
    'Longitudinal tracking of algorithm deployment patterns post-regulation; market analysis of whether stricter jurisdictions see reduced algorithmic deployment or merely geographic relocation',
    'If arbitrage is effective: scaffold sunset fails; constraint persists in weaker-regulation zones. If competitive pressure enforces global standards: scaffold perspective is structural and sunset is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_persistence, empirical, 'Whether regulatory arbitrage will undermine accountability frameworks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_accountability_gap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alg_acct_tr_t0, algorithmic_accountability_gap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(alg_acct_tr_t5, algorithmic_accountability_gap, theater_ratio, 5, 0.52).
narrative_ontology:measurement(alg_acct_tr_t10, algorithmic_accountability_gap, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(alg_acct_be_t0, algorithmic_accountability_gap, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(alg_acct_be_t5, algorithmic_accountability_gap, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(alg_acct_be_t10, algorithmic_accountability_gap, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_accountability_gap, enforcement_mechanism).
narrative_ontology:affects_constraint(algorithmic_accountability_gap, algorithmic_bias_discrimination).
narrative_ontology:affects_constraint(algorithmic_accountability_gap, corporate_regulatory_capture).
narrative_ontology:affects_constraint(algorithmic_accountability_gap, data_opacity_asymmetry).

% DUAL FORMULATION NOTE:
% The algorithmic accountability gap is upstream of specific discriminatory algorithm claims (hiring bias, lending discrimination) and represents a distinct structural constraint operating on the verification and appeal mechanisms. Downstream constraints inherit the opacity structure and must account for the baseline suppression and theater imposed by the accountability gap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_accountability_gap, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
