% ============================================================================
% CONSTRAINT STORY: shobies_existential_commitment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shobies_existential_commitment, []).

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
 *   constraint_id: shobies_existential_commitment
 *   human_readable: The Risk-Safety Paradox in Existential Commitment
 *   domain: social/existential
 *
 * SUMMARY:
 *   The Risk-Safety Paradox arises from the structural tension between the
 *   institutional imperative to minimize existential risk through coordinated
 *   protective measures and the existential requirement for individual
 *   agency, genuine risk-taking, and exploratory consciousness. As
 *   articulated in Ursula K. Le Guin's 'The Ones Who Walk Away from Omelas,'
 *   the constraint manifests as a civilization-wide enforcement mechanism
 *   that protects collective survival at the cost of suppressing the
 *   individual exploration and risk-taking necessary for meaningful
 *   existence. The paradox deepens as protection mechanisms (precautionary
 *   institutional design, suppression of high-variance futures, enforcement
 *   of safety compliance) become increasingly performative while still
 *   bearing the full suppression costs. The constraint exhibits snare
 *   characteristics from powerless and moderate perspectives (individuals and
 *   parents are trapped), rope characteristics from the institutional
 *   beneficiary perspective (legitimate coordination), mixed rope-snare
 *   characteristics from organized researchers (both genuine coordination
 *   function and institutional suppression), and piton characteristics from
 *   the institutional culture perspective (performative safety theater). The
 *   analytical observer risks mistaking this contingent institutional
 *   configuration for a law of nature.
 *
 * KEY AGENTS:
 *   - Individual Existential Seeker: Primary victim (powerless/trapped) — bears the core paradox: choosing safety means existential emptiness; choosing meaning requires transgressing safety enforcement
 *   - Parent: Secondary victim (moderate/constrained) — caught transmitting protection duty to children while witnessing its existential costs
 *   - Collective Risk Management System: Primary beneficiary (institutional/arbitrage) — captures survival advantage through synchronized protective behavior; experiences constraint as necessary coordination
 *   - Existential Risk Researchers: Organized agent (organized/constrained) — see both coordination function and institutional suppression; constrained by enforcement of precautionary principles
 *   - Safety-First Institutional Culture: Secondary beneficiary (institutional/arbitrage) — maintains institutional legitimacy through safety theater; experiences constraint as partly degraded (piton)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choice as law of nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shobies_existential_commitment, 0.62).
domain_priors:suppression_score(shobies_existential_commitment, 0.68).
domain_priors:theater_ratio(shobies_existential_commitment, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shobies_existential_commitment, extractiveness, 0.62).
narrative_ontology:constraint_metric(shobies_existential_commitment, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(shobies_existential_commitment, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shobies_existential_commitment, snare).
narrative_ontology:human_readable(shobies_existential_commitment, "The Risk-Safety Paradox in Existential Commitment").
narrative_ontology:topic_domain(shobies_existential_commitment, "social/existential").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shobies_existential_commitment, collective_survival_apparatus).
narrative_ontology:constraint_beneficiary(shobies_existential_commitment, risk_averse_institutions).
narrative_ontology:constraint_victim(shobies_existential_commitment, individual_existential_agency).
narrative_ontology:constraint_victim(shobies_existential_commitment, genuine_risk_taking).
narrative_ontology:constraint_victim(shobies_existential_commitment, exploratory_consciousness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL EXISTENTIAL SEEKER (SNARE) — Trapped between the demand for safety and the impossibility of meaningful existence without risk. Cannot exit the civilization-wide enforcement of protective measures. Bears full cost of the paradox: choosing safety means existential emptiness; choosing meaning requires transgressing safety enforcement. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.87.
constraint_indexing:constraint_classification(shobies_existential_commitment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: PARENT (SNARE) — Caught in the enforcement mechanism. Must transmit protective behaviors to children while witnessing that this transmission constrains their existential exploration. Constrained exit: can abandon protection duty but faces social and psychological penalty. d≈0.88, f(d)≈1.35, σ=1.0 → χ≈0.84.
constraint_indexing:constraint_classification(shobies_existential_commitment, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: COLLECTIVE RISK APPARATUS (ROPE) — Institutional beneficiary. Experiences the constraint as necessary coordination: reducing existential risk requires synchronized protective behavior across populations. Can arbitrage out via policy authority and institutional legitimacy. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(shobies_existential_commitment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EXISTENTIAL RISK RESEARCHERS (TANGLED ROPE) — Organized but constrained. See both coordination function (understanding existential risks requires studying worst cases) and extraction mechanism (institutional pressure to recommend maximum caution suppresses exploration of high-risk research pathways). Active enforcement of precautionary principles limits research design freedom. d≈0.62, f(d)≈0.92, σ=1.0 → χ≈0.57.
constraint_indexing:constraint_classification(shobies_existential_commitment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SAFETY-FIRST INSTITUTIONAL CULTURE (PITON) — Theater ratio=0.58 indicates performative safety measures that have partly decoupled from actual risk reduction. Many protective institutions maintain elaborate safety rituals (compliance training, risk assessment committees, precautionary reviews) that persist through inertia despite diminishing functional returns. theater_ratio=0.58 crosses the piton gate (≥0.70 threshold not met, but rising). This perspective experiences the constraint as degraded coordination, where institutional safety theater persists despite atrophied connection to actual harm prevention.
constraint_indexing:constraint_classification(shobies_existential_commitment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN CANDIDATE) — Risk of false summit: the civilizational/universal view tempts naturalization of the paradox as an irreducible law of existence. 'You cannot have safety and meaning simultaneously' sounds like a law of nature. However, base metrics (ε=0.62, suppression=0.68, theater=0.58) contradict mountain classification (ε must be ≤0.25 for mountain). The constraint is not a law of nature but a contingent institutional configuration that has been mistaken for one. The false summit detector flags this perspective as mis-classification of social artifact as natural law.
constraint_indexing:constraint_classification(shobies_existential_commitment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shobies_existential_commitment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shobies_existential_commitment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shobies_existential_commitment, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shobies_existential_commitment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(shobies_existential_commitment, TR),
    TR >= 0.70.

:- end_tests(shobies_existential_commitment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High and rising. The constraint extracts individual agency and exploratory consciousness under the justified rationale of existential risk reduction. However, the extraction is not total suppression—individuals retain constrained agency within safety parameters. The metric rises over the interval (0.35→0.62) reflecting institutional expansion of protective mechanisms and their scope. Suppression (0.68): High. Significant institutional barriers to individual risk-taking: external validation requirements, precautionary policy enforcement, social sanction of transgression, institutional architecture designed to prevent high-variance futures. Alternatives are actively suppressed, not merely unavailable. Theater (0.58): Moderate and rising. The constraint exhibits growing performative content: safety compliance training, risk assessment theater, precautionary reviews that maintain institutional legitimacy without proportional risk reduction. Theater rises as institutional mechanisms become further removed from actual threat reduction. The theater rise (0.28→0.58) over the interval suggests the constraint is degrading toward piton status.
 *
 * PERSPECTIVAL GAP:
 *   The individual seeker and parent perspectives see maximum snare extraction—they are trapped by institutional enforcement and bear the paradox without exit. The collective risk apparatus sees legitimate rope coordination—synchronized protective behavior is necessary and benefits all. The existential risk researchers see tangled rope—their work benefits from institutional coordination around risk, but they are also suppressed by institutional enforcement of precaution. The institutional safety culture sees a partly degraded system (piton perspective)—safety theater persists through inertia, and the institution witnesses its own atrophying connection to actual harm prevention. The analytical observer risks mountain classification (natural law) but the structural data reveals this as false summit. The perspectival gap is maximum: beneficiary and victims experience fundamentally incompatible realities from the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual existential seeker: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; individual cannot exit protective enforcement. Parent: Victim + constrained → d≈0.88, f(d)≈1.35. High extraction; parent faces social penalty for abandoning protection duty. Collective risk apparatus: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary via institutional authority and policy arbitrage. Existential risk researchers: Organized + constrained → d≈0.62, f(d)≈0.92. Significant extraction through suppression of research pathways, but also benefits from organizational coordination around existential questions. Safety-first culture: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Institutional beneficiary via legitimacy maintenance; piton classification emerges from theater gate, not from high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit: naturalizes institutional choice as law.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED through perspectival decomposition. The constraint could be mislabeled as either pure coordination (Rope: 'we all benefit from synchronized risk reduction') or pure extraction (Snare: 'protective institutions suppress individual agency'). Mandatrophy resolution requires recognizing that both claims are true from different structural positions. The collective risk apparatus genuinely solves a coordination problem—uncoordinated risk-taking could trigger existential catastrophe. But this coordination function is purchased through snare extraction from individuals—their exploratory agency and existential meaning-making are suppressed. The constraint is neither pure coordination nor pure extraction but an institutional configuration that conflates legitimate risk management with suppression of existential agency. The rising theater ratio (0.28→0.58) indicates the constraint is degrading: institutional legitimacy increasingly depends on performative safety theater rather than actual risk reduction. This suggests the constraint is evolving toward piton status—the coordination function is atrophying while suppression persists. The mandatrophy is resolved by showing that apparent coordination (risk reduction) is institutionally inseparable from snare extraction (agency suppression), making genuine disentanglement necessary for ethical clarity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    risk_threshold_calibration,
    'At what level of actual existential risk does protective enforcement transition from coordination function to net extraction?',
    'Empirical comparison: societies with different risk assessments (low-risk vs high-risk evaluations) and their institutional protection mechanisms; correlation between assessed risk level and suppression intensity',
    'If threshold very high (extreme risk required): current protections represent over-suppression across most contexts, making the constraint a snare. If threshold low: current protections are justified coordination, downgrading the snare to rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(risk_threshold_calibration, empirical, 'Calibration of protective enforcement to actual existential risk level').

omega_variable(
    meaningful_existence_definition,
    'Is the paradox a structural fact (you genuinely cannot have both safety and existential meaning) or a contingent institutional configuration (current institutional arrangements force the choice)?',
    'Historical analysis of cultures/periods with different risk-acceptance norms; investigation of whether lower-suppression societies show different meaning-generation outcomes; philosophical analysis of whether meaning requires ontological risk or only perceived risk',
    'If structural: snare is actually unavoidable mountain-like constraint. If contingent: the snare is a pure extraction mechanism that could be dismantled without loss of actual safety.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaningful_existence_definition, conceptual, 'Whether the risk-safety paradox is structural or institutional').

omega_variable(
    collective_action_necessity,
    'Does existential risk actually require coordinated global suppression of individual risk-taking, or could it be managed through distributed exploration with local accountability?',
    'Game-theoretic analysis of risk coordination; case studies of decentralized vs centralized risk management; examination of whether some existential risks (e.g., AGI) genuinely require suppression vs could be managed via parallel exploration',
    'If coordination necessary: constraint is justified Rope or Tangled Rope. If decentralized alternatives exist: constraint is revealed as unnecessary Snare extracting individual agency under false coordination rationale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_necessity, empirical, 'Whether existential risk requires global coordination vs distributed management').

omega_variable(
    consciousness_expression_substrates,
    'Are the specific institutional forms of safety enforcement (precaution, transparency, external validation) the only mechanisms for managing existential risk, or are alternative institutional substrates possible that reduce suppression without increasing risk?',
    'Examination of alternative institutional models (e.g., trusted-person networks, decentralized peer review, voluntary transparency); pilot studies of lower-suppression risk management frameworks; analysis of which aspects of current suppression target actual risk vs institutional predictability',
    'If alternatives viable: current snare is unnecessary, opening path toward lower-suppression scaffold. If current mechanisms are optimal: snare classification is justified but unsolvable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consciousness_expression_substrates, conceptual, 'Whether alternative institutional substrates can manage existential risk with lower suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shobies_existential_commitment, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shoby_tr_t0, shobies_existential_commitment, theater_ratio, 0, 0.28).
narrative_ontology:measurement(shoby_tr_t50, shobies_existential_commitment, theater_ratio, 50, 0.42).
narrative_ontology:measurement(shoby_tr_t100, shobies_existential_commitment, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(shoby_be_t0, shobies_existential_commitment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(shoby_be_t50, shobies_existential_commitment, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(shoby_be_t100, shobies_existential_commitment, base_extractiveness, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shobies_existential_commitment, enforcement_mechanism).
narrative_ontology:affects_constraint(shobies_existential_commitment, institutional_precaution_theater).
narrative_ontology:affects_constraint(shobies_existential_commitment, existential_autonomy_suppression).

% DUAL FORMULATION NOTE:
% The Risk-Safety Paradox decomposes into two related constraints: (1) existential risk coordination (legitimate collective action problem requiring synchronized protective behavior) and (2) institutional suppression of exploratory consciousness (snare extraction mechanism hidden within risk management apparatus). This story captures their entanglement. The upstream constraint is the empirical fact of existential risk; the downstream constraints capture how institutions respond to that risk through both coordination and extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
