% ============================================================================
% CONSTRAINT STORY: sunk_cost_fallacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sunk_cost_fallacy, []).

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
 *   constraint_id: sunk_cost_fallacy
 *   human_readable: The Sunk Cost Fallacy
 *   domain: economic/social/cognitive
 *
 * SUMMARY:
 *   The sunk cost fallacy operates as a hybrid coordination-extraction
 *   constraint at the intersection of individual decision-making psychology,
 *   organizational incentive structures, and institutional accountability
 *   gaps. A decision-maker invests money, time, or effort into a project. As
 *   new information arrives suggesting the project is no longer optimal, the
 *   rational response is to ignore sunk costs and evaluate only future costs
 *   against future benefits. Instead, the fallacy creates a binding force
 *   that compels continued investment precisely because of past commitments.
 *   From different structural positions, this same mechanism appears as pure
 *   extraction (snare from the powerless individual's view), mixed
 *   coordination and extraction (tangled rope from organizational actors and
 *   analytical observers), institutional theater (piton from the degraded
 *   accountability system), or remediable temporary coordination failure
 *   (scaffold from behavioral economics reformers). The fallacy's strength
 *   has increased over the interval (ε from 0.28 to 0.58) as organizational
 *   complexity, distributed accountability, and vendor lock-in mechanisms
 *   have deepened. Theater ratio has risen (0.42 to 0.68) as organizations
 *   develop increasingly sophisticated retrospective justifications for
 *   sunk-cost-driven projects. The constraint exemplifies how indexical
 *   classification reveals the perspectival nature of institutional
 *   constraints: the same behavioral pattern has radically different
 *   structural meaning depending on whether you are the person trapped by it,
 *   the organization locked into it, the vendor benefiting from it, or the
 *   reformer trying to design alternatives.
 *
 * KEY AGENTS:
 *   - Individual decision-maker: Primary victim (powerless/trapped) — bears direct resource loss and opportunity cost; psychological pain prevents exit
 *   - Organizational actor: Secondary victim (moderate/constrained) — locked into sunk-cost-driven projects by accountability and prior commitments; also benefits from organizational continuity coordination
 *   - Vendor/consultant/contractor: Primary beneficiary (institutional/arbitrage) — profits from escalation commitment and client entrenchment; sees the fallacy as pure coordination mechanism
 *   - Behavioral economics reformer: Organized agent (organized/mobile) — designs decision-audit frameworks, pre-mortems, and sunk-cost-blind budgeting to create alternative pathways
 *   - Organizational accountability system: Institutional actor (institutional/constrained) — maintains theater of justification; the mechanism through which sunk cost commitment perpetuates
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional behavior as immutable cognitive law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sunk_cost_fallacy, 0.58).
domain_priors:suppression_score(sunk_cost_fallacy, 0.62).
domain_priors:theater_ratio(sunk_cost_fallacy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sunk_cost_fallacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(sunk_cost_fallacy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sunk_cost_fallacy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sunk_cost_fallacy, tangled_rope).
narrative_ontology:human_readable(sunk_cost_fallacy, "The Sunk Cost Fallacy").
narrative_ontology:topic_domain(sunk_cost_fallacy, "economic/social/cognitive").

domain_priors:requires_active_enforcement(sunk_cost_fallacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sunk_cost_fallacy, sunk_cost_extractors).
narrative_ontology:constraint_beneficiary(sunk_cost_fallacy, institutional_lock_in_beneficiaries).
narrative_ontology:constraint_victim(sunk_cost_fallacy, decision_maker_resource_loss).
narrative_ontology:constraint_victim(sunk_cost_fallacy, opportunity_cost_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMITTED INVESTOR (SNARE) — Individual decision-maker trapped by past commitment. Cannot exit without admitting loss. Faces continuous extraction: compounding further investment in sunk resources. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.64.
constraint_indexing:constraint_classification(sunk_cost_fallacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ORGANIZATIONAL ACTOR (TANGLED ROPE) — Benefits from coordination of past commitments (sunk cost justifies continued infrastructure investment); constrained by commitment to prior decisions. Both coordination (organizational continuity) and extraction (blocking better resource allocation). d≈0.68, f(d)≈1.04, σ=0.9 → χ≈0.54.
constraint_indexing:constraint_classification(sunk_cost_fallacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: VENDOR/ESCALATION BENEFICIARY (ROPE) — Institutional actors (vendors, contractors, consultants) benefit from escalation commitment. The sunk cost fallacy is pure coordination from their perspective: they solve the problem of getting clients to continue funding through psychological entrenchment rather than functional necessity. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(sunk_cost_fallacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CORPORATE LOCK-IN (PITON) — Large organizations maintain sunk-cost-driven projects through inertia. Theater ratio high (0.68): annual budget reviews, sunk-cost justifications, retrospective reframing all perform organizational commitment without functional evaluation. The fallacy is theatrical — the project persists because exit is psychologically painful, not because current costs are justified. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.33.
constraint_indexing:constraint_classification(sunk_cost_fallacy, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: BEHAVIORAL ECONOMICS REFORM (SCAFFOLD) — Organized agents (behavioral economists, organizational reformers, decision scientists) view the sunk cost fallacy as a temporary coordination failure with a sunset. Decision-audit frameworks, sunk-cost-blind budgeting, and pre-mortems are building alternative decision pathways that bypass psychological entrenchment. d≈0.35, f(d)≈0.28, σ=1.2 → χ≈0.10. Low effective extraction because mobile exit and external oversight create alternatives.
constraint_indexing:constraint_classification(sunk_cost_fallacy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COGNITIVE SCIENCE / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, sunk cost bias is presented as an immutable feature of human cognition and loss-aversion heuristics. The framing naturalizes what is actually a contingent institutional arrangement (accountability structures, decision-making norms, resource control mechanisms). Falsifiable: if the bias is truly a law of cognition, it should persist across all decision contexts and incentive structures. Counter-evidence: lab-controlled conditions with clear feedback loops largely eliminate the bias. Thus this perspective commits a false natural law error.
constraint_indexing:constraint_classification(sunk_cost_fallacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: INTEGRATED ANALYTICAL VIEW (TANGLED ROPE) — Synthesizing all perspectives: the sunk cost fallacy is a hybrid constraint. It provides genuine coordination (organizational continuity, past commitments honored) AND extractive mechanisms (psychological entrenchment blocking rational resource allocation, vendor capture through escalation, institutional lock-in). Suppression is real (emotional pain of admitting loss, career risk of reversing decisions). Theater is significant (ritual justifications). ε=0.58 reflects the mixed nature: not pure extraction (vendors do solve coordination problems), not pure coordination (the fallacy causes massive resource misallocation). χ is medium-high (0.68) across perspectives, with variation by structural position.
constraint_indexing:constraint_classification(sunk_cost_fallacy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sunk_cost_fallacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sunk_cost_fallacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sunk_cost_fallacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sunk_cost_fallacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sunk_cost_fallacy, TR),
    TR >= 0.70.

:- end_tests(sunk_cost_fallacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The fallacy extracts resources through psychological entrenchment and institutional lock-in, but not maximally because (a) some continued investments are rationally justified (option value recovery, commitment-honor incentives), (b) debiasing mechanisms exist and occasionally work, and (c) exit is possible with sufficient institutional will. The value reflects the measurable resource misallocation (studies suggest 20-30% of corporate projects continue past rational exit points, representing real opportunity cost) tempered by the fact that not all sunk-cost-driven decisions are wrong. Suppression (0.62): Moderate-high. Significant barriers to rational exit: emotional loss aversion (neurobiological), reputational costs of reversing decisions (social), organizational accountability norms (institutional), contractual obligations (legal), and vendor lock-in (structural). These create a suppression vector that makes exit psychologically, socially, and institutionally costly. Theater ratio (0.68): High and increasing. Organizational justifications for sunk-cost-driven projects are increasingly theatrical: annual reviews invoke 'completing the commitment,' retrospective reframings highlight sunk investments as reasons for continued funding, and decision meetings perform the deliberation without genuine exit option consideration. The theater has risen over time as organizations have developed more sophisticated legitimation narratives. This elevated theater is a signal that the underlying coordination function (honoring commitments, organizational continuity) is being performed primarily for its optics rather than its actual value.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates dramatic perspectival divergence. The individual decision-maker (powerless/trapped) sees pure extraction: they bear all costs and gain no benefit from continued investment; the fallacy is a snare that traps them. The vendor/consultant (institutional/arbitrage) sees pure coordination: escalation commitment is how they solve the problem of client resource continuation; from their view it is rope. The organizational actor (moderate/constrained) sees tangled rope: they benefit from some commitment-honoring norms but are locked into suboptimal resource allocation. The behavioral economics reformer (organized/mobile) sees a temporary problem with a sunset: pre-mortems, sunk-cost-blind budgets, and decision audits create alternative pathways that are gradually replacing the fallacy as the primary coordination mechanism. The accountability system (institutional/constrained) sees piton: their review rituals are increasingly performative theater, maintained through inertia rather than functional necessity. The civilizational analytical observer risks seeing mountain (immutable loss-aversion law of cognition), but this commits a false summit error — controlled laboratory studies show the bias largely disappears with clear feedback and no accountability pressure, revealing it as contingent on institutional context rather than a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual decision-maker: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximal extraction because no exit options and no benefit. Organizational actor: Victim/beneficiary split + constrained → d≈0.68, f(d)≈1.04. Mixed because commitment-honoring is partially beneficial (coordination) but resource trap is extractive (locked into suboptimal allocation). Vendor/beneficiary: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Negative effective extraction; net beneficiary. Reformer: Organized + mobile → d≈0.35, f(d)≈0.28. Low effective extraction because the organized coalition can exit (alternative frameworks available) and has agency to design substitutes. Accountability system: Institutional + constrained (captured by fallacy): d≈0.45, f(d)≈0.48. Piton classification comes from theater gate (≥0.70), not from high extraction. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain perspective commits false summit; the structural data (ε=0.58, suppression=0.62, theater=0.68) contradicts the mountain gate thresholds (ε≤0.25, suppression≤0.05).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The sunk cost fallacy resolves the mandatrophy by showing that the constraint is genuinely hybrid, not mislabeled extraction-as-coordination or coordination-as-extraction. The beneficiary (vendor) experiences pure coordination: escalation commitment solves their real problem of maintaining client funding through psychological entrenchment rather than functional necessity. The victim (individual decision-maker) experiences pure extraction: continued investment extracts resources with no offsetting benefit to them. The organizational actor (moderate/constrained) experiences the hybrid: the commitment norm IS a coordination mechanism (it enables organizational continuity and honors past decisions, which have real value), BUT it is also extractive (it blocks optimal reallocation when conditions change). The tangled_rope classification is justified: the constraint has both genuine coordination (organizational continuity, commitment honoring) AND asymmetric extraction (psychological entrenchment, opportunity cost borne by trapped agents). Suppression is real (emotional loss aversion, reputational costs, institutional inertia). No single perspective sees it as pure coordination or pure extraction in aggregate — the classification depends entirely on structural position relative to the constraint. The analytical observer's Mountain perspective is marked as a false summit: loss-aversion framing naturalizes what is actually a contingent institutional arrangement. When feedback is clear, accountability is transparent, and exit costs are low, the bias largely disappears, proving it is not an immutable law of cognition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_vs_structural_attribution,
    'Is the sunk cost fallacy primarily a cognitive bias (immutable loss-aversion heuristic) or a structural consequence of institutional incentive misalignment and accountability gaps?',
    'Comparative analysis across decision contexts: laboratory settings with clear feedback vs. organizational settings with diffused accountability. Cross-cultural data: societies with different loss-aversion norms vs. Western individualist baseline. Intervention studies: whether structural (pre-mortems, sunk-cost-blind budgets) or cognitive (debiasing training) interventions are more effective.',
    'If cognitive: constraint approaches immutability (Mountain-like). Classification from powerless perspective becomes snare-locked. If structural: constraint is remediable (Scaffold logic applies). Classification becomes more plastic across contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_vs_structural_attribution, empirical, 'Whether sunk cost bias is cognitive law or structural phenomenon').

omega_variable(
    escalation_commitment_coupling,
    'How tightly coupled is the sunk cost fallacy to escalation of commitment mechanisms (vendor lock-in, reputational bonds, contractual continuity)? Can the fallacy be isolated from these institutional mechanisms?',
    'Isolation experiments: decision-makers with clear exit options (no vendor lock-in, anonymized decisions, no reputation stakes) vs. standard organizational settings. Vendor-free scenarios: DIY projects, pure internal resource allocation. Controlled reversibility: contexts where decisions are explicitly reversible.',
    'If tightly coupled: the fallacy is inseparable from institutional extraction. Tangled Rope classification is stable. If decoupled: cognitive bias and institutional lock-in are separate constraints requiring separate stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escalation_commitment_coupling, empirical, 'Coupling between sunk cost bias and escalation commitment mechanisms').

omega_variable(
    debiasing_ceiling_effect,
    'Why do standard debiasing interventions (awareness training, explicit sunk-cost-irrelevance instructions, decision audits) show large effect sizes in laboratory settings but minimal real-world impact on organizational behavior?',
    'Long-term follow-up of organizations implementing debiasing programs. Comparison of pre/post sunk-cost-driven project continuation rates. Correlation between debiasing training and actual resource reallocation decisions. Mechanism analysis: whether failure is attention (people forget the training), motivation (incentives override knowledge), or structural (institutional pressure overcomes individual decision autonomy).',
    'If attention/motivation failure: problem is institutional enforceability, not constraint structure. Suggests Piton perspective is accurate (theater without function). If structural pressure: suggests Snare classification from organizational actor''s perspective — they know better but are trapped by accountability mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debiasing_ceiling_effect, empirical, 'Why debiasing interventions fail at scale in organizations').

omega_variable(
    beneficial_commitment_vs_extraction,
    'In what percentage of sunk cost scenarios is continued investment actually optimal (recovering partial value, honoring commitments, maintaining option value) vs. clearly suboptimal (pure extraction masked by fallacy)?',
    'Longitudinal analysis of sunk-cost-driven decisions: outcome audits comparing ''continued investment'' vs. ''exit'' decisions. Bayesian analysis of information available at decision point (was continued investment ex-ante rational given asymmetric information?) vs. outcome analysis (did it ex-post make sense?). Decomposition by domain: financial losses vs. time/effort investments have different recovery trajectories.',
    'If high percentage beneficial: constraint provides real coordination function. Rope/Tangled Rope classification is justified. If low percentage beneficial: constraint is primarily extractive. Snare classification becomes dominant. Impacts all policy recommendations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficial_commitment_vs_extraction, empirical, 'Fraction of sunk cost commitments that are rationally justified vs. purely extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sunk_cost_fallacy, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sunk_tr_t0, sunk_cost_fallacy, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sunk_tr_t15, sunk_cost_fallacy, theater_ratio, 15, 0.58).
narrative_ontology:measurement(sunk_tr_t30, sunk_cost_fallacy, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(sunk_be_t0, sunk_cost_fallacy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sunk_be_t15, sunk_cost_fallacy, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(sunk_be_t30, sunk_cost_fallacy, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sunk_cost_fallacy, resource_allocation).
narrative_ontology:affects_constraint(sunk_cost_fallacy, escalation_of_commitment).
narrative_ontology:affects_constraint(sunk_cost_fallacy, organizational_inertia).
narrative_ontology:affects_constraint(sunk_cost_fallacy, loss_aversion_heuristic).

% DUAL FORMULATION NOTE:
% The sunk cost fallacy is downstream of loss-aversion cognitive heuristics but represents a distinct structural constraint operating at the organizational and institutional level. The upstream cognitive constraint (loss aversion) has different ε and suppression properties; the sunk cost fallacy's ε=0.58 reflects the institutional mechanisms that amplify and perpetuate loss-aversion-driven decision patterns. Separate constraint stories address: (1) loss aversion as a cognitive bias, (2) escalation of commitment as an institutional mechanism, and (3) sunk cost fallacy as their conjunction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sunk_cost_fallacy, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
