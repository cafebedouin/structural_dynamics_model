% ============================================================================
% CONSTRAINT STORY: inner_models
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_inner_models, []).

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
 *   constraint_id: inner_models
 *   human_readable: Confirmation Bias in Inner Model Updating
 *   domain: social/cognitive
 *
 * SUMMARY:
 *   Confirmation bias in inner model updating represents a structural
 *   constraint that operates at the intersection of cognitive necessity and
 *   social pathology. The same mechanism — the tendency to weight information
 *   confirming existing beliefs more heavily than disconfirming information —
 *   appears as an immutable feature of bounded rationality (Mountain), a
 *   coordination mechanism that binds communities (Rope), a mixed function
 *   generating both group coherence and adaptive rigidity (Tangled Rope),
 *   pure extraction trapping individuals in false models (Snare), a temporary
 *   problem being solved by epistemic reform (Scaffold), and a performative
 *   naturalization of contingent institutional patterns (Piton). The
 *   constraint's extractiveness has risen from 0.35 to 0.52 over the
 *   interval, reflecting accumulating institutional capture: as institutions
 *   learn to leverage confirmation bias for behavioral control and belief
 *   management, the extractive component grows while the functional
 *   coordination component remains constant. Theater ratio has risen from
 *   0.40 to 0.61, indicating that popular framing of confirmation bias
 *   (neuroscience legitimation, evolutionary psychology narratives) carries
 *   increasing performative content — much contemporary discussion of
 *   confirmation bias is metacognitive theater that feels like addressing the
 *   problem without reducing the underlying mechanism.
 *
 * KEY AGENTS:
 *   - Individual Belief Holder: Primary victim (powerless/trapped) — bears cost of rigid models; cannot exit without cognitive effort
 *   - Tribal Community: Secondary beneficiary (moderate/constrained) — benefits from belief consistency; bears cost of maladaptation; experiences tangled rope
 *   - Institutional Stakeholder (government, corporation, ideology): Primary beneficiary (institutional/arbitrage) — leverages population confirmation bias for behavioral predictability and control; experiences rope coordination
 *   - Epistemic Reform Coalition: Organized agents (organized/constrained) — science educators, fact-checkers, transparency advocates building alternative belief-update mechanisms; see sunset logic
 *   - Neuroscience/Psychology Establishment: Institutional actor (institutional/arbitrage) — legitimizes confirmation bias through naturalization framing; maintains piton by providing authority to 'immutable brain' narrative
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — distinguishes mathematical necessity from social enforcement; risks false mountain classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(inner_models, 0.52).
domain_priors:suppression_score(inner_models, 0.68).
domain_priors:theater_ratio(inner_models, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(inner_models, extractiveness, 0.52).
narrative_ontology:constraint_metric(inner_models, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(inner_models, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(inner_models, tangled_rope).
narrative_ontology:human_readable(inner_models, "Confirmation Bias in Inner Model Updating").
narrative_ontology:topic_domain(inner_models, "social/cognitive").

domain_priors:requires_active_enforcement(inner_models).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(inner_models, belief_holders).
narrative_ontology:constraint_beneficiary(inner_models, tribal_affiliates).
narrative_ontology:constraint_victim(inner_models, epistemic_accuracy).
narrative_ontology:constraint_victim(inner_models, adaptive_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED INDIVIDUAL (SNARE) — Cannot exit confirmation bias without explicit cognitive intervention. The constraint extracts adaptive capacity and epistemic accuracy from those whose beliefs become rigid. Maximum experienced extraction — no external escape route, only internal cognitive work.
constraint_indexing:constraint_classification(inner_models, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMMUNITY WITH SHARED PRIORS (TANGLED ROPE) — Confirmation bias serves coordination function (shared reality-tunnel binds group identity) but simultaneously extracts adaptability cost (group cannot update rapidly when environment shifts). Benefits from in-group coherence; bears cost of maladaptation.
constraint_indexing:constraint_classification(inner_models, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL BENEFICIARY (ROPE) — Governments, corporations, and ideological movements benefit from populations maintaining stable beliefs. Confirmation bias serves as low-cost coordination mechanism: consistent worldviews reduce negotiation costs, enable synchronized action, predict behavior. Experienced extractiveness is negative or minimal — institutional actors see the constraint primarily as beneficial coordination.
constraint_indexing:constraint_classification(inner_models, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EPISTEMIC REFORM COALITION (SCAFFOLD) — Organized agents (science communicators, epistemic institutions, fact-checking initiatives, education reformers) see confirmation bias as a temporary coordination failure with a sunset: transparency mechanisms, metacognitive training, and epistemic commons are building alternative belief-update pathways. Theater is high (much of reform is performative messaging) but coalition sees genuine exit path.
constraint_indexing:constraint_classification(inner_models, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NATURALIST FRAMING (PITON disguised as MOUNTAIN) — Neuroscience and evolutionary psychology frames confirmation bias as an immutable feature of human cognition: pattern-matching brains must use priors; confirmation bias is just prior-weighted updating, universal across humans, unchangeable. This naturalizes a contingent behavioral pattern as law. Theater is high (neuroscience framing provides authority) and functional content is degraded — the claim is maintained through institutional authority rather than empirical demonstration of invariance.
constraint_indexing:constraint_classification(inner_models, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL / BAYESIAN NECESSITY (MOUNTAIN) — From a formal statistical perspective, any finite agent with finite computational resources must use priors to update beliefs. Confirmation bias is the inevitable consequence of resource-bounded rationality: perfect Bayesian updating is computationally intractable, so approximations must use available priors. This is a mathematical limit, not a contingent social constraint. However, the distinction between mathematical inevitability and social pathology is precisely the mandatrophy at issue.
constraint_indexing:constraint_classification(inner_models, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(inner_models_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(inner_models, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(inner_models, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(inner_models, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(inner_models, TR),
    TR >= 0.70.

:- end_tests(inner_models_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over interval. Base value reflects that confirmation bias extracts cognitive resources (time, emotional energy, opportunity cost of missed information) from individuals and adaptive capacity from communities. The rising trend reflects institutional capture: governments, corporations, and ideological movements increasingly engineer environments that amplify confirmation bias (algorithmic feeds, tribal information silos, institutional loyalty incentives). However, extractiveness is not extreme (not 0.70+) because some confirmation bias is functionally necessary for bounded-rational agents. Suppression (0.68): High. Barriers to escaping confirmation bias include: cognitive resource constraints (perfect Bayesian updating is intractable), emotional attachments to existing models (identity sunk costs), institutional penalties for public belief changes (status/reputation costs), and the invisibility of the mechanism itself (bias blind spot). Theater ratio (0.61): Moderate-high and rising. Contemporary discourse on confirmation bias (popular neuroscience, TED talks on cognitive biases, workplace bias training) is substantially performative: awareness campaigns often reinforce rather than reduce bias, and neuroscience framing ('your brain is hardwired this way') paradoxically naturalizes what might be socially engineered away.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. The trapped individual sees pure extraction (Snare) — their beliefs become rigid, they miss important information, they suffer from maladaptation. The tribal community sees both benefit (Rope coordination through shared worldview) and cost (Tangled Rope inability to adapt). The institutional beneficiary sees pure coordination (Rope) — confirmation bias in the population provides predictable behavior and reduces negotiation costs at almost no institutional expense. The epistemic reform coalition sees a temporary problem (Scaffold) — transparency mechanisms, epistemic commons, and metacognitive training are building alternative pathways with genuine sunset logic (20-50 year timeline for cultural epistemic norms to shift). The neuroscience establishment sees an immutable natural law (Piton disguised as Mountain) — 'your brain is built this way' naturalizes what is actually contingent. The analytical observer must distinguish between Bayesian necessity (true mountain: resource-bounded agents must use priors) and social pathology (institutional capture of bias for control). This is the core mandatrophy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position relative to belief-update extraction. Individuals who cannot update beliefs experience high d (close to 1.0, full targets) because they bear costs and have no exit. Communities with shared beliefs experience moderate d (around 0.55) because they benefit from coordination but suffer from maladaptation. Institutional beneficiaries experience very low d (around 0.10-0.15) because they extract predictability and control with minimal cost. The epistemic reform coalition, positioned to build alternative mechanisms, experiences moderate d (0.40-0.50) because they have some agency but face institutional resistance. The derived f(d) values follow the sigmoid: trapped individuals experience maximum effective extractiveness; beneficiaries experience minimal or negative effective extraction; moderate agents experience balanced or slightly asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   CORE MANDATROPHY: Is confirmation bias a mathematical limit (unavoidable consequence of resource-bounded rationality) or a social extraction mechanism (institutional capture of cognitive necessity for behavioral control)? The mountain perspective claims the former: any finite agent with finite computational resources must use priors; confirmation bias is inevitable. But this conflates two distinct phenomena: (1) prior-weighted updating (mathematical necessity for bounded rationality) and (2) asymmetric information processing that favors confirming data (social pathology amplified by institutional structures). Mathematical necessity means suppression = 1.0 (inescapable) and theater = 0.0 (no performance needed). But empirical suppression = 0.68 and theater = 0.61, indicating the constraint is socially enforced, not mathematically inevitable. The mandatrophy resolves by decomposing: (A) resource-bounded Bayesian updating (true mountain), (B) institutional amplification of confirmation bias via information architecture (tangled rope), (C) naturalization of (B) via neuroscience authority (piton). Three separate constraints, not one. The piton perspective is high theater precisely because it conflates (A) with (B)-(C), claiming social pathology is mathematical necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mathematical_vs_social_confirmation,
    'Is confirmation bias a mathematical necessity (Bayesian approximation under resource constraints) or a social pathology (maladaptive belief rigidity that could be engineered away)?',
    'Comparative cognitive science across populations with different epistemic training; analysis of whether high-transparency or metacognitive-training interventions reduce confirmation bias below resource-bounded baseline; investigation of whether Bayesian approximation algorithms actually exhibit ''confirmation bias'' in formal sense or whether the phenomenon is distinctly social',
    'If mathematical: Mountain classification is correct; interventions target symptoms not cause; suppression is high because the constraint is intrinsic. If social: Snare/Tangled Rope classifications are correct; suppression reflects institutional enforcement, not inevitability; sunset is possible through epistemic reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mathematical_vs_social_confirmation, conceptual, 'Whether confirmation bias is mathematical necessity or social pathology').

omega_variable(
    institutional_vs_cognitive_locus,
    'Does confirmation bias primarily reside in individual cognitive architecture or in institutional structures that reward belief consistency and penalize updating?',
    'Empirical comparison: test confirmation bias in individuals operating in high-transparency, low-institutional-cost environments vs. those in low-transparency, high-cost environments; measure belief-update rates in epistemic commons (Reddit science, peer review, collaborative coding) vs. tribal/institutional contexts; analyze whether ''confirmation bias'' disappears when institutional incentives flip',
    'If cognitive: suppression and extractiveness reflect individual neural limits; interventions must target metacognition. If institutional: suppression reflects structural penalties for updating; extractiveness derives from those who benefit from rigid beliefs; the constraint is a Tangled Rope, not a Mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_vs_cognitive_locus, empirical, 'Locus of confirmation bias in cognitive vs. institutional structures').

omega_variable(
    transparency_sufficiency,
    'Do transparency mechanisms (showing data sources, recording reasoning, public reasoning trails) actually reduce confirmation bias or merely relocate it to higher levels of abstraction (meta-confirmation bias)?',
    'A/B testing of belief-updating with and without transparency in controlled environments; longitudinal tracking of transparency initiatives (science Twitter, open science, transparent AI) vs. opacity-baseline groups; analysis of whether transparency users update more readily or instead become more sophisticated at rationalization',
    'If transparency sufficient: Scaffold sunset logic holds; epistemic reform creates genuine exit path. If meta-confirmation persists: transparency is performative theater; suppressiveness remains high; belief rigidity shifts to higher abstraction levels.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transparency_sufficiency, empirical, 'Whether transparency mechanisms reduce or relocate confirmation bias').

omega_variable(
    adaptive_vs_maladaptive_bias,
    'Under what environmental conditions is belief consistency (confirmation bias) adaptive vs. maladaptive? Does the constraint extract value from slow-changing vs. rapidly-changing domains?',
    'Ecological analysis: compare confirmation bias costs in stable environments (physics, mathematics, geology) vs. chaotic environments (politics, finance, technology); measure belief-update lag relative to environmental change rate; correlate constraint extractiveness with environmental volatility',
    'If bias adaptive in some domains: Mountain perspective is domain-relative (mathematical limit in stable domains, social pathology in chaotic domains); constraint decomposition needed. If universally maladaptive: extractiveness is purely social; suppression reflects institutional enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_vs_maladaptive_bias, empirical, 'Adaptive vs. maladaptive domains for confirmation bias').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(inner_models, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inmcb_tr_t0, inner_models, theater_ratio, 0, 0.4).
narrative_ontology:measurement(inmcb_tr_t5, inner_models, theater_ratio, 5, 0.53).
narrative_ontology:measurement(inmcb_tr_t10, inner_models, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(inmcb_be_t0, inner_models, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(inmcb_be_t5, inner_models, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(inmcb_be_t10, inner_models, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(inner_models, information_standard).
narrative_ontology:affects_constraint(inner_models, tribal_epistemology).
narrative_ontology:affects_constraint(inner_models, institutional_narrative_control).
narrative_ontology:affects_constraint(inner_models, algorithmic_filter_bubbles).

% DUAL FORMULATION NOTE:
% Confirmation bias decomposes into three distinct constraints: (1) Resource-bounded Bayesian updating (mountain — mathematical limit), (2) Institutional amplification via information architecture and incentive structures (tangled rope — social extraction), (3) Neuroscience naturalization discourse (piton — performative authority). This story focuses on (2) as the primary social constraint; (1) is the upstream mathematical necessity; (3) is the institutional performance theater. Network edges link to downstream constraints that leverage confirmation bias: tribal epistemology (community-level extraction), institutional narrative control (top-down belief management), algorithmic filter bubbles (technological amplification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(inner_models, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
