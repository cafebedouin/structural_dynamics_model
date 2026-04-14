% ============================================================================
% CONSTRAINT STORY: audience_incentive_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_audience_incentive_mechanism, []).

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
 *   constraint_id: audience_incentive_mechanism
 *   human_readable: Audience Incentive Mechanism in Public Discourse
 *   domain: philosophy_of_language/discourse_analysis/social_epistemology
 *
 * SUMMARY:
 *   The audience incentive mechanism in public discourse creates performance
 *   rewards (engagement metrics, status markers, belonging signals) that
 *   select for social signaling over analytical work. When concepts are
 *   deployed in public contexts, speakers face a choice: use the concept as
 *   an analytical tool (applying it to generate insight) or as a badge
 *   (displaying it to signal group membership and values). The mechanism
 *   coordinates attention allocation and group identification but introduces
 *   mild extraction when performance incentives misalign with analytical
 *   goals. This constraint is downstream of
 *   context_dependent_concept_function (the mountain constraint establishing
 *   that concepts have different functions in different contexts) — the
 *   audience incentive mechanism is the specific coordination device that
 *   implements the context-dependent function shift in public discourse. The
 *   constraint's low extractiveness (0.18) reflects that the mechanism is
 *   primarily coordinative: it solves real problems (distributed attention
 *   allocation, group identification, credibility signaling) with bounded
 *   extraction. The theater ratio (0.35) captures that some badge-wearing is
 *   performative (displaying concepts without understanding them) but much of
 *   it is functional signaling.
 *
 * KEY AGENTS:
 *   - Public Discourse Participants: Primary beneficiaries (moderate/mobile) — use performance rewards to signal values and find community; low extraction because exit is available
 *   - Audience Members: Primary beneficiaries (moderate/mobile) — benefit from sorting function that reduces cognitive load in evaluating claims
 *   - Concept Originators: Primary beneficiaries (institutional/arbitrage) — academics and thought leaders whose concepts are amplified through badge-wearing
 *   - Tool-Using Analysts: Mixed position (moderate/constrained) — benefit from shared vocabulary but face career pressure to perform badge-wearing for visibility
 *   - Epistemic Reform Coalition: Organized agents (organized/constrained) — building alternative reward structures (Substack, Patreon, slow media) that reduce dependence on engagement metrics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(audience_incentive_mechanism, 0.18).
domain_priors:suppression_score(audience_incentive_mechanism, 0.22).
domain_priors:theater_ratio(audience_incentive_mechanism, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(audience_incentive_mechanism, extractiveness, 0.18).
narrative_ontology:constraint_metric(audience_incentive_mechanism, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(audience_incentive_mechanism, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(audience_incentive_mechanism, rope).
narrative_ontology:human_readable(audience_incentive_mechanism, "Audience Incentive Mechanism in Public Discourse").
narrative_ontology:topic_domain(audience_incentive_mechanism, "philosophy_of_language/discourse_analysis/social_epistemology").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(audience_incentive_mechanism, public_discourse_participants).
narrative_ontology:constraint_beneficiary(audience_incentive_mechanism, audience_members).
narrative_ontology:constraint_beneficiary(audience_incentive_mechanism, concept_originators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC DISCOURSE PARTICIPANT (ROPE) — Experiences the audience incentive mechanism as a coordination device that enables efficient social signaling and group identification. The performance rewards (likes, shares, status markers) solve the legitimate problem of 'how do I signal my values and find my community?' The extraction is minimal — the participant chooses which concepts to deploy as badges and can exit any particular discourse community at low cost.
constraint_indexing:constraint_classification(audience_incentive_mechanism, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: AUDIENCE MEMBER (ROPE) — Benefits from the sorting function: badge deployment allows rapid identification of in-group vs out-group, reducing cognitive load in evaluating claims. The mechanism coordinates attention allocation — audience members can quickly assess whether a speaker shares their values without deep engagement with arguments. Low extraction because the audience retains choice over which signals to trust.
constraint_indexing:constraint_classification(audience_incentive_mechanism, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONCEPT ORIGINATOR (ROPE) — Academics, public intellectuals, and thought leaders who introduce concepts benefit from the audience incentive mechanism through citation, influence, and status. The mechanism amplifies their work by creating a class of badge-wearers who propagate the concept. Minimal extraction — the originator's work reaches wider audiences through the signaling function, and they retain control over their analytical contributions.
constraint_indexing:constraint_classification(audience_incentive_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EPISTEMIC REFORM COALITION (SCAFFOLD) — Organized agents working on discourse norms (fact-checking initiatives, epistemic humility movements, long-form journalism) see the audience incentive mechanism as a temporary coordination problem being addressed by platform design changes and norm shifts. Substack, Patreon, and other direct-support models reduce dependence on engagement metrics; slow-media movements promote depth over virality. The coalition sees a sunset: as alternative reward structures mature, the performance incentive loses force.
constraint_indexing:constraint_classification(audience_incentive_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TOOL-USING ANALYST (TANGLED ROPE) — Participants who deploy concepts as analytical tools rather than badges experience mixed coordination and extraction. The mechanism coordinates discourse by establishing shared vocabulary, but it also creates career pressure to perform badge-wearing for visibility. The analyst benefits from the shared conceptual infrastructure but bears the cost of reduced engagement when using concepts analytically rather than performatively. Moderate extraction because the analyst has some exit options (niche audiences, alternative platforms) but faces real career costs.
constraint_indexing:constraint_classification(audience_incentive_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, the audience incentive mechanism is a low-extraction coordination device that solves the problem of distributed attention allocation in large-scale public discourse. The performance rewards are not extractive overhead but functional signals that enable efficient sorting. The mechanism's extractiveness is bounded by exit options — participants can choose alternative platforms, niche communities, or private discourse when performance incentives misalign with their goals.
constraint_indexing:constraint_classification(audience_incentive_mechanism, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(audience_incentive_mechanism_tests).
:- end_tests(audience_incentive_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The audience incentive mechanism is primarily a coordination device. The performance rewards solve legitimate problems: how to allocate attention in large-scale discourse, how to identify in-group vs out-group, how to build credibility and audience. The extraction is real but bounded — tool-using analysts face career pressure to badge-wear, and some analytical depth is traded for visibility — but the mechanism does not trap participants. Exit options are available (niche audiences, alternative platforms, private discourse), and much badge-wearing is functional rather than purely performative. Suppression (0.22): Low. The mechanism does not actively suppress alternatives. Participants can choose tool-using over badge-wearing at the cost of reduced engagement, but this is a tradeoff rather than coercion. Alternative platforms with different reward structures exist and are growing. The suppression reflects career pressure and visibility costs, not structural barriers. Theater ratio (0.35): Moderate-low. Some badge-wearing is performative (displaying concepts without understanding them), but much of it is functional signaling that serves real coordination purposes. The theater has increased slightly over the interval as engagement metrics have become more sophisticated and platform algorithms have optimized for virality, but the mechanism retains substantial functional content.
 *
 * PERSPECTIVAL GAP:
 *   The constraint classifies as rope from most perspectives because the mechanism is primarily coordinative with low extraction. Public discourse participants, audience members, and concept originators all experience net benefits. The analytical observer sees a low-extraction coordination device that solves real problems in distributed attention allocation. The tool-using analyst sees tangled_rope — mixed coordination and extraction — because they face career pressure to badge-wear despite preferring tool-using. The epistemic reform coalition sees scaffold — a temporary coordination problem with a sunset as alternative reward structures mature. The perspectival gap is narrow because the extraction is genuinely low and exit options are available. The mechanism does not trap participants or suppress alternatives; it coordinates attention with bounded performance incentives.
 *
 * DIRECTIONALITY LOGIC:
 *   All primary agents are beneficiaries with mobile or arbitrage exit options, producing low directionality values and low or negative effective extraction. Public discourse participants (moderate/mobile) experience the mechanism as coordination — they choose which concepts to deploy as badges and can exit any particular discourse community at low cost. Audience members (moderate/mobile) benefit from the sorting function without bearing significant costs. Concept originators (institutional/arbitrage) benefit from amplification with minimal extraction. The tool-using analyst (moderate/constrained) is the only agent with mixed experience — they benefit from shared vocabulary but face career pressure to badge-wear, producing moderate directionality and moderate effective extraction. The epistemic reform coalition (organized/constrained) sees a sunset — alternative reward structures are maturing — producing low effective extraction despite constrained exit because they have agency and see a path forward.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that low-extraction coordination mechanisms can introduce performance incentives without becoming extractive. The audience incentive mechanism is not a snare disguised as rope — it is a genuine rope with mild extraction visible from the tool-using analyst's perspective. The key structural features that keep extraction low: (1) exit options are available (alternative platforms, niche audiences, private discourse), (2) the mechanism does not actively suppress alternatives, (3) much badge-wearing is functional signaling rather than pure performance, (4) the coordination benefits (attention allocation, group identification, credibility signaling) are real and substantial. The tangled_rope classification from the tool-using analyst's perspective is legitimate — they experience mixed coordination and extraction — but the extraction is bounded by their constrained (not trapped) exit options and the availability of alternative reward structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    badge_tool_boundary,
    'Is there a sharp boundary between badge-wearing and tool-using, or is it a continuous spectrum with context-dependent thresholds?',
    'Linguistic analysis of concept deployment patterns; correlation between deployment type and subsequent argumentative moves; experimental manipulation of audience presence',
    'If sharp boundary: the mechanism cleanly separates coordination (badge) from analytical work (tool). If continuous spectrum: the extraction is lower than feared because most deployment is mixed-mode.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(badge_tool_boundary, empirical, 'Whether badge/tool distinction is categorical or continuous').

omega_variable(
    platform_design_causality,
    'Do engagement metrics cause badge-wearing, or do they merely reveal pre-existing social signaling preferences?',
    'Comparative analysis of discourse patterns on platforms with different reward structures (HackerNews karma vs Twitter likes vs academic citations); longitudinal tracking of individual behavior across platform migrations',
    'If causal: platform design changes can reduce extraction. If revealing: the mechanism is coordination of pre-existing preferences, not extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_design_causality, empirical, 'Whether engagement metrics cause or reveal signaling behavior').

omega_variable(
    analytical_depth_tradeoff,
    'Does badge-wearing actually reduce analytical depth, or does it enable analytical work by building audience and credibility?',
    'Longitudinal analysis of public intellectuals'' work: correlation between badge deployment frequency and depth of subsequent analytical contributions; comparison of badge-wearers vs non-badge-wearers on long-term intellectual productivity',
    'If reduces depth: extraction is real and the tangled_rope perspective is justified. If enables depth: the mechanism is pure coordination (rope from all perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analytical_depth_tradeoff, empirical, 'Whether badge-wearing trades off against or enables analytical depth').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(audience_incentive_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aud_incent_theater_t0, audience_incentive_mechanism, theater_ratio, 0, 0.25).
narrative_ontology:measurement(aud_incent_theater_t5, audience_incentive_mechanism, theater_ratio, 5, 0.3).
narrative_ontology:measurement(aud_incent_theater_t10, audience_incentive_mechanism, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(aud_incent_extract_t0, audience_incentive_mechanism, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(aud_incent_extract_t5, audience_incentive_mechanism, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(aud_incent_extract_t10, audience_incentive_mechanism, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(audience_incentive_mechanism, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of context_dependent_concept_function (the mountain constraint establishing that concepts have different functions in different contexts). The audience incentive mechanism is the specific coordination device that implements the context-dependent function shift in public discourse. The upstream constraint has ε ≈ 0.05 (mountain — the context-dependence is a structural feature of language). This constraint has ε = 0.18 (rope — the performance rewards are a coordination mechanism with bounded extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
