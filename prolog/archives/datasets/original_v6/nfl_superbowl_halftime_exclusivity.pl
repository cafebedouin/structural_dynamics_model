% ============================================================================
% CONSTRAINT STORY: nfl_superbowl_halftime_exclusivity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nfl_superbowl_halftime_exclusivity, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nfl_superbowl_halftime_exclusivity
 *   human_readable: NFL Super Bowl Halftime Show Exclusivity Agreements
 *   domain: economic/sports_entertainment
 *
 * SUMMARY:
 *   The NFL's Super Bowl Halftime Show exclusivity agreements represent a
 *   structural extraction mechanism embedded in entertainment industry
 *   practices. The constraint operates at the intersection of market
 *   dominance (the Super Bowl is the largest live entertainment event
 *   globally), intellectual property rights (broadcast and performance
 *   licensing), and cultural status (the halftime show performance is
 *   career-defining). The NFL leverages its position as the event organizer
 *   to negotiate exclusivity terms that restrict performers' ability to
 *   exploit the performance through competing broadcasts, secondary
 *   licensing, or related commercial deals. This creates a hybrid
 *   coordination-extraction structure: the exclusivity requirement solves a
 *   genuine collective action problem (preventing audience fragmentation and
 *   protecting broadcast premium) while simultaneously extracting value from
 *   performers who have limited alternatives. The constraint's extractiveness
 *   (0.58) reflects moderate but sustained asymmetry; the suppression (0.68)
 *   reflects significant barriers to performer organization and negotiation;
 *   the theater ratio (0.55) reflects that the exclusivity mechanism has
 *   declining functional necessity as streaming platforms mature.
 *
 * KEY AGENTS:
 *   - NFL League Office: Institutional beneficiary (institutional/arbitrage) — captures exclusive broadcast premium and controls the Super Bowl cultural moment
 *   - CBS/Broadcast Rights Holder: Institutional beneficiary (institutional/arbitrage) — secured exclusive rights by virtue of NFL exclusivity control
 *   - Halftime Performer: Moderate victim (moderate/trapped) — faces career opportunity structure where declining Super Bowl means losing peak visibility; accepting means submitting to exclusivity restrictions
 *   - Major Recording Labels: Powerful victim (powerful/constrained) — benefit from promotion but constrained in secondary monetization and platform licensing
 *   - Streaming Platforms/Alternative Events: Organized coalition (organized/mobile) — building alternative entertainment moments with comparable reach; have exit options and time horizon for disruption
 *   - Broadcast Television Industry: Institutional actor (institutional/arbitrage) — maintains ceremonial enforcement of exclusivity through regulatory framework and contractual obligations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nfl_superbowl_halftime_exclusivity, 0.58).
domain_priors:suppression_score(nfl_superbowl_halftime_exclusivity, 0.68).
domain_priors:theater_ratio(nfl_superbowl_halftime_exclusivity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nfl_superbowl_halftime_exclusivity, extractiveness, 0.58).
narrative_ontology:constraint_metric(nfl_superbowl_halftime_exclusivity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nfl_superbowl_halftime_exclusivity, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nfl_superbowl_halftime_exclusivity, tangled_rope).
narrative_ontology:human_readable(nfl_superbowl_halftime_exclusivity, "NFL Super Bowl Halftime Show Exclusivity Agreements").
narrative_ontology:topic_domain(nfl_superbowl_halftime_exclusivity, "economic/sports_entertainment").

domain_priors:requires_active_enforcement(nfl_superbowl_halftime_exclusivity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nfl_superbowl_halftime_exclusivity, nfl_league).
narrative_ontology:constraint_beneficiary(nfl_superbowl_halftime_exclusivity, cbs_broadcast_rights_holder).
narrative_ontology:constraint_victim(nfl_superbowl_halftime_exclusivity, halftime_performers).
narrative_ontology:constraint_victim(nfl_superbowl_halftime_exclusivity, competing_entertainment_platforms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HALFTIME PERFORMER (SNARE) — The performer is trapped by the career opportunity structure. Super Bowl halftime is the single largest live entertainment platform globally (100+ million viewers). Declining the performance ends their shot at peak visibility; accepting subjects them to exclusivity restrictions that prevent competing performances, commercial deals, or related work during the performance window and negotiated exclusivity period. The performer cannot organize collectively (one-shot opportunity per artist), has minimal exit options, and bears full extraction cost: restricted ability to monetize their peak visibility moment, prohibited side deals, career momentum captured by the NFL's broadcast terms.
constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NFL LEAGUE OFFICE (ROPE) — The NFL experiences the exclusivity agreement as a coordination mechanism. Without exclusivity clauses, performers could negotiate competing broadcast deals, stream independently, or license performances to rival platforms, fragmenting the viewing audience. The exclusivity requirement solves the collective action problem of maintaining the Super Bowl as the dominant cultural moment for that year's halftime entertainment. The league benefits (exclusive broadcast control, advertising premium), but genuinely provides coordination value: it guarantees the performer exclusive access to the largest possible audience, unified production standards, and protection from audience fragmentation. This is experienced as reciprocal coordination, not as pure coercion.
constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: MAJOR RECORDING LABEL (TANGLED ROPE) — The label has significant power but constrained exit options. The Super Bowl halftime show is a major promotional opportunity for the performer's new music, generating chart boosts, streaming surges, and cultural relevance. But the label is also constrained: it cannot negotiate independently for the performer without NFL approval; cannot exploit the performance across other platforms without NFL consent; and loses potential secondary licensing deals (Netflix, YouTube, international streaming) that the exclusivity agreement restricts. The label simultaneously benefits (massive free promotion) and bears extraction (restricted monetization paths). The constraint requires active enforcement (legal review, approval processes) and provides genuine coordination value (unified production, audience guarantee) alongside asymmetric extraction (NFL captures exclusive broadcast premium).
constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STREAMING PLATFORMS / ALTERNATIVE EVENTS (SCAFFOLD) — This organized coalition (Netflix, Amazon Prime, YouTube, Spotify, emerging metaverse entertainment venues) experiences the NFL's exclusivity agreements as a temporary barrier. The constraint has a clear sunset: as streaming platforms mature their live-event capabilities, as creator economics shift toward direct-to-audience models, and as performers gain alternative platforms with comparable reach (TikTok creators, metaverse performances, international broadcasts), the NFL's exclusive control weakens. Alternative halftime-scale entertainment moments are emerging (Grammy Awards, Oscars, international soccer finals) offering comparable visibility. The coalition has mobile exit options and can arbitrage between platforms. The constraint's theater ratio is moderate (0.55) because while the exclusivity term is enforced contractually, its functional necessity is declining as audience fragmentation across platforms makes any single broadcast less dominant.
constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: BROADCAST TELEVISION INDUSTRY (PITON) — For traditional television's regulatory framework, exclusivity agreements are largely ceremonial enforcement of a degraded coordination function. Historically, they prevented audience fragmentation across broadcast networks and ensured the league controlled the dominant cultural moment. Today, this function is atrophied: the Super Bowl's dominance as the single viewing event is genuine, but the exclusivity mechanism maintains only a fraction of its historical grip. Streaming platforms, mobile viewing, and international distribution have already fragmented the audience. The exclusivity agreement persists through institutional inertia and contractual obligation (NFL-broadcaster agreements), not because it meaningfully prevents audience flight. The theater ratio is moderate but the constraint functions primarily to preserve the implicit bargain between the NFL and its traditional broadcast partners, not because exclusivity itself is strategically necessary.
constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (NATURAL LAW VIEW) — From a civilizational scale, one might argue that exclusivity agreements are natural and immutable: any organization hosting the largest live entertainment moment must control how that moment is broadcast to preserve its status. Network effects and winner-take-all dynamics create an inherent structure where the dominant event provider captures exclusive broadcast rights. This perspective risks naturalizing what is actually a contingent institutional and legal arrangement (copyright, exclusive licensing, broadcasting regulation). The constraint is not a law of nature but a policy choice enforced by contract law and intellectual property frameworks.
constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nfl_superbowl_halftime_exclusivity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nfl_superbowl_halftime_exclusivity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nfl_superbowl_halftime_exclusivity, TR),
    TR >= 0.70.

:- end_tests(nfl_superbowl_halftime_exclusivity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The base value reflects moderate-high asymmetry in value capture. The performer receives payment (reported as pro-bono or nominal) and massive exposure (100+ million viewers), but the NFL captures the exclusive broadcast premium, advertising uplift, and downstream media rights. The performer cannot resell or rebroadcast the performance, cannot monetize secondary clips, and faces restrictions on competing entertainment during the exclusivity window. The extractiveness has increased over the 20-year interval (0.42 to 0.58) as streaming platforms created secondary monetization opportunities that the exclusivity agreement now forecloses — the constraint became more extractive as alternatives emerged. Suppression (0.68): High. Performers cannot collectively organize (one-shot opportunity per artist, not repeatable like other touring venues); cannot refuse without ending career opportunity; face contract terms set by the NFL with no meaningful negotiation capacity; and operate under asymmetric information (other performers' exact terms are confidential). Barriers to entry are structural (limited number of halftime slots per year) and exit is constrained (declining the Super Bowl eliminates peak visibility that competitors will capture). Theater ratio (0.55): Moderate. The exclusivity mechanism is contractually enforced and functional, but its necessity has declined with streaming fragmentation. The NFL still maintains the largest unified viewing audience for any entertainment event, but that dominance is eroding. The enforcement costs are moderate (legal review, secondary-use monitoring) and the functional necessity of exclusivity (preventing audience fragmentation) has diminished as fragmentation has already occurred across platforms.
 *
 * PERSPECTIVAL GAP:
 *   The snare/rope divide is the primary perspectival gap. The performer experiences pure extraction because they are locked into the opportunity structure and have no alternatives with comparable reach. The NFL experiences coordination because they are solving a genuine problem (audience unity, broadcast control) that performers benefit from (access to 100+ million viewers) — but this coordination value is asymmetrically distributed. The NFL captures the broadcast premium; the performer captures exposure but loses monetization rights. The gap is not about disagreement on facts but about structural position: from the performer's perspective, the constraint is coercive and extractive; from the league's perspective, it is coordinative and beneficial. The scaffold perspective (organized platforms with exit and sunset) provides a resolution mechanism: as alternative platforms mature, the constraint's functional necessity diminishes and the snare classification becomes increasingly inaccurate.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for each perspective follows from the agent's structural position relative to the extraction flow. The performer is trapped (no exit from the Super Bowl opportunity structure) and a victim (bears the exclusivity cost), producing high d → high f(d) → high χ. The NFL is a beneficiary (captures exclusive premium) with arbitrage options (can walk away from organizing if it becomes uneconomical), producing low d → negative f(d) → low/negative χ. The label experiences constrained exit (must participate to promote the performer, but has some negotiation capacity through roster leverage) and mixed victim/beneficiary (promotion benefit, licensing harm), producing moderate d. The streaming coalition has mobile exit options and can arbitrage between platforms, producing lower effective χ despite the formal constraint. The directionality overrides are not needed here; the structural derivation captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by explicitly acknowledging the hybrid coordination-extraction structure through the tangled rope and scaffold perspectives. The temptation toward mandatrophy would be to classify the constraint as pure extraction (snare) — 'the NFL exploits performers' — without recognizing that exclusivity does solve a genuine coordination problem (preventing fragmentation of the Super Bowl moment). The tangled rope classification (moderate performer perspective, powerful recording label perspective) captures the reality: the constraint provides genuine value (exposure, unified audience) alongside genuine extraction (licensing restriction, secondary monetization loss). The scaffold perspective provides the temporal resolution: as streaming platforms mature and alternative entertainment moments (metaverse performances, international broadcasts, creator-economy platforms) offer comparable reach, the functional necessity of NFL exclusivity declines. The constraint will transition from snare (performer has no exit) through tangled rope (hybrid coordination-extraction) to rope (pure coordination with mature alternatives) to eventually piton (ceremonial enforcement of a degraded coordination function) over a 10-20 year horizon. The mandatrophy is resolved by showing that the constraint's type changes as its structural conditions change — it is not one type misclassified as another, but a genuinely transitional constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performer_power_threshold,
    'At what level of performer fame/bargaining power do exclusivity agreements become renegotiable rather than take-it-or-leave-it?',
    'Historical analysis of performer contracts over 20-year interval; correlation between performer pre-Super Bowl fame/ranking and contract negotiation outcomes; identification of exceptions to standard terms',
    'If threshold is reached: major performers can negotiate opt-outs, reducing constraint bite on powerful beneficiaries. If no threshold exists: even superstars accept full exclusivity, indicating pure structural dominance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performer_power_threshold, empirical, 'Performer fame threshold for contract renegotiation').

omega_variable(
    alternative_platform_viability,
    'Do streaming platforms or international broadcasts offer genuinely comparable reach/cultural impact to the NFL''s Super Bowl broadcast within the 5-10 year horizon?',
    'Viewership data; performer career outcome analysis for halftime performers vs streaming-exclusive performances; audience demographic comparison; cultural impact metrics (chart performance, streaming surge, social media penetration)',
    'If yes: scaffold sunset is real, performers have genuine arbitrage options, constraint classification shifts toward rope. If no: NFL maintains monopoly, constraint remains snare for performers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Whether streaming platforms offer comparable reach to NFL broadcast').

omega_variable(
    exclusivity_enforcement_effectiveness,
    'How frequently do performers violate exclusivity agreements through secondary monetization, international licensing, or creative reinterpretation of terms? What are enforcement costs to the NFL?',
    'Contract dispute database review; performer secondary performances during exclusivity windows; NFL legal action count and settlement patterns; analysis of contract language evolution (tightening over time indicates enforcement challenges)',
    'If violation rates are high and enforcement costly: constraint is weakening piton, maintained by theater rather than functional necessity. If violation rates low: constraint has real bite, snare classification justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusivity_enforcement_effectiveness, empirical, 'Effectiveness and cost of exclusivity enforcement').

omega_variable(
    broadcasting_fragmentation_irreversibility,
    'Is the shift toward streaming and fragmented viewing irreversible, or could a unified ''Super Bowl moment'' reassert dominance through coordinated industry action?',
    'Technology adoption curves for streaming vs broadcast; behavioral data on live sports viewing; network effect modeling; regulatory analysis of potential content aggregation mandates',
    'If irreversible: scaffold sunset timeline is firm (5-15 years), constraint will degrade to rope or piton. If reversible: NFL could enforce renewed exclusivity, maintain snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broadcasting_fragmentation_irreversibility, conceptual, 'Whether broadcasting fragmentation can be reversed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nfl_superbowl_halftime_exclusivity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nflsh_tr_t0, nfl_superbowl_halftime_exclusivity, theater_ratio, 0, 0.48).
narrative_ontology:measurement(nflsh_tr_t10, nfl_superbowl_halftime_exclusivity, theater_ratio, 10, 0.52).
narrative_ontology:measurement(nflsh_tr_t20, nfl_superbowl_halftime_exclusivity, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(nflsh_be_t0, nfl_superbowl_halftime_exclusivity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nflsh_be_t10, nfl_superbowl_halftime_exclusivity, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(nflsh_be_t20, nfl_superbowl_halftime_exclusivity, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nfl_superbowl_halftime_exclusivity, resource_allocation).
narrative_ontology:boltzmann_floor_override(nfl_superbowl_halftime_exclusivity, 0.25).
narrative_ontology:affects_constraint(nfl_superbowl_halftime_exclusivity, entertainment_platform_licensing).
narrative_ontology:affects_constraint(nfl_superbowl_halftime_exclusivity, sports_broadcaster_exclusive_rights).
narrative_ontology:affects_constraint(nfl_superbowl_halftime_exclusivity, performer_secondary_monetization).

% DUAL FORMULATION NOTE:
% The NFL exclusivity agreement decomposes into three related constraints: the direct exclusivity restriction on performers (this story), the downstream effect on entertainment platform licensing (inability to acquire Super Bowl halftime rights), and the performer's loss of secondary monetization opportunities. The performer constraint has ε=0.58 (tangled rope); the platform constraint has ε=0.45 (rope with coordination failure); the secondary monetization constraint has ε=0.62 (snare for performers' estates/legacy management). All three are linked through the NFL's market dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nfl_superbowl_halftime_exclusivity, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
