% ============================================================================
% CONSTRAINT STORY: oc_donation_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_oc_donation_model, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: oc_donation_model
 *   human_readable: Open Culture's Voluntary Donation-Based Funding Model
 *   domain: economic/social/digital_culture
 *
 * SUMMARY:
 *   Open Culture's voluntary donation-based funding model represents a
 *   distinctive approach to cultural distribution that solves a genuine
 *   coordination problem — connecting dispersed creators with global
 *   audiences without commercial friction — while simultaneously creating
 *   structural extraction of creator economic value. The constraint exhibits
 *   the full spectrum of DR classification depending on observer position:
 *   content creators experience it as a snare (unpaid labor suppression), the
 *   platform experiences it as rope (pure coordination benefit), consumers
 *   experience it as rope (beneficial access coordination), donors experience
 *   it as tangled rope (mixed moral coordination and guilt-based extraction),
 *   the open-access movement experiences it as temporary scaffolding with a
 *   sunset, the legacy publishing industry sees it as a degraded vestige of
 *   their own failures, and the analytical observer recognizes it as a
 *   genuine hybrid. The theater ratio has increased over time as the platform
 *   has grown: early curation felt organic and passionate; as operations
 *   professionalize, the free-access promise increasingly performs
 *   ideological commitment rather than economic necessity. This trajectory
 *   suggests drift toward Piton (performative open-ness maintained through
 *   institutional inertia) unless the constraint transitions to
 *   institutionally-supported funding (Scaffold sunset) or acknowledges
 *   creator compensation mechanisms (shifting toward more balanced
 *   extraction).
 *
 * KEY AGENTS:
 *   - Content Creators: Primary victims (powerless/trapped) — their work is aggregated and redistributed without compensation; exit is nearly impossible once content enters digital commons
 *   - Open Culture Platform: Primary beneficiary (institutional/arbitrage) — receives reputation, traffic, and influence from curation; has exit options but maintains free model as strategic positioning
 *   - Content Consumers: Secondary beneficiary (moderate/mobile) — experience pure coordination benefit; have exit options but prefer the free-access model
 *   - Donor Base: Mixed actor (moderate/constrained) — experience both coordination (supporting cultural commons) and extraction (moral obligation without revenue guarantee)
 *   - Open Access Movement: Organized supporters (organized/constrained) — see constraint as temporary scaffolding supporting transition to institutional funding and open commons
 *   - Legacy Publishers: Institutional observers (powerful/arbitrage) — perceive constraint as degraded vestige of their own failed digital transformation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes genuine hybrid coordination-extraction structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(oc_donation_model, 0.32).
domain_priors:suppression_score(oc_donation_model, 0.28).
domain_priors:theater_ratio(oc_donation_model, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(oc_donation_model, extractiveness, 0.32).
narrative_ontology:constraint_metric(oc_donation_model, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(oc_donation_model, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(oc_donation_model, tangled_rope).
narrative_ontology:human_readable(oc_donation_model, "Open Culture's Voluntary Donation-Based Funding Model").
narrative_ontology:topic_domain(oc_donation_model, "economic/social/digital_culture").

domain_priors:requires_active_enforcement(oc_donation_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(oc_donation_model, content_consumers).
narrative_ontology:constraint_beneficiary(oc_donation_model, cultural_commons).
narrative_ontology:constraint_beneficiary(oc_donation_model, educational_access).
narrative_ontology:constraint_victim(oc_donation_model, content_creators).
narrative_ontology:constraint_victim(oc_donation_model, platform_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTENT CREATOR (SNARE) — Independent artists, educators, and creators whose work is aggregated by Open Culture receive no direct compensation. Exit is difficult: removing content from the internet is nearly impossible, and the aggregation itself provides attribution but not revenue. Creators are trapped in a system that celebrates their work while extracting economic value from its distribution. They bear the suppression of alternative revenue models (paywalls, licensing fees) through the platform's free-access ideology.
constraint_indexing:constraint_classification(oc_donation_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPEN CULTURE PLATFORM (ROPE) — The aggregator benefits from coordination: curating and distributing free content creates network effects, drives traffic, and builds reputation. The platform's founders experience the constraint as solving a coordination problem — connecting creators with audiences at scale. They have exit options (alternative monetization, premium tiers) but maintain the free model as strategic positioning. Net beneficiary through reputation and influence.
constraint_indexing:constraint_classification(oc_donation_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CONTENT CONSUMERS (ROPE) — Users experience pure coordination: the constraint solves the problem of accessing dispersed cultural and educational content without friction. Exit options are available (use individual platforms, pay for premium services), but the free-access model is highly preferable. Low suppression from the consumer perspective — they coordinate to access desired content without coercion. This is the beneficiary class experiencing clear coordination benefit.
constraint_indexing:constraint_classification(oc_donation_model, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: DONOR BASE (TANGLED ROPE) — Voluntary donors (readers who choose to support the platform) experience both coordination and extraction. They benefit from access to the curated commons and the knowledge that the platform sustains free cultural distribution. But they are also targets of the platform's sustainability model — the free-access promise creates moral obligation to donate without guarantee that donations suffice. Exit is constrained: withdrawing support means abandoning a public good they value. Moderate extraction through guilt-based funding solicitation.
constraint_indexing:constraint_classification(oc_donation_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: OPEN ACCESS MOVEMENT (SCAFFOLD) — Organized actors (academic libraries, creative commons advocates, open-education coalitions) see Open Culture as temporary scaffolding supporting a broader transition toward open cultural commons. The constraint is structured with a sunset: as institutional funding, grants, and public library partnerships mature, voluntary individual donations become less critical. Low theater because the open-access goal is genuine, not performative. High suppression tolerance because the temporary nature justifies constraints on individual creator revenue.
constraint_indexing:constraint_classification(oc_donation_model, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL PUBLISHER INDUSTRY (PITON) — Legacy publishing and media industries perceive Open Culture as a degraded constraint on their extraction model. The platform exists because publishers failed to innovate around digital distribution; Open Culture is a response to that failure. Publishers maintain their own paywalls and licensing models through institutional inertia, but the open aggregator model reveals their systems as vestigial. The publisher perspective sees the constraint as theater — the free model performs 'democratized access' while the underlying economic problem (how creators earn) remains unresolved.
constraint_indexing:constraint_classification(oc_donation_model, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the constraint exhibits genuine coordination (solving the aggregation and distribution problem) AND asymmetric extraction (creators subsidize consumers through unpaid distribution of their work). The tension is structural: maintaining free access requires either suppressing creator revenue or introducing externally-sourced funding (donations, grants, institutional support). The analytical observer sees this as a real hybrid, not a false summit or performative ritual.
constraint_indexing:constraint_classification(oc_donation_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(oc_donation_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(oc_donation_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(oc_donation_model, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(oc_donation_model, TR),
    TR >= 0.70.

:- end_tests(oc_donation_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The constraint extracts creator economic value through unpaid aggregation and distribution, but the extraction is not as severe as pure snare because many creators willingly participate for non-monetary rewards (attribution, audience reach, cultural impact). The free-access model does suppress alternative revenue streams (creator licensing, direct sales), but it also enables scale and discovery that creators might not achieve independently. The moderate value reflects this genuine tension: real value extraction paired with real (if asymmetric) coordination benefits. Suppression (0.28): Moderate. Significant barriers exist to independent creator revenue (platform economics favor aggregation, attribution without payment is normalized), but suppression is incomplete — some creators use paywalls, sponsorships, and alternative platforms. The free-access ideology suppresses these alternatives rather than making them impossible. Theater ratio (0.42): Moderate. Early Open Culture curation felt organic and passionate; as the platform scaled, the free-access promise increasingly became a brand commitment rather than economic necessity. The theater has increased over the interval as professionalization requires continuous narrative justification of the model. This trajectory suggests growing performativity if the constraint persists without resolution.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits radical perspectival divergence: creators (snare), platform (rope), consumers (rope), donors (tangled rope), open-access advocates (scaffold), publishers (piton), analytical observer (tangled rope). This gap reflects that the free-access promise creates genuine benefits for some (consumers, aggregators, the cultural commons) while imposing costs on others (creators, donors who feel obligated to sustain it). The gap is not resolvable by better communication — it reflects real structural conflict. Consumers and the platform benefit from suppression of creator compensation mechanisms; creators and theoretically-conscious donors bear the cost of that suppression. The analytical observer's Tangled Rope classification is accurate because the constraint does both: it genuinely coordinates content distribution AND it extracts creator economic value. The snare perspective (creators) and rope perspective (platform) are both valid — they reflect real, irreconcilable structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation is fundamental here. Content creators occupy maximum d (full target): they are victims with no exit (trapped), yielding high f(d) and high experienced extraction. The platform and consumers occupy low d (beneficiaries with exit): they are beneficiaries with mobile/arbitrage options, yielding negative or near-zero experienced extraction. Donors occupy intermediate d (both/constrained): they are structurally beneficiaries (they value the commons) but the constraint applies moral pressure (guilt-based donation solicitation), making them partial targets. The open-access movement occupies intermediate-low d (beneficiaries/constrained): they benefit from the scaffold but are constrained by dependence on its success. The directionality computation reveals why creators experience high chi and donors experience moderate chi even though the base extractiveness is the same — their exit options and beneficiary/victim status differ structurally.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by being genuinely hybrid. The temptation is to classify it as pure Rope (beneficial coordination) by ignoring creator extraction, or pure Snare (pure extraction of creators) by ignoring consumer benefits. The Tangled Rope classification resolves the mandatrophy by insisting that BOTH elements are real and structural: the platform genuinely solves a coordination problem AND it structurally extracts creator economic value. The resolution requires accepting that the constraint can be simultaneously beneficial and extractive depending on agent position. The theater ratio increase suggests drift toward Piton (performative open-ness) if the constraint persists without addressing the creator-extraction component. The Scaffold perspective (open-access movement) provides a resolution path: institutional funding and explicit creator compensation would transform the constraint from permanent Tangled Rope toward Rope (pure coordination) by removing the suppression of creator revenue and replacing voluntary guilt-based donor extraction with transparent institutional support. Without this transition, the constraint risks becoming Piton — maintained through moral licensing and platform narrative rather than genuine economic functionality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creator_subsidy_threshold,
    'At what level of unpaid aggregation and redistribution does creator economic displacement become severe enough to suppress new content creation?',
    'Longitudinal tracking of creator participation rates and new content submissions over time; correlation with changes in aggregation scale and donor revenue; survey of creators on impact of free distribution on their income',
    'If threshold is crossed: the free-access model becomes unsustainable because creator supply dries up. If threshold is high: the constraint can persist indefinitely because enough creators accept non-monetary rewards (attribution, audience reach). Classification moves from Tangled Rope toward pure Snare if creator suppression becomes severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_subsidy_threshold, empirical, 'Economic threshold for creator income displacement sustainability').

omega_variable(
    donation_adequacy,
    'Do voluntary donations provide sufficient revenue to sustain the platform and expand its curation function indefinitely, or does the model face a structural funding ceiling?',
    'Historical analysis of donation trends relative to operational costs and content acquisition; comparison with similar platforms'' funding mechanisms; survey of donor motivation and retention rates',
    'If donations remain adequate: Scaffold perspective confirmed (temporary model with institutional funding replacing donations as fallback). If donations plateau: the constraint requires permanent external subsidy or revenue model change (moving toward Snare of institutional dependence). Classification stability depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(donation_adequacy, empirical, 'Long-term sustainability of voluntary donation funding model').

omega_variable(
    moral_licensing_collapse,
    'Does the free-access promise without creator compensation create a moral licensing effect where donors feel virtuous about supporting ''open culture'' while implicitly endorsing creator unpaid labor?',
    'Qualitative analysis of donor communications and platform messaging; philosophical analysis of whether the free model obscures labor extraction; comparison with explicitly pro-creator funding models',
    'If licensing effect is strong: the constraint operates through normalization of unpaid creative work, making extraction less visible (moving toward Piton of performative open-ness). If weak: the moral tension remains transparent, and the Tangled Rope classification is accurate. This affects whether the constraint is sustainable or requires eventual resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_licensing_collapse, conceptual, 'Whether free-access framing creates moral licensing for creator extraction').

omega_variable(
    institutional_funding_viability,
    'Can the constraint transition to institutional funding (libraries, foundations, government agencies) as primary revenue source while maintaining independence and curation quality?',
    'Historical study of institutional funding transitions in similar cultural aggregators; analysis of mission drift when cultural commons depend on institutional support; comparison of curation independence across funding models',
    'If viable: the Scaffold sunset is real and the constraint genuinely transitions. If institutional funding corrupts curation or reduces independence: the constraint becomes permanent Tangled Rope or evolves into new extraction (institutional capture). This determines whether the model is temporary or structurally locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_funding_viability, empirical, 'Viability of institutional funding as replacement for voluntary donations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(oc_donation_model, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ocdm_tr_t0, oc_donation_model, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ocdm_tr_t5, oc_donation_model, theater_ratio, 5, 0.35).
narrative_ontology:measurement(ocdm_tr_t10, oc_donation_model, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(ocdm_be_t0, oc_donation_model, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ocdm_be_t5, oc_donation_model, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(ocdm_be_t10, oc_donation_model, base_extractiveness, 10, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(oc_donation_model, resource_allocation).
narrative_ontology:boltzmann_floor_override(oc_donation_model, 0.32).
narrative_ontology:affects_constraint(oc_donation_model, creator_economics_digital_platforms).
narrative_ontology:affects_constraint(oc_donation_model, open_access_institutional_funding).

% DUAL FORMULATION NOTE:
% Open Culture's donation model is downstream of broader creator economics constraints (whether digital platforms should pay creators) and upstream of open-access institutional funding constraints (how to sustainably fund commons-based cultural infrastructure). The three form a causal chain: creator economics determines whether artists can afford to participate; Open Culture's model is a response to that constraint; institutional funding is a possible transition beyond the donation model. Each has distinct epsilon values reflecting their empirical status and structural character.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
