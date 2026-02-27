% ============================================================================
% CONSTRAINT STORY: ulysses_chp11
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp11, []).

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
 *   constraint_id: ulysses_chp11
 *   human_readable: The Auditory Lure (Ormond Hotel)
 *   domain: social/artistic/biological
 *
 * SUMMARY:
 *   Chapter 11 of Ulysses (Sirens) models the Ormond Hotel bar as a complex
 *   auditory system where sound, attention, and commercial interest
 *   interweave. Leopold Bloom and Stephen Dedalus encounter a richly
 *   orchestrated soundscape: a piano, a harp, voices, musical notation, the
 *   clinking of glasses, the murmur of conversation layered beneath. Joyce's
 *   text itself becomes auditory — words fragment, onomatopoeia dominates,
 *   the syntax mirrors musical structure. The auditory lure at the Ormond
 *   Hotel exhibits all six constraint types from different structural
 *   positions. The proprietor experiences pure coordination (Rope) — the
 *   soundscape brings patrons, extends their stay, creates the social
 *   ambiance that justifies bar economics. The musician experiences temporary
 *   artistic coordination (Scaffold) — the performance is valued as a moment
 *   of aesthetic contribution, but the role is contingent on the bar's
 *   commercial logic. The listener experiences mixed coordination and
 *   extraction (Tangled Rope) — the music enables social bonding and
 *   emotional depth, but also captures attention and manipulates mood. The
 *   entranced drinker at maximum immersion experiences pure extraction
 *   (Snare) — trapped in the acoustic field, unable to exit without
 *   abandoning social position, bearing full cost of distraction and
 *   emotional manipulation. The ritual of pub musicality appears degraded
 *   (Piton) — the live performance persists as performative tradition despite
 *   the availability of recorded alternatives, maintained by romantic
 *   mythology of the musician and authenticity theater. The civilizational
 *   analytical perspective risks naturalizing the constraint as an immutable
 *   feature of human auditory experience (Mountain) — sound cannot be
 *   escaped, attention is involuntary — but this naturalization obscures the
 *   contingent institutional choices that create and enforce the auditory
 *   lure. The theater ratio increases across the interval as the performance
 *   becomes increasingly self-aware, as if Joyce's literary technique itself
 *   transforms the bar into a stage where the auditory lure is performed for
 *   observers rather than merely experienced by participants.
 *
 * KEY AGENTS:
 *   - Bartender/Proprietor (Sean Maginni): Primary beneficiary (institutional/arbitrage) — controls the acoustic environment, captures extended dwell time and higher margins
 *   - Musical Performers (pianist, harpist): Secondary beneficiary (moderate/arbitrage) — compensated for performance, gain social prestige and artistic platform
 *   - Patrons (Bloom, Dedalus, others): Primary victims and beneficiaries (moderate/constrained) — benefit from coordination (sociability, aesthetic experience) but bear cost of attention extraction and emotional manipulation
 *   - The Bar's Acoustic Ecology: Structural constraint (institutional/analytical) — the physical and social design that enforces immersion
 *   - The Artistic Movement: Organized beneficiary (organized/mobile) — the bar serves as temporary coordinating site for aesthetic innovation
 *   - The Ritual of Public House Performance: Institutional actor (institutional/arbitrage) — maintains performative musicality through cultural mythology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp11, 0.38).
domain_priors:suppression_score(ulysses_chp11, 0.52).
domain_priors:theater_ratio(ulysses_chp11, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp11, extractiveness, 0.38).
narrative_ontology:constraint_metric(ulysses_chp11, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ulysses_chp11, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp11, tangled_rope).
narrative_ontology:human_readable(ulysses_chp11, "The Auditory Lure (Ormond Hotel)").
narrative_ontology:topic_domain(ulysses_chp11, "social/artistic/biological").

domain_priors:requires_active_enforcement(ulysses_chp11).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp11, bartender_sean_maginni).
narrative_ontology:constraint_beneficiary(ulysses_chp11, ormond_proprietor).
narrative_ontology:constraint_beneficiary(ulysses_chp11, musical_performers).
narrative_ontology:constraint_victim(ulysses_chp11, patrons_cognitive_autonomy).
narrative_ontology:constraint_victim(ulysses_chp11, listener_epistemic_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ENTRANCED LISTENER (SNARE) — The patron seated at the bar cannot exit the auditory field without abandoning their beverage, social position, and embodied presence. The soundscape enforces attention through immersion. Maximum extraction of cognitive capacity; suppression through environmental design and social ritual. The listener bears the cost of distraction and emotional manipulation.
constraint_indexing:constraint_classification(ulysses_chp11, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE AMBIVALENT DRINKER (TANGLED ROPE) — The patron benefits from the musical coordination: the soundscape creates sociability, enables bonding, facilitates conversation through shared auditory context. But the same coordination mechanism extracts attention and emotional investment. Constrained exit because leaving means losing the coordination benefit. Moderate power because the drinker can choose which bar, which hour, but not escape the auditory economy once seated.
constraint_indexing:constraint_classification(ulysses_chp11, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: THE PROPRIETOR/BARTENDER (ROPE) — Benefits from coordination: the musical environment attracts patrons, increases dwell time, enables higher margins through sustained attention and social bonding. The proprietor has full arbitrage exit — can change the sound design, hire different performers, adjust the constraint at will. Experiences the auditory field as pure coordination mechanism, not extraction. Extraction runs toward this agent.
constraint_indexing:constraint_classification(ulysses_chp11, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE ARTISTIC MOVEMENT (SCAFFOLD) — Modernist and literary movements use the bar as a temporary coordination site: the Ormond Hotel and similar venues serve as gathering points for artists, writers, musicians to build collective identity and aesthetic innovation. The constraint has a sunset: as movements mature into institutions (journals, galleries, publishing houses), the physical bar loses its central coordinating role. High suppression is tolerated because the sunset is visible — the artistic coalition sees this venue as a temporary scaffold for a larger structure.
constraint_indexing:constraint_classification(ulysses_chp11, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE RITUAL OF PUBLIC HOUSE MUSICALITY (PITON) — The tradition of live music in bars persists as performative ritual: the social meaning of the piano player, the cultural authentication of 'authentic' pub atmosphere, the theater of musicianship. The constraint is maintained through institutional inertia and aesthetic mythology (the romanticized pub musician) despite degraded functionality — recorded music and electronic amplification have obsoleted much of what the live performance once provided. Theater ratio high because the musical performance is substantially about its own performance.
constraint_indexing:constraint_classification(ulysses_chp11, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / AUDITORY IMMERSION VIEW (MOUNTAIN) — From a civilizational/universal perspective, the auditory lure reflects a fundamental biological constraint: humans cannot cognitively exit a proximate acoustic field without active neural suppression. Sound is unavoidable; attention is partially involuntary; the bar's acoustic design exploits an inherent feature of mammalian neurology. This perspective risks naturalizing what is actually a contingent institutional arrangement (the choice to create a soundscape, the choice to locate patrons within it, the choice to make exit costly) as an immutable property of human sensation.
constraint_indexing:constraint_classification(ulysses_chp11, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp11_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp11, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp11, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp11, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp11_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Ormond Hotel's auditory lure extracts listener attention and emotional engagement, but the extraction is not as severe as a pure snare (0.70+) because coordination benefits are genuinely present — the soundscape enables conversation, social bonding, and aesthetic experience that patrons voluntarily seek. The extraction is real (attention is captured, mood is manipulated) but mixed with coordination. The proprietor captures commercial benefit from extended dwell time and emotional investment. Suppression (0.52): Moderate-high. The acoustic design and social ritual enforce immersion: patrons cannot exit the soundscape without abandoning their beverage, their social position at the bar, and the conversation they are engaged in. The suppression is environmental (sound is unavoidable in a proximate field) and social (leaving signals social disengagement). But suppression is not total — patrons can move to the back room, speak loudly to mask the music, or simply leave the bar. Theater ratio (0.68): High and increasing. The performance component — especially in Joyce's rendering — is substantially theatrical. The musicians are performing for an audience; the patrons are partly performing sociability for each other; the bar itself is a stage. The theater increases over the interval as the literary technique becomes more self-aware about its own auditory performance, as if the constraint becomes increasingly performative and less purely functional. The ratio suggests institutional maintenance through ritual rather than through essential coordination.
 *
 * PERSPECTIVAL GAP:
 *   The proprietor sees pure coordination (Rope) — the auditory lure is a tool for bringing patrons and sustaining their presence. The beneficiary has arbitrage: they can change the music, hire different performers, redesign the acoustic space at will. The entranced listener sees pure extraction (Snare) — they are captured by the soundscape, unable to exit without social cost. The listener has no arbitrage and bears maximum extraction. The moderate patron sees mixed coordination and extraction (Tangled Rope) — they benefit from the sociability and aesthetic depth the soundscape enables, but are also manipulated and distracted. They have constrained exit: they can leave, but at social cost. The artistic movement sees a temporary structure with a sunset (Scaffold) — the bar is valuable as a coordinating site for aesthetic innovation, but this role is contingent. As the movement matures into institutions (journals, galleries, publishing), the physical bar loses centrality. The ritual of pub musicality sees its own degradation (Piton) — the performance persists through cultural mythology and romantic attachment to the figure of the musician, but the functional role has been obsoleted by recorded music. The theater is high because the performance is substantially about its own performance, its own authenticity authentication. The analytical observer risks seeing immutable natural law (Mountain) — humans cannot exit acoustic fields, attention is partially involuntary, the auditory lure exploits intrinsic neurology. But this is a false summit: the bar's proprietor designed the acoustic space; the choice to locate patrons within it; the choice to make exit socially costly. These are contingent institutional arrangements, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is determined by the agent's structural position relative to the auditory lure. The proprietor has d ≈ 0.05 (full beneficiary with arbitrage) — they control the constraint and experience extraction running toward them. The trapped listener has d ≈ 0.95 (full target with no exit) — they bear maximum experienced extraction through f(d). The moderate patron has d ≈ 0.55 (mixed) — they both benefit and suffer, giving them moderate experienced extractiveness. The organized artistic movement has d ≈ 0.45 (temporary victim-beneficiary mix with mobile exit) — they use the bar as a coordinating scaffold but are not entrapped by it. The analytical observer at civilizational scale has d ≈ 0.72 (analytical position with no real exit but detached perspective) — they see the constraint as a natural feature of auditory experience. The derivation from beneficiary/victim declarations and exit options drives these d values through the sigmoid f(d), which maps structural relationship to experienced extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The auditory lure resolves the mandatrophy by showing that the constraint exhibits genuine coordination (the soundscape enables sociability and aesthetic experience that patrons seek) AND genuine extraction (the proprietor captures commercial value from extended dwell time, the music captures listener attention and manipulates mood). This is the signature of Tangled Rope: both a coordination function AND asymmetric extraction, with suppression high enough (0.52) that the extraction would be invisible without the coordination framing. The proprietor experiences the constraint as pure coordination (Rope), which would be the misdiagnosis if we ignored the listener's perspective. The listener experiences pure extraction (Snare), which would be the misdiagnosis if we ignored the proprietor's coordination benefit. The Tangled Rope classification unifies these perspectives by recognizing that the SAME constraint serves coordination for the beneficiary and extraction for the victim. The mandatrophy is resolved by refusing the false binary: this is neither pure coordination nor pure extraction; it is hybrid, and the classification holds that hybridity in tension. The theater ratio (0.68) rising over the interval indicates that the constraint is increasingly maintained through performative ritual rather than through essential coordination — this is the drift toward Piton. If theater continues rising above 0.70, the constraint degrades into Piton (inertial, degraded, maintained by institutional mythology rather than function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_coercive_immersion,
    'Is the listener''s immersion in the auditory field voluntary coordination or coercive extraction?',
    'Behavioral data on patron exit rates, survey of self-reported agency vs. entrancement, comparison of dwell time between high-suppression and low-suppression auditory environments',
    'If voluntary: perspectives shift toward Rope and Scaffold. If coercive: perspectives shift toward Snare. Determines whether the constraint is fundamentally cooperative or exploitative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_coercive_immersion, empirical, 'Whether immersion is voluntary or coercive').

omega_variable(
    auditory_design_intention,
    'Does the Ormond Hotel''s acoustic design and musical curation reflect deliberate manipulation of attention or emergent coordination?',
    'Historical records of proprietor intent, architectural acoustics analysis, comparison of design choices to competing bar environments, correspondence or statements about auditory strategy',
    'If deliberate manipulation: beneficiary perspective confirmed, snare classification strengthened. If emergent: coordination reading strengthened, rope/scaffold reading more accurate. Classification depends partly on agency attribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(auditory_design_intention, empirical, 'Whether acoustic design reflects deliberate strategy or emergent coordination').

omega_variable(
    listener_cognitive_cost_quantification,
    'What is the actual cognitive/emotional cost to the listener of the auditory lure, relative to the coordination benefit?',
    'Neurocognitive measures of listener attention state, emotional response tracking, memory formation for conversation vs. music, fatigue levels post-visit, reported subjective cost-benefit',
    'If cost > benefit: Snare classification strengthened. If cost ≤ benefit: Tangled Rope or Rope classification more accurate. Drives the extractiveness score upward or downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(listener_cognitive_cost_quantification, empirical, 'Listener cognitive and emotional cost relative to coordination benefit').

omega_variable(
    alternative_coordination_availability,
    'Are there lower-suppression alternatives that achieve equivalent coordination (conversation, social bonding, aesthetic experience)?',
    'Comparative study of patrons in silent vs. musical environments, measurement of conversation quality and social bonding metrics, availability of silent-bar alternatives in the same city',
    'If alternatives exist and are available: the auditory lure becomes contingent design choice, not necessary feature — extractiveness increases (the constraint is enforced despite alternatives). If alternatives are absent or inferior: the auditory lure becomes necessary coordination mechanism — extractiveness decreases (it solves a real problem).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_coordination_availability, empirical, 'Whether lower-suppression coordination alternatives are available').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp11, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(siren_tr_t0, ulysses_chp11, theater_ratio, 0, 0.45).
narrative_ontology:measurement(siren_tr_t5, ulysses_chp11, theater_ratio, 5, 0.58).
narrative_ontology:measurement(siren_tr_t10, ulysses_chp11, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(siren_be_t0, ulysses_chp11, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(siren_be_t5, ulysses_chp11, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(siren_be_t10, ulysses_chp11, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp11, information_standard).
narrative_ontology:affects_constraint(ulysses_chp11, molly_bloomsday_attention).
narrative_ontology:affects_constraint(ulysses_chp11, literary_consciousness_capture).

% DUAL FORMULATION NOTE:
% The auditory lure at the Ormond Hotel is structurally distinct from the literary technique Joyce uses to represent it. The hotel bar is a social/biological constraint (sound, attention, bar economics). Joyce's prose technique (fragmentation, onomatopoeia, musical structure) is a narrative constraint operating at a different level. Both exhibit similar extraction-coordination mixtures, and both have high theater ratios, but they have different ε values and different structural victims/beneficiaries. The story models the bar itself; the literary technique is a separate constraint downstream of it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
