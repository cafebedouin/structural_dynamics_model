% ============================================================================
% CONSTRAINT STORY: parable_fish_turtle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parable_fish_turtle, []).

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
 *   constraint_id: parable_fish_turtle
 *   human_readable: The Ontological Lake (Fish and Turtle Parable)
 *   domain: philosophical/epistemological
 *
 * SUMMARY:
 *   The Fish and Turtle parable illustrates a fundamental constraint on
 *   knowledge: beings interpret reality through their available experiences,
 *   and when those experiences differ radically, consensus breaks down. The
 *   fish lives entirely in water and interprets water as the totality of
 *   existence. The turtle has access to both water and land but faces
 *   systematic suppression when reporting this dual experience. The fish
 *   council institutionalizes the water-only ontology, teaching it as natural
 *   law and excluding heterodox claims. This constraint operates across six
 *   different classification types depending on the observer's structural
 *   position, making it a diagnostic case for how lived experience shapes
 *   what counts as real. The parable is not about whether the fish is
 *   empirically wrong — it is about the structural mechanism that prevents
 *   the fish from recognizing its own limitation. The constraint's theater
 *   ratio increases over time as the council's authority becomes more
 *   performative, maintaining the water-only ontology despite accumulating
 *   evidence of its incompleteness.
 *
 * KEY AGENTS:
 *   - Fish Community: Primary victim (powerless/trapped) — cannot verify cross-boundary claims; locked in water-only ontology by sensory limits and social enforcement
 *   - Turtle: Secondary victim (powerless/trapped despite some mobility) — has dual experience but cannot transmit it; suppressed when reporting land experiences
 *   - Fish Council: Primary beneficiary (organized/constrained) — maintains ontological authority through monopoly on interpretation; extracts through gatekeeping
 *   - Inquiring Fish: Tertiary victim (moderate/constrained) — begins to doubt but cannot organize effective challenge within the community
 *   - Philosophical Observer: Neutral observer (analytical/analytical) — sees the lake as a coordination mechanism; experiences no extraction due to arbitrage options
 *   - Institutionalized Epistemology: Institutional actor (institutional/arbitrage) — maintains the framework through performance and exclusion; piton perspective reflects degradation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parable_fish_turtle, 0.58).
domain_priors:suppression_score(parable_fish_turtle, 0.65).
domain_priors:theater_ratio(parable_fish_turtle, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parable_fish_turtle, extractiveness, 0.58).
narrative_ontology:constraint_metric(parable_fish_turtle, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(parable_fish_turtle, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parable_fish_turtle, tangled_rope).
narrative_ontology:human_readable(parable_fish_turtle, "The Ontological Lake (Fish and Turtle Parable)").
narrative_ontology:topic_domain(parable_fish_turtle, "philosophical/epistemological").

domain_priors:requires_active_enforcement(parable_fish_turtle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parable_fish_turtle, within_lake_dwellers).
narrative_ontology:constraint_beneficiary(parable_fish_turtle, established_interpretive_authority).
narrative_ontology:constraint_victim(parable_fish_turtle, cross_boundary_understanding).
narrative_ontology:constraint_victim(parable_fish_turtle, external_knowledge_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FISH (SNARE) — The fish has no framework to conceive of 'land' or 'air'. Its lived experience in water is total. Any claim about existence beyond water cannot be verified through its available senses. The constraint appears as an absolute limit: 'existence ends where water ends' becomes an ontological law, not a contingent boundary. Maximum extraction occurs because the fish cannot imagine alternatives and thus cannot organize to challenge the limitation.
constraint_indexing:constraint_classification(parable_fish_turtle, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE TURTLE (SNARE) — The turtle has experienced both water and land but faces suppression when reporting this knowledge back to the fish community. The fish have no experiential category for 'land' and interpret the turtle's reports as confusion or delusion. The turtle is trapped because even with access to broader reality, it cannot transmit understanding across the ontological boundary. The community's inability to verify cross-boundary claims suppresses the turtle's testimony. High extraction through exclusion from epistemic authority.
constraint_indexing:constraint_classification(parable_fish_turtle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: THE FISH COUNCIL (TANGLED ROPE) — An organized group of elder fish achieves coordination around a shared interpretation of reality. The coordination function is real: the council creates stable, reproducible teachings that organize fish behavior and enable social reproduction. But the council also extracts through monopoly on ontological interpretation. Challenging their framework means social exclusion. The constraint contains both genuine coordination (shared meaning) and asymmetric extraction (gatekeeping of what counts as real). The council benefits from the suppression of alternative ontologies because maintaining unified interpretation is their power base.
constraint_indexing:constraint_classification(parable_fish_turtle, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE PHILOSOPHICAL OBSERVER (ROPE) — An external observer with access to both water and land sees the lake as a coordination mechanism. The fish interpret water, the turtle interprets water-plus-land, the external observer interprets the whole ecosystem. Each framework has integrity within its domain. The constraint appears as pure coordination: each perspective solves the genuine problem of making sense of available phenomena. No extraction occurs from the observer's standpoint because they see the fish council's teachings as locally valid, not as false oppression. High mobility and arbitrage options yield low experienced extraction.
constraint_indexing:constraint_classification(parable_fish_turtle, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 5: THE INQUIRING FISH (TANGLED ROPE) — An individual fish begins to suspect the council's teaching might be incomplete. This fish experiences both the coordination benefit (shared meaning enables social participation) and the extraction cost (silencing of doubt, social pressure toward conformity). Constrained because leaving the community is not viable — the lake is home. Moderate power because the doubts are becoming shared by others. The constraint manifests as mixed coordination-extraction: the framework is useful but restrictive.
constraint_indexing:constraint_classification(parable_fish_turtle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: THE INSTITUTIONALIZED EPISTEMOLOGY (PITON) — The fish council's interpretive authority has become performative. They continue to assert 'water is all that exists' even as anomalies (the turtle's reports, the doubting fish's questions) accumulate. The authority persists through institutional inertia: schools teach the standard framework, credentials depend on mastery of it, institutional rewards flow to those who reinforce it. But the functional necessity has atrophied — the framework no longer effectively explains all observed phenomena. It is maintained through theater: ritual ceremonies, citation hierarchies, exclusion of heterodox voices. Theater ratio 0.68 reflects that much of the council's activity is now performative maintenance rather than genuine explanation.
constraint_indexing:constraint_classification(parable_fish_turtle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 7: THE LOGICAL NECESSITY VIEW (MOUNTAIN) — One can argue that the fish's limitation is not institutional but logical: a being cannot conceive of experiences outside its sensory apparatus. This appears as an immutable natural law. However, the structural data contradicts the mountain classification. The turtle DOES have cross-boundary experience; the inquiring fish CAN organize alternative frameworks; external observers CAN communicate different ontologies. The constraint is not logical necessity but institutional suppression. This perspective naturalizes a contingent arrangement, making it appear inevitable. The engine's false summit detector should flag this classification as naturalization.
constraint_indexing:constraint_classification(parable_fish_turtle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parable_fish_turtle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parable_fish_turtle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parable_fish_turtle, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(parable_fish_turtle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(parable_fish_turtle, TR),
    TR >= 0.70.

:- end_tests(parable_fish_turtle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high. The constraint extracts through suppression of alternative ontologies, silencing of the turtle's testimony, and social pressure toward conformity. However, it is not maximal extraction because the coordination function is genuinely valuable — the fish council does provide a stable, reproducible framework that enables social organization. Much of what appears as extraction is legitimate first-mover authority for framework maintenance. Suppression (0.65): High. Systematic barriers prevent cross-boundary knowledge: sensory limitations are real, social pressure is real, career/status incentives align with defending the framework. But suppression is not total — the turtle can exist, the inquiring fish can have doubts, heterodox voices persist. Theater ratio (0.68): Increasing over time. Initially the council's ontology was genuinely predictive and explanatory (theater=0.35). As anomalies accumulate, more effort goes to defending the framework against counterexamples rather than explaining phenomena (theater=0.68). The threshold 0.68 indicates the framework has become largely performative — maintaining authority is more important than solving problems.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the fish (Snare, d≈0.95) and the council (Tangled Rope, d≈0.40) is maximal. They experience radically different constraints from the same structural fact: the council's authority. The fish experience pure extraction (trapped, powerless, no verification path). The council experiences coordination with asymmetric benefit (they gain authority, others lose autonomy). The philosophical observer collapses the gap by seeing both as locally valid — the constraint appears as neutral coordination. The turtle occupies the most structurally interesting position: it has cross-boundary experience but no path to share it, so it experiences suppression despite its knowledge advantage. The Snare perspectives (fish, turtle) significantly outnumber the beneficiary perspectives, but the beneficiary (council) controls the institutional authority to define reality itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are determined by beneficiary/victim status and exit options. The fish (powerless/trapped) have maximum directionality toward the constraint (d ≈ 0.95): they bear full cost and cannot escape. The turtle (powerless/trapped) also experience high d (≈ 0.90) despite some mobility, because returning to report knowledge triggers suppression. The fish council (organized/constrained) have moderate d (≈ 0.40) as beneficiaries with exit options — they could abandon their authority but choose not to. The philosophical observer (analytical/analytical) have low d (≈ 0.05) due to high arbitrage — they can enter and leave the lake framework at will. The inquiring fish (moderate/constrained) have moderate-high d (≈ 0.65) because they doubt but cannot organize effective exit. The directionality values reflect structural extraction flows: benefits flow toward the council, costs flow toward those without alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   CENTRAL MANDATROPHY: Is this constraint epistemological (Mountain, based on logical limits) or institutional (Tangled Rope / Snare, based on suppression)? The parable structure deliberately creates ambiguity. One reading: the fish's limitation is logical — a water-only being cannot conceive of land. No amount of institutional change will give the fish the sensory apparatus to perceive air. From this view, the constraint is Mountain: a natural law of consciousness. Alternative reading: the fish's limitation is institutional — the council enforces water-only ontology through social mechanisms (exclusion, ridicule, credential gatekeeping). If the council opened itself to the turtle's testimony and the inquiring fish's doubts, the community could evolve toward larger ontological frameworks. From this view, the constraint is Tangled Rope or Snare: institutional extraction masked as natural law. The engine resolves this by testing the natural law signature gates: is the constraint really inevitable (accessibility_collapse ≥ 0.85, resistance ≤ 0.15)? The structural data suggests NOT — the turtle's existence and the inquiring fish's doubts prove that cross-boundary understanding is possible. Therefore, the mountain classification is a false summit: it naturalizes institutional suppression. The actual constraint is Tangled Rope / Snare (institutional) with theater masquerading as logic. This resolution prevents the parable from becoming a philosophical justification for epistemic closure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_imperialism_vs_epistemological_humility,
    'Does the fish council''s framework represent genuine limitation of understanding or illegitimate suppression of cross-boundary knowledge?',
    'Historical analysis of framework evolution: do frameworks expand when new phenomena arrive (epistemological humility) or do they resist and exclude (ontological imperialism)? Measurement of prediction failures and explanatory gaps over time.',
    'If genuine limitation: constraint classifies as Mountain or Rope (natural coordination). If suppression: constraint classifies as Snare or Tangled Rope (institutional extraction). This is the central mandatrophy question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_imperialism_vs_epistemological_humility, conceptual, 'Whether the framework limitation is logical or institutional').

omega_variable(
    cross_boundary_communication_possibility,
    'Can the turtle actually communicate the experience of land to the fish using water-based analogies or metaphors? Is the gap truly unbridgeable?',
    'Experimentation with translation frameworks: can aspects of cross-boundary experience be mapped into within-boundary terms? Success rate of turtle''s attempts to explain land across different time periods and contexts.',
    'If communicable: the constraint is primarily institutional (suppression of effort), not epistemological. If unbridgeable: the constraint has genuine epistemological core that institutional structures merely reinforce. Classification stability across perspectives changes based on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cross_boundary_communication_possibility, empirical, 'Whether cross-boundary communication is possible').

omega_variable(
    multi_perspective_integration,
    'Can the fish council integrate the turtle''s testimony and the inquiring fish''s doubts without collapsing or radically restructuring their framework?',
    'Historical cases of paradigm integration vs paradigm collapse: did established frameworks absorb contradictory evidence or did they require replacement? Rate of framework evolution when external perspectives arrive.',
    'If integrable: constraint is Tangled Rope with path to Scaffold (sunset). If requires collapse: constraint is pure Snare (no internal path to resolution). The sunset possibility depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_perspective_integration, empirical, 'Whether the framework can integrate contradictory evidence').

omega_variable(
    consciousness_and_verification,
    'Is the fish council''s insistence on direct sensory experience as the criterion for truth justified, or is it arbitrary gatekeeping?',
    'Philosophical analysis of verification standards: which experiences count as evidence? Can indirect evidence (turtle''s reports, inquiring fish''s logical arguments) be valid? Does the standard privilege the council''s position?',
    'If sensory-direct is justified: much of what appears as extraction is legitimate epistemic conservatism. If arbitrary: the entire suppression apparatus is revealed as power-maintenance. Classification as Snare vs Rope depends heavily on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consciousness_and_verification, preference, 'Justification of direct sensory verification standards').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parable_fish_turtle, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ontlake_tr_t0, parable_fish_turtle, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ontlake_tr_t3, parable_fish_turtle, theater_ratio, 3, 0.52).
narrative_ontology:measurement(ontlake_tr_t6, parable_fish_turtle, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(ontlake_be_t0, parable_fish_turtle, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ontlake_be_t3, parable_fish_turtle, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(ontlake_be_t6, parable_fish_turtle, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parable_fish_turtle, information_standard).
narrative_ontology:affects_constraint(parable_fish_turtle, paradigm_incommensurability).
narrative_ontology:affects_constraint(parable_fish_turtle, lived_experience_epistemology).
narrative_ontology:affects_constraint(parable_fish_turtle, institutional_ontology_gatekeeping).

% DUAL FORMULATION NOTE:
% The ontological lake constraint decomposes into three related constraints: (1) paradigm_incommensurability models the logical structure of non-overlapping frameworks (ε ≈ 0.15, Mountain), (2) lived_experience_epistemology models the verification problem of translating cross-boundary experience (ε ≈ 0.42, Tangled Rope), (3) institutional_ontology_gatekeeping models the social enforcement of framework monopoly (ε ≈ 0.68, Snare/Tangled Rope). The present story integrates all three, but disaggregation reveals that theater_ratio growth (0.35 → 0.68) reflects shift from genuine epistemic problem toward institutional suppression. The upstream logical constraint is stable; the downstream institutional constraint is degrading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(parable_fish_turtle, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
