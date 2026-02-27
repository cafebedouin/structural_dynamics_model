% ============================================================================
% CONSTRAINT STORY: geocentric_cosmology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geocentric_cosmology, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geocentric_cosmology
 *   human_readable: The Geocentric Cosmological Model (as embodied by the Antikythera Mechanism)
 *   domain: history_of_science/technology/cosmology
 *
 * SUMMARY:
 *   The geocentric cosmological model, as instantiated in the Antikythera
 *   Mechanism and the broader Ptolemaic framework, represents a constraint on
 *   how ancient and medieval natural philosophers could represent celestial
 *   motion. The Antikythera Mechanism (c. 100 BCE) is a bronze analog
 *   computer that encodes the geocentric model in geared machinery, enabling
 *   prediction of celestial positions and eclipse cycles. This constraint
 *   story examines whether the geocentric model is a natural law of geometry
 *   and observation (Mountain), a coordination mechanism for astronomical
 *   knowledge (Rope), an institutional capture mechanism that suppressed
 *   alternatives (Snare/Tangled Rope), or a degraded framework maintained
 *   through institutional inertia (Piton). The core ambiguity is whether the
 *   geocentric model persisted because it was the best available solution to
 *   an underdetermined observational problem, or because institutional actors
 *   (the Church, university authorities) suppressed superior alternatives and
 *   used the model to concentrate knowledge and legitimacy. The Antikythera
 *   Mechanism itself vanished from historical record for ~2000 years, only to
 *   be rediscovered in 1901 aboard an ancient shipwreck — suggesting that
 *   knowledge encoded in physical technologies is vulnerable to erasure when
 *   institutional support collapses. The constraint is not the model itself
 *   but the structural forces that determined whether it was treated as an
 *   immutable law of nature or a contingent human construction.
 *
 * KEY AGENTS:
 *   - Ancient Mathematicians and Astronomers (Hipparchus, Ptolemy): Primary users of geocentric framework; face observational necessity and mathematical tractability constraints. Powerless/trapped relative to observational data; see no alternatives. Mountain from their perspective.
 *   - Antikythera Mechanism Builders and Craftspeople: Beneficiaries and carriers of embedded knowledge. Institutional actors with constrained exit (knowledge monopoly). Rope/Piton from their perspective (coordination tool and status symbol).
 *   - Institutional Authorities (Church, Universities): Secondary beneficiaries who used geocentric dogma to legitimize authority over cosmological truth. Institutional/arbitrage; see Piton (degraded framework maintained through inertia).
 *   - Heliocentric Proposers (Copernicus, Galileo, Kepler): Victims/challengers; face suppression and institutional resistance. Moderate/constrained; see Tangled Rope (mixed coordination and extraction from institutional authority).
 *   - Modern Physicists and Historians: Analytical observers who see mathematical equivalence of frames. Analytical/analytical; see Mountain (physical isomorphism).
 *   - The Astronomical Community (as collective): Organized actors solving coordination problem. Organized/mobile; see Rope (pure coordination mechanism).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geocentric_cosmology, 0.08).
domain_priors:suppression_score(geocentric_cosmology, 0.03).
domain_priors:theater_ratio(geocentric_cosmology, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geocentric_cosmology, extractiveness, 0.08).
narrative_ontology:constraint_metric(geocentric_cosmology, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(geocentric_cosmology, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geocentric_cosmology, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(geocentric_cosmology, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geocentric_cosmology, mountain).
narrative_ontology:human_readable(geocentric_cosmology, "The Geocentric Cosmological Model (as embodied by the Antikythera Mechanism)").
narrative_ontology:topic_domain(geocentric_cosmology, "history_of_science/technology/cosmology").

domain_priors:emerges_naturally(geocentric_cosmology).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — From a purely geometric standpoint, the geocentric model is a valid coordinate system for observational astronomy. Any celestial motion can be represented in a geocentric frame with appropriate epicycles. This is not a claim about physical reality but about the invariance of mathematical description. The constraint is the geometric equivalence principle: all inertial frames are equally valid for describing planetary positions. This perspective sees the geocentric model as an immutable law of geometry, not extractive. d≈0.72, f(d)≈1.15, but suppression and theater are minimal because the mathematical framework is transparent. χ ≈ 0.12 × 1.15 × 1.0 ≈ 0.14, but the mountain gate is triggered by ε ≤ 0.25, suppression ≤ 0.05, and accessibility_collapse ≥ 0.85.
constraint_indexing:constraint_classification(geocentric_cosmology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: KINEMATIC EQUIVALENCE (MOUNTAIN) — The Antikythera Mechanism demonstrates that geocentric geometry can encode planetary positions with high precision. This is a constraint on the problem-solving space: given observational data, multiple kinematic models can fit equally well. The constraint is not that the geocentric model is true, but that it is indistinguishable from heliocentric models using ancient observational technology. This is an epistemological mountain — a limit on what can be known from the data available. The Mechanism itself proves this indistinguishability. ε ≈ 0.08 (the minimal extractiveness comes from the Mechanism's computational advantage — it made celestial prediction accessible to elite patrons, creating a small wealth concentration). accessibility_collapse ≈ 0.88 because the Mechanism's loss and rediscovery shows how completely alternative technologies can vanish. resistance ≈ 0.12 because the geocentric framework persisted despite its computational overhead.
constraint_indexing:constraint_classification(geocentric_cosmology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: INSTITUTIONAL AUTHORITY (PITON) — The geocentric model became embedded in Aristotelian physics and theological doctrine (nested celestial spheres, perfection of circular motion, Earth as center of creation). Institutional actors (the Church, universities, the Ptolemaic establishment) used the geocentric model to legitimize authority over cosmological truth. The Mechanism itself became a status symbol for elite patrons — a way to display mastery of celestial mechanics. This institutional use of the model is extractive: it concentrates knowledge (how to build and interpret the Mechanism) among craftsmen and patrons, and it provides institutional legitimacy that suppresses alternative frameworks. theater_ratio ≈ 0.15 (relatively low because the Mechanism was functional, not performative; the institutional extraction is a secondary effect). But the model persisted through institutional inertia even as observational evidence accumulated (sunspots, stellar parallax, Jupiter's moons) that contradicted pure geocentrism. ε ≈ 0.08 (low base extraction because the model still works geometrically) but theater ≈ 0.15 suggests piton classification is marginal. The constraint here is not the model itself but its institutional capture.
constraint_indexing:constraint_classification(geocentric_cosmology, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANCIENT MATHEMATICIAN (MOUNTAIN) — From the perspective of Hipparchus or the Antikythera Mechanism's designer, the geocentric model was not a choice but an observational necessity. The apparent daily rotation of the celestial sphere is immediate, undeniable phenomenology. The Earth appears stationary; the heavens appear to move. Any mathematical framework that represents planetary positions relative to the observer must include the observer's frame of reference. This is not extraction — it is the irreducible structure of the problem. The constraint is: 'Given only naked-eye observations and geometric tools, deduce planetary positions.' The geocentric model is a solution to this constraint that is indistinguishable from the heliocentric solution using the technology available. d≈0.95 (powerless mathematician has no exit) but f(d)≈1.42 is scaled by suppression ≤ 0.05, giving minimal χ. The mountain gate requires ε ≤ 0.25, suppression ≤ 0.05, accessibility_collapse ≥ 0.85 — all satisfied.
constraint_indexing:constraint_classification(geocentric_cosmology, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: MODERN PHYSICIST (MOUNTAIN) — From the perspective of 21st-century physics, the geocentric and heliocentric models are mathematically isomorphic in Newtonian mechanics. The choice between them is a matter of convenience and intuition, not empirical truth. General Relativity says all inertial frames are equivalent; there is no absolute 'center' of the universe. The geocentric model is a valid (if inconvenient) description of celestial mechanics. This perspective views the constraint as an immutable law of physics: the equivalence of reference frames. The Antikythera Mechanism, then, is not a failed model but a successful solution to an underdetermined problem. ε ≈ 0.08 (no extraction from the physical equivalence itself; the extraction comes from institutional uses of the model). accessibility_collapse ≈ 0.88 because the Mechanism vanished from history for ~2000 years despite being a sophisticated technology — suggesting that coordinate-system choice is vulnerable to erasure when institutional support collapses. resistance ≈ 0.12 because the geocentric frame remains useful in some domains (e.g., navigation, planetary position prediction) even though the heliocentric frame is more natural.
constraint_indexing:constraint_classification(geocentric_cosmology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ASTRONOMICAL COMMUNITY (ROPE) — The Antikythera Mechanism represents a coordinated solution to a collective action problem: how to make celestial prediction accessible beyond specialists. The Mechanism encoded the mathematical framework in a physical device that could be operated and updated without deep knowledge of astronomical theory. This enabled coordination between instrument builders, astronomers, and patrons. The constraint from this perspective is not extractive but coordinative: the need to agree on a common framework for celestial prediction. The geocentric model provided this common language. ε ≈ 0.05 (minimal extraction; the Mechanism primarily solved coordination problems). suppression ≈ 0.03 (low; the mathematical framework was not hidden, just complex). This perspective sees the Rope classification because the model was a solution to coordination, not a mechanism of extraction. d≈0.45 (organized community of astronomers; both beneficiaries and targets of coordination). f(d)≈0.40, σ=0.9 → χ≈0.02. Pure coordination.
constraint_indexing:constraint_classification(geocentric_cosmology, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geocentric_cosmology_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geocentric_cosmology, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geocentric_cosmology, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(geocentric_cosmology, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geocentric_cosmology, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(geocentric_cosmology, ExtMetricName, E),
    domain_priors:suppression_score(geocentric_cosmology, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(geocentric_cosmology),
    narrative_ontology:constraint_metric(geocentric_cosmology, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(geocentric_cosmology, resistance, R),
    AC >= 0.85,
    R =< 0.15.

test(piton_threshold) :-
    domain_priors:theater_ratio(geocentric_cosmology, TR),
    TR >= 0.70.

:- end_tests(geocentric_cosmology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The geocentric model itself is primarily a geometric framework with minimal extractive content. The base extraction value reflects: (a) the computational advantage of the Antikythera Mechanism if it reduced prediction labor, and (b) the institutional use of the model to legitimize authority. But both are small effects — the model is fundamentally a solution to an underdetermined observational problem, not a coercive apparatus. Suppression (0.03): Very low. The geocentric model was not hidden or obscured; it was openly taught and debated. Mathematical objections to geocentrism were legitimate (Aristarchus proposed heliocentrism in ~270 BCE, but the model was geometrically complex). Institutional suppression of alternatives (Galileo, Bruno) is a secondary effect, not intrinsic to the model. Theater ratio (0.15): Low. The Mechanism itself was functional, not performative — it actually computed celestial positions. Institutional theater around the model increased over time (Ptolemaic dogma in medieval universities), but this is institutional capture, not intrinsic to the geometric framework.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival invariance across most observations: from the analytical observer (Mountain), the ancient mathematician (Mountain), and the modern physicist (Mountain), the classification is the same because all see the geocentric model as a valid geometric solution to an observational problem. The gap emerges between (a) the analytical/geometric perspectives (Mountain) and (b) the institutional authority perspective (Piton), which reveals that the model persisted partly through institutional capture and inertia rather than inherent validity. The astronomical community sees the Rope (coordination mechanism), which is orthogonal to both Mountain and Piton — the coordination function is real and distinct from the geometric validity question. The heliocentric challengers would see extraction (Snare or Tangled Rope from their perspective as victims), but they are not included as a primary perspective because they represent a later historical phase (16th-18th centuries) when the Mechanism was already lost to memory. The perspectival gap reveals the constraint's structure: the geocentric model is simultaneously an immutable law (of geometry), a coordination solution (for astronomy), and an institutional capture mechanism (for dogma).
 *
 * DIRECTIONALITY LOGIC:
 *   Analytical observer/mathematicians: Beneficiary + analytical → d≈0.72, f(d)≈1.15. But suppression ≤ 0.05 limits χ. Antikythera builders: Beneficiary + constrained → d≈0.15, f(d)≈-0.01. Institutional authorities: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Astronomical community: Both + mobile → d≈0.45, f(d)≈0.40. Heliocentric challengers (later period): Victim + constrained → d≈0.75, f(d)≈1.10 (but not primary perspective in this story).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the geocentric model is fundamentally a Mountain (geometric equivalence, observational indistinguishability with ancient technology) that was instrumentalized as institutional theater (Piton) when institutional authorities used it to suppress alternatives. The model itself carries minimal extractiveness (ε ≈ 0.08) and suppression (≤ 0.03), placing it in the Mountain gate. The theatrical element (theater_ratio ≈ 0.15) is low enough to avoid Piton classification by the theater gate (which requires theater ≥ 0.70), but it reveals that institutional uses of the model added performative content. The Rope classification emerges because the model was a genuine coordination solution for astronomical practice. No mandatrophy confusion arises here because the base extractiveness is low (ε ≈ 0.08) — there is no ambiguity between pure extraction and pure coordination. The constraint is primarily a Mountain with secondary institutional and coordinative functions, not an extraction mechanism masquerading as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    observational_sufficiency_threshold,
    'At what observational precision does the geocentric model become empirically distinguishable from the heliocentric model?',
    'Historical analysis of telescopic observations (Galileo''s moons of Jupiter, stellar parallax measurements by Bradley, precision observations from 18th-century onwards); calculation of angular resolution thresholds required to falsify geocentrism',
    'If threshold < 1 arcminute: geocentric model was already falsifiable in Hellenistic period with improved instruments. If threshold > 10 arcminutes: geocentric framework persisted for ~1500 years because observational technology made both models indistinguishable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observational_sufficiency_threshold, empirical, 'Observational precision threshold for falsifying geocentrism').

omega_variable(
    institutional_capture_mechanism,
    'Did the geocentric model persist as a constraint because of intrinsic mathematical validity or because institutional actors (Church, universities) suppressed alternatives?',
    'Historical analysis of suppression of heliocentric models (Copernicus, Galileo, Bruno); examination of whether mathematical objections were legitimate or post-hoc justifications; comparison with adoption timelines in regions with different institutional structures (Islamic astronomy, pre-Columbian Americas)',
    'If institutional capture: the constraint is partly artificial (Snare/Tangled Rope classification; high suppression). If intrinsic validity: the constraint is genuine (Mountain classification; low suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_mechanism, empirical, 'Role of institutional suppression in persistence of geocentric model').

omega_variable(
    antikythera_computational_advantage,
    'Did the Antikythera Mechanism provide computational efficiency gains that would have made it extractive (a Rope enabling wealth concentration) or was it merely a display of existing knowledge?',
    'Reconstruction of computation times for planetary prediction using the Mechanism vs using tables and astronomical canons; analysis of who had access to the Mechanism and what advantage it conferred; historical evidence of craft knowledge transfer and secrecy',
    'If efficiency gain was significant (>50% reduction in computation time): the Mechanism was extractive (Rope classification justified). If mainly symbolic: the constraint is primarily institutional theater (Piton classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antikythera_computational_advantage, empirical, 'Computational efficiency of Antikythera Mechanism vs manual methods').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geocentric_cosmology, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geocentric_tr_t0, geocentric_cosmology, theater_ratio, 0, 0.12).
narrative_ontology:measurement(geocentric_tr_t500, geocentric_cosmology, theater_ratio, 500, 0.15).
narrative_ontology:measurement(geocentric_tr_t1000, geocentric_cosmology, theater_ratio, 1000, 0.18).

% Extraction over time
narrative_ontology:measurement(geocentric_be_t0, geocentric_cosmology, base_extractiveness, 0, 0.07).
narrative_ontology:measurement(geocentric_be_t500, geocentric_cosmology, base_extractiveness, 500, 0.08).
narrative_ontology:measurement(geocentric_be_t1000, geocentric_cosmology, base_extractiveness, 1000, 0.09).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geocentric_cosmology, information_standard).
narrative_ontology:affects_constraint(geocentric_cosmology, ptolemaic_epicycle_framework).
narrative_ontology:affects_constraint(geocentric_cosmology, medieval_celestial_spheres_dogma).
narrative_ontology:affects_constraint(geocentric_cosmology, copernican_revolution_suppression).

% DUAL FORMULATION NOTE:
% The geocentric model as a geometric/mathematical constraint (Mountain) is distinct from the geocentric model as an institutional dogma (Piton/Snare). These represent two different constraints with different ε values: (1) geometric equivalence of reference frames (ε≈0.08, Mountain) and (2) institutional enforcement of geocentric orthodoxy (ε≈0.35-0.50, Tangled Rope/Snare). They are linked because the institutional dogma exploited the geometric validity to suppress alternatives, but they are structurally distinct. This story focuses on the geometric Mountain; downstream stories address institutional capture and suppression of heliocentrism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
