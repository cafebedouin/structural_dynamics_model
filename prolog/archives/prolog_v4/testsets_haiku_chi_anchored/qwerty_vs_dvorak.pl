% ============================================================================
% CONSTRAINT STORY: qwerty_vs_dvorak
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_vs_dvorak, []).

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
 *   constraint_id: qwerty_vs_dvorak
 *   human_readable: QWERTY Keyboard Lock-in vs. Dvorak Efficiency
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The QWERTY vs. Dvorak lock-in exemplifies how technological path
 *   dependence functions as a constraint that combines coordination benefits
 *   with extractive suppression of alternatives. QWERTY became the default
 *   keyboard layout through historical accident (compatibility with early
 *   typewriter mechanics) but has persisted through a combination of network
 *   effects (users trained on QWERTY, manufacturers standardized on QWERTY)
 *   and institutional inertia. Dvorak, designed by August Dvorak in 1936 to
 *   minimize finger travel and reduce same-finger digraphs, offers measurable
 *   typing efficiency (~10% speed improvement, reduced repetitive strain
 *   injury) but has never achieved significant market adoption. This
 *   constraint is not a mountain — QWERTY's dominance is not an immutable law
 *   of physics or logic. Rather, it is a tangled rope: the QWERTY standard
 *   genuinely solves a coordination problem (shared keyboards, trained users,
 *   software uniformity), but this coordination function has become a
 *   mechanism for suppressing superior alternatives. The extractiveness has
 *   grown over time (ε from 0.18 to 0.38) as the institutional lock-in
 *   deepened and the theater ratio rose (manufacturers and standards bodies
 *   invoke the 'natural' dominance of QWERTY, obscuring the fact that it was
 *   an arbitrary historical choice). The rise of programmable input methods
 *   and OS-level remapping suggests an emerging scaffold: tools exist to
 *   dissolve the technical lock-in, making QWERTY's dominance purely social
 *   and conventional.
 *
 * KEY AGENTS:
 *   - Dvorak Adopters: Primary victim (powerless/trapped) — face sunk retraining cost, ecosystem incompatibility, social friction; cannot exit without bearing cost
 *   - Typing Efficiency Advocates: Secondary victim (moderate/constrained) — constrained by QWERTY dominance; benefit from coordination but lose efficiency gains
 *   - QWERTY Hardware Manufacturers: Primary beneficiary (institutional/arbitrage) — benefit from standardization reducing inventory and design complexity
 *   - Software Ecosystem Providers: Primary beneficiary (institutional/arbitrage) — OS and application standardization on QWERTY reduces development cost
 *   - Open-Source Input Method Coalition: Organized agents (organized/constrained) — building tools (programmable keyboards, OS remapping) to circumvent lock-in; perceive sunset as technical barriers fall
 *   - Historical QWERTY Narrative: Institutional theater — the myth of mechanical typewriter optimization persists despite being historically inaccurate; functions as social justification
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent path dependence as inevitable network-effect law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_vs_dvorak, 0.38).
domain_priors:suppression_score(qwerty_vs_dvorak, 0.62).
domain_priors:theater_ratio(qwerty_vs_dvorak, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_vs_dvorak, extractiveness, 0.38).
narrative_ontology:constraint_metric(qwerty_vs_dvorak, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(qwerty_vs_dvorak, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_vs_dvorak, tangled_rope).
narrative_ontology:human_readable(qwerty_vs_dvorak, "QWERTY Keyboard Lock-in vs. Dvorak Efficiency").
narrative_ontology:topic_domain(qwerty_vs_dvorak, "technological/economic").

domain_priors:requires_active_enforcement(qwerty_vs_dvorak).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_vs_dvorak, qwerty_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_vs_dvorak, existing_qwerty_users).
narrative_ontology:constraint_beneficiary(qwerty_vs_dvorak, software_ecosystem).
narrative_ontology:constraint_victim(qwerty_vs_dvorak, dvorak_adopters).
narrative_ontology:constraint_victim(qwerty_vs_dvorak, typing_efficiency_potential).
narrative_ontology:constraint_victim(qwerty_vs_dvorak, alternative_layout_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DVORAK ADOPTER (SNARE) — Individual user trapped in QWERTY ecosystem. Learning Dvorak requires sunk cost (retraining time) with no direct payoff in most contexts where QWERTY is mandatory (shared computers, workplace keyboards). Exit is impossible without bearing costs. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.64.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TYPING EFFICIENCY ADVOCATE (TANGLED ROPE) — Moderate power; constrained exit. Benefits from coordination (standard layout enables shared keyboards and knowledge transfer), but is also extracted from: Dvorak's genuine efficiency advantage (~10% speed, reduced finger travel, fewer same-finger digraphs) is suppressed by QWERTY dominance. The constraint enforces coordination but also prevents superior technology from diffusing. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: QWERTY HARDWARE MANUFACTURER (ROPE) — Institutional beneficiary. The QWERTY constraint enables coordination: manufacturing one standard reduces inventory complexity, interchangeability across markets, training uniformity. The manufacturer experiences the constraint as a coordination solution, not extraction. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.04. Net beneficiary.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOFTWARE ECOSYSTEM PROVIDER (ROPE) — Institutional beneficiary. Operating systems, keyboards, input systems all standardize on QWERTY. This enables broader compatibility and reduces software development complexity. The constraint is a coordination good. d≈0.10, f(d)≈-0.06, σ=1.2 → χ≈-0.03.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN-SOURCE INPUT METHOD COALITION (SCAFFOLD) — Organized agents (software developers, keyboard enthusiasts) see QWERTY lock-in as a temporary constraint being circumvented by OS-level input remapping (xmodmap, Karabiner, Windows registry hacks) and programmable keyboards. These tools enable Dvorak adoption at zero hardware cost, dissolving the extraction mechanism. The coalition perceives a sunset: as programmable input becomes standard and OS support for custom layouts becomes trivial, QWERTY's dominance rests on convention alone, not technical lock-in. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.15.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: HISTORICAL QWERTY NARRATIVE (PITON) — The 'QWERTY was designed to prevent mechanical typewriter jams' story is largely mythologized. Modern research shows QWERTY's design was incremental, not optimized for the stated purpose. Yet the narrative persists as theater: manufacturers invoke history, users rationalize acceptance, standards bodies cite precedent. The constraint's theatrical maintenance (theater_ratio=0.55) reflects how the lock-in is sustained not by functional necessity but by institutional inertia and reduced awareness of alternatives. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NETWORK EFFECTS VIEW (MOUNTAIN) — From a universal/civilizational perspective, QWERTY appears to be a natural law: network effects create an insurmountable coordination trap. The larger the user base, the less likely alternative layouts can gain traction. This creates an equilibrium that looks immutable — like a mountain of economics. However, the structural data (ε=0.38, suppression=0.62, theater=0.55) contradicts pure mountainhood. Programmable input, OS-level remapping, and falling adoption costs suggest the constraint is institutional and contingent, not a law of network dynamics. The analytical perspective risks false naturalization.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_vs_dvorak_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(qwerty_vs_dvorak, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qwerty_vs_dvorak, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(qwerty_vs_dvorak, TR),
    TR >= 0.70.

:- end_tests(qwerty_vs_dvorak_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. QWERTY's lock-in extracts value from Dvorak adopters (retraining cost, incompatibility friction) and suppresses efficiency-seeking users from switching. However, the extraction is not as severe as a pure snare because: (1) QWERTY does provide genuine coordination benefits, (2) the retraining cost is substantial but not infinite, (3) programmable input methods have begun to reduce technical barriers. The trajectory (0.18→0.38) reflects increasing extraction as network effects intensified during the computer era: the larger the installed base of QWERTY-trained users and QWERTY-optimized software, the higher the cost to alternatives. Suppression (0.62): Moderate-high. QWERTY's suppression operates through multiple channels: (1) standard office keyboards ship with QWERTY labeling, (2) most software defaults to QWERTY without easy remapping, (3) Dvorak is unknown to most users (information suppression), (4) switching requires perceived value that outweighs learning cost + social friction. Theater ratio (0.55): Moderate. The 'QWERTY prevents typewriter jams' narrative is theatrical mythology — modern historical research shows QWERTY was a gradual design choice, not optimized for the stated purpose. Yet this narrative persists as social justification. The theater has remained relatively stable as manufacturers invoke tradition without examining it.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a significant perspectival gap between beneficiaries and victims. QWERTY manufacturers and software providers (institutional actors with arbitrage exit) perceive pure coordination — the constraint solves a real problem (manufacturing uniformity, software compatibility). Dvorak adopters and typing efficiency advocates perceive extraction — the constraint prevents them from accessing a superior technology despite willingness to bear the retraining cost. The open-source coalition perceives a scaffold with a sunset — as programmable input becomes standard, the technical lock-in dissolves, leaving only convention. The piton perspective reveals that the constraint's persistence rests increasingly on institutional inertia and mythologized history, not functional necessity. The analytical observer risks falsely naturalizing the constraint as an inevitable law of network effects, obscuring the contingent institutional choices that sustain it.
 *
 * DIRECTIONALITY LOGIC:
 *   QWERTY manufacturers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; experience constraint as coordination good. Software ecosystem: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.06. Net beneficiary. Dvorak adopters: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; cannot exit without bearing full retraining cost. Typing efficiency advocates: Victim + constrained → d≈0.68, f(d)≈1.05. High extraction; partially can improve their position (programmable input) but remain dependent on ecosystem adoption. Open-source coalition: Organized + constrained → d≈0.35, f(d)≈0.35. Low-moderate extraction; have agency to build circumvention tools and perceive a path forward. Historical narrative: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification comes from theater gate, not chi. Analytical observer: analytical → d≈0.70, f(d)≈1.15. Mountain classification is perspectival; risks naturalizing contingent path dependence.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint is properly classified as Tangled Rope, not a false Mountain or pure Snare. The mandatrophy is resolved by showing that QWERTY provides genuine coordination function (shared keyboards, software compatibility, universal training) — which prevents it from being classified as pure Snare — AND that this coordination function is asymmetrically distributed (manufacturers + software gain more than users, Dvorak users gain nothing) — which prevents it from being pure Rope. The constraint requires active enforcement through institutional preference (manufacturers standardize on QWERTY, OS defaults are QWERTY, keyboards ship pre-labeled) and informational suppression (Dvorak remains unknown to most users). The extractiveness has grown over the computer era as the installed base of QWERTY-trained users increased, making the cost to alternatives higher. The theater ratio (0.55) reflects that much of the justification for QWERTY's dominance is mythological (the typewriter jam story) rather than functional. The scaffold perspective (open-source input remapping) suggests the constraint is beginning to shift: as technical barriers fall, QWERTY's dominance rests purely on convention and habit, which is easier to challenge than technical lock-in. This decomposition reveals why QWERTY lock-in is neither inevitable (mountain) nor temporary (scaffold alone) but a persistent institutional arrangement (tangled rope) that will degrade only as alternative input methods become universally easy and as the trained user base eventually shifts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_threshold,
    'What level of keyboard heterogeneity would cause significant productivity loss or frustration? Is QWERTY standardization functionally necessary or merely convenient?',
    'Historical comparison: productivity metrics during typewriter-to-computer transition when both machines and layouts were variable. User satisfaction surveys from heterogeneous keyboard environments (shared institutional spaces with mixed layouts).',
    'If heterogeneity is highly disruptive: QWERTY functions as coordination (Rope from all perspectives). If heterogeneity is manageable: suppression is primarily institutional preference, not technical requirement (Snare/Tangled Rope dominate).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_necessity_threshold, empirical, 'Whether QWERTY standardization is functionally necessary for coordination').

omega_variable(
    dvorak_adoption_barrier_elasticity,
    'If input remapping became universally trivial (one-click OS setting, no retraining required), what fraction of users would switch to Dvorak or alternatives?',
    'Controlled experiment: distribute programmable input method; measure adoption rates and retention; survey users on barriers to switching (learning cost, coordination pressure, habit, ergonomic preference).',
    'If adoption > 20%: extraction mechanism is suppression + coordination cost. If adoption < 5%: most users find QWERTY adequate or prefer familiarity; constraint is primarily social/path-dependent, not lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_adoption_barrier_elasticity, empirical, 'User willingness to switch if technical barriers are eliminated').

omega_variable(
    efficiency_gain_realization_bottleneck,
    'Why hasn''t Dvorak''s ~10% typing speed advantage translated to market adoption? Is it unrealized marginal benefit (users don''t perceive the gain as valuable relative to retraining cost), or is the suppression mechanism so effective that users never encounter the alternative?',
    'Longitudinal study: train cohorts on Dvorak for fixed duration; measure subjective vs. objective efficiency gains; track willingness to pay for retraining or incentives required to sustain adoption.',
    'If users perceive gain but choose QWERTY: constraint is pure extraction (Snare). If users never encounter Dvorak: constraint is suppression of information (Tangled Rope with high theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_gain_realization_bottleneck, empirical, 'Whether Dvorak efficiency gains are realized or suppressed from user awareness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_vs_dvorak, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_tr_t0, qwerty_vs_dvorak, theater_ratio, 0, 0.35).
narrative_ontology:measurement(qwerty_tr_t50, qwerty_vs_dvorak, theater_ratio, 50, 0.45).
narrative_ontology:measurement(qwerty_tr_t100, qwerty_vs_dvorak, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(qwerty_be_t0, qwerty_vs_dvorak, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(qwerty_be_t50, qwerty_vs_dvorak, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(qwerty_be_t100, qwerty_vs_dvorak, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_vs_dvorak, information_standard).
narrative_ontology:affects_constraint(qwerty_vs_dvorak, alternative_input_method_adoption).
narrative_ontology:affects_constraint(qwerty_vs_dvorak, keyboard_manufacturing_standards).

% DUAL FORMULATION NOTE:
% QWERTY lock-in is downstream of historical typewriter design choices but represents a distinct structural constraint in the computer era. The upstream constraint (typewriter mechanical design) is no longer relevant; the downstream manifestations (software default settings, keyboard labeling standards, user training protocols) are the active enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_vs_dvorak, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
