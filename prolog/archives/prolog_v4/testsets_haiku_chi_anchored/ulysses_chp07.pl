% ============================================================================
% CONSTRAINT STORY: ulysses_chp07
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp07, []).

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
 *   constraint_id: ulysses_chp07
 *   human_readable: The Rhetorical Press (Aeolus)
 *   domain: technological/social/political
 *
 * SUMMARY:
 *   The Aeolus episode of James Joyce's Ulysses (Chapter 7) stages the
 *   newspaper office as a symbolic space of rhetorical production and
 *   institutional gatekeeping. Leopold Bloom enters the Freeman's Journal
 *   office seeking to renew an advertisement for Alexander Dowie's bathhouse.
 *   What should be a straightforward commercial transaction becomes entangled
 *   in the newspaper's performative apparatus: editorial vanity, rhetorical
 *   display, departmental hierarchies, and the 'clanking drums' of printing
 *   machinery. The episode juxtaposes Bloom's modest commercial purpose with
 *   the editors' grandiose political and cultural rhetoric, revealing how the
 *   newspaper institution extracts value (flattery, attention, editorial
 *   favors, delayed placement) from advertisers who have no alternative
 *   access to circulation. The constraint operates at multiple levels: the
 *   technological level (printing press monopoly on information
 *   distribution), the institutional level (newspaper gatekeeping through
 *   editorial discretion), the social level (rhetorical performance as proof
 *   of cultural legitimacy), and the political level (media control over
 *   public discourse). Bloom is trapped: he needs the newspaper's circulation
 *   to reach customers; the newspaper controls that access; the only way to
 *   secure favorable placement is to participate in the rhetorical theater—to
 *   entertain the editors, to perform commercial legitimacy, to wait for
 *   editorial whim. The constraint is a snare: Bloom has no real alternative,
 *   suppression is high (no other mass circulation mechanism available), and
 *   the newspaper extracts value disguised as editorial discretion and
 *   cultural curation.
 *
 * KEY AGENTS:
 *   - Leopold Bloom: Primary victim (powerless/trapped) — needs newspaper circulation; has no alternative; must participate in rhetorical performance to secure placement
 *   - Freeman's Journal Editorial Apparatus: Primary beneficiary (institutional/arbitrage) — controls gatekeeping function; extracts flattery, editorial attention, deferred placement as leverage
 *   - Commercial Advertiser Network: Secondary victim (moderate/constrained) — dependent on newspapers for reaching middle-class customers; constrained by monopoly on urban circulation
 *   - Print-Based Public Sphere: Organized actor (organized/mobile) — benefits from coordination function of newspapers; also bears extraction cost through filtered information access
 *   - Printing Press Technology: Institutional artifact (institutional/arbitrage) — appears as constraint mechanism; actually degraded from its democratizing function to gatekeeping tool
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees hybrid coordination-extraction structure; recognizes theater ratio (0.81) indicating performative degradation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp07, 0.52).
domain_priors:suppression_score(ulysses_chp07, 0.68).
domain_priors:theater_ratio(ulysses_chp07, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp07, extractiveness, 0.52).
narrative_ontology:constraint_metric(ulysses_chp07, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ulysses_chp07, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp07, snare).
narrative_ontology:human_readable(ulysses_chp07, "The Rhetorical Press (Aeolus)").
narrative_ontology:topic_domain(ulysses_chp07, "technological/social/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp07, journal_editorial_apparatus).
narrative_ontology:constraint_beneficiary(ulysses_chp07, rhetorical_gatekeepers).
narrative_ontology:constraint_victim(ulysses_chp07, commercial_advertisers).
narrative_ontology:constraint_victim(ulysses_chp07, ordinary_circulation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEOPOLD BLOOM (SNARE) — Trapped. Requires the newspaper's circulation to reach potential customers; cannot exit without losing business opportunity. Faces extraction disguised as editorial discretion and rhetorical performance. No alternative coordination mechanism exists. d≈0.92, f(d)≈1.40, σ=0.8 → χ≈0.58.
constraint_indexing:constraint_classification(ulysses_chp07, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: FREEMAN'S JOURNAL EDITORIAL APPARATUS (ROPE) — Experiences the constraint as coordination: managing the gatekeeping function, filtering signal from noise, curating editorial voice. Benefits from advertiser dependence (leverage over content). Sees rhetorical performance as necessary coordination overhead. d≈0.08, f(d)≈-0.11, σ=0.9 → χ≈-0.05. Net beneficiary through arbitrage.
constraint_indexing:constraint_classification(ulysses_chp07, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: COMMERCIAL NEWSPAPER ECOSYSTEM (SNARE) — Must use newspapers to reach urban middle-class customers; alternative advertising channels (posters, handbills, word-of-mouth) insufficient for scale. Constrained by newspaper monopoly over circulation access in major cities. Editorial discretion and rhetorical gatekeeping extract value. d≈0.78, f(d)≈1.12, σ=0.9 → χ≈0.52.
constraint_indexing:constraint_classification(ulysses_chp07, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PRINT-BASED PUBLIC SPHERE (TANGLED ROPE) — Organized actors (readership, circulation networks, printing technology) depend on newspaper infrastructure for access to information and cultural conversation. The rhetorical filtering function serves a genuine coordination purpose (managing information overload, providing editorial curation). But extraction is embedded: editorial apparatus extracts cultural capital and political influence through gatekeeping. Mobile exit option (eventually: alternative media, oral networks, handwritten circulation) exists but requires institutional disruption. d≈0.52, f(d)≈0.68, σ=1.0 → χ≈0.35.
constraint_indexing:constraint_classification(ulysses_chp07, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PRINTING PRESS APPARATUS (PITON) — From the civilizational view, the constraint reflects degradation of the printing press's original coordination function. Gutenberg's press was a technology that democratized access to text. By 1904, the printing press apparatus has become a gatekeeping mechanism maintained by institutional inertia (newspaper monopolies, editorial traditions, advertising dependence) rather than by genuine technological necessity. Theater ratio 0.81 reflects that much of the 'editorial curation' is performative (clanking drums, rhetorical display, office theater) rather than substantive verification or filtering. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.04. Piton classification reflects theater gate (theater_ratio ≥ 0.70).
constraint_indexing:constraint_classification(ulysses_chp07, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the newspaper serves genuine coordination functions (distributing information, creating shared epistemic commons, enabling public conversation) while simultaneously extracting rent through gatekeeping authority and rhetorical performance. The constraint is hybrid: the newspaper cannot disappear without creating information asymmetry and public fragmentation; but the extraction mechanism is unnecessary and historically contingent on institutional power consolidation. d≈0.68, f(d)≈1.02, σ=1.2 → χ≈0.54.
constraint_indexing:constraint_classification(ulysses_chp07, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp07_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp07, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp07, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp07, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp07, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp07_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The newspaper extracts multiple forms of value from advertisers: payment for placement, editorial flattery (rhetorical performance as fee), delayed publication (temporal leverage), and deference to editorial authority. The extraction is not total (some advertisers do secure placement; some commercial coordination does occur), but the asymmetry is severe. Bloom enters the office with a simple transaction and leaves having surrendered considerable deference and attention to editorial vanity. Suppression (0.68): High. Advertisers cannot exit because: (1) no alternative mass circulation mechanism exists in 1904 Dublin; (2) word-of-mouth, posters, and direct communication reach insufficient scale; (3) the newspaper monopolizes urban middle-class attention. Exit is not absolutely impossible (mobile outcome exists for large commercial operations), but for small advertisers like Bloom it is effectively unavailable. Theater ratio (0.81): Very high. The episode emphasizes performative elements: the 'clanking drums' of the printing press, the editors' grandiose political rhetoric, the office's hierarchical theater, the 'clattering' of machinery. Much of what appears as editorial curation is performed for its own sake—to display cultural authority, editorial sophistication, newspaper prestige. Bloom's simple advertisement is treated as occasion for rhetorical display rather than as straightforward commercial service. The theater has increased over the ten-year interval as newspaper competition has intensified and editorials have become more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits radical perspectival divergence. From Bloom's viewpoint (powerless/trapped), it is pure extraction (Snare): he needs the newspaper and has no alternative. From the editorial institution's viewpoint (institutional/arbitrage), it appears as coordination (Rope): they are managing gatekeeping, filtering, curating editorial quality. The print-based public sphere sees a hybrid (Tangled Rope): newspapers provide genuine coordination (shared epistemic access) but the coordination is entangled with extraction (gatekeeping authority, information filtering). The printing press apparatus itself (civilizational/arbitrage) reveals degradation (Piton): the technology that democratized information access has become a gatekeeping monopoly maintained by institutional inertia and performative ritual. The analytical observer sees the full structure (Tangled Rope): the newspaper cannot disappear without creating information chaos, but the extraction mechanism is historically contingent and unnecessary. Each perspective is structurally correct from its position; the perspectival gap reveals how the same institutional apparatus produces opposite experiences for different actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Bloom (advertiser): Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. No exit option; complete dependence on editorial apparatus; must participate in rhetorical performance. Editorial apparatus: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Controls gatekeeping; leverages advertiser dependence. Commercial ecosystem: Victim + constrained → d≈0.78, f(d)≈1.12. High extraction. Can exit through local networks or large-scale alternatives, but constrained by newspaper monopoly on urban circulation. Print-based public sphere: Organized + mobile → d≈0.52, f(d)≈0.68. Mixed. Has agency (alternative media, oral networks eventually emerge); benefits from coordination function; bears extraction cost through filtered access. Printing press apparatus: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification from theater gate, not high chi. Degraded from democratizing technology to gatekeeping tool. Analytical observer: Analytical → d≈0.68, f(d)≈1.02. Hybrid classification reflects that the newspaper simultaneously enables public sphere coordination and extracts rent through institutional authority.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The snare classification is confirmed by the victim's structural position (Bloom is trapped, faces extraction, has no alternative) and by the high suppression (0.68) and theater ratio (0.81). However, the analytical perspective reveals that the snare is historically contingent and technologically degraded, not inevitable. The printing press was designed as a democratizing technology; by 1904, institutional consolidation (newspaper monopolies, advertising dependence, editorial gatekeeping) has transformed it into an extraction mechanism. The theater ratio (0.81) indicates that much of the apparent 'editorial curation' is performative maintenance of institutional authority rather than substantive service. The constraint resolves as a snare from the advertiser's perspective (trapped, no alternative, extraction disguised as editorial discretion) but reveals itself as a degraded coordination mechanism (piton) from the civilizational view. The mandatrophy is averted by recognizing that the coordination function is real (newspapers do distribute information, enable public discourse, curate signal from noise) but the extraction mechanism is unnecessary and historically specific. A future without newspaper monopolies would maintain the information coordination function while eliminating the extraction rent. The snare classification is justified for the 1904 epistemic moment; the piton classification anticipates the degradation of the newspaper institution as alternative media emerge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rhetoric_vs_substance_boundary,
    'Where is the boundary between legitimate editorial curation (substantive filtering) and performative gatekeeping (extractive rhetoric)?',
    'Analysis of advertiser outcomes: do editorial delays/rhetorical displays correlate with better-quality placements or merely with extraction of editorial flattery? Comparison of circulation impact with vs without editorial curation.',
    'If substantive: newspaper service is genuine coordination (Rope/Tangled Rope from more perspectives). If performative: extraction mechanism is pure gatekeeping theater (Snare from advertiser perspective is confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetoric_vs_substance_boundary, conceptual, 'Boundary between editorial curation and performative gatekeeping').

omega_variable(
    alternative_circulation_feasibility,
    'Could 1904-era commercial actors reach customers effectively through non-newspaper channels (pamphlets, direct mail, word-of-mouth networks, shop displays)?',
    'Historical case studies: tracking advertising effectiveness before newspapers, in small towns without newspapers, among commercial networks using non-newspaper distribution.',
    'If feasible alternatives exist: exit option should be ''mobile'' for advertisers, not ''trapped''. Classification shifts to Tangled Rope or Rope. If newspapers are monopoly gatekeepers: ''trapped'' is justified, Snare confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_circulation_feasibility, empirical, 'Whether non-newspaper advertising channels could substitute for newspaper circulation').

omega_variable(
    editorial_discretion_arbitrariness,
    'Is the newspaper''s editorial discretion over advertisement placement/timing exercised according to stated editorial principles, or is it fundamentally arbitrary/extractive?',
    'Historical analysis: do editorial delays correlate with newspaper financial incentives? Do advertisers with lower budgets face longer delays? Do rhetorical demands increase with advertiser''s bargaining power?',
    'If principled: gatekeeping serves filtering function (supports Rope classification). If arbitrary: pure extraction mechanism (confirms Snare from advertiser view).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(editorial_discretion_arbitrariness, empirical, 'Whether editorial discretion operates by principle or arbitrary extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp07, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aeolus_tr_t0, ulysses_chp07, theater_ratio, 0, 0.65).
narrative_ontology:measurement(aeolus_tr_t5, ulysses_chp07, theater_ratio, 5, 0.74).
narrative_ontology:measurement(aeolus_tr_t10, ulysses_chp07, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(aeolus_be_t0, ulysses_chp07, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(aeolus_be_t5, ulysses_chp07, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(aeolus_be_t10, ulysses_chp07, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp07, information_standard).
narrative_ontology:affects_constraint(ulysses_chp07, dublin_commercial_circulation).
narrative_ontology:affects_constraint(ulysses_chp07, print_media_gatekeeping).

% DUAL FORMULATION NOTE:
% The rhetorical press constraint is downstream of the printing press monopoly on mass circulation and upstream of specific Dublin commercial outcomes (Bloom's advertisement fate). The constraint family includes the technological-historical fact of print monopoly and the institutional-social fact of newspaper gatekeeping. Each has its own ε and classification; they are linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
