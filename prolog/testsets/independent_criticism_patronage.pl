% ============================================================================
% CONSTRAINT STORY: independent_criticism_patronage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_independent_criticism_patronage, []).

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
 *   constraint_id: independent_criticism_patronage
 *   human_readable: The Patronage Model for Independent Cultural Criticism
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The patronage model for independent cultural criticism emerged as an
 *   alternative to institutional employment and advertising-supported
 *   commodity writing. Platforms like Patreon and Substack enable direct
 *   audience-to-creator funding for long-form criticism on niche topics
 *   (experimental film, avant-garde music, architectural theory, video game
 *   narrative design) that traditional media markets cannot sustain. This
 *   constraint exhibits the Tangled Rope structure: it provides genuine
 *   coordination function (matching supply and demand for niche criticism)
 *   while simultaneously creating extraction mechanisms (platform rents,
 *   algorithmic gatekeeping, patron preference bias). The theater ratio has
 *   risen from 0.35 to 0.58 as platforms increasingly emphasize parasocial
 *   relationship-building and subscriber tier performativity over raw content
 *   delivery. The base extractiveness has increased from 0.22 to 0.38 as the
 *   system concentrates income among a small number of successful patrons
 *   while the barrier to entry for new critics has risen, creating a
 *   bifurcation between established independent critics (who capture 70%+ of
 *   available patronage) and aspiring critics who cannot reach sustainable
 *   income thresholds.
 *
 * KEY AGENTS:
 *   - Independent Critics: Primary beneficiaries (institutional/arbitrage) — gain income stability, audience ownership, and control over editorial direction unavailable in traditional media
 *   - Patronage Platforms (Patreon, Substack): Secondary beneficiary (institutional/arbitrage) — capture transaction fees (5-15%), user data, and network effects; experience constraint as coordination problem solved
 *   - Unpatronized Critics: Primary victims (powerless/trapped) — cannot sustain independent work without patron base; forced into commodity writing or institutional employment
 *   - Critical Discourse Commons: Secondary victim (powerless/trapped) — fragmentation into patron-serving niches reduces cross-cultural evaluative consensus
 *   - Legacy Media Institutions: Tertiary actors (institutional/constrained) — maintain symbolic prestige and editorial legitimacy through performative rituals despite audience migration
 *   - Analytical Observer: Sees full structure (analytical/analytical) — identifies both coordination function and extraction layers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(independent_criticism_patronage, 0.38).
domain_priors:suppression_score(independent_criticism_patronage, 0.42).
domain_priors:theater_ratio(independent_criticism_patronage, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(independent_criticism_patronage, extractiveness, 0.38).
narrative_ontology:constraint_metric(independent_criticism_patronage, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(independent_criticism_patronage, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(independent_criticism_patronage, tangled_rope).
narrative_ontology:human_readable(independent_criticism_patronage, "The Patronage Model for Independent Cultural Criticism").
narrative_ontology:topic_domain(independent_criticism_patronage, "economic/technological").

domain_priors:requires_active_enforcement(independent_criticism_patronage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(independent_criticism_patronage, independent_critics).
narrative_ontology:constraint_beneficiary(independent_criticism_patronage, patron_networks).
narrative_ontology:constraint_victim(independent_criticism_patronage, critical_discourse_commons).
narrative_ontology:constraint_victim(independent_criticism_patronage, non_patronized_perspectives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNPATRONIZED CRITIC (SNARE) — A critic without patron base cannot sustain long-form work; forced into clickbait, freelance commodity writing, or institutional employment. No viable exit from the patronage model except abandonment of independent voice. Bears full extraction cost: captured labor or silence.
constraint_indexing:constraint_classification(independent_criticism_patronage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING CRITIC (TANGLED ROPE) — Benefits from patronage as income stability and audience platform. Constrained by dependence on patron preferences, algorithmic feed placement, and sustained subscriber growth. Mixed coordination (audience discovery) and extraction (audience capture and lock-in).
constraint_indexing:constraint_classification(independent_criticism_patronage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PATRONAGE PLATFORM (ROPE) — Patreon/Substack experience the constraint as pure coordination: connecting independent creators to sustainable audiences solves a market matching problem. Platform captures 5-15% transaction fee and data access; experiences extraction as flowing toward critics, not the platform. True beneficiary.
constraint_indexing:constraint_classification(independent_criticism_patronage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CRITICAL COMMONS ORGANIZATION (SCAFFOLD) — Nonprofit organizations (arts councils, journalism foundations, university presses) attempt to build sustainable infrastructure for diverse critical voices through grants, residencies, and institutional support. See patronage as temporary bridge pending more democratic funding models. Sunset: public funding for arts criticism stabilizes, reducing patronage dependence.
constraint_indexing:constraint_classification(independent_criticism_patronage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY MEDIA CRITICISM ARCHIVE (PITON) — Traditional institutional criticism (newspaper reviews, magazines, academic journals) persists as performative ritual: fewer readers, symbolic prestige, declining resources. Theater ratio high because institutional review processes (peer review, editorial gatekeeping) are maintained through inertia despite audience migration to independent creators. Function degraded but legitimacy theater intact.
constraint_indexing:constraint_classification(independent_criticism_patronage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — At full scope, the patronage system combines coordination function (matching supply/demand for niche criticism) with asymmetric extraction (platform rent, concentration of audience access, algorithmic gatekeeping). Not a natural law; not pure coordination. Hybrid mechanism with extractive overlay.
constraint_indexing:constraint_classification(independent_criticism_patronage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(independent_criticism_patronage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(independent_criticism_patronage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(independent_criticism_patronage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(independent_criticism_patronage, TR),
    TR >= 0.70.

:- end_tests(independent_criticism_patronage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The patronage system extracts through multiple mechanisms: (1) platform rent (5-15% of creator revenue), (2) algorithmic gatekeeping concentrating audiences among established critics, (3) patron preference bias rewarding creator compliance, (4) fragmentation of critical commons into incompatible microcultures. However, it also genuinely coordinates supply and demand for niche criticism, enabling work that would be economically invisible in traditional markets. The increase from 0.22 to 0.38 over 15 years reflects growing concentration and algorithmic lock-in. Suppression (0.42): Moderate. Barriers include patron-finding friction (high marketing overhead for new critics), income volatility (subscriber churn), algorithmic opacity, and the psychological pressure of parasocial subscriber relationships constraining critical independence. But suppression is not total — platforms provide transparent discovery mechanisms and critics retain full publishing control. Theater ratio (0.58): Moderate-high. Rising from 0.35 reflects increasing emphasis on patron engagement theater: behind-the-scenes content, subscriber-exclusive streams, tier-differentiated access, and algorithmic ranking based on engagement metrics rather than critical quality. The performative element is substantial but not dominant — content quality still drives subscriptions.
 *
 * PERSPECTIVAL GAP:
 *   The unpatronized critic sees pure extraction (Snare) — the patronage barrier is absolute. The emerging critic sees mixed coordination and extraction (Tangled Rope) — genuinely enabled by the platform but constrained by patron lock-in. The platform sees coordination (Rope) — they are solving a matching problem. The critical commons organization sees a temporary solution pending better institutional support (Scaffold with sunset). The legacy media institution sees its own degraded ritual (Piton) — still maintained through editorial prestige theater despite functional decline. The analytical observer sees the full tangled structure — coordination plus extraction, neither reducible to the other. This perspectival gap is diagnostic: if all agents perceived it as pure coordination (Rope), there would be no victims; if all saw it as pure extraction (Snare), the platform would not experience it as coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Independent critics derive low d (beneficiary + arbitrage exit) → experienced χ is dampened despite moderate base extraction. Unpatronized critics derive high d (victim + trapped) → experienced χ is amplified. Platform derives very low d (beneficiary + arbitrage) → experiences constraint as coordinating force with minimal extraction overhead. Legacy media institutions derive moderate d (constrained exit despite institutional power) — they are victims of audience migration but maintain escape through prestige and institutional affiliation. The piton classification for legacy media derives from theater gates: their review processes persist through ritual (theater ≥ 0.70 in their own system) despite functional decline. The scaffold perspective for critical commons organizations derives from constrained exit + organized power + sunset clause (public arts funding expansion as exit).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES THROUGH PERSPECTIVAL DECOMPOSITION: The mandatrophy question is 'Is this liberation from gatekeeping or just repackaged gatekeeping via algorithmic capture?' The answer is BOTH, depending on the agent's structural position. For unpatronized critics: pure extraction (Snare). For patronized critics: genuine coordination with extraction overlay (Tangled Rope). For platforms: pure coordination (Rope). No single type naturalizes the structure. The key diagnostic: if patronage truly liberated all critics equally, suppression would be near-zero and all perspectives would converge on Rope. The fact that suppression is 0.42 and perspectives diverge (Snare/Tangled Rope/Rope) confirms the hybrid structure. The increasing theater ratio (0.35→0.58) suggests gradual Goodhart drift: as patronage success becomes parasocial relationship success, the incentive structure tilts toward theater-maximizing content rather than criticism-maximizing content. This is classic extraction accumulation over the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    patron_preference_alignment,
    'To what extent does patronage funding bias critical discourse toward patron-pleasing analysis versus independent assessment?',
    'Content analysis comparing patronized critic output to pre-patronage work or control group academics; correlation between patron demographics and critic topic selection/tone',
    'If high bias: patronage functions as pure extraction mechanism masquerading as liberation (Snare from analytical). If low bias: coordination function dominates (Rope/Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patron_preference_alignment, empirical, 'Degree of patron preference bias in critical output').

omega_variable(
    barrier_to_entry_sustainability,
    'What minimum patronage threshold permits sustainable independent criticism versus subsistence struggle requiring supplementary income?',
    'Survey of patronized critics on hours worked, income stability, ability to reject commissions. Comparison across fields (film, music, visual art, literature). Tracking of critic churn and burnout rates.',
    'If threshold high (>$50K/year): patronage remains inaccessible to most potential critics; system is extraction from unpatronized (Snare). If threshold low (<$15K/year): system enables genuine economic diversity (Rope/Scaffold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(barrier_to_entry_sustainability, empirical, 'Minimum patronage for sustainable independent criticism').

omega_variable(
    algorithmic_discoverability_decay,
    'Does algorithmic ranking of patronized critics on Patreon/Substack reinforce existing popularity hierarchies, creating de facto gatekeeping equivalence to legacy media?',
    'Network analysis of creator discovery pathways; measurement of income concentration (Gini coefficient) across patronized critics; A/B testing of algorithmic ranking impact on new creator visibility',
    'If gatekeeping equivalent to legacy systems: patronage is false liberation (Piton theater). If discovery genuinely distributed: patronage enables market competition (Rope/Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_discoverability_decay, empirical, 'Whether algorithmic ranking reproduces legacy gatekeeping').

omega_variable(
    commons_fragmentation,
    'Does patronage model''s natural alignment with niche audiences fragment critical discourse into incompatible microcultures, preventing shared evaluation of cultural works?',
    'Cross-patronage analysis of critical consensus on major works; identification of systematic disagreement patterns by patron demographic; comparison to pre-patronage institutional consensus rates',
    'If fragmentation high: patronage extracts from discourse commons through specialization (Snare victim). If moderate: niche specialization is efficiency gain (Rope/Scaffold).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_fragmentation, conceptual, 'Whether patronage fragments critical commons into incommensurable microcultures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(independent_criticism_patronage, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crit_pat_tr_t0, independent_criticism_patronage, theater_ratio, 0, 0.35).
narrative_ontology:measurement(crit_pat_tr_t8, independent_criticism_patronage, theater_ratio, 8, 0.48).
narrative_ontology:measurement(crit_pat_tr_t15, independent_criticism_patronage, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(crit_pat_be_t0, independent_criticism_patronage, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(crit_pat_be_t8, independent_criticism_patronage, base_extractiveness, 8, 0.3).
narrative_ontology:measurement(crit_pat_be_t15, independent_criticism_patronage, base_extractiveness, 15, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(independent_criticism_patronage, resource_allocation).
narrative_ontology:affects_constraint(independent_criticism_patronage, legacy_media_institutional_collapse).
narrative_ontology:affects_constraint(independent_criticism_patronage, algorithmic_cultural_curation).
narrative_ontology:affects_constraint(independent_criticism_patronage, creator_economic_precarity).

% DUAL FORMULATION NOTE:
% The patronage system is downstream of the decline of advertising-supported institutional media (legacy_media_institutional_collapse) and upstream of algorithmic cultural curation. It is a different constraint because it has its own internal extractive mechanisms (platform rent, algorithmic gatekeeping, patron bias) distinct from the institutional media's theater-driven decline or the curation system's visibility mechanisms. Each story addresses a distinct ε value reflecting different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(independent_criticism_patronage, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
