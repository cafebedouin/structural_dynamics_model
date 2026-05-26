% ============================================================================
% CONSTRAINT STORY: mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mourning_practice_reading, []).

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
 *   constraint_id: mourning_practice_reading
 *   human_readable: Mourning Practice: Ritual Preservation of Symbolic Continuity and Collective Identity
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Mourning practices across religious and cultural communities represent a
 *   fundamental constraint on how collectives maintain symbolic continuity
 *   and identity formation across generational discontinuity created by
 *   death. This constraint story models mourning ritual as a voluntary
 *   coordination mechanism — one specific reading of a contested kernel about
 *   how catastrophe memory is preserved. In this reading, mourning ritual
 *   solves the genuine problem of how a community can collectively process
 *   loss while maintaining the symbolic and relational structures that
 *   constitute group identity. The constraint exhibits pure coordination
 *   characteristics: beneficiaries are the in-group cohesion and
 *   intergenerational identity transmission; there are no victim sets because
 *   participation is largely voluntary and low-suppression; the
 *   extractiveness is low (0.18) because the coordination function and the
 *   symbolic form are aligned. Theater is moderate (0.35) and increasing
 *   slightly over the 40-year interval, reflecting gradual formalization and
 *   potential decoupling of ritual form from functional grief processing as
 *   communities shift across geographic and institutional contexts. This
 *   reading is distinct from sibling readings that would emphasize either the
 *   survival-competence function (knowledge transfer about collective
 *   resilience) or the degradation of former survival coordination into
 *   performative tradition.
 *
 * KEY AGENTS:
 *   - Mourning Community Members: Primary agents (moderate/mobile) — voluntarily participate in collective grief coordination and identity reformation
 *   - Religious Community Leadership: Institutional guardians (organized/constrained) — maintain and transmit ritual forms across generations; benefit from role as tradition-keepers
 *   - Diasporic Identity Networks: Institutional preservers (powerful/arbitrage) — adapt and transmit practices across geographic displacement; benefit from ritual as identity anchor
 *   - Grief Processing Communities: Functional coordinators (organized/constrained) — integrate ritual into broader grief support systems; see practice as time-limited scaffold
 *   - Ritual Archives/Heritage Institutions: Performative maintainers (institutional/arbitrage) — preserve practice forms potentially separated from original function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent cultural practices as inevitable or immutable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mourning_practice_reading, 0.18).
domain_priors:suppression_score(mourning_practice_reading, 0.12).
domain_priors:theater_ratio(mourning_practice_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mourning_practice_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(mourning_practice_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(mourning_practice_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mourning_practice_reading, rope).
narrative_ontology:human_readable(mourning_practice_reading, "Mourning Practice: Ritual Preservation of Symbolic Continuity and Collective Identity").
narrative_ontology:topic_domain(mourning_practice_reading, "religious_studies/collective_memory/ritual_practice").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mourning_practice_reading, in_group_cohesion).
narrative_ontology:constraint_beneficiary(mourning_practice_reading, intergenerational_identity_transmission).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOURNING COMMUNITY MEMBER (ROPE) — Participates in collective mourning practice as voluntary coordinator of grief and identity. Low suppression; exit is available at modest social cost, not institutional barrier. The constraint solves a genuine coordination problem: how to collectively process loss while maintaining group identity continuity. Experiences the practice as meaningful coordination, not extraction.
constraint_indexing:constraint_classification(mourning_practice_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: RELIGIOUS COMMUNITY LEADERSHIP (ROPE) — Maintains and standardizes mourning rituals across generations. Constrained by intergenerational obligation to preserve symbolic forms, but benefits from the coordination structure itself — leadership's authority and role are constituted through ritual guardianship. No asymmetric extraction; beneficiary and victim sets overlap. The leadership experiences low theater because ritual's function (identity continuity) and form (symbolic practice) are aligned.
constraint_indexing:constraint_classification(mourning_practice_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DIASPORIC IDENTITY MAINTENANCE AGENT (ROPE) — Institutions and networks that preserve mourning practices across diaspora (émigré communities, transnational religious networks, cultural heritage organizations). Mobile/arbitrage position: can adopt, adapt, or drop practices based on diaspora context. Benefits from ritual's function (maintaining symbolic continuity when geographic continuity is severed). Low extraction because the constraint's value to this agent is genuinely realized through participation.
constraint_indexing:constraint_classification(mourning_practice_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RITUAL ARCHIVE / PERFORMANCE WITHOUT FUNCTION (PITON) — From a civilizational view, mourning practices sometimes persist as performed forms after their original operational function has degraded. The ritual is maintained for symbolic continuity alone, separated from any genuine grief-processing or identity-formation work. Theater is high because the performative content exceeds the functional content — agents go through the motions to preserve 'our traditions' without the ritual addressing actual collective needs. This is the perspective from which ritual appears as pure theater.
constraint_indexing:constraint_classification(mourning_practice_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GRIEF INTERVENTION COORDINATOR (SCAFFOLD) — Mental health, community support, or secular institutional actors who integrate mourning practices as temporary structural scaffolding during acute grief phases. View the practice as time-limited coordination support: its value is high during the immediate/biographical grief window, declining as the community moves into post-loss identity reformation. Not a permanent constraint, but a structured support mechanism with built-in sunset — the ritual's intensity and necessity both decline as the community adapts.
constraint_indexing:constraint_classification(mourning_practice_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Anthropological and cognitive science perspective: mourning rituals are natural law-level features of how human collectives process collective loss and maintain identity across generational discontinuity. Some ritual marker of death and identity reformation is inevitable in any human community — it emerges from the cognitive and social requirements of grief processing. However, this mountain classification is vulnerable to false-summit detection: the specific form and intensity of mourning practices are contingent cultural constructions, not immutable laws.
constraint_indexing:constraint_classification(mourning_practice_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mourning_practice_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(mourning_practice_reading, TR),
    TR >= 0.70.

:- end_tests(mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. Mourning ritual in this reading is primarily coordinative — it solves the collective action problem of grief processing and identity reformation without significant asymmetric extraction. The original research group captures no disproportionate benefit; costs and benefits are distributed across the participating community. The low value reflects that this reading emphasizes the voluntary, non-coercive nature of the practice — participants engage because the coordination genuinely serves their needs (processing loss, maintaining identity), not because they are trapped or suppressed. Suppression (0.12): Very low. While social expectations may gently encourage participation, alternative grief expressions are generally tolerated in most contemporary contexts. Exit costs are primarily relational (risk of being perceived as rejecting group identity) rather than material or institutional. Theater ratio (0.35): Low-to-moderate, increasing. At the origin (T=0), the practice is highly functional — ritual form and grief function are tightly aligned, participants experience the ritual as directly addressing their need to process loss and maintain collective identity. Over 40 years, theater increases slightly as the practice potentially becomes more formalized and institutional, risk of decoupling between symbolic form and functional content. The increase reflects gradual shifts in how ritual is maintained (by heritage institutions, diaspora networks) vs how it is experienced (by active grievers).
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap is between the functional (Rope) perspective of active mourning communities and the performative (Piton) perspective from which ritual is separated from functional grief processing. A community for whom mourning ritual directly processes collective loss and reforms identity experiences low extraction and genuine coordination (Rope). A heritage institution preserving ritual forms as tradition-markers — decoupled from active grief work — experiences higher theater and potential performativity (Piton). The analytical observer risks naturalizing the practice as immutable (Mountain) when it is actually a contingent institutional arrangement. The grief intervention coordinator sees it as temporary structural support (Scaffold), declining in necessity as the community moves past acute grief. These gaps emerge from different structural relationships to the practice's function: active participants benefit directly from coordination; heritage preservers benefit from the preservation role; analytical observers risk seeing inevitability; interventionists see temporary scaffolding.
 *
 * DIRECTIONALITY LOGIC:
 *   In this reading, directionality is low across all perspectives because the constraint is primarily coordinative, not extractive. Community members experience d ≈ 0.40-0.50 (both cost and benefit of participation, no systematic asymmetry). Leadership experiences d ≈ 0.35 (slight beneficiary position — authority and role are constituted through ritual guardianship — but constrained by intergenerational obligation to preserve form). Diaspora networks experience d ≈ 0.25 (net beneficiaries — ritual serves their identity preservation needs — with high mobility). In each case, the derived directionality is moderate and symmetric, producing low effective extraction chi. If either suppression or asymmetric benefits were higher, directionality would shift upward and classification would move toward Tangled Rope or Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by clearly specifying which observables and beneficiary structures it uses. The constraint is modeled as low-extraction coordination (Rope) specifically when: (a) ritual content addresses genuine collective grief processing needs, (b) participation is low-suppression and voluntary, (c) beneficiaries are shared across the community, and (d) there are no victim sets bearing disproportionate costs. If any of these conditions fails (suppression increases, function degrades, benefits asymmetrically distribute, victims emerge), the reading becomes inaccurate and alternative readings (survival_competence or hybrid_atrophy) become structurally appropriate. The mandatrophy is not 'what is the true nature of mourning ritual?' but 'what reading of the catastrophe-memory kernel is most accurate given the observable context?' This reading assumes the context where ritual's symbolic and functional aspects are aligned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_function_persistence,
    'Does the mourning practice continue to genuinely coordinate grief processing and identity formation, or has it become performative theater maintained primarily for tradition preservation?',
    'Ethnographic observation of ritual participation and post-ritual outcomes; interviews with participants about functional vs performative experience; measurement of grief-processing efficacy and identity-reformation outcomes across ritual-participating vs non-participating subpopulations',
    'If genuine coordination: classification remains Rope across most perspectives. If primarily performative: piton classification spreads, theater_ratio rises above 0.50, extractiveness may shift toward Snare if suppression of alternatives (non-ritual grief processes) is high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ritual_function_persistence, empirical, 'Whether mourning ritual functions as genuine grief coordination or performative tradition').

omega_variable(
    suppression_of_alternative_grief_expression,
    'To what degree does the ritualized grief framework suppress alternative or non-traditional forms of grief expression, processing, or identity reformation?',
    'Ethnographic documentation of community responses to non-ritual grief expression; interviews with grief practitioners about pressure to conform to ritual forms; measurement of institutional/social costs of declining to participate in prescribed mourning practices',
    'If suppression is low (alternative expressions are accepted/valued): constraint remains Rope (coordination without coercion). If suppression is high (alternative expressions are stigmatized/prohibited): constraint shifts toward Tangled Rope or Snare, depending on beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternative_grief_expression, empirical, 'Suppression of alternative grief expression modalities').

omega_variable(
    kernel_reading_boundary,
    'What structural signal distinguishes this reading (mourning practice as voluntary coordination) from the sibling reading ''survival_competence_reading'' (mourning practice as pragmatic knowledge transfer) and ''hybrid_atrophy_reading'' (mourning practice as degraded survival coordination)?',
    'Analysis of whether ritual content transfers substantive survival knowledge (e.g., economic practices, resource management, social cooperation patterns) or primarily symbolic/emotional content. Measurement of whether participation in ritual correlates with improved collective survival outcomes vs solely improved identity/meaning coherence.',
    'If this reading (symbolic continuity) is correct: ε ≈ 0.18 (low extraction, pure coordination). If survival_competence_reading is correct: ε ≈ 0.25-0.35 (ritual contains functional knowledge transfer). If hybrid_atrophy_reading is correct: ε ≈ 0.50+ (former survival coordination degraded into theater). Each reading produces distinct beneficiary/victim structures and classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Boundary between mourning-as-symbolic-coordination and mourning-as-survival-knowledge-transfer').

omega_variable(
    voluntary_participation_paradox,
    'If mourning practice is genuinely low-suppression and low-extraction (Rope), why does participation persist across generations despite high mobility and low material barriers to exit?',
    'Comparative analysis of participation persistence across diaspora communities with varying degrees of geographic/material separation from origin practice; measurement of participation rates across agents with high vs low identity fusion with the practice; identification of whether persistence reflects genuine coordination value vs internalized identity lock',
    'If persistence reflects genuine coordination value: validates Rope classification. If persistence reflects identity lock (agents cannot imagine exit despite low material barriers): classification from identity-locked perspectives shifts to Rope (biographical), suggesting the constraint is more constraining than the base properties indicate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_participation_paradox, empirical, 'Why low-suppression ritual maintains participation across generations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mourning_practice_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mour_tr_t0, mourning_practice_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mour_tr_t20, mourning_practice_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(mour_tr_t40, mourning_practice_reading, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(mour_be_t0, mourning_practice_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(mour_be_t20, mourning_practice_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement(mour_be_t40, mourning_practice_reading, base_extractiveness, 40, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mourning_practice_reading, attachment_coordination).
narrative_ontology:affects_constraint(mourning_practice_reading, catastrophe_memory_survival_competence_reading).
narrative_ontology:affects_constraint(mourning_practice_reading, catastrophe_memory_hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'catastrophe_memory_preservation.' Sibling readings (survival_competence_reading and hybrid_atrophy_reading) model alternative structural interpretations of mourning practice, each with distinct ε values and classification profiles. All three readings share the same kernel (catastrophe memory preservation) but decompose it into different constraint stories based on which observable functions (symbolic continuity, survival knowledge transfer, performative degradation) are primary. Link the stories via network.affects_constraints to indicate kernel family relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
