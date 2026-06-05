% ============================================================================
% CONSTRAINT STORY: ulysses_chp16
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp16, []).

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
 *   constraint_id: ulysses_chp16
 *   human_readable: The Exhausted Coordination (Cabman's Shelter)
 *   domain: social/economic/linguistic
 *
 * SUMMARY:
 *   In Chapter 16 of James Joyce's Ulysses (2 June 1904, approximately 2:00
 *   AM), Leopold Bloom and Stephen Dedalus seek shelter from the Dublin night
 *   in a cabman's shelter near Butt Bridge. Both are exhausted: Bloom from
 *   his wandering and grief, Stephen from his nocturnal debauchery and
 *   existential disorientation. The shelter offers physical refuge, warmth,
 *   and human company—but at the cost of enduring a stagnant linguistic and
 *   narrative environment. The proprietor, other patrons, and the shelter
 *   itself enforce a discourse of mock-heroic tales, recycled narratives,
 *   tabloid phantasmagoria, and false epics. The wanderers desire genuine
 *   intellectual connection and meaningful conversation but find themselves
 *   trapped in a social space where authentic speech is suppressed by
 *   exhausted cultural forms. The constraint is the shelter itself understood
 *   as a mechanism that both coordinates refuge (providing safety, warmth,
 *   food, conversation) and extracts (suppressing authenticity, enforcing
 *   stale narrative, constraining the wanderers' agency to speak or think
 *   freshly). The theater_ratio rises over the encounter as the performative
 *   character of the shelter becomes more visible: the stories offered are
 *   increasingly hollow; the narratives increasingly echo without meaning.
 *   This is the classic structure of exhausted coordination—a system that
 *   once genuinely solved a problem (shelter, food, community) but now
 *   persists primarily through institutional inertia and the comfort of
 *   avoiding change.
 *
 * KEY AGENTS:
 *   - Leopold Bloom & Stephen Dedalus: Primary victims (powerless/trapped) — exhausted wanderers with no alternative refuge available in the Dublin night; bear the cost of narrative suppression and confinement
 *   - Shelter Proprietor: Primary beneficiary (institutional/arbitrage) — profits from consumption and attention; coordinates genuine refuge; experiences the constraint as successful rope
 *   - Other Shelter Patrons: Secondary agents (moderate/constrained) — reinforce the exhausted narrative environment; both benefit from and are trapped by the shelter's cultural forms
 *   - The Shelter's Narrative Environment: Systemic victim (analytical/analytical) — linguistic authenticity is suppressed; discourse is forced into mock-heroic and tabloid forms
 *   - The Dawn/External World: Structural boundary (powerful/mobile) — provides exit option; makes the shelter's extractiveness time-limited
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the shelter as exemplary of how exhausted systems persist through hybrid coordination-extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp16, 0.38).
domain_priors:suppression_score(ulysses_chp16, 0.52).
domain_priors:theater_ratio(ulysses_chp16, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp16, extractiveness, 0.38).
narrative_ontology:constraint_metric(ulysses_chp16, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ulysses_chp16, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp16, tangled_rope).
narrative_ontology:human_readable(ulysses_chp16, "The Exhausted Coordination (Cabman's Shelter)").
narrative_ontology:topic_domain(ulysses_chp16, "social/economic/linguistic").

domain_priors:requires_active_enforcement(ulysses_chp16).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp16, shelter_proprietor).
narrative_ontology:constraint_beneficiary(ulysses_chp16, narrative_momentum).
narrative_ontology:constraint_victim(ulysses_chp16, seekers_of_refuge).
narrative_ontology:constraint_victim(ulysses_chp16, linguistic_authenticity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXHAUSTED WANDERER (SNARE) — Bloom and Stephen are physically and psychologically depleted; the shelter offers the only available refuge in the Dublin night. Exit is not available; they must accept whatever the shelter provides. d≈0.92, f(d)≈1.40, σ=0.8 → χ≈0.46.
constraint_indexing:constraint_classification(ulysses_chp16, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SEEKER OF CONNECTION (TANGLED ROPE) — Both wanderers desire genuine contact and intellectual exchange (Bloom-Stephen communion); the shelter provides a space for this, but at cost of enduring false epics, stale narratives, and linguistic simulacra. They could leave but doing so means abandoning hope of refuge. d≈0.65, f(d)≈0.98, σ=0.8 → χ≈0.31.
constraint_indexing:constraint_classification(ulysses_chp16, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: SHELTER PROPRIETOR (ROPE) — Provides shelter (genuine coordination function) in exchange for consumption and narrative attention. The proprietor sees the constraint as successful coordination: bodies are sheltered, conversation fills the night, economic exchange occurs. d≈0.08, f(d)≈-0.08, σ=0.8 → χ≈-0.03. Net beneficiary through arbitrage.
constraint_indexing:constraint_classification(ulysses_chp16, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: NARRATIVE AUTHORITY (PITON) — The shelter's culture enforces stale narrative forms (mock-heroic tales, recycled epics, tabloid phantasmagoria). These forms persist through institutional inertia and the comfort of the familiar, even though they no longer carry authentic meaning. theater_ratio=0.68 indicates substantial performative content. The narrative authority sees the shelter as degraded — the myths are hollow but persist because they structure social interaction and require no mental effort. d≈0.25, f(d)≈0.15, σ=0.9 → χ≈0.09.
constraint_indexing:constraint_classification(ulysses_chp16, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: DAWN APPROACH (SCAFFOLD) — The shelter exists as a way-station only. Both Bloom and Stephen know their stay is temporary; dawn brings exit and resumption of daytime life. The constraint has built-in sunset: the shelter's extractiveness declines as light approaches. d≈0.35, f(d)≈0.30, σ=0.8 → χ≈0.08. Low effective extraction because the time horizon is explicitly limited.
constraint_indexing:constraint_classification(ulysses_chp16, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the shelter exemplifies how exhausted human systems persist. The coordination function (refuge, warmth, conversation) is real and necessary. The extraction mechanism (forced consumption of stale narrative, suppression of authentic speech, confinement to false epics) is equally real. The constraint is neither pure coordination nor pure extraction but a hybrid where each sustains the other. ε=0.38 reflects moderate extraction; suppression=0.52 reflects significant barriers to authentic discourse. d≈0.68, f(d)≈1.10, σ=1.0 → χ≈0.42.
constraint_indexing:constraint_classification(ulysses_chp16, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp16_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp16, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp16, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp16, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp16_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The shelter's proprietor and culture extract value from the wanderers through forced consumption (food, drink, attention) and suppression of authentic discourse. However, the extraction is not severe (ε would be >0.46 for a snare) because the shelter also provides genuine coordination: refuge, warmth, food, human presence. The wanderers do choose to stay, accepting the cost for the benefit. The extraction mechanism is real but balanced by legitimate coordination. Suppression (0.52): Moderate-high. Significant barriers exist to authentic speech: the cultural norms of the shelter enforce mock-heroic narrative, recycled epics, and tabloid phantasmagoria. The proprietor and patrons actively suppress deviation from these forms. Discourse is channeled into familiar, exhausted patterns. However, suppression is not total (>0.60) because the wanderers can and do speak, even if constrained. Theater ratio (0.68): High. A substantial portion of the shelter's activity is performative: the stories told are increasingly hollow, the epics mock their own epic pretensions, the conversation serves to fill time rather than generate meaning. The proprietor maintains the shelter's narrative ritual more from inertia than from belief in its power. However, theater is not dominant (>0.70) because the shelter does provide real refuge and real (if stale) conversation.
 *
 * PERSPECTIVAL GAP:
 *   The wanderers see the shelter as primarily extractive (snare), enduring it only because they have no alternative and hope for genuine connection. The proprietor sees it as primarily coordinating (rope), a successful business. The narrative environment sees itself as degraded (piton), maintaining hollow forms through institutional habit. The dawn/external boundary sees the shelter as temporary (scaffold), with built-in sunset. The analytical observer sees the fundamental hybrid (tangled_rope), where coordination and extraction are structurally inseparable—the shelter exists precisely because it offers refuge, and it extracts precisely through that same refuge by confining the wanderers to exhausted narrative forms. The perspectival gap is not a disagreement about facts but a difference in structural position: each perspective reveals a real feature of the constraint, and no single perspective captures the whole.
 *
 * DIRECTIONALITY LOGIC:
 *   Wanderers (Bloom & Stephen): Victim + trapped → d≈0.92, f(d)≈1.40. Severe extraction from the perspective of those with no exit option. Proprietor: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; can exit the shelter (it is their business) or arbitrage their position (raise prices, change clientele). Narrative environment: Victim + analytical → d≈0.68, f(d)≈1.10. Linguistic authenticity is suppressed; the narrative forms are forced into mock-heroic and tabloid patterns. Other patrons: Moderate + constrained → d≈0.65, f(d)≈0.98. Mixed experience: they are both served (refuge) and constrained (forced discourse). Dawn/external world: Powerful + mobile → d≈0.12, f(d)≈-0.02. Provides exit option; reduces the shelter's effective extractiveness by making confinement temporary. Analytical observer: Analytical + analytical → d≈0.68, f(d)≈1.10. Sees the constraint's hybrid nature; does not experience extraction directly but recognizes its structural presence.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the shelter is genuinely both rope and snare depending on structural position and time horizon. For the proprietor (institutional/arbitrage), it is rope: a coordination mechanism that solves the problem of nocturnal shelter and generates profit. For the wanderers (powerless/trapped), it is snare: an extraction mechanism that confines them to exhausted narrative while offering no exit. For the narrative environment (analytical/analytical), it is piton: the forms persist through inertial force, not because they serve a real function. For the dawn approach (organized/mobile), it is scaffold: the constraint has built-in sunset; sunrise makes the shelter accessible again only to those who choose to return. The constraint's classification as tangled_rope (hybrid coordination-extraction) is not a failure to classify but a recognition that the same institutional mechanism (the shelter) serves genuine coordination for some agents and genuine extraction for others. The mandatrophy is resolved by observing that extractiveness (0.38) and suppression (0.52) both exceed the rope threshold, but both fall below the snare threshold (ε≥0.46, suppression≥0.60). The constraint is intrinsically hybrid: it cannot be reduced to pure coordination or pure extraction without losing the structural truth of the situation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_threshold,
    'At what point does the shelter''s narrative environment transition from protective (allowing respite through familiar forms) to extractive (suppressing authentic voice)?',
    'Close linguistic analysis of the dialogue; comparison of Bloom-Stephen discourse inside vs outside the shelter; measurement of narrative dissonance between offered stories and actual experience',
    'If threshold is crossed early: shelter is primarily snare. If threshold remains uncrossed: shelter is primarily rope with theatrical elements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_threshold, conceptual, 'Threshold between protective narrative and extractive suppression').

omega_variable(
    refuge_necessity_binding,
    'Do the wanderers choose to remain in the shelter because it is genuinely the only available refuge, or because they fear re-engaging with the external world?',
    'Analysis of exit options available in 1904 Dublin night; psychological profile of Bloom''s and Stephen''s fear/fatigue states; alternative shelter availability',
    'If genuinely trapped: exit=trapped, snare classification confirmed. If psychologically trapped: exit=constrained, tangled_rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(refuge_necessity_binding, empirical, 'Whether remaining is structural necessity or psychological choice').

omega_variable(
    epical_degradation_cause,
    'Is the shelter''s exhausted narrative environment a result of accumulated institutional wear (piton), or is it an active mechanism for controlling patron behavior (snare)?',
    'Historical trace of shelter narrative traditions; comparison with other shelters of the period; identification of whether proprietor actively reinforces stale forms or passively tolerates them',
    'If piton (inertial): theater_ratio high but suppression moderate. If snare (active control): both theater_ratio and suppression remain high, d for proprietor increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epical_degradation_cause, empirical, 'Whether narrative exhaustion is institutional decay or active control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp16, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eumaeus_tr_t0, ulysses_chp16, theater_ratio, 0, 0.52).
narrative_ontology:measurement(eumaeus_tr_t3, ulysses_chp16, theater_ratio, 3, 0.62).
narrative_ontology:measurement(eumaeus_tr_t6, ulysses_chp16, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(eumaeus_be_t0, ulysses_chp16, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(eumaeus_be_t3, ulysses_chp16, base_extractiveness, 3, 0.33).
narrative_ontology:measurement(eumaeus_be_t6, ulysses_chp16, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp16, resource_allocation).
narrative_ontology:affects_constraint(ulysses_chp16, interior_monologue_suppression).
narrative_ontology:affects_constraint(ulysses_chp16, nomadic_exhaustion_accumulation).

% DUAL FORMULATION NOTE:
% The cabman's shelter constraint is part of the Ulysses constraint family. It is downstream of the accumulating exhaustion that drives Bloom and Stephen into the shelter (nomadic_exhaustion_accumulation, ε≈0.35) and upstream of the interior monologue dynamics that emerge in the shelter's linguistic environment (interior_monologue_suppression, ε≈0.42). The shelter itself (ε=0.38) represents the midpoint where exhaustion becomes confinement and confinement becomes linguistic constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
