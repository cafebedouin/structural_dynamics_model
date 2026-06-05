% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__mourning_practice_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Ritual as Mourning Practice: Symbolic Continuity and Collective Identity
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint documents ritual mourning practice as a coordinate
 *   mechanism for preserving catastrophe memory and collective identity
 *   without requiring operational transfer of threat-recognition capacity. In
 *   this reading, the ritual functions primarily through symbolic continuity
 *   — structured collective action that marks the shared past and anchors
 *   intergenerational identity — rather than through preserving
 *   survival-relevant knowledge or operational threat response. Participants
 *   experience the constraint as rope: a genuine coordination mechanism that
 *   solves the problem of how a dispersed or succeeding generation maintains
 *   psychological and identity continuity with the catastrophe. The
 *   constraint exhibits low extractiveness (0.22) because participation is
 *   voluntary, beneficiaries are diffuse (the participating community), and
 *   no victim set exists — unlike survival-competence reading (where ritual
 *   failure costs operational risk) or hybrid-atrophy reading (where
 *   institutional maintenance extracts institutional resources), this reading
 *   emphasizes the coordination function and identity benefits without
 *   positing extraction. The theater-ratio increase over time (0.28 → 0.42)
 *   reflects institutional atrophy: early-period ritual has higher
 *   operational content (direct memory transmission, trauma processing in
 *   intergenerational presence), while later periods see increasing
 *   performative maintenance (institution keeping the form alive even as
 *   experiential intensity declines).
 *
 * KEY AGENTS:
 *   - Practicing Mourners: Individual ritual participants (moderate/mobile) — experience constraint as identity coordination and grief expression; benefit from shared frame without extraction cost
 *   - Ritual-Transmitting Community: Religious, cultural, or community leaders maintaining ritual transmission (organized/constrained) — bear coordination cost of maintaining infrastructure but benefit through institutional identity continuity
 *   - Memory Institutions: Museums, archives, documentation projects (institutional/arbitrage) — leverage ritual's cultural authority for their own institutional legitimacy; pure coordination function
 *   - Catastrophe Survivors/Direct Memory Keepers: Intergenerational transmission nodes (powerful/mobile, declining over time) — may experience constraint differently as transmission conduit; bear witness role
 *   - Diaspora Communities: Geographically dispersed groups maintaining ritual (organized/constrained in regional scope, mobile in biographical exit) — experience ritual as essential identity anchor under conditions of cultural pressure
 *   - Institutional Ritual Framework: Religious or civic institutions with regulatory or inertial stake in ritual continuation (institutional/constrained) — increasingly see ritual as theater (piton perspective) rather than live coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.18).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Ritual as Mourning Practice: Symbolic Continuity and Collective Identity").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, '589710d3-a944-4754-b90d-fb8c28a04733').
narrative_ontology:cs_kernel_codification('589710d3-a944-4754-b90d-fb8c28a04733', distributed).
narrative_ontology:cs_authority_grounding('589710d3-a944-4754-b90d-fb8c28a04733', practice).
narrative_ontology:cs_interpretation_layer_present('589710d3-a944-4754-b90d-fb8c28a04733').
narrative_ontology:cs_reading_relation('589710d3-a944-4754-b90d-fb8c28a04733', catastrophe_memory_preservation__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('589710d3-a944-4754-b90d-fb8c28a04733', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_axiom('589710d3-a944-4754-b90d-fb8c28a04733', foundational, identity_continuity_primary_function).
narrative_ontology:cs_axiom_status(identity_continuity_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('589710d3-a944-4754-b90d-fb8c28a04733', identity_continuity_primary_function, conventional).
narrative_ontology:cs_axiom('589710d3-a944-4754-b90d-fb8c28a04733', foundational, voluntary_participation_model).
narrative_ontology:cs_axiom_status(voluntary_participation_model, holdable).
narrative_ontology:cs_axiom_grounding('589710d3-a944-4754-b90d-fb8c28a04733', voluntary_participation_model, deontological).
narrative_ontology:cs_reference_frame('589710d3-a944-4754-b90d-fb8c28a04733', embodied_intergenerational_mourning).
narrative_ontology:cs_created_at('589710d3-a944-4754-b90d-fb8c28a04733', '2026-02-26T18:42:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, participating_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, intergenerational_identity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTICING MOURNER (ROPE) — Individual participant in ritual mourning practice. Has full mobility (can choose whether to participate) and experiences the constraint as pure coordination: the ritual solves the problem of expressing collective grief and maintaining identity connection to the catastrophe. No suppression — participation is opt-in. Benefits from the shared frame for meaning-making without bearing extraction cost.
constraint_indexing:constraint_classification(catastrophe_memory_preservation__mourning_practice_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: RITUAL-TRANSMITTING COMMUNITY (ROPE) — Institutional actors (community leaders, religious authorities, cultural organizations) that maintain and transmit the ritual across generations. Experiences constraint as coordination with moderate cost: maintaining ritual infrastructure (timing, space, education of new practitioners) requires organized effort. Exit is constrained by community expectation and identity commitment, but the coordination benefit (collective identity preservation) is genuine and asymmetrically distributed to this agent as transmitter. Low extraction because the community voluntarily sustains transmission as essential to its own reproduction.
constraint_indexing:constraint_classification(catastrophe_memory_preservation__mourning_practice_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MEMORY INSTITUTION (ROPE) — Museums, archives, documentation projects that preserve catastrophe memory. Experiences constraint as enabling coordination: ritual documentation and public commemoration practices generate legitimacy for the institution's preservation work and secure funding/political support. Pure coordination function — no extraction. Net beneficiary through arbitrage (can leverage ritual's cultural authority for institutional authority).
constraint_indexing:constraint_classification(catastrophe_memory_preservation__mourning_practice_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / SUNSET POSSIBILITY (SCAFFOLD) — From a civilizational perspective, ritual mourning practice is a temporary coordination mechanism that may atrophy as embodied intergenerational transmission breaks down through diaspora, assimilation, or material changes to community structure. The ritual's function (preserving catastrophe memory as identity anchor) may be served by alternative mechanisms (digital archives, state commemoration, memorial infrastructure). If alternative mechanisms mature, ritual participation could sunset — the coordination problem it solves might be solved differently. This perspective sees a low-extraction coordination mechanism with a possible long-term sunset trajectory.
constraint_indexing:constraint_classification(catastrophe_memory_preservation__mourning_practice_reading, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL RITUAL FRAMEWORK (PITON) — Religious or civic institutions that maintain ritual through formal structure (liturgical calendar, official commemoration protocols, institutional memory). At the institutional biographical level, these frameworks increasingly perform theater: the official ritual persists through regulatory mandate or organizational inertia, but actual participation and emotional intensity have declined. The institution continues the ritual because it is 'what we do,' not because participants experience it as solving a live coordination problem. Theater ratio at this perspective is higher (0.62+) — the institutional apparatus maintains the form while the experiential content attenuates.
constraint_indexing:constraint_classification(catastrophe_memory_preservation__mourning_practice_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: CIVILIZATIONAL NATURAL LAW VIEW (MOUNTAIN) — At the universal/civilizational scale, ritual mourning practice might appear as an immutable feature of human culture: all known societies develop commemorative rituals for catastrophe, and the structure of ritual (symbolic action, temporal regularity, collective participation) appears as a natural-law response to the human need to process trauma. This perspective risks naturalizing what is actually a historically contingent practice. The engine's false summit detector will flag this if beneficiary analysis reveals that the 'naturalness' of ritual serves specific agents' interest in its continuation.
constraint_indexing:constraint_classification(catastrophe_memory_preservation__mourning_practice_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low-to-moderate. The mourning_practice reading posits no significant extraction because (1) participation is genuinely voluntary — individuals can lapse without material sanctions, (2) beneficiaries are diffuse (the participating community) and include participants themselves, (3) no victim set exists — the constraint does not distribute costs asymmetrically. The extractiveness arises from two sources: identity-based exit costs (participants feel that exiting damages their community belonging and identity continuity, creating psychological suppression of the exit option) and organizational labor (ritual transmitters invest effort in maintaining infrastructure). Neither constitutes predatory extraction; both are coordination costs. Suppression (0.18): Low. Participation is not formally coerced, legal barriers to exit do not exist, and economic dependency on the ritual is absent. The suppression that does exist is psychological/identity-based: the shared catastrophe narrative and identity commitment create internal pressure to participate. However, this is not the high-suppression signature of a snare — many individuals successfully lapse from ritual practice without material harm, suggesting the suppression is soft and identity-modulated rather than structural. Theater ratio (0.35 initial): Moderate. The ritual has both operational and performative dimensions. Operational content (49%): structured transmission of catastrophe narrative, collective grief processing, identity continuity marking. Performative content (51%): the symbolic/expressive dimension, the fact that participants know the ritual does not prevent future catastrophe but engage for meaning-making. The increase to 0.42 by t=50 reflects institutional lifecycle: as communities mature and direct memory fades, the ratio shifts toward performative maintenance (institution keeps form alive through regulatory/inertial momentum) and away from operational transmission.
 *
 * PERSPECTIVAL GAP:
 *   The mourning_practice reading produces perspectival coherence across low and moderate power contexts (mourner, ritual-transmitting community) where all perspectives classify as rope — participants experience genuine coordination with low extraction. The gap emerges at institutional and analytical scales. The institutional ritual framework (piton perspective) sees increasingly performative theater as intergenerational transmission breaks down and institutional maintenance relies on organizational inertia. The analytical sunset view (scaffold perspective) sees the constraint as temporary — alternative mechanisms (digital archives, state memorials) may eventually substitute for embodied ritual transmission. The civilizational natural-law view (mountain perspective) risks naturalizing the practice as immutable human response to catastrophe, which the engine's false-summit detector flags as problematic — the routine institutional beneficiaries (memory institutions, religious establishments) have incentive to maintain the 'natural law' framing. The perspectival gap signals that mourning_practice reading is epistemically stable at community and biographical scales but faces challenges at institutional and civilizational scales where atrophy and sunset dynamics become visible.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) derives from the agent's structural relationship to the constraint. Practicing mourners are beneficiaries with mobile exit (low d, low/negative effective extraction). Ritual-transmitting communities are beneficiaries with constrained exit (higher d due to community obligation, but still net beneficiaries — not victims). Memory institutions are pure beneficiaries with arbitrage exit (very low d). The analytical observers (scaffold and mountain perspectives) do not appear in the beneficiary/victim structure; their d values derive from canonical fallback (organizational/analytical power atoms). Suppression does not scale with scope — it remains 0.18 across all perspectives as a raw structural property. Effective extraction (chi) scales with f(d): lower d produces lower chi, confirming that this constraint experiences as rope across most perspectives. The piton perspective has higher theater content but not higher extraction — piton classification derives from the theater gate, not from experienced extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by distinguishing ritual's coordination function (mourning-practice reading) from its alternative functional claims (survival-competence reading, hybrid-atrophy reading). If ritual is primarily mourning-practice (identity continuity without operational transfer), it is rope — genuine coordination with low extraction, low suppression, and diffuse beneficiaries. If ritual preserves operational threat-recognition capacity, the same institutional structure becomes tangled_rope (mixed coordination + operational function). If ritual has atrophied from one to the other, different time periods exhibit different constraint types. The reading's stability depends on empirical resolution of the omegas: Does ritual transmission actually preserve operational capacity (falsifies mourning_practice)? Does it exhibit intergenerational breakdown (supports piton/atrophy dynamics)? The analytical observer's mountain classification is a false summit — ritual mourning practice is not a natural law even if all human societies develop rituals. The 'naturalness' may reflect institutional incentives (religious establishments, memory institutions, state commemoration authorities all benefit from ritual legitimacy) rather than immutable human universals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_kernel_vs_siblings,
    'Is ritual mourning practice a stable coordination mechanism (this reading), an atrophied survival practice (hybrid_atrophy_reading), or a still-functional threat-recognition system (survival_competence_reading)?',
    'Ethnographic study of ritual transmission: Do new participants acquire operational threat-recognition capacity (survival_competence evidence)? Is transmission breaking down generationally (atrophy evidence)? Or is ritual functioning primarily as identity/memory marker (mourning_practice evidence)? Cross-community comparison of ritual content and participant demographics.',
    'If survival_competence: constraint type shifts to tangled_rope (mixed coordination + operational function). If hybrid_atrophy: constraint exhibits piton dynamics (performative theater increasing over time). This reading (mourning_practice) stands if ritual transmits identity and memory frames but not operational threat recognition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_kernel_vs_siblings, empirical, 'Which reading of ritual function is epistemically primary').

omega_variable(
    intergenerational_transmission_mechanism,
    'Does ritual mourning practice preserve catastrophe memory effectively across generational breaks (diaspora, assimilation, institutional decline)?',
    'Longitudinal study of ritual participation and memory retention across diaspora generations. Measure: Do second/third-generation diaspora participants retain narrative understanding of the catastrophe? Does ritual alone maintain collective identity without alternative institutional reinforcement (state education, family narrative, media)? Can the ritual survive without at least one ''keeper'' generation with direct memory?',
    'If transmission is robust: rope classification confirmed across generational time. If transmission requires direct-memory-keeper presence: constraint is structurally more fragile; may shift toward piton (inertial, dependent on institutional enforcement rather than living practice).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_transmission_mechanism, empirical, 'Robustness of intergenerational ritual transmission').

omega_variable(
    voluntary_participation_vs_internalized_obligation,
    'Is ritual participation genuinely mobile (exit cost = purely psychological identity loss, not material sanctions) or identity_locked (participants cannot exit without self-concept dissolution)?',
    'Interview study: Can participants articulate reasons for exiting without contradiction? Do lapsed participants maintain identity connection to the catastrophe/community despite non-participation? Or do they experience non-participation as identity dissolution? Post-exit trajectory analysis: do lapsed participants seek alternative markers of community membership?',
    'If mobile: suppression metric is accurate (0.18). If identity_locked: exit option is mislabeled; constraint may exhibit higher effective extraction from perspective of identity-fused participants; suppression metric may underestimate psychological binding cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_participation_vs_internalized_obligation, empirical, 'Whether participation exit is structurally mobile or identity-locked').

omega_variable(
    catastrophe_narrative_stability,
    'Does the ritual preserve stable catastrophe narrative or does ritual participation allow narrative contestation and revision?',
    'Content analysis of ritual over time: Do permitted narrative frames narrow (ritualizing toward monolithic memory) or allow divergence (diaspora communities develop alternative ritual forms)? Study of ritual innovation during transmission: when communities adapt ritual (due to diaspora, cultural contact, or material conditions), does adaptation preserve essential catastrophe narrative or allow divergence?',
    'If narrative stability is enforced: constraint exhibits suppression of alternative memories; this reading may be misclassified as pure rope when it contains identity-based suppression of competing narratives. If narrative divergence is allowed: mourning_practice reading confirmed; different communities develop different rituals while maintaining identity continuity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_narrative_stability, empirical, 'Whether ritual enforces narrative stability or permits contestation').

omega_variable(
    alternative_mechanism_substitution,
    'Can digital archive access, state memorial infrastructure, or secular commemoration rituals substitute for intergenerational mourning practice?',
    'Natural experiment: Communities that adopt digital archiving + institutional commemoration without traditional ritual. Measure identity retention and catastrophe memory preservation. Compare to communities maintaining traditional ritual without alternative mechanisms.',
    'If substitution works: scaffold perspective confirmed — mourning practice is temporary coordination mechanism with genuine sunset trajectory. If substitution fails: mourning practice solves a coordination problem that alternatives cannot replicate (embodied intergenerational transmission of identity frames); constraint is more stable than scaffold classification suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_mechanism_substitution, empirical, 'Substitutability of alternative memorial mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmp_mourn_theater_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cmp_mourn_theater_t25, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(cmp_mourn_theater_t50, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(cmp_mourn_extract_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cmp_mourn_extract_t25, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 25, 0.2).
narrative_ontology:measurement(cmp_mourn_extract_t50, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 50, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the catastrophe_memory_preservation kernel family. The mourning_practice_reading isolates ritual's identity-coordination function. Sibling readings (survival_competence, hybrid_atrophy) represent alternative structural interpretations of the same ritual practice with different ε values and beneficiary/victim structures. All three share the same base phenomenon (ritual mourning) but frame it through different epistemic lenses. The network edges indicate mutual influence: evidence for survival_competence would downgrade mourning_practice's ε and shift type toward tangled_rope; evidence for hybrid_atrophy would show piton dynamics (increasing theater_ratio) and institutional inertia.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_preservation__mourning_practice_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
