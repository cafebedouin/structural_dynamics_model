% ============================================================================
% CONSTRAINT STORY: diaspora_synchronization_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_diaspora_synchronization_mechanism, []).

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
 *   constraint_id: diaspora_synchronization_mechanism
 *   human_readable: Diaspora Synchronization Through Catastrophe Ritual
 *   domain: religious_studies/collective_memory/diaspora_studies
 *
 * SUMMARY:
 *   Diaspora synchronization through catastrophe ritual examines how
 *   dispersed Jewish communities maintained collective coherence across
 *   centuries and continents through synchronized commemoration of
 *   persecution (Tisha B'Av, Passover). The constraint embeds a contested
 *   kernel: is this ritual transmission preserving survival-competence
 *   (adaptive knowledge for persecuted populations) or only symbolic
 *   mourning-practice? The structure exhibits all six classification types,
 *   depending on the observer's structural position relative to the
 *   transmission authority. For identity-locked assimilating members, the
 *   constraint appears as total extraction (Snare) — they cannot imagine
 *   exiting without ceasing to be Jewish. For rabbinic authority, it appears
 *   as pure coordination (Rope) — they are solving the genuine problem of
 *   maintaining collective identity across geographic rupture. For heterodox
 *   communities, it is mixed coordination and suppression (Tangled Rope) —
 *   they benefit from the diaspora infrastructure but are constrained by
 *   conformity pressure. For secular organizing movements, it is a
 *   transitional problem with a sunset (Scaffold) — alternative
 *   synchronization mechanisms are emerging. For the ritual system itself,
 *   viewed civilizationally, it is mostly performative (Piton) — the
 *   operational content (survival knowledge) has atrophied, leaving only
 *   symbolic form. The analytical observer at universal scope risks
 *   naturalizing this as an immutable law of diaspora (Mountain) — but the
 *   structural data reveals beneficiaries and victims, indicating false
 *   summit.
 *
 * KEY AGENTS:
 *   - Assimilating Community Members: Primary victims (powerless/identity_locked) — identity fused with diaspora continuity; cannot exit without abandoning identity category
 *   - Heterodox Practice Communities: Secondary victims (moderate/constrained) — benefit from diaspora infrastructure but constrained by conformity pressure to canonical forms
 *   - Rabbinic Authority / Institutional Memory Keepers: Primary beneficiaries (institutional/arbitrage) — set terms of commemoration; preserve interpretive authority through coordinating diaspora practice
 *   - Secular Jewish Diaspora Movements: Organized alternatives (organized/mobile) — building non-ritual diaspora synchronization mechanisms with sunset logic
 *   - The Commemoration Ritual System Itself: Institutional actor (institutional/arbitrage) — maintains performative form as function atrophies; theater ratio high
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional design as immutable diaspora law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(diaspora_synchronization_mechanism, 0.52).
domain_priors:suppression_score(diaspora_synchronization_mechanism, 0.48).
domain_priors:theater_ratio(diaspora_synchronization_mechanism, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(diaspora_synchronization_mechanism, extractiveness, 0.52).
narrative_ontology:constraint_metric(diaspora_synchronization_mechanism, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(diaspora_synchronization_mechanism, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(diaspora_synchronization_mechanism, tangled_rope).
narrative_ontology:human_readable(diaspora_synchronization_mechanism, "Diaspora Synchronization Through Catastrophe Ritual").
narrative_ontology:topic_domain(diaspora_synchronization_mechanism, "religious_studies/collective_memory/diaspora_studies").

domain_priors:requires_active_enforcement(diaspora_synchronization_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(diaspora_synchronization_mechanism, ritual_transmission_authority).
narrative_ontology:constraint_beneficiary(diaspora_synchronization_mechanism, diaspora_identity_continuity).
narrative_ontology:constraint_victim(diaspora_synchronization_mechanism, heterodox_practice_communities).
narrative_ontology:constraint_victim(diaspora_synchronization_mechanism, non_commemorative_survival_knowledge).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASSIMILATING MEMBER (SNARE) — Identity fused with diaspora continuity; cannot exit without becoming 'other.' Biographical horizon: cannot see the constraint as changeable during their lifetime. Structurally mobile (could leave) but identity-locked — abandoning the ritual means abandoning the identity category itself. Maximum experienced extraction: coercion is internalized through identity fusion.
constraint_indexing:constraint_classification(diaspora_synchronization_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: HETERODOX PRACTICE COMMUNITY (TANGLED ROPE) — Benefits from diaspora infrastructure (synagogue networks, educational materials, collective memory preservation) but constrained by pressure to conform to canonical commemoration forms. Generational timescale: can imagine alternative practices emerging. High cost to exit (lose community access) but not impossible. Mixed extraction and coordination.
constraint_indexing:constraint_classification(diaspora_synchronization_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RABBINIC AUTHORITY (ROPE) — Institutional beneficiary with arbitrage options. Can modify ritual interpretations to accommodate change while maintaining core transmission. Immediate timescale: sees ritual coordination as functioning well. Net positive: authority is preserved through coordinating diaspora practice. Low experienced extraction because the institution sets the terms.
constraint_indexing:constraint_classification(diaspora_synchronization_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SECULAR DIASPORA MOVEMENT (SCAFFOLD) — Organized agents (secular Jewish organizations, cultural centers) see ritual synchronization as temporary — they are building alternative diaspora-coherence mechanisms (cultural festivals, linguistic revival, historical scholarship) that bypass religious commemoration. Sunset logic: as secular alternatives mature, religious ritual constraint loses its synchronization monopoly. Mobile exit: secular diaspora communities can organize without traditional rabbinic structure.
constraint_indexing:constraint_classification(diaspora_synchronization_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: HISTORICAL COMMEMORATION SYSTEM (PITON) — The ritual itself is largely theatrical at this remove from active persecution. Tisha B'Av and Passover no longer encode survival-competence (how to hide, forage, organize clandestinely) — they preserve only the mourning form. The original function (transmitting operational knowledge for persecuted diaspora) has atrophied; what remains is performance of memory. Theater ratio is high because much of the ritual's contemporary function is aesthetic/symbolic rather than operational.
constraint_indexing:constraint_classification(diaspora_synchronization_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal/civilizational perspective, diaspora synchronization is a structural necessity: dispersed populations with no central authority must use repeated ritual markers to maintain coherence. This perspective sees the constraint as an immutable property of diaspora existence itself — scattered communities MUST synchronize through shared commemoration or lose cohesion. However, the structural data contradicts this: clear beneficiaries (rabbinic authority), clear victims (heterodox communities), and clear suppression mechanisms (identity fusion, conformity pressure) indicate this is a false summit naturalizing a contingent institutional arrangement.
constraint_indexing:constraint_classification(diaspora_synchronization_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(diaspora_synchronization_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(diaspora_synchronization_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(diaspora_synchronization_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(diaspora_synchronization_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(diaspora_synchronization_mechanism, TR),
    TR >= 0.70.

:- end_tests(diaspora_synchronization_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over the interval. The constraint's extractiveness has increased as the operational content (survival knowledge) has atrophied and the performative content (symbolic mourning) has deepened. Early in the interval (t=0), when persecution was active and rituals encoded actual survival tactics, extractiveness was lower (0.35) — the coordination was genuinely needed. As persecution diminished (t=250-750), extractiveness rose (0.35→0.52) because the ritual's function shifted from operational to symbolic: communities continue to synchronize through the same ritual form, but the knowledge transmitted is mourning-practice rather than survival-competence. Suppression (0.48): Moderate and declining. Early suppression was high (0.72) when assimilating members faced real barriers (external persecution, legal prohibition, economic dependency on diaspora networks). Contemporary suppression (0.48) is lower because structural barriers have decreased in many contexts — but identity fusion and internalized conformity pressure remain. Theater ratio (0.64): High and rising. The ritual's contemporary function is largely theatrical: Tisha B'Av and Passover commemorate historical catastrophe but rarely encode clandestine organization methods or actual survival techniques. The ratio has risen from 0.48 to 0.64 as the operational content has been stripped away, leaving only the form of remembrance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates full classification divergence from a single base. The assimilating member experiences maximum extraction through identity fusion (Snare, chi near 1.0) — they cannot perceive exit options because their identity is constituted through the diaspora category. The rabbinic authority experiences coordination (Rope, chi near 0.0) — they are solving the genuine problem of maintaining collective identity across dispersion; they set the terms and benefit. The heterodox community experiences mixed coordination and extraction (Tangled Rope) — they both benefit from diaspora infrastructure and are constrained by conformity pressure. The secular movement experiences a transitional problem (Scaffold) — they have exit options and are building alternatives. The ritual system viewed civilizationally appears as mostly performance (Piton) — the function has degraded but the form persists. The analytical observer at universal scope risks seeing an immutable natural law (Mountain) — but the false summit detector identifies this as naturalization: clear beneficiaries and victims indicate contingent institutional arrangement, not law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) ranges from near 1.0 (full target) for identity-locked assimilating members to near 0.0 (full beneficiary) for rabbinic authority. For assimilating members: trapped or identity_locked exit + victim status (bear costs of conformity pressure, identity fusion) → high d → high chi. For rabbinic authority: institutional power + arbitrage options (can modify interpretations while maintaining authority) + beneficiary status (collects interpretive authority and institutional prestige) → low d → negative chi (subsidy). For heterodox communities: moderate power + constrained exit (can leave but at cost) + mixed beneficiary/victim status (benefit from infrastructure, bear costs of conformity) → d near 0.5 → moderate chi. For secular movements: organized power + mobile exit (can build alternatives) + neither beneficiary nor victim (external to the transmission system) → d near 0.4 → low chi. The piton classification derives from theater_ratio (0.64) rather than from experienced extraction — the ritual system itself is the 'constraint,' and it is maintained largely through performative activity. The mountain perspective at analytical/universal scope is where the false summit detection fires: the claimed type (mountain) contradicts the structural data (beneficiaries, victims, suppression), revealing that 'diaspora synchronization is a natural law' is a false natural law — a contingent institutional design naturalized through universalizing rhetoric.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits unresolved mandatrophy: the original mandate (preserving survival-competence for persecuted diaspora) has outlived its function (modern diaspora communities face less active persecution in many contexts, and survival knowledge has become less relevant). Yet the institutional structure persists through theater and identity fusion. The classification depends on whether you measure the constraint at the level of the mandate (has it been fulfilled/outlived?) or the level of the institutional structure (does it still function?). From the mandate perspective: mandatrophy is clear — survival-competence transmission is no longer the primary function; mourning-practice performance is. From the institutional perspective: no mandatrophy — the rabbinic authority has successfully adapted the constraint to new social contexts (diaspora identity maintenance rather than persecution survival). The six-question battery resolves this: founding problem (survive persecution while scattered), founding_problem_status (dead in many modern contexts, live in others), disappearance_verdict (world rearranges — diaspora communities would reorganize but likely survive). The unresolved mandatrophy is visible in the rising theater_ratio (0.48→0.64) and rising extractiveness (0.35→0.52) even as suppression_requirement declines (0.72→0.48) — the constraint is performing more and coordinating less, a signature of degraded mandate masked by institutional persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_competence_vs_symbolic_preservation,
    'Does contemporary ritual transmission preserve actual survival-competence (adaptive knowledge for persecuted diaspora) or only symbolic continuity (mourning form without operational content)?',
    'Historical analysis of ritual content: does the transmitted knowledge still encode methods for clandestine organization, resource acquisition, or evasion tactics? Or does it encode only symbolic markers of remembrance?',
    'If survival-competence is preserved: the constraint''s coordination function is genuine and life-sustaining (Rope or Tangled Rope). If only symbolic: the constraint is mostly performative (Piton, or Snare disguised as coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(survival_competence_vs_symbolic_preservation, empirical, 'Whether diaspora ritual encodes operational survival knowledge or only symbolic continuity').

omega_variable(
    identity_lock_mechanism,
    'Is the assimilating member''s inability to exit based on internalized identity fusion (identity_locked) or structural economic/legal barriers (trapped)?',
    'Comparison of post-exit trajectories: assimilating members who leave the diaspora community; analysis of identity reformation after exit; measurement of persistent identity-fusion effects vs. elimination of barriers.',
    'If identity_locked (primary binding mechanism): the constraint is cognitive capture disguised as natural community continuity. Targets could exit if their identity frame shifted (diagnosis: oracle gap, Theorem 4). If trapped (structural barriers): the constraint is structural coercion; exit is materially impossible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether diaspora exit barriers are identity-based or structural').

omega_variable(
    secular_alternative_sufficiency,
    'Can secular alternatives (cultural festivals, linguistic revival, historical scholarship, political organizing) actually provide diaspora synchronization equivalent to religious ritual?',
    'Comparative analysis of diaspora coherence in secular vs. religious communities; measurement of organizational capacity and intergenerational transmission in both modes.',
    'If secular alternatives are sufficient: scaffold perspective is correct — sunset is real, religious ritual constraint is transitional. If insufficient: secular movement lacks the synchronization depth of ritual (scaffold classification is aspirational, not structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_alternative_sufficiency, empirical, 'Whether secular practices can replace ritual as diaspora synchronization mechanism').

omega_variable(
    kernel_reading_natural_law_vs_constructed,
    'Is diaspora synchronization through catastrophe ritual a reading of a deeper kernel about collective survival (that would make the mountain perspective legitimate), or a constructed institutional arrangement that benefits rabbinic authority?',
    'Ethnographic and historical: do other dispersed populations (diaspora without religious tradition, stateless peoples, migrant communities) spontaneously develop analogous ritual synchronization? Or is ritual synchronization specific to institutional religious transmission?',
    'If universal diaspora property: the mountain perspective captures something real about human collective behavior under dispersion. If contingent institutional choice: the mountain is a false summit; the constraint is tangled rope masked as natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_natural_law_vs_constructed, conceptual, 'Whether diaspora ritual is necessary law or contingent institutional design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(diaspora_synchronization_mechanism, 0, 750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(diasync_tr_t0, diaspora_synchronization_mechanism, theater_ratio, 0, 0.48).
narrative_ontology:measurement(diasync_tr_t250, diaspora_synchronization_mechanism, theater_ratio, 250, 0.55).
narrative_ontology:measurement(diasync_tr_t500, diaspora_synchronization_mechanism, theater_ratio, 500, 0.62).
narrative_ontology:measurement(diasync_tr_t750, diaspora_synchronization_mechanism, theater_ratio, 750, 0.64).

% Extraction over time
narrative_ontology:measurement(diasync_be_t0, diaspora_synchronization_mechanism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(diasync_be_t250, diaspora_synchronization_mechanism, base_extractiveness, 250, 0.42).
narrative_ontology:measurement(diasync_be_t500, diaspora_synchronization_mechanism, base_extractiveness, 500, 0.48).
narrative_ontology:measurement(diasync_be_t750, diaspora_synchronization_mechanism, base_extractiveness, 750, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(diasync_su_t0, diaspora_synchronization_mechanism, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(diasync_su_t250, diaspora_synchronization_mechanism, suppression_requirement, 250, 0.65).
narrative_ontology:measurement(diasync_su_t500, diaspora_synchronization_mechanism, suppression_requirement, 500, 0.52).
narrative_ontology:measurement(diasync_su_t750, diaspora_synchronization_mechanism, suppression_requirement, 750, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(diaspora_synchronization_mechanism, attachment_coordination).
narrative_ontology:affects_constraint(diaspora_synchronization_mechanism, heterodox_jewish_practice_suppression).
narrative_ontology:affects_constraint(diaspora_synchronization_mechanism, diaspora_linguistic_transmission).
narrative_ontology:affects_constraint(diaspora_synchronization_mechanism, rabbinic_interpretive_authority).

% DUAL FORMULATION NOTE:
% Diaspora synchronization decomposes into three structurally distinct constraints: (1) the synchronization mechanism itself (this story) — extractiveness 0.52, theater 0.64; (2) suppression of heterodox practice forms (separate story) — extractiveness higher because it is pure enforcement without coordination benefit; (3) rabbinic authority preservation (separate story) — extractiveness lower because authority perceives genuine coordination function. Each has different ε values and different perspectives. The three stories are linked: synchronization depends on suppressing alternatives (story 2) and maintaining institutional interpretive authority (story 3).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(diaspora_synchronization_mechanism, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
