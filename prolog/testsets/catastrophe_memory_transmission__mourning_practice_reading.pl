% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__mourning_practice_reading, []).

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
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_transmission__mourning_practice_reading
 *   human_readable: Catastrophe-Memory Transmission: Mourning Practice Reading
 *   domain: religious_studies/cultural_anthropology/memory_studies
 *
 * SUMMARY:
 *   This constraint is the mourning_practice_reading of the
 *   catastrophe_memory_transmission kernel. The mourning practice reading
 *   interprets catastrophe-memory transmission as a mechanism for preserving
 *   community identity through ritual compartmentalization: specific days
 *   (often annual memorials) are designated for collective grief-processing,
 *   while other days are reserved for living, working, and forward-building.
 *   The practice teaches that to survive catastrophe collectively without
 *   being consumed by inherited trauma, a community must establish and
 *   enforce boundaries between mourning-time and living-time. This reading is
 *   distinct from the survival_competence_reading (which emphasizes
 *   transmitted adaptive capacity and early-warning systems) and the
 *   hybrid_pedagogical_reading (which frames grief-processing as vigilance
 *   training). The mourning_practice_reading focuses on the psychological and
 *   structural mechanism of boundary-maintenance as the primary survival
 *   function. The constraint exhibits Tangled Rope structure: it provides
 *   genuine coordination (a collective action solution to prevent trauma
 *   dissolution) while simultaneously extracting emotional labor from
 *   present-day bearers, particularly youth cohorts who inherit the
 *   obligation to perform grief but may not have direct memory of the
 *   catastrophe. The boundary-maintenance mechanism is the locus: it prevents
 *   uncontrolled grief from destroying community coherence, but the cost is
 *   that grief is suppressed and strictly regulated, requiring continuous
 *   enforcement through ritual authority.
 *
 * KEY AGENTS:
 *   - Present-day community bearers (powerless/identity_locked): Obligated to perform annual mourning rituals; identity fused with grief-bearing role; structurally mobile but cannot exit without identity dissolution
 *   - Youth cohorts inheriting trauma (moderate/constrained): Learn mourning practice from elders; bear disproportionate emotional labor as new practitioners; benefit from community structure but constrained by expectations
 *   - Ritual authority keepers—elders and clergy (institutional/arbitrage): Maintain the framework for boundary-maintenance; experience constraint as pure coordination; benefit from authority preservation
 *   - Community structural coherence apparatus—kinship networks and religious institutions (institutional/constrained): Depend on mourning practice to maintain collective identity; require active enforcement of ritual participation
 *   - Diasporic and assimilating populations (organized/mobile): See mourning practice as temporary transition mechanism; experience it as scaffold with sunset clause as structural integration increases
 *   - Academic anthropological discourse (institutional/constrained): Preserves mourning practice as timeless cultural exemplar; maintains high theater by naturalizing extractive mechanisms
 *   - Analytical observer at civilizational scale (analytical/analytical): Risks false-summit reading of mourning practice as immutable natural law of human psychology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__mourning_practice_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_transmission__mourning_practice_reading, 0.65).
domain_priors:theater_ratio(catastrophe_memory_transmission__mourning_practice_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__mourning_practice_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__mourning_practice_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__mourning_practice_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__mourning_practice_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__mourning_practice_reading, "Catastrophe-Memory Transmission: Mourning Practice Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__mourning_practice_reading, "religious_studies/cultural_anthropology/memory_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__mourning_practice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__mourning_practice_reading, '77369690-3afb-42c4-aa43-9be0978ed333').
narrative_ontology:cs_kernel_codification('77369690-3afb-42c4-aa43-9be0978ed333', fixed_text).
narrative_ontology:cs_authority_grounding('77369690-3afb-42c4-aa43-9be0978ed333', lineage).
narrative_ontology:cs_interpretation_layer_present('77369690-3afb-42c4-aa43-9be0978ed333').
narrative_ontology:cs_reading_relation('77369690-3afb-42c4-aa43-9be0978ed333', catastrophe_memory_transmission__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('77369690-3afb-42c4-aa43-9be0978ed333', catastrophe_memory_transmission__hybrid_pedagogical_reading, coexists_with).
narrative_ontology:cs_axiom('77369690-3afb-42c4-aa43-9be0978ed333', foundational, boundary_maintenance_identity_preservation).
narrative_ontology:cs_axiom_status(boundary_maintenance_identity_preservation, holdable).
narrative_ontology:cs_axiom_grounding('77369690-3afb-42c4-aa43-9be0978ed333', boundary_maintenance_identity_preservation, conventional).
narrative_ontology:cs_axiom('77369690-3afb-42c4-aa43-9be0978ed333', secondary, grief_suppression_necessity).
narrative_ontology:cs_axiom_status(grief_suppression_necessity, holdable).
narrative_ontology:cs_axiom_grounding('77369690-3afb-42c4-aa43-9be0978ed333', grief_suppression_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('77369690-3afb-42c4-aa43-9be0978ed333', boundary_separated_grief_processing).
narrative_ontology:cs_drift_state('77369690-3afb-42c4-aa43-9be0978ed333', contemporary_diaspora_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('77369690-3afb-42c4-aa43-9be0978ed333', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__mourning_practice_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__mourning_practice_reading, ritual_authority_keepers).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__mourning_practice_reading, community_structural_coherence).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__mourning_practice_reading, present_day_community_bearers).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__mourning_practice_reading, youth_cohorts_inheriting_trauma).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GRIEF-BEARING GENERATION (SNARE) — Present-day community members who inherit the catastrophe narrative and are obligated to perform annual mourning rituals. Identity is fused with the role of grief-bearer; exit would require abandoning cultural identity and community belonging. The mourning practice compartmentalizes grief to specific ritual days, but this boundary-maintenance extracts continuous emotional labor year-round. The agent is identity-locked: structurally mobile (could skip rituals, migrate, assimilate) but cannot exit without becoming unrecognizable to themselves.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__mourning_practice_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: YOUTH COHORTS (TANGLED ROPE) — Younger generations who learn mourning practice from elders. Constrained by expectations to participate, carry the narrative forward, and internalize the boundary-maintenance discipline. But they also benefit from the coordination function: the ritual provides structure for processing inherited trauma collectively, prevents the alternative of unprocessed grief that would fragment families and communities. Genuine coordination (how to mourn without dissolution) plus asymmetric extraction (emotional labor burden falls on youth as new practitioners).
constraint_indexing:constraint_classification(catastrophe_memory_transmission__mourning_practice_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RITUAL AUTHORITY KEEPERS (ROPE) — Elders, clergy, cultural custodians who maintain the mourning practice framework. Experience the constraint as pure coordination: teaching younger generations how to remember without being consumed by grief is the core function. They benefit from maintaining authority over interpretation (who can lead rituals, what narratives are canonical), but this benefit flows from the coordination function itself, not from extractive overlay. They perceive the boundary-maintenance as solution rather than burden.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__mourning_practice_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMUNITY STRUCTURAL COHERENCE (TANGLED ROPE) — The institutional structures (kinship networks, religious organizations, memory councils) that depend on the mourning practice to maintain collective identity and prevent trauma-driven dissolution. These actors benefit from the ritual's coordination function (holds the community together) and from the extraction function (emotional labor of grief-processing is channeled through institutions that gain authority and resources from managing it). Requires active enforcement: participants must show up, perform roles, internalize the boundary between mourning and living.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__mourning_practice_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: DIASPORIC POPULATIONS (SCAFFOLD) — Communities migrating or assimilating into majority cultures see mourning practice as temporary—necessary for first and second generations but with a sunset clause. The ritual serves as transition mechanism: it processes inherited trauma during the period when cultural boundaries are still salient, then can be abandoned or substantially modified when structural integration into the host society is complete. Exit costs decrease as generational remove increases. Theater is moderate—the practice is functionally keeping community together, not purely performative.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__mourning_practice_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ACADEMIC ANTHROPOLOGICAL DISCOURSE (PITON) — Scholarly frameworks that treat catastrophe-memory transmission as a natural, timeless cultural adaptation—a specimen of how 'traditional societies' process trauma. The discourse maintains the mourning practice as exemplary but increasingly detached from its actual extractive mechanisms and boundary-enforcement costs. High theater: the academic preservation of the practice as 'authentic cultural expression' often naturalizes what is actually difficult emotional labor for present-day practitioners. The function has atrophied to validation and documentation; the constraint persists through institutional inertia.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__mourning_practice_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER—NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, catastrophe-memory transmission appears as an immutable feature of human collective psychology: all societies that survive catastrophe develop ritual compartmentalization to prevent trauma transmission from dissolving group coherence. This reading sees the mourning practice as a natural law of cultural survival—inevitable, necessary, unchangeable. However, this perspective risks false summitry: the beneficiaries and extraction mechanisms are observable, suggesting the constraint is constructed institutional practice, not a law of nature.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__mourning_practice_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__mourning_practice_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_transmission__mourning_practice_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_transmission__mourning_practice_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_transmission__mourning_practice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_transmission__mourning_practice_reading, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_transmission__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.58): Moderate-high. The mourning practice extracts significant emotional labor from present-day bearers and youth cohorts. This is not the emotional labor of grieving the original catastrophe (which would be a legitimate cost of shared tragedy), but rather the labor of performing grief annually in prescribed ritual forms, maintaining boundaries between mourning and living, and transmitting the practice to new generations who have less direct connection to the original catastrophe. The extractiveness increases over the 10-unit interval as ritual practice becomes increasingly about remembrance and less about processing lived trauma (theater_ratio rises from 0.35 to 0.48). The extraction is moderate rather than severe because genuine coordination happens—the ritual does prevent uncontrolled trauma transmission and community dissolution—but the extraction is real because the boundary-maintenance mechanism suppresses grief outside designated times. Suppression (0.65): High. Significant enforcement barriers exist: (1) Ritual participation is mandatory through social enforcement (shame, exclusion, family pressure); (2) Grief expression outside mourning-days is suppressed (seen as indulgent, destabilizing, disrespectful to the dead); (3) Alternative approaches to memory (therapeutic, secular, individualized) are suppressed as threats to community coherence; (4) Exit from the community to avoid the practice is economically and psychologically costly. Suppression is high because the boundary-maintenance mechanism depends on collective compliance—deviation by any participant threatens the entire structure. Theater (0.48): Moderate. The practice is functionally performing grief-processing (not pure ritual theater), but theater increases over time as direct catastrophe experience fades. Early in post-catastrophe period (time_point=0), theater is lower (0.35) because grief is fresh and ritual is processing real trauma. Over time (time_point=10), theater increases (0.48) because the practice becomes about remembrance, historical transmission, and cultural preservation rather than actual grief management. The constraint transitions from Tangled Rope with high functional coordination to Tangled Rope with increasing performative overlay.
 *
 * PERSPECTIVAL GAP:
 *   This reading generates perspectival gaps across multiple dimensions. The ritual authority keepers (Rope perspective) experience the mourning practice as pure coordination—a solution to a collective problem—whereas present-day bearers (Snare perspective) experience it as extraction disguised as coordination. The beneficiary view (institutional, arbitrage) sees boundary-maintenance as natural and necessary; the victim view (powerless, identity_locked) sees it as coercive suppression. The diasporic perspective (Scaffold) sees the practice as temporary and sunset-able, whereas the institutional coherence apparatus (Tangled Rope) sees it as perpetual. The academic observer (Piton) preserves the practice as timeless cultural expression, while the analytical civilizational view (Mountain) risks naturalizing it as a law of human psychology. These gaps reveal that the same practice is simultaneously a solution (from the coordinator's view), an extraction mechanism (from the bearer's view), and a naturalizing false-summit narrative (from the analytical distance). The perspectival gap is irreducible—no single perspective captures the full structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position relative to extraction flow. Ritual authority keepers are beneficiaries with arbitrage exit options (they can maintain or modify the practice at will): d≈0.05-0.15, f(d)≈-0.12 to -0.01, making their experienced extraction minimal or negative (they benefit). Present-day bearers are victims with identity_locked exit (they are psychologically trapped by identity fusion even if structurally mobile): d≈0.89, f(d)≈1.28, making their experienced extraction high. Youth cohorts are victims with constrained exit (they can exit at moderate cost—relocation, community severance, identity disruption): d≈0.60, f(d)≈0.85, making their experienced extraction moderate-high. The diasporic populations have mobile exit (they can leave the practice through assimilation over time): d≈0.50, f(d)≈0.65, making their experienced extraction moderate. These directionality values drive the perspectival classifications: low d → Rope (beneficiary), high d → Snare or Tangled Rope (victim), moderate d → Tangled Rope or Scaffold. The engine computes d automatically from beneficiary/victim declarations and exit_options; the commentary simply explains the structural reasoning.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: This reading is not high-extractiveness (ε=0.58 is moderate), so mandatrophy conditions are not formally triggered. However, the analysis documents why the Tangled Rope classification holds: genuine coordination function (boundary-maintenance prevents trauma dissolution) is present alongside extraction (emotional labor is asymmetrically distributed and suppressed outside ritual times). The tension between these is not resolved by higher classification—Tangled Rope is precisely the category that holds both. The reading-specific mandatrophy question is whether the mourning practice is genuinely a coordination mechanism (in which case the extraction is a byproduct to be minimized) or whether the coordination function is a cover story for extraction (in which case the classification should shift toward Snare). The omega variables address this: measurement of actual trauma outcomes, ethnographic documentation of whether the practice heals or perpetuates inherited trauma, and structural analysis of whether authority preservation is the primary driver. If omega resolution favors 'extraction masquerading as coordination,' the classification would shift to Snare and extractiveness would increase. If it favors 'genuine coordination with extraction byproduct,' the classification remains Tangled Rope but suggests reform (reducing suppression, increasing voluntary participation, making boundary-maintenance optional).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_maintenance_mechanism_nature,
    'Is the boundary between mourning-day and living-days a functional psychological container (enabling grief processing without dissolution) or a coercive enforcement mechanism (suppressing grief outside designated times)?',
    'Longitudinal ethnographic study of grief affect and expression in everyday contexts across full calendar year; measurement of trauma symptoms, depression, and family cohesion across ritual-intensive vs ritual-attenuated communities; analysis of whether individual grief is genuinely compartmentalized or suppressed and re-emerges in other symptom forms',
    'If functional container: classification remains Tangled Rope (genuine coordination + moderate extraction). If coercive suppression: classification shifts toward Snare (extraction mechanism wearing a coordination mask). Boundary mechanism determines whether present-day bearers experience the practice as enabling or constraining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_maintenance_mechanism_nature, empirical, 'Whether boundary-maintenance is functional grief container or coercive suppression').

omega_variable(
    intergenerational_trauma_transmission_pathway,
    'Does the mourning practice actually reduce inherited trauma transmission to youth cohorts, or does it encode and stabilize trauma patterns across generations?',
    'Comparative analysis of trauma symptom prevalence and severity in high-ritual-participation vs low-ritual-participation households within same catastrophe-affected population; intergenerational trauma marker studies; interviews on whether ritual participation feels like processing or perpetuation',
    'If reduces transmission: supports coordination reading (ritual solves a real collective action problem). If stabilizes patterns: shifts classification toward Snare (extraction mechanism that appears to heal but maintains victim status). Affects victim set interpretation—are youth cohorts being protected or initialized into perpetual grief-bearing?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_trauma_transmission_pathway, empirical, 'Whether mourning practice reduces or encodes intergenerational trauma').

omega_variable(
    reading_contest_foreclosure_structure,
    'Do the three readings of the catastrophe_memory_transmission kernel (mourning_practice_reading, survival_competence_reading, hybrid_pedagogical_reading) logically foreclose each other within single community commitments, or do they coexist as different parties'' readings of the same phenomenon?',
    'Ethnographic documentation of whether religious/cultural authorities accept or reject the other readings; historical analysis of doctrine evolution; structural examination of whether adopting one reading necessitates rejecting others within the same institutional framework',
    'If foreclosure: one reading is the canonical interpretation and others are heterodox/suppressed (affects how committer-axis grounds this story in kernel landscape). If coexistence: readings are held by different factions within same tradition (this reading is one live option among peers). Determines whether narrative_context should frame this as contested or as one perspective among coequal alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_foreclosure_structure, conceptual, 'Whether kernel readings foreclose each other or coexist').

omega_variable(
    extractiveness_measurement_ambiguity,
    'Is the extractiveness (ε=0.58) measuring the emotional labor cost to present-day bearers, or the degree to which the ritual extracts cultural continuity value from younger generations for older generations'' authority preservation?',
    'Decompose extractiveness into two separate stories (emotional labor constraint vs. authority-preservation constraint) with different epsilon values; measure each independently to establish whether they are the same structural phenomenon or two distinct constraints conflated under one label',
    'If same constraint: ε=0.58 is the integrated measure. If two constraints: emotional labor story likely has ε≈0.45-0.50 (Tangled Rope); authority-preservation story likely has ε≈0.65+ (closer to Snare). Current composite ε may be obscuring which mechanism is dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractiveness_measurement_ambiguity, empirical, 'Whether extractiveness measures emotional labor or authority-preservation').

omega_variable(
    mourning_practice_reading_vs_survival_competence_reading_axiom_divergence,
    'Does the mourning_practice_reading''s core axiom (grief-compartmentalization preserves identity by preventing trauma-dissolution) fundamentally contradict the survival_competence_reading''s axiom (transmitted adaptive capacity preserves identity by encoding early-warning systems)?',
    'Close reading of how each reading frames the mechanism: does compartmentalization foreclose competence-transmission, or do they describe the same practice from different analytical lenses?',
    'If contradiction: mourning_practice_reading forecloses survival_competence_reading. If compatible: they coexist as different descriptions of the same ritual. Determines reading_relations.relation value (forecloses vs coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mourning_practice_reading_vs_survival_competence_reading_axiom_divergence, conceptual, 'Whether mourning_practice and survival_competence readings are contradictory or compatible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__mourning_practice_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catmem_mourn_tr_t0, catastrophe_memory_transmission__mourning_practice_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(catmem_mourn_tr_t5, catastrophe_memory_transmission__mourning_practice_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(catmem_mourn_tr_t10, catastrophe_memory_transmission__mourning_practice_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(catmem_mourn_be_t0, catastrophe_memory_transmission__mourning_practice_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(catmem_mourn_be_t5, catastrophe_memory_transmission__mourning_practice_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(catmem_mourn_be_t10, catastrophe_memory_transmission__mourning_practice_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(catmem_mourn_su_t0, catastrophe_memory_transmission__mourning_practice_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(catmem_mourn_su_t5, catastrophe_memory_transmission__mourning_practice_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(catmem_mourn_su_t10, catastrophe_memory_transmission__mourning_practice_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__mourning_practice_reading, attachment_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__mourning_practice_reading, catastrophe_memory_transmission__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__mourning_practice_reading, catastrophe_memory_transmission__hybrid_pedagogical_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the catastrophe_memory_transmission kernel. Sibling readings (survival_competence_reading and hybrid_pedagogical_reading) offer competing structural mechanisms for the same phenomenon—catastrophe-memory transmission. Each reading has its own ε, perspectives, and victim/beneficiary sets. They are linked as a constraint family via network.affects_constraints. The extractiveness difference (if this reading has ε=0.58 while survival_competence_reading has ε=0.35) reflects not different measurements but fundamentally different framings of what the mourning practice extracts. The mourning_practice_reading emphasizes emotional labor extraction; the survival_competence_reading emphasizes transmission of capacity; the hybrid_pedagogical_reading emphasizes vigilance encoding. These are not alternative measurements of the same extraction—they are distinct structural mechanisms described by different readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_transmission__mourning_practice_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
