% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__survival_competence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_memory_transmission__survival_competence_reading
 *   human_readable: Catastrophe-Memory Transmission: Survival Competence Reading
 *   domain: religious_studies/cultural_anthropology/memory_studies
 *
 * SUMMARY:
 *   Catastrophe-memory transmission preserves community identity by encoding
 *   adaptive capacity in ritual form. When a community experiences
 *   catastrophe—famine, persecution, displacement, genocide—it develops
 *   transmissive practices to teach future generations how to recognize
 *   recurrence patterns, organize collective response, and maintain cultural
 *   coherence under pressure. The seder ritual (in Jewish tradition),
 *   memorial days (Armenian, Holocaust, Rwanda commemorations), origin
 *   narratives of diaspora (Palestinian, Irish, Native American), and
 *   initiation rites that encode survival knowledge (Indigenous Australian
 *   songlines, Pacific navigation routes) are all vehicles for this
 *   transmission. The constraint exhibits Tangled Rope structure: there is
 *   genuine coordination function (the ritual teaches the community how to
 *   tell its own story and prepare for recurrence), but asymmetric extraction
 *   occurs through pedagogical labor burden (transmitted must spend time and
 *   emotional energy maintaining fidelity) and institutional gatekeeping
 *   (religious authorities and cultural leaders control the transmission
 *   mechanism and who has authority to interpret or modify it). Future
 *   generations depend on this transmission for cultural continuity and
 *   adaptive knowledge, creating a victim set (generations deprived if
 *   transmission fails) alongside beneficiaries (the transmitting community
 *   and institutional authorities who gain status/authority from controlling
 *   transmission). The constraint's extractiveness has increased over the
 *   measured interval (0.22 → 0.38) as formalized commemoration has replaced
 *   informal family transmission, raising the theater ratio (performative
 *   content) while maintaining suppression (obligation to participate
 *   remains, whether or not the adaptive knowledge content is preserved).
 *
 * KEY AGENTS:
 *   - Future Generations: Primary victim (powerless/trapped) — depend on transmission for adaptive knowledge and cultural identity; have no voice in how transmission occurs
 *   - Ritual Practitioners and Knowledge Bearers: Secondary victim (moderate/constrained) — bear pedagogical labor burden and time commitment required to maintain transmission fidelity
 *   - Institutional Community: Primary beneficiary (institutional/arbitrage) — benefits from transmission as coordination mechanism that solves the collective-action problem of cross-generational knowledge transfer
 *   - Religious/Cultural Authorities: Secondary beneficiary (powerful/mobile) — control gatekeeping role and interpretive authority; benefit from social status and resource flow but also bear burden of ensuring transmission
 *   - Diasporic Communities: Complex victim-beneficiary (organized/mobile) — face both increased extraction (adaptation pressure) and increased necessity (transmission becomes MORE essential under displacement), retaining choice about how to engage with the mechanism
 *   - Formalized Commemoration Systems: Institutional actor (institutional/constrained) — maintain ritual form through inertia even as pedagogical function degrades; theater-driven rather than competence-driven
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__survival_competence_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_transmission__survival_competence_reading, 0.48).
domain_priors:theater_ratio(catastrophe_memory_transmission__survival_competence_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__survival_competence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__survival_competence_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__survival_competence_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__survival_competence_reading, "Catastrophe-Memory Transmission: Survival Competence Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__survival_competence_reading, "religious_studies/cultural_anthropology/memory_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__survival_competence_reading, '01e8e466-6a6f-4ab2-9a90-3e3d8938be70').
narrative_ontology:cs_kernel_codification('01e8e466-6a6f-4ab2-9a90-3e3d8938be70', distributed).
narrative_ontology:cs_authority_grounding('01e8e466-6a6f-4ab2-9a90-3e3d8938be70', practice).
narrative_ontology:cs_interpretation_layer_present('01e8e466-6a6f-4ab2-9a90-3e3d8938be70').
narrative_ontology:cs_reading_relation('01e8e466-6a6f-4ab2-9a90-3e3d8938be70', catastrophe_memory_transmission__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('01e8e466-6a6f-4ab2-9a90-3e3d8938be70', catastrophe_memory_transmission__hybrid_pedagogical_reading, coexists_with).
narrative_ontology:cs_axiom('01e8e466-6a6f-4ab2-9a90-3e3d8938be70', foundational, transmission_primary_function_is_adaptive_competence).
narrative_ontology:cs_axiom_status(transmission_primary_function_is_adaptive_competence, holdable).
narrative_ontology:cs_axiom_grounding('01e8e466-6a6f-4ab2-9a90-3e3d8938be70', transmission_primary_function_is_adaptive_competence, instrumental).
narrative_ontology:cs_axiom('01e8e466-6a6f-4ab2-9a90-3e3d8938be70', secondary, ritual_structure_encodes_competence).
narrative_ontology:cs_axiom_status(ritual_structure_encodes_competence, holdable).
narrative_ontology:cs_axiom_grounding('01e8e466-6a6f-4ab2-9a90-3e3d8938be70', ritual_structure_encodes_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('01e8e466-6a6f-4ab2-9a90-3e3d8938be70', community_embedded_transmission).
narrative_ontology:cs_drift_state('01e8e466-6a6f-4ab2-9a90-3e3d8938be70', contemporary_formalized_commemoration, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('01e8e466-6a6f-4ab2-9a90-3e3d8938be70', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__survival_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__survival_competence_reading, transmitting_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__survival_competence_reading, cultural_continuity_maintenance).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__survival_competence_reading, future_generations_knowledge_deprivation).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__survival_competence_reading, pedagogical_labor_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Cannot exit the knowledge deprivation if transmission fails. Structurally unable to negotiate terms of cultural inheritance. Bears the cost of adaptive capacity loss without recourse or voice in the transmission mechanism. Maximum experienced extraction because the trap is intergenerational and inescapable within the constraint's scope.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__survival_competence_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RITUAL PRACTITIONERS (TANGLED ROPE) — Constrained by time, cultural obligation, and memory labor required to maintain transmission fidelity. Also benefits from the coordination function: the ritual structure itself teaches them how to organize and transmit knowledge, how to recognize patterns in catastrophe, and how to prepare for recurrence. Significant extraction (pedagogical labor) but genuine coordination benefit (competence transmission mechanism).
constraint_indexing:constraint_classification(catastrophe_memory_transmission__survival_competence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL CONTINUITY (ROPE) — The community itself benefits from the transmission mechanism as a coordination device. The constraint solves the collective action problem of how to preserve adaptive knowledge across generational breaks. Ritual teaches the community how to tell itself its own story, how to recognize recurrent patterns, and how to prepare. Net beneficiary position — the institution is strengthened by the transmission function. Low experienced extraction because the coordination benefit is genuine and the community controls the mechanism.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__survival_competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIASPORIC COMMUNITIES (TANGLED ROPE) — Organized agents facing migration, exile, or cultural displacement experience the transmission constraint differently. The ritual becomes more extractive (it must adapt to new contexts while preserving core competence) but also more essential (it is the mechanism by which the community survives cultural displacement). Mobile exit option because diaspora communities can adopt, modify, or abandon transmission practices, but they choose not to because the competence transmission function becomes MORE valuable under dispersion. Moderate effective extraction because the agents retain choice while the necessity increases.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__survival_competence_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: FORMALIZED COMMEMORATION (PITON) — State-mandated or institutionalized catastrophe-memory practices (Holocaust remembrance days, national tragedy commemorations) often become decoupled from actual survival competence transmission. The ritual persists through institutional inertia and formal obligation, but its pedagogical function has degraded. Theater ratio is high because the formal structure is maintained while the adaptive knowledge transmission has migrated elsewhere (informal family practices, specialized training). The piton classification reflects that the formalized mechanism is largely performative—it maintains the appearance of transmission without the functional content.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__survival_competence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a sufficiently abstract civilizational/universal perspective, catastrophe-memory transmission appears as an immutable feature of how human communities survive: all societies that experience catastrophe develop transmission mechanisms for adaptive knowledge, and this mechanism appears structurally necessary to cultural continuity. However, this perspective is vulnerable to false-summit detection: the 'necessity' naturalizes what is actually a contingent institutional arrangement controlled by specific agents (religious authorities, community leaders, state actors) with extractive interests in maintaining the transmission structure's form even as its content degrades.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__survival_competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: RELIGIOUS/CULTURAL AUTHORITIES (TANGLED ROPE) — Powerful institutional actors (clergy, cultural councils, family patriarchs) who control the transmission mechanism. They benefit from their gatekeeping role (social authority, resource control, interpretive power) while also bearing the burden of ensuring transmission fidelity. The constraint extracts their labor while granting them authority. Effective extraction is moderate because they have mobile options (can abandon their role, redefine their authority, or modify the transmission structure) but the coordination function they control is too valuable to abandon. The perspectival gap with the powerless (perspective 1) is maximal.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__survival_competence_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__survival_competence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_transmission__survival_competence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_transmission__survival_competence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_transmission__survival_competence_reading, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_transmission__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint requires sustained pedagogical labor to maintain transmission fidelity, and it concentrates interpretive authority in institutional gatekeepers. However, extractiveness is moderate rather than high because: (1) the coordination function is genuine—future generations genuinely benefit from receiving adaptive knowledge, and the community genuinely benefits from the mechanism that ensures the transmission occurs; (2) participants retain meaningful choice (practitioners can modify transmission, adapt rituals, or exit through diaspora or assimilation, though at cost); (3) extractive mechanisms are not hidden—the obligation to transmit is explicit and culturally honored rather than coercive. The measurement trajectory shows extractiveness increasing from 0.22 to 0.38 as formal state commemoration has supplemented or replaced informal family transmission, reducing the adaptive content while increasing performative requirements. Suppression (0.48): Moderate-high. The constraint operates through cultural obligation, social shame for non-participation, and institutional control over who has authority to teach or interpret. These suppress alternatives (abandoning transmission, radically reinterpreting narratives, refusing to teach younger generations), but suppression is not total—diaspora communities routinely modify transmission, assimilating populations choose not to transmit, and reformist movements can reinterpret the meaning of transmission. Theater ratio (0.55): Moderate-high. Formalized commemoration days and institutional rituals often preserve performative structure (the ritual happens at the appointed time) while the actual competence-transmission function has migrated elsewhere—to informal family meals, specialized training programs, or youth organizations. The measurement increase (0.35 → 0.55) reflects the growing gap between formal ritual structure and adaptive knowledge content. Claimed type (Tangled Rope): The constraint exhibits both genuine coordination (the ritual mechanism enables cross-generational knowledge transfer) and asymmetric extraction (pedagogical labor burden + institutional gatekeeping). The tangled rope classification requires both elements, and both are present.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival range is broad because agents occupy structurally different positions relative to the transmission mechanism. Future generations (powerless/trapped) experience maximal extraction because they cannot negotiate terms and must receive whatever knowledge is (or is not) transmitted—for them, the constraint appears as Snare. Practitioners (moderate/constrained) experience mixed coordination and extraction—the ritual teaches them survival competence while burdening them with transmission labor—so it appears as Tangled Rope. The institutional community (institutional/arbitrage) benefits from the coordination function and controls the mechanism, experiencing it as Rope (pure coordination). Diasporic communities (organized/mobile) face paradoxical positioning: as they become more dispersed and at-risk, the transmission constraint becomes MORE valuable (extraction increases) but they also retain more choice (mobile exit options), producing Tangled Rope at higher effective extraction than modality-stable communities. Formalized commemorations (institutional/constrained) see their own ritual as performative and degraded—Piton classification reflects awareness that the mechanism is maintained through inertia rather than functional necessity. The analytical observer risks seeing the constraint as Mountain (immutable structural feature of human culture) but this naturalizes contingent institutional arrangements (gatekeeping authority, formalized vs. informal modality) that are actually subject to reform. The false-summit risk is highest here: the analytical observer may treat 'all societies maintain catastrophe-memory' as natural law when the actual constraint is specifically about gatekeeping control and institutional authority over transmission form.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural position relative to the extraction flow. Future generations (trapped, powerless) derive d → 0.95 (full target, bearing all costs of transmission failure or degradation). Practitioners (constrained, moderate power) derive d → 0.60 (mixed: bear labor burden but benefit from competence transmission). Institutional community (arbitrage, institutional) derives d → 0.10 (net beneficiary: coordination benefit outweighs costs). Religious authorities (mobile, powerful) derive d → 0.45 (mixed: gatekeeping benefit offset by responsibility burden). Diasporic communities show the complexity: (organized/mobile) nominally derives d → 0.50 (symmetric), but the structured data reveals asymmetry—they experience the constraint as MORE extractive (higher d) than the nominal value because adaptation pressure increases even as choice increases. The range of d values across perspectives produces proportionally different χ values from the same base ε, explaining why the same structural feature (transmission ritual) classifies as Snare, Tangled Rope, Rope, and Piton from different perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that Tangled Rope is the correct analytical classification (the institutional community's perspective), while acknowledging that the powerless experience Snare and the beneficiary institution experiences Rope. The mandatrophy dissolves when we recognize that different agents have fundamentally different structural relationships to the constraint—it is NOT the case that the constraint is 'really' one type and other perspectives are misperceiving it. Rather, the constraint exhibits both genuine coordination function (transmission of adaptive knowledge) and asymmetric extraction (labor burden + gatekeeping), making Tangled Rope the accurate middle classification. The false-summit risk (mountain from the analytical perspective) represents a failure to recognize that the apparent 'necessity' of catastrophe-memory transmission is actually a contingent institutional arrangement. What is universal is that communities need to preserve adaptive knowledge across generational breaks; what is contingent is that they do so through ritual gatekeeping controlled by specific authorities. The constraint could be reorganized (democratize transmission, migrate to different modalities, decentralize authority) without losing the adaptive function. The analytical observer's job is to notice this possibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_transmission_fidelity,
    'What constitutes sufficient fidelity in transmitting ''adaptive capacity'' across generational breaks? Does the transmission require specific ritual forms, or can the core competence migrate to new vehicles?',
    'Historical analysis of catastrophe-memory practices across cultures experiencing similar crises; measurement of competence retention rates under different transmission modalities (formal ritual vs. informal narrative vs. practical training); examination of whether diaspora communities that abandon formal ritual maintain adaptive capacity',
    'If fidelity requires specific ritual form: constraint is more extractive (future generations are locked into one transmission pathway). If competence can migrate: constraint is less extractive (community has flexibility in how to transmit). This directly affects whether the constraint is Tangled Rope (what we classify) or Snare (what the false-summit perspective risks).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_transmission_fidelity, empirical, 'Fidelity requirements for adaptive capacity transmission').

omega_variable(
    identity_preservation_mechanism,
    'Is ''preservation of identity through catastrophe-memory transmission'' a distinct mechanism from identity preservation through other cultural practices (language, dietary law, kinship norms)? Or is it one component of a larger identity-coordination system that the decomposition treats as separable when it is structurally entangled?',
    'Ethnographic analysis of which cultural practices communities abandon first under assimilation or displacement pressure, and which are preserved as non-negotiable; correlation with identity-salience surveys; comparison across diaspora populations with different resource levels and acculturation pressures',
    'If distinct mechanism: this constraint story is appropriately decomposed. If entangled with broader identity coordination: this reading may be artificially isolating one extraction mechanism from others, and the true constraint family is larger than three siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_preservation_mechanism, conceptual, 'Whether catastrophe-memory is separable from broader identity-coordination practices').

omega_variable(
    reading_boundary_clarity,
    'Does this survival-competence reading remain clearly bounded from the hybrid-pedagogical reading (which also emphasizes ritual teaching function), or do they collapse into one constraint under ethnographic scrutiny?',
    'Detailed comparison of what each reading identifies as the primary extraction mechanism: this reading emphasizes knowledge-transmission burden and future-generation deprivation; hybrid-pedagogical emphasizes mourning-as-vigilance and early-warning competence. Are these two distinct mechanisms in the same ritual, or two names for one mechanism?',
    'If collapse occurs: constraint family should be two siblings (mourning-practice reading + this reading as a unified survival mechanism) rather than three. If boundaries hold: the three-way decomposition is appropriate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_clarity, conceptual, 'Boundary clarity between survival-competence and hybrid-pedagogical readings').

omega_variable(
    institutional_authority_extraction_level,
    'How much of the measured extractiveness (0.38) is attributable to institutional gatekeeping (religious authorities controlling transmission) versus genuine pedagogical labor burden on practitioners? Are the two separable?',
    'Analysis of communities where transmission has been decentralized (shared pedagogical roles, democratized ritual leadership) versus hierarchical (centralized authority); measurement of resource flow and authority concentration in each model; ethnographic interviews on perceived extraction burden',
    'If gatekeeping dominates extractiveness: the constraint could be significantly reduced by democratizing transmission authority. If pedagogical labor is primary: democratization does not reduce the constraint. This affects whether reform pathways exist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_extraction_level, empirical, 'Attribution of extractiveness to gatekeeping vs. labor burden').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__survival_competence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catmem_surv_tr_t0, catastrophe_memory_transmission__survival_competence_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(catmem_surv_tr_t30, catastrophe_memory_transmission__survival_competence_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(catmem_surv_tr_t60, catastrophe_memory_transmission__survival_competence_reading, theater_ratio, 60, 0.55).

% Extraction over time
narrative_ontology:measurement(catmem_surv_be_t0, catastrophe_memory_transmission__survival_competence_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(catmem_surv_be_t30, catastrophe_memory_transmission__survival_competence_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(catmem_surv_be_t60, catastrophe_memory_transmission__survival_competence_reading, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(catmem_surv_su_t0, catastrophe_memory_transmission__survival_competence_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(catmem_surv_su_t30, catastrophe_memory_transmission__survival_competence_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(catmem_surv_su_t60, catastrophe_memory_transmission__survival_competence_reading, suppression_requirement, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__survival_competence_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__survival_competence_reading, catastrophe_memory_transmission__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__survival_competence_reading, catastrophe_memory_transmission__hybrid_pedagogical_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel: catastrophe_memory_transmission. The kernel itself is the practice of ritually transmitting narratives about catastrophic events. This reading emphasizes the survival-competence and pedagogical transmission function. Sibling readings emphasize mourning-practice (boundary maintenance) and hybrid-pedagogical (vigilance encoding). All three readings share the same base institutional structure but identify different primary functions and extraction mechanisms. The three stories are linked via reading_relations (coexists_with) because they represent competing analytical claims held by different research communities and different cultural practitioners—no single framework can determine which reading is 'correct' without prior assumptions about what catastrophe-memory is 'for.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_transmission__survival_competence_reading, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
