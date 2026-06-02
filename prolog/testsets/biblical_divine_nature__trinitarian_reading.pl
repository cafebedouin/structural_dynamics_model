% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__trinitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__trinitarian_reading, []).

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
 *   constraint_id: biblical_divine_nature__trinitarian_reading
 *   human_readable: Trinitarian Dogma: Three Hypostases, One Ousia
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   The trinitarian doctrine—three eternally coequal persons (hypostases)
 *   sharing one divine essence (ousia)—emerged as the orthodox Christian
 *   answer to the theological crisis of the 4th century. The Council of
 *   Nicaea (325 CE) was convened to resolve the Arian controversy: Christ's
 *   status as begotten of the Father seemed to imply subordination and
 *   creatureliness, threatening monotheism. The trinitarian reading asserts
 *   that Christ (Logos) and the Spirit are eternally coequal with the Father
 *   while maintaining strict monotheism through essence-unity rather than
 *   person-unity. This constraint exhibits asymmetric institutional authority
 *   (Nicene orthodoxy enforced through imperial decree, anathema, and
 *   institutional suppression) combined with a genuine coordination function
 *   (unifying Christian doctrine across the empire). The constraint's
 *   enforcement created clear victims: Arian communities, later Unitarian
 *   movements, and Oneness Pentecostals face systematic institutional
 *   suppression and exclusion. The originators and institutional
 *   beneficiaries (the Nicene Council, Orthodox institutional authority)
 *   experience the constraint as coordination. This constraint is ONE READING
 *   of a contested kernel (biblical_divine_nature); it coexists with
 *   unitarian and modalist readings held by different Christian communities.
 *   The constraint's extraction mechanism combines logical enforcement (the
 *   paradox of three-in-one requires intellectual submission) with
 *   institutional enforcement (imperial law, anathema, property confiscation,
 *   exile).
 *
 * KEY AGENTS:
 *   - Arian communities: Primary victims (powerless/identity_locked) — face imperial suppression; belief in Christ's created nature is fused with community identity and spiritual practice
 *   - Unitarian movements: Secondary victims (powerless/trapped) — rejected Trinitarian formula due to logical objection; face systematic institutional suppression across Christendom territories
 *   - Oneness Pentecostal groups: Tertiary victims (powerless/constrained) — contemporary groups rejecting Trinitarian formula face social exclusion from mainstream Christianity despite reduced legal suppression
 *   - Nicene Council Coalition: Primary beneficiary (institutional/arbitrage) — authority to define orthodoxy is the coordinating mechanism; institutional legitimacy flows from doctrinal enforcement
 *   - Imperial authority (Constantine and successors): Secondary beneficiary (institutional/arbitrage) — unified Christian doctrine serves political cohesion across the empire
 *   - Provincial clergy: Secondary actor (moderate/constrained) — pressured to assent; benefit from institutional structure but face coercion and cognitive dissonance
 *   - Liturgical apparatus: Tertiary actor (institutional/arbitrage) — institutional inertia sustains the doctrine through repetition and performative authority
 *   - Analytical observer: Meta-observer (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, 0.58).
domain_priors:suppression_score(biblical_divine_nature__trinitarian_reading, 0.72).
domain_priors:theater_ratio(biblical_divine_nature__trinitarian_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__trinitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__trinitarian_reading, "Trinitarian Dogma: Three Hypostases, One Ousia").
narrative_ontology:topic_domain(biblical_divine_nature__trinitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__trinitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__trinitarian_reading, 'trinitarian-reading-kernel-instantiation-2026').
narrative_ontology:cs_kernel_codification('trinitarian-reading-kernel-instantiation-2026', fixed_text).
narrative_ontology:cs_authority_grounding('trinitarian-reading-kernel-instantiation-2026', extraction).
narrative_ontology:cs_interpretation_layer_present('trinitarian-reading-kernel-instantiation-2026').
narrative_ontology:cs_reading_relation('trinitarian-reading-kernel-instantiation-2026', biblical_divine_nature__unitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('trinitarian-reading-kernel-instantiation-2026', biblical_divine_nature__modalist_reading, coexists_with).
narrative_ontology:cs_axiom('trinitarian-reading-kernel-instantiation-2026', foundational, three_eternally_coequal_persons).
narrative_ontology:cs_axiom_status(three_eternally_coequal_persons, holdable).
narrative_ontology:cs_axiom_grounding('trinitarian-reading-kernel-instantiation-2026', three_eternally_coequal_persons, deontological).
narrative_ontology:cs_axiom('trinitarian-reading-kernel-instantiation-2026', foundational, essence_unity_preserves_monotheism).
narrative_ontology:cs_axiom_status(essence_unity_preserves_monotheism, holdable).
narrative_ontology:cs_axiom_grounding('trinitarian-reading-kernel-instantiation-2026', essence_unity_preserves_monotheism, empirically_contingent).
narrative_ontology:cs_reference_frame('trinitarian-reading-kernel-instantiation-2026', logos_incarnation_unified_essence).
narrative_ontology:cs_drift_state('trinitarian-reading-kernel-instantiation-2026', contemporary_ecumenical_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('trinitarian-reading-kernel-instantiation-2026', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__trinitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, orthodox_institutional_authority).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, nicene_council_coalition).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, arian_communities).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, unitarian_movements).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, oneness_pentecostal_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARIAN COMMUNITY (SNARE) — Identity-locked: belief in Christ's created nature is fused with community identity, theological tradition, and spiritual practice. Structurally, Arians face imperial proscription (suppression ≥ 0.72), exile, anathema, loss of church property. Exit would require abandoning not just doctrinal position but community belonging, religious identity, and ancestral tradition. Effective extraction is maximal — the constraint extracts theological legitimacy, institutional standing, and physical safety from Arian agents while offering no coordination benefit.
constraint_indexing:constraint_classification(biblical_divine_nature__trinitarian_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(continental))).

% PERSPECTIVE 2: UNITARIAN LINEAGE (SNARE) — Trapped: belief in strict divine unity and rejection of three-person doctrine faces systematic institutional suppression — theological censure, property confiscation, legal prohibition in Christendom territories. Exit from the constraint (acceptance of Trinitarian dogma) is materially possible but carries permanent career, social, and sometimes legal consequences. Suppression is structural (imperial law, church authority, social exclusion) rather than merely cognitive. Maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(biblical_divine_nature__trinitarian_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 3: PROVINCIAL CLERGY (TANGLED ROPE) — Constrained: regional bishops and clerics face pressure to assent to Nicene doctrine but benefit from the institutional structure it provides — church authority, sacramental power, social standing, appointment security. Exit costs exist (forced assent creates cognitive dissonance; defection risks deposition) but agency remains. Genuine coordination function: the doctrine unifies Christian communities across the empire. But asymmetric extraction: institutional benefits flow to those already embedded in the orthodox hierarchy; dissenting clergy are marginalized. Mixed extractiveness and coordination.
constraint_indexing:constraint_classification(biblical_divine_nature__trinitarian_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: NICENE COUNCIL COALITION (ROPE) — Institutional beneficiaries with arbitrage exit: the council's authority to define orthodoxy is itself the coordinating mechanism. The doctrine solves a genuine coordination problem — how to maintain unified imperial Christendom across regional theological variation. From this perspective, the constraint is purely coordinative: establishing a shared doctrine enables institutional cohesion, liturgical unity, and doctrinal clarity. Extraction runs toward this agent. The beneficiary experiences the constraint as coordination, not extraction.
constraint_indexing:constraint_classification(biblical_divine_nature__trinitarian_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LITURGICAL APPARATUS (PITON) — The trinitarian formula (doxologies, creeds, sacramental language) persists through institutional inertia and liturgical tradition. Contemporary parishes often perform the doctrine without engaging its logical paradox: 'three persons, one essence' is accepted through repetition and authority rather than coherent theology. Theater ratio is high (0.65) — the performative content (reciting the creed, invoking the Trinity in prayer) is sustained through institutional force, not through living theological resolution of the three/one paradox. The constraint's coordination function has atrophied; it persists through liturgical maintenance.
constraint_indexing:constraint_classification(biblical_divine_nature__trinitarian_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal logical perspective, the three-in-one paradox is treated as a natural law of revealed theology: the doctrine is seen as a necessary consequence of combining monotheism, incarnation, and Pentecost, not as an institutional artifact. The Trinity appears as an immutable logical constraint, not as a social construction. However, the structural beneficiary/victim data reveals this as a false summit: the doctrine naturalizes what is actually a contested institutional arrangement enforced through imperial power and anathema.
constraint_indexing:constraint_classification(biblical_divine_nature__trinitarian_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__trinitarian_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biblical_divine_nature__trinitarian_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biblical_divine_nature__trinitarian_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(biblical_divine_nature__trinitarian_reading, TR),
    TR >= 0.70.

:- end_tests(biblical_divine_nature__trinitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high. The constraint extracts theological legitimacy and institutional standing from dissenting communities while providing genuine coordination benefit to the mainstream hierarchy. The extractiveness is not maximal (would be 0.72+) because the doctrine does solve a real coordination problem — harmonizing monotheism with incarnation and Pentecost across diverse communities. However, the 'solution' is asymmetric: benefits accrue to institutional authority; costs fall on those who cannot assent to the logical paradox. Suppression (0.72): High. Structural suppression is extreme in the immediate post-Nicene period (imperial decree, anathema, property confiscation, exile) and remains substantial across subsequent centuries (legal prohibition, institutional exclusion). The constraint's suppression mechanism combines structural coercion (imperial law) with internalized suppression (theological terror: rejection of Trinitarianism is anathema, ipso facto damnation). Theater ratio (0.65): Moderate-to-high. The trinitarian formula persists through liturgical repetition and institutional authority. Contemporary Christian practice treats the doctrine as essential while acknowledging the logical paradox is inexplicable—the creed is recited as performative authority rather than coherent theology. The formula's coherence increases through theological sophistication (apophatic theology argues the paradox is beyond reason) but this very move transforms the constraint from rational doctrine into performatively enforced mystery.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single doctrinal claim can appear as pure extraction (Snare from Arian perspective), mixed coordination-extraction (Tangled Rope from provincial clergy perspective), pure coordination (Rope from institutional beneficiary perspective), degraded ritual (Piton from liturgical apparatus perspective), and immutable logical necessity (Mountain from analytical observer perspective). The maximum perspectival gap opens between the Arian community (Snare: maximum experienced extraction with identity lock) and the Nicene Coalition (Rope: coordination function, arbitrage exit, institutional benefit). The gap reveals the structure: agents who benefit from the doctrine see coordination; agents whose theological coherence rejects the doctrine see pure extraction. The analytical observer risks naturalizing the constraint into logical necessity, missing the institutional enforcement mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The directional value (d) captures each agent's structural relationship to THIS reading. Arian and Unitarian communities: d → 0.95 (full targets of extraction). They bear maximum cost (institutional exclusion, suppression) with no coordination benefit from a doctrine they reject. The identity-lock component is crucial: Arian believers cannot exit because their identity is constituted through their theological rejection of Nicene subordinationism. Unitarian believers similarly cannot exit because strict monotheism is non-negotiable to their theological coherence. Provincial clergy: d → 0.60 (mixed). They benefit from institutional structures that the doctrine legitimates but face coercion to assent. They have some agency and some benefit, making them intermediate rather than full targets. Nicene Coalition: d → 0.10 (primary beneficiary). The doctrine is their authority; they experience the constraint as pure coordination. Imperial authority: d → 0.15 (secondary beneficiary, with extraction upside). They benefit from unified Christendom while using the doctrine as a control mechanism. The analytical observer at biographical time: d → 0.72 (analytical target). The observer sees the full structure—coordination function plus asymmetric extraction—and cannot escape the framework without abandoning the analytical posture itself.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION for trinitarian_reading: This constraint's classification as Tangled Rope prevents falsely categorizing it as pure Rope (pure coordination) or pure Snare (pure extraction). The doctrine has BOTH genuine coordination function (unifying Christian communities, solving theological coherence problems through institutional authority) AND asymmetric extraction (victims face suppression, exclusion, identity lock). The mandatrophy is resolved by recognizing: (1) the coordination function is real—the Nicene framework does harmonize trinitarian theology across diverse communities; (2) the extraction mechanism is also real—the harmonization is enforced through coercion, anathema, and institutional punishment against those who cannot or will not assent; (3) these two mechanisms are not separable—the coordination function creates legitimacy for the extraction mechanism, and the extraction mechanism sustains the coordination function. The analytical observer's Mountain perspective is itself mandatrophic: it risks naturalizing contingent institutional enforcement as immutable logical necessity. The FSM (False Summit Mountain) signature will fire on this story because it declares beneficiaries + institutional enforcement + clear victims. The engine will reclassify the Mountain perspective as Tangled Rope, revealing that 'inherent to logic' is actually 'enforced by institutional authority.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    logical_coherence_vs_dogmatic_necessity,
    'Is the trinitarian formula (three persons, one ousia) a logically coherent theological statement or a performatively enforced paradox that coheres through authority rather than logic?',
    'Systematic examination of substantia/hypostasis/ousia terminology evolution; correlation between theological coherence (apophatic vs cataphatic approaches) and institutional enforcement intensity; analysis of alternative formulations (modalism, monarchianism) and their logical status',
    'If coherent: the constraint is a genuine logical discovery warranting enforced assent. If paradoxical: the constraint''s enforcement mechanism is what creates its appearance of necessity — reclassifies from mountain to tangled_rope across all perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(logical_coherence_vs_dogmatic_necessity, conceptual, 'Whether trinitarian formula is logically coherent or performatively enforced paradox').

omega_variable(
    reading_foreclosure_structure,
    'Does the trinitarian reading logically foreclose alternative readings (Arian, Unitarian, Modalist) within a single commitment framework, or do these readings coexist across different institutional parties?',
    'Analysis of the Nicene Council''s stated intent (definition vs. prohibition); historical documentation of simultaneous adherence to multiple readings in different Christian communities; examination of whether rejection of Arianism required accepting Nicene Trinity or merely accepting Christ''s equality of honor (σύντιμος vs. σύνουσιος distinction)',
    'If foreclosure: the trinitarian reading eliminates logical space for alternatives in any coherent framework (rare structural condition). If coexistence: the readings are held by different parties simultaneously; the constraint''s enforcement mechanism creates the appearance of mutual exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Whether trinitarian reading forecloses alternatives or coexists with them').

omega_variable(
    semantic_stability_ousia,
    'Did ousia (essence/substance) carry a stable meaning across pre-Nicene, Nicene, and post-Nicene periods, or did the doctrine impose a new semantic regime that retroactively unified otherwise divergent concepts?',
    'Philological analysis of ousia usage in pre-Christian philosophy (Aristotle, Stoics, Neoplatonism); comparison with patristic exegesis before and after Nicaea; examination of whether Athanasius and the Council invented or discovered the ousia/hypostasis distinction',
    'If semantic continuity: the doctrine represented a unified theological commitment across centuries. If semantic innovation: the constraint imposed a new conceptual regime — reclassifies as more extractive (higher epsilon) because the constraint requires reinterpreting prior theological tradition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(semantic_stability_ousia, empirical, 'Whether ousia maintained semantic stability or was semantically innovated by Nicene doctrine').

omega_variable(
    suppression_mechanism_source,
    'What proportion of suppression (0.72) is structural (imperial law, institutional authority) versus internalized (theological terror, fear of anathema, belief that rejection is theologically impossible)?',
    'Historical analysis of Arian communities that persisted despite imperial suppression (Germanic Arian kingdoms, Nestorian communities); examination of post-suppression theological reassessment (did communities abandon Arian theology when legal pressure ceased, or did they maintain it?); study of contemporary Arian sympathizers to identify which barriers are material vs. internalized',
    'If suppression is primarily structural: removing imperial enforcement (as happened after Constantine) should have enabled Arian resurgence. Historical persistence of Trinitarianism despite reduced enforcement suggests internalized suppression is significant — the constraint''s binding mechanism operates through cognitive capture, not merely material coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_source, empirical, 'Proportion of suppression that is structural versus internalized').

omega_variable(
    this_reading_as_kernel_instantiation,
    'This constraint (trinitarian_reading) is one reading of the kernel ''biblical_divine_nature''. What makes THIS reading the reading you are authoring rather than the unitarian_reading or modalist_reading?',
    'The three readings occupy different institutional spaces: Trinitarian dominates Western/Eastern Orthodox mainstream; Unitarian emerges in Reformation-era radical movements; Modalist appears in certain Pentecostal traditions and early Christianity. Each reading has its own epsilon, its own suppression profile, and its own perpektival gap.',
    'Recognizing this as a kernel reading prevents false unification. The constraint_biblical_divine_nature__trinitarian_reading is NOT ''the divine nature constraint.'' It is ''the trinitarian reading of the divine nature kernel.'' The unitarian and modalist readings are separate constraints with potentially different ε values and enforcement mechanisms. The three readings do not collapse into a single constraint across observables — they ARE observably distinct in institutional practice, enforcement intensity, and victim/beneficiary structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(this_reading_as_kernel_instantiation, conceptual, 'Kernel structure: trinitarian reading as one instantiation of contested biblical_divine_nature kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__trinitarian_reading, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trin_theater_early_nicene, biblical_divine_nature__trinitarian_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(trin_theater_medieval, biblical_divine_nature__trinitarian_reading, theater_ratio, 200, 0.58).
narrative_ontology:measurement(trin_theater_modern, biblical_divine_nature__trinitarian_reading, theater_ratio, 1700, 0.65).

% Extraction over time
narrative_ontology:measurement(trin_extract_early_nicene, biblical_divine_nature__trinitarian_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(trin_extract_medieval, biblical_divine_nature__trinitarian_reading, base_extractiveness, 200, 0.56).
narrative_ontology:measurement(trin_extract_modern, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1700, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(trin_suppress_early_nicene, biblical_divine_nature__trinitarian_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(trin_suppress_medieval, biblical_divine_nature__trinitarian_reading, suppression_requirement, 200, 0.75).
narrative_ontology:measurement(trin_suppress_modern, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1700, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__trinitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__trinitarian_reading, 0.12).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, biblical_divine_nature__unitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, biblical_divine_nature__modalist_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, arianism_institutional_suppression).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, nicene_imperial_authority).

% DUAL FORMULATION NOTE:
% The trinitarian reading is one of three readings of the biblical_divine_nature kernel. The unitarian and modalist readings are separate constraints with distinct epsilon values and enforcement mechanisms. Each reading emerges from the same scriptural material but interprets it through different theological frameworks. The three readings coexist across different Christian communities and represent structurally distinct constraints, not observationally dependent variations of a single constraint. Network links establish that reading the Trinity requires understanding its alternatives—the unitarian and modalist readings are not rivals to be defeated but structural alternatives that reveal what is at stake in each reading's choice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_divine_nature__trinitarian_reading, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
