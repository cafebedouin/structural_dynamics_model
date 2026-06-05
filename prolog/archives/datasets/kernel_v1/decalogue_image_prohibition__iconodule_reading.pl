% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconodule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__iconodule_reading, []).

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
 *   constraint_id: decalogue_image_prohibition__iconodule_reading
 *   human_readable: Icon Veneration as Permitted Material Mediation (Iconodule Reading)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   The Iconodule reading of the Decalogue's image prohibition represents one
 *   pole of a millennia-long theological dispute within Christianity. This
 *   reading asserts that the Incarnation—God's assumption of material flesh
 *   in Christ—sanctifies matter as a valid and permissible conduit to the
 *   divine. Therefore, visual images (icons) of Christ, the Virgin Mary, and
 *   the saints can be honored and venerated, not as objects of worship in
 *   themselves (latria, reserved for God alone) but as media through which
 *   honor flows toward their prototypes (dulia, a permissible form of
 *   reverence). The constraint permits icon creation, display, and veneration
 *   under careful doctrinal boundaries. The sibling reading (iconoclast)
 *   forbids such images entirely, treating them as violations of the
 *   prohibition regardless of intent. This constraint story captures only the
 *   iconodule reading—the permissive interpretation and its structural logic.
 *   The iconoclast reading, with its opposite permissibility judgment and
 *   different victim/beneficiary structure, is a separate constraint story in
 *   the same kernel family.
 *
 * KEY AGENTS:
 *   - Orthodox Ecclesiastical Authority: Primary beneficiary (institutional/arbitrage) — gains pastoral function and doctrinal coherence through legitimate icon system
 *   - Icon Painters and Artisan Guilds: Secondary beneficiary (powerful/mobile) — gain sacred legitimacy for their craft; maintain high-status employment through liturgical demand
 *   - Lay Devotional Practitioners: Beneficiary (moderate/constrained) — gain direct access to devotional practice; spiritual identity constituted through permitted forms
 *   - Icon Theologians and Doctrinal Framers: Mixed (moderate/identity_locked) — provide necessary coordination (boundary-maintenance between latria and dulia) but bear interpretive labor and vulnerability to heresy accusations
 *   - Lay Practitioners Under Iconoclast Enforcement: Victim (powerless/trapped) — when iconoclast authority suppresses this reading, these practitioners face forced conformity, hidden practice, or exile
 *   - The Theological Tradition (as embodied in ecumenical councils): Authority (institutional/arbitrage) — codifies and maintains the reading through formal doctrinal pronouncement; benefits from authoritative status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.28).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.32).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Icon Veneration as Permitted Material Mediation (Iconodule Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, 'ede47094-f3c7-407e-b120-fccd14f3ecdc').
narrative_ontology:cs_kernel_codification('ede47094-f3c7-407e-b120-fccd14f3ecdc', fixed_text).
narrative_ontology:cs_authority_grounding('ede47094-f3c7-407e-b120-fccd14f3ecdc', lineage).
narrative_ontology:cs_interpretation_layer_present('ede47094-f3c7-407e-b120-fccd14f3ecdc').
narrative_ontology:cs_reading_relation('ede47094-f3c7-407e-b120-fccd14f3ecdc', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('ede47094-f3c7-407e-b120-fccd14f3ecdc', foundational, incarnation_sanctifies_matter).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter, holdable).
narrative_ontology:cs_axiom_grounding('ede47094-f3c7-407e-b120-fccd14f3ecdc', incarnation_sanctifies_matter, deontological).
narrative_ontology:cs_axiom('ede47094-f3c7-407e-b120-fccd14f3ecdc', foundational, latria_dulia_distinction_maintainable).
narrative_ontology:cs_axiom_status(latria_dulia_distinction_maintainable, holdable).
narrative_ontology:cs_axiom_grounding('ede47094-f3c7-407e-b120-fccd14f3ecdc', latria_dulia_distinction_maintainable, conventional).
narrative_ontology:cs_reference_frame('ede47094-f3c7-407e-b120-fccd14f3ecdc', incarnational_material_redemption).
narrative_ontology:cs_drift_state('ede47094-f3c7-407e-b120-fccd14f3ecdc', contemporary_reformation_contestation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ede47094-f3c7-407e-b120-fccd14f3ecdc', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, orthodox_liturgical_practice).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_painters_guild).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, lay_devotional_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORTHODOX ECCLESIASTICAL AUTHORITY (ROPE) — Benefits from icon veneration as a coordination mechanism that structures lay access to the divine without requiring clerical mediation for every act of prayer. The constraint enables the church's pastoral function and reinforces doctrinal coherence around the Incarnation. Net coordination, minimal extraction — the authority benefits from a functioning system, not from suppression.
constraint_indexing:constraint_classification(decalogue_image_prohibition__iconodule_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 2: ICON PAINTERS AND ARTISAN GUILDS (ROPE) — Benefit from liturgical demand for icons; the constraint legitimizes their craft as sacred work (not mere decoration). Coordination function is genuine: the rule specifies proper theological iconography, enabling quality standards and guild reputation. Exit options exist (secular art production) but are not exercised because icon painting offers both income and spiritual coherence. Mobile exit, but mobile toward staying.
constraint_indexing:constraint_classification(decalogue_image_prohibition__iconodule_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: LAY DEVOTIONAL PRACTITIONERS (ROPE) — Gain direct access to devotional practice through permitted icon veneration; the constraint enables rather than restricts. Exit option exists (non-liturgical prayer, direct contemplation) but is not experienced as available within the orthodox framework — spiritual identity is constituted through permitted forms. Constrained but benefiting from the constraint, not victimized by it.
constraint_indexing:constraint_classification(decalogue_image_prohibition__iconodule_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ICON THEOLOGIANS AND DOCTRINAL FRAMERS (TANGLED ROPE) — Bear the intellectual and interpretive labor of maintaining the boundary between latria (worship toward God alone) and dulia (honor toward the saint's prototype via the image). This boundary-maintenance work is genuinely necessary coordination — prevents the constraint from collapsing into idolatry — but also extracts from these agents in the form of continuous doctrinal vigilance, defense against heresy charges, and responsibility for distinguishing licit from illicit practice. Identity-locked because the theologian's entire intellectual tradition and authority derives from managing this distinction.
constraint_indexing:constraint_classification(decalogue_image_prohibition__iconodule_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 5: LAY PRACTITIONERS UNDER ICONOCLAST ENFORCEMENT (SNARE) — When the iconodule reading is suppressed by iconoclast authority, the same lay practitioners cannot practice their faith according to the iconodule interpretation. They bear the extraction cost: either hidden practice (spiritual guilt, risk of exposure), conformity (identity split), or exit from the community (exile or apostasy). This perspective represents the constraint under reversed authority — not the iconodule reading's own experience, but what happens to its adherents when opposition rules.
constraint_indexing:constraint_classification(decalogue_image_prohibition__iconodule_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INCARNATIONAL LOGIC (MOUNTAIN) — From the standpoint of Christian theological logic, once the Incarnation is accepted as a premise (God becoming matter in Christ), the icon constraint follows as a logical consequence: matter is now a valid conduit to the divine. The constraint appears immutable under this logical structure. However, this is a false summit — the logical consequence depends on contested theological premises (the Incarnation itself, the doctrine of theosis) and on interpretive decisions about what the Incarnation entails for material representation. The engine will flag this as a natural-law appearance with identifiable beneficiaries (the theological authority structure).
constraint_indexing:constraint_classification(decalogue_image_prohibition__iconodule_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconodule_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(decalogue_image_prohibition__iconodule_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(decalogue_image_prohibition__iconodule_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(decalogue_image_prohibition__iconodule_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The constraint enables rather than restricts the primary beneficiaries (ecclesiastical authority, icon painters, lay practitioners). The coordination function is genuine—the latria/dulia boundary provides a rule system that prevents icon veneration from collapsing into idolatry. Some extraction does occur in the form of theological boundary-maintenance labor borne by doctrinal framers, and in the implicit requirement that lay practitioners adopt the church's interpretation of proper intent. But the base extraction is not high because the constraint appears to most agents as enabling rather than extracting. The primary extraction flow is toward the ecclesiastical authority, which benefits from a functioning system that structures lay access to the divine. Suppression (0.32): Moderate. The constraint requires doctrinal training to maintain (understanding the latria/dulia distinction), specialist validation (bishops and theologians must approve icons), and enforcement against heterodox interpretations (suppressing iconoclast readings when iconodule authority holds). But suppression is not total because the rule permits substantive practice once the theological framework is accepted. Theater ratio (0.41): Moderate. Icon veneration involves performative elements—the formalized gesture of honor, the prescribed prayers, the ritual context—but these are not disconnected from function. The theater serves the coordination function by providing standardized forms through which lay practitioners can direct their devotion. The theater ratio remains moderate rather than high because the connection between form and theological meaning is close; the ritual is not purely theatrical even though it has theatrical elements.
 *
 * PERSPECTIVAL GAP:
 *   The iconodule reading creates minimal perspectival gap because the constraint appears as enabling to all its primary agents. The ecclesiastical authority sees coordination and legitimacy. Icon painters see sacred craft and demand. Lay practitioners see access to devotion. The only significant perspectival gap occurs when iconoclast authority suppresses the reading—then lay practitioners who understood themselves as venerators suddenly experience the constraint as a snare (forced conformity or exile). The analytical observer risks collapsing into a false summit, naturalizing the Incarnational doctrine as a logical law when it is a contested theological premise. The gap between the rope classification (most agents' experience) and the false-summit mountain classification (analytical risk) reveals that the constraint's legitimacy depends on acceptance of specific Christian doctrinal commitments, not on universal logical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality (d) from the agent's beneficiary/victim status and exit options. Ecclesiastical authority appears as beneficiary with arbitrage exit (can opt out of the icon system, can reorganize the church without it)—low d, negative effective extraction. Icon painters appear as beneficiary with mobile exit (could paint secular art, but choose icon work for its legitimacy and income)—low d, slightly negative. Lay practitioners appear as beneficiary with constrained exit (cannot exit the orthodoxy without community departure, but benefit within it)—low-moderate d, near-zero extraction. Icon theologians appear as mixed (both benefit from authority derived from the tradition AND bear interpretive labor that could be avoided by adopting simpler readings)—moderate d, producing moderate extraction. The analytical observer at civilizational scope would derive high d if treating the theological tradition itself as trapped by logical necessity—but this is a false summit (the Incarnation is a contested premise, not a logical law). The constraint's overall directionality is beneficiary-weighted: net flow of benefit toward institutional authority, with moderate costs distributed across boundary-maintenance workers and identity-locked practitioners.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy—it is a stable rope that coordinates practice without requiring masking or substitution. The boundary-maintenance labor (latria vs dulia) is genuine work with genuine function. The beneficiaries experience real coordination benefit, not extracted surplus masked as coordination. The suppression is moderate because the rule can be followed without high coercion once the theological framework is internalized. The theater ratio is moderate because the ritual forms (icon placement, prayer gesture, liturgical integration) genuinely serve the coordination function rather than replacing it. Where mandatrophy would emerge is in the false summit risk—if an observer naturalizes the constraint as a mountain (inherent to Christian logic itself), they are substituting the appearance of logical necessity for the actual contingency of theological interpretation. That is a form of theater at the analytical level: making a rope appear as a mountain through rhetorical necessity claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latria_dulia_boundary_detectability,
    'Is the boundary between worship directed toward God through the image (latria via image, forbidden) and honor directed toward the prototype (dulia toward saint, permitted) empirically distinguishable in actual practice?',
    'Ethnographic observation of icon veneration; interview data on worshiper intent; analysis of liturgical formulae and prayer direction in practice vs. doctrinal specification',
    'If boundary is reliably maintained in practice: constraint is genuine rope (coordination with low extraction). If boundary collapses or is widely violated: constraint is theater (piton, with high suppression required to maintain it). If boundary is cognitively meaningful but behaviorally indistinguishable: constraint is identity_locked rope (intent-based rather than externally verifiable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latria_dulia_boundary_detectability, empirical, 'Whether the latria/dulia boundary is empirically distinguishable in practice').

omega_variable(
    incarnational_premise_necessity,
    'Is the Incarnation a logically necessary premise for permitting icon veneration, or is icon permission consistent with other Christologies (e.g., docetic, adoptionist)?',
    'Comparative theological analysis: document which Christologies or theological traditions permit or forbid image veneration; test whether non-incarnational frameworks can derive the same permission rule',
    'If Incarnation is strictly necessary: the constraint''s foundation is a contested theological commitment (not a mountain). If other frameworks permit icons: the boundary between readings is more ideological than logical (shifts reading_relations from forecloses toward coexists_with). If Incarnation is necessary but optional: constraint becomes paradoxical (false summit with high confidence).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incarnational_premise_necessity, conceptual, 'Whether Incarnational doctrine is strictly necessary for icon permission').

omega_variable(
    iconoclast_suppression_mechanism,
    'When iconoclast authority suppresses iconodule practice, is the suppression rooted in the iconoclast reading''s intrinsic logic, or is it a secondary political/institutional enforcement mechanism?',
    'Historical analysis of iconoclast policy documents and theological justifications; comparison of suppression intensity during different theological periods; test whether iconoclast authority employs rhetorical but not punitive enforcement when politically secure',
    'If rooted in theology alone: both readings are logically foreclosing (each rules out the other). If political/institutional: suppression is contingent (readings coexist, but enforcement asymmetries favor one during certain periods). If purely rhetorical under security: readings are deeply coexisting (suppression is theater, not structural enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iconoclast_suppression_mechanism, empirical, 'Whether iconoclast suppression is doctrinally necessary or politically contingent').

omega_variable(
    kernel_interpretation_authority,
    'Who has legitimate authority to interpret the Decalogue''s image prohibition — the original text itself, scriptural tradition, ecumenical councils, local bishops, or the reading community''s collective understanding?',
    'Genealogy of interpretation authority within Christianity; analysis of which authorities are cited to justify each reading; test whether readings claim different grounds of legitimacy or merely dispute the same ground',
    'If readings rely on different authority sources: constraint decomposes into multiple readings with distinct authority_grounding values (each with own cs_structure). If readings cite the same authorities but diverge in interpretation: constraint is single kernel with distributed authority_grounding. If one reading claims exclusive authority: reading_relations shift toward forecloses (from coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_interpretation_authority, conceptual, 'Who holds legitimate authority to interpret the Decalogue''s image prohibition').

omega_variable(
    materiality_sanctification_extent,
    'Does the Incarnation sanctify matter universally, or only certain matter under certain conditions (icons painted by believers, with proper intent, depicting orthodoxly-approved subjects)?',
    'Theological texts on theosis doctrine; analysis of which material forms are and aren''t icons in iconodule theology; test whether the constraint permits image worship of non-religious subjects, or restricts to religious figures only',
    'If universal: the constraint dissolves (all images are mediums, none are forbidden). If conditional: the constraint becomes a complex rule system (rope with high theater—lots of boundary-maintenance work). If the conditions are purely doctrinal and not material: the constraint is enforcement-heavy (high suppression required).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(materiality_sanctification_extent, conceptual, 'Scope and conditions of matter''s sanctification via Incarnation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(icon_dule_tr_t0, decalogue_image_prohibition__iconodule_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(icon_dule_tr_t200, decalogue_image_prohibition__iconodule_reading, theater_ratio, 200, 0.39).
narrative_ontology:measurement(icon_dule_tr_t400, decalogue_image_prohibition__iconodule_reading, theater_ratio, 400, 0.41).

% Extraction over time
narrative_ontology:measurement(icon_dule_be_t0, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(icon_dule_be_t200, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 200, 0.26).
narrative_ontology:measurement(icon_dule_be_t400, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 400, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__iconodule_reading, 0.12).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__iconoclast_reading).

% DUAL FORMULATION NOTE:
% The Decalogue image prohibition kernel splits into two mutually foreclosing readings: iconodule_reading (this file) and iconoclast_reading (sibling). Each reading has distinct ε, distinct beneficiaries, distinct suppression mechanisms. The iconodule reading permits images under doctrinal conditions (ε=0.28, rope); the iconoclast reading forbids all images (separate constraint with higher ε and snare structure). The readings coexist historically across different Christian institutional actors but foreclose each other within any single framework that adopts one. Network link records the mutual dependence: iconodule reading's coherence depends partly on addressing iconoclast critiques; iconoclast reading's definition partly in opposition to iconodule practices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decalogue_image_prohibition__iconodule_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
