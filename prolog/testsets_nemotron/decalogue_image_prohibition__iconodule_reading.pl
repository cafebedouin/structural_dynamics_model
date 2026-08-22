% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconodule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: decalogue_image_prohibition__iconodule_reading
 *   human_readable: Iconodule Reading of the Decalogue Image Prohibition (Dulia Permitted)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   The iconodule reading of the Decalogue's image prohibition (Exodus
 *   20:4–5) distinguishes between latria (worship due to God alone) and dulia
 *   (veneration/honor rendered to saints and through their images to the
 *   prototype). The Incarnation — God becoming visible in Christ — is the
 *   theological hinge: if the invisible Word became tangible flesh, matter
 *   can bear the divine. The Second Council of Nicaea (787) and the Triumph
 *   of Orthodoxy (843) established this reading as binding. The constraint
 *   coordinates a global visual culture: every Orthodox church has an
 *   iconostasis, every home an icon corner, every feast its icon. Extraction
 *   is low — the constraint enables rather than takes — but enforcement was
 *   historically intense during iconoclast periods. The claimed type is rope:
 *   genuine coordination with minimal coercive overhead in its stable state,
 *   though the interval captures the violent suppression phases when the
 *   opposing reading held power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.15).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.05).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Iconodule Reading of the Decalogue Image Prohibition (Dulia Permitted)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconodule_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, '82d83ba0-5b69-4c5b-9734-d38a39500c9c').
narrative_ontology:cs_kernel_codification('82d83ba0-5b69-4c5b-9734-d38a39500c9c', fixed_text).
narrative_ontology:cs_authority_grounding('82d83ba0-5b69-4c5b-9734-d38a39500c9c', lineage).
narrative_ontology:cs_interpretation_layer_present('82d83ba0-5b69-4c5b-9734-d38a39500c9c').
narrative_ontology:cs_reading_relation('82d83ba0-5b69-4c5b-9734-d38a39500c9c', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_reading_relation('82d83ba0-5b69-4c5b-9734-d38a39500c9c', decalogue_image_prohibition__moderate_iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('82d83ba0-5b69-4c5b-9734-d38a39500c9c', foundational, incarnation_sanctifies_matter_for_veneration).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter_for_veneration, holdable).
narrative_ontology:cs_axiom_grounding('82d83ba0-5b69-4c5b-9734-d38a39500c9c', incarnation_sanctifies_matter_for_veneration, deontological).
narrative_ontology:cs_axiom('82d83ba0-5b69-4c5b-9734-d38a39500c9c', foundational, latria_dulia_distinction_preserves_commandment).
narrative_ontology:cs_axiom_status(latria_dulia_distinction_preserves_commandment, holdable).
narrative_ontology:cs_axiom_grounding('82d83ba0-5b69-4c5b-9734-d38a39500c9c', latria_dulia_distinction_preserves_commandment, deontological).
narrative_ontology:cs_axiom('82d83ba0-5b69-4c5b-9734-d38a39500c9c', secondary, icon_honors_prototype_not_material).
narrative_ontology:cs_axiom_status(icon_honors_prototype_not_material, holdable).
narrative_ontology:cs_axiom_grounding('82d83ba0-5b69-4c5b-9734-d38a39500c9c', icon_honors_prototype_not_material, deontological).
narrative_ontology:cs_reference_frame('82d83ba0-5b69-4c5b-9734-d38a39500c9c', nicene_christology_applied_to_decalogue).
narrative_ontology:cs_drift_state('82d83ba0-5b69-4c5b-9734-d38a39500c9c', triumph_of_orthodoxy_843, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('82d83ba0-5b69-4c5b-9734-d38a39500c9c', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, lay_venerators).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_painters_guilds).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, monastic_image_producers).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, patriarchal_liturgical_authority).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, destroyed_icons).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, suppressed_veneration_practices).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, persecuted_iconodules_under_iconoclast_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, icon_painters_guilds).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordinary Christians who venerate icons in churches and homes as windows to the divine. They receive spiritual coordination through sanctioned visual mediation — kissing icons, lighting candles before them, processing with them. Their exit from this practice would mean losing the primary sensory access to the holy their tradition provides. Under iconoclast enforcement they faced persecution, but the iconodule reading restores and protects their practice.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, lay_venerators, beneficiary,
    moderate, biographical, constrained, global).

% Craftspeople who produce icons according to strict canonical rules (canons of the Quinisext Council, later patriarchal typika). They benefit from protected professional status and patronage, but bear the cost of rigorous technical and theological training, submission to episcopal review, and the constraint that innovation in depiction is forbidden — the image must conform to the prototype.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_painters_guilds, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconodule_reading, icon_painters_guilds, payer).

% Monasteries (especially St. Catherine's Sinai, Athos, Studion) that became centers of icon production. For them, icon-painting is not merely a craft but a spiritual discipline — the painter fasts and prays while working. Their identity is fused to the practice; exit would mean abandoning their charism. They benefit from the reading's legitimization of their vocation.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, monastic_image_producers, beneficiary,
    moderate, generational, identity_locked, global).

% The ecumenical patriarchate and autocephalous hierarchs who define orthodoxy, approve iconographic programs, and enforce the latria/dulia boundary through councils and typika. They set the agenda: which images are canonical, which feasts require icons, how veneration is properly performed. They hold the interpretive keys to the reading and can adjust the coordination parameters (e.g., the 787/843 Triumph of Orthodoxy settled the boundary).
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, patriarchal_liturgical_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Imperial and ecclesiastical authorities during iconoclast periods (726–787, 814–843) who destroyed icons, whitewashed churches, persecuted venerators, and imposed the iconoclast reading by force. They are structurally excluded from the iconodule framework — their position is that the constraint itself is idolatry. Under iconodule restoration they were anathematized; their exit from power was the condition for the reading's victory.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconoclast_enforcers, excluded,
    institutional, biographical, trapped, regional).

% The physical icons smashed, burned, or scraped from walls during iconoclast enforcement. They bear the extraction of the opposing reading — their destruction is the suppression cost paid when the iconodule reading is not in force. They are not agents but are the material record of the constraint's victim set under iconoclast regimes.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, destroyed_icons, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(decalogue_image_prohibition__iconodule_reading, destroyed_icons).

% The liturgical and devotional practices (processions, prostrations, anointing icons with oil) that were banned under iconoclast enforcement. Their suppression is the functional extraction borne by the laity when the iconodule reading is displaced. The practices themselves have no agency but constitute the coordination function the reading protects.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, suppressed_veneration_practices, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(decalogue_image_prohibition__iconodule_reading, suppressed_veneration_practices).

% Monks, clergy, and laity tortured, exiled, or executed for venerating icons during iconoclast periods (e.g., Stephen the Younger, Theodore the Studite, Theophanes the Confessor). They are the human victim set of the opposing reading's enforcement. Under the iconodule reading they are canonized as martyrs and confessors — their suffering vindicates the reading's claim to be the true coordination structure.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, persecuted_iconodules_under_iconoclast_regimes, payer,
    powerless, biographical, trapped, regional).

% Scholars who study the icon controversy as a structural conflict over material mediation, authority, and the Incarnation's implications. They see both readings as live historical options and analyze the coordination/extraction dynamics without partisan commitment.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, historical_theologian, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the sensory access of the laity to the divine through material images that are theologically sanctioned as conduits to their prototypes. Solves the problem: how can embodied humans worship an invisible God? Answer: through the Incarnation, matter becomes a valid medium; the icon makes the prototype present without collapsing the distinction.
% TRANSFER_FUNCTION: Moves devotional attention, labor (icon production), and institutional authority from the center (patriarchal/council definition of orthodoxy) to the periphery (every church and home). The patriarchal authority sets the canon; icon painters execute it; lay people receive the coordinated access. No monetary extraction — the transfer is authority and grace, not rent.
% ABSENT_VOICES: The iconoclast reading's adherents (imperial theologians like John of Grammaton, patriarchs like Anthony I Kassymatas) are structurally excluded from the iconodule framework — they would object that ANY material veneration is idolatry. Their voices are preserved only in the refutations written against them (Nicea II acts, Theodore the Studite's Antirrhetics). The moderate iconoclast position (2D images permitted, 3D forbidden) is also excluded — it was never a stable equilibrium.
% DISAPPEARANCE_RATIONALE: If the iconodule reading vanished overnight, the entire sensory architecture of Eastern Christian worship would collapse: iconostases would be emptied, processions would cease, the liturgical year's visual rhythm would disappear, and the laity would lose their primary mediated access to the saints and Christ. The coordination function is not decorative — it is the operating system of Orthodox piety. The world rearranges because the constraint carries the weight of sacramental theology.
% FOUNDING_PROBLEM: How can the Second Commandment's prohibition of graven images be reconciled with the Incarnation's implication that the invisible God became visible, tangible matter? The founding problem is the tension between biblical aniconism and christological materialism — resolved by the latria/dulia distinction and the doctrine that the icon honors the prototype, not the wood and paint.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live in contemporary ecumenical dialogue: Roman Catholics (post-Trent, Vatican II) affirm the distinction but with different theological machinery; Protestants largely reject it (Calvin's Institutes 1.11.12–15). The corroboration that the problem is structural and unresolved comes from outside the Orthodox beneficiary set — from the persistent division of Christendom on this axis. No non-Orthodox tradition has adopted the full iconodule reading; the founding problem persists across the ecclesiastical boundary.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconodule_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconodule_reading_tests).
:- end_tests(decalogue_image_prohibition__iconodule_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.15 at interval end) because the constraint does not extract resources from participants — it channels devotion. The victims listed are victims of the *opposing* reading's enforcement (iconoclast periods), not of the iconodule reading itself. This is a constraint family where the reading's extractiveness is measured relative to its own operation: when the iconodule reading is in force, it functions as rope; when the iconoclast reading is in force, the iconodule reading's beneficiaries become victims. The measurement series captures this oscillation. Theater ratio is low (0.12) because the liturgical function is genuine — icons are not performative but sacramentally operative. Accessibility collapse is moderate (0.3) because alternatives (word-only worship, mental prayer) exist and are practiced (e.g., hesychasm), but the iconodule reading claims they are incomplete without material mediation. Resistance is low (0.2) because the reading's beneficiaries embrace it; resistance comes from the excluded iconoclast position.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (patriarchal authority), the constraint is pure coordination: they define the canon, the laity receives grace. From the lay venerator seat, it is a gift — sensory access to the holy. From the icon painter seat, it is a disciplined vocation with costs (canonical conformity) and benefits (protected status). From the excluded iconoclast seat, the SAME constraint is a snare — idolatry enforced by councils. The engine computes these divergent per-seat types from the structural data; the iconodule reading claims rope, the iconoclast reading would claim snare. The divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The patriarchal authority (agenda_setter, d ≈ 0.1) benefits by holding the interpretive keys and liturgical center. Lay venerators (beneficiary, d ≈ 0.2) receive coordination with minimal cost. Icon painters (beneficiary/payer, d ≈ 0.35) bear canonical conformity costs but gain protected vocation. Monastic producers (beneficiary, identity_locked, d ≈ 0.25) are fused to the practice — exit is vocation-death. Iconoclast enforcers (excluded, trapped) are the structural counter-position — their exclusion is the condition of the reading's coherence. The destroyed icons and suppressed practices are non-agent victims of the opposing enforcement, recorded here to document the constraint family's extraction asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's mandate (material mediation of the divine via Incarnation) has not atrophied — it remains the operating theology of Eastern Orthodoxy and the basis for its visual culture. The founding problem (reconciling aniconism with Incarnation) is still live across Christendom. No mandatrophy: the constraint continues to solve the coordination problem it was built for. The theater ratio's historical spikes (during iconoclast reactions) were defensive, not performative decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_committer_structure,
    'How does the iconodule reading''s structural relationship to the decalogue_image_prohibition kernel differ from its siblings, and what classification consequences follow?',
    'Comparative analysis of all three readings'' ε, beneficiary/victim sets, and coordination functions using the constraint family method (BGS pattern). Each reading gets its own constraint story; the engine computes per-seat types from structural data.',
    'If the iconodule reading computes as rope while iconoclast reads as snare (high extraction, victims = venerators), the kernel''s contest is revealed as a genuine structural divergence, not a semantic dispute. The false_summit_mountain check does not apply (claimed_type is rope, not mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_structure, conceptual, 'Committer-frame structural delta: this reading''s ε is low (coordination), siblings'' ε would be high (extraction/suppression). The kernel is the contested referent; readings are distinct constraints.').

omega_variable(
    incarnation_as_coordination_ground,
    'Is the Incarnation''s sanctification of matter a genuine coordination ground (making the constraint a rope) or a theological cover for material practice that could be abandoned without loss?',
    'Counterfactual: if icons were removed from Orthodox worship tomorrow, would the liturgical and devotional system lose irreducible coordination capacity? Hesychast tradition (pure mental prayer) exists as a control — compare outcomes.',
    'If the Incarnation ground is indispensable, the constraint is structural rope. If hesychasm achieves equivalent coordination without images, the iconodule reading''s coordination claim is partial and the constraint may be tangled_rope (coordination + some extraction from those who cannot practice hesychasm).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incarnation_as_coordination_ground, empirical, 'Whether the theological ground (Incarnation → matter as conduit) is functionally necessary for the coordination the constraint provides.').

omega_variable(
    latria_dulia_operational_boundary,
    'Can the latria/dulia distinction be operationally maintained in popular practice, or does it collapse into de facto image-worship (making the constraint extractive by deceiving participants about their own practice)?',
    'Ethnographic study of lay veneration behavior: do practitioners consciously distinguish honor-to-prototype from worship-of-image? Compare with theological catechesis.',
    'If the distinction collapses in practice, the constraint extracts devotion under false pretenses — effective extraction rises, potentially shifting classification toward tangled_rope. If maintained, the coordination is genuine and the reading''s claim holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(latria_dulia_operational_boundary, empirical, 'Operational fidelity of the theological distinction that legitimizes the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 726, 843).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t726, decalogue_image_prohibition__iconodule_reading, theater_ratio, 726, 0.25).
narrative_ontology:measurement(deca_tr_t754, decalogue_image_prohibition__iconodule_reading, theater_ratio, 754, 0.2).
narrative_ontology:measurement(deca_tr_t787, decalogue_image_prohibition__iconodule_reading, theater_ratio, 787, 0.1).
narrative_ontology:measurement(deca_tr_t814, decalogue_image_prohibition__iconodule_reading, theater_ratio, 814, 0.3).
narrative_ontology:measurement(deca_tr_t826, decalogue_image_prohibition__iconodule_reading, theater_ratio, 826, 0.22).
narrative_ontology:measurement(deca_tr_t843, decalogue_image_prohibition__iconodule_reading, theater_ratio, 843, 0.12).

% Extraction over time
narrative_ontology:measurement(deca_be_t726, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 726, 0.45).
narrative_ontology:measurement(deca_be_t754, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 754, 0.35).
narrative_ontology:measurement(deca_be_t787, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 787, 0.18).
narrative_ontology:measurement(deca_be_t814, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 814, 0.55).
narrative_ontology:measurement(deca_be_t826, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 826, 0.45).
narrative_ontology:measurement(deca_be_t843, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 843, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t726, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 726, 0.8).
narrative_ontology:measurement(deca_su_t754, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 754, 0.7).
narrative_ontology:measurement(deca_su_t787, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 787, 0.1).
narrative_ontology:measurement(deca_su_t814, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 814, 0.85).
narrative_ontology:measurement(deca_su_t826, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 826, 0.75).
narrative_ontology:measurement(deca_su_t843, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 843, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__iconodule_reading, 0.08).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the decalogue_image_prohibition constraint family. The iconoclast_reading and moderate_iconoclast_reading are sibling constraints with distinct ε values, beneficiary/victim structures, and claimed types. The iconodule reading claims rope (coordination); the iconoclast reading would claim snare (extraction/suppression of veneration); the moderate reading would claim tangled_rope (partial coordination + partial suppression). All three share the kernel (the commandment text) but instantiate different constraints. Linked via affects_constraints for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
