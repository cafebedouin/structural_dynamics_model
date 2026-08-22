% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__jerusalem_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__jerusalem_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: human_transcendence_pathway__jerusalem_reading
 *   human_readable: Authentic Human Community Rebuilt Through Patient Participatory Labor Under Divine Blessing
 *   domain: religious/political_theological/social_ethics
 *
 * SUMMARY:
 *   This constraint models the 'Jerusalem reading' of the human transcendence
 *   pathway kernel: authentic community is rebuilt through patient,
 *   participatory labor under divine blessing, where plurality is integrated
 *   into communion rather than flattened into uniformity. The image is the
 *   post-exilic return — a diverse, wounded people reconstructing common life
 *   not by imposing a single mold but by weaving difference into a shared
 *   covenantal fabric. The coordination function is the integration of
 *   irreducible plurality (tribes, gifts, wounds, histories) into a communion
 *   that preserves each strand; the mechanism is participatory labor
 *   (Nehemiah's wall-building, Ezra's Torah-reading, the liturgical calendar)
 *   sustained by the conviction that divine blessing attends the work.
 *   Extraction is low (epsilon ~0.25) because the constraint operates through
 *   persuasion, formation, and liturgical imagination rather than coercion;
 *   suppression is minimal because alternatives are not actively crushed —
 *   they are witnessed and engaged. The beneficiaries are the community as a
 *   whole, especially the returning exiles and marginalized members who find
 *   place in the rebuilt fabric. There are no structural victims, though the
 *   pathway demands the sacrifice of efficiency for solidarity — a cost that
 *   is either functional coordination overhead (Boltzmann floor) or
 *   deadweight, an ambiguity carried in the omegas.
 *
 * KEY AGENTS:
 *   - returning_exiles: Primary beneficiaries (moderate/identity_locked) — bear the labor, receive the integration
 *   - wider_community: Beneficiaries (organized/constrained) — receive the reconstituted communion
 *   - marginalized_members: Beneficiaries (powerless/constrained) — find place in the fabric that uniformity would erase
 *   - divine_actor: Agenda setter (analytical/analytical) — the blessing that grounds the coordination, not an extractive power
 *   - prophetic_voice: Observer (analytical/analytical) — reads the constraint from within the tradition
 *   - technocratic_optimizer: Excluded (powerful/trapped) — would impose efficiency and uniformity, structurally absent from this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__jerusalem_reading, 0.25).
domain_priors:suppression_score(human_transcendence_pathway__jerusalem_reading, 0.15).
domain_priors:theater_ratio(human_transcendence_pathway__jerusalem_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__jerusalem_reading, rope).
narrative_ontology:human_readable(human_transcendence_pathway__jerusalem_reading, "Authentic Human Community Rebuilt Through Patient Participatory Labor Under Divine Blessing").
narrative_ontology:topic_domain(human_transcendence_pathway__jerusalem_reading, "religious/political_theological/social_ethics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__jerusalem_reading, 'f744e607-77e3-4ac3-b718-2362c3f3dbfd').
narrative_ontology:cs_kernel_codification('f744e607-77e3-4ac3-b718-2362c3f3dbfd', fixed_text).
narrative_ontology:cs_authority_grounding('f744e607-77e3-4ac3-b718-2362c3f3dbfd', lineage).
narrative_ontology:cs_interpretation_layer_present('f744e607-77e3-4ac3-b718-2362c3f3dbfd').
narrative_ontology:cs_reading_relation('f744e607-77e3-4ac3-b718-2362c3f3dbfd', human_transcendence_pathway__babel_reading, coexists_with).
narrative_ontology:cs_reading_relation('f744e607-77e3-4ac3-b718-2362c3f3dbfd', human_transcendence_pathway__technocratic_vs_incarnational_reading, coexists_with).
narrative_ontology:cs_axiom('f744e607-77e3-4ac3-b718-2362c3f3dbfd', foundational, plurality_integrated_into_communion_not_uniformity).
narrative_ontology:cs_axiom_status(plurality_integrated_into_communion_not_uniformity, holdable).
narrative_ontology:cs_axiom_grounding('f744e607-77e3-4ac3-b718-2362c3f3dbfd', plurality_integrated_into_communion_not_uniformity, deontological).
narrative_ontology:cs_axiom('f744e607-77e3-4ac3-b718-2362c3f3dbfd', foundational, transcendence_as_gift_received_in_vulnerability_not_achieved_by_optimization).
narrative_ontology:cs_axiom_status(transcendence_as_gift_received_in_vulnerability_not_achieved_by_optimization, holdable).
narrative_ontology:cs_axiom_grounding('f744e607-77e3-4ac3-b718-2362c3f3dbfd', transcendence_as_gift_received_in_vulnerability_not_achieved_by_optimization, theological).
narrative_ontology:cs_reference_frame('f744e607-77e3-4ac3-b718-2362c3f3dbfd', post_exilic_covenantal_rebuilding).
narrative_ontology:cs_drift_state('f744e607-77e3-4ac3-b718-2362c3f3dbfd', contemporary_technocratic_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f744e607-77e3-4ac3-b718-2362c3f3dbfd', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, returning_exiles).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, wider_community).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, marginalized_members).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, diversity_as_communion_resource).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, shared_responsibility_over_efficiency).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, divine_blessing_as_foundation).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, plurality_integrated_not_uniform).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ones who return from exile to rebuild. They bear the patient, participatory labor — clearing rubble, laying stones, keeping watch, reading Torah in the square. Their identity is fused with the rebuilding; exit would mean abandoning the vocation that constitutes them. They receive the integration of their fragmented history into a communion that honors the particularity of their exile and return.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, returning_exiles, beneficiary,
    moderate, biographical, identity_locked, local).

% The settled and returning populations together who receive the reconstituted communion — the walls that hold, the liturgy that binds, the economy of mutual care that replaces the scramble for survival. Their exit is constrained by the communal bonds the pathway creates; leaving means losing the communion that makes their flourishing possible.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, wider_community, beneficiary,
    organized, generational, constrained, regional).

% Those whom uniformity would erase — the foreigner, the eunuch, the poor, the disabled, the ones whose difference is not a problem to be solved but a gift to be integrated. The Jerusalem pathway makes room for them in the fabric; they receive belonging that the efficient/unified alternatives would deny. Their exit options are constrained because the alternative arrangements offer them less place, not more.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, marginalized_members, beneficiary,
    powerless, biographical, constrained, local).

% The divine blessing that grounds and sustains the coordination — not as an extractive power collecting tribute, but as the gift that makes the patient labor fruitful. The blessing is the 'yes' that attends the work, the promise that the slow rebuilding is not in vain. This is not an agent in the stakeholder sense (agent=false) but the structural grounding of the pathway's legitimacy.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, divine_actor, agenda_setter,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(human_transcendence_pathway__jerusalem_reading, divine_actor).

% The interpretive voice within the tradition that reads the constraint from inside — the prophets who call the community back to the covenantal logic when it drifts toward Babel or technocracy. They neither set the agenda nor pay the costs; they witness and name. Their exit is analytical: they can always step outside the frame to analyze it.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, prophetic_voice, observer,
    analytical, generational, analytical, regional).

% The actor who would impose efficiency, measurability, and uniformity as the path to transcendence — the technocratic reading's agenda setter. In this reading's frame, they are structurally excluded because their logic (optimization, elimination of limits, instrumental rationality) is incommensurable with the patient, participatory, gift-receiving logic of Jerusalem. They are trapped in their own reading's framework; they cannot enter this pathway without abandoning their core premise.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, technocratic_optimizer, excluded,
    powerful, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrating irreducible human plurality — diverse gifts, wounds, histories, cultures — into a communion that preserves each strand rather than flattening difference into uniformity. The coordination problem is: how can a fragmented, exiled, diverse people become a genuine 'we' without erasing the 'I's and 'you's that constitute it?
% TRANSFER_FUNCTION: Moves the burden of rebuilding from isolated individuals to the shared labor of the community; moves the fruit of that labor (walls, liturgy, mutual care, identity) from private possession to common inheritance. No monetary or status transfer from victims to beneficiaries — the transfer is from fragmentation to communion, from exile to homecoming.
% ABSENT_VOICES: The technocratic optimizer and Babel builder are structurally absent — they would object that this pathway is too slow, too uncertain, too dependent on an unmeasurable 'blessing,' and that unity achieved through unified systems is superior. They are absent because their premises are incommensurable with this reading's frame; they are not excluded by enforcement but by the logic of the pathway itself.
% DISAPPEARANCE_RATIONALE: If the Jerusalem pathway vanished overnight, communities facing fragmentation would lose the model of patient, participatory, plurality-integrating rebuilding grounded in divine promise. They would be left with only the Babel and technocratic alternatives — faster, more measurable, but producing unity through uniformity or optimization. The world would rearrange toward those alternatives by default.
% FOUNDING_PROBLEM: How to rebuild authentic human community after exile and fragmentation without imposing a false unity that erases the very people it claims to gather — the problem of post-exilic Judah, and of every generation facing the temptation of Babel.
% FOUNDING_PROBLEM_CORROBORATION: The biblical witness (Ezra-Nehemiah, Isaiah 40-55, Zechariah) attests the founding problem from within the tradition. Christian political theology (Augustine's two cities, Catholic social doctrine's solidarity/subsidiarity, liberation theology's base communities) corroborates from outside the immediate beneficiary set. Secular communitarian critics (MacIntyre, Hauerwas) attest the problem's persistence from a non-theological frame. The technocratic and Babel readings contest the status, claiming the problem is solved or solvable by their methods.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__jerusalem_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__jerusalem_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__jerusalem_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(human_transcendence_pathway__jerusalem_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__jerusalem_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__jerusalem_reading_tests).
:- end_tests(human_transcendence_pathway__jerusalem_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the constraint's operation is formative and persuasive — the 'cost' to participants is the patience and vulnerability of participatory labor, not a transfer extracted by a beneficiary. Suppression is low (0.15) because the pathway does not rely on crushing alternatives; the Babel and technocratic readings exist as live alternatives in the world, not as suppressed options. Theater ratio is very low (0.10) because the participatory labor and liturgical life are the substance, not a performance masking extraction. Accessibility collapse is moderate (0.30) because the pathway is demanding and particular — not everyone can or will enter it — but it does not claim to be the only possible human arrangement. Resistance is moderate (0.45) because the reading faces active contestation from the technocratic and Babel readings, which offer faster, more measurable, more controllable pathways. The claimed type is rope: a genuine coordination problem (integrating plurality into communion) solved with minimal coercive overhead, where participants are net beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   The returning exiles and marginalized members are structural beneficiaries — they receive the integration and belonging that the pathway produces. The wider community benefits as the communion holds. The divine actor (grounding the blessing) is the agenda setter but does not extract; the blessing is gift, not levy. The technocratic optimizer and Babel builder are excluded from this reading's frame — they would impose a different pathway, but their exclusion is not this constraint's enforcement action; they are excluded by the reading's own logic. No agent bears net extraction; the 'cost' of efficiency-sacrifice is either the Boltzmann floor of attachment/identity coordination or a deadweight loss (omega_open).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to rebuild authentic community from fragmentation without imposing false unity — remains live (contested status). The reading does not suffer mandatrophy because the problem it addresses (fragmentation, exile, the temptation of Babel) recurs in each generation. The constraint is not a degraded remnant of a solved problem; it is a perennially re-instantiated pathway. The theater ratio stays low because the participatory labor is not performative — it is the thing itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the human_transcendence_pathway kernel, and how does it structurally differ from the babel_reading and technocratic_vs_incarnational_reading?',
    'Comparative structural analysis of the three readings'' beneficiary/victim structures, epsilon referents, and coordination/extraction profiles within the shared kernel commitment.',
    'If the readings are not structurally distinct constraints, they collapse into one story; if distinct, each requires its own epsilon and classification. This reading''s low extraction and coordination-focused profile must be shown to differ from the extractive/coercive profile attributed to babel_reading and the instrumental optimization of technocratic_vs_incarnational_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural distinctness of this kernel reading from its siblings.').

omega_variable(
    divine_blessing_operationalization,
    'How does ''divine blessing'' function structurally as a coordination mechanism rather than an extraction cover — what does it coordinate, and what would fail without it?',
    'Examine whether communities attempting the same participatory rebuilding without the divine-blessing framing achieve comparable integration of plurality and durability, or whether the theological frame performs irreducible coordination work.',
    'If divine blessing is structurally load-bearing for coordination (not merely motivational), the constraint''s rope classification is reinforced; if it is substitutable by secular solidarity mechanisms, the theological framing may be incidental and the constraint''s coordination type shifts toward attachment_coordination or identity_coordination without theological specificity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_blessing_operationalization, empirical, 'Whether the theological frame is structurally necessary for the coordination function.').

omega_variable(
    efficiency_solidarity_tradeoff,
    'Does the ''sacrifice of efficiency for solidarity'' constitute a genuine coordination cost (Boltzmann floor) or an extractive burden on participants?',
    'Measure whether the slower, participatory process produces outcomes that the faster efficient process cannot — specifically, whether the integration of plurality into communion is achievable only through the patient labor pathway, or whether the efficiency loss is a deadweight cost.',
    'If the tradeoff is structurally necessary for the coordination function, it falls within the Boltzmann floor for attachment/identity coordination; if it is a deadweight loss imposed on participants without functional return, it registers as extractive overhead and raises epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_solidarity_tradeoff, empirical, 'Whether the efficiency-solidarity tradeoff is functional coordination cost or extractive overhead.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__jerusalem_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(htp_jerusalem_tr_t0, human_transcendence_pathway__jerusalem_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(htp_jerusalem_tr_t5, human_transcendence_pathway__jerusalem_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(htp_jerusalem_tr_t10, human_transcendence_pathway__jerusalem_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(htp_jerusalem_tr_t15, human_transcendence_pathway__jerusalem_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(htp_jerusalem_tr_t20, human_transcendence_pathway__jerusalem_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(htp_jerusalem_be_t0, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(htp_jerusalem_be_t5, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(htp_jerusalem_be_t10, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(htp_jerusalem_be_t15, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(htp_jerusalem_be_t20, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(htp_jerusalem_su_t0, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(htp_jerusalem_su_t5, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(htp_jerusalem_su_t10, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement(htp_jerusalem_su_t15, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(htp_jerusalem_su_t20, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__jerusalem_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(human_transcendence_pathway__jerusalem_reading, 0.08).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__babel_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__technocratic_vs_incarnational_reading).

% DUAL FORMULATION NOTE:
% This is the jerusalem_reading of the human_transcendence_pathway kernel. The babel_reading models transcendence as collective self-sufficiency through unified systems (high extraction, suppression of difference). The technocratic_vs_incarnational_reading models the contest between technological optimization and incarnational gift. All three share the kernel commitment but instantiate structurally distinct constraints with different epsilon referents and beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
