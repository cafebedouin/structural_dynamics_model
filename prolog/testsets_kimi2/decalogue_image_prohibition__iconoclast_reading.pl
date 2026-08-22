% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__iconoclast_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: decalogue_image_prohibition__iconoclast_reading
 *   human_readable: Decalogue Image Prohibition â Iconoclast Reading
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This constraint instantiates the iconoclast reading of the Decalogue
 *   image prohibition: any material representation used in worship
 *   constitutes idolatry and is categorically forbidden. Structurally, the
 *   reading operates as a wall-type constraint against material mediation of
 *   the holy. It generates a victim set of icon producers, monastic
 *   communities, and devotional practitioners whose liturgical and economic
 *   lives depend on sacred imagery, while the primary beneficiary is a
 *   centralizing imperial authority that monopolizes legitimate religious
 *   form. The constraint is actively enforced through image destruction,
 *   persecution of iconodules, and the concentration of interpretive power in
 *   aniconic clerical and imperial channels.
 *
 * KEY AGENTS:
 *   - imperial_religious_authority (institutional/arbitrage) â agenda-setter and beneficiary, monopolizes religious form
 *   - icon_producers (powerless/identity_locked) â payer, craft criminalized, identity fused with sacred art
 *   - monastic_communities (organized/identity_locked) â payer, liturgical practice attacked, doctrinal identity threatened
 *   - devotional_practitioners (powerless/constrained) â payer, visual mediation removed, channeled through imperial clergy
 *   - iconodule_theologians (organized/constrained) â excluded, writings suppressed, voice ruled out by definitional boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.72).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.8).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, tangled_rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Decalogue Image Prohibition â Iconoclast Reading").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, '1a572f03-ed3e-4949-99b1-0e6e30464058').
narrative_ontology:cs_kernel_codification('1a572f03-ed3e-4949-99b1-0e6e30464058', fixed_text).
narrative_ontology:cs_authority_grounding('1a572f03-ed3e-4949-99b1-0e6e30464058', extraction).
narrative_ontology:cs_interpretation_layer_present('1a572f03-ed3e-4949-99b1-0e6e30464058').
narrative_ontology:cs_reading_relation('1a572f03-ed3e-4949-99b1-0e6e30464058', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_reading_relation('1a572f03-ed3e-4949-99b1-0e6e30464058', decalogue_image_prohibition__moderate_iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('1a572f03-ed3e-4949-99b1-0e6e30464058', foundational, material_mediation_constitutes_idolatry).
narrative_ontology:cs_axiom_status(material_mediation_constitutes_idolatry, holdable).
narrative_ontology:cs_axiom_grounding('1a572f03-ed3e-4949-99b1-0e6e30464058', material_mediation_constitutes_idolatry, theological).
narrative_ontology:cs_axiom('1a572f03-ed3e-4949-99b1-0e6e30464058', foundational, absolute_transcendence_requires_aniconia).
narrative_ontology:cs_axiom_status(absolute_transcendence_requires_aniconia, holdable).
narrative_ontology:cs_axiom_grounding('1a572f03-ed3e-4949-99b1-0e6e30464058', absolute_transcendence_requires_aniconia, theological).
narrative_ontology:cs_reference_frame('1a572f03-ed3e-4949-99b1-0e6e30464058', aniconic_worship_purity).
narrative_ontology:cs_drift_state('1a572f03-ed3e-4949-99b1-0e6e30464058', iconodule_restoration_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('1a572f03-ed3e-4949-99b1-0e6e30464058', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, imperial_religious_authority).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_producers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the prohibition on religious imagery, claiming sole authority to define legitimate worship. Derives political centralization and monopoly over religious form from the constraint's universality. Can adjust theological framing to maintain power but is structurally committed to aniconic worship as a boundary marker.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, imperial_religious_authority, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconoclast_reading, imperial_religious_authority, beneficiary).

% Their craft and livelihood depend on producing religious images. The prohibition criminalizes their work and forces them into other trades or underground production. Their identity as artisans is fused with sacred image-making, making exit costly beyond mere economic loss.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_producers, payer,
    powerless, biographical, identity_locked, local).

% Monasteries traditionally cultivate icon veneration as a core devotional practice. The prohibition attacks their liturgical life, theological identity, and economic base tied to pilgrimage and icon production. Resistance is doctrinal and existential; their identity is fused with image-based devotion.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_communities, payer,
    organized, generational, identity_locked, regional).

% Ordinary believers who rely on visual mediationâicons, frescoes, statuaryâfor access to the sacred. The prohibition removes their devotional objects and channels all religious mediation through the imperial-approved aniconic clergy. Their options are clandestine practice or compliance.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners, payer,
    powerless, biographical, constrained, local).

% Theologians who argue for the legitimacy of image-honor based on Incarnation and Christology. They are excluded from imperial councils and synods that define the prohibition, and their writings are suppressed or anathematized.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconodule_theologians, excluded,
    organized, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconoclast_reading, imperial_religious_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconoclast_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the absolute transcendence of the divine by eliminating material mediation in worship, preventing the collapse of creature-Creator distinction into idolatry, and unifying the community around aniconic practice.
% TRANSFER_FUNCTION: Transfers religious authority, legitimation, and control over devotional form from image-dependent monastic, artisan, and lay communities to the centralizing imperial religious authority.
% ABSENT_VOICES: Iconodule theologians who distinguish latria from dulia, artisan guilds dependent on sacred image production, and monastic confessors who testify to grace mediated through icons are structurally excluded from the interpretive conversation; their voices are ruled out by the prohibition's definitional equation of all material representation with idolatry.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight, icon production would resume, monastic liturgical cycles would reintegrate visual mediation, pilgrimage economies would restore image-rich shrines, and the imperial center would lose its monopoly over legitimate religious form; the devotional landscape would reorganize around materially mediated worship.
% FOUNDING_PROBLEM: The risk of idolatryâthe worship of created objects rather than the Creatorâand the need to preserve the absolute transcendence and unrepresentability of the divine in a religious environment surrounded by materialist and polytheistic alternatives.
% FOUNDING_PROBLEM_CORROBORATION: Imperial theologians and the agenda-setting authority attest the problem remains live, citing perpetual human tendency toward material fixation. Iconodule theologians and monastic historians outside the benefiting party attest that the Incarnation has resolved the material-transcendence tension and that the prohibition's founding problem is a misreading of the economy of salvation; their testimony is corroborated by liturgical continuity prior to the prohibition's enforcement.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconoclast_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconoclast_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(decalogue_image_prohibition__iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the prohibition transfers not only economic activity but entire devotional modalities from image-dependent communities to the imperial center. Suppression (0.80) is high because the constraint cannot persist without active enforcement: icons must be physically destroyed and iconodule resistance must be suppressed. Theater ratio (0.45) is moderate-to-high because enforcement increasingly includes performative displays of icon destruction that assert imperial authority as much as they prevent idolatry. Accessibility collapse (0.78) is high because once the prohibition is established, alternative devotional economies largely collapse. Resistance (0.70) is high because monastic communities and iconodule theologians mount sustained doctrinal and political opposition. The measurement series share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   From the imperial seat, the constraint is a necessary defense of divine transcendence and communal purity; from the monastic and artisan seats, it is an assault on the Incarnational economy and their existential lifeworld. The engine should compute divergent per-seat classifications: the agenda-setter experiences a rope-like coordination (theological unity), while the identity-locked payer seats experience a snare-like extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial religious authority is the declared beneficiary and agenda-setter; it collects centralization and monopoly rents, giving it a low directionality (near the beneficiary pole). Icon producers, monastic communities, and devotional practitioners are declared victims/payers; they bear the cost of destroyed livelihoods, suppressed liturgy, and removed devotional objects, and their exit is identity-locked or constrained, giving them high directionality (near the full-target pole). No override is needed because the structural derivation from beneficiary/victim declarations plus exit options captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification prevents two mislabeling errors. First, it prevents reading the constraint as a pure rope (mere theological coordination) by requiring the declaration of victims and the active enforcement that extracts from them. Second, it prevents reading it as a pure snare (mere imperial extraction) by requiring acknowledgment of the genuine coordination function: the prohibition does solve a real collective-action problem of maintaining boundary clarity between creature and Creator in a context where material mediation risks polytheistic drift. The asymmetric extraction is layered onto this coordination function through the imperial monopoly on legitimate form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the Decalogue prohibition admit the iconodule distinction between latria and dulia, or is the iconoclast total ban the only coherent reading of the kernel?',
    'Historical-grammatical analysis of the Hebrew and Septuagint text plus comparative Semitic philology; also patristic reception history before the eighth century.',
    'If the kernel text is ambiguous, the iconoclast reading is one construction among several, and its high extraction profile is driven by imperial authority''s selection of this reading rather than by textual necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the kernel text structurally mandates aniconia or permits iconodule mediation.').

omega_variable(
    imperial_extraction_driver,
    'Is the prohibition''s persistence driven primarily by theological commitment to divine transcendence, or by the imperial authority''s extraction of political centralization from a univocal religious form?',
    'Comparative analysis of aniconic periods across empires: if non-imperial aniconic communities show substantially lower enforcement and suppression, the driver is political.',
    'A political driver would shift the coordination function toward extraction-dominant (snare-like); a theological driver would sustain the tangled-rope classification with a genuine coordination side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_extraction_driver, empirical, 'Theological versus political driver of the prohibition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of icon veneration structural (imperial edicts, military destruction of images, legal penalties) or internalized (believers accept that image-desire is idolatrous and self-censor)?',
    'Post-exit trajectory: if iconodules continue venerating clandestinely after structural enforcement relaxes, suppression was primarily structural; if clandestine practice collapses without enforcement, it was internalized.',
    'Internalized suppression raises effective extraction beyond the structural measure because the target carries the constraint after external enforcement is removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(deca_tr_t60, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement(deca_tr_t80, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 80, 0.44).
narrative_ontology:measurement(deca_tr_t100, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement(deca_be_t60, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 60, 0.71).
narrative_ontology:measurement(deca_be_t80, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 80, 0.72).
narrative_ontology:measurement(deca_be_t100, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement(deca_su_t60, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 60, 0.81).
narrative_ontology:measurement(deca_su_t80, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 80, 0.8).
narrative_ontology:measurement(deca_su_t100, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 100, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, iconodule_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the decalogue_image_prohibition family. The iconoclast reading and its sibling readings are structurally distinct constraints derived from the same kernel text, with different epsilon values, victim/beneficiary structures, and coordination functions. Decomposition follows the epsilon-invariance principle: the same label covers claims with different empirical and normative status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
