% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__moderate_iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__moderate_iconoclast_reading, []).

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
 *   constraint_id: decalogue_image_prohibition__moderate_iconoclast_reading
 *   human_readable: Decalogue Image Prohibition (Moderate Iconoclast Reading): Dimensional Sorting with Regulatory Gatekeeping
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   The moderate iconoclast reading of the Decalogue's image prohibition
 *   splits the categorical difference between total prohibition (pure
 *   iconoclasm) and unrestricted permission (iconodulism). By permitting
 *   two-dimensional images under strict regulation while forbidding
 *   three-dimensional statuary, this reading attempts to mediate the idolatry
 *   risk while preserving visual devotional practice. However, the structural
 *   analysis reveals that this compromise instantiates a regulatory
 *   gatekeeping mechanism: the permission for 2D images appears to grant
 *   freedom while the regulatory apparatus ensures compliance monitoring and
 *   authorization control. The extractiveness trajectory (0.35 → 0.52 over
 *   the interval) reflects the accumulation of regulatory burden as
 *   enforcement infrastructure matures. The theater ratio (0.48 → 0.61)
 *   indicates increasing performative content in enforcement as the
 *   theological rationale for the dimensional distinction loses coherence and
 *   the machinery persists through institutional inertia.
 *
 * KEY AGENTS:
 *   - Visual Practitioners: Primary victim (powerless/trapped) — desire to create devotional imagery suppressed by regulation and heresy threat
 *   - Regulatory Authority: Primary beneficiary (institutional/arbitrage) — maintains gatekeeping power through authorization control and compliance monitoring
 *   - Theological Innovation Community: Secondary victim (moderate/constrained) — constrained from exploring new artistic-theological directions; must navigate regulatory boundaries
 *   - Established Cult with Permitted Imagery: Secondary beneficiary (institutional/arbitrage) — legitimacy advantage from clear regulatory status; can use images to outcompete unregulated competitors
 *   - Organized Iconoclast Movement: Organized victim (organized/constrained) — sees the permission for 2D images as a co-optation trap that divides resistance
 *   - Ecclesiastical Authority System: Institutional actor (institutional/arbitrage) — maintains enforcement machinery through theatrical performance of rule coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, 0.52).
domain_priors:suppression_score(decalogue_image_prohibition__moderate_iconoclast_reading, 0.58).
domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Decalogue Image Prohibition (Moderate Iconoclast Reading): Dimensional Sorting with Regulatory Gatekeeping").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, '2e853ec1-cb93-4222-8f65-af5e4dd325ec').
narrative_ontology:cs_kernel_codification('2e853ec1-cb93-4222-8f65-af5e4dd325ec', formalized).
narrative_ontology:cs_authority_grounding('2e853ec1-cb93-4222-8f65-af5e4dd325ec', extraction).
narrative_ontology:cs_interpretation_layer_present('2e853ec1-cb93-4222-8f65-af5e4dd325ec').
narrative_ontology:cs_reading_relation('2e853ec1-cb93-4222-8f65-af5e4dd325ec', decalogue_image_prohibition__pure_iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('2e853ec1-cb93-4222-8f65-af5e4dd325ec', decalogue_image_prohibition__iconodule_reading, influences).
narrative_ontology:cs_axiom('2e853ec1-cb93-4222-8f65-af5e4dd325ec', foundational, dimensional_idolatry_gradient).
narrative_ontology:cs_axiom_status(dimensional_idolatry_gradient, holdable).
narrative_ontology:cs_axiom_grounding('2e853ec1-cb93-4222-8f65-af5e4dd325ec', dimensional_idolatry_gradient, empirically_contingent).
narrative_ontology:cs_axiom('2e853ec1-cb93-4222-8f65-af5e4dd325ec', foundational, regulatory_gatekeeping_necessity).
narrative_ontology:cs_axiom_status(regulatory_gatekeeping_necessity, holdable).
narrative_ontology:cs_axiom_grounding('2e853ec1-cb93-4222-8f65-af5e4dd325ec', regulatory_gatekeeping_necessity, conventional).
narrative_ontology:cs_reference_frame('2e853ec1-cb93-4222-8f65-af5e4dd325ec', dimensional_safety_hierarchy).
narrative_ontology:cs_drift_state('2e853ec1-cb93-4222-8f65-af5e4dd325ec', contemporary_artistic_pluralism, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2e853ec1-cb93-4222-8f65-af5e4dd325ec', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, regulatory_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, institutional_gatekeepers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, visual_practitioners).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, theological_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VISUAL PRACTITIONER (SNARE) — Trapped by the dimensional sorting rule. Desire to create visual devotional objects is suppressed through threat of heresy judgment. The permission for 2D images under regulation creates the illusion of exit ('you may make images') while the regulatory gatekeeping ensures that actual practice requires authorization and compliance monitoring. Maximum extraction from a powerless agent with no structural exit.
constraint_indexing:constraint_classification(decalogue_image_prohibition__moderate_iconoclast_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THEOLOGICAL INNOVATION COMMUNITY (TANGLED ROPE) — Moderate extraction with genuine coordination function. The constraint solves the real problem of preventing idolatrous substitution of the image for the divine, while simultaneously constraining theological creativity and artistic expression. High cost to exit (religious community ostracism) but some agency in interpreting the dimensional boundary.
constraint_indexing:constraint_classification(decalogue_image_prohibition__moderate_iconoclast_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ESTABLISHED CULT WITH PERMITTED IMAGERY (ROPE) — Institutional beneficiary. The constraint enables their practice by clearly demarcating what is permitted: 2D images under regulation are acceptable, creating a legitimacy advantage over unregulated competitors. Experiences the constraint as coordinating devotional practice, not as extractive.
constraint_indexing:constraint_classification(decalogue_image_prohibition__moderate_iconoclast_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZED ICONOCLAST MOVEMENT (SNARE) — Organized agents who reject the entire dimensional sorting as false compromise. See the permission for 2D images as a trap that co-opts would-be allies and fragments resistance. The regulatory structure extracts compliance costs even from those who disagree with the fundamental permission, by forcing choice: either accept regulation or be classified as heretical extremists. High suppression despite organization because the constraint structures the orthodoxy itself.
constraint_indexing:constraint_classification(decalogue_image_prohibition__moderate_iconoclast_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 5: ECCLESIASTICAL AUTHORITY SYSTEM (PITON) — The institutional machinery of enforcement has become largely theatrical. The dimensional distinction (3D = forbidden, 2D = regulated) is maintained through enforcement ritual despite losing theological coherence as artistic and magical practices evolve. The authority system persists by maintaining gatekeeping authority over interpretation, not by the functional clarity of the rule itself.
constraint_indexing:constraint_classification(decalogue_image_prohibition__moderate_iconoclast_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — The risk of idolatrous substitution is framed as inherent to human religious cognition: the dimensional boundary (statue = high idolatry risk, painting = lower risk) is presented as a natural law of devotional psychology. However, this reading naturalizes what is an empirical claim about cognition and a normative choice about regulation.
constraint_indexing:constraint_classification(decalogue_image_prohibition__moderate_iconoclast_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__moderate_iconoclast_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(decalogue_image_prohibition__moderate_iconoclast_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(decalogue_image_prohibition__moderate_iconoclast_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, TR),
    TR >= 0.70.

:- end_tests(decalogue_image_prohibition__moderate_iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The permission for 2D images under regulation appears to grant freedom while the regulatory gatekeeping ensures that actual practice requires authorization and continuous compliance monitoring. The extractiveness is not as high as a pure prohibition (which would score closer to 0.65) because there is genuine permission for some practices. However, the extractiveness exceeds simple coordination (which would be ~0.25) because the regulatory apparatus creates authorization dependency and behavioral conformity. The trajectory from 0.35 → 0.52 reflects accumulating regulatory burden as enforcement infrastructure matures. Suppression (0.58): Moderate-high. Significant barriers to deviation include heresy threat, authorization dependency, and community ostracism. The 3D prohibition eliminates the highest-stakes creative option entirely; the 2D permission creates the illusion of outlet while regulatory monitoring constrains practice. Theater ratio (0.61): Moderate-high. The dimensional distinction (3D = high idolatry risk, 2D = lower risk) is maintained as a rule even as its theological coherence declines. Enforcement focuses on authorization ritual rather than on detecting actual idolatrous behavior. The increasing theater ratio reflects that the rule persists through institutional habit rather than through evidence that the dimensional boundary actually prevents idolatry.
 *
 * PERSPECTIVAL GAP:
 *   The moderate iconoclast reading produces a 5-way perspectival gap. The powerless visual practitioner sees a trap disguised as permission (Snare). The theological innovation community sees mixed coordination and extraction (Tangled Rope). The established cult with permitted imagery sees genuine coordination (Rope). The organized iconoclast movement sees a mechanism for co-opting and fragmenting resistance (Snare at the organized level). The ecclesiastical authority system sees its own enforcement as increasingly theatrical (Piton). The analytical observer risks naturalizing the regulatory distinction as an immutable feature of human religious cognition (Mountain — false summit). This reading is structurally distinct from the pure iconoclast reading (which would show higher extraction because it forbids all images) and from the iconodule reading (which would show lower extraction because it permits images with lighter regulation).
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness is computed from their structural position relative to the regulatory mechanism. Visual practitioners with no exit options and no authorization power experience maximum extraction (powerless/trapped → d ≈ 0.95 → high f(d)). The regulatory authority with control over authorization and benefit from gatekeeping exercises low d (institutional/arbitrage → d ≈ 0.10 → low f(d) → institutional beneficiary). Moderate practitioners (theological innovators) experience intermediate extraction because they have some exit options (leave the community, accept the regulatory constraints, or practice covertly) but face high costs for each. The organized iconoclast movement experiences high extraction despite organization because the constraint structures the orthodoxy itself — organization does not provide escape routes when the entire legitimate discourse space is constrained. The ecclesiastical authority system experiences negative effective extraction (benefits from the constraint) because the mechanism maintains their power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dimensional_boundary_cognitivity,
    'Does the 3D/2D distinction actually map onto psychological risk of idolatrous substitution, or is this a post-hoc rationalization of a prudential regulatory boundary?',
    'Cognitive-historical analysis of actual devotional practice: do practitioners report different cognitive relationships to 3D vs 2D images? Do prohibitions track idolatry incidents or track institutional power consolidation?',
    'If maps onto cognition: the constraint has a genuine safety coordination function (Tangled Rope from most perspectives). If post-hoc rationalization: the constraint is regulatory gatekeeping with naturalized framing (Snare from powerless/moderate perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dimensional_boundary_cognitivity, empirical, 'Whether 3D/2D boundary reflects genuine idolatry risk or serves as regulatory gatekeeping pretext').

omega_variable(
    regulation_cost_opacity,
    'What is the actual enforcement cost of the 2D regulatory system? Does the permission for 2D images under regulation impose higher compliance burden than the prohibition of 3D images?',
    'Historical documentation of regulatory processes: licensing requirements, examination procedures, doctrinal review cycles. Comparison of enforcement infrastructure required for permitting vs prosecuting.',
    'If regulatory burden is substantial: the ''permission'' for 2D images is a Trojan horse, extracting more compliance cost than total prohibition would. If burden is minimal: the permission is genuine coordination. High burden reclassifies the constraint toward pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulation_cost_opacity, empirical, 'Enforcement cost of 2D image regulation relative to enforcement of 3D prohibition').

omega_variable(
    sibling_reading_foreclosure_status,
    'Does this moderate reading''s dimensional sorting axiom logically foreclose the pure iconoclast reading (all images forbidden) or the iconodule reading (all images permitted under supervision)?',
    'Axiom-level logical analysis: do the foundational normative claims of the moderate reading contradict or merely differ from the sibling readings? Can a single theological framework hold both moderate and pure iconoclast positions, or are they mutually exclusive?',
    'If moderate forecloses pure iconoclast: the three readings are in a foreclosure hierarchy (moderate reading dominates). If coexists: the three readings are alternative positions held by different factions (normal theological disagreement). If influences: the moderate reading creates structural pressure that weakens the pure iconoclast position without eliminating it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_status, conceptual, 'Whether moderate iconoclast axiom forecloses or coexists with sibling readings').

omega_variable(
    authorization_capture,
    'Who controls the authorization of 2D images under the regulatory regime? Does authorization authority accumulate power in institutional gatekeepers, creating extractive incentive to maintain the regulatory boundary?',
    'Structural analysis of authorization chains: are gatekeepers rewarded for granting authorization (expanding permitted imagery) or for withholding it (maintaining orthodoxy). Historical tracking of authorization policies: do they liberalize over time (coordination function) or become more restrictive (extraction function)?',
    'If gatekeepers profit from withholding: the regulatory permission is a Trojan horse extracting behavioral conformity while appearing to permit. If gatekeepers profit from granting: the regulation genuinely coordinates permissible practice. Outcome determines whether suppression is structural (inherent to the rule) or institutional (contingent on gatekeeper incentives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorization_capture, empirical, 'Whether authorization gatekeepers accumulate extractive power through regulatory control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(decal_mod_icon_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(decal_mod_icon_tr_t3, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 3, 0.54).
narrative_ontology:measurement(decal_mod_icon_tr_t6, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 6, 0.61).

% Extraction over time
narrative_ontology:measurement(decal_mod_icon_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(decal_mod_icon_be_t3, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(decal_mod_icon_be_t6, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(decal_mod_icon_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(decal_mod_icon_su_t3, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 3, 0.54).
narrative_ontology:measurement(decal_mod_icon_su_t6, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__moderate_iconoclast_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__pure_iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconodule_reading).

% DUAL FORMULATION NOTE:
% The moderate iconoclast reading is one of three structurally distinct constraints derived from the same contested kernel (the Decalogue image prohibition). Each reading has different ε, different beneficiary/victim structure, and different classification outcomes. The pure iconoclast reading (all images forbidden) exhibits higher extractiveness because it offers no legitimate outlet. The iconodule reading (all images permitted under supervision) exhibits lower extractiveness and less regulatory gatekeeping. The moderate reading occupies the middle ground but instantiates regulatory capture: the permission for 2D images creates dependency on authorization authorities. All three are live interpretations; none forecloses the others within theological discourse. Sibling relationships declared in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
