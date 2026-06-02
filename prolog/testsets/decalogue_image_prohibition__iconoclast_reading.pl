% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint_id: decalogue_image_prohibition__iconoclast_reading
 *   human_readable: Decalogue Image Prohibition (Iconoclast Reading)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   The Decalogue's prohibition on graven images (Exodus 20:4-5, Deuteronomy
 *   5:8) is a contested kernel in Western theology. This constraint story
 *   instantiates the iconoclast reading: material representation used in
 *   worship is categorically forbidden and constitutes idolatry. Under this
 *   reading, any visual mediation of the divine is theologically
 *   impermissible. The reading was formally codified in eighth-century
 *   Byzantine theology (Leo III, 726 CE onward) and enforced through imperial
 *   authority, resulting in the destruction of icons, suppression of monastic
 *   illumination traditions, and prohibition of image-veneration practices.
 *   Victims include icon producers, monastic communities, and devotional
 *   practices structured around visual encounter with the sacred.
 *   Beneficiaries include centralizing imperial authority (which monopolized
 *   religious form by eliminating rival image-veneration traditions) and any
 *   theological framework claiming that monotheistic purity requires
 *   imageless worship. The constraint exhibits high extractiveness (0.68)
 *   reflecting the complete prohibition of victim practices, high suppression
 *   (0.72) reflecting enforcement machinery and threat of punishment, and
 *   elevated theater ratio (0.58) reflecting the performance of theological
 *   justification for a prohibition that serves imperial interests.
 *   Measurement trajectory shows extractiveness rising from adoption (t=0)
 *   through peak enforcement (t=50) and declining after formal reversal
 *   (t=120, post-Seventh Ecumenical Council), while suppression requirement
 *   remains high despite the formal permission of icons — indicating
 *   persisting theological internalization of the iconoclast position.
 *
 * KEY AGENTS:
 *   - Icon Producers: Primary victim (powerless/trapped) — their craft is prohibited as idolatry; no exit mechanism under the reading's enforcement
 *   - Monastic Communities: Primary victim (moderate/constrained) — illumination traditions, reliquaries, and liturgical imagery are prohibited; monastic identity is partially constituted through visual religious practice
 *   - Devotional Image-Dependent Practices: Victim set (powerless/trapped) — entire spiritual traditions (icon veneration, intercessory presence through imagery) are declared idolatrous; across generations, prohibited
 *   - Centralizing Imperial Authority: Primary beneficiary (institutional/arbitrage) — gains monopoly over religious form and eliminates rival authority structures based on image-veneration
 *   - Abstract Monotheistic Purity: Secondary beneficiary — theological framework claiming imageless worship preserves divine transcendence; benefits from the prohibition's enforcement
 *   - Theological Establishment (Post-Iconoclasm): Institutional actor (institutional/arbitrage) — after formal reversal (787 CE), maintains theatrical performance of the prohibition through interpretive tradition despite institutional permission of icons
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the iconoclast reading as a necessary truth of monotheistic theology rather than a contingent institutional and political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.68).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.72).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Decalogue Image Prohibition (Iconoclast Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, '8974864f-d11d-49d9-9e24-ca8b335c09b3').
narrative_ontology:cs_kernel_codification('8974864f-d11d-49d9-9e24-ca8b335c09b3', fixed_text).
narrative_ontology:cs_authority_grounding('8974864f-d11d-49d9-9e24-ca8b335c09b3', lineage).
narrative_ontology:cs_interpretation_layer_present('8974864f-d11d-49d9-9e24-ca8b335c09b3').
narrative_ontology:cs_reading_relation('8974864f-d11d-49d9-9e24-ca8b335c09b3', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('8974864f-d11d-49d9-9e24-ca8b335c09b3', foundational, material_image_categorical_idolatry).
narrative_ontology:cs_axiom_status(material_image_categorical_idolatry, holdable).
narrative_ontology:cs_axiom_grounding('8974864f-d11d-49d9-9e24-ca8b335c09b3', material_image_categorical_idolatry, theological).
narrative_ontology:cs_axiom('8974864f-d11d-49d9-9e24-ca8b335c09b3', foundational, transcendence_requires_imageless_worship).
narrative_ontology:cs_axiom_status(transcendence_requires_imageless_worship, holdable).
narrative_ontology:cs_axiom_grounding('8974864f-d11d-49d9-9e24-ca8b335c09b3', transcendence_requires_imageless_worship, deontological).
narrative_ontology:cs_reference_frame('8974864f-d11d-49d9-9e24-ca8b335c09b3', material_image_divine_incompatibility).
narrative_ontology:cs_drift_state('8974864f-d11d-49d9-9e24-ca8b335c09b3', post_seventh_ecumenical_council, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('8974864f-d11d-49d9-9e24-ca8b335c09b3', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, abstract_monotheistic_purity).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_producers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_visual_traditions).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, devotional_image_dependent_practices).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, visual_theological_literacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ICON PRODUCER (SNARE) — Trapped by theological prohibition backed by enforcement machinery (destruction of works, excommunication, exile). Cannot practice their craft without constituting idolatry by the reading's definition. Full extraction: the constraint prohibits the victim's primary labor and identity. No exit mechanism.
constraint_indexing:constraint_classification(decalogue_image_prohibition__iconoclast_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: MONASTIC COMMUNITY (SNARE) — Constrained by imperial enforcement and theological prohibition. Monastic illumination traditions, reliquary decoration, and liturgical imagery are prohibited. High cost to exit (dissolution of community identity, loss of religious vocation form); no genuine exit path within the constraint's geographic and political reach. Experiences pure extraction — practices defining monastic spirituality are criminalized.
constraint_indexing:constraint_classification(decalogue_image_prohibition__iconoclast_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: DEVOTIONAL IMAGE-DEPENDENT PRACTICE (SNARE) — Communities whose piety is structured around visual encounter (icon veneration, intercessory presence through imagery, liturgical visualization) are trapped without exit. The practices themselves are declared idolatrous. Across generations, entire forms of spirituality are prohibited. No coordination benefit — pure extraction and suppression.
constraint_indexing:constraint_classification(decalogue_image_prohibition__iconoclast_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 4: CENTRALIZING IMPERIAL AUTHORITY (ROPE) — Benefits from monopolizing religious form and eliminating rival sources of spiritual authority (monastic image-veneration traditions that appeal to populations outside imperial control). The constraint functions as coordinating mechanism from this perspective: standardizing doctrine (imageless worship) across jurisdictions, reducing competing authority structures, establishing uniformity of religious practice. Net beneficiary with exit via arbitrage — can reinterpret or abandon the prohibition if strategic imperatives shift. Experienced as coordination, not extraction.
constraint_indexing:constraint_classification(decalogue_image_prohibition__iconoclast_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THEOLOGICAL ESTABLISHMENT (PITON) — After iconoclasm formally resolves (Seventh Ecumenical Council, 787 CE, formally permits icons), the constraint persists through inertia in constituencies that have absorbed the prohibition as doctrinal identity. The theater ratio is elevated (0.58): theological justifications for the prohibition continue despite the formal institutional overturning. Piton classification reflects degraded function — the constraint no longer serves the empire's centralizing interests, but persists through interpretive tradition and invested identity.
constraint_indexing:constraint_classification(decalogue_image_prohibition__iconoclast_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL THEOLOGY VIEW (MOUNTAIN) — From a civilizational/universal perspective, material mediation cannot access the transcendent; the infinite God cannot be contained in finite representation; any image is categorically inadequate. This perspective sees the image prohibition as a necessary truth of theology itself, not a contingent institutional arrangement. However, the structural data contradicts this — identifiable beneficiaries (imperial authority), clear enforcement machinery, and alternative readings held by equally devout communities reveal this as a false summit: the 'categorical impossibility of image' naturalizes what is actually a contested interpretive choice grounded in specific institutional interests.
constraint_indexing:constraint_classification(decalogue_image_prohibition__iconoclast_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconoclast_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(decalogue_image_prohibition__iconoclast_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(decalogue_image_prohibition__iconoclast_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, TR),
    TR >= 0.70.

:- end_tests(decalogue_image_prohibition__iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The iconoclast reading completely prohibits victim practices (icon production, image veneration, visual theological literacy) with no mitigation or coordination benefit for the victims. The extractiveness is not maximal (0.72+) because some communities can practice imageless worship without experiencing the constraint as extraction — the constraint extracts from image-dependent practitioners but not from all believers. The reading targets specific practices, not all worship. Suppression (0.72): High. The constraint is enforced through destruction of artworks, excommunication, imperial decree, and threat of divine punishment for idolatry. Exit barriers are substantial: producers cannot practice their craft without violating the prohibition; monastic communities cannot maintain their traditions; devotional practitioners cannot engage in their spiritual practices. The suppression is not total (not 0.95) because some resistance exists historically (clandestine image production, theological resistance from iconodule communities) and enforcement decays over time. Theater ratio (0.58): Moderate-high. The iconoclast reading provides theological justification (material mediation cannot access transcendence; images lead to idolatrous worship) that functions as rationalization for the political project of imperial authority monopolizing religious form. The theater is significant but not maximal because the theological arguments are genuinely engaged with by believers (not purely performative), even though the reading simultaneously serves imperial interests. The measurement trajectory (theater rising from 0.42 to 0.68) reflects increasing theological elaboration and performance of the justification as resistance emerges and as the constraint's institutional grounding weakens. Claimed type: Snare. The constraint exhibits pure extraction (no coordination benefit for victims), high suppression, and victim dependency on the enforcing authority. The beneficiary (imperial authority) experiences this as coordination, but the overall structure is extractive toward the victims.
 *
 * PERSPECTIVAL GAP:
 *   The iconoclast reading is sustained by a gap between the beneficiary's rope perspective and the victim's snare perspective. The beneficiary (imperial authority) sees coordination and function; the victim sees extraction and suppression. This gap is not resolvable within the iconoclast reading's own framework — the reading asserts that images are categorically impermissible, which defines the constraint's boundary. The iconodule reading (sibling constraint) resolves the gap by permitting images, collapsing the extraction. The post-Seventh Ecumenical Council theological establishment's piton perspective reveals that the original extractive function (imperial authority monopolization) has been achieved, and the constraint's persistence is now performative rather than functional.
 *
 * DIRECTIONALITY LOGIC:
 *   The iconoclast reading's directionality structure is asymmetric. The centralizing imperial authority and abstract monotheistic purity are beneficiaries: they benefit from the constraint's enforcement and have arbitrage options (can reinterpret or abandon the reading if strategic interests shift). Derived d ≈ 0.10-0.15 for the beneficiary: low extraction because this agent extracts benefit, not cost. Icon producers, monastic communities, and devotional practitioners are victims: they bear complete cost with no exit mechanism. Derived d ≈ 0.92-0.98 for the victims: maximum extraction because these agents have no exit and bear the full suppressive force. The monastic community at moderate power and constrained exit has intermediate d ≈ 0.65-0.75: more trapped than mobile actors, less trapped than powerless actors with zero exit capacity. The piton perspective (institutional/arbitrage, post-iconoclasm) has d derived from the reading's current function (theatrical maintenance of a former extractive mechanism) rather than the original function, producing a lower d ≈ 0.20-0.30 reflecting the constraint's degraded extraction capacity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transcendence_mediation_possibility,
    'Can the transcendent God be mediated through material representation without categorically constituting idolatry, or is material mediation inherently incompatible with monotheistic purity?',
    'Comparative theology of image-permitting monotheisms (Orthodox iconodule tradition, Islamic calligraphy traditions, Jewish mystical visualization practices) — identification of defensible theological frameworks that permit material mediation while maintaining monotheistic commitment. The coexistence of multiple theological traditions holding opposite conclusions resolves this as a conceptual/preference question, not an empirical fact.',
    'If mediation is possible: the iconoclast reading''s core axiom (material_image_categorical_idolatry) is overridden by defensible alternative theology. The constraint reclassifies from snare/mountain to tangled_rope or rope depending on how enforcement is understood. If mediation is impossible: the reading''s natural-theology view holds, though the mountain classification still faces false-summit pressure from the identifiable beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transcendence_mediation_possibility, conceptual, 'Whether material mediation of the transcendent is categorically prohibited or contingently contested').

omega_variable(
    reading_grounding_in_imperial_interest,
    'To what degree does the iconoclast reading''s appeal derive from its doctrinal content versus its utility for centralizing imperial authority? Is this a coherent theological position held on its merits, or an interpretation selected for political expediency?',
    'Historical analysis of the reading''s adoption by imperial authorities (Byzantine emperors Leo III onward); comparison to adoption by non-imperial religious movements; examination of theological arguments made by iconoclast defenders independent of imperial promotion. If the reading is consistently adopted only by authorities with interest in centralizing power, and rejected by communities with decentralized structure, the grounding is primarily in imperial interest rather than theological merit.',
    'If primarily imperial: the constraint is revealed as a snare from all perspectives except the beneficiary''s (imperial authority). The mountain perspective becomes clearly a false summit. If theological: the constraint may be a genuine mountain that happens to also serve imperial interests. This does not resolve whether the reading is correct, only whether it is grounded in coherent theological reasoning or institutional interest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_grounding_in_imperial_interest, empirical, 'Whether the iconoclast reading is grounded in theological reasoning or imperial strategic interest').

omega_variable(
    enforcement_mechanism_internalization,
    'Is the suppression of image-production and devotion primarily structural (legal prohibition, destruction of works, excommunication) or internalized (believers have genuinely absorbed the theological argument and suppress their own image-impulses)?',
    'Historical evidence of resistance: are images still produced clandestinely despite prohibition? Do communities practice image-veneration secretly? Are there public recantations forced by authority versus private recantations suggesting internalization? The iconoclast-to-iconodule shift (787 CE) provides a natural experiment: if suppression was internalized, the formal reversal should not produce immediate return to image practices; if structural, reversal should quickly restore the practices.',
    'If primarily structural: victims'' constraint experience is trapped/constrained (external barriers). If internalized: victims'' experience may shift toward identity_locked (theological conviction that images are genuinely wrong). The theological reading''s power is amplified if believers have internalized the axiom; the constraint''s extractive character is revealed if enforcement is purely external and reversal quickly restores practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_internalization, empirical, 'Whether suppression of image-veneration is structural or internalized in believer consciousness').

omega_variable(
    kernel_reading_asymmetry,
    'This constraint instantiates the iconoclast reading of the Decalogue image prohibition kernel. The iconodule reading holds that images are permissible meditations on the divine and that the Decalogue prohibits only idolatrous worship of images, not veneration-with-proper-theological-framing. What is the structural nature of the disagreement — do the readings foreclosure each other within a single framework, or do they coexist as live options for different communities?',
    'Formal theological analysis: Can a single authority (church, text tradition, doctrinal council) hold both the iconoclast and iconodule readings without contradiction? The Seventh Ecumenical Council (787) formally permits icons while maintaining the prohibition of idolatrous worship — this suggests coexistence within a unified framework is possible (though contested). The iconoclast reading attempts to read idolatry as inherent to image itself, which would foreclose the iconodule reading. But the iconodule reading''s theological move (distinguishing image-as-object from image-as-conduit) logically permits both readings to coexist if idolatry is defined as worship-of-the-material rather than worship-through-the-material.',
    'If readings foreclose each other: one must be selected; the constraint story has a unique terminal state (the reading is either canonical or overridden). If readings coexist: both are live positions in the disputed kernel; the constraint represents one faction''s position in an ongoing theological dispute, not a resolution. The coexistence model implies the iconoclast reading is not a mountain but a contingent position defended by institutional interests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_asymmetry, conceptual, 'Whether iconoclast and iconodule readings foreclose each other or coexist as live theological positions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(decal_icon_tr_t0, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(decal_icon_tr_t50, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement(decal_icon_tr_t120, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 120, 0.68).

% Extraction over time
narrative_ontology:measurement(decal_icon_be_t0, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(decal_icon_be_t50, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(decal_icon_be_t120, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 120, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(decal_icon_su_t0, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(decal_icon_su_t50, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(decal_icon_su_t120, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 120, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__iconodule_reading).

% DUAL FORMULATION NOTE:
% The decalogue_image_prohibition kernel admits two structurally distinct constraint stories with different ε values and beneficiary/victim structures. The iconoclast reading (this file) models the prohibition as categorically impermissible material mediation; ε=0.68, snare type. The iconodule reading models the prohibition as forbidding idolatrous worship only, permitting images as devotional conduits; ε=0.15-0.25, rope or scaffold type. The readings are NOT observables of the same constraint — they are different constraints with different enforcement mechanisms and victim sets. They are linked as readings of the same kernel (the Exodus text). Each story is a clean, ε-invariant constraint. The network relationship indicates that adoption of one reading affects the viability and structural position of the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decalogue_image_prohibition__iconoclast_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
