% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconodule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: decalogue_image_prohibition__iconodule_reading
 *   human_readable: Iconodule Reading: Veneration of Images as Sanctioned Material Mediation
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This story authors the iconodule reading of the Decalogue image
 *   prohibition as its own constraint: worship (latria) is reserved for God
 *   alone, but honor paid to an image (dulia) passes through the image to its
 *   prototype and is therefore licit — a position grounded doctrinally in the
 *   Incarnation's claim that matter itself was sanctified when the divine
 *   took visible form. Under this reading the constraint functions as
 *   coordination: it authorizes a shared visual devotional culture (icons,
 *   veneration practices, icon production) without requiring individual
 *   believers to resolve difficult metaphysical questions themselves. This is
 *   deliberately NOT the same constraint as the iconoclast reading, which
 *   holds that any material representation used in worship is idolatry
 *   regardless of intent, nor the moderate iconoclast reading, which permits
 *   two-dimensional images under strict regulation while forbidding statuary.
 *   Each reading has a different ε: the iconoclast reading's constraint is
 *   comparatively higher-suppression and higher-victim (destroyed icons,
 *   persecuted venerators) precisely because it enforces against the practice
 *   this story sanctions. This story's ε is authored low because, under the
 *   iconodule reading's own operation, there is no active suppression
 *   apparatus and no identified victim group — the historical persecution of
 *   icon-venerators belongs to the iconoclast constraint's operation, not
 *   this one's, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - icon_venerating_laity: primary beneficiary (powerless/constrained) — gains devotional access
 *   - orthodox_ecclesial_hierarchy: agenda-setter and beneficiary (institutional/arbitrage) — administers doctrine, gains legitimacy
 *   - icon_painters_and_workshops: beneficiary (moderate/constrained) — trade legitimized
 *   - monastic_communities: beneficiary (organized/constrained) — institutional mission and historically bore persecution under the sibling reading's enforcement
 *   - theological_rigorists_wary_of_idolatry: excluded minority voice within the tradition
 *   - historians_of_byzantine_religious_conflict: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.28).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.22).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Iconodule Reading: Veneration of Images as Sanctioned Material Mediation").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, 'f99b26ac-3d21-4917-a8b2-447e4b0c3872').
narrative_ontology:cs_kernel_codification('f99b26ac-3d21-4917-a8b2-447e4b0c3872', fixed_text).
narrative_ontology:cs_authority_grounding('f99b26ac-3d21-4917-a8b2-447e4b0c3872', lineage).
narrative_ontology:cs_interpretation_layer_present('f99b26ac-3d21-4917-a8b2-447e4b0c3872').
narrative_ontology:cs_reading_relation('f99b26ac-3d21-4917-a8b2-447e4b0c3872', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_reading_relation('f99b26ac-3d21-4917-a8b2-447e4b0c3872', decalogue_image_prohibition__moderate_iconoclast_reading, coexists_with).
narrative_ontology:cs_axiom('f99b26ac-3d21-4917-a8b2-447e4b0c3872', foundational, incarnation_sanctifies_matter_as_conduit).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter_as_conduit, holdable).
narrative_ontology:cs_axiom_grounding('f99b26ac-3d21-4917-a8b2-447e4b0c3872', incarnation_sanctifies_matter_as_conduit, deontological).
narrative_ontology:cs_axiom('f99b26ac-3d21-4917-a8b2-447e4b0c3872', foundational, latria_dulia_distinction_is_theologically_stable).
narrative_ontology:cs_axiom_status(latria_dulia_distinction_is_theologically_stable, holdable).
narrative_ontology:cs_axiom_grounding('f99b26ac-3d21-4917-a8b2-447e4b0c3872', latria_dulia_distinction_is_theologically_stable, conventional).
narrative_ontology:cs_reference_frame('f99b26ac-3d21-4917-a8b2-447e4b0c3872', incarnational_sanctification_settlement).
narrative_ontology:cs_drift_state('f99b26ac-3d21-4917-a8b2-447e4b0c3872', post_reformation_iconoclast_revival, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f99b26ac-3d21-4917-a8b2-447e4b0c3872', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_venerating_laity).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, orthodox_ecclesial_hierarchy).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_painters_and_workshops).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, monastic_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordinary believers use icons as a devotional aid: kissing, bowing before, and praying in front of images of Christ, Mary, and saints, understanding the honor as passing through the image to the person depicted, not terminating in the wood and paint itself. The practice gives concrete, embodied access to divine relationship for people with no theological training. Exit means abandoning a visual and ritual vocabulary that structures most of their religious life.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_venerating_laity, beneficiary,
    powerless, biographical, constrained, regional).

% Bishops and councils (notably Nicaea II, 787) articulate and defend the latria/dulia distinction, authorize which images and iconographic conventions count as orthodox, and derive institutional legitimacy from having successfully defended sanctioned image use against iconoclast emperors. They administer the theology; they also benefit from the pastoral and political capital the settlement generates.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, orthodox_ecclesial_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconodule_reading, orthodox_ecclesial_hierarchy, beneficiary).

% Craftspeople whose livelihood depends on producing devotional images under theologically sanctioned conventions. The iconodule settlement legitimizes their trade; a sustained iconoclast enforcement regime (documented under the sibling reading) previously destroyed workshops and disrupted transmission of technique across generations.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_painters_and_workshops, beneficiary,
    moderate, generational, constrained, regional).

% Monasteries, especially in the Byzantine world, function as centers of icon production, theological defense (John of Damascus, Theodore the Studite), and resistance to imperial iconoclasm. They gain institutional standing and a defined theological mission from the iconodule position and historically bore direct persecution (exile, flogging, monastery closures) when the iconoclast reading held state power.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, monastic_communities, beneficiary,
    organized, civilizational, constrained, regional).

% Clergy and laypeople who worry that ordinary devotional practice does not reliably track the latria/dulia distinction in the minds of unsophisticated venerators — that honor paid to the image slides into worship of the image in practice, whatever the official doctrine says. Their concern is acknowledged in conciliar texts but their preferred remedy (restriction or elimination of imagery) is not adopted under this reading; they remain a minority voice inside iconodule communities rather than a suppressed party under this reading's own enforcement (the iconoclast reading is the one that suppresses, not this one).
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, theological_rigorists_wary_of_idolatry, excluded,
    moderate, generational, constrained, regional).

% Study the councils, imperial edicts, and material record (destroyed and surviving icons, hagiographic accounts of persecution) to reconstruct how the iconodule/iconoclast contest actually played out across the 8th–9th centuries and what each side's victory or defeat meant materially for the populations involved.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, historians_of_byzantine_religious_conflict, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconodule_reading, diffuse).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconodule_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The latria/dulia distinction coordinates a shared practice of visual devotion across a large, doctrinally diverse population: it lets believers use images as aids to prayer and instruction without each person needing to individually resolve the theological risk of idolatry, and it gives the hierarchy a stable, teachable rule for what counts as legitimate versus illegitimate image use.
% TRANSFER_FUNCTION: The arrangement does not primarily move material resources between parties; it allocates legitimacy — sanctioning a whole visual and devotional economy (icon production, veneration practices, pilgrimage to icon shrines) that would otherwise be vulnerable to periodic suppression. Where transfer is visible, it flows toward icon workshops, monasteries, and the ecclesial hierarchy in the form of continued commissions, endowments, and institutional standing.
% ABSENT_VOICES: Rigorist voices within the tradition who suspect that ordinary practice does not track the intended distinction are noted in the conciliar record but not given veto power; under the sibling iconoclast reading, icon-venerators themselves are the excluded/suppressed party, but that suppression belongs to the sibling constraint's operation, not this one's.
% DISAPPEARANCE_RATIONALE: If the iconodule settlement disappeared and the iconoclast reading became dominant instead (as it did, twice, in Byzantine history), the material and devotional world would visibly rearrange: existing icons would be destroyed or whitewashed, icon workshops would close, monasteries defending the practice would be persecuted or exiled, and the entire visual vocabulary of popular devotion would need to be replaced with non-figurative alternatives. The settlement is load-bearing for a large, tangible material and institutional apparatus.
% FOUNDING_PROBLEM: The early Church needed to resolve whether the Incarnation's claim that God took on visible, material human form licensed a visible, material devotional culture, or whether the prohibition on images inherited from the Decalogue foreclosed that culture regardless. Iconoclast controversies (particularly 726–787 and 814–842) forced explicit doctrinal resolution rather than leaving the question ambiguous.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the immediate beneficiary set by Byzantine imperial court records and chronicles documenting both iconodule and iconoclast emperors' reasoning, by non-Chalcedonian and Islamic-world observers of the controversy who had no stake in the internal Byzantine settlement, and by modern art-historical and religious-studies scholarship reconstructing the material destruction record on both sides. Historians broadly treat the theological question as genuinely contested at the time, not merely settled retroactively by the winning side's narrative.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconodule_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.28) because under this reading's own operation there is no rent extraction: the settlement distributes legitimacy broadly (laity, painters, monasteries, hierarchy) rather than concentrating gains against an identified payer class. Suppression is authored low-moderate (0.22) reflecting that the iconodule position, once established, requires occasional conciliar reaffirmation but not an ongoing coercive apparatus against dissenters within its own framework — the coercive apparatus belongs to the iconoclast reading when it holds power. Theater ratio is low (0.15): conciliar defense of the doctrine (Nicaea II, patristic writings) is substantive theological argument, not performative maintenance of an evacuated function. Accessibility collapse is moderate (0.35): once the latria/dulia distinction is accepted, alternative modes of devotion are not foreclosed — non-iconic prayer, textual devotion, and liturgical worship remain fully available alongside iconic veneration, so collapse is partial, consistent with a rope rather than mountain profile. Resistance is moderate (0.30): the position was and remains actively contested (by iconoclasts historically, by rigorist voices within iconodule communities, and by other traditions), but that contest is over doctrine, not over an extraction the position imposes.
 *
 * PERSPECTIVAL GAP:
 *   The hierarchy's seat (agenda_setter/beneficiary, institutional/arbitrage) and the laity's seat (beneficiary, powerless/constrained) both compute favorably under this reading because both genuinely benefit from the coordination the settlement provides — there is no structural payer class within this reading's own operation. The gap that matters is not between seats within this story but between this story and its sibling: the same believers who benefit here become the victim class under the iconoclast reading's enforcement. This is why the two readings must be separate constraint files rather than one story with a measurement parameter.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (laity, hierarchy, painters, monastics) sit near the low end of directionality because the settlement subsidizes their practice, trade, or institutional standing without extracting from them. No victims are declared under this reading because, taken on its own terms, the iconodule settlement does not identify a payer class — the historically real suffering of icon-venerators under enforcement belongs structurally to the iconoclast reading, where the same population appears as victims of an actively suppressive apparatus. Declaring victims here would improperly import the sibling constraint's structure into this one, violating the ε-invariance principle.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling the Incarnation's material sanctification with the Decalogue's prohibition) remains contested rather than dead or fully resolved even within traditions that have institutionally settled on the iconodule position — periodic revivalist and reformist movements (various strands of Protestant iconoclasm, some strands of Islamic-influenced critique in border regions) continue to reopen the question. Because the founding problem's status is 'contested' rather than 'dead,' and because no concentrated capture of rents is identified, this reading is not itself a case of mandatrophy — it is a live theological settlement that continues doing the coordination work it was built for, distinguishing it from a scenario where an institution kept enforcing a rule whose original problem had disappeared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    iconodule_reading_kernel_identity,
    'Is the iconodule reading a stable doctrinal settlement that resolved the underlying tension in the kernel, or does it remain one contested reading among several that could still be displaced by renewed iconoclast pressure?',
    'Track institutional durability across subsequent centuries and traditions: does the reading hold across doctrinal challenge (e.g., Reformation-era iconoclasm, modern minimalist devotional movements) without requiring renewed active suppression of alternatives, or does its persistence depend on continuous institutional defense against live rival readings?',
    'If the reading has genuinely stabilized as settled doctrine within its tradition with no serious internal challenge, its rope classification is robust. If it persists only because the hierarchy actively suppresses rigorist or iconoclast minority positions within its own communities, the constraint would need to be re-examined for a tangled-rope profile with an internal victim class not currently declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iconodule_reading_kernel_identity, conceptual, 'Whether the iconodule settlement is a stabilized rope or a still-contested reading requiring ongoing internal enforcement.').

omega_variable(
    latria_dulia_distinction_tracks_practice,
    'Does the theological latria/dulia distinction reliably track what ordinary venerators actually experience and intend when honoring an icon, or does official doctrine diverge from folk practice in ways that make the rigorist critique partly correct?',
    'Ethnographic and historical study of lay devotional practice and testimony (confession records, catechetical texts, popular piety literature) compared against the official conciliar distinction.',
    'If lay practice systematically collapses the distinction in practice, the low accessibility_collapse and low resistance scores authored here may understate a real internal tension the doctrine papers over, which could support authoring a distinct ''folk practice'' constraint alongside this doctrinal one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(latria_dulia_distinction_tracks_practice, empirical, 'Whether the doctrinal distinction this reading relies on is empirically borne out in lay devotional practice.').

omega_variable(
    sibling_reading_victim_overlap,
    'The same population (icon-venerating laity, monastic communities) appears as beneficiary under this reading and as victim under the iconoclast reading''s enforcement — is this overlap a sign that the two readings are not fully independent constraints but phases of a single oscillating historical struggle?',
    'Map the actual historical alternation of imperial policy (iconoclast emperors 726-787, 814-842; iconodule restoration in between) against the constraint family''s network edges to determine whether the family should be modeled as two static constraints or as a single constraint with a temporal reading-switch mechanism.',
    'If modeled as reading-switch rather than two static constraints, the temporal measurements in this story (which show flat, stable low extraction) would need to be replaced with a story that captures the actual alternation — but per the ε-invariance principle each reading keeps its own ε, so the current two-file decomposition is retained pending further analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_victim_overlap, conceptual, 'Whether the iconodule/iconoclast split should be modeled as two static constraints or a single alternating constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconodule_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(deca_tr_t200, decalogue_image_prohibition__iconodule_reading, theater_ratio, 200, 0.17).
narrative_ontology:measurement(deca_tr_t400, decalogue_image_prohibition__iconodule_reading, theater_ratio, 400, 0.16).
narrative_ontology:measurement(deca_tr_t600, decalogue_image_prohibition__iconodule_reading, theater_ratio, 600, 0.15).
narrative_ontology:measurement(deca_tr_t900, decalogue_image_prohibition__iconodule_reading, theater_ratio, 900, 0.15).
narrative_ontology:measurement(deca_tr_t1200, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1200, 0.15).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(deca_be_t200, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 200, 0.3).
narrative_ontology:measurement(deca_be_t400, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 400, 0.29).
narrative_ontology:measurement(deca_be_t600, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 600, 0.28).
narrative_ontology:measurement(deca_be_t900, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 900, 0.28).
narrative_ontology:measurement(deca_be_t1200, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1200, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(decalogue_image_prohibition__iconodule_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the decalogue_image_prohibition kernel. iconodule_reading (this file) authors low extraction and no victim class, reflecting its own internal operation as broad-based coordination. iconoclast_reading (sibling) authors substantially higher suppression and a named victim class (destroyed artworks, suppressed practices, persecuted icon-venerators) because that reading's enforcement actively suppresses the practice this reading sanctions. moderate_iconoclast_reading occupies an intermediate structural position, permitting two-dimensional images while forbidding statuary, and should author its own distinct victim set (makers and venerators of religious statuary specifically). All three share the same kernel_id (decalogue_image_prohibition) but are authored as separate constraints per the ε-invariance principle, since measuring 'the image prohibition' under each reading's own lights yields three different ε values, not one constraint with an observer parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
