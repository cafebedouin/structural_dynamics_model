% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Iconoclast Reading of the Decalogue Image Prohibition (Total Ban on Religious Imagery)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This story instantiates the totalizing (iconoclast) reading of the
 *   Decalogue's second commandment: all material religious imagery used in
 *   worship is idolatry, full stop, with no distinction between worship of an
 *   image and honor rendered through it to its prototype. Historically this
 *   reading was enforced by Byzantine imperial authority across two major
 *   periods (roughly 726-787 and 815-843 CE, modeled here on a compressed
 *   timeline), backed by state coercion — image destruction, deposition of
 *   iconodule patriarchs, monastic property seizure, and exile or execution
 *   of resistant monastics. The reading functioned simultaneously as a
 *   sincere theological position for many clergy and as a mechanism for the
 *   imperial center to break the independent economic and spiritual authority
 *   that monasteries and provincial churches had built around image
 *   veneration. This is ONE of three declared readings of the shared kernel
 *   (decalogue_image_prohibition); the iconodule_reading and
 *   moderate_iconoclast_reading are separate constraint stories with their
 *   own ε values, beneficiary/victim structures, and classifications — this
 *   file does not average across them or describe the contest internally.
 *
 * KEY AGENTS:
 *   - centralizing_imperial_authority: primary beneficiary (institutional/arbitrage) — uses the totalizing doctrine to break monastic and provincial independent religious authority and consolidate control over religious form
 *   - iconoclast_clergy_faction: co-beneficiary (institutional/constrained) — advances genuinely held doctrinal position that also elevates their faction's authority within church hierarchy
 *   - icon_producers: primary target (powerless/trapped) — livelihood and craft criminalized, workshops destroyed
 *   - monastic_communities: primary target (organized/constrained) — property seized, communities dispersed or forced to comply, resistant monastics exiled or killed
 *   - lay_devotional_practitioners: diffuse victim (powerless/trapped) — established devotional practice criminalized, generational religious culture disrupted
 *   - provincial_churches_with_established_image_cults: institutional victim (moderate/constrained) — local religious authority and pilgrimage economies tied to venerated images dismantled
 *   - iconodule_theologians: excluded/resistant voice (moderate/constrained) — articulate the latria/dulia distinction but are suppressed, exiled, or executed for the duration of enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.68).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.79).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, tangled_rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Iconoclast Reading of the Decalogue Image Prohibition (Total Ban on Religious Imagery)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, 'fbfc6395-c126-474e-ae49-490dd208bac7').
narrative_ontology:cs_kernel_codification('fbfc6395-c126-474e-ae49-490dd208bac7', fixed_text).
narrative_ontology:cs_authority_grounding('fbfc6395-c126-474e-ae49-490dd208bac7', extraction).
narrative_ontology:cs_interpretation_layer_present('fbfc6395-c126-474e-ae49-490dd208bac7').
narrative_ontology:cs_reading_relation('fbfc6395-c126-474e-ae49-490dd208bac7', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_axiom('fbfc6395-c126-474e-ae49-490dd208bac7', foundational, material_representation_is_categorically_idolatrous).
narrative_ontology:cs_axiom_status(material_representation_is_categorically_idolatrous, holdable).
narrative_ontology:cs_axiom_grounding('fbfc6395-c126-474e-ae49-490dd208bac7', material_representation_is_categorically_idolatrous, deontological).
narrative_ontology:cs_axiom('fbfc6395-c126-474e-ae49-490dd208bac7', secondary, latria_dulia_distinction_is_incoherent).
narrative_ontology:cs_axiom_status(latria_dulia_distinction_is_incoherent, overridden).
narrative_ontology:cs_axiom_grounding('fbfc6395-c126-474e-ae49-490dd208bac7', latria_dulia_distinction_is_incoherent, conventional).
narrative_ontology:cs_reference_frame('fbfc6395-c126-474e-ae49-490dd208bac7', mosaic_aniconic_covenant).
narrative_ontology:cs_drift_state('fbfc6395-c126-474e-ae49-490dd208bac7', post_nicaea_ii_restoration, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('fbfc6395-c126-474e-ae49-490dd208bac7', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, iconoclast_clergy_faction).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_producers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, lay_devotional_practitioners).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, provincial_churches_with_established_image_cults).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgates and enforces the total prohibition through imperial edict, deposes iconodule patriarchs, and directs seizure of monastic property associated with image veneration. Can revise or reverse the doctrine at will when politically advantageous, as later emperors did. Collects consolidated authority over religious form and redirected monastic wealth.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Provides theological justification for the total ban and administers enforcement within church structures, elevating their faction's standing over rival iconodule clergy. Their institutional position depends on imperial backing continuing; when it withdraws, as in the interregnum, their authority contracts sharply.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconoclast_clergy_faction, beneficiary,
    institutional, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconoclast_reading, iconoclast_clergy_faction, agenda_setter).

% Their craft — painting and carving devotional images — is criminalized outright. Workshops are destroyed, existing stock confiscated or burned, and continued practice risks punishment. They have no institutional voice and no alternative market for the skill within the enforcement zone; exit means abandoning the trade entirely or practicing in secret at personal risk.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_producers, payer,
    powerless, biographical, trapped, local).

% Monasteries that had built spiritual authority, pilgrimage revenue, and property around venerated images face seizure of that property and forced doctrinal compliance. Resistant monastics face exile or execution. They retain organizational cohesion enabling some collective resistance (flight to unenforced regions, underground practice, later doctrinal counter-argument at councils) but cannot simply exit the empire's jurisdiction without losing their institutional base.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_communities, payer,
    organized, generational, constrained, regional).

% Ordinary believers whose devotional practice centered on venerating images of Christ, Mary, and saints have that practice declared idolatrous overnight. They have no institutional standing to contest the doctrine and must either abandon inherited devotional forms, practice covertly, or accept the reinterpretation.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, lay_devotional_practitioners, payer,
    powerless, biographical, trapped, local).

% Local churches whose regional authority and pilgrimage economy depended on venerated images lose both religious standing and revenue when those images are removed or destroyed. Some negotiate partial compliance; open defiance risks direct imperial intervention.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, provincial_churches_with_established_image_cults, payer,
    moderate, generational, constrained, regional).

% Articulate the latria/dulia distinction and the Incarnation-based argument for permissible image veneration, but are excluded from the doctrinal conversation by force during enforcement periods — deposed, exiled, or in extreme cases executed. Their argument persists in written form and eventually prevails at Nicaea II and in 843, but during the periods this story models, they have no seat at the table.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconodule_theologians, excluded,
    moderate, biographical, constrained, continental).

% The ecumenical councils (Nicaea II in 787, and the 843 restoration) that eventually adjudicate the kernel dispute and formally reverse the totalizing reading, restoring the iconodule position as binding doctrine. Observes the enforcement record retrospectively and rules on it, but was not a live check on enforcement during the periods this story covers.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, later_conciliar_authority, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconoclast_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, unambiguous interpretive standard for a commandment whose scope is genuinely contested, removing the need for case-by-case theological adjudication of which images are acceptable and which constitute idolatry.
% TRANSFER_FUNCTION: Moves monastic property, provincial pilgrimage revenue, craft livelihoods, and devotional practice away from icon-dependent religious communities and toward centralized imperial and allied-clerical authority, under cover of doctrinal purification.
% ABSENT_VOICES: Iconodule theologians and the lay devotional communities whose practice is criminalized would object on both theological grounds (the latria/dulia distinction, the Incarnation argument) and practical grounds (loss of livelihood and inherited practice), but are excluded from the enforcement-period conversation by exile, deposition, or execution; their position resurfaces only once the coercive backing shifts at the councils.
% DISAPPEARANCE_RATIONALE: If the totalizing prohibition and its enforcement apparatus vanished, icon production would resume immediately (as it did historically once enforcement lapsed), monastic communities would recover devotional and economic functions built around images, and the imperial center would lose a mechanism for suppressing rival provincial and monastic religious authority — the visual and institutional religious landscape would visibly reorganize, as it did at Nicaea II and in 843.
% FOUNDING_PROBLEM: The commandment against graven images was originally directed at literal worship of carved idols as gods in themselves — the founding concern was that material objects would be mistaken for or substituted for the divine reality they were meant, at most, to represent.
% FOUNDING_PROBLEM_CORROBORATION: Iconoclast emperors and allied clergy attest the founding problem remains fully live and that any material representation risks the same idolatrous confusion regardless of intent. Iconodule theologians — writing from exile, and later vindicated at two ecumenical councils — attest the founding problem was never about honor-through-image and that the totalizing reading conflates a real danger (worship OF matter) with a distinct and permissible practice (honor THROUGH matter to its prototype). Later conciliar authority, external to both live factions at the time of the dispute, ultimately corroborates the iconodule reading as the doctrinally settled one, which is itself evidence that the totalizing reading's claim to have correctly identified the founding problem was not sustained outside the enforcing faction.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconoclast_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction rises through the enforcement period (0.35 to 0.68 across the interval) as the doctrine moves from proclamation to systematic asset seizure and suppression of image-dependent religious economies. Suppression is authored high (0.79 at peak) because persistence depends entirely on active imperial coercion — deposed patriarchs, exiled monastics, destroyed icons — not on voluntary theological consensus; the moment coercive backing weakens (the interregnum represented at t=100), enforcement visibly relaxes before the second iconoclast period reasserts it. Theater ratio is moderate (0.42) reflecting that some enforcement is genuine doctrinal conviction while a substantial share is performative assertion of imperial religious authority over provincial and monastic rivals. Accessibility collapse (0.6) and resistance (0.72) are both substantial and roughly matched — the doctrine did not eliminate the alternative (the iconodule position persisted underground and eventually prevailed at Nicaea II and again in 843) but it did meet and require sustained active resistance, which is inconsistent with a genuine mountain and consistent with a constructed, contested, enforcement-dependent constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the imperial/clerical agenda-setter seat, this reads as a Rope: solving a genuine coordination problem (preventing what they perceive as literal idolatry, unifying religious practice under one interpretive standard). From the icon producer, monastic, and lay devotional seat, the identical structure reads as a Snare: their livelihood, community, and inherited devotional practice are criminalized and destroyed by an apparatus whose coordination story does not survive contact with its selective and instrumentalized application. The engine's per-seat computation should reflect this asymmetry directly from the declared power/exit/beneficiary-victim structure, not from any claim I make about which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Centralizing imperial authority sits at the full-beneficiary end: it collects consolidated religious-political authority and monastic wealth, and holds arbitrage-level exit (it authors and can revise the doctrine at will, subject only to eventual conciliar reversal). Iconoclast clergy are near-beneficiaries but with constrained exit — their institutional position depends on the doctrine holding, so if the political wind shifts (as it does at t~100) they are institutionally exposed. Icon producers and lay devotional practitioners are near-full targets: trapped exit, no institutional recourse, their entire economic or devotional activity is criminalized by the same structure that claims to coordinate correct worship. Monastic communities are targets with organized (not merely powerless) standing — they retain some capacity for collective resistance (flight, underground practice, eventual doctrinal counter-argument) which is why they are not modeled as fully trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine concern about idol worship displacing worship of God — was a live theological question at the commandment's origin and remains contestable in principle. But by the height of Byzantine enforcement, the mechanism had visibly outgrown any function distinguishable from imperial consolidation: enforcement intensity tracked political contest for religious authority (rising under emperors seeking to break monastic power, receding during the interregnum) rather than tracking any change in the theological facts. Classifying this as tangled_rope rather than snare preserves the genuine (if contested) coordination function the doctrine claims — some clergy held it sincerely, and the underlying question of image veneration is not frivolous — while still naming the asymmetric extraction (monastic property, provincial religious economies, craft livelihoods) that required continuous imperial coercion to sustain. A pure snare classification would erase the sincere theological content; a pure rope classification would erase the victims and the coercion record.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Is the total prohibition on religious imagery the correct reading of the second commandment''s scope, or does the commandment target only worship-directed latria while leaving honor-through-image (dulia) untouched, as the iconodule reading holds?',
    'No empirical resolution exists; the question is theological/exegetical. Historically it was ''resolved'' politically by imperial council decree (Nicaea II, 787; Constantinople, 843) rather than by argument that persuaded all parties — the losing side did not concede the premise, it lost the coercive apparatus.',
    'If the totalizing reading is correct, the constraint''s victim set (icon producers, monastics, lay devotional practice) genuinely violates a divine command and the extraction is justified enforcement of sacred law. If the latria/dulia distinction holds, the same enforcement apparatus is extracting compliance and destroying capital (icons, monastic livelihoods, devotional infrastructure) on the basis of a category error, making the coordination story a cover for centralizing control over religious form.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Whether the totalizing reading or the latria/dulia distinction is the correct interpretation of the kernel.').

omega_variable(
    imperial_motive_vs_doctrinal_conviction,
    'Was iconoclasm primarily a genuine theological conviction among clergy and emperors, or primarily an instrument for centralizing religious authority away from monasteries and provincial cults that had accumulated independent legitimacy and wealth through image veneration?',
    'Comparative analysis of iconoclast emperors'' other centralizing acts (monastic land seizure, patriarchal appointments, provincial administrative reform) against the timing and selectivity of iconoclast enforcement; correspondence and conciliar records for stated versus revealed motive.',
    'If primarily doctrinal, the constraint is closer to a sincerely-held (if contested) Rope/Mountain-adjacent claim administered badly. If primarily instrumental, the theological claim is cover for asset transfer from monasteries/provincial churches to imperial center — supporting the tangled_rope classification over a pure doctrinal reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_motive_vs_doctrinal_conviction, empirical, 'Whether doctrinal conviction or imperial centralization was the primary driver of enforcement.').

omega_variable(
    material_mediation_categorical_claim,
    'Is ''any material representation used in worship'' a coherent, stable category, or does it collapse under scrutiny (e.g., does it also prohibit the cross, relics, the consecrated elements, illuminated scripture)?',
    'Trace enforcement consistency: did iconoclast authorities apply the total-prohibition logic evenly across all material religious objects, or selectively to painted/carved images of persons while exempting the cross and other material objects?',
    'Selective application would indicate the categorical claim is not applied as stated, suggesting the operative rule is narrower (or differently motivated) than the totalizing doctrine announces — evidence for the imperial-motive omega above.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_mediation_categorical_claim, empirical, 'Whether the totalizing category is applied consistently or selectively.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(deca_tr_t60, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(deca_tr_t80, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(deca_tr_t100, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 100, 0.34).
narrative_ontology:measurement(deca_tr_t120, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 120, 0.42).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(deca_be_t60, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(deca_be_t80, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 80, 0.66).
narrative_ontology:measurement(deca_be_t100, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 100, 0.55).
narrative_ontology:measurement(deca_be_t120, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 120, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(deca_su_t60, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 60, 0.79).
narrative_ontology:measurement(deca_su_t80, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 80, 0.7).
narrative_ontology:measurement(deca_su_t100, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 100, 0.5).
narrative_ontology:measurement(deca_su_t120, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 120, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__iconoclast_reading, 0.1).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, iconodule_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This story, iconodule_reading, and moderate_iconoclast_reading form a three-member constraint family reading the same kernel (decalogue_image_prohibition) differently. Each reading has its own ε: this totalizing reading is authored substantially extractive and enforcement-dependent (tangled_rope) because its victim set is broad and its persistence tracked imperial political contest rather than settled doctrine. The moderate_iconoclast_reading is expected to show a narrower victim set (three-dimensional statuary producers only) and correspondingly lower ε. The iconodule_reading is expected to show the lowest ε and closer to rope/mountain status, since it is the reading that ultimately achieved durable conciliar consensus without requiring sustained coercion to hold. Do not average these three ε values — they are three distinct constraints sharing a textual kernel, not three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decalogue_image_prohibition__iconoclast_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
