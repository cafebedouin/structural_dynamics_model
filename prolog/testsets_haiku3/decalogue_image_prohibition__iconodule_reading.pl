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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Iconodule Image Mediation (Dulia vs. Latria Distinction)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   The iconodule reading (also called the Second Nicene reading) interprets
 *   the Decalogue prohibition on image worship as targeting LATRIA (direct
 *   worship of the image itself) while permitting DULIA (honor paid through
 *   the image to its prototype—Christ, Mary, or the saints). The Incarnation
 *   doctrine—God became flesh in Christ—is taken to sanction matter as a
 *   valid conduit to the divine, making material images theologically
 *   coherent rather than idolatrous. This reading was formalized at the
 *   Second Council of Nicaea (787 CE) and remains the framework of Eastern
 *   Orthodox theology and medieval Catholic theology. The constraint operates
 *   as a Rope when it successfully coordinates devotional practice (laity can
 *   venerate icons without idolatry guilt) and enables the craft of icon
 *   production; it requires active enforcement (teaching the distinction,
 *   anathematizing those who conflate latria and dulia, suppressing or
 *   regulating artwork that appears to encourage latria). The constraint's
 *   type is CLAIMED as rope by the iconodule tradition; the authored metrics
 *   reflect a genuine coordination function with moderate extraction
 *   (hierarchical authority, artisan control, devotional gatekeeping) and
 *   suppression (enforcing the boundary against transgression, persecuting
 *   iconoclasts when the reading holds power, destroying imagery when rival
 *   readings dominate).
 *
 * KEY AGENTS:
 *   - Theological hierarchy: teaches and enforces the latria-dulia distinction; defines which images are orthodox
 *   - Icon artisans: produce authorized religious imagery; economic and cultural beneficiaries of the framework
 *   - Liturgical practitioners (laity and clergy): venerate icons under the framework; identity-locked participation (icon veneration is constitutive of Orthodox Christian identity)
 *   - Iconoclast enforcer (during periods of iconoclast ascendancy): forced to suppress icons and persecute icon-venerators, trapped by competing institutional authority
 *   - Icon venerators under persecution: face exile, destruction of their devotional objects, and death if they continue veneration during iconoclast dominance
 *   - Second Council of Nicaea: the institutional anchor and canonical authority for this reading
 *   - Reformed theology and theological dissenters: excluded voices arguing the latria-dulia distinction is incoherent or insufficient
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.35).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.62).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Iconodule Image Mediation (Dulia vs. Latria Distinction)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconodule_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, 'd71ca6a1-03fa-4e88-99fc-92aa8f19100c').
narrative_ontology:cs_kernel_codification('d71ca6a1-03fa-4e88-99fc-92aa8f19100c', fixed_text).
narrative_ontology:cs_authority_grounding('d71ca6a1-03fa-4e88-99fc-92aa8f19100c', lineage).
narrative_ontology:cs_interpretation_layer_present('d71ca6a1-03fa-4e88-99fc-92aa8f19100c').
narrative_ontology:cs_reading_relation('d71ca6a1-03fa-4e88-99fc-92aa8f19100c', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('d71ca6a1-03fa-4e88-99fc-92aa8f19100c', decalogue_image_prohibition__moderate_iconoclast_reading, influences).
narrative_ontology:cs_axiom('d71ca6a1-03fa-4e88-99fc-92aa8f19100c', foundational, incarnation_sanctifies_matter).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter, holdable).
narrative_ontology:cs_axiom_grounding('d71ca6a1-03fa-4e88-99fc-92aa8f19100c', incarnation_sanctifies_matter, deontological).
narrative_ontology:cs_axiom('d71ca6a1-03fa-4e88-99fc-92aa8f19100c', foundational, dulia_latria_boundary_coherent).
narrative_ontology:cs_axiom_status(dulia_latria_boundary_coherent, holdable).
narrative_ontology:cs_axiom_grounding('d71ca6a1-03fa-4e88-99fc-92aa8f19100c', dulia_latria_boundary_coherent, conventional).
narrative_ontology:cs_reference_frame('d71ca6a1-03fa-4e88-99fc-92aa8f19100c', nicene_material_mediation_framework).
narrative_ontology:cs_drift_state('d71ca6a1-03fa-4e88-99fc-92aa8f19100c', post_reformation_pluralization, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d71ca6a1-03fa-4e88-99fc-92aa8f19100c', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, theological_hierarchy).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_artisans).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, liturgical_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, iconoclast_enforcer).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, icon_venerators_under_persecution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and adjudicates the distinction between latria (worship of the image itself, forbidden) and dulia (honor paid through the image to its prototype, permitted). Sets the rules for which depictions are theologically acceptable and which constitute idolatry. Enforces this distinction by reviewing imagery, teaching doctrine, and anathematizing those who collapse the categories.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, theological_hierarchy, agenda_setter,
    institutional, civilizational, analytical, universal).

% Produce religious images under institutional authorization. Gain economic livelihood and cultural standing from authorized image-making; their work is vindicated as a legitimate craft when the latria-dulia distinction holds. Their exit involves abandoning the craft or relocating to regions without the enforcement.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_artisans, beneficiary,
    moderate, biographical, constrained, regional).

% Laity and clergy who use icons in prayer and devotion. The constraint permits them to honor Christ, Mary, and saints through material images without committing idolatry—a framework that reconciles their devotional practice with the Decalogue prohibition. Their identity as Orthodox Christians (or within other iconodule traditions) is constituted through participation in icon veneration.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, liturgical_practitioners, beneficiary,
    organized, biographical, identity_locked, universal).

% During periods of iconoclast ascendancy, those holding iconoclast theology are forced to suppress or destroy icons and prosecute icon-venerators. They are trapped by the competing institutional authority; their theological position is excluded from the framework this constraint instantiates, yet they must execute its opposite when they hold power.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconoclast_enforcer, payer,
    institutional, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconodule_reading, iconoclast_enforcer, excluded).

% During periods of iconoclast enforcement, icon-venerators face persecution, exile, or death. They are trapped: renouncing icon veneration means abandoning their constitutive devotional practice; continuing it means risking severe sanctions. Their suppression is the enforcement cost of the opposing reading's dominance.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_venerators_under_persecution, payer,
    powerless, biographical, trapped, regional).

% The ecumenical council (787 CE) that formalized the latria-dulia distinction and anathematized iconoclasm. It is the institutional anchor for this reading's authority, not itself a party to the arrangement.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, second_council_of_nicaea, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(decalogue_image_prohibition__iconodule_reading, second_council_of_nicaea).

% Protestant theological traditions that reject the latria-dulia distinction as a false solution to the idolatry problem. Would argue for stricter image prohibition or material indifference. Excluded from the framework this constraint establishes.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, reformed_theology, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(decalogue_image_prohibition__iconodule_reading, reformed_theology).

% Priests, monks, and theologians who privately doubt the latria-dulia distinction is coherent or who fear icon veneration slides into actual worship despite the categorical distinction. Not in the conversation that defines permissible practice; risking anathema if they voice dissent.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, theological_dissenters, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconodule_reading, theological_hierarchy).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconodule_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem: how can laity and clergy venerate Christ, Mary, and saints through material images without violating the Decalogue prohibition on idolatry? The latria-dulia distinction provides a framework—honor paid to the image flows through to its prototype; the image is not itself the object of worship. This coordinates devotional practice with scriptural law and enables a unified liturgical tradition across dispersed communities.
% TRANSFER_FUNCTION: Moves cultural authority and interpretive power to the theological hierarchy (they alone define what counts as proper dulia vs. illicit latria); moves economic benefit to icon artisans (whose craft is sanctioned); moves devotional legitimacy to liturgical practitioners (icon veneration is permitted under the constraint). In periods of iconoclast ascendancy, the same apparatus transfers destruction, exile, and persecution to icon-venerators and artisans.
% ABSENT_VOICES: Iconoclast theologians, Reformed critics, and theological dissenters who believe the latria-dulia distinction is incoherent or that any material mediation in worship violates the commandment. Their objection—that the framework enables a form of idolatry by a different name—is structurally excluded from the conversation that defines orthodoxy. Those who hold this view face anathema or exile.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared, icon veneration would either cease (adopting the strict prohibition reading) or operate without theological justification (collapsing dulia into latria, making practitioners tacit idolaters by their own tradition's standards). The devotional infrastructure built on the latria-dulia distinction would lose its authority foundation. Alternatively, the iconoclast reading would become the sole canonical framing, reorganizing visual culture and suppressing iconodule practice entirely.
% FOUNDING_PROBLEM: The Incarnation doctrine asserts that the divine became material in Christ. This creates a theological tension: if matter has been sanctified as a vehicle for the divine presence, how can material representation in worship be categorically forbidden by the Decalogue? Conversely, how can depiction not slide into idolatry? The founding problem is to reconcile the Incarnation (matter is sanctified) with the prohibition (no worship through images).
% FOUNDING_PROBLEM_CORROBORATION: The theological hierarchy and iconodule tradition attest the problem remains live and is solved by the latria-dulia distinction. Iconoclast theologians and Reformed critics attest the distinction is incoherent and the problem is NOT solved—they argue the founding problem persists and requires stricter prohibition. Historical analysis from outside the benefiting parties (modern theology scholars, art historians) documents that the founding problem was real, the proposed solution (latria-dulia) was intellectually coherent within medieval frameworks but remains contested, and enforcement history shows the problem's status is genuinely disputed across Christian traditions.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconodule_reading, 0.35, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.35) because the framework does solve a genuine coordination problem—it permits devotional practice that would otherwise be forbidden—but also concentrates authority in the theological hierarchy and constrains artistic and devotional expression to approved forms. Suppression is higher (0.62) because the enforcement machinery exists to maintain the latria-dulia boundary and to suppress competing interpretations; the boundary is cognitively and institutionally demanding, and transgressions (actual or alleged) trigger sanctions. Theater ratio is low-to-moderate (0.28) in stable periods—the coordination function is real and the teaching is consistent. Accessibility collapse is moderate (0.45): alternatives exist (strict prohibition, Protestant frameworks, iconoclast theology) but are socially costly and institutionally blocked during periods when the iconodule reading holds power; in other periods, the alternatives become dominant. Resistance is substantial (0.58): iconoclast theologies persist, theological dissenters question the distinction, and during periods of iconoclast enforcement, icon-venerators mount active resistance. The measurement series tracks extractiveness and suppression as relatively stable over 1200 years (post-787 CE) with slight elevation in the 400-600 year band (high-medieval institutional hardening of the distinction) and slight decline thereafter (post-Reformation pluralization reducing the framework's exclusive authority). Theater ratio rises modestly (0.12 → 0.28) as institutional enforcement becomes more theatrical over time—defending the distinction requires increasing pedagogical labor. Suppression requirement peaks at 600 years (height of medieval enforcement machinery) and plateaus thereafter.
 *
 * PERSPECTIVAL GAP:
 *   The theological hierarchy and icon artisans experience the constraint as genuine coordination—a framework that reconciles scriptural prohibition with devotional necessity and enables their institutional authority and economic livelihood. Liturgical practitioners experience it as liberation: they can venerate without guilt. Iconoclast enforcers and persecuted icon-venerators experience it as an oppressive, incoherent boundary imposed on them by rival theology. The engine's per-seat classification should reflect this divergence: the hierarchy and beneficiaries compute toward rope; the persecuted seats compute toward snare or tangled_rope (they bear costs and suppression without the coordination benefit). The perspectival gap derives from asymmetric beneficiary and victim positions mapped onto the same framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Directional values vary by agent. Theological hierarchy: d ≈ 0.1–0.2 (full beneficiary, low directionality toward target—they set the agenda, face minimal suppression, and collect authority rents). Icon artisans: d ≈ 0.25–0.35 (beneficiary but constrained—they gain livelihood but must conform to orthodox criteria; exit is costly). Liturgical practitioners: d ≈ 0.4–0.5 (near-symmetric in stable periods—genuine benefit from the coordination but identity-locked, unable to exit without abandoning their constitutive practice). Persecuted icon-venerators: d ≈ 0.85–1.0 (full target during iconoclast dominance—trapped, bearing severe suppression, identity-locked to a practice that is now forbidden). Theological dissenters: d ≈ 0.7–0.8 (high target when they voice doubts—they risk anathema and exclusion despite holding power-equivalent institutional positions). The authorized beneficiaries listed in base_properties (theological_hierarchy, icon_artisans, liturgical_practitioners) skew d downward for those seats; the victims and persecuted (icon_venerators_under_persecution, iconoclast_enforcer trapped by opposite mandates) skew d upward. No overrides are needed—structural derivation captures the asymmetry from beneficiary/victim declarations + exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy arises if the founding problem (reconciling Incarnation with image prohibition) becomes dead or contested while the constraint persists. The authored founding_problem_status is 'contested'—iconodule theologians attest the problem is live and the solution (latria-dulia distinction) works; iconoclast and Reformed theologians attest the problem persists and the solution fails. The constraint avoids mandatrophy classification because the founding problem remains genuinely contested; neither reading can claim the problem is obsolete. However, in periods of strict iconoclast dominance (e.g., Byzantine Iconoclastic period, Reformation zones), the iconodule reading's framework is SUPPRESSED rather than mandatrophied—the framework is intact conceptually but lacks institutional enforcement power. After the framework is restored (e.g., post-Nicaea II), no evidence of mandatrophy arises; practitioners resume icon veneration without shame. This suggests the framework's mandate does not atrophy when suppressed; it is a contestable but live interpretation, not an inert institutional remnant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latria_dulia_coherence,
    'Is the latria-dulia distinction (worship vs. honor-through) a coherent theological boundary, or does it collapse in practice into a distinction without difference?',
    'Historical analysis of devotional texts, prayer practices, and theological disputes within iconodule communities to determine whether the boundary held in teaching and in practice, or whether icon veneration regularly crossed the line into latria. Comparative study with iconoclast critiques of the same practices.',
    'If the distinction is incoherent or regularly transgressed, the constraint operates as a cover story for material mediation practices that functionally amount to latria—reclassifying the constraint from rope (genuine coordination) toward snare (extraction/suppression under a false rationale). If the distinction holds coherently across communities, the rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latria_dulia_coherence, empirical, 'Whether the latria-dulia distinction is a stable or illusory theological boundary in actual practice.').

omega_variable(
    incarnation_sanctification_necessity,
    'Does the Incarnation doctrine necessarily sanctify matter as a valid conduit to the divine in images, or can the Incarnation be affirmed while maintaining strict image prohibition?',
    'Comparative theology examining how different Christian traditions (Orthodox, Catholic, Reformed) reconcile Incarnation theology with image practice. Analysis of whether the logical entailment from ''God became matter'' to ''matter in images mediates to God'' is theological necessity or interpretive choice.',
    'If the entailment is necessary, the iconodule reading is the only coherent Christian framework for reconciling Incarnation + Decalogue. If the entailment is interpretive choice, other readings (strict prohibition, moderate regulation) remain equally coherent, and the iconodule reading''s theological exclusivity dissolves—reclassifying it from an ordained framework to one competing reading among options.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incarnation_sanctification_necessity, conceptual, 'Whether Incarnation doctrine logically necessitates the latria-dulia framework or permits alternative reconciliations with the image prohibition.').

omega_variable(
    enforcement_persecutory_cost,
    'Under iconoclast political dominance, does the suppression of icon veneration constitute a cost to the iconodule framework, and if so, how should it be weighted against the framework''s coordination benefits?',
    'Historical documentation of iconoclast enforcement periods (8th-9th centuries, Reformation zones) tracking the scale of icon destruction, persecution, exile, and death. Theological and ethical analysis of whether the framework is responsible for costs incurred when it is suppressed by rival readings.',
    'If suppression costs are attributed to the iconodule framework (as the target of destruction), extraction rises and the rope classification shifts toward tangled_rope or snare. If costs are attributed to iconoclast enforcement (rival reading''s actions), they do not affect the iconodule reading''s classification. This is a dispute over boundary attribution: does the framework own the cost of its own suppression?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_persecutory_cost, preference, 'Whether persecution of icon-venerators should count as extraction cost borne by the iconodule reading, or as harm caused by rival readings enforcing their position.').

omega_variable(
    kernel_contest_identity,
    'This constraint instantiates ONE reading of the contested kernel ''decalogue_image_prohibition''. The sibling reading ''iconoclast_reading'' holds that the Decalogue prohibition covers all religious imagery and that any material representation in worship violates the commandment. Which reading correctly interprets the scriptural kernel, and on what grounds?',
    'Textual hermeneutics: analysis of Exodus 20:4-5 and parallel texts in Hebrew Scripture, examining the original context, prohibitive scope, and subsequent Jewish interpretive traditions. Patristic and medieval commentary lineages from both iconodule and iconoclast sources. Theological frameworks (Incarnation doctrine, semiotics of representation) determining how each reading grounds its interpretation of the same text.',
    'Resolution would establish one reading as the canonical interpretation of the kernel, foreclosing the other within a single Christian framework. Failure to resolve maintains contestation: both readings remain live positions held by different Christian communities, coexisting without logical foreclosure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_identity, conceptual, 'The kernel-level interpretive contest: does the Decalogue prohibition permit or forbid religious imagery under any conditions?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconodule_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(deca_tr_t200, decalogue_image_prohibition__iconodule_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement(deca_tr_t400, decalogue_image_prohibition__iconodule_reading, theater_ratio, 400, 0.26).
narrative_ontology:measurement(deca_tr_t600, decalogue_image_prohibition__iconodule_reading, theater_ratio, 600, 0.29).
narrative_ontology:measurement(deca_tr_t900, decalogue_image_prohibition__iconodule_reading, theater_ratio, 900, 0.28).
narrative_ontology:measurement(deca_tr_t1200, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1200, 0.28).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(deca_be_t200, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 200, 0.32).
narrative_ontology:measurement(deca_be_t400, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 400, 0.36).
narrative_ontology:measurement(deca_be_t600, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 600, 0.35).
narrative_ontology:measurement(deca_be_t900, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 900, 0.33).
narrative_ontology:measurement(deca_be_t1200, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1200, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(deca_su_t200, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 200, 0.58).
narrative_ontology:measurement(deca_su_t400, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 400, 0.64).
narrative_ontology:measurement(deca_su_t600, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 600, 0.62).
narrative_ontology:measurement(deca_su_t900, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 900, 0.6).
narrative_ontology:measurement(deca_su_t1200, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1200, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__iconodule_reading, 0.1).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% The decalogue_image_prohibition kernel admits at least three structurally distinct readings: iconodule (matter sanctified by Incarnation, images mediate prayer, dulia permitted), strict iconoclast (all imagery forbidden), and moderate (two-dimensional images under regulation). Each reading has different ε, different beneficiary/victim structure, different enforcement mechanisms. They are linked as readings of a single contested kernel, not as variations of a single constraint. This story (iconodule_reading) treats images as a valid coordination mechanism enabling devotional practice; the iconoclast reading treats image-use as a violation of scriptural law. The moderate reading splits the difference by permitting regulated two-dimensional imagery. Each reading generates different directed extraction profiles from the same devotional community, depending on which reading holds institutional power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
