% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__moderate_iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: decalogue_image_prohibition__moderate_iconoclast_reading
 *   human_readable: Moderate Iconoclast Prohibition on Three-Dimensional Statuary with Regulated Two-Dimensional Images
 *   domain: theological/religious authority/visual culture
 *
 * SUMMARY:
 *   The moderate iconoclast reading of the Decalogue's image prohibition
 *   forbids three-dimensional statuary as inherently higher-risk for idolatry
 *   while permitting two-dimensional icons under a detailed regulatory regime
 *   (canonical iconography, painter licensing, episcopal approval, prescribed
 *   veneration protocols). This reading became the dominant praxis in much of
 *   the Eastern Christian world after the Triumph of Orthodoxy (843) and
 *   persists in contemporary Orthodox canonical practice. The constraint
 *   presents itself as a theological compromise — preserving icons while
 *   guarding against idolatry — but operates as a regulatory apparatus that
 *   extracts compliance costs from local communities, eliminates sculptural
 *   competitors, and sustains the gatekeeping authority of the ecclesiastical
 *   hierarchy and its allied icon-painting guilds. The claimed type is snare:
 *   the coordination story (a middle path) is real but thin; the extraction
 *   (bureaucratic overhead, monopoly rents, suppressed alternatives) is
 *   structural and enforced.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, 0.68).
domain_priors:suppression_score(decalogue_image_prohibition__moderate_iconoclast_reading, 0.75).
domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Moderate Iconoclast Prohibition on Three-Dimensional Statuary with Regulated Two-Dimensional Images").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theological/religious authority/visual culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, '2ba8657a-ff61-4a7c-b7e2-5e156b845852').
narrative_ontology:cs_kernel_codification('2ba8657a-ff61-4a7c-b7e2-5e156b845852', fixed_text).
narrative_ontology:cs_authority_grounding('2ba8657a-ff61-4a7c-b7e2-5e156b845852', lineage).
narrative_ontology:cs_interpretation_layer_present('2ba8657a-ff61-4a7c-b7e2-5e156b845852').
narrative_ontology:cs_reading_relation('2ba8657a-ff61-4a7c-b7e2-5e156b845852', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ba8657a-ff61-4a7c-b7e2-5e156b845852', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('2ba8657a-ff61-4a7c-b7e2-5e156b845852', foundational, dimensional_hierarchy_of_idolatry_risk).
narrative_ontology:cs_axiom_status(dimensional_hierarchy_of_idolatry_risk, holdable).
narrative_ontology:cs_axiom_grounding('2ba8657a-ff61-4a7c-b7e2-5e156b845852', dimensional_hierarchy_of_idolatry_risk, deontological).
narrative_ontology:cs_axiom('2ba8657a-ff61-4a7c-b7e2-5e156b845852', foundational, regulated_material_mediation_doctrine).
narrative_ontology:cs_axiom_status(regulated_material_mediation_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('2ba8657a-ff61-4a7c-b7e2-5e156b845852', regulated_material_mediation_doctrine, conventional).
narrative_ontology:cs_reference_frame('2ba8657a-ff61-4a7c-b7e2-5e156b845852', patristic_iconic_theology).
narrative_ontology:cs_drift_state('2ba8657a-ff61-4a7c-b7e2-5e156b845852', contemporary_canonical_praxis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2ba8657a-ff61-4a7c-b7e2-5e156b845852', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_regulatory_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, icon_painting_guilds).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, sculptors_and_statuary_workshops).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, lay_devotional_practitioners).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, local_parish_communities).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, dimensional_hierarchy_of_idolatry_risk).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__moderate_iconoclast_reading, regulated_material_mediation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues detailed canons governing the production, blessing, placement, and veneration of two-dimensional icons; maintains censors and visitation circuits to enforce compliance; collects fees for licensing icon painters and approving iconographic programs. The authority's institutional survival and relevance depend on being the indispensable gatekeeper of licit visual culture.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_regulatory_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Hold a legal monopoly on the production of licit two-dimensional icons; the prohibition on statuary eliminates their sculptural competitors and the regulatory regime creates barriers to entry that protect guild masters' market position. They comply with iconographic canons in exchange for protected status and steady commissions from parishes and monasteries.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, icon_painting_guilds, beneficiary,
    organized, biographical, constrained, regional).

% Their entire craft is declared illicit; workshops are closed, tools confiscated, practitioners fined or exiled. Some convert to icon painting or architectural ornament, but the three-dimensional devotional tradition is extinguished. Exit means abandoning a multi-generational skill set and patron network for an uncertain retraining.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, sculptors_and_statuary_workshops, payer,
    moderate, biographical, constrained, regional).

% Lose access to the three-dimensional devotional forms (crucifixes, statues of saints, nativity groups) that structured their piety for generations. Permitted two-dimensional icons are regulated in ways that feel alien — prescribed gestures, approved formulas, licensed painters — turning intimate devotion into a bureaucratic procedure. Their identity is fused with the prohibited forms; exit from the constraint means exit from their own devotional self.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, lay_devotional_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Bear the compliance costs: funding licensed icon programs, hosting visitation inspections, navigating approval processes for every devotional image. The regulatory overhead consumes resources that once went directly to poor relief and building maintenance. Wealthy urban parishes absorb it; rural parishes defer maintenance or ignore rules, risking interdict.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, local_parish_communities, payer,
    moderate, generational, constrained, local).

% Argue that ANY material image in worship is idolatry; the permission of two-dimensional icons under regulation is a concession to worldliness that betrays the commandment. Their voice is structurally excluded because the moderate reading defines itself precisely against them — acknowledging their position would collapse the distinction that justifies the regulatory regime.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconoclast_hardliners, excluded,
    moderate, generational, trapped, continental).

% Maintain that the Incarnation sanctifies ALL matter, making the dimensional distinction arbitrary and the regulation a human tradition contradicting the logic of veneration. They are excluded from the regulatory apparatus because their theology would dissolve the authority's gatekeeping function — if matter is sanctified, no licensing is needed.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_theologians, excluded,
    organized, generational, constrained, continental).

% Traces how the dimensional distinction shaped Byzantine, Slavic, and Western visual culture differently; notes the correlation between regulatory intensity and artistic standardization; sees the constraint as a case study in how religious authority uses aesthetic criteria to extract institutional rent.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, art_historian_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bounded space for visual piety that avoids the extreme of total iconoclasm (which destabilizes popular devotion) and the extreme of unrestricted image use (which authorities fear enables idolatry). The regulatory apparatus coordinates iconographic orthodoxy across a vast communion.
% TRANSFER_FUNCTION: Moves compliance labor, licensing fees, and artistic control from local communities and sculptors to the ecclesiastical regulatory authority and its allied icon-painting guilds. The constraint extracts the three-dimensional devotional tradition as a raw material and converts it into a regulated two-dimensional monopoly.
% ABSENT_VOICES: The sculptors whose craft is criminalized, the rural parishioners who cannot afford licensed icons, and the lay devotees whose piety is restructured by bureaucratic prescription — none sit at the synods where canons are written. The iconoclast hardliners and iconodule theologians are excluded because their positions would dissolve the regulatory middle ground.
% DISAPPEARANCE_RATIONALE: If the prohibition and its regulatory apparatus vanished overnight, sculptors would resume three-dimensional devotional production within months; icon-painting guilds would lose their monopoly and face market competition; parishes would commission images directly from artists without licensing fees; lay devotional practice would diversify beyond prescribed formulas. The ecclesiastical authority would lose its primary lever over visual culture.
% FOUNDING_PROBLEM: The 8th-9th century iconoclast controversy threatened to fracture the empire and the church; the moderate position emerged as a compromise that preserved visual piety while drawing a defensible line against 'idolatrous' statuary.
% FOUNDING_PROBLEM_CORROBORATION: The founding crisis (imperial iconoclasm) ended in 843 with the Triumph of Orthodoxy; the moderate reading's dimensional distinction was a specific historical compromise, not a perennial principle. Modern iconodule theologians (e.g., Leonid Ouspensky, Vladimir Lossky) and art historians (e.g., Hans Belting) attest that the original polemical context is gone, yet the regulatory structure persists and has elaborated far beyond the founding settlement.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(decalogue_image_prohibition__moderate_iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__moderate_iconoclast_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(decalogue_image_prohibition__moderate_iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is substantial: the regulatory regime creates a licensed monopoly for icon painters, imposes compliance costs on every parish, and criminalizes an entire artistic tradition (statuary). The gains flow to the ecclesiastical authority (institutional relevance, fee income, control over visual culture) and the guilds (protected market). Suppression (0.75) is high: statuary is not merely discouraged but prohibited under penalty; rival theologies are excluded from the regulatory conversation; local deviations are corrected by visitation. Theater (0.42) is significant and rising: the theological rationale (dimensional hierarchy of idolatry risk) is elaborated in treatises and councils, but an increasing share of regulatory activity concerns licensing fees, painter certification, and procedural compliance rather than doctrinal fidelity. The temporal series show extraction and theater rising together over 120 time units (roughly centuries), while suppression requirement hardens — a pattern of regulatory capture and mission creep.
 *
 * PERSPECTIVAL GAP:
 *   From the authority's seat, the constraint is a necessary theological safeguard that incidentally requires administration — a rope with administrative overhead. From the sculptor's seat, it is a snare that destroys their livelihood for a distinction they reject. From the lay devotee's seat, it is a snare that reorganizes their piety around bureaucratic permission. The engine computes this divergence from the structural data; the authored claim (snare) reflects the analytical observer's reading of the overall structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The ecclesiastical regulatory authority is the primary beneficiary (d near 0.0): it collects the rents, sets the rules, and its institutional identity is constituted by the gatekeeping function. Icon-painting guilds are secondary beneficiaries (d ~ 0.2): they gain a protected monopoly but must submit to the authority's canons. Sculptors are full targets (d ~ 0.95): their craft is criminalized, exit is constrained by skill specificity. Lay devotees are identity-locked targets (d ~ 0.85): their devotional self is fused with the prohibited forms; the permitted alternative is bureaucratized and alien. Local parishes are constrained payers (d ~ 0.7): they bear compliance costs but have some absorptive capacity. The excluded seats (iconoclast hardliners, iconodule theologians) are trapped or constrained precisely because their inclusion would dissolve the regulatory middle ground.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (8th-9th century imperial iconoclasm threatening church unity) is dead — resolved in 843. The constraint persists because the regulatory apparatus it created became self-sustaining: the authority that administers it benefits from it, the guilds allied with it benefit from it, and no constituency exists that is both motivated and empowered to dismantle it. The identity-locked laity cannot organize against it; the excluded theologians lack institutional leverage. This is classic mandatrophy: a transitional compromise that became a permanent extraction structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dimensional_idolatry_risk_empirical_basis,
    'Is there any empirical or psychological basis for the claim that three-dimensional statuary poses inherently higher idolatry risk than two-dimensional images?',
    'Cross-cultural and experimental study of devotional responses to dimensional variations in religious imagery; historical analysis of whether statuary-rich traditions show higher rates of practices classified as idolatrous by the authority''s own criteria.',
    'If the dimensional risk hierarchy is empirically unfounded, the core theological justification for the prohibition collapses, revealing the constraint as pure regulatory capture. If supported, part of the measured extraction is the price of a genuine coordination function (idolatry prevention).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dimensional_idolatry_risk_empirical_basis, empirical, 'Whether the constraint''s foundational theological distinction has empirical warrant').

omega_variable(
    regulatory_elaboration_vs_theological_core,
    'What fraction of the current regulatory apparatus (licensing, visitation, iconographic canons, fee schedules) traces to the founding theological compromise vs. later institutional accretion?',
    'Historical stratification of canonical texts: distinguish the 9th-century conciliar definitions from later patriarchal encyclicals, synodal canons, and local chancery regulations.',
    'If most regulation is post-founding accretion, the constraint''s extraction is almost entirely mandatrophic — the theological core is a vestigial justification for a self-elaborated bureaucratic regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_elaboration_vs_theological_core, empirical, 'Provenance of the regulatory overhead: founding compromise vs. institutional mission creep').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of statuary and alternative theologies primarily structural (canonical penalties, episcopal enforcement) or internalized (devotees believing statuary is spiritually dangerous, artists self-censoring)?',
    'Post-reformation natural experiment: in regions where the constraint was lifted (e.g., post-Reformation Protestant areas that later reintroduced statuary), did devotional practice revert immediately or did internalized suppression persist? Ethnographic study of contemporary Orthodox laity''s affective responses to statuary.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — the target carries the suppression after formal enforcement relaxes. This would increase the omega-adjusted extraction for identity-locked seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the devotional subject').

omega_variable(
    kernel_framing_ambiguity,
    'Does the decalogue_image_prohibition kernel admit a single coherent reading, or is the ''moderate'' position structurally unstable — a forced compromise that cannot satisfy either sibling''s logic?',
    'Formal analysis of the three readings'' axiom sets: test whether the moderate reading''s axioms (dimensional hierarchy, regulated mediation) are logically compatible with each sibling''s axioms, or whether holding the moderate position requires suppressing a contradiction that the siblings expose.',
    'If the moderate reading is logically unstable (coherent only as a political compromise, not a theological position), its persistence is purely institutional — the constraint is a snare maintained by the authority that benefits from the compromise, not a genuine coordination solution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel''s moderate reading is logically coherent or a forced institutional compromise').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(deca_tr_t15, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(deca_tr_t30, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(deca_tr_t45, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 45, 0.33).
narrative_ontology:measurement(deca_tr_t60, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 60, 0.37).
narrative_ontology:measurement(deca_tr_t75, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 75, 0.39).
narrative_ontology:measurement(deca_tr_t90, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 90, 0.4).
narrative_ontology:measurement(deca_tr_t105, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 105, 0.41).
narrative_ontology:measurement(deca_tr_t120, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 120, 0.42).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(deca_be_t15, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(deca_be_t30, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement(deca_be_t45, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 45, 0.56).
narrative_ontology:measurement(deca_be_t60, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(deca_be_t75, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 75, 0.63).
narrative_ontology:measurement(deca_be_t90, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 90, 0.65).
narrative_ontology:measurement(deca_be_t105, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 105, 0.67).
narrative_ontology:measurement(deca_be_t120, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 120, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(deca_su_t15, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(deca_su_t30, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(deca_su_t45, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 45, 0.68).
narrative_ontology:measurement(deca_su_t60, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(deca_su_t75, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement(deca_su_t90, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 90, 0.73).
narrative_ontology:measurement(deca_su_t105, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 105, 0.74).
narrative_ontology:measurement(deca_su_t120, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 120, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__moderate_iconoclast_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__moderate_iconoclast_reading, 0.12).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition__iconodule_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, ecclesiastical_licensing_regime).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, icon_painting_guild_monopoly).

% DUAL FORMULATION NOTE:
% The decalogue_image_prohibition kernel decomposes into three constraint stories: iconoclast_reading (mountain/tangled_rope per seat), iconodule_reading (rope/scaffold per seat), and this moderate_iconoclast_reading (snare). The moderate reading occupies the institutional middle ground historically; it influences both siblings by defining the regulatory terms within which the dispute is managed. The iconoclast reading forecloses the moderate reading's permission of 2D images; the iconodule reading forecloses the moderate reading's prohibition of 3D statuary. The moderate reading coexists with both as live institutional positions but influences both by controlling the licensing apparatus that determines what counts as licit visual culture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decalogue_image_prohibition__moderate_iconoclast_reading, powerless, 0.85).
constraint_indexing:directionality_override(decalogue_image_prohibition__moderate_iconoclast_reading, moderate, 0.7).
constraint_indexing:directionality_override(decalogue_image_prohibition__moderate_iconoclast_reading, organized, 0.2).
constraint_indexing:directionality_override(decalogue_image_prohibition__moderate_iconoclast_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
