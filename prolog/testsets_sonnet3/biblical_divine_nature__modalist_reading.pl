% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__modalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__modalist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: biblical_divine_nature__modalist_reading
 *   human_readable: Modalist (Sabellian) Reading of the Divine Nature: Sequential Modes of One Person
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   This constraint authors the modalist (Sabellian/Monarchian) reading of
 *   the biblical divine nature kernel: Father, Son, and Spirit are one person
 *   appearing sequentially in different modes of self-revelation rather than
 *   three simultaneously existing persons sharing one essence. This is a live
 *   theological commitment held by historic Monarchian communities (Noetus,
 *   Praxeas, Sabellius) and by strands of the modern Oneness Pentecostal
 *   tradition, generating real institutional consequences: teachers and
 *   congregants holding this view have been formally condemned, disciplined,
 *   and excluded from mainstream Trinitarian communion since at least the
 *   early third century. The story is authored from within the modalist
 *   commitment's own structural logic, not as an even-handed survey of the
 *   underlying biblical text (which is a separate, distinct question from any
 *   single reading of it).
 *
 * KEY AGENTS:
 *   - modalist_clergy_and_teachers: agenda-setters who administer the doctrine within their communities
 *   - jesus_centered_devotional_communities: beneficiaries of doctrinal simplicity and direct Christocentric piety
 *   - modalist_congregants_denied_mainstream_communion: powerless payers bearing exclusion costs
 *   - clergy_disciplined_for_modalist_teaching: moderate-power payers bearing career and institutional costs
 *   - trinitarian_ecclesial_authorities: the excluded-from-this-frame but dominant enforcing power
 *   - unitarian_communities: excluded critics from the opposite doctrinal direction
 *   - religious_historians_and_patristics_scholars: analytical observers of the whole contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__modalist_reading, 0.42).
domain_priors:suppression_score(biblical_divine_nature__modalist_reading, 0.55).
domain_priors:theater_ratio(biblical_divine_nature__modalist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__modalist_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__modalist_reading, "Modalist (Sabellian) Reading of the Divine Nature: Sequential Modes of One Person").
narrative_ontology:topic_domain(biblical_divine_nature__modalist_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__modalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__modalist_reading, '963e8ed7-8442-4096-bc86-469fdba42a47').
narrative_ontology:cs_kernel_codification('963e8ed7-8442-4096-bc86-469fdba42a47', fixed_text).
narrative_ontology:cs_authority_grounding('963e8ed7-8442-4096-bc86-469fdba42a47', lineage).
narrative_ontology:cs_interpretation_layer_present('963e8ed7-8442-4096-bc86-469fdba42a47').
narrative_ontology:cs_reading_relation('963e8ed7-8442-4096-bc86-469fdba42a47', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('963e8ed7-8442-4096-bc86-469fdba42a47', biblical_divine_nature__unitarian_reading, coexists_with).
narrative_ontology:cs_axiom('963e8ed7-8442-4096-bc86-469fdba42a47', foundational, divine_persons_are_sequential_not_simultaneous).
narrative_ontology:cs_axiom_status(divine_persons_are_sequential_not_simultaneous, holdable).
narrative_ontology:cs_axiom_grounding('963e8ed7-8442-4096-bc86-469fdba42a47', divine_persons_are_sequential_not_simultaneous, conventional).
narrative_ontology:cs_axiom('963e8ed7-8442-4096-bc86-469fdba42a47', foundational, monotheism_requires_numerical_identity_of_subject_across_modes).
narrative_ontology:cs_axiom_status(monotheism_requires_numerical_identity_of_subject_across_modes, holdable).
narrative_ontology:cs_axiom_grounding('963e8ed7-8442-4096-bc86-469fdba42a47', monotheism_requires_numerical_identity_of_subject_across_modes, deontological).
narrative_ontology:cs_reference_frame('963e8ed7-8442-4096-bc86-469fdba42a47', monarchian_economic_revelation_framework).
narrative_ontology:cs_drift_state('963e8ed7-8442-4096-bc86-469fdba42a47', post_nicene_conciliar_consolidation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('963e8ed7-8442-4096-bc86-469fdba42a47', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__modalist_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, modalist_clergy_and_teachers).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, jesus_centered_devotional_communities).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, modalist_congregants_denied_mainstream_communion).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, clergy_disciplined_for_modalist_teaching).
narrative_ontology:constraint_vindicates(biblical_divine_nature__modalist_reading, strict_monotheism_doctrine).
narrative_ontology:constraint_vindicates(biblical_divine_nature__modalist_reading, economic_self_revelation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach that Father, Son, and Spirit are successive self-revelations of a single divine person across salvation history rather than three co-existing persons. They administer congregations, catechize believers into this reading, and defend it against charges of heresy from Trinitarian bishops. Their institutional standing and doctrinal authority depend on this framework's continued acceptance within their communities.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, modalist_clergy_and_teachers, agenda_setter,
    organized, generational, constrained, regional).

% Ordinary believers who find in modalism a simplified, emotionally direct relationship to Jesus as fully God without needing to hold Trinitarian logical apparatus (three persons, one essence) in mind. They benefit from doctrinal simplicity and strong Christocentric piety, but their communities risk isolation from broader Christian fellowship networks that require Trinitarian confession.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, jesus_centered_devotional_communities, beneficiary,
    moderate, biographical, constrained, regional).

% Lay believers raised in or converted to modalist teaching who are excluded from communion, ordination, or fellowship in Trinitarian-majority churches once their view becomes known. They bear the social and spiritual cost of a doctrinal dispute they did not create, often without the theological training to defend their position or the resources to relocate to modalist-friendly communities.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, modalist_congregants_denied_mainstream_communion, payer,
    powerless, biographical, trapped, local).

% Pastors and teachers who adopt or drift toward modalist language (often to make the Trinity comprehensible to congregants) and are subsequently investigated, censured, defrocked, or expelled by Trinitarian denominational authorities citing the Sabellian condemnation. They pay in career, reputation, and community standing for a Christological reading that predates most institutional creeds by over a century.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, clergy_disciplined_for_modalist_teaching, payer,
    moderate, biographical, constrained, regional).

% Historic and contemporary church bodies (councils, magisteria, denominational hierarchies) that regard modalism as the condemned heresy of Sabellianism, incompatible with baptismal formulas and creedal confession of three co-eternal persons. They are 'excluded' from this constraint's internal frame only in the sense that this reading is authored from within modalism; they are the primary external force enforcing exclusion against modalist adherents.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, trinitarian_ecclesial_authorities, excluded,
    institutional, civilizational, analytical, global).

% Communities holding that only the Father is fully God and that Son and Spirit are subordinate or created, who regard modalism as an inadequate half-measure that still improperly attributes full deity to Jesus and the Spirit as manifestations, diluting numerical divine singularity. They are outside this reading's own frame and are not consulted by it, but their critique sharpens the doctrinal contest around this kernel.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, unitarian_communities, excluded,
    organized, generational, constrained, regional).

% Study the textual and historical record of Sabellius, Noetus, Praxeas, and the third-century Monarchian controversies, along with modern revivals (e.g., certain Oneness Pentecostal traditions). They document how the modalist reading emerged, was condemned, and persists in living communities, without being party to the ecclesial dispute over its legitimacy.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, religious_historians_and_patristics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Modalism solves a genuine problem for believers: how to affirm the full, undiluted deity of Jesus Christ and the Holy Spirit while preserving strict numerical monotheism, without requiring mastery of the philosophical vocabulary (hypostasis, ousia, persona) that Trinitarian formulations demand. It coordinates devotional life around a single, simple divine subject who acts differently at different times.
% TRANSFER_FUNCTION: The arrangement moves doctrinal authority and communal belonging: modalist teachers who successfully hold congregations within this framework retain their teaching authority and community cohesion, while transferring the risk of ecclesial censure and exclusion onto ordinary adherents and lower clergy who must bear the consequences when the reading collides with Trinitarian institutional power.
% ABSENT_VOICES: Trinitarian conciliar authorities and unitarian communities are structurally absent from the modalist reading's own internal justification — the reading is authored from within modalist commitment and does not internally represent the conciliar arguments (from Tertullian, the Cappadocians, or later creeds) that led to its condemnation, nor the unitarian charge that it still over-attributes deity to Jesus.
% DISAPPEARANCE_RATIONALE: Modalist communities would say the world rearranges: without this reading, their entire devotional and catechetical structure collapses and they would have to either adopt Trinitarian metaphysics or unitarian subordinationism, both of which they reject. Mainstream Trinitarian institutions would say the world is largely unchanged or improved, since modalism's disappearance simply removes a historically condemned position; the parties dispute which frame is correct.
% FOUNDING_PROBLEM: Early Christian communities (2nd-3rd century, associated with Noetus, Praxeas, and especially Sabellius) needed a way to affirm that Jesus was fully and truly God — against adoptionist or subordinationist views that made him a lesser or created being — while avoiding what looked to them like tritheism (worship of three separate gods) inherent in emerging language about a distinct Father, Son, and Spirit.
% FOUNDING_PROBLEM_CORROBORATION: Patristic-era opponents (Tertullian, writing against Praxeas, and later the Cappadocian fathers) attest that the problem modalism responded to — the demand for a coherent monotheism that still fully honored Christ's deity — was real, even as they argued Sabellius's solution was incoherent and heretical. Contemporary historians of doctrine (outside both the modalist and Trinitarian confessing communities) corroborate that the underlying tension between monotheism and Christ's full deity was the genuine founding problem, while noting the specific modalist resolution was a minority position defeated within the developing conciliar consensus by the fourth century.
narrative_ontology:disappearance_verdict(biblical_divine_nature__modalist_reading, contested).
narrative_ontology:founding_problem_status(biblical_divine_nature__modalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__modalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_divine_nature__modalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__modalist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__modalist_reading_tests).
:- end_tests(biblical_divine_nature__modalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate: the doctrine itself extracts little in the way of material resources, but it extracts communal belonging and institutional standing from adherents who face exclusion once Trinitarian-majority bodies identify them as modalist. Suppression (0.55) reflects genuine historical enforcement — formal condemnation at regional synods, excommunication, loss of clerical office — directed at modalist teachers and congregants by the dominant Trinitarian institutional apparatus, though this suppression is exercised primarily BY the sibling reading's institutions rather than by modalism's own administration of its adherents. Theater ratio (0.3) reflects that a meaningful share of the ongoing doctrinal defense (catechetical apologetics distinguishing modalism from both Trinitarianism and unitarianism) is genuine theological work, not pure performance, though some heresiological rhetoric on both sides functions performatively to mark group boundaries rather than resolve substance. Accessibility collapse (0.4) is moderate-low: because modalism is a minority reading contested by two other live readings, alternatives have not collapsed — a modalist adherent can and often does encounter Trinitarian or unitarian arguments and can shift positions, unlike a genuine natural law. Resistance (0.68) is high, capturing the sustained, centuries-long resistance modalism has met from the conciliar Trinitarian mainstream, which treats it as a named heresy (Sabellianism) rather than a live option.
 *
 * DIRECTIONALITY LOGIC:
 *   Modalist clergy and teachers sit near the beneficiary end: they administer the doctrine, retain teaching authority within their communities, and are insulated by institutional position from the harshest costs of exclusion when their congregations remain doctrinally homogeneous. Jesus-centered devotional communities also skew toward benefit: they receive spiritual and devotional simplicity. The powerless congregants and the moderate-power disciplined clergy sit near the target end: they bear the concrete costs of exclusion, censure, and loss of standing when their modalist commitments collide with dominant Trinitarian institutions, and their exit options are constrained or trapped — leaving means abandoning either their community or their theological conviction, both costly. Trinitarian and unitarian communities are exogenous to this reading's internal structure; they are not coordinated or extracted from BY modalism, they are the external force acting on it, hence 'excluded' rather than beneficiary/payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than a clean rope or a pure snare) prevents two mislabelings. It is not a pure snare, because there is a genuine coordination function — modalism does solve a real devotional and logical problem (affirming Christ's full deity within strict monotheism) for the communities that hold it, and its adherents are not simply being defrauded; many find it theologically and spiritually satisfying on its own terms. But it is not a clean rope either, because the same structure that coordinates devotional life for its administering teachers imposes real, asymmetric costs — exclusion, discipline, loss of communion — on congregants and lower clergy who did not choose the disciplinary consequences that flow from holding the position, and because active enforcement (from the OPPOSING Trinitarian institutions, but structurally required for the extraction to register) is a necessary feature of the situation modalist adherents actually live in. The tangled_rope frame keeps both truths in view without collapsing the doctrine's genuine coordination value into pure cynicism, or excusing away the real costs borne by ordinary believers caught in a doctrinal dispute inherited from the third century.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sabellian_heresy_label_provenance,
    'Is the ''Sabellian heresy'' condemnation a genuine theological refutation of an incoherent position, or a boundary-marking exercise by the winning conciliar faction that could equally have gone the other way given different fourth-century political outcomes?',
    'Comparative historical analysis of the political and institutional dynamics of the councils that condemned Monarchianism/Sabellianism, weighed against the internal theological coherence of the Trinitarian alternative as debated at the time, informed by scholarship independent of confessional commitment to either outcome.',
    'If primarily political, the suppression metric for modalist adherents should be read as closer to pure institutional power exercise; if primarily theological, the suppression reflects a genuine doctrinal disqualification that modalist communities have not adequately answered.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sabellian_heresy_label_provenance, conceptual, 'Whether the historic condemnation of modalism was theological substance or institutional power consolidation.').

omega_variable(
    modalism_beneficiary_administrator_gap,
    'Do modalist clergy and teachers genuinely benefit from administering this doctrine, or are they themselves also constrained by inherited tradition and personal conviction such that ''beneficiary'' overstates their agency?',
    'Examine whether modalist teachers who convert away from the position face comparable or lesser institutional costs than lay converts, which would indicate whether clerical position provides genuine protective benefit or merely delays the same costs.',
    'If clergy face costs comparable to laity upon conversion, the beneficiary/payer asymmetry central to the tangled_rope classification weakens, pushing the reading closer to a rope (shared risk, shared conviction) than a tangled_rope (asymmetric extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modalism_beneficiary_administrator_gap, empirical, 'Whether clerical administration of modalism constitutes genuine structural benefit or merely deferred shared risk.').

omega_variable(
    kernel_framing_text_vs_creed,
    'Should this constraint be framed as a reading of the underlying biblical text itself, or as a reading of the POST-biblical doctrinal tradition that formalized ''persons'' and ''modes'' language the text itself does not use?',
    'None fully available — this is a live hermeneutical dispute; a resolution would require agreement on whether later creedal vocabulary (hypostasis, mode, person) is a faithful development of the biblical data or an imposition upon it.',
    'If the kernel is better framed as ''reading of post-biblical creedal formulation'' rather than ''reading of biblical text,'' the ε referent and the coordination function shift from textual interpretation to institutional doctrine-formation, which could change how the coordination function is described without changing the authored ε for this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_text_vs_creed, conceptual, 'Alternative framing of the kernel as text-interpretation versus post-textual creedal formation; documented per the CS-framing under-determination guidance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__modalist_reading, 0, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__modalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t220, biblical_divine_nature__modalist_reading, theater_ratio, 220, 0.2).
narrative_ontology:measurement_basis(bibl_tr_t220, observed).
narrative_ontology:measurement(bibl_tr_t325, biblical_divine_nature__modalist_reading, theater_ratio, 325, 0.28).
narrative_ontology:measurement_basis(bibl_tr_t325, observed).
narrative_ontology:measurement(bibl_tr_t900, biblical_divine_nature__modalist_reading, theater_ratio, 900, 0.3).
narrative_ontology:measurement_basis(bibl_tr_t900, observed).
narrative_ontology:measurement(bibl_tr_t1500, biblical_divine_nature__modalist_reading, theater_ratio, 1500, 0.32).
narrative_ontology:measurement_basis(bibl_tr_t1500, observed).
narrative_ontology:measurement(bibl_tr_t1800, biblical_divine_nature__modalist_reading, theater_ratio, 1800, 0.3).
narrative_ontology:measurement_basis(bibl_tr_t1800, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__modalist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t220, biblical_divine_nature__modalist_reading, base_extractiveness, 220, 0.38).
narrative_ontology:measurement_basis(bibl_be_t220, observed).
narrative_ontology:measurement(bibl_be_t325, biblical_divine_nature__modalist_reading, base_extractiveness, 325, 0.5).
narrative_ontology:measurement_basis(bibl_be_t325, observed).
narrative_ontology:measurement(bibl_be_t900, biblical_divine_nature__modalist_reading, base_extractiveness, 900, 0.4).
narrative_ontology:measurement_basis(bibl_be_t900, observed).
narrative_ontology:measurement(bibl_be_t1500, biblical_divine_nature__modalist_reading, base_extractiveness, 1500, 0.35).
narrative_ontology:measurement_basis(bibl_be_t1500, observed).
narrative_ontology:measurement(bibl_be_t1800, biblical_divine_nature__modalist_reading, base_extractiveness, 1800, 0.42).
narrative_ontology:measurement_basis(bibl_be_t1800, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__modalist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t220, biblical_divine_nature__modalist_reading, suppression_requirement, 220, 0.45).
narrative_ontology:measurement_basis(bibl_su_t220, observed).
narrative_ontology:measurement(bibl_su_t325, biblical_divine_nature__modalist_reading, suppression_requirement, 325, 0.7).
narrative_ontology:measurement_basis(bibl_su_t325, observed).
narrative_ontology:measurement(bibl_su_t900, biblical_divine_nature__modalist_reading, suppression_requirement, 900, 0.55).
narrative_ontology:measurement_basis(bibl_su_t900, observed).
narrative_ontology:measurement(bibl_su_t1500, biblical_divine_nature__modalist_reading, suppression_requirement, 1500, 0.5).
narrative_ontology:measurement_basis(bibl_su_t1500, observed).
narrative_ontology:measurement(bibl_su_t1800, biblical_divine_nature__modalist_reading, suppression_requirement, 1800, 0.55).
narrative_ontology:measurement_basis(bibl_su_t1800, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__modalist_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__unitarian_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language kernel 'the nature of God in Christian scripture' per the ε-invariance principle: modalist_reading, trinitarian_reading, and unitarian_reading each interpret the same textual corpus to structurally distinct, mutually exclusive conclusions about the number and simultaneity of divine persons, and each carries a different beneficiary/victim structure and a different authored ε. Trinitarian_reading (the conciliar-mainstream reading) is expected to show the lowest extraction and highest institutional stability, having become the dominant enforcing structure; unitarian_reading and modalist_reading both carry higher extraction as minority positions subject to exclusion by the dominant reading's institutions, though for different structural reasons (unitarian: denial of Christ's full deity draws Trinitarian and modalist censure alike; modalist: denial of simultaneous persons draws Trinitarian censure while its Christocentric piety draws no unitarian sympathy either).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
