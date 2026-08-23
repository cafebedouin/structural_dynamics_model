% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__trinitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__trinitarian_reading, []).

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
 *   constraint_id: biblical_divine_nature__trinitarian_reading
 *   human_readable: Trinitarian Orthodoxy Boundary (Homoousion Enforcement)
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   Since the Council of Nicaea (325) and Constantinople (381), the
 *   confession that the Father, Son, and Holy Spirit are three hypostases
 *   sharing one ousia has functioned as the boundary of Christian orthodoxy:
 *   first under conciliar anathema backed by imperial law, then under
 *   confessional subscription, and today under ecumenical membership rules
 *   and ministerial credentialing. The arrangement solves a real coordination
 *   problem — how monotheists can worship Christ and invoke the Spirit
 *   without collapsing into polytheism or denying his divinity — and it
 *   simultaneously extracts from those who read the sources differently:
 *   Arian communities were condemned, exiled, and extinguished; unitarian
 *   Christians faced execution and expulsion; Oneness Pentecostals are
 *   excluded from ecumenical bodies today. This file instantiates ONE reading
 *   of the kernel biblical_divine_nature — the trinitarian reading — as a
 *   clean, epsilon-invariant constraint; the unitarian and modalist readings
 *   are separate stories with their own victim sets and enforcement
 *   economics. Epsilon's referent is the standing enforcement arrangement
 *   itself, not any rival arrangement this reading would prefer.
 *
 * KEY AGENTS:
 *   - episcopal_hierarchy: agenda-setter (institutional/arbitrage) — convenes councils, defines the required confession, administers the boundary, collects guardianship authority
 *   - roman_imperial_authority: historical co-agenda-setter (institutional/arbitrage) — supplied the enforcement arm from Constantine through Justinian; defunct but structurally founding
 *   - credal_denominations: beneficiary (organized/constrained) — collect mutual recognition and ecumenical standing, pay internal discipline costs
 *   - ordinary_worshippers: beneficiary with payer costs (moderate/identity_locked) — receive belonging, carry assent and exit costs, voiceless on the formula itself
 *   - arian_communities: primary historical target (organized/trapped) — condemned, exiled, extinguished as an organized alternative
 *   - unitarian_christians: target (moderate/constrained) — from Servetus's execution to modern ecumenical exclusion
 *   - oneness_pentecostals: target (organized/constrained) — excluded from ecumenical bodies, baptisms unrecognized
 *   - historians_of_doctrine: analytical observer — document the record all seats argue from
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, 0.6).
domain_priors:suppression_score(biblical_divine_nature__trinitarian_reading, 0.26).
domain_priors:theater_ratio(biblical_divine_nature__trinitarian_reading, 0.66).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, suppression_requirement, 0.26).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, theater_ratio, 0.66).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__trinitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__trinitarian_reading, "Trinitarian Orthodoxy Boundary (Homoousion Enforcement)").
narrative_ontology:topic_domain(biblical_divine_nature__trinitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__trinitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__trinitarian_reading, 'f228db21-bbd6-4de8-b46f-4f1ad2a2d21f').
narrative_ontology:cs_kernel_codification('f228db21-bbd6-4de8-b46f-4f1ad2a2d21f', fixed_text).
narrative_ontology:cs_authority_grounding('f228db21-bbd6-4de8-b46f-4f1ad2a2d21f', lineage).
narrative_ontology:cs_interpretation_layer_present('f228db21-bbd6-4de8-b46f-4f1ad2a2d21f').
narrative_ontology:cs_reading_relation('f228db21-bbd6-4de8-b46f-4f1ad2a2d21f', biblical_divine_nature__unitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('f228db21-bbd6-4de8-b46f-4f1ad2a2d21f', biblical_divine_nature__modalist_reading, forecloses).
narrative_ontology:cs_axiom('f228db21-bbd6-4de8-b46f-4f1ad2a2d21f', foundational, son_consubstantial_with_father).
narrative_ontology:cs_axiom_status(son_consubstantial_with_father, holdable).
narrative_ontology:cs_axiom_grounding('f228db21-bbd6-4de8-b46f-4f1ad2a2d21f', son_consubstantial_with_father, theological).
narrative_ontology:cs_axiom('f228db21-bbd6-4de8-b46f-4f1ad2a2d21f', secondary, homoousion_denial_anathematized).
narrative_ontology:cs_axiom_status(homoousion_denial_anathematized, holdable).
narrative_ontology:cs_axiom_grounding('f228db21-bbd6-4de8-b46f-4f1ad2a2d21f', homoousion_denial_anathematized, conventional).
narrative_ontology:cs_reference_frame('f228db21-bbd6-4de8-b46f-4f1ad2a2d21f', nicene_constantinopolitan_settlement).
narrative_ontology:cs_drift_state('f228db21-bbd6-4de8-b46f-4f1ad2a2d21f', contemporary_ecumenical_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f228db21-bbd6-4de8-b46f-4f1ad2a2d21f', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__trinitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, credal_denominations).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, arian_communities).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, unitarian_christians).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, oneness_pentecostals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, roman_imperial_authority).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, ordinary_worshippers).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, ordinary_worshippers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes councils, promulgates creeds, and defines the confession required for ordination, communion, and ecumenical recognition. Administers the boundary between recognized and unrecognized teaching and collects the institutional cohesion, deference, and guardianship authority that flow from being keeper of the shared confession. Holds the same conciliar machinery that established the requirement, so it can in principle revise what it requires.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, episcopal_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).

% Historical co-author of the arrangement: from Constantine through Justinian it convened councils, legalized the Nicene confession, and exiled or executed dissenting teachers, collecting religious legitimation of rule and internal unity from a single enforced confession. Its enforcement arm dissolved with the empire; it is seated here because the arrangement's founding enforcement was imperial and its legacy shaped every later enforcement structure.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, roman_imperial_authority, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__trinitarian_reading, roman_imperial_authority, beneficiary).

% Confess the shared formula in liturgy and constitutional documents and receive mutual recognition, intercommunion, and ecumenical standing from conformity. Bear the ongoing costs of maintaining doctrinal statements, credentialing clergy, and disciplining outliers within their own ranks. Their institutional identity has grown fused with the confession they inherited.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, credal_denominations, beneficiary,
    organized, generational, constrained, global).

% Recite the creed weekly and receive belonging, shared identity, and liturgical continuity. Most hold the underlying metaphysics loosely and do not participate in defining the formula they confess. Leaving a confessional community costs family ties, community, and identity even where no rule formally forbids departure.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, ordinary_worshippers, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__trinitarian_reading, ordinary_worshippers, payer).

% Fourth-century teachers and communities confessing the Son as the first-created being rather than coessential with the Father. Condemned at Nicaea, exiled under imperial law, their writings systematically destroyed. Germanic kingdoms adopted their confession and were ultimately conquered or absorbed, ending the movement as an organized alternative. Exit meant conversion, exile, or silence.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, arian_communities, payer,
    organized, generational, trapped, continental).

% Christians affirming the Father alone as fully God. From Michael Servetus (executed in Geneva, 1553) through the Polish Brethren (expelled from Poland, 1658) to modern denominations, they have faced capital punishment, expulsion, and today exclusion from Trinitarian ecumenical bodies, non-recognition of ministry, and barriers to teaching posts in confessional institutions.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, unitarian_christians, payer,
    moderate, biographical, constrained, global).

% Twentieth-century movement baptizing in the name of Jesus alone and identifying Father, Son, and Spirit as manifestations of one person. Excluded from the World Council of Churches and most evangelical alliances; their baptisms are frequently not recognized by Trinitarian bodies. They sustain large congregations and their own institutions despite the exclusion.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, oneness_pentecostals, payer,
    organized, biographical, constrained, global).

% Academic scholars reconstructing the conciliar controversies, comparing the readings of the source texts, and documenting the human costs of enforcement across the centuries. Take no side in the confession itself but supply the record from which the other seats argue.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, historians_of_doctrine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__trinitarian_reading, episcopal_hierarchy).
narrative_ontology:fixing_cost_class(biblical_divine_nature__trinitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared metaphysical grammar that lets monotheists worship Christ and invoke the Spirit without collapsing into polytheism or denying his divinity; standardizes baptismal confession and liturgy across languages and cultures; and enables mutual recognition between churches that confess the same formula.
% TRANSFER_FUNCTION: Moves doctrinal assent and institutional standing from non-Trinitarian Christians to the creedal hierarchy: exclusion from office, communion, and ecumenical recognition flows outward from dissenters, while authority, deference, and boundary-control accrue to the councils and episcopate that administer the requirement.
% ABSENT_VOICES: Non-Trinitarian Christians are the structurally absent voices: Arians were silenced by exile and book destruction before they could answer at scale; unitarian Christians answered from the margins or the scaffold; Oneness Pentecostals stand outside the ecumenical tables where the basis of membership is set. Ordinary worshippers are present in body but absent in voice — the formula is recited over them, not negotiated with them.
% DISAPPEARANCE_RATIONALE: If the enforced boundary vanished overnight, the ecumenical architecture built on the Trinitarian basis (WCC membership conditions, bilateral dialogues, mutual baptismal recognition) would reorganize, seminary curricula and credentialing standards would shift, and the confessional identity of hundreds of millions of worshippers would lose its organizing grammar. The underlying theological debate would continue, but the institutional arrangement and its exclusion machinery would not survive the night.
% FOUNDING_PROBLEM: How can Christians who worship Jesus and baptize in the name of Father, Son, and Spirit remain monotheists? The fourth-century church needed a grammar that preserved both the worship practice it already had and the monotheistic inheritance it would not surrender — and, after Constantine, a single confession capable of holding an empire's church together.
% FOUNDING_PROBLEM_CORROBORATION: Jewish and Muslim interlocutors press the monotheism question from entirely outside the Christian beneficiary set; unitarian and modalist theologians accept the underlying problem (worshipping Christ while remaining monotheists) while disputing this solution; academic patristics scholarship corroborates the fourth-century problem-context of liturgical practice outrunning doctrine and post-Constantinian unity needs. No corroborating source attests that this particular formula is the uniquely necessary solution — the corroboration covers the problem, not the reading's monopoly on answering it.
narrative_ontology:disappearance_verdict(biblical_divine_nature__trinitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__trinitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__trinitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_divine_nature__trinitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__trinitarian_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__trinitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__trinitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.60 at interval end) is substantial but no longer extreme: the transfer today runs through exclusion from recognition — ecumenical membership, ministerial standing, baptismal acceptance — rather than the exile and capital enforcement that peaked around 1553. Suppression (0.26) is the residual enforcement capacity: anathemas remain formally on the books in several traditions but are rarely executed. Theater (0.66) has crossed the Goodhart threshold: across much of global Christianity the creed is recited weekly with little metaphysical comprehension and less disciplinary consequence, so a growing share of activity maintains the form rather than the function. Accessibility_collapse (0.48) is moderate: unitarian and oneness alternatives were never fully eliminated and persist at the margins. Resistance (0.68) is high: the Arian controversy convulsed the fourth-century church, antitrinitarianism repeatedly regrew after suppression, and dissenting movements sustain institutions today. One structural fact stabilizes the arrangement: the victim groups are themselves mutually opposed readings — unitarians deny what oneness teachers affirm about the Son — so no durable payer coalition has ever formed across the victim set. The three measurement series share one time grid; the suppression_requirement series is authored deliberately because this story specifically tracks enforcement-capacity build-up (imperial arm, medieval heresy machinery) and decay (disestablishment, voluntary-association exclusion), not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is guardianship: the hierarchy experiences anathema as protection of the flock and the creed as the church's self-constitution. From the payer seats the same machinery is a door closed in front of them — exile for Arius, fire for Servetus, a rejected membership application for a Oneness denomination. Ordinary worshippers occupy a third position: they inherit the formula as belonging, recite it without engaging its metaphysics, and would find exit costly in family and community terms even where no rule forbids it. The credal denominations sit between: beneficiaries of mutual recognition, payers of the discipline costs of policing their own outliers. Institutional identity fusion is load-bearing here — the denominations have substantially become their confession, so the constraint persists partly because its holders cannot imagine themselves without it. The engine computes these per-seat classifications from the structural data; the divergence between guardianship and exclusion is the measurement, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   The episcopal hierarchy sits nearest the beneficiary end: it writes and administers the required confession, and its arbitrage-grade exit (it can reconvene councils and revise what it requires) means the constraint subsidizes it. Credal denominations derive low directionality from their beneficiary declaration, tempered by their own discipline costs. The victim groups sit near the target end: Arian communities were trapped (conversion, exile, or silence — no third option once imperial law bound the confession), which pushes them toward the full-target end; unitarian Christians and Oneness Pentecostals are constrained today (exit available but costly in identity and community terms). Ordinary worshippers sit near symmetric: belonging received, assent and exit costs paid. Global spatial scope modestly amplifies effective extraction on the target seats because verification of doctrinal conformity at planetary scale favors institutional gatekeeping over individual conscience. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how worshippers of Christ remain monotheists — is live wherever Christianity exists and is corroborated from outside the beneficiary set, so this is not a resolved-mandatrophy case and no zombie flag is warranted. But the constraint is internally bifurcated: the doctrinal-grammar limb remains functional, while the enforcement limb (anathema, exclusion) is progressively theatrical in most jurisdictions. The tangled-rope classification prevents mislabeling in both directions: a pure-extraction reading would erase the real coordination (a shared grammar that lets thousands of languages and cultures confess one faith and recognize each other's baptism), and a pure-coordination reading would erase the identifiable people burned, exiled, and excluded through the same structure. If the founding problem ever dies — a post-metaphysical age abandoning the question while recitation continues — the remaining form is piton-shaped, which the theater_ratio series is positioned to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kernel biblical_divine_nature — how would instantiating the unitarian_reading or modalist_reading instead change the structural data?',
    'Generate the sibling stories and compare victim sets, enforcement economics, and epsilon: the unitarian reading relocates the enforcement burden (Trinitarians become the dissenters in any polity organized around numerical singularity), and the modalist reading changes the coherence profile of the person grammar entirely.',
    'The victim set, directionality structure, and effective extraction are indexical to the reading chosen; cross-reading comparison is the only way to distinguish properties of the kernel from properties of this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: all structural data in this story is relative to the trinitarian reading of the shared kernel.').

omega_variable(
    hypostatic_distinction_coherence,
    'Is the ousia/hypostasis distinction a coherent metaphysical solution to the monotheism-plus-worship problem, or a semantic screen that defers the contradiction without resolving it?',
    'The analytic-theology program: test social-trinitarian, relative-identity, and Latin vs Greek models for whether any preserves monotheism without equivocation on ''God''; watch for convergence or persistent stalemate in the philosophy-of-religion literature.',
    'If the distinction is a screen, the coordination function degrades toward a boundary-shibboleth, excess extraction rises, and the classification drifts toward snare; if coherent, part of the measured extraction is the genuine price of the coordination the formula performs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypostatic_distinction_coherence, conceptual, 'Whether the essence/person grammar is real metaphysics or enforcement-compatible ambiguity.').

omega_variable(
    enforcement_revival_vs_decay,
    'Will the soft exclusion regime continue decaying toward purely liturgical recitation, or revive under confessionalist movements reasserting disciplinary boundaries?',
    'Track credentialing standards, ecumenical membership rules, and denominational discipline cases over coming decades; count institutions that remove versus reinstate the Trinitarian requirement.',
    'Continued decay pushes theater_ratio past the functional threshold and makes the remnant a piton candidate; revival raises suppression_requirement and increases snare pressure on the victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_revival_vs_decay, empirical, 'Trajectory of the enforcement limb: decay toward inertial recitation or revival of discipline.').

omega_variable(
    coordination_thinness_separability,
    'Could the monotheism-plus-worship coordination be served by thinner shared formulas (for example, a kerygmatic ''Jesus is Lord'' with eschatological deferral of metaphysics), making the full homoousion enforcement separable from the coordination need?',
    'Comparative study of non-creedal Christian movements that sustain worship-of-Jesus monotheism across cultures without consubstantiality enforcement; measure whether mutual recognition and liturgical coherence survive without the full formula.',
    'If separable, the enforcement component is extraction riding on a real coordination function; if inseparable, part of the measured extraction is constitutive of the coordination itself and the tangled-rope reading is strengthened against snare drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_thinness_separability, conceptual, 'Whether the metaphysical thickness of the requirement is separable from its coordinating work.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__trinitarian_reading, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bdn_trinitarian_tr_t325, biblical_divine_nature__trinitarian_reading, theater_ratio, 325, 0.12).
narrative_ontology:measurement(bdn_trinitarian_tr_t381, biblical_divine_nature__trinitarian_reading, theater_ratio, 381, 0.16).
narrative_ontology:measurement(bdn_trinitarian_tr_t553, biblical_divine_nature__trinitarian_reading, theater_ratio, 553, 0.22).
narrative_ontology:measurement(bdn_trinitarian_tr_t1215, biblical_divine_nature__trinitarian_reading, theater_ratio, 1215, 0.34).
narrative_ontology:measurement(bdn_trinitarian_tr_t1553, biblical_divine_nature__trinitarian_reading, theater_ratio, 1553, 0.4).
narrative_ontology:measurement(bdn_trinitarian_tr_t1787, biblical_divine_nature__trinitarian_reading, theater_ratio, 1787, 0.52).
narrative_ontology:measurement(bdn_trinitarian_tr_t1916, biblical_divine_nature__trinitarian_reading, theater_ratio, 1916, 0.56).
narrative_ontology:measurement(bdn_trinitarian_tr_t1948, biblical_divine_nature__trinitarian_reading, theater_ratio, 1948, 0.61).
narrative_ontology:measurement(bdn_trinitarian_tr_t2025, biblical_divine_nature__trinitarian_reading, theater_ratio, 2025, 0.66).

% Extraction over time
narrative_ontology:measurement(bdn_trinitarian_be_t325, biblical_divine_nature__trinitarian_reading, base_extractiveness, 325, 0.55).
narrative_ontology:measurement(bdn_trinitarian_be_t381, biblical_divine_nature__trinitarian_reading, base_extractiveness, 381, 0.62).
narrative_ontology:measurement(bdn_trinitarian_be_t553, biblical_divine_nature__trinitarian_reading, base_extractiveness, 553, 0.72).
narrative_ontology:measurement(bdn_trinitarian_be_t1215, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1215, 0.84).
narrative_ontology:measurement(bdn_trinitarian_be_t1553, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1553, 0.88).
narrative_ontology:measurement(bdn_trinitarian_be_t1787, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1787, 0.74).
narrative_ontology:measurement(bdn_trinitarian_be_t1916, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1916, 0.66).
narrative_ontology:measurement(bdn_trinitarian_be_t1948, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1948, 0.63).
narrative_ontology:measurement(bdn_trinitarian_be_t2025, biblical_divine_nature__trinitarian_reading, base_extractiveness, 2025, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(bdn_trinitarian_su_t325, biblical_divine_nature__trinitarian_reading, suppression_requirement, 325, 0.5).
narrative_ontology:measurement(bdn_trinitarian_su_t381, biblical_divine_nature__trinitarian_reading, suppression_requirement, 381, 0.66).
narrative_ontology:measurement(bdn_trinitarian_su_t553, biblical_divine_nature__trinitarian_reading, suppression_requirement, 553, 0.76).
narrative_ontology:measurement(bdn_trinitarian_su_t1215, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1215, 0.86).
narrative_ontology:measurement(bdn_trinitarian_su_t1553, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1553, 0.9).
narrative_ontology:measurement(bdn_trinitarian_su_t1787, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1787, 0.7).
narrative_ontology:measurement(bdn_trinitarian_su_t1916, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1916, 0.48).
narrative_ontology:measurement(bdn_trinitarian_su_t1948, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1948, 0.36).
narrative_ontology:measurement(bdn_trinitarian_su_t2025, biblical_divine_nature__trinitarian_reading, suppression_requirement, 2025, 0.26).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__trinitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, biblical_divine_nature__unitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, biblical_divine_nature__modalist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel biblical_divine_nature. The colloquial label 'the doctrine of the Trinity' conflates three structurally distinct enforceable claims: the trinitarian reading (this file), the unitarian reading, and the modalist reading. Each instantiates a different constraint with a different victim set, different enforcement economics, and its own stable epsilon; forcing them into one story would make epsilon observer-relative, violating epsilon-invariance. The fixed text (scripture) is the shared upstream under-determined source; each reading is a downstream stabilization of it. This file links both siblings via affects_constraints; the sibling files link back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
