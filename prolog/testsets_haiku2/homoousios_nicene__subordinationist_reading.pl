% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__subordinationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__subordinationist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: homoousios_nicene__subordinationist_reading
 *   human_readable: Homoousios compatible with subordination—shared divinity without ontological equality
 *   domain: theological/ecclesiastical/philosophical
 *
 * SUMMARY:
 *   The subordinationist reading of homoousios interprets the Nicene formula
 *   (homoousios tō patri—of one substance with the Father) as compatible with
 *   the Son's ontological or functional subordination to the Father. This
 *   reading preserves both the Council's linguistic framework and the
 *   legitimacy of New Testament subordination-language (John 5:19, 14:28,
 *   etc.). It sits in tension with the metaphysical-equality reading (which
 *   interprets homoousios as requiring co-equality) and the
 *   honorific-similarity reading (which interprets homoousios as mere
 *   likeness-by-name). This constraint story models the subordinationist
 *   reading as ONE instantiation of the contested kernel homoousios_nicene.
 *   The constraint's operation is the enforcement machinery that maintains
 *   the boundary between what homoousios permits and what it forecloses; from
 *   the subordinationist perspective, that boundary is artificially
 *   restrictive (extracted authority from subordinationist communities and
 *   scriptural priority). From the metaphysical-equality perspective, the
 *   subordinationist reading itself IS the threat that the enforcement
 *   machinery must suppress.
 *
 * KEY AGENTS:
 *   - subordinationist_theological_communities: Arian, Semi-Arian, later Protestant, and Nestorian traditions whose theological identity depends on permitting the Son's subordination. They benefit from the ambiguity of homoousios but bear the cost of conciliar policing.
 *   - metaphysical_equality_proponents: Athanasian and Cappadocian traditions committed to co-equality. They bear the cost of having to repeatedly argue that homoousios REQUIRES equality, not merely permits it.
 *   - conciliar_tradition (institutional agenda-setter): The conciliar succession (Nicaea, Constantinople I, later councils) that claims interpretive monopoly over homoousios. They enforce the boundary but also bear costs when the boundary proves ambiguous.
 *   - scriptural_authority_tradition (vindicated proposition): The claim that New Testament subordination-language remains theologically authoritative. The subordinationist reading vindicates this proposition.
 *   - philosophical_substance_framework (observer): The Aristotelian-Platonic metaphysical vocabulary that makes both readings possible and intelligible.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, 0.62).
domain_priors:suppression_score(homoousios_nicene__subordinationist_reading, 0.71).
domain_priors:theater_ratio(homoousios_nicene__subordinationist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, accessibility_collapse, 0.69).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__subordinationist_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__subordinationist_reading, "Homoousios compatible with subordination—shared divinity without ontological equality").
narrative_ontology:topic_domain(homoousios_nicene__subordinationist_reading, "theological/ecclesiastical/philosophical").

domain_priors:requires_active_enforcement(homoousios_nicene__subordinationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__subordinationist_reading, 'ad091e63-99d0-4f2a-8046-8ff2eab79d53').
narrative_ontology:cs_kernel_codification('ad091e63-99d0-4f2a-8046-8ff2eab79d53', fixed_text).
narrative_ontology:cs_authority_grounding('ad091e63-99d0-4f2a-8046-8ff2eab79d53', lineage).
narrative_ontology:cs_interpretation_layer_present('ad091e63-99d0-4f2a-8046-8ff2eab79d53').
narrative_ontology:cs_reading_relation('ad091e63-99d0-4f2a-8046-8ff2eab79d53', homoousios_nicene__metaphysical_equality_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad091e63-99d0-4f2a-8046-8ff2eab79d53', homoousios_nicene__honorific_similarity_reading, influences).
narrative_ontology:cs_axiom('ad091e63-99d0-4f2a-8046-8ff2eab79d53', foundational, scriptural_subordination_theologically_primary).
narrative_ontology:cs_axiom_status(scriptural_subordination_theologically_primary, holdable).
narrative_ontology:cs_axiom_grounding('ad091e63-99d0-4f2a-8046-8ff2eab79d53', scriptural_subordination_theologically_primary, empirically_contingent).
narrative_ontology:cs_axiom('ad091e63-99d0-4f2a-8046-8ff2eab79d53', foundational, ontological_equality_not_necessary_homoousios).
narrative_ontology:cs_axiom_status(ontological_equality_not_necessary_homoousios, holdable).
narrative_ontology:cs_axiom_grounding('ad091e63-99d0-4f2a-8046-8ff2eab79d53', ontological_equality_not_necessary_homoousios, deontological).
narrative_ontology:cs_reference_frame('ad091e63-99d0-4f2a-8046-8ff2eab79d53', scriptural_hermeneutics_priority).
narrative_ontology:cs_drift_state('ad091e63-99d0-4f2a-8046-8ff2eab79d53', post_constantinople_i_orthodoxy_establishment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ad091e63-99d0-4f2a-8046-8ff2eab79d53', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(homoousios_nicene__subordinationist_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, subordinationist_theological_communities).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, scriptural_literalist_interpreters).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, nicene_conciliar_authority).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, metaphysical_equality_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, reformation_protestant_communities).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, conciliar_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theological lineages and communities (Arian descendants, Semi-Arian remnants, some Nestorian branches, later Protestant subordinationists) whose doctrinal identity depends on the permissibility of ontological or functional subordination of the Son. They benefit from a reading that permits homoousios while preserving subordination-language, avoiding the need to either repudiate Nicaea or abandon their scriptural hermeneutics.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, subordinationist_theological_communities, beneficiary,
    organized, generational, identity_locked, continental).

% Theologians and interpretive communities emphasizing the plain sense of scriptural subordination-language (John 5:19, 14:28; 1 Cor 11:3) as theologically authoritative. They benefit from a reading that honors both homoousios AND the biblical priority of subordination-texts, avoiding the subordination-vs.-homoousios dilemma that forces a choice between conciliar tradition and scriptural reading.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, scriptural_literalist_interpreters, beneficiary,
    moderate, biographical, constrained, global).

% The conciliar succession from Nicaea onward that claims interpretive authority over homoousios. It bears costs through the need to continuously enforce the boundary between permitted and forbidden readings, to suppress the natural ambiguity of the term, and to perform confidence that homoousios unambiguously entails equality. The conciliar tradition could change the interpretation (mobile exit), but chooses enforcement instead, marking itself as both agenda-setter and partially-payer of the constraint.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, conciliar_tradition, payer,
    institutional, civilizational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, conciliar_tradition, agenda_setter).

% The Athanasian and Cappadocian synthesis and its successors (Eastern Orthodox, Catholic, mainstream Protestant theology) that interprets homoousios as requiring full ontological co-equality of Father and Son. They bear costs through the need to defend the interpretation against the natural reading of subordination-language in scripture, to argue why conciliar metaphysics overrules biblical anthropomorphism, and to maintain the boundary against subordinationist re-readings.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, metaphysical_equality_proponents, payer,
    institutional, generational, constrained, continental).

% Eastern Christian (Oriental Orthodox, Eastern Orthodox, Byzantine-rite) liturgical and theological traditions that prioritize theosis (deification/union with God) and the essence/energies distinction over substantialist metaphysics. They would argue that the entire equality-vs.-subordination binary presupposes a philosophical framework (substance-metaphysics) that is itself contestable and that a more apophatic theology would bracket the question. They are excluded because the constraint presupposes substance-talk as the proper idiom.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, eastern_liturgical_communities, excluded,
    organized, generational, identity_locked, regional).

% Protestant reformers and reformed theological lineages that use the subordinationist reading as evidence that conciliar authority has over-determined what scripture permits. They benefit from demonstrating that the conciliar equality-interpretation is not an inevitable reading of homoousios but a choice—one that privileges medieval scholasticism and Hellenistic metaphysics over biblical language.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, reformation_protestant_communities, beneficiary,
    organized, generational, constrained, continental).

% The Aristotelian-Platonic metaphysical vocabulary (ousia, hypostasis, substantia, etc.) on which homoousios rides. It is the framework that makes the constraint intelligible and enforceable. The constraint's persistence depends on substance-talk remaining the authoritative register for theological precision. This is an observer because it does not collect or pay; it enables the constraint's structure.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, philosophical_substance_framework, observer,
    powerful, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(homoousios_nicene__subordinationist_reading, philosophical_substance_framework).

% The normative claim that scripture (especially the Gospels and Pauline letters) remains the primary court of appeal in Christian theology, not conciliar pronouncements or philosophical abstraction. The subordinationist reading vindicates this proposition by demonstrating that homoousios can be read as compatible with scriptural subordination-language and that scriptural authority need not be overruled by conciliar metaphysics.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, scriptural_authority_proposition, beneficiary,
    powerful, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(homoousios_nicene__subordinationist_reading, scriptural_authority_proposition).

% The normative claim that conciliar definition carries binding interpretive force and that homoousios, once conciliarly established, has a determinate meaning that forecloses alternative readings. This proposition is victimized by the subordinationist reading because the reading demonstrates that the formula does not on its face foreclose subordination; the proposition must be continuously supplemented by tradition and hermeneutical authority to stick.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, conciliar_authority_proposition, payer,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(homoousios_nicene__subordinationist_reading, conciliar_authority_proposition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__subordinationist_reading, conciliar_tradition).
narrative_ontology:fixing_cost_class(homoousios_nicene__subordinationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the Nicene linguistic framework while allowing scriptural authority and subordination-language to remain theologically legitimate. Coordinates a theological middle ground: the Son is fully divine (homoousios), not a creature as Arius claimed, but remains subordinate to the Father in being or function, as much New Testament language suggests. This mediates between communities whose identity depends on affirming both Nicene conciliarity AND the theological permissibility of scriptural subordination-language.
% TRANSFER_FUNCTION: Transfers interpretive authority from subordinationist communities (who lose the right to claim homoousios requires or permits subordination without qualification) and from scriptural literalism (which must defer to conciliar-metaphysical precision) to conciliar tradition and metaphysical-equality proponents (who gain monopoly over homoousios meaning). The reading also transfers dignity from philosophical substance-talk (which is vindicated as the proper register) away from apophatic or liturgical-eschatological frameworks that would bracket the question.
% ABSENT_VOICES: Eastern liturgical theology (theosis-centered, essence/energies framework) is excluded because the constraint presupposes that substance-language and the equality-vs.-subordination binary are the right register for Christology. Radical anti-metaphysical movements (medieval nominalists, later modern skeptics of essence-talk) are excluded because they would reject the entire framework the constraint rides on. Pagan philosophical critics are excluded because they attack the philosophical vocabulary itself rather than participating in the hermeneutical dispute over what the vocabulary entails. Unitarian and purely humanitarian Christologies are excluded because they deny homoousios altogether.
% DISAPPEARANCE_RATIONALE: If the subordinationist reading disappeared—if homoousios were authoritatively interpreted to require full metaphysical equality with no permissible subordination whatsoever—the theological landscape would reorganize: Subordinationist communities would either exit conciliar Christianity entirely (formal schism, as some did historically), capitulate to the equality reading despite their preferences, or fragment into smaller sects maintaining the tradition outside Christendom. Scriptural authority would formally surrender to conciliar-metaphysical authority in Christology. The hermeneutical permission to read subordination-language as theologically primary would close. Reformation Protestant identity-markers (rejecting over-determination by conciliar metaphysics) would weaken. The mediating space this reading holds open would collapse.
% FOUNDING_PROBLEM: The Council of Nicaea (325 CE) adopted homoousios to combat Arianism and establish that the Son shares the Father's divine substance, not that the Son is a creature. But the formula's metaphysical entailments were contested immediately: How can homoousios accommodate the New Testament's repeated emphasis on the Son's subordination to the Father (John 5:19, 14:28, 1 Cor 11:3, Heb 1:3-4)? Can the Son be homoousios (of one substance) while remaining subordinate in being or function? The founding problem: can homoousios be interpreted as compatible with scriptural subordination-language, or does it necessarily require metaphysical co-equality?
% FOUNDING_PROBLEM_CORROBORATION: Subordinationist communities from the 4th century onward attested that homoousios did not settle the question against them—they remained convinced it was compatible with subordination. Athanasius and later conciliar champions attested the same but interpreted the implication as requiring metaphysical equality. Later councils (Constantinople I 381, Ephesus 431, Chalcedon 451) issued clarifications that are read by equality-proponents as settling the matter and by subordinationists as illegitimate impositions. Modern historical scholarship (including secular historians of Christianity) documents that homoousios was genuinely ambiguous at Nicaea and that subordinationist interpretations persisted for centuries within Christian communities—a corroboration from outside the benefiting parties (modern scholars without doctrinal investment) that the founding problem remained intellectually live for the historical agents who held it.
narrative_ontology:disappearance_verdict(homoousios_nicene__subordinationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__subordinationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__subordinationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_nicene__subordinationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__subordinationist_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__subordinationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__subordinationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.62 (end of interval) because the constraint operates as a transfer of interpretive authority: from subordinationist communities (who lose the right to claim homoousios requires subordination) and from scriptural literal-ism (which must defer to metaphysical abstraction). Suppression is higher (0.71) because the constraint's persistence requires actively excluding subordinationist interpretations—not through external force but through hermeneutical authority-claims and doctrinal supplementation. Alternatives (leaving homoousios-language for pure Arianism, or inverting the hierarchy to require subordination) are accessible but carry costs (schism, loss of conciliar legitimacy). Theater is substantial (0.48) because both sides invest in interpretive performance: the equality-proponents must argue that homoousios OBVIOUSLY entails equality (suppressing the text's natural ambiguity), while subordinationists must argue they honor the formula while retaining subordination-language (a performative high-wire act). The measurement series shows extractiveness and suppression rising over the interval: from time 0 (immediately post-Nicaea, ambiguity still acknowledged) through time 12 (high medieval/Reformation period), the conciliar tradition increasingly forecloses subordinationist readings and performs confidence in the formula's metaphysical univocity. Theater also rises as the performance becomes more elaborate (more sophisticated theological arguments, more careful boundary-maintenance).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (subordinationist communities, scriptural literalists) compute dramatically different classifications than the beneficiary seats (metaphysical-equality proponents, conciliar tradition). From the payer perspective, this is a Snare: the founding problem (integrating homoousios with subordination-language) is formally ruled out, yet the reading persists, making it a zombie constraint maintained only by institutional power and the identity-fusion of subordinationist communities. From the beneficiary perspective, this is a Rope: it coordinates genuine theological coherence (insisting that the Son is fully God in substance, not a creature), with the enforcement merely clarifying what the formula means. The engine's per-seat computation will show this divergence; the authored metrics are intermediate (Tangled Rope), reflecting that both readings have structural validity and active enforcement is genuinely required on both sides.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinationist communities are structural targets: they depend on scriptural authority and have identity-fusion with subordination theology. Their exit is costly (leaving Christendom, or capitulating to the equality reading). Effective extraction runs toward them because they have limited alternatives and high commitment costs. Metaphysical-equality proponents are ambiguously positioned—they are beneficiaries of the constraint's enforcement (their reading gets conciliar validation) but also payers (they must continuously defend why homoousios REQUIRES equality and suppress the text's natural equivocation). They have powerful institutional backing (conciliar authority) so their effective extraction is damped. Conciliar tradition itself is the agenda-setter with mobile exit (they set the rules and could change them), so they compute as low-directionality beneficiaries or near-symmetric (they incur enforcement costs but control the frame). Scriptural-authority-proposition is a non-agent vindicated by the reading (beneficiaries array lists it because the reading vindicates scriptural priority), but it collects no rents and feeds no extraction metric. The directionality structure makes this a Tangled Rope: there is a genuine coordination function (preserving both conciliar language and scriptural authority), but the coordination is asymmetric (subordinationists are constrained while beneficiaries are empowered), active enforcement is required (hermeneutical policing), and beneficiaries and payers are named and structurally distinct.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is the erosion of a constraint's founding justification while the constraint persists. The subordinationist reading sits at the intersection of three mandatrophy threats: (1) Historical refutation: once Constantinople I, Ephesus, and Chalcedon clarified that homoousios entails metaphysical co-equality with no permissible subordination, the founding problem (how to read homoousios compatibly with subordination) became formally rejected—but subordinationist communities persisted anyway, treating the conciliar clarifications as illegitimate impositions. (2) Empirical erosion: modern scriptural scholarship (even among conservative scholars) shows that subordination-language in John and Paul reflects christological development and does not constitute univocal doctrine—this undermines the scriptural-authority justification the reading depends on. (3) Philosophical obsolescence: the substance metaphysics homoousios rides on has largely collapsed in modern theology; post-Harnack Protestantism and modern Orthodoxy increasingly bracket the essence/substance framework. The constraint persists by theatrical maintenance: subordinationist readings persist as historically-acknowledged positions that are treated as formally refuted but not actually foreclosed (they remain defensible for those willing to bear the schism cost). This is the piton signature—the reading is mostly performatively maintained rather than actively enforced, because conciliar authority no longer needs to suppress it (modern Christianity has secularized or de-emphasized Christological precision). However, the reading's extractiveness and suppression scores remain substantial (0.62 and 0.71 respectively) because in conservative Protestant and Eastern Orthodox communities that still operate within the substance-metaphysics frame, the constraint is very much active. The mandatrophy analysis therefore does not fully resolve: the constraint exhibits piton signatures in mainstream theology but remains functionally Tangled Rope in conservative-traditional communities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    homoousios_ambiguity_kernel,
    'Does the Nicene formula homoousios necessarily entail metaphysical co-equality, or is it genuinely ambiguous with respect to subordination?',
    'Philological analysis of homoousios in pre-Nicene sources; examination of the Council''s own documentation and intent; comparison with how post-Nicene councils (Constantinople I 381, Ephesus 431, Chalcedon 451) glossed or clarified the term; modern historical-critical consensus on the formula''s original range of interpretations.',
    'If homoousios is shown to permit subordination philologically, this reading''s coherence is secured at the historical level and conciliar authority is limited to linguistic adoption. If post-Nicene clarifications are deemed authoritative, this reading is overridden by the later conciliar development. If modern scholarship shows genuine ambiguity persisted for centuries, the reading''s historical resilience is explained and the suppression becomes an active hermeneutical choice, not a necessary clarification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(homoousios_ambiguity_kernel, empirical, 'Does homoousios logically entail or merely permit metaphysical equality?').

omega_variable(
    subordination_readings_collapsed_or_live,
    'In contemporary Christian theology, does the subordinationist reading remain a live, intellectually defensible option within Nicene Christianity, or has it been effectively foreclosed and relegated to historical artifact?',
    'Survey of contemporary theologians (Catholic, Orthodox, Protestant, evangelical) who affirm homoousios: how many defend some form of subordination-compatibility, how many argue it is incoherent, how many treat it as historically-acknowledged but invalid? Examination of whether the reading is defended in peer-reviewed theology vs. only in historical surveys. Analysis of whether the reading requires rejecting post-Nicene conciliar authority (thus genuine schism) or can be maintained within orthodoxy.',
    'If live and coherent, the constraint remains active and classifies as Tangled Rope (asymmetric enforcement of boundary). If effectively foreclosed, it transitions to Piton (theatrical maintenance of defeated option, extracted authority used to suppress a reading that is no longer genuinely alive). This omega determines the endgame classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subordination_readings_collapsed_or_live, empirical, 'Whether subordinationist readings of homoousios remain live theological options or are historical artifacts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__subordinationist_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_nicene__subordinationist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(homo_tr_t0, projected).
narrative_ontology:measurement(homo_tr_t3, homoousios_nicene__subordinationist_reading, theater_ratio, 3, 0.4).
narrative_ontology:measurement_basis(homo_tr_t3, observed).
narrative_ontology:measurement(homo_tr_t6, homoousios_nicene__subordinationist_reading, theater_ratio, 6, 0.43).
narrative_ontology:measurement_basis(homo_tr_t6, observed).
narrative_ontology:measurement(homo_tr_t9, homoousios_nicene__subordinationist_reading, theater_ratio, 9, 0.46).
narrative_ontology:measurement_basis(homo_tr_t9, observed).
narrative_ontology:measurement(homo_tr_t12, homoousios_nicene__subordinationist_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement_basis(homo_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_nicene__subordinationist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(homo_be_t0, projected).
narrative_ontology:measurement(homo_be_t3, homoousios_nicene__subordinationist_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement_basis(homo_be_t3, observed).
narrative_ontology:measurement(homo_be_t6, homoousios_nicene__subordinationist_reading, base_extractiveness, 6, 0.57).
narrative_ontology:measurement_basis(homo_be_t6, observed).
narrative_ontology:measurement(homo_be_t9, homoousios_nicene__subordinationist_reading, base_extractiveness, 9, 0.61).
narrative_ontology:measurement_basis(homo_be_t9, observed).
narrative_ontology:measurement(homo_be_t12, homoousios_nicene__subordinationist_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement_basis(homo_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_nicene__subordinationist_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(homo_su_t0, projected).
narrative_ontology:measurement(homo_su_t3, homoousios_nicene__subordinationist_reading, suppression_requirement, 3, 0.59).
narrative_ontology:measurement_basis(homo_su_t3, observed).
narrative_ontology:measurement(homo_su_t6, homoousios_nicene__subordinationist_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement_basis(homo_su_t6, observed).
narrative_ontology:measurement(homo_su_t9, homoousios_nicene__subordinationist_reading, suppression_requirement, 9, 0.69).
narrative_ontology:measurement_basis(homo_su_t9, observed).
narrative_ontology:measurement(homo_su_t12, homoousios_nicene__subordinationist_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement_basis(homo_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__subordinationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__subordinationist_reading, 0.12).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% The homoousios_nicene kernel decomposes into three constraint stories, each a reading of the same contested formula. (1) subordinationist_reading: homoousios permits subordination; low ε for conciliar enforcement, moderate extraction from subordinationist communities. (2) metaphysical_equality_reading: homoousios requires co-equality; higher ε for hermeneutical enforcement, extraction from subordinationist communities, low extraction overall because the reading is conciliarly dominant. (3) honorific_similarity_reading: homoousios signifies similarity-by-name, not strict identity; medium ε for defending the term against both full equality and full subordination. Each reading has its own ε (the standing arrangement under that reading's interpretation) and its own beneficiary/victim structure. The network edges capture influence: subordinationist reading influences equality reading (if subordination remains permissible, equality must be defended explicitly); equality reading influences subordinationist reading (if equality is conciliarly mandatory, subordination becomes an excluded option). The kernel is the shared textual artifact (homoousios); the readings are the distinct constraints that compete for interpretive authority over it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_nicene__subordinationist_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
