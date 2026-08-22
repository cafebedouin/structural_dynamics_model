% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__orthodox_literalist, []).

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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Dharmasastra Varna Hierarchy: Orthodox Literalist Reading
 *   domain: religious/legal/normative
 *
 * SUMMARY:
 *   The orthodox literalist reading of Dharmasastra asserts that the varna
 *   hierarchy and its occupational prescriptions are eternal, divinely
 *   revealed truths inscribed in sacred texts and requiring literal
 *   observance. Under this reading, Brahminical priesthood, Kshatriya
 *   rulership, Vaishya commerce, Shudra servitude, and the exclusion of
 *   Dalits constitute a cosmic order (rita) that should not be questioned or
 *   reformed. Women across all varnas are prescribed subordination and
 *   exclusion from Vedic knowledge. The constraint extracts labor, dignity,
 *   and autonomy from lower castes and all women and concentrates authority
 *   and benefit in upper-caste males. This is ONE reading of the contested
 *   Dharmasastra kernel; sibling readings (reformist_contextual,
 *   abolitionist_rejection) claim different ε values and different victim
 *   structures because they instantiate different understandings of the same
 *   textual corpus.
 *
 * KEY AGENTS:
 *   - brahmin_priesthood: Guards the interpretation of sacred texts and Vedic knowledge; justifies their authority as revealed and eternal.
 *   - kshatriya_rulers: Derive legitimacy from the varna framework for hierarchical governance and enforcement of caste law.
 *   - vaishya_merchants: Benefit from third-varna status and access to wealth-accumulation, constrained but privileged relative to Shudras and Dalits.
 *   - shudras: Prescribed to serve, excluded from Vedic knowledge and ritual purity, trapped in inherited occupational roles.
 *   - dalits: Placed outside the varna system entirely, subject to enforced segregation and humiliation, assigned polluting occupations.
 *   - women_all_varnas: Excluded from Vedic learning and independent authority across all castes, prescribed lifelong guardianship and subordination.
 *   - reformist_interpreters: Argue for reinterpretation separating dharma's ethical core from caste prescriptions; systematically excluded from institutional authority.
 *   - abolitionist_movements: Reject Dharmasastra entirely and demand abolition of caste; silenced within orthodox authority structures.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.87).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.91).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.87).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, snare).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Dharmasastra Varna Hierarchy: Orthodox Literalist Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious/legal/normative").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, 'd21418ff-c45b-4e3b-a5b7-589ae771a326').
narrative_ontology:cs_kernel_codification('d21418ff-c45b-4e3b-a5b7-589ae771a326', fixed_text).
narrative_ontology:cs_authority_grounding('d21418ff-c45b-4e3b-a5b7-589ae771a326', lineage).
narrative_ontology:cs_interpretation_layer_present('d21418ff-c45b-4e3b-a5b7-589ae771a326').
narrative_ontology:cs_reading_relation('d21418ff-c45b-4e3b-a5b7-589ae771a326', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_reading_relation('d21418ff-c45b-4e3b-a5b7-589ae771a326', dharmasastra_corpus__abolitionist_rejection, forecloses).
narrative_ontology:cs_axiom('d21418ff-c45b-4e3b-a5b7-589ae771a326', foundational, varna_hierarchy_eternal_revealed).
narrative_ontology:cs_axiom_status(varna_hierarchy_eternal_revealed, holdable).
narrative_ontology:cs_axiom_grounding('d21418ff-c45b-4e3b-a5b7-589ae771a326', varna_hierarchy_eternal_revealed, theological).
narrative_ontology:cs_axiom('d21418ff-c45b-4e3b-a5b7-589ae771a326', foundational, textual_immutability_binding).
narrative_ontology:cs_axiom_status(textual_immutability_binding, holdable).
narrative_ontology:cs_axiom_grounding('d21418ff-c45b-4e3b-a5b7-589ae771a326', textual_immutability_binding, deontological).
narrative_ontology:cs_reference_frame('d21418ff-c45b-4e3b-a5b7-589ae771a326', vedic_cosmic_order_eternally_prescribed).
narrative_ontology:cs_drift_state('d21418ff-c45b-4e3b-a5b7-589ae771a326', contemporary_post_abolition_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d21418ff-c45b-4e3b-a5b7-589ae771a326', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, kshatriya_rulers).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, vaishya_merchants).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudras).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalits).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women_all_varnas).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces Dharmasastra prescriptions through ritual authority, textual commentary, and educational gatekeeping. Claims hereditary right to Vedic knowledge and ritual conduct. Sets the boundaries of legitimate religious practice and determines who may perform sacred functions. Birth-locked into this role; social identity and professional authority are fused with Brahmin status and textual mastery.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood, agenda_setter,
    institutional, civilizational, identity_locked, regional).

% Derive legitimacy from Dharmasastra's prescription of Kshatriya rule and martial duty. Use varna hierarchy to justify hierarchical governance and enforcement of caste law. Benefit from a framework that places them second only to Brahmins and subordinates all others. Identity and authority are constitutionally entwined with varna status and dharmic kingship.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, kshatriya_rulers, beneficiary,
    institutional, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, kshatriya_rulers, agenda_setter).

% Occupy the third varna, permitted to accumulate wealth and conduct commerce while shielded from the lowest ritual status. Benefit from access to education, ritual participation, and freedom to conduct economic activity denied to Shudras and Dalits. Constrained by varna duties but not by enforced ritual purity violations. Exit would mean relinquishing merchant-caste identity and the wealth-accumulation freedom it permits.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, vaishya_merchants, beneficiary,
    powerful, biographical, constrained, regional).

% Prescribed to serve the upper three varnas with no claim to Vedic knowledge, ritual purity, or political authority. Excluded from learning sacred texts and performing Vedic rites. Forced to accept occupational specialization inherited from birth. Required to pay taxes and rents to rulers and landholding Brahmins and Kshatriyas. Exit means social death and loss of all community structures and identity.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudras, payer,
    powerless, civilizational, trapped, regional).

% Placed entirely outside the varna system as 'untouchable,' assigned occupations involving ritual pollution (tanning, leather work, removal of human and animal waste). Subject to enforced segregation, denied access to water sources, temples, and public spaces shared with caste Hindus. Prescribed duties include touching no one ritually higher, bearing additional humiliation burdens, and providing menial labor without compensation. Exit from untouchability status is structurally impossible within the framework.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalits, payer,
    powerless, civilizational, trapped, regional).

% Excluded from Vedic learning and independent ritual authority across all varnas. Subject to lifelong guardianship: first father, then husband, then son. Denied property rights, access to public life, and independent decision-making. Prescribed duties emphasize obedience, sexual fidelity, and service. Identity as wife, mother, and dutiful dependent is the only legitimate frame; exit to independent life is socially and economically impossible.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, women_all_varnas, payer,
    powerless, civilizational, identity_locked, regional).

% Hindu reformers who argue for reinterpretation of Dharmasastra to separate its ethical core from time-bound caste prescriptions. Are systematically excluded from orthodox textual authority structures. Can publish arguments and attract intellectual followers but cannot access institutional control of temples, schools, or ritual authority. Their exclusion is maintained by the very authority structure they critique.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, reformist_interpreters, excluded,
    moderate, biographical, constrained, regional).

% Dalit and women's movements rejecting Dharmasastra entirely and demanding abolition of caste. Lack access to institutional authority over religious education and interpretation. Can organize resistance and advocate for legal prohibition but are structurally silenced within orthodox textual authority circles. Their exclusion and suppression are structural to maintaining the constraint.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, abolitionist_movements, excluded,
    powerless, biographical, constrained, regional).

% Modern academic scholars analyzing Dharmasastra texts, their historical context, and their interpretive traditions. Take testimony from affected communities, study variant commentaries, and map the text's use in justifying oppression. Their analysis documents how the constraint operates and persists.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahminical_scholars, observer,
    institutional, civilizational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__orthodox_literalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Dharmasastra framework purports to establish cosmic order (rita) and social stability by prescribing each varna with divinely ordained duties (svadharma). The coordination problem framed is: how is social organization legitimated, and who determines proper ritual and occupational roles? The answer: through eternal textual prescription, interpreted by Brahmins, enforced by Kshatriya rulers.
% TRANSFER_FUNCTION: Moves labor, ritual deference, sexual control, and economic surplus from lower varnas and all women upward: Shudras and Dalits provide service labor and pay taxes; women provide domestic and reproductive labor; upper-caste males claim exclusive rights to Vedic knowledge, ritual authority, property, and political power. The constraint transfers dignity and autonomy upward and outward from those prescribed lowest.
% ABSENT_VOICES: Dalits and Shudras are explicitly excluded from participation in Vedic knowledge and ritual authority — their objections to their prescribed roles cannot be voiced within the framework's own terms (since they lack standing to interpret dharma). Women across all varnas are excluded from independent theological voice and education. Reformist and abolitionist interpreters are systematically excluded from institutional authority over textual interpretation and religious education, though they can mount external critiques.
% DISAPPEARANCE_RATIONALE: If the Dharmasastra varna hierarchy and its claim to revealed, eternal validity disappeared overnight, the entire institutional structure legitimating Brahminical priesthood, Kshatriya rule, gender subordination, and caste-based occupational assignment would lose its primary warrant. Alternative social orderings (democratic governance, occupational choice, gender equality, inter-caste intermarriage) could emerge. The constraint's persistence depends on its authority being accepted as sacred and immutable; loss of that framing would trigger institutional reorganization.
% FOUNDING_PROBLEM: In early Vedic period, the constraint was justified as solving the problem of how a complex, stratified agricultural society organizes labor and ritual authority. The problem framed was: how are specialized roles assigned and maintained? The Dharmasastra answer: through birth-determined varna and jati, justified as cosmic necessity.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — justifying occupational specialization in a pre-industrial agricultural economy — ceased to be live once industrialization, democratic governance, and market economies arrived. Modern Hindu scholars outside the orthodox literalist tradition, Dalit scholars, and women's-rights advocates attest the founding problem is obsolete. Even orthodox scholars acknowledge the problem's historical anchorage, though they argue the prescription's eternal validity transcends its original context. Legislative history (abolition of untouchability in India's Constitution) and international human-rights consensus document the problem's displacement. No credible secular authority defending occupational caste-assignment exists outside the literalist reading itself.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__orthodox_literalist, 0.87, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.87) because the constraint systematically moves labor, dignity, autonomy, and economic surplus upward; it is decoupled from service provision (the 'cosmic order' framing invokes no cost-sharing, only duty-compliance from below). Suppression is higher still (0.91) because the constraint's persistence depends on silencing competing interpretations, denying education and textual authority to those prescribed lowest, and enforcing ritual purity rules backed by social ostracism and economic penalties. Theater ratio is moderate (0.42): the ritual and textual functions are real (temples function, scholarship occurs), but an increasing share of institutional effort goes to defending the hierarchy against internal and external challenge rather than performing its purported coordination role. Accessibility_collapse is very high (0.89) because once the framework is understood, alternatives appear structurally unavailable within its logic — Dalits and Shudras cannot 'choose' a different varna; women cannot 'exit' to gender equality within the framework. Resistance is substantial (0.76) because Dalit movements, women's movements, and reformist voices mount continuous challenge to the constraint's legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (Brahmin priesthood, Kshatriya rulers) should perceive the constraint as genuine cosmic order and legitimate coordination — they authored its claim and benefit from its enforcement. The victim seats (Shudras, Dalits, women) should perceive it as coerced extraction justified by false authority — they bear its costs and are denied voice in its interpretation. The engine computes this divergence from the directionality structure: beneficiaries approach d=0.0 (full subsidy), targets approach d=1.0 (full extraction). Vaishya merchants sit intermediately — they benefit from third-varna privilege but remain subject to some hierarchical constraints, giving them partial interest in the framework's stability but also potential arbitrage (if occupational rules loosen, their wealth could translate to caste upward mobility, but only through breaking the constraint).
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priesthood: identity-locked beneficiary (d ≈ 0.05). Their entire professional identity, educational pathway, and social authority rest on being Brahmin and sole interpreters of Dharmasastra. Exit means losing priesthood, ritual authority, textual mastery authority, and caste status — all their identity markers are fused with this role. Kshatriya rulers: identity-locked beneficiary (d ≈ 0.08). Rulership and legitimacy derive from varna status; exit means losing governance authority justified by the framework. Vaishya merchants: beneficiary with constrained exit (d ≈ 0.20). They benefit from third-varna status and wealth-accumulation freedom, but their exit is constrained: leaving merchant communities and pursuing other occupations would forfeit their wealth-accumulation privilege. Shudras: trapped target (d ≈ 0.95). They are assigned servitude, denied education and ritual authority, and forced to provide labor without commensurate return. Their exit is structurally impossible — they cannot change varna and have no alternative social role. Dalits: trapped target (d ≈ 0.99). They are placed entirely outside the varna system, subjected to enforced segregation and humiliating occupations. Exit from untouchability is logically impossible within the framework. Women (all varnas): identity-locked target (d ≈ 0.92). Their identity as daughters, wives, mothers is constitutively dependent on male guardianship and subordination. Exit to independent life is economically and socially impossible; their entire identity frame binds them to the hierarchy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (justifying occupational specialization in pre-industrial agricultural economies) is DEAD. Modern Hindu societies have industrialized, adopted democratic governance, opened education and occupational choice, and legally abolished untouchability. The constraint persists not because the founding problem lives but because the beneficiary seats (Brahminical priesthood, Kshatriya-descended rulers, Vaishya merchants) continue to extract authority and material benefit from it. The reformist reading emerged precisely to resolve this mandatrophy: it claims the ethical core of dharma (righteous conduct) is eternal while the caste prescriptions are time-bound and superseded. The abolitionist reading goes further: it denies the constraint's legitimacy entirely and demands its abolition. The orthodox literalist reading responds by reasserting the constraint's eternal, revealed status — it rejects the mandatrophy framing itself, treating the founding problem as still live at the cosmic level even if its empirical instantiation has changed. The constraint's persistence despite founding-problem death is precisely the signal that it is snare, not coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_internalized_suppression,
    'How much of the measured suppression (0.91) operates through structural barriers (ritual purity segregation, legal exclusion, economic restriction) versus internalized beliefs (socialization into acceptance, identity fusion with subordination)?',
    'Post-legal-abolition trajectories: in Indian post-Constitutional context, when structural barriers (untouchability, gender property restrictions) were legally abolished, did suppression persist and at what intensity? Comparison with communities that experienced similar structural barriers but different socialization histories.',
    'If suppression is mostly structural, removing the constraint would liberate subordinated groups relatively quickly. If suppression is heavily internalized, the constraint''s persistence would require not just legal abolition but sustained de-conditioning and identity reconstruction — persistence might outlast the structural constraint itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Whether suppression is structural (external barriers) or internalized (cognitive/identity patterns that persist after barrier removal).').

omega_variable(
    eternal_vs_historical_referent,
    'When the orthodox literalist reading claims Dharmasastra prescriptions are ''eternal,'' does it refer to a timeless cosmic order (metaphysical claim) or to the permanence of the textual corpus itself (historical/textual claim)?',
    'Analysis of orthodox commentarial tradition: do Brahminical scholars distinguish between the eternal validity of the dharmic principles and the application of those principles in different yugas (ages)? If they do distinguish, the eternality claim may be narrower than the literalist framing suggests.',
    'If eternality is a metaphysical claim, the constraint''s persistence would require continued belief in cosmic order—losing that belief would dissolve the authority structure. If eternality is a textual-permanence claim, the constraint persists as long as the texts are treated as authoritative, regardless of whether their prescriptions are applied literally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eternal_vs_historical_referent, conceptual, 'The semantics of ''eternal'' in the orthodox literalist reading.').

omega_variable(
    beneficiary_awareness_and_consent,
    'Do the beneficiary seats (Brahmin priesthood, Kshatriya rulers, Vaishya merchants) actively defend the constraint because they consciously extract from it, or do they defend it because they have internalized its legitimacy as cosmic order?',
    'Historical documentation of Brahminical responses to challenge: do orthodox scholars argue they preserve the constraint because it is true and eternal, or because its preservation maintains their authority? Textual analysis of commentarial tradition for explicit acknowledgment of extractive benefit.',
    'If beneficiaries defend the constraint primarily from internalized legitimacy-belief, they would experience it as mountain or rope even though it is snare. If they consciously defend extraction, they fully understand its coercive operation and are engaged in deliberate oppression. This affects the narrative of constraint persistence: is it false consciousness or deliberate capture?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_awareness_and_consent, conceptual, 'Whether beneficiary defense of the constraint rests on internalized legitimacy or conscious extraction.').

omega_variable(
    reading_committer_ambiguity,
    'Is the ''orthodox literalist reading'' a genuine live intellectual position held by contemporary Hindu scholars and practitioners, or is it an authored analytical construction that no actual person fully embodies?',
    'Ethnographic and textual study of contemporary Hindu orthodox scholars: do any of them explicitly defend the varna hierarchy and gender subordination as eternal prescriptions in the literal sense this reading claims? Or do contemporary orthodox interpreters rely on implicit acceptance and deflection rather than explicit defense?',
    'If the reading is truly live, the constraint has active defenders and this story accurately captures how it persists. If the reading is analytically constructed, the actual persistence mechanism may involve more unconscious maintenance and less explicit defense than this analysis suggests—the performance/theater ratio might be understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, empirical, 'Whether the orthodox literalist reading represents a live position or an analytical reconstruction.').

omega_variable(
    reading_kernel_boundary,
    'Is the Dharmasastra textual corpus itself the kernel, or is ''Hinduism as a religious tradition with caste as its core organizing principle'' the broader kernel, and Dharmasastra is one authoritative instantiation of it?',
    'Map the boundaries of the contested readings: do the reformist and abolitionist readings also contest the Dharmasastra texts specifically, or do they contest caste as a principle that extends beyond Dharmasastra? If caste contestation extends beyond Dharmasastra, the kernel may be broader than the textual corpus.',
    'If the kernel is the Dharmasastra corpus, this constraint is the orthodox literalist reading of those specific texts. If the kernel is ''caste as organizing principle,'' Dharmasastra is one authority among others (Vedic hymns, practice traditions, ritual commentaries), and this reading is one way of defending caste through textual authority rather than the only way caste is justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_boundary, conceptual, 'The appropriate boundaries of the contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.38).
narrative_ontology:measurement(dhar_tr_t5, dharmasastra_corpus__orthodox_literalist, theater_ratio, 5, 0.39).
narrative_ontology:measurement(dhar_tr_t10, dharmasastra_corpus__orthodox_literalist, theater_ratio, 10, 0.4).
narrative_ontology:measurement(dhar_tr_t15, dharmasastra_corpus__orthodox_literalist, theater_ratio, 15, 0.41).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__orthodox_literalist, theater_ratio, 20, 0.42).
narrative_ontology:measurement(dhar_tr_t25, dharmasastra_corpus__orthodox_literalist, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(dhar_be_t5, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 5, 0.84).
narrative_ontology:measurement(dhar_be_t10, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 10, 0.86).
narrative_ontology:measurement(dhar_be_t15, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 15, 0.87).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 20, 0.87).
narrative_ontology:measurement(dhar_be_t25, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 25, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(dhar_su_t5, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 5, 0.89).
narrative_ontology:measurement(dhar_su_t10, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 10, 0.9).
narrative_ontology:measurement(dhar_su_t15, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 15, 0.91).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 20, 0.91).
narrative_ontology:measurement(dhar_su_t25, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 25, 0.91).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(dhar_grid_01, dharmasastra_corpus__orthodox_literalist, accessibility_collapse(class), 0, 0.88).
narrative_ontology:measurement(dhar_grid_02, dharmasastra_corpus__orthodox_literalist, accessibility_collapse(class), 25, 0.85).
narrative_ontology:measurement(dhar_grid_03, dharmasastra_corpus__orthodox_literalist, accessibility_collapse(individual), 0, 0.87).
narrative_ontology:measurement(dhar_grid_04, dharmasastra_corpus__orthodox_literalist, accessibility_collapse(individual), 25, 0.88).
narrative_ontology:measurement(dhar_grid_05, dharmasastra_corpus__orthodox_literalist, accessibility_collapse(organizational), 0, 0.91).
narrative_ontology:measurement(dhar_grid_06, dharmasastra_corpus__orthodox_literalist, accessibility_collapse(organizational), 25, 0.9).
narrative_ontology:measurement(dhar_grid_07, dharmasastra_corpus__orthodox_literalist, accessibility_collapse(structural), 0, 0.89).
narrative_ontology:measurement(dhar_grid_08, dharmasastra_corpus__orthodox_literalist, accessibility_collapse(structural), 25, 0.87).
narrative_ontology:measurement(dhar_grid_09, dharmasastra_corpus__orthodox_literalist, resistance(class), 0, 0.74).
narrative_ontology:measurement(dhar_grid_10, dharmasastra_corpus__orthodox_literalist, resistance(class), 25, 0.8).
narrative_ontology:measurement(dhar_grid_11, dharmasastra_corpus__orthodox_literalist, resistance(individual), 0, 0.68).
narrative_ontology:measurement(dhar_grid_12, dharmasastra_corpus__orthodox_literalist, resistance(individual), 25, 0.74).
narrative_ontology:measurement(dhar_grid_13, dharmasastra_corpus__orthodox_literalist, resistance(organizational), 0, 0.71).
narrative_ontology:measurement(dhar_grid_14, dharmasastra_corpus__orthodox_literalist, resistance(organizational), 25, 0.78).
narrative_ontology:measurement(dhar_grid_15, dharmasastra_corpus__orthodox_literalist, resistance(structural), 0, 0.62).
narrative_ontology:measurement(dhar_grid_16, dharmasastra_corpus__orthodox_literalist, resistance(structural), 25, 0.68).
narrative_ontology:measurement(dhar_grid_17, dharmasastra_corpus__orthodox_literalist, stakes_inflation(class), 0, 0.83).
narrative_ontology:measurement(dhar_grid_18, dharmasastra_corpus__orthodox_literalist, stakes_inflation(class), 25, 0.85).
narrative_ontology:measurement(dhar_grid_19, dharmasastra_corpus__orthodox_literalist, stakes_inflation(individual), 0, 0.84).
narrative_ontology:measurement(dhar_grid_20, dharmasastra_corpus__orthodox_literalist, stakes_inflation(individual), 25, 0.86).
narrative_ontology:measurement(dhar_grid_21, dharmasastra_corpus__orthodox_literalist, stakes_inflation(organizational), 0, 0.79).
narrative_ontology:measurement(dhar_grid_22, dharmasastra_corpus__orthodox_literalist, stakes_inflation(organizational), 25, 0.82).
narrative_ontology:measurement(dhar_grid_23, dharmasastra_corpus__orthodox_literalist, stakes_inflation(structural), 0, 0.81).
narrative_ontology:measurement(dhar_grid_24, dharmasastra_corpus__orthodox_literalist, stakes_inflation(structural), 25, 0.83).
narrative_ontology:measurement(dhar_grid_25, dharmasastra_corpus__orthodox_literalist, suppression(class), 0, 0.9).
narrative_ontology:measurement(dhar_grid_26, dharmasastra_corpus__orthodox_literalist, suppression(class), 25, 0.91).
narrative_ontology:measurement(dhar_grid_27, dharmasastra_corpus__orthodox_literalist, suppression(individual), 0, 0.89).
narrative_ontology:measurement(dhar_grid_28, dharmasastra_corpus__orthodox_literalist, suppression(individual), 25, 0.91).
narrative_ontology:measurement(dhar_grid_29, dharmasastra_corpus__orthodox_literalist, suppression(organizational), 0, 0.92).
narrative_ontology:measurement(dhar_grid_30, dharmasastra_corpus__orthodox_literalist, suppression(organizational), 25, 0.92).
narrative_ontology:measurement(dhar_grid_31, dharmasastra_corpus__orthodox_literalist, suppression(structural), 0, 0.88).
narrative_ontology:measurement(dhar_grid_32, dharmasastra_corpus__orthodox_literalist, suppression(structural), 25, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__orthodox_literalist, 0.12).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, caste_enforcement_legal_apparatus).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, gender_guardianship_hindu_law).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, ritual_purity_segregation_mechanisms).

% DUAL FORMULATION NOTE:
% The Dharmasastra corpus kernel is contested across three distinct readings: (1) orthodox_literalist (this story) — claims eternal, revealed varna hierarchy with high ε and broad victim set; (2) reformist_contextual — separates eternal ethical dharma from time-bound caste prescriptions, substantially lowering ε; (3) abolitionist_rejection — rejects the constraint entirely. These are not different measurements of one constraint; they are three structurally distinct constraints instantiating the same kernel. Each reading declares a different ε, different beneficiary/victim structure, and different claim about the constraint's legitimacy. The three stories are linked via network.affects_constraints to show their kernel kinship and mutual influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dharmasastra_corpus__orthodox_literalist, powerless, 0.95).
constraint_indexing:directionality_override(dharmasastra_corpus__orthodox_literalist, organized, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
