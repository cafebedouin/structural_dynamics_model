% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Dharmasastra Varna/Jati Hierarchy as Eternal Revealed Truth
 *   domain: religious/normative/textual
 *
 * SUMMARY:
 *   The Orthodox Literalist reading of Dharmasastra treats the varna/jati
 *   hierarchy and associated prescriptions (gender subordination,
 *   occupational restriction, ritual exclusion, educational prohibition) as
 *   eternal, cosmically-ordained, revealed truth immutable by human
 *   interpretation. This is one reading of a contested kernel: the
 *   Dharmasastra corpus itself. Under this reading, the varna system is not a
 *   human social arrangement but a reflection of cosmic order (rita) that
 *   must be maintained through literal textual observance to prevent chaos
 *   (varna-sankar) and preserve access to Vedic knowledge. The reading
 *   creates a stark beneficiary structure (Brahmin and Kshatriya authority,
 *   property control, ritual monopoly) and an expansive victim set (Shudras
 *   denied all three, Dalits placed outside the system entirely, women across
 *   all castes denied autonomy and education). The measured extractiveness
 *   (0.87) and suppression (0.89) reflect the comprehensive nature of the
 *   hierarchy: it governs occupational choice, ritual participation,
 *   marriage, inheritance, and knowledge access simultaneously. Theater ratio
 *   (0.62 at interval end, rising from 0.45) indicates increasing
 *   performative maintenance: as literacy spread and reformist
 *   reinterpretations emerged, orthodox enforcers required more theatrical
 *   legitimacy work (emphasis on textual immutability, cosmic necessity
 *   narratives, doctrinal purity) to sustain the system against challenge.
 *   The accessibility_collapse (0.91) reflects how thoroughly the
 *   Dharmasastra framework forecloses alternatives: once a person is assigned
 *   jati by birth, all occupational, marital, and ritual options are
 *   determined; exit requires either accepting social death (leaving the
 *   village) or repudiating the entire framework (conversion or movement to
 *   anonymity). The resistance (0.58) is moderate, not negligible, because
 *   Bhakti movements, heterodox religions, and later reform movements all
 *   mounted sustained challenges to literal observance — but the Orthodox
 *   Literalist framework remained institutionally entrenched through Brahmin
 *   control of temples, education, and textual interpretation until colonial
 *   disruption and post-colonial legal abolition.
 *
 * KEY AGENTS:
 *   - Brahmin priestly class: Institutional beneficiary; monopolizes textual interpretation and ritual authority; claims eternal cosmic assignment
 *   - Kshatriya warrior/king class: Powerful beneficiary; relies on Brahmin legitimation for rule; controlled exit
 *   - Shudra laborers: Powerless payer; prescribed servile labor; trapped exit by birth assignment
 *   - Dalits/untouchables: Powerless payer; placed outside varna system; identity_locked exit (pollution assignment)
 *   - Women across castes: Powerless payer; interdicted autonomy; identity_locked exit (gender + natal varna)
 *   - Reform Hindu movements: Excluded; advocate reinterpretation of dharma; barred from orthodox frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.87).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.89).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.87).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Dharmasastra Varna/Jati Hierarchy as Eternal Revealed Truth").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious/normative/textual").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, '71a94dc7-c96d-43e4-828a-c0305dc367c0').
narrative_ontology:cs_kernel_codification('71a94dc7-c96d-43e4-828a-c0305dc367c0', fixed_text).
narrative_ontology:cs_authority_grounding('71a94dc7-c96d-43e4-828a-c0305dc367c0', lineage).
narrative_ontology:cs_interpretation_layer_present('71a94dc7-c96d-43e4-828a-c0305dc367c0').
narrative_ontology:cs_reading_relation('71a94dc7-c96d-43e4-828a-c0305dc367c0', dharmasastra_corpus__reformist_contextual, coexists_with).
narrative_ontology:cs_reading_relation('71a94dc7-c96d-43e4-828a-c0305dc367c0', dharmasastra_corpus__abolitionist_rejection, coexists_with).
narrative_ontology:cs_axiom('71a94dc7-c96d-43e4-828a-c0305dc367c0', foundational, varna_hierarchy_eternally_revealed).
narrative_ontology:cs_axiom_status(varna_hierarchy_eternally_revealed, holdable).
narrative_ontology:cs_axiom_grounding('71a94dc7-c96d-43e4-828a-c0305dc367c0', varna_hierarchy_eternally_revealed, deontological).
narrative_ontology:cs_axiom('71a94dc7-c96d-43e4-828a-c0305dc367c0', foundational, literal_textual_observance_maintains_cosmic_order).
narrative_ontology:cs_axiom_status(literal_textual_observance_maintains_cosmic_order, holdable).
narrative_ontology:cs_axiom_grounding('71a94dc7-c96d-43e4-828a-c0305dc367c0', literal_textual_observance_maintains_cosmic_order, empirically_contingent).
narrative_ontology:cs_reference_frame('71a94dc7-c96d-43e4-828a-c0305dc367c0', vedic_cosmic_order_maintained_by_varna_hierarchy).
narrative_ontology:cs_drift_state('71a94dc7-c96d-43e4-828a-c0305dc367c0', contemporary_post_colonial_scientific_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('71a94dc7-c96d-43e4-828a-c0305dc367c0', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, kshatriya_warrior_class).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudra_laborers).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalits_untouchables).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women_across_castes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, vaisya_merchant_class).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, varna_system_cosmically_ordained).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, ritual_purity_graded_by_birth).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, vedic_textual_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces Dharmasastra prescriptions as the priestly authority. Claims monopoly on Vedic knowledge, ritual performance, and textual exegesis. Collects ritual fees (dakshina), controls temple authority, and adjudicates moral-legal disputes. Their birth-varna status is claimed as intrinsically suited to this role by eternal cosmic order. Can exit through reinterpretation (reformist reading) but that would require abandoning their structural authority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahmin_priestly_class, agenda_setter,
    institutional, civilizational, arbitrage, regional).

% Holds secular authority (kingship, land control, military power) legitimated by Brahmin endorsement of their varna status and dharmic right to rule. Benefits from Brahmin religious authority validating their dominion. Their power depends on Brahmin sanction; they cannot fully exit without losing religious legitimacy, but can pressure Brahmins to adjust interpretation within bounds.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, kshatriya_warrior_class, beneficiary,
    powerful, civilizational, constrained, regional).

% Permitted to accumulate wealth and conduct commerce under varna constraints; excluded from ritual authority and political rule but gain property security through the cosmic order narrative. Benefits from Brahmin legitimation of their economic rights within hierarchy. Exit would mean economic vulnerability outside the structured system.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, vaisya_merchant_class, beneficiary,
    moderate, biographical, constrained, regional).

% Mandated to serve the three upper varnas (Brahmins, Kshatriyas, Vaishyas) without property rights or education access. Prescribed roles are servile labor, agriculture, and crafts. Ritual purity rules forbid them from entering temples, hearing Vedic recitation, or performing rituals. Bound by birth-jati assignment; exit requires movement to outside communities or accepting social death.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudra_laborers, payer,
    powerless, biographical, trapped, regional).

% Placed outside and below the varna system itself by Dharmasastra; branded as 'untouchable' due to occupations (leather work, cremation, waste handling, meat processing) deemed polluting by ritual law. Comprehensive exclusion: forbidden touch, shadow, food sharing, temple entry, Vedic learning. Their very person is declared ritually contagious. Identity is locked by birth and polluting-occupation assignment; escape requires departure from the community and severing all kinship ties.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalits_untouchables, payer,
    powerless, biographical, identity_locked, regional).

% Prescribed subordination to father, husband, and son across all varnas. Denied direct Vedic study (Brahmin women), denied political authority (Kshatriya/Vaisya women), and Shudra/Dalit women face intersecting caste + gender suppression. Permitted narrowly to manage household and bear heirs; forbidden widow remarriage (high-varna prescriptions); identity locked by both gender and natal varna. Cannot contract independently, inherit equally, or choose marriage partner.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, women_across_castes, payer,
    powerless, biographical, identity_locked, regional).

% Advocate reinterpretation of Dharmasastra (reformist reading) or outright rejection (abolitionist reading), but are excluded from authoritative textual interpretation by the Orthodox Literalist framework. Their voices — rejecting eternal varna truth — are structurally barred from shaping the orthodox framework because the framework declares its own interpretation the only valid one. They operate outside the constraint system itself.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, reform_hindu_movements, excluded,
    organized, generational, constrained, regional).

% Records and codifies Dharmasastra through colonial legal translation (e.g., selections in Anglo-Hindu law); treats the texts as fixed ethnographic artifacts. Their documentation role has unintended consequence: freezing living interpretive tradition into purported static doctrine, enabling Orthodox Literalist claims to textual immutability. They do not enforce Dharmasastra but their administrative translation into law amplifies its reach.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, british_colonial_administration, observer,
    institutional, biographical, analytical, regional).

% Communities (temples, ashrams, village councils) that continue daily application of Dharmasastra prescriptions through ritual, food restrictions, marriage rules, occupational boundaries. They maintain the constraint through repetition and social enforcement, not through theological argument. Their compliance enables the constraint but they are not its primary beneficiary (Brahmins are).
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, ritual_observance_communities, observer,
    moderate, civilizational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__orthodox_literalist, brahmin_priestly_class).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__orthodox_literalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes Hindu society into hereditary functional orders (varnas) for putative social stability: each varna performs its prescribed role (Brahmins teach/ritualize, Kshatriyas rule/protect, Vaishyas trade, Shudras serve). Declares the hierarchy cosmically ordained and thus self-justifying coordination, removing need for continuous renegotiation of social roles.
% TRANSFER_FUNCTION: Moves ritual authority, political legitimacy, property rights, and educational access upward to Brahmin and Kshatriya castes, while moving labor obligation, exclusion from knowledge, and ritual pollution downward to Shudras and Dalits. Women across all castes transfer autonomy (marriage choice, property control, movement) to male guardians.
% ABSENT_VOICES: Reformist interpreters (advocating recontextualization of Dharmasastra) and abolitionists (rejecting the entire framework) are structurally excluded from authoritative interpretation within this reading's epistemic bounds — the Orthodox Literalist reading declares itself the only legitimate reading and dismisses alternatives as apostasy or modern corruption. Their testimony about whether the hierarchy is eternal, whether literal observance is defensible, whether the victim set is truly content — these voices are pre-excluded from the orthodox framework itself.
% DISAPPEARANCE_RATIONALE: If literal Dharmasastra prescription disappeared overnight, the entire varna/jati system would require renegotiation. Property inheritance patterns would need rewriting (Shudras and women currently excluded from equal inheritance). Education access would open (Vedic knowledge is now restricted). Occupational boundaries would blur (Dalit occupations would lose their prescribed polluting status). The framework that legitimates Brahmin authority, Kshatriya kingship, and gender subordination would collapse, forcing redefinition of social organization around different principles.
% FOUNDING_PROBLEM: According to Orthodox Literalist reading: the Vedas reveal eternal cosmic order (rita) that must be maintained through varna-based functional specialization; without enforced hierarchy, society would descend into chaos (varna-sankar, intermixture) and lose access to Vedic knowledge and ritual truth. The founding problem is metaphysical: preservation of cosmic order through textual obedience.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox authorities (Brahmin scholars, contemporary Hindu nationalism advocates, traditionalist exegetes) attest the problem is live and the hierarchy is necessary. Reformist scholars, Dalit movements, and secular historians document that the 'founding problem' was constructed retrospectively to justify existing dominance — the Vedas themselves show evidence of redaction and that Dharmasastra was authored in specific historical periods (not eternal). International human rights frameworks and Indian constitutional law (especially post-Ambedkar) treat the hierarchy as a constructed harm, not a cosmic necessity. The 'living problem' reading is attested by reformist Hindu movements and abolitionist traditions from within Hinduism, not only from outside observers.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.87): The hierarchy extracts from Shudras and Dalits nearly complete subordination — they are prohibited from owning land equally, studying Vedas, entering temples, and choosing occupations. Women lose property control and marriage choice. The extraction is systematic across every life domain. It is not mere redistribution but comprehensive assignment of inferior status. The claim of eternality (avyaya-dharma) attempts to naturalize the extraction as cosmic necessity, removing the possibility of remedy. Suppression (0.89): The suppression operates at multiple levels. Structurally: Brahmins control ritual gatekeeping (entry to temples, access to Vedic recitation), Kshatriyas control military force (enforcement of occupational boundaries), and property law (excluding Shudras from land ownership) enforces the hierarchy. Internalized: The system teaches victims that their birth-assigned status is cosmic truth and social contamination, creating psychological acceptance of subordination. Ritual purity rules reinforce suppression by marking Dalit touch as polluting, making the boundary absolute. Resistance to exit is compounded by lack of alternatives: a Dalit leaving the village faces discrimination elsewhere without the inherited occupational structure; a woman leaving her guardian faces homelessness. Theater ratio (0.62): Rising from 0.45, indicating theatrical maintenance. In early centuries, literal observance had more direct enforcement machinery (Brahmin adjudication, Kshatriya military). By the colonial and modern periods, as literacy spread and rationalist critique emerged, the orthodox framework shifted toward more rhetorical work: claiming unchanging textual authority, asserting cosmic necessity, treating challenges as modern corruption. Contemporary Hindu nationalism, for example, invests heavily in theater about dharma's transcendent truth while defending a diminished material enforcement apparatus (post-constitutional abolition of untouchability, education access, legal property equality). Accessibility collapse (0.91): Once born into a jati, alternatives collapse nearly completely. The Dharmasastra framework assigns occupation, marriage partner pool, ritual role, and knowledge access by birth. Mobility within the system exists (a Brahmin can become a sage, a king can patronize temples) but mobility OUT requires violent rupture (conversion, departure, legal abolition). The constraint is nearly totalizing in its scope — social existence outside it is made difficult by lack of institutional support. Resistance (0.58): Not negligible. Bhakti poets (Ravidas, Kabir, later Marathi figures) explicitly rejected caste and offered devotion without Brahmin mediation. Buddhist and Jain movements offered exit paths that still existed within the cultural framework. Shudra movements periodically asserted claims to education and ritual participation. Dalit movements have, especially post-Ambedkar, organized institutional alternatives (Dalit Buddhism, secular education). The Orthodox Literalist reading has been continuously contested, not universally accepted, which is why it requires such high suppression and theater to persist. Claim/metric independence: The constraint is CLAIMED as tangled_rope (coordination via functional varna assignment + extraction via hierarchy) but the metrics describe a system substantially more extractive (0.87) and suppressively maintained (0.89) than a genuine coordination mechanism would be. A pure coordination system would show lower suppression (people willingly cooperate) and rising accessibility (alternatives remain available; people choose the system). This constraint shows high suppression and near-total accessibility collapse, suggesting the 'coordination' element is deeply compromised and the extraction is primary. The claim is the Orthodox framework's own framing; the metrics describe what the system actually does to victims.
 *
 * PERSPECTIVAL GAP:
 *   From the Brahmin priestly seat: This is genuine coordination. The varna system solves the problem of social order by assigning each varnas its proper function. Brahmins teach and ritualize (accessing Vedic truth), Kshatriyas rule and protect (maintaining order), Vaishyas trade (creating prosperity), Shudras serve (enabling the upper varnas' higher functions). Each varna is suited by nature (guna and karma) to its role. The system is cosmically sanctioned (revealed in the Vedas) and thus immutable. Literal observance maintains rita (cosmic order) and prevents varna-sankar (intermixture that leads to chaos). From this seat, the constraint type is rope (genuine coordination without extractive intent) or at most tangled rope with the extraction being the necessary cost of coordination infrastructure. From the Shudra laborer seat: This is pure extraction. You are assigned servile work and forbidden from education, ritual, and autonomous decision-making. You pay a transfer of labor to the upper varnas who do not reciprocate with equal benefit. The system forbids you from learning why it is supposedly necessary or from leaving it. From this seat, the constraint is snare (extraction defended by suppression and prevented alternatives). From the Dalit untouchable seat: This is maximum extraction and dehumanization. You are placed outside the system entirely, branded with cosmic pollution for occupations the system needs you to perform (cremation, leather work, waste). You cannot touch others, cannot enter temples, cannot access knowledge. The system has created a paradox: it requires your work while denying your humanity. From this seat, the constraint is snare or worse — a total capture mechanism where even conversion may not fully release you (residual caste discrimination). From a reformist or abolitionist analytical seat: The claim of eternality is fabricated. The Dharmasastra texts were authored in specific historical periods (Manusmriti circa 200 BCE–200 CE) and redacted multiple times. The varna hierarchy reflects the interests of Brahmin and Kshatriya power-holders at those times, not cosmic truth. The constraint is constructed, maintained by force, and defended by theater (the elaborate mythology of cosmic eternality). The fact that different communities hold different readings simultaneously — some literalist, some reformist, some abolitionist — shows the eternality claim is contested, not universally accepted. From this analytical seat, the constraint oscillates between snare (for the literalist reading, high extraction) and rope (for the reformist reading, interpreting the coordination core while rejecting the hierarchy) — the type depends on which reading is institutionally dominant at a given time. The engine computes per-seat classifications from the structural data; these perspectival gaps show why: the Brahmin seat and the Dalit seat experience entirely different constraints from the same texts because their structural relationships (beneficiary vs. target) are inverse.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priestly class (d approaching 1.0, full target beneficiary): Collects ritual fees (dakshina), monopolizes textual interpretation, adjudicates disputes, controls education. Exit is high-cost (would require surrendering institutional authority and claim to cosmic assignment). Their power is institutional; their exit is arbitrage (can reinterpret dharma, move to new role) but that exit costs them the hierarchical advantage they receive from literal interpretation. d ≈ 0.1 (beneficiary end). Kshatriya warrior/king class (d ≈ 0.2): Benefits from Brahmin legitimation of their rule. Constrained exit (need Brahmin endorsement to maintain secular authority). Power is powerful but dependent. Some arbitrage (can pressure Brahmins, can turn to other religions like Buddhism for legitimacy) but heavy cost. Vaishya merchants (d ≈ 0.25): Limited benefits (property rights, commerce permission within hierarchy) and limited costs (not directly oppressed like Shudras). Mobile exit (can move to urban markets, become merchant-princes). Shudra laborers (d ≈ 0.95): Near-total target. All prescribed roles are servile. Exit is trapped (no occupation outside servitude in prescribed role). Power is powerless. The constraint extracts their labor, restricts their education, and forbids their participation in knowledge systems. Dalits (d = 1.0): Full target. Placed outside the varna system itself, branded with cosmic pollution. Comprehensive prohibition on touch, food sharing, temple entry, knowledge access. Exit is identity_locked (cannot shed untouchable status short of conversion or violent rupture). Women across castes (d ≈ 0.95): Near-total target. Subordination to male guardians, loss of property control, marriage assignment, childbearing obligation, widow prohibition. Exit is identity_locked (cannot choose partner or economic independence short of widowhood or rupture from family/community). The directionality profile shows steep gradation: beneficiaries sit at the far beneficiary end (d near 0.1) while victims sit at the far target end (d near 1.0). This steep gradient drives high effective extraction — the constraint concentrates benefits at the top and costs at the bottom with minimal middle ground. The identity_locked exits for Dalits and women indicate the constraint's persistence relies not only on force but on cognitive/relational fusion: they are taught to internalize their assigned status as natural, making exit psychologically unthinkable even when structural barriers could technically be crossed.
 *
 * MANDATROPHY ANALYSIS:
 *   Founding problem: According to the Orthodox Literalist reading, the problem is the maintenance of cosmic order (rita) and preservation of Vedic knowledge against social chaos (varna-sankar, intermixture). The system was built to answer this metaphysical problem through hierarchical functional specialization mandated by eternal revelation. Founding problem status per Orthodox reading: Live — the problem persists because society still tends toward intermixture and chaos without structured hierarchy; Vedic knowledge still requires protected priesthood. Founding problem status per reformist/abolitionist readings: Dead — the founding problem (actual disorder) was solved centuries ago; the modern constraint persists only because it benefits Brahmins and upper castes institutionally, not because the original problem is live. It has become a zombie: the stated reason (cosmic order maintenance) no longer matches the functional reality (institutional power defense). Disappearance verdict: World rearranges — if literal Dharmasastra prescription disappeared, property inheritance would be redefined, occupational boundaries would blur, ritual authority would decentralize, education would open. This confirms the founding problem is contested. The constraint's actual function (maintaining Brahmin/Kshatriya authority) is distinct from its stated function (maintaining cosmic order). Mandatrophy resolution: The divergence between founding problem status and disappearance verdict points to mandatrophy. The founding problem (cosmic order via ritual hierarchy) is either dead or contested. But the constraint persists because institutional beneficiaries (Brahmins, Kshatriya elites) gain from it regardless. The theater_ratio's rise (0.45 → 0.62) is a mandatrophy signal: as the founding problem's relevance dimmed (rationalist critique, scientific worldview spread, legal abolition of untouchability), the orthodox framework responded by increasing performative work (theater) to maintain the constraint's appearance of necessity while its actual function shifted from coordination to rent defense. The constraint has outlived its founding mandate and now persists primarily through inertia, institutional gatekeeping (Brahmin control of temples and seminaries), and internalized suppression. Piton risk: The high theater_ratio (0.62) and the gap between founding problem status and actual enforcement machinery suggest piton properties — an atrophied function (cosmic order maintenance is no longer the primary mechanism; institutional power defense is) persisting through theatrical maintenance. However, unlike a pure piton, the constraint still has concentrated beneficiaries (Brahmins, Kshatriya elites) who actively defend it, so it remains tangled rope rather than piton — the beneficiaries still profit enough to maintain it, even if the original coordination story no longer fully holds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_eternality_claim_vs_empirical_textual_history,
    'Is Dharmasastra truly eternal revealed truth (avyaya-dharma) as the Orthodox Literalist reading claims, or was it authored in specific historical periods (circa 200 BCE – 200 CE for Manusmriti) and redacted multiple times?',
    'Textual-historical analysis comparing Dharmasastra manuscripts across centuries, linguistic evolution studies, and cross-referencing against datable historical events mentioned in the texts. Comparative Vedic scholarship examining whether varna prescriptions appear in early Vedas or are Brahminical later addition.',
    'If authored in specific historical periods, the claim to eternality collapses — the constraint would be exposed as constructed (likely tangled_rope or snare), not naturally occurring. The Orthodox Literalist reading''s core authority claim (revelation) would be undermined. Would shift classification toward snare for payer seats (Shudras, Dalits, women) and away from legitimacy-via-eternity for beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_eternality_claim_vs_empirical_textual_history, empirical, 'Whether Dharmasastra is empirically eternal or historically authored.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.89) primarily structural (brahminical institutional enforcement, ritual barriers, legal exclusion) or internalized (victims come to believe they deserve untouchable/subservient status by birth, accepting polluting identity)?',
    'Post-exit suppression trajectory: when individuals escape the varna system (conversion, migration to urban anonymity, reformist education), do they retain the internalized belief in caste inferiority, or do they rapidly reconstruct identity? Survey data on caste identity persistence among diaspora. Comparative case study of communities that rejected the system wholesale (e.g., Ambedkar''s conversion movements) to assess cognitive liberation post-exit.',
    'If primarily structural, suppression metrics reflect external force; the system requires continuous enforcement machinery (temples, Brahmin adjudication, ritual gatekeeping). If substantially internalized, victims carry the suppression with them after exit, making the system more persistent but also more vulnerable to cognitive intervention (education, consciousness-raising). High internalization would suggest the extracted suppression is partly cognitive lock-in (related to identity_locked exit options) rather than pure coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is structural enforcement or internalized belief.').

omega_variable(
    beneficiary_alignment_fragility,
    'Is the beneficiary coalition (Brahmins + Kshatriyas) stable across the interval, or did upper-caste interests diverge (e.g., merchant Vaishyas accumulating wealth without Brahmin approval, Kshatriya kings sometimes defecting to Buddhism/Jainism, weakening Brahmin authority)?',
    'Historical documentation of caste-coalition tensions: instances of Kshatriya patronage of non-Brahminical religions, Vaishya sponsorship of heterodox movements, kings refusing to defer to Brahmin interpretation. Evidence of Brahmin necessity to reformulate prescriptions to retain allied castes'' loyalty (already visible in texts like Arthashastra recommending pragmatic deviation from Vedic law).',
    'If the beneficiary coalition was fragile and required continuous renegotiation, the constraint is less a stable eternal system and more an actively-maintained tangled rope (coordination + extraction) that had to adapt to keep upper-caste defection at bay. A fragile coalition suggests the theater_ratio''s rise (0.45 → 0.62) reflects increasing need for legitimacy theater to hold allies, not deepening naturalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_alignment_fragility, empirical, 'Whether beneficiary coalition (Brahmin + Kshatriya dominance) remained unified or fractured.').

omega_variable(
    reading_contest_framing_choice,
    'Does the Orthodox Literalist reading''s claim to represent ''eternal unchanging dharma'' foreclose or coexist with reformist and abolitionist readings within contemporary Hindu discourse?',
    'Survey of current Hindu institutional landscape (seminaries, temples, publishing, social movements). Do contemporary organizations hold that literalist and reformist readings are logically incompatible and mutually foreclosing? Or do they coexist as different-caste or different-community interpretations of the same tradition without direct contradiction?',
    'If readings foreclose one another, the classification of this constraint (Orthodox Literalist) is vulnerable to reclassification once sibling readings gain institutional power — one reading''s type would flip as another reading''s interpretation became authoritative. If readings coexist, the constraint persists because different communities hold different readings simultaneously, suggesting the tangled rope is sustained by institutional inertia and divided authority rather than consensus on eternality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_framing_choice, conceptual, 'Whether Orthodox Literalist reading forecloses or coexists with reformist/abolitionist siblings in contemporary Hindu interpretation landscape.').

omega_variable(
    victim_coalition_counterforce_presence,
    'Did victim groups (Shudras, Dalits, women) mount organized resistance to literal Dharmasastra observance, and if so, when and with what institutional backing?',
    'Historical documentation of Dalit movements (Dr. Ambedkar''s Dalit Buddhism, etc.), Bhakti movements (often lower-caste led, challenging ritual hierarchy), women''s education movements, Shudra assertion movements (e.g., Marathi Shudra mobilization). Record of when these movements became institutionally coherent enough to offer an alternative framing of dharma.',
    'High organized resistance (via Bhakti, Dalit movements) would elevate the ''resistance'' metric and suggest the constraint''s persistence is not due to universal acceptance but active suppression of organized alternatives. This would strengthen the snare classification for payer seats, since their acceptance appears enforced rather than chosen. The theater_ratio rise would reflect increasing need for suppression machinery, not deeper legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_coalition_counterforce_presence, empirical, 'Presence and timing of organized victim-group resistance to literal Dharmasastra observance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(dhar_tr_t0, observed).
narrative_ontology:measurement(dhar_tr_t250, dharmasastra_corpus__orthodox_literalist, theater_ratio, 250, 0.48).
narrative_ontology:measurement_basis(dhar_tr_t250, observed).
narrative_ontology:measurement(dhar_tr_t500, dharmasastra_corpus__orthodox_literalist, theater_ratio, 500, 0.52).
narrative_ontology:measurement_basis(dhar_tr_t500, observed).
narrative_ontology:measurement(dhar_tr_t1000, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1000, 0.58).
narrative_ontology:measurement_basis(dhar_tr_t1000, observed).
narrative_ontology:measurement(dhar_tr_t1500, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1500, 0.61).
narrative_ontology:measurement_basis(dhar_tr_t1500, observed).
narrative_ontology:measurement(dhar_tr_t2000, dharmasastra_corpus__orthodox_literalist, theater_ratio, 2000, 0.62).
narrative_ontology:measurement_basis(dhar_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.85).
narrative_ontology:measurement_basis(dhar_be_t0, observed).
narrative_ontology:measurement(dhar_be_t250, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 250, 0.86).
narrative_ontology:measurement_basis(dhar_be_t250, observed).
narrative_ontology:measurement(dhar_be_t500, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 500, 0.87).
narrative_ontology:measurement_basis(dhar_be_t500, observed).
narrative_ontology:measurement(dhar_be_t1000, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1000, 0.88).
narrative_ontology:measurement_basis(dhar_be_t1000, observed).
narrative_ontology:measurement(dhar_be_t1500, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1500, 0.87).
narrative_ontology:measurement_basis(dhar_be_t1500, observed).
narrative_ontology:measurement(dhar_be_t2000, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 2000, 0.87).
narrative_ontology:measurement_basis(dhar_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.82).
narrative_ontology:measurement_basis(dhar_su_t0, observed).
narrative_ontology:measurement(dhar_su_t250, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 250, 0.84).
narrative_ontology:measurement_basis(dhar_su_t250, observed).
narrative_ontology:measurement(dhar_su_t500, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 500, 0.85).
narrative_ontology:measurement_basis(dhar_su_t500, observed).
narrative_ontology:measurement(dhar_su_t1000, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1000, 0.87).
narrative_ontology:measurement_basis(dhar_su_t1000, observed).
narrative_ontology:measurement(dhar_su_t1500, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1500, 0.89).
narrative_ontology:measurement_basis(dhar_su_t1500, observed).
narrative_ontology:measurement(dhar_su_t2000, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 2000, 0.89).
narrative_ontology:measurement_basis(dhar_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__orthodox_literalist, 0.12).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% The Dharmasastra kernel has three structurally distinct readings. This constraint (Orthodox Literalist) claims the varna hierarchy is eternal and justified by cosmic order; it computes as tangled rope with very high extraction and suppression, yet claims genuine coordination function. The reformist reading reinterprets the texts to permit social mobility and gender inclusion; it would compute as rope or reformed tangled rope with lower extraction and higher accessibility. The abolitionist reading rejects the texts entirely as oppressive structures; it would compute as snare identifying the hierarchy as pure extraction with comprehensive suppression. The three readings share the same kernel (Dharmasastra corpus) but produce entirely different constraints because they disagree on the core question: is the hierarchy eternally justified (Orthodox), historically time-bound and reinterpretable (Reformist), or fundamentally unjust and indefensible (Abolitionist)? Each reading must be authored as a separate constraint with its own beneficiary/victim structure, metrics, and type, linked via network edges showing mutual influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dharmasastra_corpus__orthodox_literalist, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
