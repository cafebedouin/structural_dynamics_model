% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Naskh Principle: Chronological Abrogation of Quranic Verses
 *   domain: religious/legal/textual
 *
 * SUMMARY:
 *   The naskh principle (classical abrogation reading) is the dominant
 *   interpretive doctrine in mainstream Islamic jurisprudence: later Quranic
 *   verses chronologically abrogate earlier verses on the same
 *   legal/theological topic, rendering the earlier verse(s) legally inert
 *   while retaining them as scripture. This reading coordinates legal
 *   practice by providing a formal hierarchy (chronological revelation order)
 *   that resolves apparent Quranic contradictions deterministically. However,
 *   it extracts from alternative hermeneutical approaches (contextual
 *   harmonization, progressive pedagogy readings) by foreclosing their
 *   authority in jurisprudence. The classical schools and hadith specialists
 *   who maintain naskh doctrine are both beneficiaries (they collect
 *   institutional authority from being its guardians) and its enforcement
 *   agents (they determine chronologies and mark verses as abrogated). This
 *   is a kernel reading — one of three contested readings of how the Quran
 *   handles apparent contradictions. The classical_abrogation reading claimed
 *   here FORECLOSES the contextual_harmonization reading within the framework
 *   of jurisprudential authority (one cannot simultaneously claim
 *   chronological abrogation AND hold that all verses remain contextually
 *   valid at law), but COEXISTS WITH the progressive_restriction reading (one
 *   can hold that later verses restricted permissions without fully
 *   abrogating earlier verses).
 *
 * KEY AGENTS:
 *   - Classical legal schools (Hanafi, Maliki, Shafi'i, Hanbali, and Twelver Shi'a jurisprudence): institutional beneficiaries and agenda-setters; their authority depends on naskh doctrine providing definitive answers.
 *   - Hadith specialists (muhaddithun): enforcement agents; they gatekeep the chronological orderings that make naskh operative.
 *   - Contextual interpretation scholars: systematically disadvantaged; their method of reading verses in situational context is foreclosed by naskh doctrine.
 *   - Theological coherence seekers: bear the cost of apparent divine inconsistency or must accept legal inertness of verses they find spiritually central.
 *   - Contemporary reformers: excluded from authoritative conversation; cannot invoke abrogated verses without first overturning abrogation doctrine.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.68).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.41).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.68).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Naskh Principle: Chronological Abrogation of Quranic Verses").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "religious/legal/textual").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, 'e3b963a7-a685-4244-88b5-31f20ed624db').
narrative_ontology:cs_kernel_codification('e3b963a7-a685-4244-88b5-31f20ed624db', fixed_text).
narrative_ontology:cs_authority_grounding('e3b963a7-a685-4244-88b5-31f20ed624db', lineage).
narrative_ontology:cs_interpretation_layer_present('e3b963a7-a685-4244-88b5-31f20ed624db').
narrative_ontology:cs_reading_relation('e3b963a7-a685-4244-88b5-31f20ed624db', naskh_principle__contextual_harmonization, forecloses).
narrative_ontology:cs_reading_relation('e3b963a7-a685-4244-88b5-31f20ed624db', naskh_principle__progressive_restriction, coexists_with).
narrative_ontology:cs_axiom('e3b963a7-a685-4244-88b5-31f20ed624db', foundational, chronological_determinism_of_legal_force).
narrative_ontology:cs_axiom_status(chronological_determinism_of_legal_force, holdable).
narrative_ontology:cs_axiom_grounding('e3b963a7-a685-4244-88b5-31f20ed624db', chronological_determinism_of_legal_force, deontological).
narrative_ontology:cs_axiom('e3b963a7-a685-4244-88b5-31f20ed624db', secondary, historical_abrogation_as_invalidation).
narrative_ontology:cs_axiom_status(historical_abrogation_as_invalidation, holdable).
narrative_ontology:cs_axiom_grounding('e3b963a7-a685-4244-88b5-31f20ed624db', historical_abrogation_as_invalidation, conventional).
narrative_ontology:cs_reference_frame('e3b963a7-a685-4244-88b5-31f20ed624db', chronological_hierarchy_of_revelation).
narrative_ontology:cs_drift_state('e3b963a7-a685-4244-88b5-31f20ed624db', contemporary_pluralist_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e3b963a7-a685-4244-88b5-31f20ed624db', '').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_legal_schools).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, jurisprudential_consensus_doctrine).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, contextual_interpretation_frameworks).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, theological_coherence_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, contextual_interpretation_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The four canonical Sunni schools (Hanafi, Maliki, Shafi'i, Hanbali) and the Twelver Shi'a jurisprudential tradition. They institutionalized naskh doctrine over centuries, embedding it in their jurisprudential methodologies and case law. Naskh doctrine provides them with definitive answers to difficult cases, reinforcing their authority as the sole legitimate interpreters of Islamic law. Their collective identity is bound to being the custodians of this method. They set the abrogation determinations that bind believers' practice.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, classical_legal_schools, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__classical_abrogation, classical_legal_schools, agenda_setter).

% Hadith scholars (muhaddithun) who determine the chronological order of Quranic revelation through chains of narration (isnad), historical reports, and internal textual analysis. Their expertise is the enforcement mechanism: they validate which verses were revealed when, thereby justifying which abrogation determinations are valid. They exercise substantial control over which abrogation rulings stick (by validating or challenging the chronological claims behind them). Their professional authority depends on being recognized as the experts in this determination.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, hadith_specialists, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Scholars who read the Quran contextually — placing each verse in its historical moment, social situation, and linguistic particularity. They practice a legitimate Islamic hermeneutical tradition but find themselves systematically disadvantaged by naskh doctrine. When they argue that a verse should be read in its context rather than chronologically abrogated, they are told the matter is settled by the schools and hadith specialists. Their method is not forbidden, but institutionally marginalized. They cannot exit without losing professional standing in Islamic jurisprudence.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, contextual_interpretation_scholars, payer,
    moderate, biographical, constrained, global).

% Believers (lay Muslims and some scholars) who seek theological unity and coherence in the Quranic message. They experience naskh doctrine as costly: verses marked as abrogated appear to violate the Quranic theme of God's mercy and consistency. They must either accept that God's guidance changed (troubling for their theology) or that some verses are legally inert despite being scripture (cognitively dissonant). They have no institutional power to challenge this and their identity as believers makes leaving the tradition unthinkable.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, theological_coherence_seekers, payer,
    powerless, biographical, identity_locked, global).

% Jurists who determine when ijma (consensus among scholars) has been reached on abrogation determinations. Once consensus is declared, challenging the abrogation becomes nearly impossible — it would require re-opening settled consensus. These gatekeepers reinforce the naskh doctrine's enforcement by declaring abrogation determinations as consensus facts rather than ongoing debates.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, jurisprudential_consensus_gatekeepers, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Modern Muslim scholars and intellectuals seeking to reinterpret Islamic law in light of contemporary values, scientific knowledge, and social change. They would like to revive verses the classical schools marked as abrogated (e.g., verses on women's legal witness, on slavery prohibition, on economic regulation). But to do so, they must first overturn centuries of institutional consensus about which verses are abrogated — a task that appears to require either massive textual scholarship or a challenge to the schools' authority itself. They are structurally kept from the authoritative conversation.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, contemporary_islamic_reformers, excluded,
    organized, biographical, constrained, national).

% Sufi and philosophical Islamic traditions that emphasize multiple valid ways of reading the Quran for spiritual and theological purposes. They would hold that verses marked as legally abrogated retain theological and spiritual force. They are kept from the jurisprudential conversation because naskh doctrine defines legal force exhaustively by chronological order, leaving no room for contextual spiritual reading to affect law.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, theological_pluralists, excluded,
    moderate, biographical, constrained, regional).

% Historians, philologists, and comparative scholars analyzing how different religious traditions resolve apparent textual contradictions. They treat naskh as one method among many (allegorical reading in Christian and Jewish traditions, genre-based distinction, developmental theory, harmonization). Their seat enables structural comparison but carries no authority within Islamic jurisprudence itself.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, comparative_religion_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__classical_abrogation, classical_legal_schools).
narrative_ontology:fixing_cost_class(naskh_principle__classical_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the practical problem of conflicting legal directives within a sacred text: when two verses from the Quran appear to command opposite things (e.g., drinking wine is permitted vs. drinking wine is forbidden), which one governs legal practice? Naskh provides a formal method: chronological order determines which supersedes which. This enables a single, determinate legal hierarchy from a potentially contradictory corpus, preventing legal paralysis.
% TRANSFER_FUNCTION: Moves interpretive authority from the reader's contextual judgment to the institutional guardians (classical schools, hadith specialists). Transfers power to determine which verses count legally from theological reasoning about textual coherence to historical-philological claims about revelation order. Gains (legitimacy, institutional authority) accrue to the classical schools and hadith specialists; costs (foreclosure of alternative readings, theological dissonance) fall on contextual scholars and theological coherence seekers.
% ABSENT_VOICES: Scholars who favor contextual harmonization (reading apparent contradictions as complementary within their respective contexts) are structurally kept from authoritative jurisprudential conversation. So too are those who read abrogation as progressive restriction (a pedagogical movement where later restrictions refine earlier permissions rather than invalidating them). Contemporary Islamic reformers cannot credibly revive verses marked as abrogated without first dismantling the naskh doctrine itself — they would be heard as challenging textual authority rather than offering legitimate hermeneutical alternatives.
% DISAPPEARANCE_RATIONALE: If classical naskh doctrine disappeared, Islamic jurisprudence would reorganize: legal rulings that depend on chronological abrogation determinations would become contested. Verses long treated as legally inert would re-enter jurisprudential conversation. Schools would have to defend their rulings on grounds other than 'this verse is abrogated' — either through contextual reasoning, theological coherence, progressive restriction narratives, or other hermeneutical methods. The institutional authority of the classical schools would weaken relative to individual scholars, reformed reading communities, and pluralist approaches. Believers would face a fragmented jurisprudential landscape rather than the settled hierarchy naskh provides.
% FOUNDING_PROBLEM: Early Islamic history presented a coordination problem: as Quranic revelation occurred over 23 years (7th century Arabia and Syria), directives on particular practices (alcohol, warfare, women's dress, usury, slavery) appeared at different moments and sometimes seemed directly contradictory. Jurists needed a method to determine: given two apparently conflicting verses, which one governs legal practice? Chronological abrogation (later verse supersedes earlier) provided a method grounded in the historical facts of revelation.
% FOUNDING_PROBLEM_CORROBORATION: Classical Islamic historians (al-Tabari, al-Suyuti) and hadith specialists attest the founding problem is real: they document the historical revelation sequence and note cases where verses address the same topics at different moments. Mainstream jurisprudential consensus treats this as a settled matter requiring naskh doctrine. However, contemporary Islamic scholars (Abdulaziz Sachedina, Khaled Abou El Fadl, Asma Afsaruddin) and comparative scripture scholars attest that the founding problem admits alternative solutions — contextual harmonization, genre-based distinction, theological coherence readings — that do not require chronological abrogation. They argue the 'necessity' of naskh was constructed by institutional preference for legal certainty over theological richness. Outside the benefiting parties (classical schools), the contested status is documented.
narrative_ontology:disappearance_verdict(naskh_principle__classical_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__classical_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__classical_abrogation, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.68 at interval end) because naskh doctrine consolidates interpretive authority into the hands of institutional schools and hadith specialists. The cost to alternative hermeneutical approaches is real: contextual readers cannot credibly challenge the schools' abrogation rulings without appearing to reject textual hierarchy itself. The extractiveness is not as high as a snare (0.68 vs. 0.85+) because naskh doctrine does solve a genuine coordination problem (what to do when verses conflict) and provides a method that believers can learn and apply. However, the method's rigidity means believers pay a cost: they cannot hold certain verses as both spiritually central AND legally inert without cognitive dissonance. Suppression is moderate (0.41) because alternative readings (contextual harmonization, progressive restriction) are not formally forbidden — they exist in Islamic jurisprudence — but they are institutionally marginalized in the dominant schools. Contemporary scholars CAN advocate contextual reading, but do so against established precedent and institutional resistance. Theater ratio is low (0.22) because the doctrine's legitimating narrative (chronological revelation order) is not purely theatrical — chronology is a real historical fact that genuine hadith scholarship attempts to establish. However, a growing share of modern scholarship (evident in publications and reformed communities) questions whether chronology should determine legal force, suggesting some theatrical maintenance. The measurement series show modest extraction accumulation from T0 to T20 and rising theater ratio, consistent with the doctrine becoming increasingly questioned in contemporary Islamic thought while institutional adherence persists.
 *
 * PERSPECTIVAL GAP:
 *   From the classical schools' seat, naskh is a mature jurisprudential method that provides certainty and stability — the constraint appears as rope (genuine coordination enabling legal clarity). From the contextual scholar's seat, naskh appears as enforced hierarchy systematically suppressing alternative methods — closer to tangled rope or snare. From the theological coherence seeker's seat, naskh is a cost imposed by institutional authority that they cannot credibly refuse without leaving the tradition entirely. The divergence is structural, not merely perspectival: the schools have power to determine which verses are abrogated; contextual scholars and theological seekers have no such power. The engine should compute naskh as rope for the schools' seat and as tangled_rope or snare-flavored tangled rope for the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical schools sit at d near 0.0 (full beneficiaries): they benefit from naskh doctrine without bearing its costs. They control the determination of abrogation and use it to consolidate authority. Contextual scholars sit at d near 0.8 (targets): they face systematic disadvantage, institutionalized resistance, and are excluded from authoritative conversation by the abrogation framework. Theological coherence seekers sit at d near 0.65 (substantial targets): they bear the cost of apparent contradiction or legal inertness, but are not actively excluded — they are constrained by the framework's logical force. The power/exit axes differentiate within payer groups: contextual scholars are 'moderate' power with 'constrained' exit because they cannot simply leave Islamic jurisprudence without losing professional standing; they are locked into the tradition. Theological coherence seekers are 'powerless' and 'identity_locked' because they have no institutional power and their identity as believers makes exit unthinkable. This divergence in directionality within the payer class is the seat divergence the engine should detect.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating contradictory legal directives in the Quran) is live and enduring: Muslims still need a method to resolve apparent contradictions. The classical naskh doctrine successfully solved this problem for 1400 years in the dominant schools. However, a secondary problem has emerged in contemporary contexts: believers and reformers want to reinterpret Islamic law in light of modern knowledge and values. The naskh doctrine now prevents this by marking verses as legally inert. Mandatrophy has NOT resolved because the original problem remains live (contradiction resolution is still needed), but the doctrine has acquired a secondary extractive function (protecting institutional authority against reinterpretation). This is why extraction and theater ratio both rise over the measurement interval: the doctrine's original mandate persists, but an increasing share of enforcement energy goes to preventing alternative readings rather than solving the original coordination problem. A snare classification would be premature (the coordination function is real), but the constraint is trending toward mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chronological_determination_empirical,
    'Can the chronological order of Quranic revelation be determined with sufficient certainty to ground legal abrogation decisions?',
    'Comparative analysis of hadith-based chronologies vs. internal textual evidence vs. modern historical-linguistic analysis. Examination of cases where different chronologies produce different abrogation rulings.',
    'If chronological determination is intrinsically underdetermined or contested, the objective ground naskh doctrine claims (chronology as fact, not opinion) collapses. This would reclassify naskh from coordination (objective hierarchy) to extraction (opinion masquerading as fact).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chronological_determination_empirical, empirical, 'Whether chronology can determine abrogation, or remains interpretively contested.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the abrogation determination necessary for legal practice, or does it serve primarily to consolidate interpretive authority?',
    'Historical-sociological analysis: trace which abrogation rulings were adopted because they solved genuine legal conflict vs. those adopted to ratify institutional school precedent. Examine whether contextual harmonization produces practical legal incoherence or merely interpretive pluralism.',
    'If abrogation rulings trace primarily to institutional authority-consolidation rather than necessity, the constraint reclassifies from tangled_rope (real coordination + asymmetric extraction) to snare (extraction with coordination cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether naskh doctrine solves a necessary coordination problem or primarily serves institutional authority.').

omega_variable(
    theological_coherence_cost_internalization,
    'Is the theological dissonance borne by believers (verses appearing contradictory or legally inert) structural suppression of the doctrine, or internalized acceptance of the cost of legal certainty?',
    'Post-exit trajectory analysis: do believers who adopt contextual harmonization frameworks report reduced theological dissonance? Do they maintain commitment to Islamic law? Survey evidence on believer satisfaction and theological coherence across reading methods.',
    'If internalized (believers choose naskh despite cost), suppression is lower and extraction is voluntary coordination. If structural (believers are locked into naskh by institutional gatekeeping), suppression is higher and extraction is coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_coherence_cost_internalization, empirical, 'Whether theological cost is willingly borne or structurally imposed.').

omega_variable(
    alternative_reading_foreclosure_mechanism,
    'Is contextual harmonization foreclose by classical_abrogation doctrine logically (the two readings cannot coexist in any single framework) or institutionally (the doctrine suppresses contextual reading through authority mechanisms)?',
    'Logical analysis: can a coherent jurisprudential framework hold that (a) verses are chronologically ordered AND (b) all verses retain contextual legal validity? If no, it is foreclosure; if yes, it is institutional suppression of a live alternative.',
    'If logically foreclose: classical_abrogation is a genuine kernel reading that eliminates a competing reading. If institutionally suppressed: both readings are live and the engine should model them as coexisting alternatives, not foreclosed rivals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure_mechanism, conceptual, 'Whether contextual harmonization is logically excluded or institutionally suppressed by classical abrogation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__classical_abrogation, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(nask_tr_t0, observed).
narrative_ontology:measurement(nask_tr_t3, naskh_principle__classical_abrogation, theater_ratio, 3, 0.12).
narrative_ontology:measurement_basis(nask_tr_t3, observed).
narrative_ontology:measurement(nask_tr_t6, naskh_principle__classical_abrogation, theater_ratio, 6, 0.15).
narrative_ontology:measurement_basis(nask_tr_t6, observed).
narrative_ontology:measurement(nask_tr_t10, naskh_principle__classical_abrogation, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(nask_tr_t10, observed).
narrative_ontology:measurement(nask_tr_t15, naskh_principle__classical_abrogation, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(nask_tr_t15, observed).
narrative_ontology:measurement(nask_tr_t20, naskh_principle__classical_abrogation, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(nask_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__classical_abrogation, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(nask_be_t0, observed).
narrative_ontology:measurement(nask_be_t3, naskh_principle__classical_abrogation, base_extractiveness, 3, 0.58).
narrative_ontology:measurement_basis(nask_be_t3, observed).
narrative_ontology:measurement(nask_be_t6, naskh_principle__classical_abrogation, base_extractiveness, 6, 0.62).
narrative_ontology:measurement_basis(nask_be_t6, observed).
narrative_ontology:measurement(nask_be_t10, naskh_principle__classical_abrogation, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(nask_be_t10, observed).
narrative_ontology:measurement(nask_be_t15, naskh_principle__classical_abrogation, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(nask_be_t15, observed).
narrative_ontology:measurement(nask_be_t20, naskh_principle__classical_abrogation, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(nask_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__classical_abrogation, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(nask_su_t0, observed).
narrative_ontology:measurement(nask_su_t3, naskh_principle__classical_abrogation, suppression_requirement, 3, 0.32).
narrative_ontology:measurement_basis(nask_su_t3, observed).
narrative_ontology:measurement(nask_su_t6, naskh_principle__classical_abrogation, suppression_requirement, 6, 0.35).
narrative_ontology:measurement_basis(nask_su_t6, observed).
narrative_ontology:measurement(nask_su_t10, naskh_principle__classical_abrogation, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(nask_su_t10, observed).
narrative_ontology:measurement(nask_su_t15, naskh_principle__classical_abrogation, suppression_requirement, 15, 0.4).
narrative_ontology:measurement_basis(nask_su_t15, observed).
narrative_ontology:measurement(nask_su_t20, naskh_principle__classical_abrogation, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(nask_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__contextual_harmonization).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__progressive_restriction).

% DUAL FORMULATION NOTE:
% The naskh_principle kernel has three structurally distinct readings. classical_abrogation (this story) instantiates the doctrine that chronological revelation order determines legal force. contextual_harmonization and progressive_restriction are alternative readings of the same kernel — different parties hold different readings simultaneously. Each reading has its own constraint story, ε value, stakeholder structure, and type classification. The three stories are linked via network.affects_constraints so the corpus records their kinship. They share a kernel (the standing commitment that Quranic contradictions must be resolved) but diverge in how they resolve contradictions and who benefits from each method.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__classical_abrogation, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
