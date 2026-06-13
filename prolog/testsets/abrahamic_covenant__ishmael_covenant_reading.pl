% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__ishmael_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__ishmael_covenant_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: abrahamic_covenant__ishmael_covenant_reading
 *   human_readable: Abrahamic Covenant: Ishmael Inclusive Reading
 *   domain: religious/institutional/theological
 *
 * SUMMARY:
 *   The Abrahamic covenant (Genesis 12, 15, 17, 21) is a foundational text in
 *   three religious traditions. One key interpretive crux is whether the
 *   covenant passes exclusively through Isaac (Jewish and Christian reading,
 *   based on Genesis 17:19-21) or includes Ishmael through Islamic prophetic
 *   succession (Islamic reading, based on Quranic reinterpretation and
 *   hadith). This constraint story instantiates the Ishmael-inclusive
 *   reading: a theological claim that the covenant is not limited to Isaac's
 *   line but flows through Ishmael to Muhammad and the Islamic community. The
 *   reading emerged in early Islamic exegetical tradition and became
 *   institutionalized through hadith collections, tafsir commentaries, and
 *   legal doctrine. It is actively enforced by Islamic religious authorities
 *   and is contested by Jewish and some Christian scholars who maintain the
 *   Isaac-exclusive reading. The claim and the metrics are authored
 *   independently: this reading is CLAIMED as tangled_rope (coordination
 *   function + asymmetric extraction + active enforcement) while the metrics
 *   describe how extraction has accumulated and suppression has stabilized
 *   over the interval.
 *
 * KEY AGENTS:
 *   - islamic_community: Global Muslim believers and practitioners who internalize the Ishmael covenant reading as foundational to Islamic identity and prophetic legitimacy.
 *   - muhammadan_prophetic_lineage: The institutional Islamic scholarly and jurisprudential tradition (hadith masters, tafsir scholars, legal schools) that maintains and enforces the Ishmael reading through textual interpretation and educational transmission.
 *   - jewish_exclusive_interpretation_holders: Jewish communities and scholars committed to the Isaac-exclusive covenant reading (Genesis 17:19-21 as limiting covenant to Isaac's seed); they experience the Ishmael reading as a challenge to Jewish particularity.
 *   - christian_supersessionist_stakeholders: Christian communities historically claiming the Church supersedes Israel as covenant heir; the Ishmael reading offers an alternative that bypasses both Jewish exclusivity and Christian supersession.
 *   - academic_biblical_scholars: Neutral analytical observers who assess the textual basis for both readings and document the exegetical traditions supporting them.
 *   - interfaith_dialogue_practitioners: Communities seeking common Abrahamic ground; excluded from formal interpretive authority but increasingly engaged in dialogue spaces.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__ishmael_covenant_reading, 0.58).
domain_priors:suppression_score(abrahamic_covenant__ishmael_covenant_reading, 0.62).
domain_priors:theater_ratio(abrahamic_covenant__ishmael_covenant_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__ishmael_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__ishmael_covenant_reading, "Abrahamic Covenant: Ishmael Inclusive Reading").
narrative_ontology:topic_domain(abrahamic_covenant__ishmael_covenant_reading, "religious/institutional/theological").

domain_priors:requires_active_enforcement(abrahamic_covenant__ishmael_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__ishmael_covenant_reading, 'd7ef8035-b774-496d-b8db-1606abc57a85').
narrative_ontology:cs_kernel_codification('d7ef8035-b774-496d-b8db-1606abc57a85', fixed_text).
narrative_ontology:cs_authority_grounding('d7ef8035-b774-496d-b8db-1606abc57a85', lineage).
narrative_ontology:cs_interpretation_layer_present('d7ef8035-b774-496d-b8db-1606abc57a85').
narrative_ontology:cs_reading_relation('d7ef8035-b774-496d-b8db-1606abc57a85', abrahamic_covenant__isaac_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7ef8035-b774-496d-b8db-1606abc57a85', abrahamic_covenant__land_promise_constraint, influences).
narrative_ontology:cs_axiom('d7ef8035-b774-496d-b8db-1606abc57a85', foundational, covenant_inclusive_through_ishmael).
narrative_ontology:cs_axiom_status(covenant_inclusive_through_ishmael, holdable).
narrative_ontology:cs_axiom_grounding('d7ef8035-b774-496d-b8db-1606abc57a85', covenant_inclusive_through_ishmael, deontological).
narrative_ontology:cs_axiom('d7ef8035-b774-496d-b8db-1606abc57a85', foundational, muhammadan_prophetic_succession_continuous_with_abraham).
narrative_ontology:cs_axiom_status(muhammadan_prophetic_succession_continuous_with_abraham, holdable).
narrative_ontology:cs_axiom_grounding('d7ef8035-b774-496d-b8db-1606abc57a85', muhammadan_prophetic_succession_continuous_with_abraham, empirically_contingent).
narrative_ontology:cs_reference_frame('d7ef8035-b774-496d-b8db-1606abc57a85', abrahamic_covenant_as_inclusive_of_ishmael_line).
narrative_ontology:cs_drift_state('d7ef8035-b774-496d-b8db-1606abc57a85', contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d7ef8035-b774-496d-b8db-1606abc57a85', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_community).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, muhammadan_prophetic_lineage).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, jewish_exclusive_interpretation_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, christian_supersessionist_stakeholders).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, prophetic_succession_continuity).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, abrahamic_lineage_inclusivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Global Muslim believers whose religious identity and theological understanding of Islam is anchored in the covenant claim: that Islam continues Abraham's prophetic mission through Muhammad. This reading provides theological legitimacy, prophetic continuity, and answers the question 'how does Islam relate to Judaism and Christianity?' Muslims experience the Ishmael reading as foundational to their faith identity — it is taught in mosques, embedded in hadith collections, and central to Islamic law and devotional practice. Exiting this reading would mean abandoning a core identity claim that structures how they understand themselves in relation to the Abrahamic traditions.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, islamic_community, beneficiary,
    organized, civilizational, identity_locked, global).

% The Islamic scholarly and jurisprudential tradition — hadith masters (muhaddithun), Quranic exegetes (mufassirun), legal scholars (fuqaha), and theological schools (madhabs) — that explicitly maintains, teaches, and enforces the Ishmael covenant reading. These authorities authenticate hadith supporting prophetic continuity, write tafsir commentaries interpreting Quranic engagement with Abraham, and develop legal doctrines anchored in the covenant claim. They have institutional power to determine what counts as authoritative Islamic knowledge. The institution's identity is fused with the reading: to abandon it would be to admit that Islamic foundational theology rests on misinterpretation or fabrication — an existential threat to the institution's legitimacy.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, muhammadan_prophetic_lineage, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Jewish scholars and communities committed to the interpretation that Genesis 17:19-21 limits the covenant exclusively to Isaac's line. They experience the Ishmael reading as a direct challenge to Jewish covenantal particularity and chosenness. Their theological response argues that the Quranic claim to Abraham is a later reinterpretation unsupported by the original Hebrew text. They pay a cost: their interpretive monopoly on the Abrahamic covenant is contested; they must defend their reading against the Islamic alternative; and they experience institutional authority (Islamic jurisprudential tradition) making authoritative claims that contradict their theological claims. Accepting the Ishmael reading would mean abandoning Jewish particularity — the understanding that the covenant is a special relationship binding Abraham's descendants (through Isaac) to God. Exit is not available.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, jewish_exclusive_interpretation_holders, payer,
    moderate, civilizational, identity_locked, global).

% Christian communities historically claiming that the Church has superseded Israel as the true heir to the Abrahamic covenant. The Ishmael reading threatens this by offering an alternative supersession: Islam, not Christianity, is the continuation of Abraham's prophetic mission. Classical supersessionism is undermined. Some Christian denominations have moved toward more inclusivist readings (Catholic 'Nostra Aetate,' Protestant interfaith movements) that accept Judaism and Islam as separate covenantal communities; the Ishmael reading both challenges and supports such moves. The cost they bear is theological destabilization: if Islam is the Abrahamic continuation, what becomes of Christian claims? They are excluded from the institutional authority structures (Islamic jurisprudential councils) that adjudicate the covenant reading.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, christian_supersessionist_stakeholders, payer,
    moderate, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__ishmael_covenant_reading, christian_supersessionist_stakeholders, excluded).

% Scholars (Jewish, Christian, Muslim, secular) who examine the textual basis for both Isaac-exclusive and Ishmael-inclusive readings. They analyze the Hebrew of Genesis 17:19-21, the Quranic interpretation of Abraham (2:130-140, 3:95-97), hadith collections claiming prophetic continuity, and the exegetical traditions that developed each reading. They assess historical questions (when did each reading develop, who authored it, what textual evidence supports it) and theological questions (are the readings logically compatible, which is more coherent). They do not control the outcome — institutional authorities (Islamic hadith councils, Jewish interpretive authorities) decide what counts as valid interpretation. But they document the structural facts: that both readings are textually defensible, that the Ishmael reading was developed for theological reasons (establishing Islamic legitimacy), and that the reading is actively enforced through institutional authority.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, academic_biblical_scholars, observer,
    analytical, generational, analytical, global).

% Communities (Jewish, Christian, Muslim participants) engaged in interfaith dialogue seeking common ground in shared Abrahamic heritage. They would like to affirm both Jewish particularity and Islamic prophetic succession without forcing a zero-sum competition: 'Abraham is ancestor to both Jews and Muslims; both traditions carry his legacy.' They are excluded from the institutional authorities that adjudicate the readings — they operate in dialogue circles, universities, and civil-society spaces, not in Islamic jurisprudential councils or Jewish rabbinic seminaries. Their constrained exit: staying in dialogue means acknowledging both communities' claims without resolving the contradiction; leaving means abandoning the interfaith commitment itself.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, interfaith_dialogue_practitioners, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__ishmael_covenant_reading, muhammadan_prophetic_lineage).
narrative_ontology:fixing_cost_class(abrahamic_covenant__ishmael_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified genealogy and covenantal family spanning Abraham, Isaac, Ishmael, Moses, Jesus, and Muhammad. Solves the theological coordination problem of how different Abrahamic communities relate to a shared source and how Islam positions itself within the religious history of Abraham. Without this reading, Islam would be external to the covenant (a later faith, superseding or independent), creating permanent theological exile. With the reading, Islam is positioned as the continuation and restoration of Abraham's original monotheistic faith.
% TRANSFER_FUNCTION: Moves theological legitimacy, interpretive authority, and covenantal inclusion from a Jewish-exclusive framework to an inclusive Abrahamic framework that centers Islamic prophetic succession. The transfer is specifically the covenantal claim: the Ishmael reading asserts that the promise of descendants and blessing passes through Ishmael to Muhammad, displacing the Jewish claim to exclusive transmission through Isaac. Additionally, it redistributes interpretive authority from Jewish textual tradition to Islamic exegetical and jurisprudential tradition — Islamic scholars become authoritative interpreters of Abraham's covenant, not Jewish scholars.
% ABSENT_VOICES: Jewish scholars who hold the Isaac-exclusive reading are not seated in the institutional authorities that adjudicate the Ishmael reading — they are contesting interpreters, not decision-makers in Islamic hadith councils or tafsir schools. Christian supersessionists are similarly excluded from formal Islamic authority. Interfaith practitioners who would advocate for a non-zero-sum both/and reading are not seated in the seminaries or jurisprudential councils; they operate in dialogue spaces. Historical Ishmael (the person) and the diverse Ishmaelite tribes of Arabia cannot testify to how they understood the covenant. Subaltern Muslims without access to elite hadith and tafsir scholarship experience this reading as enforced institutional doctrine, not as freely chosen interpretation.
% DISAPPEARANCE_RATIONALE: If the Ishmael covenant reading and its institutional enforcement vanished overnight, Islamic theology would lose its primary claim to Abrahamic continuity and would require radical reformulation around non-covenantal grounds (maybe prophetic universalism, maybe rejection of Abrahamic ancestry entirely). Muslim identity would shift from 'heirs of Abraham's covenant' to 'later monotheistic faith.' Jewish-Muslim theological relationship would become purely oppositional (no shared covenant to dispute, only competing imperial claims). Interfaith dialogue would lose the Abrahamic framework entirely. Some Islamic jurisprudential structures (those anchored in hadith about Muhammad's status as 'seal of prophets' continuing Abraham's mission) would need doctrinal reconstruction.
% FOUNDING_PROBLEM: Early Islamic communities, establishing themselves in the 7th-9th centuries in conversation with Christian and Jewish theological traditions, faced the theological question: how is Muhammad related to Abraham and the earlier prophets? The covenant reading answers: Muhammad is the completion of Abraham's prophetic mission; through Ishmael (Abraham's excluded son in Jewish tradition), the covenant passes to Muhammad and his followers. This solves the problem of Islamic legitimacy and continuity with prior revelation.
% FOUNDING_PROBLEM_CORROBORATION: Islamic hadith collections (Sahih Bukhari, Sahih Muslim, Sunan Ibn Majah) contain hadiths explicitly claiming Muhammad's prophetic continuity with Abraham. Tafsir traditions (al-Tabari, Ibn Kathir, al-Zamakhshari) develop detailed exegetical arguments for Ishmael's inclusion in the covenant and Muhammad's status. Islamic legal schools ground jurisprudential reasoning in this covenant understanding. Jewish scholars (rabbinic commentaries, Talmudic sources) recognize the theological problem Islamic tradition faced and document it as a source of Jewish-Muslim theological dispute. Christian scholars document Islamic claims to Abrahamic succession in early polemical literature. Academic biblical scholars (Wellhausen, Noeldeke, modern Quranic studies) confirm that the Ishmael covenant reading is a live theological claim in early Islamic tradition, though they debate its textual basis. The corroboration is abundant but contested: Islamic scholars and communities affirm the claim; Jewish and Christian scholars deny its textual validity while acknowledging it as a real theological move.
narrative_ontology:disappearance_verdict(abrahamic_covenant__ishmael_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__ishmael_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__ishmael_covenant_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__ishmael_covenant_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__ishmael_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__ishmael_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__ishmael_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.58 at interval end) reflects that the Ishmael reading redistributes theological legitimacy away from Jewish exclusivity toward Islamic tradition. The redistribution is not zero-sum neutrally — the Islamic institutional authority actively enforces the reading through teaching, hadith certification, and legal doctrine, which constitutes extraction from those (Jewish scholars, interfaith practitioners) who contest it. Suppression is moderate-high (0.62) because the reading's persistence depends on institutional enforcement: it is not universally accepted or freely chosen but is maintained through authority structures. Theater ratio is low-moderate (0.28) because the reading does have genuine theological content (it provides prophetic continuity), but a growing share of its enforcement is performative maintenance — rehearsal in liturgy and education that reinforces identity without engaging with the counter-reading. The measurement series shows extractiveness and suppression rising gradually from 600 to 1600 CE as the reading became institutionalized in Islamic jurisprudence, then stabilizing from 1600 to 2024 as it became embedded in teaching and cultural practice. Theater ratio remains lower (less theatrical performance relative to substantive function) because the reading continues to solve a real theological coordination problem.
 *
 * PERSPECTIVAL GAP:
 *   The Islamic institutional seat (muhammadan_prophetic_lineage) should compute as beneficiary experiencing genuine coordination: they maintain a reading that solves a real theological problem (how Islam relates to Abraham) and that provides identity continuity and legitimacy. From the Jewish exclusive-interpretation seat, the same reading computes as extractive and imposed: their particular covenant claim is diluted, their textual monopoly is challenged, and they experience enforcement through institutional authority they do not control. From the academic observer seat, the structure is asymmetric: Islamic authority adjudicates the reading (institutional power), Jewish scholars counter-interpret but lack equal authority in Islamic exegetical councils, and the reading persists despite scholarly counter-evidence because it solves an identity problem for the Muslim community. The engine computes these seat differences from the structural data: identity-locked exit for Muslims (leaving the reading means abandoning Islamic identity), identity-locked exit for Jews (accepting it means abandoning Jewish particularity), and analytical exit for scholars (they observe but do not decide the institutional outcome).
 *
 * DIRECTIONALITY LOGIC:
 *   The Islamic community and prophetic institution are structural beneficiaries: the reading provides them theological legitimacy and identity continuity. The directionality for these seats is low (near the beneficiary end, d ~ 0.2-0.3) because they collect the legitimacy gain without bearing enforcement costs — the enforcement infrastructure is part of their institutional identity. Jewish exclusive-interpretation holders are structural targets of extraction: they bear the cost of a contested covenant claim, lose exclusive legitimacy, and experience the reading as imposed by institutional authority they do not control. Their directionality is high (d ~ 0.75-0.85) because they bear costs and have constrained exit (identity_locked: accepting the inclusive reading means abandoning Jewish particularity). Interfaith practitioners are excluded: they experience the reading as constraining dialogue possibilities (locked into either/or rather than both/and), but they are not seated in the formal authority structure. Academic observers have analytical exit (they can observe multiple readings without adopting any), so d is symmetric (d ~ 0.5) — they experience no extraction because they are not bound by the institutional outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is CLAIMED as tangled_rope: it solves a genuine coordination problem (how Islamic tradition relates to Abraham and prophetic history) while extracting from Jewish tradition (diluting their exclusive covenant claim). Mandatrophy resolves by confirming this classification: the reading is NOT a mountain (natural law) — it is a constructed interpretation with clear beneficiaries and payers. It is NOT pure snare (extraction without coordination) — the Islamic institutional authority genuinely maintains it to solve a theological problem, not merely to dominate. It IS tangled rope: the coordination function is real (establishing prophetic continuity), but the asymmetric extraction is also real (Jewish particularity is constrained). The active enforcement is structural: Islamic jurisprudential authority enforces the reading through teaching, hadith authentication, and legal doctrine. The mandatrophy test passes: this is not a zombie constraint (losing function but persisting by inertia). The reading remains functionally live because it solves a real theological coordination problem for the Islamic community.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genesis_17_19_21_scope_ambiguity,
    'Does Genesis 17:19-21 (''I will establish my covenant with Isaac'') logically exclude Ishmael from the covenant, or does it specify a particular line of transmission while leaving the broader covenant open?',
    'Close exegetical analysis of the Hebrew grammar, the literary context (Genesis 16-17 extends blessing to Ishmael separately), and rabbinic and Islamic exegetical traditions'' treatment of the passage. Scholars assess whether ''my covenant'' is singular (one covenant with exclusive transmission) or plural/layered (one covenant with multiple transmission lines).',
    'If the passage is read as exclusive, the Ishmael reading is exegetically indefensible and represents pure ideological reinterpretation. If the passage is read as specifying Isaac''s line while not foreclosing Ishmael''s blessing, the Ishmael reading becomes textually defensible and the two readings coexist on different grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genesis_17_19_21_scope_ambiguity, empirical, 'The textual scope of Genesis 17:19-21 — does it exclude Ishmael or only specify Isaac''s line?').

omega_variable(
    quran_2_130_140_covenant_referent,
    'When the Quran (2:130-140) claims Abraham''s followers as Muslim believers and rejects the notion that Abraham was Jewish or Christian, is it claiming Abraham''s original covenant, constructing a new Islamic covenant, or both?',
    'Quranic exegesis, hadith analysis of Muhammad''s claims to Abrahamic continuity, and Islamic legal reasoning about prophetic authority. Scholars assess whether Islamic tradition understands the Quranic claim as recovery/restoration of Abraham''s original covenant or as supersession by a new Muhammadan covenant.',
    'If the claim is restoration/continuity, the Ishmael reading is a coherent interpretation of the shared kernel (same covenant, inclusive reading). If the claim is supersession, the Ishmael reading is ideologically useful but textually secondary — Islamic tradition is making a new claim under the cover of Abrahamic ancestry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quran_2_130_140_covenant_referent, conceptual, 'Whether Islamic Quranic claims to Abraham represent covenant continuity or supersession.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of alternative Ishmael readings (e.g., readings that deny Islamic covenantal status) structural (enforced through institutional authority, educational gatekeeping, textual certification) or internalized (believers internalize the reading as identity, making alternatives psychologically unavailable)?',
    'Historical and contemporary analysis of how the reading is transmitted: enforcement machinery (hadith authentication, tafsir canon formation, madrasah curricula) versus believer adoption (do Muslims hold the reading because they internalize it or because they are taught it as the authoritative interpretation?). Post-exit interviews or historical accounts of Muslims who abandon the reading would clarify whether suppression persists after institutional exit.',
    'If structural, the constraint''s measured suppression (0.62) reflects institutional enforcement and exit options are more open than the identity_locked designation suggests. If internalized, believers carry the suppression with them after institutional exit — the constraint''s true suppression is higher than measured, and identity_lock is deeply rooted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of the reading is structural or internalized in Muslim identity.').

omega_variable(
    interfaith_alternative_reading_possibility,
    'Could a ''both/and'' reading exist that honors both Jewish particularity (Isaac''s covenant as a particularistic call) and Islamic inclusivity (Ishmael''s covenant as an alternative path), without forcing zero-sum competition?',
    'Theological and philosophical analysis: are the two readings logically incompatible, or do they operate on different levels (ethnic particularity vs. universal monotheism)? Interfaith dialogue practitioners'' lived experience of managing both claims simultaneously.',
    'If incompatible, the reading is genuinely extractive from Jewish particularity. If compatible, the extraction is partly ideological (choice to frame as zero-sum) rather than structural, and the constraint could be reframed as coordination without extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interfaith_alternative_reading_possibility, preference, 'Whether a non-zero-sum inclusive theological reading is possible.').

omega_variable(
    kernel_reading_vs_new_covenant,
    'Is this constraint a genuine READING of the shared Abrahamic covenant kernel, or is it a new covenant claim (Islamic prophetic succession) that borrows Abrahamic language for legitimacy?',
    'Analysis of whether the Ishmael reading explicates the existing Genesis text (reading) or introduces new theological content not present in the kernel (new doctrine). Does the reading follow exegetically from the kernel''s language, or does it require substantial extra-textual addition?',
    'If a genuine reading, the constraint is clarifying an ambiguity in the shared kernel. If a new covenant, the constraint is an interpretive move that strategically deploys Abraham-language to establish Islamic legitimacy — the extraction is from the Jewish tradition''s monopoly on covenant interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_vs_new_covenant, conceptual, 'Whether this constraint represents a reading of the shared kernel or a new Islamic covenantal claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__ishmael_covenant_reading, 600, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t600, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 600, 0.05).
narrative_ontology:measurement_basis(abra_tr_t600, projected).
narrative_ontology:measurement(abra_tr_t900, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 900, 0.08).
narrative_ontology:measurement_basis(abra_tr_t900, projected).
narrative_ontology:measurement(abra_tr_t1200, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1200, 0.12).
narrative_ontology:measurement_basis(abra_tr_t1200, projected).
narrative_ontology:measurement(abra_tr_t1600, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1600, 0.22).
narrative_ontology:measurement_basis(abra_tr_t1600, observed).
narrative_ontology:measurement(abra_tr_t1900, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1900, 0.27).
narrative_ontology:measurement_basis(abra_tr_t1900, observed).
narrative_ontology:measurement(abra_tr_t2024, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(abra_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(abra_be_t600, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 600, 0.35).
narrative_ontology:measurement_basis(abra_be_t600, projected).
narrative_ontology:measurement(abra_be_t900, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 900, 0.42).
narrative_ontology:measurement_basis(abra_be_t900, projected).
narrative_ontology:measurement(abra_be_t1200, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1200, 0.48).
narrative_ontology:measurement_basis(abra_be_t1200, projected).
narrative_ontology:measurement(abra_be_t1600, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1600, 0.55).
narrative_ontology:measurement_basis(abra_be_t1600, observed).
narrative_ontology:measurement(abra_be_t1900, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1900, 0.58).
narrative_ontology:measurement_basis(abra_be_t1900, observed).
narrative_ontology:measurement(abra_be_t2024, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(abra_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t600, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 600, 0.45).
narrative_ontology:measurement_basis(abra_su_t600, projected).
narrative_ontology:measurement(abra_su_t900, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 900, 0.5).
narrative_ontology:measurement_basis(abra_su_t900, projected).
narrative_ontology:measurement(abra_su_t1200, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1200, 0.55).
narrative_ontology:measurement_basis(abra_su_t1200, projected).
narrative_ontology:measurement(abra_su_t1600, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1600, 0.61).
narrative_ontology:measurement_basis(abra_su_t1600, observed).
narrative_ontology:measurement(abra_su_t1900, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1900, 0.62).
narrative_ontology:measurement_basis(abra_su_t1900, observed).
narrative_ontology:measurement(abra_su_t2024, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 2024, 0.62).
narrative_ontology:measurement_basis(abra_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__ishmael_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__ishmael_covenant_reading, 0.12).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant__isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant__land_promise_constraint).

% DUAL FORMULATION NOTE:
% The Abrahamic covenant kernel has at least three structurally distinct constraint readings: (1) isaac_covenant_reading (exclusive transmission through Isaac), (2) ishmael_covenant_reading (inclusive transmission through Ishmael to Muhammad), and (3) land_promise_constraint (territorial grant dimensions orthogonal to lineage). Each reading instantiates a different constraint with different epsilon values: Isaac-exclusive has low epsilon (natural-law-like inevitability for Jewish tradition), Ishmael-inclusive has moderate-high epsilon (contested, institutionally enforced, extractive from Jewish particularity), and land-promise has very high epsilon (violently contested in contemporary Israeli-Palestinian conflict). The readings do NOT coexist peacefully — they form a constraint family linked by affects_constraints. The epsilon difference reflects that the Ishmael reading's extraction from Jewish interpretive monopoly is the central feature, whereas Isaac-exclusive reading asserts natural-law-like textual clarity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__ishmael_covenant_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
