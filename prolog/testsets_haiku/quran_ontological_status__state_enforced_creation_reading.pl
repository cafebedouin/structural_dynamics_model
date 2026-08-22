% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__state_enforced_creation_reading, []).

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
 *   constraint_id: quran_ontological_status__state_enforced_creation_reading
 *   human_readable: State-Enforced Mu'tazilite Doctrine: Qur'an as Created (mihna inquisition)
 *   domain: Islamic theology / political authority / philosophy of language
 *
 * SUMMARY:
 *   Early Abbasid caliphate (8th–9th centuries CE): A theological
 *   metaphysical claim — the Qur'an is created divine speech (makhlūq), not
 *   uncreated coeternal with God — becomes state doctrine under rationalist
 *   (Mu'tazilite) patronage. The Caliph Ma'mun (r. 813–833) enforces this via
 *   inquisition (mihna) tribunals that demand public affirmation of the
 *   created-Qur'an doctrine and punish traditionalist scholars who refuse.
 *   The constraint is not the theological dispute itself (coexisting
 *   doctrines are not extraction mechanisms), but the STATE ENFORCEMENT of
 *   one doctrine using inquisitorial suppression as the tool. This reading
 *   instantiates the dispute as a snare: state power converts metaphysical
 *   claim into a suppression mechanism; the created-Qur'an doctrine becomes
 *   the justification for purging traditionalist scholarship; victims include
 *   Ahmad ibn Hanbal (imprisoned, tortured) and literalist scholarly
 *   communities. This is one reading of the kernel 'quran_ontological_status'
 *   — the kernel is the contested claim about the Qur'an's ontic status; this
 *   reading adds state enforcement as the defining structural feature,
 *   transforming an intellectual disagreement into an extraction regime.
 *
 * KEY AGENTS:
 *   - caliphal_authority (Caliph Ma'mun, successor caliphs employing mihna): agenda-setter, institutional power — controls inquisition tribunals, defines orthodoxy, enforces public affirmation. Exit: none — state authority is the ultimate arbiter.
 *   - rationalist_scholars (Mu'tazilite theologians): temporary beneficiaries (state backing, institutional patronage during mihna) — their doctrine is adopted as official; power: institutional (during state backing), moderate otherwise. Exit: constrained during mihna (publish under state approval or face tribunal).
 *   - traditionalist_scholars (Ahmad ibn Hanbal, Hanbalite school): primary victims — refuse to affirm created-Qur'an doctrine, face imprisonment, torture, career destruction. Power: initially organized (scholarly schools with transmission chains), degraded to powerless under mihna suppression. Exit: identity-locked (scholarly identity tied to transmitted doctrine; apostasy from Tradition means loss of scholarly standing and lineage).
 *   - literalist_communities (ordinary believers, traditional jurisprudents): victims — expected to affirm state doctrine, face legal penalties for refusal. Power: organized as communities, but subject to state enforcement. Exit: constrained (formal apostasy rare; practical exit is conforming to state doctrine or silent dissent).
 *   - scholarly_pluralism (the institutional practice of coexisting madhhabs): victim entity (not an agent) — mihna suppresses the legitimacy of multiple competing theological schools, enforcing single doctrine. Recovery contingent on post-mihna political realignment.
 *   - inquisition_tribunals (mihna magistrates, qadis instructed by caliph): mechanism enforcers — not independent agents but instruments of state power. Power: delegated institutional (acting on caliphal authority).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.82).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.91).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "State-Enforced Mu'tazilite Doctrine: Qur'an as Created (mihna inquisition)").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "Islamic theology / political authority / philosophy of language").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, '27c6968f-1632-4b05-8f0f-ac52dd6945bc').
narrative_ontology:cs_kernel_codification('27c6968f-1632-4b05-8f0f-ac52dd6945bc', fixed_text).
narrative_ontology:cs_authority_grounding('27c6968f-1632-4b05-8f0f-ac52dd6945bc', extraction).
narrative_ontology:cs_interpretation_layer_present('27c6968f-1632-4b05-8f0f-ac52dd6945bc').
narrative_ontology:cs_reading_relation('27c6968f-1632-4b05-8f0f-ac52dd6945bc', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('27c6968f-1632-4b05-8f0f-ac52dd6945bc', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_axiom('27c6968f-1632-4b05-8f0f-ac52dd6945bc', foundational, qur_an_is_created_divine_speech).
narrative_ontology:cs_axiom_status(qur_an_is_created_divine_speech, holdable).
narrative_ontology:cs_axiom_grounding('27c6968f-1632-4b05-8f0f-ac52dd6945bc', qur_an_is_created_divine_speech, empirically_contingent).
narrative_ontology:cs_axiom('27c6968f-1632-4b05-8f0f-ac52dd6945bc', foundational, state_enforces_single_theological_orthodoxy).
narrative_ontology:cs_axiom_status(state_enforces_single_theological_orthodoxy, overridden).
narrative_ontology:cs_axiom_grounding('27c6968f-1632-4b05-8f0f-ac52dd6945bc', state_enforces_single_theological_orthodoxy, instrumental).
narrative_ontology:cs_reference_frame('27c6968f-1632-4b05-8f0f-ac52dd6945bc', unified_imperial_ideology_through_rationalist_doctrine).
narrative_ontology:cs_drift_state('27c6968f-1632-4b05-8f0f-ac52dd6945bc', post_mutawakkil_pluralism_restoration, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('27c6968f-1632-4b05-8f0f-ac52dd6945bc', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, rationalist_scholars_temporary).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_communities).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, rationalist_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, rationalist_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Caliph (Ma'mun and successor caliphs during mihna) sets inquisition policy, defines orthodoxy, appoints inquisition magistrates, and enforces public affirmation of created-Qur'an doctrine. Justifies the inquisition as protecting true faith and rational understanding. Collects the benefit of unified theological ideology and scholarly loyalty to caliphal authority. Could exit by abandoning mihna policy entirely, but chooses to maintain enforcement to consolidate ideological control.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, caliphal_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Mu'tazilite theologians benefit from caliphal patronage and institutional backing during mihna — their doctrine becomes state orthodoxy, they gain scholarly prestige and access to caliphal resources. But they also pay a cost: they are dependent on state backing for legitimacy (if state switches support, they lose standing); they must publicly defend the created-Qur'an doctrine under inquisition scrutiny even if private doubts arise; their philosophical independence is compromised by institutional loyalty requirements. Exit options: constrained because leaving state-backed doctrine means losing institutional standing and patronage.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, rationalist_scholars, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__state_enforced_creation_reading, rationalist_scholars, payer).

% Hanbalite and traditionalist scholars refuse to affirm created-Qur'an doctrine, citing transmitted precedent and literal revelation. Face inquisition tribunals, imprisonment (Ahmad ibn Hanbal), torture, career destruction, confiscation of writings. Identity-locked: their scholarly identity is constituted through transmitted tradition; affirming created-Qur'an doctrine means breaking with lineage and ceasing to be a Hanbalite scholar. Exit is apostasy-grade (loss of scholarly standing, community alienation). Resistance is substantial but futile against state coercion: Ahmad ibn Hanbal's public defiance was legendary and led to torture; many others chose silence or underground transmission.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars, payer,
    organized, biographical, identity_locked, national).

% Ordinary believers and jurisprudents who understood the Qur'an as uncreated eternal divine speech, following traditional piety. Mihna enforces public affirmation of created-Qur'an doctrine. Cost: violation of conscience (forced to affirm doctrine they reject spiritually), legal penalties for non-compliance (fines, exile, career barriers for judges and officials). Exit: constrained but not identity-locked — unlike scholars, they can conform to state doctrine without losing non-scholarly roles, though spiritual cost is high. Organized resistance is possible but risky; most conform outwardly while maintaining private belief.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, literalist_communities, payer,
    organized, biographical, constrained, national).

% The institutional machinery of mihna tribunals, magistrates (qadis), and enforcement procedures — not an agent but a structural artifact maintained by caliphal authority to conduct inquisitions. Its existence and operation are entirely dependent on caliphal will. Listed for analytical completeness; the functional agent is caliphal_authority.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, inquisition_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(quran_ontological_status__state_enforced_creation_reading, inquisition_apparatus).

% The institutional practice of coexisting theological schools (madhhabs) and doctrinal pluralism. Mihna actively suppresses this by enforcing single-doctrine orthodoxy and delegitimizing competing schools. Listed as excluded (not in the conversation) because the mihna is defined by excluding pluralistic discourse; pluralism would have objected to forced uniformity but was structurally unable to voice objection (no seat at inquisition tribunal). Post-mihna, pluralism recovered as a valued principle of Islamic jurisprudence.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism, excluded,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).

% Traditionalist scholars who maintained Hanbalite transmission chains and scriptural scholarship in hidden circles during mihna, avoiding official tribunals. Operated under severe risk; discoveries meant inquisition referral. Analytical role: their persistence demonstrates that suppression is not total — some resistance survives, though suppressed. Post-mihna, these underground networks became the institutional basis for Hanbalite ascendancy.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, underground_transmission, observer,
    moderate, generational, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:fixing_cost_class(quran_ontological_status__state_enforced_creation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Abbasid caliphate sought unified theological ideology to consolidate imperial authority and prevent sectarian fragmentation. A single enforced doctrine (created-Qur'an) was intended to unify scholars under rational, coherent metaphysics. The coordination problem: how to achieve doctrinal unity across a vast, theologically diverse empire?
% TRANSFER_FUNCTION: Transfers scholarly autonomy and doctrinal legitimacy from traditionalist schools to rationalist schools (Mu'tazilite), and transfers authority to define orthodoxy from decentralized scholarly consensus to centralized caliphal decree. Money moves from caliphal treasury to rationalist scholars (patronage, salaries). Punishment and coercion move from caliphs to traditionalist scholars (imprisonment, torture, career destruction).
% ABSENT_VOICES: Scholars and believers committed to uncreated-Qur'an doctrine are excluded from mihna tribunals — their voice is only heard through suppression and forced testimony under threat. They would argue that theological diversity is a feature of Islamic tradition, not a disease; that forced orthodoxy corrupts spiritual authenticity; that the Qur'an's uncreated status is a foundation of piety and cannot be rationalized away. Also absent: ordinary believers whose spiritual conscience conflicts with state doctrine but who lack institutional voice to object formally.
% DISAPPEARANCE_RATIONALE: If mihna enforcement vanished overnight, traditionalist scholarship would resurface immediately (many scholars only went silent, not ceased existing). Hanbalite and literalist doctrine would regain legitimacy. Multiple coexisting theological schools would return to normalcy. The scholarly ecosystem would reorganize around decentralized consensus instead of caliphal decree. In fact, this is what happened: Caliph al-Mutawakkil ended mihna enforcement (c. 847 CE), and traditionalist schools — especially Hanbalite — emerged as ascendant, eventually dominating Sunni orthodoxy.
% FOUNDING_PROBLEM: Theological doctrinal unity: the Abbasid caliphate faced a fragmented scholarly landscape with competing schools of interpretation. Rationalist (Mu'tazilite) and traditionalist (literalist, Hanbalite) scholars disagreed fundamentally on the Qur'an's ontological status and the role of reason in theology. The caliphate interpreted this as a threat to imperial coherence: an empire needs unified ideology to prevent sectarian rebellion. The created-Qur'an doctrine was selected as the rational, universally defensible position that could unify the educated elite.
% FOUNDING_PROBLEM_CORROBORATION: Mu'tazilite and caliphal authorities attest the problem is live: theological confusion threatens faith and imperial stability (attested in theological writings and court decrees). Traditionalist scholars attest the problem is dead or illusory: theological diversity is a healthy feature of Islamic jurisprudence; forced uniformity creates ossification and spiritual death. Independent corroboration from later Islamic history: post-mihna (after al-Mutawakkil ended enforcement), the Islamic scholarly tradition evolved to embrace BOTH created and uncreated positions through doctrinal pluralism in the four Sunni madhhabs, with the Hanbalite school (which championed uncreated doctrine) becoming ascendant and broadly respected. This suggests the founding problem (need for unified doctrine) was not inherent but was a temporary political choice; pluralism recovered and became institutionalized. The legacy: no Islamic school today enforces the created-Qur'an doctrine through state inquisition; pluralism is the norm.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_ontological_status__state_enforced_creation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__state_enforced_creation_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__state_enforced_creation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__state_enforced_creation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82): The constraint extracts scholarly autonomy and doctrinal legitimacy from traditionalist scholars and transfers both to the rationalist school (temporarily) and caliphal authority (permanently — the ability to define orthodoxy). The extraction is not reciprocal: caliphs and rationalist scholars gain institutional standing; traditionalists lose career, freedom, and intellectual legacy (many works destroyed or suppressed). The constraint is structured extraction because alternatives (coexisting schools, decentralized theological authority) are actively suppressed, not naturally obsolete. Suppression (0.91): Very high because the constraint's persistence depends entirely on inquisition coercion — tribunals, torture, imprisonment, career expulsion. Without active enforcement, traditionalist scholarship would resurface immediately (as it did post-mihna). Theater ratio (0.58): Moderate-high because mihna rhetoric emphasizes doctrinal purity and rational correctness, but much of the actual activity serves political consolidation (eliminating rival authority claims, securing scholarly loyalty to caliphal ideology). The theological arguments are real, but they are deployed theatrically to justify institutional purge. Accessibility collapse (0.78): High because traditionalist scholars face near-total elimination of the alternative of non-compliance — public dissent means tribunal, imprisonment, torture; silent non-compliance means loss of scholarly voice; the only accessible exit is affirmation. Resistance (0.72): Substantial resistance from traditionalist scholars and communities (Ahmad ibn Hanbal's defiance was legendary; Hanbalite transmission chains persisted underground), but resistance is suppressed violently rather than accommodated. Leveled coercion grid: Individual-level suppression is highest at t20 (0.93) because inquisition hits individual scholars directly (Ahmad ibn Hanbal tortured); organizational-level suppression is also very high (rationalist schools gain state backing, traditionalist schools are purged). Structural-level coercion is sustained throughout (caliphal authority defines orthodoxy by law). Stakes inflation rises over the interval because the cost of non-compliance escalates from social shunning (t0) to imprisonment and torture (t10–15). Resistance peaks in the middle interval (t10–15) during the height of enforcement but then declines (t20) as traditionalists either convert, flee, or accept underground silence.
 *
 * PERSPECTIVAL GAP:
 *   The caliphal authority and rationalist scholars would experience this constraint very differently. From the caliph's seat: the constraint is a necessary unification of theological doctrine to prevent societal fragmentation, justified as rational clarification of revealed truth (a coordination function). From the traditionalist scholar's seat: the constraint is pure extraction — state power applied to eliminate competing authority claims and force doctrinal loyalty. From the literalist community's seat: the constraint is suppression of spiritual autonomy and religious conscience (identity-locked victims forced to affirm a doctrine that contradicts their lived understanding of revelation). The engine should compute high effective extraction (χ) for the traditionalist seats and low/negative χ (subsidy) for the rationalist seats during mihna, reflecting the asymmetric structural relationship. The leveled coercion grid shows this: organizational suppression (0.93) targets the traditionalist scholarly hierarchy directly, while rationalist scholars face no organizational suppression (they are the organized arm of state authority).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (caliphal_authority, rationalist_scholars_temporary): Both have directionality near the beneficiary end (low d). Caliphal authority collects the benefit of doctrinal monopoly indefinitely (d near 0.1–0.2: powerful, stable, controlling the mechanism, no exit). Rationalist scholars benefit from state backing temporarily but face reversal risk if political winds shift — their d sits at 0.3–0.4 (institutional power under state patronage, but contingent). Victims (traditionalist_scholars, literalist_communities): Both have directionality near the target end (high d). Traditionalist scholars face identity-locked exit (their identity IS their scholarly tradition; leaving the tradition means ceasing to be a scholar in that lineage) — d near 0.85–0.95 (trapped, high stakes, powerful suppression, no acceptable exit). Literalist communities face constrained exit (conforming to state doctrine is enforced, but organized mass resistance is possible, especially post-mihna) — d near 0.75–0.85. The scholarly_pluralism victim (not an agent) does not have a directionality value; it is a structural loss rather than a seat-based extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Founding problem (R5): The founding problem is theological doctrinal clarity combined with political consolidation — the Abbasid caliphate sought unified ideology to stabilize a vast empire. Founding problem status: CONTESTED. Mu'tazilites attest it is live (doctrinal confusion threatens faith). Traditionalists attest it is dead — the problem of theological diversity is not a disease but a healthy feature of Islamic jurisprudence; forced uniformity creates the disease (intellectual ossification, spiritual deadness). Disappearance verdict: WORLD_REARRANGES. The constraint is not a natural law; it is an institutional choice. If mihna enforcement vanished (as it did under Caliph al-Mutawakkil ~847 CE), the scholarly ecosystem would immediately reorganize: traditionalist schools resurface, multiple theological positions coexist again, caliphal monopoly over doctrine dissolves. The Hanbalite school that Ahmad ibn Hanbal founded emerged stronger post-mihna, suggesting the constraint's disappearance triggered reorganization in favor of suppressed constituencies. Mandatrophy signal: High theater ratio (0.58) + divergence between founding problem (doctrinal clarity) and actual operation (career suppression) + post-mihna reversal (Hanbalite ascendancy) suggests the founding problem is dead and the constraint persists as institutional inertia and caliphal-rationalist alliance maintenance. The constraint should flag mandatrophy_resolved = true: the consolidation was achieved (empire unified under rationalist doctrine for ~30 years), but the remedy (mihna enforcement) outlived its purpose and eventually caused institutional breakdown (loss of scholarly legitimacy, sectarian alienation, caliphal authority erosion).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creation_doctrine_vs_political_tool,
    'Is the created-Qur''an doctrine genuinely motivated by theological metaphysics, or is state enforcement converting it into a pure political instrument for doctrinal purge?',
    'Historical analysis of rational-school scholars'' writings before and after caliphal backing. If theological arguments appear before state power, the doctrine carries intrinsic motivation; if arguments intensify only under state enforcement, the doctrine is a post-hoc justification. Comparative analysis with competing theological systems that emerged under different state regimes.',
    'If politically driven, the constraint''s classification remains snare and extraction rises further (the doctrine itself becomes a suppression cover). If theologically motivated, the classification might shift toward tangled_rope (genuine coordination of metaphysical understanding + asymmetric political enforcement), but the mihna mechanism keeps it snare-flavored regardless.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creation_doctrine_vs_political_tool, empirical, 'Whether the created-Qur''an doctrine''s persistence depends on intrinsic theological coherence or on state coercion.').

omega_variable(
    kernel_reading_foreclosure_ambiguity,
    'Does the state-enforced creation reading foreclose the uncreated reading, or do both remain live positions despite mihna suppression?',
    'Post-mihna scholarly ecology: if uncreated-doctrine scholarship persists and is eventually rehabilitated (Ahmad ibn Hanbal vindicated posthumously), the readings coexist despite enforcement. If uncreated doctrine is permanently erased or driven underground without recovery, the enforced creation reading forecloses it in institutional memory even if not in logical necessity.',
    'Forecloses → the engine''s cross-index_coupling protocol flags the enforcement as total doctrinal suppression, not disagreement management. Coexists_with → the mihna is a faction battle with recovery possible, not an irreversible kernel rewrite. The reading_relations declaration in cs_structure directly models this uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_ambiguity, empirical, 'Whether state-enforced creation reading forecloses the uncreated reading or merely suppresses it temporarily.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the measured suppression of traditionalist scholars structural (inquisition torture, career expulsion) or does it persist internalized in the post-mihna scholarly habitus?',
    'Post-mihna trajectory: if traditionalist scholars resume writing freely once state enforcement relaxes (post-Ma''mun caliphate), suppression is primarily structural. If scholars remain silent or self-censor even after legal barriers drop, suppression is partially internalized. Documentary evidence from 9th–10th century manuscripts and biographical records.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — scholars carry the suppression with them after exit from direct state coercion. Classification stays snare either way, but internalization indicates deeper identity-fusion with doctrinal conformity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Suppression mechanism: structural enforcement vs. internalized doctrinal conformity.').

omega_variable(
    scholarly_pluralism_recovery,
    'Is ''scholarly pluralism'' (listed as a victim) a recoverable institutional practice, or does state-enforced doctrine entrench itself permanently?',
    'Post-mihna institutional evolution: if multiple theological schools eventually coexist in formal acceptance (Sunni madhhab pluralism, post-Abbasid decline), pluralism recovered. If one doctrine ossifies as institutional orthodoxy (creedal councils, madrasa curricula locked into single doctrine), it does not. Long-term institutional ecology over 2–3 centuries.',
    'If pluralism recovers, the snare is temporary-institutional (scaffold-like, but enforced rather than consensual). If it stays locked, the victim status indicates permanent institutional capture. Either way, the constraint''s type remains snare; the recovery timeline affects mandatrophy analysis — is the founding problem (establishing orthodoxy) still live, or is it dead yet the enforcement persists?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scholarly_pluralism_recovery, empirical, 'Whether state-enforced theological orthodoxy permits or prevents restoration of scholarly pluralism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(qura_tr_t0, projected).
narrative_ontology:measurement(qura_tr_t5, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement_basis(qura_tr_t5, observed).
narrative_ontology:measurement(qura_tr_t10, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement_basis(qura_tr_t10, observed).
narrative_ontology:measurement(qura_tr_t15, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 15, 0.62).
narrative_ontology:measurement_basis(qura_tr_t15, observed).
narrative_ontology:measurement(qura_tr_t20, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 20, 0.51).
narrative_ontology:measurement_basis(qura_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(qura_be_t0, projected).
narrative_ontology:measurement(qura_be_t5, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement_basis(qura_be_t5, observed).
narrative_ontology:measurement(qura_be_t10, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement_basis(qura_be_t10, observed).
narrative_ontology:measurement(qura_be_t15, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 15, 0.81).
narrative_ontology:measurement_basis(qura_be_t15, observed).
narrative_ontology:measurement(qura_be_t20, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement_basis(qura_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(qura_su_t0, projected).
narrative_ontology:measurement(qura_su_t5, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 5, 0.82).
narrative_ontology:measurement_basis(qura_su_t5, observed).
narrative_ontology:measurement(qura_su_t10, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 10, 0.91).
narrative_ontology:measurement_basis(qura_su_t10, observed).
narrative_ontology:measurement(qura_su_t15, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 15, 0.89).
narrative_ontology:measurement_basis(qura_su_t15, observed).
narrative_ontology:measurement(qura_su_t20, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(qura_su_t20, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=20
narrative_ontology:measurement(qura_grid_01, quran_ontological_status__state_enforced_creation_reading, accessibility_collapse(class), 0, 0.55).
narrative_ontology:measurement(qura_grid_02, quran_ontological_status__state_enforced_creation_reading, accessibility_collapse(class), 20, 0.68).
narrative_ontology:measurement(qura_grid_03, quran_ontological_status__state_enforced_creation_reading, accessibility_collapse(individual), 0, 0.62).
narrative_ontology:measurement(qura_grid_04, quran_ontological_status__state_enforced_creation_reading, accessibility_collapse(individual), 20, 0.75).
narrative_ontology:measurement(qura_grid_05, quran_ontological_status__state_enforced_creation_reading, accessibility_collapse(organizational), 0, 0.68).
narrative_ontology:measurement(qura_grid_06, quran_ontological_status__state_enforced_creation_reading, accessibility_collapse(organizational), 20, 0.82).
narrative_ontology:measurement(qura_grid_07, quran_ontological_status__state_enforced_creation_reading, accessibility_collapse(structural), 0, 0.78).
narrative_ontology:measurement(qura_grid_08, quran_ontological_status__state_enforced_creation_reading, accessibility_collapse(structural), 20, 0.85).
narrative_ontology:measurement(qura_grid_09, quran_ontological_status__state_enforced_creation_reading, resistance(class), 0, 0.68).
narrative_ontology:measurement(qura_grid_10, quran_ontological_status__state_enforced_creation_reading, resistance(class), 20, 0.75).
narrative_ontology:measurement(qura_grid_11, quran_ontological_status__state_enforced_creation_reading, resistance(individual), 0, 0.58).
narrative_ontology:measurement(qura_grid_12, quran_ontological_status__state_enforced_creation_reading, resistance(individual), 20, 0.62).
narrative_ontology:measurement(qura_grid_13, quran_ontological_status__state_enforced_creation_reading, resistance(organizational), 0, 0.72).
narrative_ontology:measurement(qura_grid_14, quran_ontological_status__state_enforced_creation_reading, resistance(organizational), 20, 0.78).
narrative_ontology:measurement(qura_grid_15, quran_ontological_status__state_enforced_creation_reading, resistance(structural), 0, 0.62).
narrative_ontology:measurement(qura_grid_16, quran_ontological_status__state_enforced_creation_reading, resistance(structural), 20, 0.68).
narrative_ontology:measurement(qura_grid_17, quran_ontological_status__state_enforced_creation_reading, stakes_inflation(class), 0, 0.72).
narrative_ontology:measurement(qura_grid_18, quran_ontological_status__state_enforced_creation_reading, stakes_inflation(class), 20, 0.85).
narrative_ontology:measurement(qura_grid_19, quran_ontological_status__state_enforced_creation_reading, stakes_inflation(individual), 0, 0.58).
narrative_ontology:measurement(qura_grid_20, quran_ontological_status__state_enforced_creation_reading, stakes_inflation(individual), 20, 0.88).
narrative_ontology:measurement(qura_grid_21, quran_ontological_status__state_enforced_creation_reading, stakes_inflation(organizational), 0, 0.65).
narrative_ontology:measurement(qura_grid_22, quran_ontological_status__state_enforced_creation_reading, stakes_inflation(organizational), 20, 0.92).
narrative_ontology:measurement(qura_grid_23, quran_ontological_status__state_enforced_creation_reading, stakes_inflation(structural), 0, 0.68).
narrative_ontology:measurement(qura_grid_24, quran_ontological_status__state_enforced_creation_reading, stakes_inflation(structural), 20, 0.78).
narrative_ontology:measurement(qura_grid_25, quran_ontological_status__state_enforced_creation_reading, suppression(class), 0, 0.68).
narrative_ontology:measurement(qura_grid_26, quran_ontological_status__state_enforced_creation_reading, suppression(class), 20, 0.82).
narrative_ontology:measurement(qura_grid_27, quran_ontological_status__state_enforced_creation_reading, suppression(individual), 0, 0.62).
narrative_ontology:measurement(qura_grid_28, quran_ontological_status__state_enforced_creation_reading, suppression(individual), 20, 0.85).
narrative_ontology:measurement(qura_grid_29, quran_ontological_status__state_enforced_creation_reading, suppression(organizational), 0, 0.75).
narrative_ontology:measurement(qura_grid_30, quran_ontological_status__state_enforced_creation_reading, suppression(organizational), 20, 0.93).
narrative_ontology:measurement(qura_grid_31, quran_ontological_status__state_enforced_creation_reading, suppression(structural), 0, 0.81).
narrative_ontology:measurement(qura_grid_32, quran_ontological_status__state_enforced_creation_reading, suppression(structural), 20, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__uncreated_reading).

% DUAL FORMULATION NOTE:
% The kernel 'quran_ontological_status' decomposes into THREE constraints, each instantiating a reading: (1) created_reading — Mu'tazilite theological assertion, primarily intellectual; (2) state_enforced_creation_reading — THIS constraint — the mihna inquisition converting the theological claim into a suppression regime; (3) uncreated_reading — traditionalist/Hanbalite assertion, also primarily intellectual until later periods when it becomes state-backed. ε-invariance disciplines: created_reading (theological claim only) has low ε because coexisting schools are not extraction mechanisms. state_enforced_creation_reading (THIS) has high ε because state power extracts doctrinal compliance and scholarly loyalty. uncreated_reading (post-mihna traditionalist ascendancy) has low ε when pluralistic, high ε if state-enforced. Each constraint is a separate story; links trace political/doctrinal influence. This reading is upstream to created_reading (state enforcement backs Mu'tazilite doctrine), and influences the later uncreated_reading (state enforcement of traditionalist doctrine was a reaction to and reversal of state-enforced creation doctrine).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_ontological_status__state_enforced_creation_reading, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
