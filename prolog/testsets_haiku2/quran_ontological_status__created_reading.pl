% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__created_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__created_reading, []).

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
 *   constraint_id: quran_ontological_status__created_reading
 *   human_readable: Qur'an as Created Divine Speech (Makhlūq)
 *   domain: theological/philosophical
 *
 * SUMMARY:
 *   The Qur'an's ontological status—whether it is created (makhlūq) or
 *   uncreated (qadīm)—has been a constitutive fault line in Islamic theology
 *   for over a thousand years. This constraint story instantiates the
 *   created-reading: the view that the Qur'an is a created divine artifact,
 *   that God's absolute transcendence (tawḥīd) requires that no text or
 *   attribute be coeternal with God, and that this ontological status
 *   licenses rational, contextual interpretation of the text. The
 *   created-reading benefits rationalist theologians, reform movements, and
 *   philosophical schools; it imposes costs on traditionalist jurists and
 *   literalist communities whose jurisprudential and devotional authority
 *   depends on treating the Qur'an as uncreated, eternally fixed, and
 *   directly divine speech. The constraint operates as rope (a genuine
 *   coordination solution to the metaphysical problem of reconciling divine
 *   transcendence with historical revelation) from the beneficiary seats, but
 *   as enforced extraction (hermeneutic authority seized from
 *   traditionalists) from the payer seats. The claim/metric divergence is
 *   intentional: the beneficiary-seat framing (rope) represents the
 *   created-reading's own legitimating narrative, while the authored metrics
 *   capture the asymmetric extraction structure that observation reveals.
 *
 * KEY AGENTS:
 *   - Rationalist theologians (Mu'tazilites, later Ash'arites, philosophical schools): primary beneficiaries; intellectually mobile; derive authority from the created-reading's framework
 *   - Traditionalist jurists (Ḥanbalī, conservative Mālikī, literalist Shāfi'ī): primary payers; identity-locked to textual fixity; institutional authority undermined by hermeneutic pluralism
 *   - Literalist communities (fundamentalist movements, literalist exegetes): payers; constrained exit; their devotional identity fused with the uncreated-reading
 *   - Reform movements (modernist scholars, social reformers): beneficiaries; institutionally organized; use the created-reading to license contextual jurisprudence
 *   - Philosophical schools (Aristotelian-trained theologians, logic-based scholars): beneficiaries; geographically mobile; intellectual practice depends on tawḥīd defense
 *   - State authority (Abbasid caliphate during mihna, Ottoman sultanate, modern Islamic states): agenda-setter; determines which reading is officially taught and enforced; institutional exit is locked to legitimacy claims
 *   - Uncreated-reading defenders: excluded by definition from this constraint; their presence would reframe the entire dispute
 *   - Textual interpretation community (tafsīr scholars, exegetes): observers; document the practical consequences of each reading without adjudicating metaphysics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.62).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.41).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Qur'an as Created Divine Speech (Makhlūq)").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "theological/philosophical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, '16a7c405-db72-4331-bd01-cc6ab0c1cd7a').
narrative_ontology:cs_kernel_codification('16a7c405-db72-4331-bd01-cc6ab0c1cd7a', formalized).
narrative_ontology:cs_authority_grounding('16a7c405-db72-4331-bd01-cc6ab0c1cd7a', lineage).
narrative_ontology:cs_interpretation_layer_present('16a7c405-db72-4331-bd01-cc6ab0c1cd7a').
narrative_ontology:cs_reading_relation('16a7c405-db72-4331-bd01-cc6ab0c1cd7a', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('16a7c405-db72-4331-bd01-cc6ab0c1cd7a', quran_ontological_status__state_enforced_creation_reading, coexists_with).
narrative_ontology:cs_axiom('16a7c405-db72-4331-bd01-cc6ab0c1cd7a', foundational, divine_transcendence_requires_no_coeternal).
narrative_ontology:cs_axiom_status(divine_transcendence_requires_no_coeternal, holdable).
narrative_ontology:cs_axiom_grounding('16a7c405-db72-4331-bd01-cc6ab0c1cd7a', divine_transcendence_requires_no_coeternal, deontological).
narrative_ontology:cs_axiom('16a7c405-db72-4331-bd01-cc6ab0c1cd7a', foundational, rational_interpretation_licensed_by_createdness).
narrative_ontology:cs_axiom_status(rational_interpretation_licensed_by_createdness, holdable).
narrative_ontology:cs_axiom_grounding('16a7c405-db72-4331-bd01-cc6ab0c1cd7a', rational_interpretation_licensed_by_createdness, instrumental).
narrative_ontology:cs_reference_frame('16a7c405-db72-4331-bd01-cc6ab0c1cd7a', divine_transcendence_absolute_oneness).
narrative_ontology:cs_drift_state('16a7c405-db72-4331-bd01-cc6ab0c1cd7a', contemporary_post_traditionalist_revival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('16a7c405-db72-4331-bd01-cc6ab0c1cd7a', '2026-06-12T14:23:45Z').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, reform_movements).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, philosophical_schools).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars trained in philosophical theology who argue God's transcendence requires rejecting any coeternal text. They benefit from the created-reading because it legitimizes their hermeneutic methods and intellectual practice. Their exit is mobile: they can migrate between courts, schools, regions based on patronage and intellectual opportunity. They are organized (they form coherent theological traditions like Mu'tazilism and later Ash'arism) and their power derives from institutional positions and intellectual prestige.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, rationalist_theologians, beneficiary,
    organized, generational, mobile, continental).

% Traditionalist scholars and communities whose jurisprudence and piety rest on the uncreated-reading. They bear the cost of the created-reading through institutional marginalization and delegitimization of their textual methods. Their exit is constrained by the pervasiveness of the created-reading's institutional framework (they cannot simply abandon Islamic theology without abandoning their tradition). They are powerful institutionally (traditionalist jurisprudential schools are entrenched) but their power is reactive—they defend against philosophical challenge rather than setting the intellectual agenda.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_communities, payer,
    powerful, generational, constrained, continental).

% Legal scholars whose jurisprudential methodology assumes the Qur'an's textual fixity and uncreated status. They bear costs through methodological destabilization: if the text is created, their reasoning procedures (qiyās, analogy; ijmāʿ, consensus; literalist tafsīr) lose their metaphysical ground. Their exit is identity-locked: their entire intellectual identity and institutional role depends on defending traditionalist jurisprudence. They have powerful institutional positions (Ḥanbalī schools, conservative Mālikī communities) but are excluded from setting the agenda of theological discussion—the rationalist establishment determines which metaphysical framework is officially legitimate.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    powerful, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__created_reading, traditionalist_jurists, excluded).

% Modernist and reformist scholars seeking to reconcile Islamic jurisprudence with contemporary contexts. They benefit directly from the created-reading because it licenses contextual reinterpretation of revelation. Their hermeneutic agenda (adapting law to new circumstances) becomes theologically warranted under the created-reading, whereas it faced resistance under the uncreated-reading's insistence on textual fixity. Their exit is mobile: they operate across jurisdictions and intellectual communities, seeking institutional support wherever available.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, reform_movements, beneficiary,
    organized, generational, mobile, continental).

% Aristotelian-trained and logic-based theological schools (Mu'tazilites, later Ash'arites, Ibn Sina's tradition) who benefit from the created-reading's metaphysical coherence. The framework confirms their philosophical commitment to tawḥīd (absolute oneness) and enables their intellectual practice. Their exit is mobile: they migrate between caliphal courts, religious schools, and intellectual centers based on patronage and the vitality of the philosophical tradition. They are organized into coherent schools of thought and their power derives from intellectual prestige and institutional positions in elite educational networks.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, philosophical_schools, beneficiary,
    organized, generational, mobile, global).

% Political authority (caliphate, sultanate, or modern Islamic state) that shapes which reading becomes officially taught and enforced. The state's agenda-setting capacity derives from its institutional monopoly on patronage, appointment of judges and scholars, control of educational institutions, and capacity for persecution of dissenting voices. The state's exit is trapped: abandoning all theological frameworks would undermine the state's legitimacy claims. The state uses the created-reading (or the uncreated-reading, depending on period) to rationalize its authority and control over religious scholars.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, state_authority, agenda_setter,
    institutional, generational, trapped, continental).

% Tafsīr scholars, exegetes, and hermeneutic specialists who parse the practical consequences of ontological claims about the Qur'an without adjudicating the metaphysics. They observe and document how the created-reading licenses different exegetical methods than the uncreated-reading, and they remain institutionally available to both readings' communities. Their exit is analytical: they can shift their analytical focus based on intellectual development without changing their institutional role.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, textual_interpretation_community, observer,
    organized, generational, analytical, continental).

% Those who institutionally defend the sibling uncreated-reading and would argue that the created-reading misunderstands the kernel itself. They are excluded from this constraint's framing by definition: this constraint IS the created-reading's operation, not the dispute itself. If they were given voice to define this constraint, they would argue that it is a category error—that the created-reading conflates God's transcendence with textual createdness in a way that falsifies both the kernel and the metaphysical problem. Their exclusion is structural: the created-reading's internal logic excludes its sibling by foreclosure, not by institutional suppression alone.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, uncreated_reading_institutional_defenders, excluded,
    powerful, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:fixing_cost_class(quran_ontological_status__created_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a coherent metaphysical framework reconciling God's absolute transcendence (tawḥīd) with the historical particularity of revelation. Solves the problem: how can God be strictly one if an eternal, uncreated text coexists with God's essence? By classifying the Qur'an as created, the framework preserves divine uniqueness and permits rational theological interpretation. This is a genuine coordination solution that resolves a real metaphysical tension in Islamic doctrine.
% TRANSFER_FUNCTION: Moves hermeneutic authority from literalist-traditionalist scholars (who claim meaning is fixed by the text's eternal status) to rationalist-philosophical scholars (who claim meaning is accessible through rational interpretation of a created artifact). The cost is borne by traditionalist jurists and literalist communities whose institutional authority and identity claims depend on the uncreated-reading.
% ABSENT_VOICES: Institutional defenders of the sibling uncreated-reading are systematically excluded by the created-reading's logical framework—they cannot speak within the created-reading's terms without contradicting themselves. Pre-Islamic philosophical traditions (Neoplatonism, Aristotelian logic outside Islamic adaptation) might provide external critical leverage, but they are absent from the Islamic theological conversation. Ordinary believers whose piety is devotional rather than metaphysical are affected but absent: they depend on whichever ontological reading their community has adopted, but they do not participate in the theological debate.
% DISAPPEARANCE_RATIONALE: If the created-reading disappeared and the uncreated-reading became universally restored: rationalist theology would face a metaphysical crisis (tawḥīd defense would require different philosophical moves); jurisprudential methodology would stabilize around textual fixity; reform movements would lose theological warrant for contextual interpretation; philosophical schools would need to reformulate their coherence claims. But communities already operating under the uncreated-reading would continue their practices largely unchanged. The world would rearrange substantially for beneficiary seats and minimally for traditionalist seats—hence contested rather than world_unchanged or world_rearranges.
% FOUNDING_PROBLEM: How to reconcile God's absolute oneness and transcendence (tawḥīd) with the reality that revelation is expressed in human language, at a particular historical moment, with particular addressees? The uncreated-reading claims this is no problem: the text's divine status and God's transcendence coexist as separate attributes. The created-reading insists tawḥīd logically requires denying any coeternal with God—including revelation—such that revelation must be understood as a created artifact of divine will, which preserves God's transcendence and makes the text's particularity theologically intelligible.
% FOUNDING_PROBLEM_CORROBORATION: Rationalist theologians and philosophical schools affirm the founding problem is live and urgent: tawḥīd requires philosophical rigor, and the created-reading provides it. Traditionalist jurists and literalist communities contest the problem itself: they argue the Qur'an's divine status and God's transcendence are metaphysically compatible and do not require the created-reading's solution. Modern scholars outside the benefiting parties (secular historians of Islamic thought, comparative theologians) document that the created-reading historically emerged as an attempt to solve this problem, but they do not adjudicate whether the problem is real or whether the solution works. No external authority corroborates the problem as solved or permanently present—the verdict remains contested across the relevant scholarly communities.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, contested).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__created_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__created_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__created_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__created_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.62 at interval end) reflects that the created-reading transfers hermeneutic authority from one institutionalized group (traditionalist jurists controlling textual transmission chains) to another (rationalist theologians and philosophers). It is not pure extraction—there is real coordination benefit (the framework solves a genuine metaphysical problem)—but the transfer is asymmetric and imposed against the resistance of those who lose authority. The trajectory from 0.35 to 0.62 over 1200 time units reflects the historical pattern: the created-reading emerged gradually in Mu'tazilite theology (early Abbasid period), intensified during the mihna (state enforcement), then plateaued as traditionalist counter-movements (Ash'arite synthesis, Hanbalī revival) absorbed some of the created-reading's insights while restoring traditionalist authority. The theater_ratio (rising from 0.12 to 0.28) indicates that over time, more of the created-reading's operation shifted from genuine coordination function to performative maintenance of the rational-theology establishment—later philosophers defending it spent less time on the foundational metaphysical argument and more time rehearsing its institutional legitimacy. Suppression (0.41, stable across the interval) captures that the created-reading requires active institutional pressure to prevent literalist and traditionalist re-ascendance, but this suppression does not rise to the level of snare-grade coercion because the created-reading does solve a real problem that rationalist communities genuinely prefer—they are not trapped by external force alone, but by commitment to the reading's intellectual coherence. Accessibility_collapse (0.68) reflects that once someone understands the created-reading's metaphysical framework, the alternatives collapse substantially: one is forced to choose between affirming tawḥīd (which the created-reading frames as requiring creation) or accepting a coeternal with God (which feels like compromise). Resistance (0.71) is high because traditionalist and literalist communities have mounted sustained intellectual and institutional pushback: they have not accepted the created-reading's premises despite centuries of rationalist argument.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (rationalist theologians, philosophical schools), the created-reading is genuine rope: it solves the tawḥīd problem and enables coherent metaphysical theology. They experience it as enabling intellectual freedom and rigorous argument. From the payer seat (traditionalist jurists, literalist communities), the same constraint operates as snare: their authority is delegitimized, their textual methodology is displaced, and they experience the created-reading as philosophical colonization of the revelation. The engine computes this divergence from directionality: beneficiaries sit near d=0.0 (subsidized by the hermeneutic flexibility the reading licenses), payers sit near d=0.9 (extracted from via authority displacement). The state authority seat (agenda-setter) occupies a distinct position: the created-reading is a tool the state can wield to rationalize its authority (by claiming to enforce philosophical truth) or to subordinate religious scholars (by forcing them into rational justification). The state's exit is identity-locked because maintaining legitimacy requires some coherent framework for relating revelation to governance, and the created-reading offers one such framework. Excluded parties (defenders of the uncreated-reading) would compute the constraint entirely differently: they would argue the created-reading is not rope at all but a misconstruction of the kernel itself—that it mistakes the problem it purports to solve.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality (d near 0.0 for rationalist theologians, reform movements, philosophical schools): These groups benefit directly and structurally from the created-reading's operation. Their hermeneutic authority increases, their interpretive methods gain theological legitimacy, their institutional position is strengthened. Their exit options are mobile (they can advocate for the reading or abandon it; they have alternative intellectual frameworks if it falls away). Their power is organized (they form coherent theological schools) or institutional (they hold positions in caliphal courts, universities). They experience the created-reading as enabling, not constraining. Payer directionality (d near 1.0 for traditionalist jurists, literalist communities): These groups bear substantial costs. Their jurisprudential authority is undermined by the permission for contextual reinterpretation. Their identity claims (unmediated divine speech, textual fixity) are negated by the created-reading's framework. Their exit is identity-locked: they cannot abandon the uncreated-reading without reconstructing their entire theological self-understanding. Their power is powerful (Hanbalī jurisprudence is institutionally entrenched) but constrained by the need to defend against philosophical challenge. They experience the created-reading as coercive, not enabling. State authority directionality (d varies by period): When the state enforces the created-reading (Abbasid mihna, some reform periods), the state derives benefit and d approaches 0.3 (moderate beneficiary). When the state patronizes traditionalist readings, d shifts toward 0.7 (moderate payer). The state's position is institutionally powerful but temporally unstable—state enforcement of philosophical doctrine is historically episodic. Observers (textual interpretation community) sit at d=0.5: they are affected by whichever reading becomes dominant (it shapes interpretive methodology), but they have no direct stake in the metaphysical outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatrophic in its current form. The founding problem (how to reconcile God's transcendence with revelation's particularity) remains live and contested. Traditionalist and literalist communities dispute that the problem requires the created-reading's solution and argue that the uncreated-reading solves the same problem via different metaphysical moves (affirming both transcendence and revelation's divine status). However, there is a potential mandatrophy trajectory: if institutional pressure (state patronage, philosophical establishment) maintains the created-reading long enough, and if succeeding generations of literalist and traditionalist scholars absorb the philosophical criticism of their position without recovering a robust counter-argument, the founding problem could become technically dead (no living community actively defends the uncreated-reading's full metaphysical case) while the created-reading persists as institutionalized theology. At that point, the constraint would transition to piton: maintained theatrically by a philosophical establishment no longer actively defending the founding problem, but kept in place by institutional inertia. The measurement series shows theater_ratio rising from 0.12 to 0.30, suggesting some movement in this direction—more recent periods show the created-reading functioning more as philosophically-certified orthodoxy than as a live contested position. But the resistance measurement (0.71, stable) and the survival of traditionalist jurisprudential schools indicates the founding problem is still live: the constraint has not yet entered mandatrophy because the alternative reading remains institutionally and intellectually defended.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_transcendence_vs_textual_creation,
    'Is God''s absolute transcendence (tawḥīd) metaphysically compatible with an eternally uncreated text, or does affirming tawḥīd necessarily require classifying the Qur''an as created?',
    'Systematic comparison of Ash''arite, Maturidi, and Traditionalist metaphysical frameworks: do all three affirm God''s transcendence and differ only on whether it REQUIRES creation, or do they fundamentally disagree about what transcendence entails? Philosophical analysis of whether coexistence with an eternal effect contradicts God''s oneness or is merely a minority position within Islamic metaphysics.',
    'If tawḥīd does NOT require creation (i.e., coexistence with an eternal text is metaphysically coherent), then the created-reading loses its primary philosophical warrant and becomes one optional interpretation rather than a necessary defense of transcendence. The constraint would reclassify toward snare (pure extraction of hermeneutic authority from traditionalists without genuine coordination benefit). If tawḥīd DOES require creation, the created-reading is metaphysically grounded regardless of institutional politics and the rope classification is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_transcendence_vs_textual_creation, conceptual, 'Whether divine transcendence logically entails textual createdness or permits coexistent eternality.').

omega_variable(
    rational_interpretation_vs_textual_fixity,
    'Is the permission for rational, contextual interpretation (ijtihād) structurally dependent on the created-reading, or can traditionalist jurisprudence also license contextual reading within the uncreated-reading framework?',
    'Comparative jurisprudential history: document historical instances where traditionalist schools (Ḥanbalī, conservative Mālikī, literalist Shāfi''ī) performed contextual reasoning despite affirming the uncreated-reading. Examine whether the uncreated-reading actually constrains interpretive method or whether both readings permit methodological diversity.',
    'If contextual interpretation can proceed equally well under either reading, the created-reading loses its claim to enable flexibility and becomes instead a marker of philosophical allegiance rather than a practical necessity. If contextual interpretation is systematically blocked by the uncreated-reading''s textual fixity, the created-reading''s benefit to reformists is real and structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_interpretation_vs_textual_fixity, empirical, 'Whether interpretive method causally depends on ontological status of the text.').

omega_variable(
    kernel_framing_under_determination,
    'Is the kernel adequately framed as a dispute about the Qur''an''s ontological status (created vs. uncreated), or does the real kernel concern God''s relationship to time, the status of divine attributes, and the metaphysics of causation—with the Qur''an''s status being a downstream inference rather than the primary commitment?',
    'Genealogical tracing: do philosophers defending the created-reading argue primarily from Qur''anic ontology or from prior commitments about God''s causality and temporal transcendence? Would a philosopher defend the created-reading even if the Qur''an''s status were not at stake (e.g., in debates about divine attributes, creation ex nihilo, or prophecy)?',
    'If the kernel is properly located at God''s relationship to time and causation, then the created-reading''s true competitive advantage over the uncreated-reading is narrower than advertised—it solves a metaphysical problem about causation, not uniquely about revelation. This would shift the reading''s scope and possibly require decomposition into separate constraints (one for the causation-metaphysics kernel, one for the textual-status kernel).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the kernel is correctly identified as textual-ontology dispute or should be reframed as metaphysical-causation dispute.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of the literalist and traditionalist reading (suppression = 0.41) primarily structural (institutional mechanisms like state persecution, funding denial, book-burning) or partially internalized (literalists have absorbed the philosophical argument and doubt their own position), or primarily institutional?',
    'Post-institutional-pressure trajectory analysis: in periods and regions where state patronage of the created-reading ceased (e.g., post-Mu''tazilite retreat, later Ottoman periods where traditionalist readings were favored), did literalist and traditionalist communities revive and claim confidence in their position, or did philosophical residue persist and attenuate their confidence? If literalists regain institutional voice and authority when pressure lifts, suppression is primarily structural; if doubt persists despite institutional restoration, suppression is partially internalized.',
    'If suppression is primarily structural, removing institutional support for the created-reading would rapidly restore traditionalist authority and the constraint would reclassify toward lower-suppression categories. If suppression is internalized, the created-reading has contaminated the traditionalists'' own epistemic confidence—they would need philosophical counterargument, not just institutional reversal, to recover their position. This affects whether the constraint is truly rope (coordination) or snare (enforced extraction with internalized compliance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Degree to which suppression of literalist reading is institutional coercion versus internalized doubt.').

omega_variable(
    committer_reading_relations_foreclosure,
    'Do the created-reading and uncreated-reading logically foreclose each other (no single theological framework can hold both), or do they coexist (different parties maintain them in parallel without logical incompatibility)?',
    'Formal logical analysis: Does the created-reading''s core axiom (divine_transcendence_requires_no_coeternal) logically contradict the uncreated-reading''s core axiom (revelation is coeternal divine speech)? Can a single coherent theological system affirm both, or do they require mutually exclusive metaphysical starting points? Historical analysis: have any major Islamic schools synthesized both readings, or have they remained institutionally and doctrinally distinct?',
    'If they foreclose: the reading that becomes institutionally dominant will tend to eliminate the other from live discourse (foreclosure is rare and marks genuine metaphysical incompatibility). If they coexist: both will persist as live positions across different parties and schools despite institutional competition. The relation type determines whether the constraint operates as an elimination mechanism (snare + foreclosure) or as an ongoing negotiation (rope or tangled_rope with coexistence).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_relations_foreclosure, conceptual, 'Logical relationship (foreclosure vs. coexistence) between created and uncreated readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t150, quran_ontological_status__created_reading, theater_ratio, 150, 0.15).
narrative_ontology:measurement(qura_tr_t300, quran_ontological_status__created_reading, theater_ratio, 300, 0.21).
narrative_ontology:measurement(qura_tr_t600, quran_ontological_status__created_reading, theater_ratio, 600, 0.28).
narrative_ontology:measurement(qura_tr_t900, quran_ontological_status__created_reading, theater_ratio, 900, 0.3).
narrative_ontology:measurement(qura_tr_t1200, quran_ontological_status__created_reading, theater_ratio, 1200, 0.28).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qura_be_t150, quran_ontological_status__created_reading, base_extractiveness, 150, 0.48).
narrative_ontology:measurement(qura_be_t300, quran_ontological_status__created_reading, base_extractiveness, 300, 0.58).
narrative_ontology:measurement(qura_be_t600, quran_ontological_status__created_reading, base_extractiveness, 600, 0.62).
narrative_ontology:measurement(qura_be_t900, quran_ontological_status__created_reading, base_extractiveness, 900, 0.63).
narrative_ontology:measurement(qura_be_t1200, quran_ontological_status__created_reading, base_extractiveness, 1200, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__created_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(qura_su_t150, quran_ontological_status__created_reading, suppression_requirement, 150, 0.32).
narrative_ontology:measurement(qura_su_t300, quran_ontological_status__created_reading, suppression_requirement, 300, 0.38).
narrative_ontology:measurement(qura_su_t600, quran_ontological_status__created_reading, suppression_requirement, 600, 0.41).
narrative_ontology:measurement(qura_su_t900, quran_ontological_status__created_reading, suppression_requirement, 900, 0.42).
narrative_ontology:measurement(qura_su_t1200, quran_ontological_status__created_reading, suppression_requirement, 1200, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__created_reading, 0.12).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__state_enforced_creation_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, islamic_jurisprudential_authority).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, rational_theology_legitimacy).

% DUAL FORMULATION NOTE:
% The quran_ontological_status kernel decomposes into at least three structurally distinct constraints: (1) created-reading (this file) — the Qur'an is created, God transcends all text, rationalist theology is licensed. (2) uncreated-reading — the Qur'an is uncreated, coeternal with divine attributes, textual fixity is metaphysically grounded. (3) state-enforced-creation-reading — same ontological claim as created-reading but with state-enforcement machinery as a constitutive element of the constraint. These are linked because the sibling readings share a kernel (Qur'an's ontological status) but generate different constraint classifications from different institutional seats and presuppositions. ε-invariance is maintained: each reading's ε is measured against the standing arrangement IT describes, not against a neutral referent. The created-reading's ε (0.62) describes extraction from those who lose hermeneutic authority; the uncreated-reading's ε (calculated in its own file) describes extraction from those denied rational interpretive methods; the state-enforced-reading's ε describes extraction via state coercion of philosophical doctrine. The readings are not observational variations on one constraint—they are separate constraints linked by kernel kinship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
