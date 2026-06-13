% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__created_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_ontological_status__created_reading
 *   human_readable: Qur'an as Created Divine Speech (Makhlūq Reading)
 *   domain: islamic_theology/philosophy_of_language/political_authority
 *
 * SUMMARY:
 *   The Qur'an's ontological status—whether it is created (makhlūq) or
 *   uncreated (qadīm)—constitutes one of the most consequential metaphysical
 *   disputes in Islamic intellectual history. This constraint story
 *   instantiates the created reading: the position that God's absolute
 *   transcendence requires locating the Qur'an as a created divine speech—a
 *   temporal artifact that communicates divine will without compromising
 *   God's ontic independence from time and creation. This reading preserves
 *   divine transcendence, enables rational theology, grants hermeneutic
 *   flexibility to reform movements and philosophical schools, but displaces
 *   traditionalist jurists whose authority rested on textual fixity. The
 *   created reading is NOT the natural or default Islamic position; it is a
 *   deliberate theological commitment that benefits specific intellectual
 *   constituencies while extracting from others—a rope coordinate by rational
 *   deliberation, not a mountain of received doctrine.
 *
 * KEY AGENTS:
 *   - Rationalist theologians (Mu'tazilites, philosophical schools): beneficiaries; gain hermeneutic authority to interpret the Qur'an through reason; powerful, institutional position, mobile exit within intellectual markets.
 *   - Reform movements: beneficiaries; gain theological cover for interpretive innovation and social policy adaptation; organized power, generational horizon.
 *   - Traditionalist jurists (hadith scholars, textual fixity schools): victims; lose certainty of literal meaning; their authority structure depends on unmediated textual access; institutional power, constrained exit.
 *   - Literalist communities: victims; identity locked in direct divine speech access; moderate power, biographical horizon, trapped exit due to identity fusion with literal Qur'an.
 *   - Political authorities: agenda-setters; can invoke or suppress this reading strategically; institutional power, arbitrage exit.
 *   - Uncreated-reading defenders: excluded; their core premise is logically foreclosed by this reading's framework; cannot participate in rational deliberation within the created-reading structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.62).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.45).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Qur'an as Created Divine Speech (Makhlūq Reading)").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "islamic_theology/philosophy_of_language/political_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, '7e9a75aa-8b7f-457e-859a-155d613e49e0').
narrative_ontology:cs_kernel_codification('7e9a75aa-8b7f-457e-859a-155d613e49e0', fixed_text).
narrative_ontology:cs_authority_grounding('7e9a75aa-8b7f-457e-859a-155d613e49e0', lineage).
narrative_ontology:cs_interpretation_layer_present('7e9a75aa-8b7f-457e-859a-155d613e49e0').
narrative_ontology:cs_reading_relation('7e9a75aa-8b7f-457e-859a-155d613e49e0', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('7e9a75aa-8b7f-457e-859a-155d613e49e0', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('7e9a75aa-8b7f-457e-859a-155d613e49e0', foundational, divine_transcendence_incompatible_with_eternal_text).
narrative_ontology:cs_axiom_status(divine_transcendence_incompatible_with_eternal_text, holdable).
narrative_ontology:cs_axiom_grounding('7e9a75aa-8b7f-457e-859a-155d613e49e0', divine_transcendence_incompatible_with_eternal_text, deontological).
narrative_ontology:cs_axiom('7e9a75aa-8b7f-457e-859a-155d613e49e0', foundational, rational_theology_legitimate_hermeneutic_authority).
narrative_ontology:cs_axiom_status(rational_theology_legitimate_hermeneutic_authority, holdable).
narrative_ontology:cs_axiom_grounding('7e9a75aa-8b7f-457e-859a-155d613e49e0', rational_theology_legitimate_hermeneutic_authority, instrumental).
narrative_ontology:cs_reference_frame('7e9a75aa-8b7f-457e-859a-155d613e49e0', rational_preservation_of_transcendence).
narrative_ontology:cs_drift_state('7e9a75aa-8b7f-457e-859a-155d613e49e0', post_abbasid_political_collapse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7e9a75aa-8b7f-457e-859a-155d613e49e0', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, reform_movements).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, philosophical_schools).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_communities).
narrative_ontology:constraint_vindicates(quran_ontological_status__created_reading, divine_transcendence_doctrine).
narrative_ontology:constraint_vindicates(quran_ontological_status__created_reading, rational_hermeneutics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theological scholars operating within Mu'tazilite and philosophical traditions gain hermeneutic authority to interpret the Qur'an through rational principles. This reading grants them intellectual standing to mediate between divine transcendence and textual meaning, allowing systematic theology and philosophical reasoning to shape Islamic doctrine. They benefit from the opening of textual interpretation to reasoned analysis.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, rationalist_theologians, beneficiary,
    institutional, generational, arbitrage, regional).

% Social and political reform groups find this reading's interpretive flexibility valuable: if the Qur'an is created speech rather than eternally fixed, its meaning can be adapted to contemporary circumstances without denying its divine source. Reform programs that modernize Islamic law and social practice gain theological legitimacy.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, reform_movements, beneficiary,
    organized, generational, mobile, regional).

% Falsafa (Islamic philosophical inquiry influenced by Greek thought) schools integrate this reading into their systematic cosmology: if God is absolutely transcendent and beyond temporal attributes, the Qur'an must be a created artifact in time, preserving God's ontic separation from creation. Philosophy gains authority as a legitimate tool for understanding revelation.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, philosophical_schools, beneficiary,
    institutional, generational, mobile, regional).

% Jurists and hadith scholars whose interpretive authority and legal rulings derived from treating the Qur'an as unmediated divine speech face loss of textual certainty. If the Qur'an is created—a product of divine action within time—its literal meaning becomes subject to interpretive renovation. Their traditionalist methodology, which emphasized textual fixity and precedent, loses ground to rational reinterpretation.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    institutional, generational, constrained, regional).

% Religious communities whose spiritual identity and practice are grounded in the Qur'an as unmediated divine word—whose recitation, memorization, and literal observance constitute their relationship to God—experience the created reading as a dissolution of the direct link between human and divine. Their framework for interpreting the Qur'an's authority is undermined; they are locked into this identity and cannot exit without profound existential rupture.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_communities, payer,
    moderate, biographical, identity_locked, local).

% State actors can selectively invoke or suppress this reading to serve political ends: adopting it opens interpretive flexibility for social policy; rejecting it aligns the state with traditionalist constituencies. Political authority mediates the constraint's enforcement within specific jurisdictions and historical periods.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, political_authorities, agenda_setter,
    institutional, generational, arbitrage, regional).

% Communities committed to the uncreated (qadīm) reading are structurally excluded from this constraint's justification framework: the created reading's core premise—that the Qur'an is a temporal artifact—directly contradicts their foundational claim that revelation is eternally coexistent with God. They have no seat at the table of rational deliberation within this reading's epistemic structure.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, uncreated_reading_defenders, excluded,
    institutional, generational, trapped, regional).

% The corpus of Islamic legal reasoning and methodology stands as the analytical site where the constraint's operation is measured: does rational interpretation of created speech strengthen or fragment juridical consensus? The institution itself is neither beneficiary nor victim but the domain where consequences unfold.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, islamic_jurisprudence, observer,
    analytical, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(quran_ontological_status__created_reading, islamic_jurisprudence).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:fixing_cost_class(quran_ontological_status__created_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves divine transcendence by locating God ontologically above all temporal artifacts, including revelation itself. Coordinates a defensible theological position: God cannot be limited by or identified with any created thing, including the Qur'an. Enables systematic philosophy to integrate Islamic theology with rationalist metaphysics. Provides a hermeneutic framework in which textual interpretation can develop without denying the text's divine origin.
% TRANSFER_FUNCTION: Transfers hermeneutic authority from traditionalist jurists and literalist custodians to rationalist theologians and philosophical schools. Moves interpretive power from fixed-text precedent-based reasoning to flexible, reason-informed reinterpretation. Relocates the locus of religious authority from textual fixity to rational understanding of divine intention.
% ABSENT_VOICES: Defenders of the uncreated (qadīm) reading are structurally excluded—the created reading's core premise forecloses their position within any single framework. Literalist scholars from traditions that stake their authority on textual immutability are not consulted on the reading's legitimacy; they experience the constraint as imposed. Mystical traditions that emphasize unmediated encounter with God are marginalized by rationalist mediation.
% DISAPPEARANCE_RATIONALE: If this reading vanished—if the created/uncreated debate collapsed into consensus on the uncreated position—Islamic jurisprudence would reorganize around textual fixity, reform movements would lose theological cover for interpretive innovation, philosophical integration with Greek rationalism would lose its foundation, and traditionalist authority structures would reassert themselves. The entire landscape of Islamic intellectual authority would shift.
% FOUNDING_PROBLEM: How can God remain absolutely transcendent, beyond all temporal and created limitations, while also communicating through a text to human beings? If the Qur'an were eternally coexistent with God (uncreated), would God's transcendence be compromised by being bound to a created artifact? Can the divine will be exercised within time without violating God's ontic independence from time?
% FOUNDING_PROBLEM_CORROBORATION: Mu'tazilite theologians attest the founding problem is still live, citing the logical necessity of divine transcendence. Uncreated-reading defenders attest the problem is ill-posed—that true transcendence cannot be threatened by the Qur'an's eternality. Independent philosophical analysis from medieval and modern Islamic thought confirms the problem is genuine and unresolved; both readings claim to solve it differently. Contemporary scholars outside both benefiting parties (e.g., historians of Islamic theology, comparative philosophers) confirm the problem remains contested with no scholarly consensus on resolution.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).

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
 *   Extractiveness measures 0.62 at interval end, rising from 0.48 at start and plateauing at 21. The reading is extractive because it systematically advantages rationalist-friendly positions (hermeneutic flexibility, interpretive authority) over traditionalist ones (textual fixity, precedent-based reasoning). The rise reflects historical periods in which Mu'tazilite doctrine gained institutional backing and then stabilized when political support fluctuated. Suppression is lower (0.45) than in pure snares because the constraint persists through reasoned argument and intellectual appeal, not raw coercion—though traditionalist jurists do experience it as suppressive when political authorities enforce rationalist readings. Theater ratio is low (0.28) because the rational-theological function is genuine; the constraint is not theatre. Accessibility collapse is high (0.71) because once the created reading is articulated, its logical structure becomes difficult to un-know—alternatives do not simply persist; they require active intellectual defense. Resistance is moderate-to-high (0.58) because traditionalist constituencies mount sustained intellectual and institutional resistance; the reading is not passively accepted. The measurement series show extractiveness and suppression plateauing after the Abbasid period (points 21+), reflecting stabilization of this reading as one legitimate intellectual tradition among several, rather than continuing ascendancy.
 *
 * PERSPECTIVAL GAP:
 *   From the rationalist theologian seat, this reading is a genuine solution to the transcendence problem—it coordinates a defensible metaphysical position and opens intellectual and hermeneutic possibilities. From the traditionalist jurist seat, the same structure operates as an assault on textual authority and the methodology that grounds Islamic law. From the literalist community seat, the reading dissolves the unmediated divine connection they stake their spiritual identity on. The engine computes these divergent seat-level types from the structural data: the agenda-setter and beneficiaries will likely compute as rope/coordination; the victims will compute with higher effective extraction and classify differently. This divergence is the measurement this story exists to enable.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist theologians are beneficiaries with institutional power and arbitrage-like exit options (they can move between philosophical schools, integrate with other traditions); their directionality sits near the beneficiary end (d ~0.2-0.35). Traditionalist jurists are victims with institutional power but constrained exit (their entire authority structure is rooted in textual methodologies); their directionality is mid-range to target end (d ~0.6-0.75). Literalist communities are victims with moderate power and identity-locked exit (spiritual identity fused with literal interpretation); their directionality is strongly target-end (d ~0.75-0.85). Political authorities who set the reading are agenda-setters with institutional power and arbitrage exit; their directionality depends on how they use the reading (deployed strategically, not internally committed), placing them near symmetric to beneficiary (d ~0.3-0.45). Suppression is relatively low (0.45) because the constraint rides on reasoned argument rather than coercion; the exclusion of uncreated-reading defenders is structural (they are foreclosed by logical contradiction, not suppressed by force).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving divine transcendence while explaining revelation) is contestable but live—it is a genuine theological problem that rational theology still engages. The founded solution (created speech) addresses the problem from a specific rational perspective. Mandatrophy would arise if the founding problem became dead (if transcendence were no longer contested) while the created reading persisted as pure tradition with no functional role. Currently, the reading remains intellectually active and defended by rationalist constituencies; it has not atrophied into pure theater. However, the measurement plateau (extractiveness flat from point 21 onward) and moderate theater ratio (0.28) suggest that in periods when political support weakens, the reading persists more through institutional habit than active rational defense. The low suppression (0.45) is diagnostic here: this is not a snare held by force, but neither is it a pure rope—it is a partially mandatrophic structure that depends on rationalist constituencies to regenerate its justification. The reading risks sliding toward piton (pure inertia) if rationalist theology declines, which is an omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rational_theology_vs_received_doctrine,
    'Is the created reading a genuine theological solution to the transcendence problem, or is it a rationalist ideology disguised as metaphysics—one that benefits specific intellectual constituencies at the expense of traditionalist authority?',
    'Examine the internal logic of the created reading''s defense of transcendence: does it successfully resolve the problem, or does it relocate it (e.g., by making God''s action within time problematic)? Compare with uncreated reading''s defense. Assess whether the reading''s appeal tracks with rational arguments or with institutional power shifts.',
    'If the created reading is a genuine solution, extractiveness reflects legitimate intellectual disagreement and the constraint computes as rope. If it is ideological, extractiveness reflects power capture and the constraint computes as tangled rope or snare. This is the fundamental read-across between ''rational discourse about God'' and ''politics of theological authority.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rational_theology_vs_received_doctrine, conceptual, 'Whether the created reading is metaphysical truth or rationalist ideology.').

omega_variable(
    identity_lock_suppression_internalization,
    'For literalist communities whose identity is fused with literal Qur''anic interpretation, is their resistance to the created reading rooted in internalized theological conviction or in structural suppression of their interpretive standing?',
    'Post-exit observation: if literalist scholars move to contexts where the created reading does not dominate institutional power, do they maintain their interpretive commitments with equal vigor? If so, suppression was structural (institutional context-dependent). If their commitments weaken or reframe, internalization is significant. Examine autobiographical accounts from scholars who navigated between traditions.',
    'If identity lock is structural, the literalist communities are victims of suppression that could be lifted by institutional change. If internalized, the created reading has successfully reshaped their self-understanding even when institutional pressure is absent—a deeper form of extraction. This affects whether the constraint is a snare (coercive suppression) or a tangled rope (mixed coordination and internalized extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_internalization, empirical, 'Structural vs. internalized mechanism of identity lock in literalist communities.').

omega_variable(
    mandatrophy_trajectory,
    'As rationalist theology declines in institutional power (measured by political patronage, university positions, circulation of texts), does the created reading persist through active intellectual defense or through institutional inertia?',
    'Track the ratio of new defenses of the created reading to historical invocations. If new rational arguments appear in each generation, intellectual vitality persists. If historical citations accumulate without new argumentation, the reading is sliding toward piton. Examine whether political support is necessary for the reading''s institutional survival.',
    'A piton trajectory means the constraint is becoming pure theater—the rational-theological justification is increasingly a cover story for institutional habit. The constraint could reclassify from rope (genuine coordination of rational theology) to piton (degraded performance). If mandatrophy is underway, strategies for constraining it (reviving active defense, opening interpretive innovation) differ from strategies for a stable rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_trajectory, empirical, 'Whether the created reading is sustaining active intellectual engagement or sliding into inertial maintenance.').

omega_variable(
    kernel_foreclose_status,
    'Does the created reading''s core logical structure (God is absolutely transcendent; therefore the Qur''an must be created to avoid binding God to time) necessarily foreclose the uncreated reading, or do they represent incommensurable frameworks that could coexist in different theoretical registers?',
    'Detailed logical reconstruction of both readings'' metaphysical commitments. Can a single framework hold both ''God is absolutely transcendent'' and ''the Qur''an is eternally coexistent with God'' without contradiction? If yes, they coexist; if no, they foreclose each other. Examine medieval Islamic philosophy''s actual treatment of this question: did philosophers treat them as truly incompatible or as emphasizing different aspects of a unified theology?',
    'If the readings foreclose each other, the kernel is a genuine logical contradiction that any single framework must resolve. If they coexist in different registers, the kernel is a difference in theological emphasis rather than a contradiction, and all three readings could be held simultaneously by different communities without logical incoherence. This affects how the constraint family is modeled: as mutually exclusive positions or as complementary emphases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_foreclose_status, conceptual, 'Whether the created and uncreated readings are logically incompatible or represent different theological emphases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t7, quran_ontological_status__created_reading, theater_ratio, 7, 0.16).
narrative_ontology:measurement_basis(qura_tr_t7, observed).
narrative_ontology:measurement(qura_tr_t14, quran_ontological_status__created_reading, theater_ratio, 14, 0.21).
narrative_ontology:measurement_basis(qura_tr_t14, observed).
narrative_ontology:measurement(qura_tr_t21, quran_ontological_status__created_reading, theater_ratio, 21, 0.26).
narrative_ontology:measurement_basis(qura_tr_t21, observed).
narrative_ontology:measurement(qura_tr_t28, quran_ontological_status__created_reading, theater_ratio, 28, 0.28).
narrative_ontology:measurement_basis(qura_tr_t28, observed).
narrative_ontology:measurement(qura_tr_t35, quran_ontological_status__created_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(qura_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t7, quran_ontological_status__created_reading, base_extractiveness, 7, 0.54).
narrative_ontology:measurement_basis(qura_be_t7, observed).
narrative_ontology:measurement(qura_be_t14, quran_ontological_status__created_reading, base_extractiveness, 14, 0.59).
narrative_ontology:measurement_basis(qura_be_t14, observed).
narrative_ontology:measurement(qura_be_t21, quran_ontological_status__created_reading, base_extractiveness, 21, 0.62).
narrative_ontology:measurement_basis(qura_be_t21, observed).
narrative_ontology:measurement(qura_be_t28, quran_ontological_status__created_reading, base_extractiveness, 28, 0.62).
narrative_ontology:measurement_basis(qura_be_t28, observed).
narrative_ontology:measurement(qura_be_t35, quran_ontological_status__created_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement_basis(qura_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__created_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(qura_su_t0, observed).
narrative_ontology:measurement(qura_su_t7, quran_ontological_status__created_reading, suppression_requirement, 7, 0.41).
narrative_ontology:measurement_basis(qura_su_t7, observed).
narrative_ontology:measurement(qura_su_t14, quran_ontological_status__created_reading, suppression_requirement, 14, 0.43).
narrative_ontology:measurement_basis(qura_su_t14, observed).
narrative_ontology:measurement(qura_su_t21, quran_ontological_status__created_reading, suppression_requirement, 21, 0.45).
narrative_ontology:measurement_basis(qura_su_t21, observed).
narrative_ontology:measurement(qura_su_t28, quran_ontological_status__created_reading, suppression_requirement, 28, 0.45).
narrative_ontology:measurement_basis(qura_su_t28, observed).
narrative_ontology:measurement(qura_su_t35, quran_ontological_status__created_reading, suppression_requirement, 35, 0.45).
narrative_ontology:measurement_basis(qura_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__created_reading, 0.12).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% The Qur'an's ontological status decomposes into three constraint stories sharing the kernel quran_ontological_status but instantiating different readings. Each reading makes distinct ε claims: (1) created_reading (this story) = ε 0.62, rope, rational-theological coordination with asymmetric benefit; (2) uncreated_reading = ε ~0.25-0.35 (estimated), mountain or rope, natural-law metaphysics with minimal extraction; (3) state_enforced_creation_reading = ε ~0.80+, snare, political coercion disguised as theological commitment. The family structure reflects the historical relationship: medieval Abbasid state enforced Mu'tazilite creation doctrine via mihna, then the enforcement collapsed and the created reading persisted as one legitimate intellectual tradition. The created_reading is downstream of political authority's capacity to impose it; the uncreated_reading constrains what the created reading can claim about metaphysical necessity. Network links enable analysis of how the readings' relative institutional power affects each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_ontological_status__created_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
