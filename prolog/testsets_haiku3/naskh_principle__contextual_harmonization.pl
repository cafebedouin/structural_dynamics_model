% ============================================================================
% CONSTRAINT STORY: naskh_principle__contextual_harmonization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_contextual_harmonization, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: naskh_principle__contextual_harmonization
 *   human_readable: Naskh Principle: Contextual Harmonization Reading
 *   domain: religious/legal/hermeneutical
 *
 * SUMMARY:
 *   The contextual harmonization reading of the naskh principle holds that
 *   all Quranic verses remain valid and binding within their specific
 *   revelatory contexts and the situational conditions they address. Apparent
 *   textual contradictions (such as differing rules on inheritance,
 *   intoxication, or marital dissolution across verses) are resolved not by
 *   chronological supersession but by contextual specification: each verse
 *   governs the circumstances it was revealed to address. This reading
 *   benefits theological coherence (all verses are preserved in divine
 *   purpose) and jurisprudential adaptability (scholars can apply different
 *   verses to different modern situations). It imposes costs on legal
 *   predictability (the same situation can receive different rulings
 *   depending on which context a jurist selects) and on the definitional
 *   authority of individual jurists (questions are perpetually reopenable
 *   through recontextualization). This story instantiates ONE reading of the
 *   contested naskh kernel; the sibling readings (classical_abrogation and
 *   progressive_restriction) are separate constraint stories with their own ε
 *   values and beneficiary structures.
 *
 * KEY AGENTS:
 *   - adaptive_jurisprudential_schools: institutional agenda-setter (powerful, generational, constrained exit) — maintain the interpretive authority to determine which context applies when
 *   - theological_coherence_tradition: non-agent beneficiary proposition — the claim that the Quran is unified when properly understood
 *   - jurists_seeking_definitional_closure: powerful payer (biographical, constrained exit) — bear the cost of perpetual reinterpretive labor when questions can be reopened contextually
 *   - non_specialist_believers: powerless payer (biological, trapped exit) — receive conflicting rulings from different schools applying the same contextual apparatus
 *   - classical_abrogation_schools: excluded institutional actor (generational) — their chronological framework is challenged by the contextual reading
 *   - progressive_restriction_schools: observer institutional actor (generational) — interpret the same apparent contradictions through pedagogical sequence rather than contextual specification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__contextual_harmonization, 0.58).
domain_priors:suppression_score(naskh_principle__contextual_harmonization, 0.41).
domain_priors:theater_ratio(naskh_principle__contextual_harmonization, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, extractiveness, 0.58).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__contextual_harmonization, tangled_rope).
narrative_ontology:human_readable(naskh_principle__contextual_harmonization, "Naskh Principle: Contextual Harmonization Reading").
narrative_ontology:topic_domain(naskh_principle__contextual_harmonization, "religious/legal/hermeneutical").

domain_priors:requires_active_enforcement(naskh_principle__contextual_harmonization).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__contextual_harmonization, '23834a50-8170-4cb6-bf58-009a3e0ee8cd').
narrative_ontology:cs_kernel_codification('23834a50-8170-4cb6-bf58-009a3e0ee8cd', fixed_text).
narrative_ontology:cs_authority_grounding('23834a50-8170-4cb6-bf58-009a3e0ee8cd', lineage).
narrative_ontology:cs_interpretation_layer_present('23834a50-8170-4cb6-bf58-009a3e0ee8cd').
narrative_ontology:cs_reading_relation('23834a50-8170-4cb6-bf58-009a3e0ee8cd', naskh_principle__classical_abrogation, coexists_with).
narrative_ontology:cs_reading_relation('23834a50-8170-4cb6-bf58-009a3e0ee8cd', naskh_principle__progressive_restriction, coexists_with).
narrative_ontology:cs_axiom('23834a50-8170-4cb6-bf58-009a3e0ee8cd', foundational, all_verses_retain_validity_in_context).
narrative_ontology:cs_axiom_status(all_verses_retain_validity_in_context, holdable).
narrative_ontology:cs_axiom_grounding('23834a50-8170-4cb6-bf58-009a3e0ee8cd', all_verses_retain_validity_in_context, deontological).
narrative_ontology:cs_axiom('23834a50-8170-4cb6-bf58-009a3e0ee8cd', foundational, contextual_specification_resolves_apparent_contradiction).
narrative_ontology:cs_axiom_status(contextual_specification_resolves_apparent_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('23834a50-8170-4cb6-bf58-009a3e0ee8cd', contextual_specification_resolves_apparent_contradiction, conventional).
narrative_ontology:cs_reference_frame('23834a50-8170-4cb6-bf58-009a3e0ee8cd', quranic_unity_and_divine_wisdom).
narrative_ontology:cs_drift_state('23834a50-8170-4cb6-bf58-009a3e0ee8cd', contemporary_academic_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('23834a50-8170-4cb6-bf58-009a3e0ee8cd', '').
narrative_ontology:cs_kernel_id(naskh_principle__contextual_harmonization, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, theological_coherence_tradition).
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, adaptive_jurisprudential_schools).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, jurists_seeking_definitional_closure).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, non_specialist_believers).
narrative_ontology:constraint_vindicates(naskh_principle__contextual_harmonization, quranic_unity_thesis).
narrative_ontology:constraint_vindicates(naskh_principle__contextual_harmonization, divine_wisdom_in_textual_layering).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Quranic law by contextualizing each verse within its historical revelation moment and situational particularity. They maintain the authority to determine which context applies at any given time, preserving flexibility to apply different verses to different circumstances. This authority depends on the contextual reading framework remaining dominant — if abrogation became standard, their interpretive discretion would collapse to a simple chronological rule.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, adaptive_jurisprudential_schools, agenda_setter,
    institutional, generational, constrained, global).

% The proposition that the Quran is internally harmonious when properly understood. This reading vindicates that thesis: no verse is truly cancelled; all are preserved in unified purpose across contexts. The tradition benefits insofar as coherence claims escape falsification by apparent contradiction.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, theological_coherence_tradition, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(naskh_principle__contextual_harmonization, theological_coherence_tradition).

% Face indefinite reopenings of settled legal questions when contextual reinterpretation is available as an alternative to abrogation. They bear the cost of perpetual interpretive labor — questions they thought resolved through chronological reasoning can be re-examined if a new contextual reading emerges. Their authority to close questions definitively is subordinated to the adaptive framework.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, jurists_seeking_definitional_closure, payer,
    powerful, biographical, constrained, national).

% Depend on authoritative legal guidance but receive conflicting rulings from different schools using the same contextual apparatus to reach opposite conclusions on identical modern situations. The flexibility that the reading provides to jurisprudential schools translates to unpredictability for lay followers: the same question can be answered multiple ways depending on which context the answering jurist selects.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, non_specialist_believers, payer,
    powerless, biographical, trapped, local).

% Maintain that chronological supersession is the proper frame; this reading's contextual apparatus directly challenges their authority structure. They are excluded from the decision space that determines which framework governs Quranic interpretation — the outcome favors contextual harmonization in academies and among adaptively-oriented jurists.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, classical_abrogation_schools, excluded,
    institutional, generational, constrained, global).

% Hold that apparent abrogations represent pedagogical progression rather than textual invalidation. Their framework parallels the contextual reading in preserving all verses but interprets the progression as divine teaching strategy. They analyze the same constraint differently — focusing on the semantic relation (pedagogical sequence) rather than the resolution method (contextual specification).
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, progressive_restriction_schools, observer,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__contextual_harmonization, adaptive_jurisprudential_schools).
narrative_ontology:fixing_cost_class(naskh_principle__contextual_harmonization, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves apparent Quranic contradictions through contextual analysis rather than textual elimination. This preserves the entire corpus as live law while enabling unified interpretation: jurisprudential schools coordinate on the principle that context determines application, avoiding the fragmentation that would occur if some verses were simply cancelled.
% TRANSFER_FUNCTION: Transfers interpretive authority from automatic chronological rules (which any reader could apply) to contextual analysis (which requires specialized jurisprudential knowledge). Non-specialist believers transfer epistemic dependence to jurisprudential schools; schools transfer definitional certainty away in exchange for perpetual reinterpretive capacity.
% ABSENT_VOICES: Classical abrogation schools object but are largely excluded from contemporary Islamic legal academia; their voice shapes sectarian divides rather than the governing discourse. Non-specialist believers who might argue for definitive, unchanging law are suppressed by the technical nature of contextual analysis — the apparatus makes their objection literally untranslatable into the professional conversation.
% DISAPPEARANCE_RATIONALE: If the contextual harmonization reading disappeared and classical abrogation became standard, Islamic jurisprudence would reorganize: chronology would become the deciding variable; legal schools would fragment into those following different chronologies; thousands of settled questions would require re-adjudication; the corpus of fiqh (Islamic jurisprudence) built on contextual flexibility would need wholesale revision.
% FOUNDING_PROBLEM: Early Islamic legal scholarship encountered apparent Quranic contradictions (e.g., verses permitting then restricting alcohol, verses specifying inheritance with later verses appearing to alter rules). The founding question: are these true contradictions requiring explanation, or is there a harmonizing principle that preserves all verses?
% FOUNDING_PROBLEM_CORROBORATION: Classical Islamic scholars (al-Shāfiʿī, al-Jaṣṣāṣ, Ibn Qayyim al-Jawziyyah) from outside the contextual-harmonization school attest that the contradiction problem was real and urgent. Modern comparative Quranic analysis confirms the textual tensions. However, whether these are genuine contradictions or apparent ones (resolvable through context) remains disputed — classical abrogation scholars deny the coherence claim; contextual-harmonization scholars affirm it. No neutral external corroboration exists; the corroboration itself partitions along reading lines.
narrative_ontology:disappearance_verdict(naskh_principle__contextual_harmonization, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__contextual_harmonization, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__contextual_harmonization, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__contextual_harmonization, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__contextual_harmonization, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__contextual_harmonization_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__contextual_harmonization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__contextual_harmonization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at interval end) reflects the asymmetric benefit accrual: adaptive jurisprudential schools capture the right to perpetually reinterpret, while non-specialist believers lose the ability to know definitively what the law is. The measurement series shows gradual extraction accumulation from 0.38 to 0.58 over the first 60 time points, then plateaus — this reflects historical institutional consolidation of the contextual framework in Islamic legal academia (roughly corresponding to late medieval through modern period). Suppression (0.41) is moderate: the apparatus is not coercive in the police sense, but it suppresses non-specialist objections by rendering them technically untranslatable; classical abrogation schools are excluded from governance rather than violently defeated. Theater (0.28, moderate-low) reflects that contextual analysis performs real hermeneutical labor — questions genuinely do require contextual specification — but growing share of the labor goes to defending the framework against empirical challenges (can the same verse really apply to contradictory modern circumstances?) rather than resolving the founding problem. Accessibility collapse (0.62) moderately high: once the contextual reading is established, alternatives (raw contradiction, pure chronology) are partly inaccessible — but contextual flexibility itself creates apparent alternative solutions, so complete closure does not occur. Resistance (0.71) is substantial: classical abrogation schools, jurisprudential conservatives, and non-specialist believers all mount real resistance, though it is largely excluded from institutional governance.
 *
 * PERSPECTIVAL GAP:
 *   From the adaptive jurisprudential seat: this is rope — genuine coordination solving an authentic problem (Quranic textual tensions) while preserving theological coherence and enabling prudent interpretation. From the definitional-closure seat: this is snare — the flexible apparatus prevents them from ever definitively settling a question; every judgment can be revisited by recontextualizing. From the non-specialist believer seat: this is tangled_rope at best, snare at worst — coordination benefit (unified interpretive framework, not anarchic contradiction) coupled with extraction (variable rulings based on jurist selection, no way to predict outcome).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: adaptive jurisprudential schools (institutional, powerful, preserve reinterpretive discretion via the contextual apparatus) and the theological coherence tradition (preserved as a vindicated proposition by making apparent contradictions into contextual multiplicity). Victims: jurists seeking definitional closure (powerful but lose the right to close questions definitively) and non-specialist believers (powerless, trapped exit, receive variable rulings depending on jurist-selected context). The beneficiary group has institutional authority; the payer groups lack the expertise to challenge the technical apparatus. Identity-lock is present among adaptive jurisprudential schools: careers and reputational stakes are built on the contextual framework; leaving it would mean losing professional standing. Non-specialist believers are trapped not by identity but by dependency: they must follow someone's jurisprudence and cannot evaluate which school's contextual selection is correct.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy signals would appear if the contextual apparatus became primarily theatrical (judges select contexts not because contexts are real but to reach predetermined conclusions). The theater_ratio plateaus at 0.28 in the measurement series, indicating the functionality remains real but growing share of labor goes to defending the framework itself. Early danger signs: (1) if context-selection becomes untethered from textual particularity and becomes pure outcome-optimization, theater ratio should spike; (2) if successor generations of jurists stop learning the contextual skill and merely perform it, theater ratio should climb sharply. Neither has occurred in the historical record — the contextual apparatus retains real technical content. The classical abrogation reading would not have mandatrophy (its chronological rule is unambiguous) but would face the opposite problem: legal rigidity and institutional brittleness if chronology cannot accommodate new problems. The contextual reading trades definitional closure for perpetual adaptability; that trade is stable and defensible, not mandatrophic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coherence_vs_cover_story,
    'Is the theological coherence claim (all verses are harmonized within context) a genuine discovery about textual unity, or a cover story that allows jurists to avoid the hard question of whether the Quran genuinely contains internally conflicting rules?',
    'Comparative hermeneutics: if contextual harmonization breaks down on specific verses (jurists cannot find coherent contexts that preserve all verses simultaneously), the cover-story reading gains credibility. If the apparatus remains generatively powerful across new cases, coherence is supported.',
    'If coherence is a cover story, extractiveness should be reclassified upward — the apparatus serves institutional interest (jurist authority) under the guise of theological principle. If coherence is real, extractiveness remains at the current level (justified by the genuine coordination benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_vs_cover_story, conceptual, 'Whether theological coherence is discovered or constructed.').

omega_variable(
    context_selection_mechanism,
    'When two jurists apply the contextual apparatus to the same modern question and reach opposite conclusions, what selects which historical context each jurist invokes? Is it the text, professional training, institutional affiliation, or outcome preference?',
    'Case analysis: examine disagreements among similarly-trained jurists from the same school, same time period, same institutional setting. If they diverge on context selection despite identical inputs, outcome preference or affiliation rather than textual determination is operative.',
    'If selection is textually determined, suppression (0.41) is justified — the apparatus has real constraints. If selection is outcome-driven, suppression should be reclassified upward — the apparatus is de facto arbitrary, and its power to suppress objections becomes coercive rather than technical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(context_selection_mechanism, empirical, 'Whether context selection is constrained by the text or driven by jurist preference.').

omega_variable(
    reading_foreclosure_uncertainty,
    'Do the contextual harmonization and classical abrogation readings logically foreclose each other (neither can be held in a single framework), or can they coexist as alternative interpretive traditions?',
    'Logical analysis: if a jurist could say ''in this case I harmonize contextually, in that case I apply chronological abrogation'' without internal contradiction, they coexist; if saying both contradicts a foundational premise, they foreclose.',
    'If they foreclose, the cs_structure.reading_relations entry for classical_abrogation should read forecloses. If they coexist, it should read coexists_with. This determines whether the kernel contest is a true binary opposition or a spectrum of live positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_uncertainty, conceptual, 'Whether readings foreclose or coexist logically.').

omega_variable(
    suppression_of_non_specialist_voice,
    'Is the suppression of non-specialist objections structural (the contextual apparatus is technically opaque to non-experts, so objections cannot be formulated in its language) or internalized (non-specialists accept their own incompetence and defer without objecting)?',
    'Historical evidence: when non-specialists have been empowered to speak (social media, translation of fiqh into vernacular languages), do they mount systematic objections to contextual flexibility? If yes, suppression is structural; if no, it is internalized.',
    'If suppression is structural, it is a feature of the apparatus that could be remedied by accessibility (translating contextual analysis into lay language). If internalized, remedying it would require changing self-concepts and institutional hierarchies, not just pedagogy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_of_non_specialist_voice, empirical, 'Structural vs. internalized suppression of non-specialist voice.').

omega_variable(
    kernel_reading_instantiation,
    'This constraint instantiates the contextual-harmonization reading of the naskh kernel — but is the kernel itself real, or constructed by modern scholars imposing a unified frame on historically independent debates?',
    'Historiography: trace how classical Islamic scholars formulated the problem. Did they ask ''what principle governs apparent Quranic contradictions?'' (kernel framing), or did they ask narrower questions about specific pairs of verses? If the latter, the kernel may be a modern scholarly retrospection, not a historical live question.',
    'If the kernel is constructed, the reading_relations entries depend on modern framing choices, not logical structure. Alternative kernel framings might produce different sibling sets and different foreclosure relations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Whether the naskh kernel is a historical or scholarly-constructed frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__contextual_harmonization, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__contextual_harmonization, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(nask_tr_t0, observed).
narrative_ontology:measurement(nask_tr_t12, naskh_principle__contextual_harmonization, theater_ratio, 12, 0.16).
narrative_ontology:measurement_basis(nask_tr_t12, observed).
narrative_ontology:measurement(nask_tr_t25, naskh_principle__contextual_harmonization, theater_ratio, 25, 0.21).
narrative_ontology:measurement_basis(nask_tr_t25, observed).
narrative_ontology:measurement(nask_tr_t40, naskh_principle__contextual_harmonization, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(nask_tr_t40, observed).
narrative_ontology:measurement(nask_tr_t60, naskh_principle__contextual_harmonization, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(nask_tr_t60, observed).
narrative_ontology:measurement(nask_tr_t80, naskh_principle__contextual_harmonization, theater_ratio, 80, 0.29).
narrative_ontology:measurement_basis(nask_tr_t80, observed).
narrative_ontology:measurement(nask_tr_t100, naskh_principle__contextual_harmonization, theater_ratio, 100, 0.28).
narrative_ontology:measurement_basis(nask_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__contextual_harmonization, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(nask_be_t0, observed).
narrative_ontology:measurement(nask_be_t12, naskh_principle__contextual_harmonization, base_extractiveness, 12, 0.45).
narrative_ontology:measurement_basis(nask_be_t12, observed).
narrative_ontology:measurement(nask_be_t25, naskh_principle__contextual_harmonization, base_extractiveness, 25, 0.52).
narrative_ontology:measurement_basis(nask_be_t25, observed).
narrative_ontology:measurement(nask_be_t40, naskh_principle__contextual_harmonization, base_extractiveness, 40, 0.56).
narrative_ontology:measurement_basis(nask_be_t40, observed).
narrative_ontology:measurement(nask_be_t60, naskh_principle__contextual_harmonization, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(nask_be_t60, observed).
narrative_ontology:measurement(nask_be_t80, naskh_principle__contextual_harmonization, base_extractiveness, 80, 0.59).
narrative_ontology:measurement_basis(nask_be_t80, observed).
narrative_ontology:measurement(nask_be_t100, naskh_principle__contextual_harmonization, base_extractiveness, 100, 0.58).
narrative_ontology:measurement_basis(nask_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__contextual_harmonization, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(nask_su_t0, observed).
narrative_ontology:measurement(nask_su_t12, naskh_principle__contextual_harmonization, suppression_requirement, 12, 0.37).
narrative_ontology:measurement_basis(nask_su_t12, observed).
narrative_ontology:measurement(nask_su_t25, naskh_principle__contextual_harmonization, suppression_requirement, 25, 0.39).
narrative_ontology:measurement_basis(nask_su_t25, observed).
narrative_ontology:measurement(nask_su_t40, naskh_principle__contextual_harmonization, suppression_requirement, 40, 0.41).
narrative_ontology:measurement_basis(nask_su_t40, observed).
narrative_ontology:measurement(nask_su_t60, naskh_principle__contextual_harmonization, suppression_requirement, 60, 0.41).
narrative_ontology:measurement_basis(nask_su_t60, observed).
narrative_ontology:measurement(nask_su_t80, naskh_principle__contextual_harmonization, suppression_requirement, 80, 0.41).
narrative_ontology:measurement_basis(nask_su_t80, observed).
narrative_ontology:measurement(nask_su_t100, naskh_principle__contextual_harmonization, suppression_requirement, 100, 0.41).
narrative_ontology:measurement_basis(nask_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__contextual_harmonization, identity_coordination).
narrative_ontology:boltzmann_floor_override(naskh_principle__contextual_harmonization, 0.12).
narrative_ontology:affects_constraint(naskh_principle__contextual_harmonization, naskh_principle__classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__contextual_harmonization, naskh_principle__progressive_restriction).

% DUAL FORMULATION NOTE:
% The contextual-harmonization, classical-abrogation, and progressive-restriction readings form a constraint family sharing a single kernel: the naskh principle (the frame that encompasses all three readings). Each reading instantiates a different constraint because each has a different ε (contextual harmonization: high ε, moderate suppression; classical abrogation: low ε, high suppression; progressive restriction: moderate ε, moderate suppression), different beneficiary/victim structures, and different types. The three readings are linked via network.affects_constraints: contextual harmonization influences both siblings by establishing the contest space; classical abrogation forecloses contextual harmonization if chronology becomes the standard; progressive restriction coexists with contextual harmonization but influences abrogation by offering a tertiary alternative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__contextual_harmonization, powerful, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
