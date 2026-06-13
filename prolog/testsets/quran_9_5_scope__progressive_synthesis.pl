% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__progressive_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__progressive_synthesis, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: quran_9_5_scope__progressive_synthesis
 *   human_readable: Quranic Ethical Trajectory Supersedes Literalist 9:5 Application
 *   domain: religious/hermeneutical/political
 *
 * SUMMARY:
 *   Verse 9:5 (the 'Verse of the Sword') has been read across Islamic history
 *   as establishing a perpetual divine command to fight polytheists until
 *   submission. The progressive-synthesis reading repositions this verse as a
 *   historical directive addressing a specific 7th-century Medinan breach of
 *   treaty by polytheist tribes, embedded within the Quran's larger ethical
 *   arc of mercy, justice, and covenant-keeping. Under this reading, the
 *   verse exits active constraint space: it neither binds contemporary
 *   Muslims to perpetual offensive action nor binds polytheists to accept
 *   Quranic authority. Instead, the Quranic ethical trajectory (the
 *   diachronic synthesis across Meccan pacifism and Medinan defense to mature
 *   Quranic universals) becomes the constraint that supersedes literalist
 *   application. This constraint story instantiates the PROGRESSIVE_SYNTHESIS
 *   READING of the kernel quran_9_5_scope; two sibling readings
 *   (abrogating_universal and contextual_defensive) are separate constraint
 *   stories in the same family, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - progressive_quranic_scholars (agenda setter, organized, mobile exit): position the verse as time-bound directive; justify pluralist contemporary Muslim practice
 *   - textualist_legal_schools (payer, institutional, constrained exit): treat verse as perpetually binding; institutional authority depends on literalist hermeneutics
 *   - contemporary_muslim_pluralists (beneficiary, moderate, constrained exit): gain theological warrant for peaceful coexistence
 *   - secular_liberal_frameworks (observer, institutional, arbitrage): have structural interest in readings that subordinate theocratic literal claims
 *   - polytheist_communities_historical (excluded, powerless, trapped): original parties absent from contemporary interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__progressive_synthesis, 0.18).
domain_priors:suppression_score(quran_9_5_scope__progressive_synthesis, 0.12).
domain_priors:theater_ratio(quran_9_5_scope__progressive_synthesis, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, extractiveness, 0.18).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__progressive_synthesis, mountain).
narrative_ontology:human_readable(quran_9_5_scope__progressive_synthesis, "Quranic Ethical Trajectory Supersedes Literalist 9:5 Application").
narrative_ontology:topic_domain(quran_9_5_scope__progressive_synthesis, "religious/hermeneutical/political").

domain_priors:emerges_naturally(quran_9_5_scope__progressive_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__progressive_synthesis, 'e667e0e7-46a8-4fe8-a82d-e25b30d1c629').
narrative_ontology:cs_kernel_codification('e667e0e7-46a8-4fe8-a82d-e25b30d1c629', fixed_text).
narrative_ontology:cs_authority_grounding('e667e0e7-46a8-4fe8-a82d-e25b30d1c629', lineage).
narrative_ontology:cs_interpretation_layer_present('e667e0e7-46a8-4fe8-a82d-e25b30d1c629').
narrative_ontology:cs_reading_relation('e667e0e7-46a8-4fe8-a82d-e25b30d1c629', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('e667e0e7-46a8-4fe8-a82d-e25b30d1c629', quran_9_5_scope__contextual_defensive, influences).
narrative_ontology:cs_axiom('e667e0e7-46a8-4fe8-a82d-e25b30d1c629', foundational, quranic_ethical_arc_supreme).
narrative_ontology:cs_axiom_status(quranic_ethical_arc_supreme, holdable).
narrative_ontology:cs_axiom_grounding('e667e0e7-46a8-4fe8-a82d-e25b30d1c629', quranic_ethical_arc_supreme, deontological).
narrative_ontology:cs_axiom('e667e0e7-46a8-4fe8-a82d-e25b30d1c629', foundational, textual_coherence_over_abrogation).
narrative_ontology:cs_axiom_status(textual_coherence_over_abrogation, holdable).
narrative_ontology:cs_axiom_grounding('e667e0e7-46a8-4fe8-a82d-e25b30d1c629', textual_coherence_over_abrogation, deontological).
narrative_ontology:cs_reference_frame('e667e0e7-46a8-4fe8-a82d-e25b30d1c629', quranic_synthesis_principle).
narrative_ontology:cs_drift_state('e667e0e7-46a8-4fe8-a82d-e25b30d1c629', contemporary_pluralist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e667e0e7-46a8-4fe8-a82d-e25b30d1c629', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__progressive_synthesis, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, progressive_islamic_theology).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, textualist_authority_structures).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, literalist_legal_schools).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, contemporary_muslim_pluralists).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, textualist_legal_schools).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, quranic_ethical_continuity).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, historical_contextualization_principle).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, supersession_of_partial_by_complete_revelation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Quranic directives through diachronic ethical arc: early Meccan pacifism, mediate Medinan defense, mature Quranic synthesis emphasizing mercy and covenant-keeping. They position 9:5 as addressing a specific treaty-breaking crisis, not as establishing perpetual offensive duty. Their scholarship justifies contemporary Islamic pluralism and peaceful coexistence as faithful to the Quran's trajectory rather than departure from it.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, progressive_quranic_scholars, agenda_setter,
    organized, generational, mobile, global).

% Treat verse 9:5 as a standing divine command (potentially abrogating earlier peaceful verses) whose literal scope applies until the Day of Judgment. Their interpretive authority depends on treating revealed text as timelessly binding. Accepting the progressive-synthesis reading would require abandoning literalist hermeneutics and the institutional supremacy of text-bound legal schools. Their position is defended via scholarly argumentation and institutional gate-keeping in traditional Islamic education.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, textualist_legal_schools, payer,
    institutional, civilizational, constrained, global).

% Seek Islamic theological grounding for peaceful coexistence with non-Muslims and acceptance of pluralist polities. The progressive-synthesis reading licenses their contemporary practice as faithful to deeper Quranic commitments rather than as capitulation to secular pressure. They depend on scholarly authority from progressive interpreters to defend their position against textualist critique.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, contemporary_muslim_pluralists, beneficiary,
    moderate, biographical, constrained, global).

% Operate institutional orders premised on religious freedom, minority protection, and separation of religious authority from state law. They do not directly author Islamic interpretation but have structural interest in readings that subordinate literal theocratic claims to ethical universals. They observe the contest without claiming Islamic hermeneutical authority.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, secular_liberal_frameworks, observer,
    institutional, generational, arbitrage, global).

% The historical polytheist parties to the 7th-century Medinan context are absent from contemporary interpretation entirely. Their own account of the treaty-breaking episode, their perception of Muslim defensive necessity, and their objections to the directive are not in the room. The constraint's reading contest proceeds without the original parties' voices.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, polytheist_communities_historical, excluded,
    powerless, immediate, trapped, regional).

% The proposition that the Quran exhibits ethical consistency across its revelatory arc (mercy, justice, covenant-keeping as the grounding themes) rather than internal contradiction requiring abrogation clauses. This is not an agent but a meta-principle guiding interpretation in this reading.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, quranic_ethical_coherence, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(quran_9_5_scope__progressive_synthesis, quranic_ethical_coherence).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__progressive_synthesis, progressive_quranic_scholars).
narrative_ontology:fixing_cost_class(quran_9_5_scope__progressive_synthesis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves potential hermeneutical incoherence in the Quranic text: instead of treating 9:5 as eternally binding command that contradicts Quranic mercy themes, positions it as context-bound response to a specific breach, allowing the Quranic ethical arc (mercy, justice, covenant-keeping) to function as the stable coordinating principle across time.
% TRANSFER_FUNCTION: Transfers hermeneutical authority from literalist schools (whose institutional power rests on treating each verse as independently binding) to progressive scholars (whose authority rests on claims about textual coherence and ethical trajectory). The constraint removes 9:5 from the domain of perpetually active legal directives and relocates it to historical narrative.
% ABSENT_VOICES: The original 7th-century parties (polytheist tribes, Medinan Muslim community members who experienced the historical treaty-breaking) are entirely absent. Contemporary Salafi and classical textualist scholars object to the reading but are often not seated at the same scholarly forums where progressive synthesis is developed. Layfolk in Muslim-majority countries whose legal frameworks cite 9:5 are also structurally excluded from hermeneutical contests happening in academic and progressive institutional spaces.
% DISAPPEARANCE_RATIONALE: Textualist scholars argue the verse's binding force is immutable; removing the reading from discourse would leave the literal text standing uninterpreted. Progressive scholars argue the verse ceases to function as active constraint once its historical context and the Quranic ethical trajectory are properly understood. The disagreement is about whether understanding the constraint makes it vanish or merely reframes it.
% FOUNDING_PROBLEM: The Quran exhibits both pacifist and militant directives; verse 9:5 appears to command perpetual offensive action against polytheists. Classical jurisprudence resolved the tension via abrogation (nasikh): 9:5 abrogates peaceful verses. But abrogation doctrine itself depends on the principle that later revelation supersedes earlier — which the progressive reading flips: the mature Quranic synthesis (mercy, justice, covenant as the arc) supersedes the literalist reading of 9:5 as perpetual command.
% FOUNDING_PROBLEM_CORROBORATION: Textualist scholars and classical Islamic legal traditions attest the abrogation framework is necessary to resolve hermeneutical tension. Progressive scholars and contemporary Quranic studies (drawing on historical-critical methods) attest the founding problem dissolves once context and diachronic arc are centered. Secular comparative religion scholars outside the Islamic tradition attest the contest is live and unresolved. No unified corroboration exists; the founding problem itself is jurisdictionally contested.
narrative_ontology:disappearance_verdict(quran_9_5_scope__progressive_synthesis, contested).
narrative_ontology:founding_problem_status(quran_9_5_scope__progressive_synthesis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__progressive_synthesis, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__progressive_synthesis, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__progressive_synthesis_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, ExtMetricName, E),
    domain_priors:suppression_score(quran_9_5_scope__progressive_synthesis, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quran_9_5_scope__progressive_synthesis),
    narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quran_9_5_scope__progressive_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.18) because this reading claims to REMOVE the constraint from active legal force, not to enforce it against anyone. The verse is repositioned as historical narrative, not binding command. Suppression is VERY LOW (0.12) because the reading does not require coercive enforcement — instead it invokes hermeneutical authority (scholarly consensus, Quranic coherence, ethical principle) to reframe the text's meaning. Theater ratio is LOW (0.08) because the reading's mechanism is genuine textual-coherence argument, not performative maintenance. Resistance is HIGH (0.72) because textualist schools actively defend the literalist reading via scholarly argumentation, institutional gate-keeping, and appeals to traditional Islamic jurisprudence. The measurement series declines from t0 to t30 as the progressive reading accumulates scholarly adoption (observed data points), then stabilizes with slight uptick at t50 (projected) reflecting potential institutional backlash or reassertion. The shared time grid ensures every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   Textualist scholars perceive this reading as illegitimate hermeneutical innovation that subordinates divine command to human ethical reasoning; they compute the constraint as a false claim that cannot make the verse disappear. Progressive scholars perceive the reading as faithful to the Quran's own deepest commitments; they compute the constraint as successful liberation of Islamic ethics from literalist distortion. The engine should compute these as divergent type classifications from the same structural data: textualists see a Snare (the progressive reading falsely claims to remove binding force that persists); progressives see this as removing a Mountain from legal force by demonstrating its context-boundedness.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, textualist legal schools are the structural VICTIMS: their institutional authority depends on treating each revealed verse as timelessly binding; the reading removes a key text they cite as foundational to their jurisprudence. Progressive scholars and secular pluralist frameworks are the BENEFICIARIES: the reading licenses their contemporary practice (peaceful coexistence, religious pluralism) as faithful to deeper Quranic commitments. The directionality derivation should produce high d for textualists (they lose institutional ground) and low d for progressives (they gain hermeneutical legitimacy and institutional space).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (Quranic textual coherence vs. literalist abrogation doctrine) remains CONTESTED because both readings cite Quranic evidence and both claim hermeneutical principle. The progressive reading does NOT resolve the contest by appeal to additional revelation or new textual discovery; instead it reframes the hermeneutical principle (ethical trajectory supersedes literal verse-by-verse binding). This is a CONCEPTUAL REFRAMING, not an empirical resolution. Mandatrophy would apply if the founding problem's conditions changed (e.g., new historical evidence about 7th-century treaties, or a unified Islamic scholarly consensus) — but absent such change, the reading persists as CONTESTED. The classification should reflect this: the reading makes a real claim about Quranic meaning, but the claim is not adjudicated by empirical discovery alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutical_authority_source,
    'What is the source of hermeneutical authority: the literal text itself (with context-limiting constraints), the Quran''s ethical trajectory as inferred from diachronic reading, or divine intention reconstructed from both text and context?',
    'Scholarly consensus within Islamic jurisprudence and Quranic studies; emergence of unified methodological principle for resolving literal vs. arc-based readings across the Quranic corpus.',
    'If literal text with context constraints is authoritative, the contextual-defensive reading prevails. If the ethical trajectory is authoritative, the progressive-synthesis reading prevails. If they must be balanced, a hybrid framework emerges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hermeneutical_authority_source, conceptual, 'Whether hermeneutical authority centers text, arc, or intention.').

omega_variable(
    quranic_ethical_continuity,
    'Does the Quran exhibit a coherent ethical arc (mercy, justice, covenant as primary themes) such that later revelation is understood as synthesizing and deepening earlier revelation rather than abrogating it?',
    'Detailed textual analysis comparing Meccan and Medinan passages on mercy, justice, and treaty obligations; scholarly consensus on whether abrogation doctrine or coherence principle best explains the corpus structure.',
    'Strong coherence evidence supports the progressive-synthesis reading (arc supersedes literal verse). Weak coherence evidence supports the abrogating-universal reading (abrogation is necessary to resolve contradictions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quranic_ethical_continuity, empirical, 'Whether the Quran''s ethical themes form a coherent arc or require abrogation to resolve internal contradictions.').

omega_variable(
    historical_context_authoritativeness,
    'To what extent does accurate historical reconstruction of the 7th-century Medinan context (the treaty-breaking incident, military necessity, political circumstances) bind Islamic law''s interpretation of verse 9:5 in contemporary contexts?',
    'Historical-critical scholarship on the Sira and Quran; legal-philosophical argument about whether law is bound to its origin context or transcends it.',
    'If history strictly bounds interpretation, the contextual-defensive reading holds; the verse''s meaning is fixed by its originating context. If the Quran transcends its context through ethical principle, the progressive-synthesis reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_context_authoritativeness, conceptual, 'Whether historical origin context is binding on legal interpretation or transcended by ethical universals.').

omega_variable(
    false_summit_mountain_ambiguity,
    'Is the Quranic ethical trajectory (mercy, justice, covenant) a natural feature of the text, or a hermeneutical construct benefiting contemporary pluralist and secular frameworks that read it back into the text?',
    'Comparative examination of how different readings (literalist, contextual, progressive) derive their principles from the same textual corpus; detection of whether the arc is discovered in the text or imposed upon it.',
    'If the arc is discovered (natural), the reading is a genuine mountain of textual coherence. If the arc is imposed (constructed), the reading benefits particular frameworks and may itself be extractive under different analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_mountain_ambiguity, conceptual, 'Whether the Quranic ethical trajectory is a natural feature of the text or a beneficiary-serving hermeneutical construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__progressive_synthesis, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__progressive_synthesis, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(qura_tr_t0, projected).
narrative_ontology:measurement(qura_tr_t10, quran_9_5_scope__progressive_synthesis, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(qura_tr_t10, observed).
narrative_ontology:measurement(qura_tr_t20, quran_9_5_scope__progressive_synthesis, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(qura_tr_t20, observed).
narrative_ontology:measurement(qura_tr_t30, quran_9_5_scope__progressive_synthesis, theater_ratio, 30, 0.08).
narrative_ontology:measurement_basis(qura_tr_t30, observed).
narrative_ontology:measurement(qura_tr_t40, quran_9_5_scope__progressive_synthesis, theater_ratio, 40, 0.07).
narrative_ontology:measurement_basis(qura_tr_t40, projected).
narrative_ontology:measurement(qura_tr_t50, quran_9_5_scope__progressive_synthesis, theater_ratio, 50, 0.08).
narrative_ontology:measurement_basis(qura_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__progressive_synthesis, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(qura_be_t0, projected).
narrative_ontology:measurement(qura_be_t10, quran_9_5_scope__progressive_synthesis, base_extractiveness, 10, 0.28).
narrative_ontology:measurement_basis(qura_be_t10, observed).
narrative_ontology:measurement(qura_be_t20, quran_9_5_scope__progressive_synthesis, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(qura_be_t20, observed).
narrative_ontology:measurement(qura_be_t30, quran_9_5_scope__progressive_synthesis, base_extractiveness, 30, 0.18).
narrative_ontology:measurement_basis(qura_be_t30, observed).
narrative_ontology:measurement(qura_be_t40, quran_9_5_scope__progressive_synthesis, base_extractiveness, 40, 0.16).
narrative_ontology:measurement_basis(qura_be_t40, projected).
narrative_ontology:measurement(qura_be_t50, quran_9_5_scope__progressive_synthesis, base_extractiveness, 50, 0.18).
narrative_ontology:measurement_basis(qura_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__progressive_synthesis, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(qura_su_t0, projected).
narrative_ontology:measurement(qura_su_t10, quran_9_5_scope__progressive_synthesis, suppression_requirement, 10, 0.18).
narrative_ontology:measurement_basis(qura_su_t10, observed).
narrative_ontology:measurement(qura_su_t20, quran_9_5_scope__progressive_synthesis, suppression_requirement, 20, 0.14).
narrative_ontology:measurement_basis(qura_su_t20, observed).
narrative_ontology:measurement(qura_su_t30, quran_9_5_scope__progressive_synthesis, suppression_requirement, 30, 0.12).
narrative_ontology:measurement_basis(qura_su_t30, observed).
narrative_ontology:measurement(qura_su_t40, quran_9_5_scope__progressive_synthesis, suppression_requirement, 40, 0.11).
narrative_ontology:measurement_basis(qura_su_t40, projected).
narrative_ontology:measurement(qura_su_t50, quran_9_5_scope__progressive_synthesis, suppression_requirement, 50, 0.12).
narrative_ontology:measurement_basis(qura_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__progressive_synthesis, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__progressive_synthesis, 0.08).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__contextual_defensive).

% DUAL FORMULATION NOTE:
% The kernel quran_9_5_scope decomposes into three structurally distinct constraint stories: abrogating_universal (perpetual offensive duty, high extraction), contextual_defensive (historical defense doctrine, moderate extraction), and progressive_synthesis (ethical trajectory supersedes verse, low extraction). The three readings differ on whether 9:5 remains active constraint, on who is beneficiary/victim, and on how Quranic authority is grounded. Each story is a separate ε-invariant constraint; the network links them as a constraint family because they share a kernel and each reading's authority depends partly on refutation of the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_9_5_scope__progressive_synthesis, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
