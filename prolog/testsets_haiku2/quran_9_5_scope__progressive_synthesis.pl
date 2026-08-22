% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__progressive_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: quran_9_5_scope__progressive_synthesis
 *   human_readable: Quranic Ethical Trajectory Over Literalist Command (Progressive Reading)
 *   domain: theological/jurisprudential
 *
 * SUMMARY:
 *   Verse 9:5 of the Quran (the Ayat al-Sayf, 'verse of the sword') appears
 *   to command Muslims to fight and kill polytheists wherever they find them.
 *   Under the abrogating_universal reading, this verse abrogates all prior
 *   peaceful verses and establishes permanent offensive jihad as standing
 *   law. Under the contextual_defensive reading, it addresses only the
 *   specific Medinan context of treaty-breaking tribes and does not override
 *   peaceful verses. The progressive_synthesis reading instantiated here
 *   claims the verse is fundamentally time-bound and historical, that the
 *   Quranic ethical trajectory (coexistence, mercy, prohibition of
 *   compulsion) overrides any literalist extraction of 9:5 as binding law,
 *   and that the verse has exited active constraint space entirely—neither
 *   Muslims nor polytheists are presently constrained by it. This reading
 *   benefits secular-pluralist governance frameworks (which require the
 *   de-activation of scriptural commands as standing law) and progressive
 *   Islamic scholars (whose authority depends on contextual hermeneutics),
 *   and imposes costs on textualist authority structures (whose legitimacy
 *   rests on fixed textual meaning and the abrogation doctrine). The
 *   claim/metric gap is deliberate: the constraint is authored as rope (the
 *   coordination it supplies: preserving Islamic textual authority while
 *   rendering it compatible with pluralism), but extractiveness and theater
 *   are minimal because the reading's primary function is NOT to extract but
 *   to de-activate.
 *
 * KEY AGENTS:
 *   - Progressive Islamic scholars: institutional actors who control academic interpretation and propagate contextualist readings
 *   - Textualist authority structures: institutional actors defending fixed textual meaning and the abrogation doctrine
 *   - Secular pluralist frameworks: beneficiary institutional actors whose governance legitimacy depends on de-activating scriptural commands as law
 *   - Textualist Muslim communities: identity-locked payers bearing the cost of delegitimized literalist authority
 *   - Abrogation doctrine proponents: excluded from the progressive frame's conversation because contextualization bypasses the need for nasikh
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__progressive_synthesis, 0.15).
domain_priors:suppression_score(quran_9_5_scope__progressive_synthesis, 0.08).
domain_priors:theater_ratio(quran_9_5_scope__progressive_synthesis, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, extractiveness, 0.15).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__progressive_synthesis, rope).
narrative_ontology:human_readable(quran_9_5_scope__progressive_synthesis, "Quranic Ethical Trajectory Over Literalist Command (Progressive Reading)").
narrative_ontology:topic_domain(quran_9_5_scope__progressive_synthesis, "theological/jurisprudential").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__progressive_synthesis, '24e1af68-01f9-4960-9338-b0938a99c91c').
narrative_ontology:cs_kernel_codification('24e1af68-01f9-4960-9338-b0938a99c91c', fixed_text).
narrative_ontology:cs_authority_grounding('24e1af68-01f9-4960-9338-b0938a99c91c', lineage).
narrative_ontology:cs_interpretation_layer_present('24e1af68-01f9-4960-9338-b0938a99c91c').
narrative_ontology:cs_reading_relation('24e1af68-01f9-4960-9338-b0938a99c91c', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('24e1af68-01f9-4960-9338-b0938a99c91c', quran_9_5_scope__contextual_defensive, influences).
narrative_ontology:cs_axiom('24e1af68-01f9-4960-9338-b0938a99c91c', foundational, quranic_ethical_arc_supreme).
narrative_ontology:cs_axiom_status(quranic_ethical_arc_supreme, holdable).
narrative_ontology:cs_axiom_grounding('24e1af68-01f9-4960-9338-b0938a99c91c', quranic_ethical_arc_supreme, deontological).
narrative_ontology:cs_axiom('24e1af68-01f9-4960-9338-b0938a99c91c', foundational, historical_context_determinative).
narrative_ontology:cs_axiom_status(historical_context_determinative, holdable).
narrative_ontology:cs_axiom_grounding('24e1af68-01f9-4960-9338-b0938a99c91c', historical_context_determinative, conventional).
narrative_ontology:cs_reference_frame('24e1af68-01f9-4960-9338-b0938a99c91c', immutable_divine_text_direct_meaning).
narrative_ontology:cs_drift_state('24e1af68-01f9-4960-9338-b0938a99c91c', contemporary_pluralist_governance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('24e1af68-01f9-4960-9338-b0938a99c91c', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(quran_9_5_scope__progressive_synthesis, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, progressive_islamic_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, textualist_muslim_communities).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, textualist_authority_structures).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, textualist_muslim_communities).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, quranic_ethical_coherence).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, historical_contextuality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and propagate the interpretive framework that reads Verse 9:5 as historically bounded rather than eternally binding. They control academic journals, university positions, and influential commentary traditions within liberal Islamic institutions. They argue the Quranic ethical arc (mercy, coexistence, rejection of compulsion) overrides literalist readings of isolated verses.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, progressive_islamic_scholars, agenda_setter,
    organized, generational, mobile, global).

% Maintain interpretive authority by defending the claim that Verse 9:5 retains active binding force as standing law. Their legitimacy rests partly on textual immutability and the abrogation doctrine (nasikh). The progressive reading undermines their authority structure by relocating legal meaning from the text itself to historical circumstance and ethical trajectory—a move they cannot easily integrate without losing claims to fixed divine law.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, textualist_authority_structures, payer,
    institutional, generational, constrained, global).

% Benefit from the progressive reading insofar as it removes Verse 9:5 from the space of active constraints on Muslim-majority polities and individuals. Secular governance models depend on the de-activation of scriptural commands as standing law; the progressive reading aligns Islamic jurisprudence with that posture without requiring Muslims to abandon the tradition.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks, beneficiary,
    institutional, generational, analytical, global).

% Hold that the traditional nasikh framework (abrogation of earlier peaceful verses by 9:5) is the only Quranic coherence mechanism. They are structurally excluded from the progressive reading's conversation because that reading rejects their coherence premise: the progressive frame treats the apparent conflict as historical rather than logical, bypassing the need for nasikh altogether.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, abrogation_doctrine_proponents, excluded,
    institutional, generational, trapped, global).

% Bears the cost of the progressive reading's delegitimation of literalist authority within their own tradition. They lose the claim that the text speaks directly to present circumstances; interpretation becomes mediated through historical scholarship and ethical reasoning, which requires epistemic resources not all communities access equally. Yet some also benefit from the reading's decoupling of religious identity from political obligation.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, textualist_muslim_communities, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__progressive_synthesis, textualist_muslim_communities, beneficiary).

% Analyze how different faith traditions navigate the tension between scriptural authority and historical context. They document the progressive reading as evidence that Islamic jurisprudence possesses internal mechanisms for rendering ancient commands contextual rather than abrogating them.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, observer_comparative_theologians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__progressive_synthesis, progressive_islamic_scholars).
narrative_ontology:fixing_cost_class(quran_9_5_scope__progressive_synthesis, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of maintaining Islamic textual authority while rendering it compatible with pluralist governance: establishes a hermeneutic framework in which the Quranic ethical trajectory (coexistence, mercy, rejection of compulsion) takes precedence over literalist extraction of isolated commandments as standing law.
% TRANSFER_FUNCTION: Transfers interpretive authority from the text-as-direct-command to the exegetical-scholar-as-mediator; moves legitimacy from immutability to historical contextuality; transfers constraint-space occupation from Verse 9:5 (no longer active) to the ethical meta-principles (which remain binding in principle but not in specific command form).
% ABSENT_VOICES: Textualist jurists and traditionalist communities who hold that the text's meaning is immutable and binding are structurally excluded—the progressive reading rejects their hermeneutic premise (that direct textual meaning can be extracted independent of historical context). They would argue that contextualizing away the verse's binding force is a capitulation to secularism, not an internal Islamic solution.
% DISAPPEARANCE_RATIONALE: If the progressive reading vanished and textualist readings regained interpretive monopoly, Muslim-majority polities would face renewed pressure to justify or implement directives from Verse 9:5 as standing law, and pluralist governance models grounded in Islamic legitimacy would lose their primary reconciliation mechanism. The constraint's absence would not restore the verse to legal force (other institutional, political, and secular mechanisms prevent that), but it would eliminate the internal Islamic defense of its de-activation.
% FOUNDING_PROBLEM: Early Islamic jurisprudence faced the problem of coherent interpretation when isolated verses appeared to command permanent war against non-Muslims, while the broader Quranic message emphasized mercy, coexistence, and freedom of conscience. The abrogation doctrine (nasikh) was one solution; the progressive reading offers another: historical contextualization that preserves textual authority while rendering it non-binding for present application.
% FOUNDING_PROBLEM_CORROBORATION: Progressive Islamic scholars and comparative theologians attest the founding problem is live and the progressive solution is coherent. Textualist authorities attest the founding problem is solved by the abrogation doctrine and that the progressive reading dissolves rather than solves the problem. Secular governance analysts document that the progressive reading has enabled Muslim-majority democracies to remain Islamically legitimate while operating under secular legal codes—corroboration from outside both benefiting and victimized parties.
narrative_ontology:disappearance_verdict(quran_9_5_scope__progressive_synthesis, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__progressive_synthesis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__progressive_synthesis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__progressive_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__progressive_synthesis, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__progressive_synthesis_tests).
:- end_tests(quran_9_5_scope__progressive_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the progressive reading's primary function is coordination—solving the problem of how to maintain Islamic textual authority while rendering ancient commands non-binding—rather than extraction. The beneficiaries (progressive scholars, secular frameworks) gain legitimacy and authority, not rent; the payers (textualist structures, identity-locked communities) lose authority claims and interpretive monopoly, not material resources. Suppression is minimal (0.08) because the reading propagates through persuasion within academic and interpretive institutions rather than through coercion. Theater is low (0.12) because the reading is intellectually substantive (grounded in historical criticism, ethical philosophy, comparative theology) rather than performative. The measurement trajectory shows slight increases over the interval reflecting the reading's growing institutional presence in academic Islamic studies and in progressive Muslim-majority governance, but the metrics stabilize because the reading's core function—the coordination of textual authority with pluralism—is established and does not require intensified extraction or suppression. Accessibility_collapse is low (0.22) because the reading leaves genuine alternatives open: the abrogating_universal reading remains available to those who reject historical contextualization, and the contextual_defensive reading remains available as a middle position.
 *
 * PERSPECTIVAL GAP:
 *   Textualist authority structures and progressive scholars compute extremely different types from the same constraint: the textualist seat experiences the progressive reading as a threat to their interpretive monopoly and thus as extractive in its displacement of their authority; progressive scholars experience it as coordination enabling them to maintain Islamic legitimacy. The payer seat (textualist Muslim communities) sits between: they gain the benefit of pluralist governance compatibility but lose direct access to divine law as they understood it. The secular-pluralist seat experiences near-pure benefit with negligible cost. The engine computes these divergences from the structural data: different power levels (institutional vs. organized vs. moderate), different exit options (constrained vs. mobile vs. identity-locked), and different beneficiary/victim positioning. The authored claim (rope) reflects the progressive scholars' framing; the metrics reflect the actual operational costs and benefits across all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive Islamic scholars benefit from the reading (they gain interpretive authority and institutional presence); secular-pluralist frameworks benefit from it (they gain Islamic legitimacy for pluralist governance); textualist authority structures pay the cost (they lose the claim to immutable textual meaning); identity-locked Muslim communities pay a cost (their direct access to divine law is mediated through exegetical scholarship) but also gain a benefit (their religious identity is now compatible with secular governance). The directionality for the beneficiary seats is near 0.0 (subsidy/benefit); for the payer seats it ranges from 0.5 (symmetric, textualist communities) to 0.7 (institutional textualist structures losing authority). No directionality overrides are needed because the structural derivation from beneficiary/victim + power + exit produces accurate values: progressive scholars hold organized power and mobile exit, so they derive low d; textualist institutions hold institutional power but constrained exit, so they derive high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The progressive reading avoids mandatrophy by solving a genuine coordination problem rather than perpetuating a dead function. The founding problem (how to maintain textual authority while rendering ancient commands non-binding) remains live, the reading solves it coherently, and the payers (textualist structures) cannot simply ignore the reading—they must engage it or lose authority with educated audiences. This is not inert theater; it is active, contested coordination. However, the reading does face a mandatrophy risk if it becomes merely rhetorical: if progressive scholars invoke historical contextualization to dismiss any scriptural constraint as 'just cultural context' without actually engaging the ethical principles the text establishes, then the reading would degrade from coordination to pure extraction (authority without accountability). The low theater_ratio (0.12) suggests the reading has not yet reached that point, but this is an omega-class uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quranic_ethical_trajectory_determinacy,
    'Is the Quranic ethical trajectory sufficiently determinate to override the apparent command of 9:5, or is ''ethical trajectory'' itself an exegetical construction imposed on the text?',
    'Comparative analysis of how different scholarly communities extract ethical principles from the Quran; examination of whether the trajectory-based reading produces convergent results across independent scholars or diverges into multiple ethical narratives.',
    'If trajectory is determinate across independent scholars, the reading has coherent grounding; if it fragments into multiple narratives, the progressive reading risks collapsing into authority-assertion without substantive constraint. High divergence would suggest the reading is more theater than coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quranic_ethical_trajectory_determinacy, conceptual, 'Whether the Quranic ethical trajectory is an objective feature of the text or an exegetical construction.').

omega_variable(
    kernel_reading_asymmetry,
    'Is this reading genuinely a reading of the same kernel as abrogating_universal and contextual_defensive, or does it operate on a different semantic level (meta-interpretation rather than exegesis)?',
    'Linguistic and philosophical analysis of what makes two competing interpretations ''readings of the same thing'': do all three readings directly address the meaning of 9:5, or does the progressive reading address the meta-question of how to interpret scriptural meaning?',
    'If the progressive reading is meta-level, it may be a different constraint entirely (a constraint about constraints), which would require decomposition into separate stories. If it is a direct reading of the verse, the kernel-framing is correct and the three readings form a coherent contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_asymmetry, conceptual, 'Whether the progressive reading addresses the same semantic level as the sibling readings or operates at the meta-level of hermeneutics.').

omega_variable(
    textualist_community_identity_lock,
    'For identity-locked textualist Muslim communities, is the cost of the progressive reading the loss of direct divine command, or the exposure of interpretive pluralism within their own tradition?',
    'Ethnographic and interview-based study of how textualist communities experience the progressive reading: do they report the loss of textual authority, or the destabilization of their hermeneutic monopoly?',
    'If the cost is loss of direct command, the reading''s extraction is from the meaning-making capacity of the community; if the cost is exposure of pluralism, the reading''s extraction is from institutional authority. The directionality and suppression mechanism differ accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_community_identity_lock, empirical, 'What specific aspect of textualist community position the progressive reading threatens.').

omega_variable(
    secular_framework_beneficiary_autonomy,
    'Are secular-pluralist frameworks genuine beneficiaries of the progressive reading, or are they external observers whose interests happen to align with the reading''s output?',
    'Genealogical analysis of the progressive reading''s development: did it emerge from internal Islamic jurisprudential reasoning, or was it constructed in response to secular political pressure? Does it satisfy secular frameworks incidentally or by design?',
    'If designed in response to secular pressure, the reading risks being classified as extractive from the perspective of textualist structures (instrumental subordination to external power). If it emerged internally, it is genuine coordination. The ε value would not change, but the interpretation would.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_framework_beneficiary_autonomy, empirical, 'Whether secular-framework benefit is incidental or intentional in the reading''s development.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__progressive_synthesis, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__progressive_synthesis, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t8, quran_9_5_scope__progressive_synthesis, theater_ratio, 8, 0.06).
narrative_ontology:measurement_basis(qura_tr_t8, observed).
narrative_ontology:measurement(qura_tr_t16, quran_9_5_scope__progressive_synthesis, theater_ratio, 16, 0.09).
narrative_ontology:measurement_basis(qura_tr_t16, observed).
narrative_ontology:measurement(qura_tr_t24, quran_9_5_scope__progressive_synthesis, theater_ratio, 24, 0.11).
narrative_ontology:measurement_basis(qura_tr_t24, observed).
narrative_ontology:measurement(qura_tr_t32, quran_9_5_scope__progressive_synthesis, theater_ratio, 32, 0.12).
narrative_ontology:measurement_basis(qura_tr_t32, observed).
narrative_ontology:measurement(qura_tr_t40, quran_9_5_scope__progressive_synthesis, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(qura_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__progressive_synthesis, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t8, quran_9_5_scope__progressive_synthesis, base_extractiveness, 8, 0.11).
narrative_ontology:measurement_basis(qura_be_t8, observed).
narrative_ontology:measurement(qura_be_t16, quran_9_5_scope__progressive_synthesis, base_extractiveness, 16, 0.13).
narrative_ontology:measurement_basis(qura_be_t16, observed).
narrative_ontology:measurement(qura_be_t24, quran_9_5_scope__progressive_synthesis, base_extractiveness, 24, 0.14).
narrative_ontology:measurement_basis(qura_be_t24, observed).
narrative_ontology:measurement(qura_be_t32, quran_9_5_scope__progressive_synthesis, base_extractiveness, 32, 0.15).
narrative_ontology:measurement_basis(qura_be_t32, observed).
narrative_ontology:measurement(qura_be_t40, quran_9_5_scope__progressive_synthesis, base_extractiveness, 40, 0.15).
narrative_ontology:measurement_basis(qura_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__progressive_synthesis, suppression_requirement, 0, 0.03).
narrative_ontology:measurement_basis(qura_su_t0, observed).
narrative_ontology:measurement(qura_su_t8, quran_9_5_scope__progressive_synthesis, suppression_requirement, 8, 0.04).
narrative_ontology:measurement_basis(qura_su_t8, observed).
narrative_ontology:measurement(qura_su_t16, quran_9_5_scope__progressive_synthesis, suppression_requirement, 16, 0.06).
narrative_ontology:measurement_basis(qura_su_t16, observed).
narrative_ontology:measurement(qura_su_t24, quran_9_5_scope__progressive_synthesis, suppression_requirement, 24, 0.07).
narrative_ontology:measurement_basis(qura_su_t24, observed).
narrative_ontology:measurement(qura_su_t32, quran_9_5_scope__progressive_synthesis, suppression_requirement, 32, 0.08).
narrative_ontology:measurement_basis(qura_su_t32, observed).
narrative_ontology:measurement(qura_su_t40, quran_9_5_scope__progressive_synthesis, suppression_requirement, 40, 0.08).
narrative_ontology:measurement_basis(qura_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__progressive_synthesis, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__progressive_synthesis, 0.12).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__contextual_defensive).

% DUAL FORMULATION NOTE:
% This story is one reading of the kernel quran_9_5_scope. The kernel comprises three structurally distinct constraints: abrogating_universal (Verse 9:5 abrogates peaceful verses and establishes permanent offensive jihad), contextual_defensive (Verse 9:5 addresses only the specific Medinan context and does not override peaceful verses), and progressive_synthesis (Verse 9:5 is time-bound and historically superseded by the Quranic ethical trajectory, exiting active constraint space). Each reading instantiates different ε values, beneficiary/victim structures, and types. The three readings coexist as live positions held by different institutional communities; the network links them as a family rather than a sequence of refinements. The progressive reading influences both siblings by raising the epistemic bar for any claim that 9:5 retains active binding force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
