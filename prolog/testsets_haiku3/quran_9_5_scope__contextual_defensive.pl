% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__contextual_defensive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__contextual_defensive, []).

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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Qur'an 9:5 Contextual Defensive Interpretation
 *   domain: religious/political
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   qur'an_9_5_scope. The reading asserts that Verse 9:5 (often cited as
 *   authorizing unlimited offensive warfare) is contextually bound to
 *   7th-century treaty violations and does not abrogate prior Qur'anic
 *   emphases on covenant obligation, mercy, and defensive engagement. The
 *   reading provides hermeneutical scaffolding for Muslim-majority states
 *   seeking legitimacy for peaceful pluralism and international law
 *   compliance. Its beneficiaries are institutional actors (integrationist
 *   states) and scholarly movements (coexistence hermeneuticists) seeking to
 *   anchor pluralism in scriptural tradition rather than bracketing
 *   tradition. The constraint's extractiveness is LOW because it does not
 *   extract value from subordinate parties—it coordinates hermeneutical
 *   authority around a framework that protects coexistence norms. Suppression
 *   is low because the reading does not require active suppression of
 *   alternatives; it operates through reinterpretation and scholarly debate.
 *
 * KEY AGENTS:
 *   - integrationist_muslim_states: Institutional beneficiary (d ≈ 0.2); governments of Muslim-majority nations seeking scriptural warrant for pluralistic coexistence
 *   - coexistence_scholars: Organized beneficiary (d ≈ 0.35); ulama and academics emphasizing contextual exegesis and harmony with pluralism
 *   - abrogation_doctrine_holders: Excluded party (d ≈ 0.85); scholars holding that 9:5 abrogates peace verses; their hermeneutical premises are rejected outright
 *   - armed_jihadist_movements: Excluded party (d ≈ 0.90); non-state actors claiming 9:5 as warrant for offensive action; this reading denies them scriptural foundation
 *   - pluralist_nonmuslim_polities: Secondary beneficiary (d ≈ 0.15); non-Muslim institutional actors benefit from Muslim-majority commitment to peaceful interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.28).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.15).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.28).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Qur'an 9:5 Contextual Defensive Interpretation").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "religious/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, '969456be-d389-4b8c-841b-baa0dcd7c227').
narrative_ontology:cs_kernel_codification('969456be-d389-4b8c-841b-baa0dcd7c227', fixed_text).
narrative_ontology:cs_authority_grounding('969456be-d389-4b8c-841b-baa0dcd7c227', lineage).
narrative_ontology:cs_interpretation_layer_present('969456be-d389-4b8c-841b-baa0dcd7c227').
narrative_ontology:cs_reading_relation('969456be-d389-4b8c-841b-baa0dcd7c227', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('969456be-d389-4b8c-841b-baa0dcd7c227', quran_9_5_scope__progressive_synthesis, coexists_with).
narrative_ontology:cs_axiom('969456be-d389-4b8c-841b-baa0dcd7c227', foundational, contextual_revelation_principle).
narrative_ontology:cs_axiom_status(contextual_revelation_principle, holdable).
narrative_ontology:cs_axiom_grounding('969456be-d389-4b8c-841b-baa0dcd7c227', contextual_revelation_principle, empirically_contingent).
narrative_ontology:cs_axiom('969456be-d389-4b8c-841b-baa0dcd7c227', foundational, treaty_obligation_supremacy).
narrative_ontology:cs_axiom_status(treaty_obligation_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('969456be-d389-4b8c-841b-baa0dcd7c227', treaty_obligation_supremacy, deontological).
narrative_ontology:cs_reference_frame('969456be-d389-4b8c-841b-baa0dcd7c227', covenant_obligation_non_aggression_doctrine).
narrative_ontology:cs_drift_state('969456be-d389-4b8c-841b-baa0dcd7c227', contemporary_pluralist_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('969456be-d389-4b8c-841b-baa0dcd7c227', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, coexistence_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, pluralist_nonmuslim_polities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governments of Muslim-majority nations seeking legitimacy for pluralistic coexistence with non-Muslim populations and treaty-bound neighbors. This reading provides hermeneutical cover for statecraft that prioritizes treaty obligations and defensive security doctrine over expansionist interpretations. Benefits from having a coherent Islamic jurisprudential foundation for peaceful foreign policy.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_states, beneficiary,
    institutional, generational, mobile, national).

% Ulama and Islamic scholars emphasizing harmonization of scriptural tradition with pluralist ethics and international law. This reading provides scholarly authority for their teaching and hermeneutical methodology. Constrains them because their exit from interpretive tradition (abandoning textual engagement) is socially costly, but within the tradition this reading amplifies their voice.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, coexistence_scholars, beneficiary,
    organized, biographical, constrained, global).

% The documented historical record of 7th-century Medina: tribal confederacy agreements, broken treaties by Quraysh polytheist groups, specific military engagements, chronological placement of revelations. This reading's evidentiary foundation. Not an actor but the epistemic substrate upon which the reading rests.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, historical_treaty_context, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(quran_9_5_scope__contextual_defensive, historical_treaty_context).

% Scholars and jurists holding that Verse 9:5 abrogates (nasikh) prior peaceful verses and establishes universal offensive jihad. They are excluded from this reading's framework—their hermeneutical premises are treated as incoherent within contextual exegesis. They would argue this reading waters down a binding command; they are not consulted in its construction.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, abrogation_doctrine_holders, excluded,
    organized, generational, constrained, global).

% Non-state armed groups claiming Islamic justification for offensive operations. This reading denies them scriptural warrant: it rebuts their citation of 9:5 as an abiding command by establishing it as contextual and defensive only. They are excluded because their theological premises (that 9:5 abrogates peace doctrine) are rejected outright.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, armed_jihadist_movements, excluded,
    moderate, biographical, trapped, regional).

% Non-Muslim-majority nations and pluralist international order. This reading strengthens the hermeneutical foundation for Muslim-majority states' commitment to treaty obligations and non-aggression, which benefits pluralist international governance. They are beneficiaries in that the constraint legitimizes peaceful conduct by Muslim-majority parties; they are observers in that they do not author the reading.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, pluralist_nonmuslim_polities, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__contextual_defensive, pluralist_nonmuslim_polities, observer).

% Scholars and communities emphasizing minimal interpretive addition to scriptural text, treating verse as standing command detached from historical circumstance. They observe this constraint as a rejection of their hermeneutical method. They are not attacked but their methodology is implicitly declared insufficient.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, literalist_scriptural_interpreters, observer,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__contextual_defensive, integrationist_muslim_states).
narrative_ontology:fixing_cost_class(quran_9_5_scope__contextual_defensive, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides unified hermeneutical framework for Muslim majority-states to coordinate peaceful coexistence with non-Muslim populations and treaty-bound rivals while maintaining scriptural fidelity. Solves the alignment problem: how to honor both Qur'anic authority and commitments to pluralism and international law without treating them as contradictory.
% TRANSFER_FUNCTION: Transfers interpretive authority from literalist readings emphasizing abrogation and universality to contextual, historical-exegetical readings emphasizing covenant obligation and circumstantial application. Moves the burden of proof onto claims of eternal command: they must now establish that historical context does not limit application.
% ABSENT_VOICES: Armed jihadist groups and abrogation-doctrine holders are structurally excluded from this reading's construction. They would argue that contextual exegesis is rationalist departure from binding text; their objections are not incorporated into the framework but are treated as methodologically incoherent.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, integrationist Muslim states would lose hermeneutical scaffolding but would likely adopt parallel justifications (necessity doctrine, maqasid-based reasoning, or explicit jurisprudential innovation). The political function—peaceful pluralism—does not depend on this one reading. However, the scholarly consensus favoring contextual exegesis would suffer strategic loss, and jihadist movements would face less counterargument within Islamic tradition.
% FOUNDING_PROBLEM: How to interpret Verse 9:5 in a way consistent with both scriptural authority and historical evidence that the verse addressed a specific treaty-violation crisis, without abandoning textual engagement or adopting rationales that contradict other Qur'anic emphases on covenant obligation and mercy.
% FOUNDING_PROBLEM_CORROBORATION: Islamic history scholars (Tabari, Ibn Kathir, modern historians of Medina) attest to the specific treaty-breaking context; comparative Qur'anic exegesis confirms the pattern of contextual revelation; international relations scholars from outside Islamic tradition corroborate that the 7th-century crisis was militarily and diplomatically specific. Scholars committed to pluralism (Abdullahi Ahmed An-Na'im, Khaled Abou El Fadl, others from outside the benefiting state apparatus) affirm contextual exegesis as methodology.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, contested).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__contextual_defensive, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__contextual_defensive_tests).
:- end_tests(quran_9_5_scope__contextual_defensive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28 at interval end) because the constraint does not extract from a subordinate population but coordinates hermeneutical authority. No group bears costs imposed by this reading; costs are borne only by those whose hermeneutical methods are rejected (abrogation-doctrine holders, literalists), but rejection of a methodology is not extraction from a victim. Suppression is low (0.15) because the reading gains no active coercive machinery—it works through scholarly debate, textual exegesis, and institutional adoption of interpretive frameworks. Theater ratio is low (0.12) because the constraint's function is genuine: contextual exegesis solves a real alignment problem for pluralist states. The measurement series tracks the reading's stabilization over time as it moves from nascent scholarship (t=0) through institutional adoption (t=10+), plateauing as consensus coheres. No cyclical oscillation is present; the trajectory is monotonic shallow growth then stability.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (integrationist states, coexistence scholars), this constraint is ROPE: genuine coordination solving a real problem (how to honor both scriptural authority and pluralism). From the excluded seats (abrogation holders, jihadists), it is SNARE: a hermeneutical cage preventing them from accessing scriptural warrant they believe is rightfully theirs. From the analyst seat, it is ROPE: the constraint does coordinate—it does solve the alignment problem—but it does so by reinterpreting the text rather than by distributing new resources or imposing new costs. The engine should compute coordination-type classification from the beneficiary seats and measure exclusion from the excluded seats as a network effect (not as extraction per se, but as relationship conflict). The perspectival gap arises because excluded parties experience the constraint as access-denial (snare-like) even though the beneficiary parties are not extracting from them.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading has ASYMMETRIC directionality by seat. (1) Integrationist states approach d ≈ 0.2 (mild beneficiary): they benefit from hermeneutical scaffolding but are not trapped or extracted from—they can adopt other rationales (necessity, jurisprudential innovation) if this reading disappeared. (2) Coexistence scholars approach d ≈ 0.35 (symmetric, slightly beneficiary): they gain interpretive authority and teaching credibility, but they are identity-locked by commitment to textual engagement—exiting would require abandoning the scholarly tradition itself. (3) Excluded parties (abrogation holders, jihadists) are NOT extraction targets; they are excluded from the framework's construction. Exclusion is not extraction. Their high d values (0.85+) reflect that they would classify this constraint as hostile to their interests, not that they bear costs it imposes. The absence of a victim set is structural: the constraint coordinates around a hermeneutical choice; it does not impose costs on anyone. The measurement of low extractiveness reflects this: no value flows from subordinate to dominant parties because the constraint does not establish a dominant-subordinate relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandate is LIVE: it remains functionally necessary for integrationist states to coordinate peaceful pluralism with scriptural fidelity. Unlike a piton (atrophied mandate, theatrical maintenance), this reading is actively refined and deployed by state and scholarly institutions. However, a mandatrophy alert IS warranted if historical analysis shows the constraint's function is shifting: if the reading is increasingly used as a pretext to suppress alternative interpretations (without textual debate) rather than as a genuine hermeneutical framework, theater_ratio would rise and extractiveness would climb as the constraint becomes a cover story for exclusion. Current measurements show stable theater and minimal extractiveness, suggesting the mandate remains genuine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_context_sufficiency,
    'Does the historical record of 7th-century Medina sufficiently establish that Verse 9:5 was revealed in response to specific treaty violations, or is the historical context reconstructed from later exegetical tradition?',
    'Comparative analysis of Hadith chains (isnad) and Tafsir sources against independently dated historical records; archaeological and epigraphic evidence of Medinan treaties and their violations; scholarly consensus on dating of revelations.',
    'If the historical context is well-established from early sources independent of exegetical tradition, the reading gains epistemically robust foundation. If the context is largely reconstructed by later tradition, the reading''s claim to historical limitation becomes more vulnerable to the charge of retroactive rationalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_context_sufficiency, empirical, 'Whether the historical specificity of 9:5''s revelation can be established independently of exegetical tradition.').

omega_variable(
    abrogation_doctrine_status,
    'Does the principle of abrogation (nasikh) validly apply to Verse 9:5, or is the concept of abrogation itself a later jurisprudential invention not clearly rooted in Qur''anic self-reference?',
    'Qur''anic textual analysis for explicit self-reference to abrogation; historical tracing of nasikh doctrine emergence in early jurisprudence; comparison with Jewish and Christian hermeneutical traditions to assess whether abrogation is theologically necessary or contingent.',
    'If abrogation is demonstrated as textually grounded and historically early, abrogating_universal reading gains methodological support. If abrogation emerges as later jurisprudential invention, contextual_defensive reading is strengthened (the verse need not be read as abrogating because abrogation doctrine itself is not mandated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_doctrine_status, empirical, 'Whether abrogation doctrine is Qur''anically grounded or jurisprudentially constructed.').

omega_variable(
    defensive_warfare_definition,
    'What constitutes ''defensive warfare'' in Qur''anic jurisprudence? Are preemptive strikes against imminent threats defensive, or only reactive strikes after attack?',
    'Survey of classical and modern Islamic law scholarship on qital (fighting) classifications; analysis of how Muslim-majority states operationalize the defensive/offensive distinction in military doctrine; examination of whether contextual reading supports preemptive defense or only reactive response.',
    'A narrow definition of defensive (reactive only) strengthens contextual_defensive reading and limits justifications for military action. A broader definition (including preemptive defense against credible threat) opens space for states to justify more expansive military operations under the defensive frame, weakening the constraint''s effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defensive_warfare_definition, conceptual, 'The definitional boundary between defensive and offensive warfare within Islamic jurisprudence.').

omega_variable(
    institutional_capture_of_hermeneutics,
    'To what extent is the contextual-defensive reading adopted by integrationist states as genuine hermeneutical conviction versus instrumental cover for political interests that would persist regardless of scriptural interpretation?',
    'Post-exit behavior analysis: do states adopting this reading maintain peaceful pluralism when political incentives shift? Consistency check: do states consistently apply contextual exegesis to other verses or only to 9:5? Documentary evidence of whether adoption preceded or followed political reorientation toward pluralism.',
    'If the reading is genuine conviction, its spreading stabilizes peaceful norms. If it is instrumental cover, institutional adoption does not constrain future belligerence—the reading becomes a snare (cover story for politics-as-usual) rather than rope (genuine coordination). High institutional capture would warrant upgrade of extractiveness and addition of a suppression dynamic (suppression of alternative readings to prevent exposure of instrumentality).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_capture_of_hermeneutics, empirical, 'Whether institutional adoption of contextual-defensive reading reflects hermeneutical conviction or political convenience.').

omega_variable(
    method_counterfactual_foreclosure,
    'Does the contextual-defensive reading logically foreclose the abrogating_universal reading, or do the two readings represent incommensurable methodological choices (contextual exegesis vs. literal application) that allow both to coexist as live positions?',
    'Logical analysis: can a party hold both ''this verse is contextually bound'' AND ''this verse abrogates prior verses'' in the same coherent framework? Or do they require mutually exclusive premises about textual authority, historical contingency, and legal obligation?',
    'If the readings logically foreclose each other (a single coherent framework cannot hold both), the relation is forecloses. If they represent different methodological families (contextualists vs. literalists, each internally coherent), the relation is coexists_with. Foreclosure would suggest one reading will eventually dominate; coexistence suggests long-term interpretive pluralism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(method_counterfactual_foreclosure, conceptual, 'Whether contextual and abrogating readings are logically foreclosing or incommensurable methodologies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__contextual_defensive, theater_ratio, 0, 0.08).
narrative_ontology:measurement(qura_tr_t5, quran_9_5_scope__contextual_defensive, theater_ratio, 5, 0.09).
narrative_ontology:measurement(qura_tr_t10, quran_9_5_scope__contextual_defensive, theater_ratio, 10, 0.11).
narrative_ontology:measurement(qura_tr_t15, quran_9_5_scope__contextual_defensive, theater_ratio, 15, 0.12).
narrative_ontology:measurement(qura_tr_t20, quran_9_5_scope__contextual_defensive, theater_ratio, 20, 0.12).
narrative_ontology:measurement(qura_tr_t25, quran_9_5_scope__contextual_defensive, theater_ratio, 25, 0.12).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__contextual_defensive, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(qura_be_t5, quran_9_5_scope__contextual_defensive, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(qura_be_t10, quran_9_5_scope__contextual_defensive, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(qura_be_t15, quran_9_5_scope__contextual_defensive, base_extractiveness, 15, 0.27).
narrative_ontology:measurement(qura_be_t20, quran_9_5_scope__contextual_defensive, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(qura_be_t25, quran_9_5_scope__contextual_defensive, base_extractiveness, 25, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__contextual_defensive, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(qura_su_t5, quran_9_5_scope__contextual_defensive, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(qura_su_t10, quran_9_5_scope__contextual_defensive, suppression_requirement, 10, 0.14).
narrative_ontology:measurement(qura_su_t15, quran_9_5_scope__contextual_defensive, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(qura_su_t20, quran_9_5_scope__contextual_defensive, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(qura_su_t25, quran_9_5_scope__contextual_defensive, suppression_requirement, 25, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__contextual_defensive, 0.06).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% This constraint is part of the qur'an_9_5_scope constraint family. The family decomposes a single Qur'anic verse into three structurally distinct readings with different ε values, victim sets, and beneficiary structures. Contextual_defensive (this story) emphasizes historical specificity and treaty obligation, yielding low extraction and no victim set. Abrogating_universal reads the same verse as abrogating prior peace doctrine and establishing universal jihad obligation, yielding high extraction and broad victim set (all non-Muslims). Progressive_synthesis treats 9:5 as time-bound political directive, superseded by Qur'anic ethical trajectory, yielding high extraction for traditionalist scholars but benefiting modernist reformers. Each reading is a coherent constraint with its own ε, stakeholder configuration, and classification. Links via network.affects_constraints represent interpretive dependency: the historical-context claim of contextual_defensive influences (and may foreclose) abrogating_universal's universality premise; progressive_synthesis's ethical-trajectory claim influences both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_9_5_scope__contextual_defensive, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
