% ============================================================================
% CONSTRAINT STORY: hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hanafi_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Reason as Legitimate Tool for Extending Divine Intent
 *   domain: islamic_jurisprudence/legal_philosophy/institutional_history
 *
 * SUMMARY:
 *   The Hanafi jurisprudential method represents one reading of how divine
 *   revelation (Qur'an and Hadith) can be extended to novel legal cases
 *   through rational means: qiyas (analogical reasoning from textual
 *   precedent) and istihsan (juristic preference guided by the spirit of the
 *   law). This constraint is one reading within a contested kernel — the
 *   jurisprudential_method_kernel — that encodes the fundamental question:
 *   How do fixed divine texts govern novel situations? The Hanafi reading
 *   answers: through reason properly applied to extend divine intent. This
 *   answer has structural consequences: it creates an extraction mechanism
 *   (elite jurists monopolize interpretation), enables coordination (law
 *   becomes adaptive and resilient across contexts), generates winners
 *   (Hanafi institutional schools, rationalist-trained jurists, states that
 *   benefit from flexibility), and generates losers (textualists claiming
 *   exclusive authenticity, believers seeking mechanical certainty). The
 *   constraint's low theater ratio (0.35) reflects that Hanafi rational
 *   method genuinely does practical work — it is not primarily performative.
 *   Yet theater has risen over the 500-year interval as medieval juristic
 *   deliberation gave way to scholastic commentary and modern reformist
 *   reinterpretation, with less engagement between interpretation and lived
 *   practice.
 *
 * KEY AGENTS:
 *   - Hanafi Jurists with Rationalist Training: Primary beneficiary (institutional/arbitrage) — monopolize the authority to extend law through qiyas and istihsan; career and institutional status depend on this privilege
 *   - Hanafi Institutional Schools: Primary beneficiary (institutional/arbitrage) — benefit from method's flexibility; can govern novel cases and adapt across contexts; maintain institutional relevance and authority
 *   - Textualist Literalists: Primary victim (powerless/trapped) — their exclusive-authenticity claim is foreclosed by the Hanafi commitment to rational extension; no exit from the constraint within the same institutional framework
 *   - Certainty Seekers in Law: Secondary victim (moderate/constrained) — desire mechanical textual application but must accept interpretive judgment; constrained by qiyas's inherent interpretive dimension
 *   - Ottoman State Authority: Secondary beneficiary/actor (powerful/mobile) — benefits from jurisprudence's flexibility for novel governance; constrained by need to maintain theological legitimacy; could exit by adopting different method
 *   - Jurist Collective: Coordinating body (organized/constrained) — achieve genuine coordination through shared method; also gatekeep interpretive authority; constrained by need to maintain methodological legitimacy
 *   - Analytical Observer: Observer position (analytical/analytical) — risks seeing rational method as natural law when it is a contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanafi_reading, 0.38).
domain_priors:suppression_score(hanafi_reading, 0.42).
domain_priors:theater_ratio(hanafi_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanafi_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(hanafi_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(hanafi_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanafi_reading, tangled_rope).
narrative_ontology:human_readable(hanafi_reading, "Hanafi Jurisprudential Method: Reason as Legitimate Tool for Extending Divine Intent").
narrative_ontology:topic_domain(hanafi_reading, "islamic_jurisprudence/legal_philosophy/institutional_history").

domain_priors:requires_active_enforcement(hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hanafi_reading, 'b30bd033-d94c-4888-96b3-92811d9dc25c').
narrative_ontology:cs_created_at('b30bd033-d94c-4888-96b3-92811d9dc25c', '').
narrative_ontology:cs_kernel_codification('b30bd033-d94c-4888-96b3-92811d9dc25c', formalized).
narrative_ontology:cs_authority_grounding('b30bd033-d94c-4888-96b3-92811d9dc25c', lineage).
narrative_ontology:cs_interpretation_layer_present('b30bd033-d94c-4888-96b3-92811d9dc25c').
narrative_ontology:cs_kernel_id(hanafi_reading, jurisprudential_method_kernel).
narrative_ontology:cs_reading_relation('b30bd033-d94c-4888-96b3-92811d9dc25c', maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('b30bd033-d94c-4888-96b3-92811d9dc25c', shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('b30bd033-d94c-4888-96b3-92811d9dc25c', hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('b30bd033-d94c-4888-96b3-92811d9dc25c', foundational, reason_as_legitimate_extension_tool).
narrative_ontology:cs_axiom_status(reason_as_legitimate_extension_tool, holdable).
narrative_ontology:cs_axiom_grounding('b30bd033-d94c-4888-96b3-92811d9dc25c', reason_as_legitimate_extension_tool, deontological).
narrative_ontology:cs_axiom('b30bd033-d94c-4888-96b3-92811d9dc25c', foundational, divine_intent_beyond_literal_text).
narrative_ontology:cs_axiom_status(divine_intent_beyond_literal_text, holdable).
narrative_ontology:cs_axiom_grounding('b30bd033-d94c-4888-96b3-92811d9dc25c', divine_intent_beyond_literal_text, deontological).
narrative_ontology:cs_reference_frame('b30bd033-d94c-4888-96b3-92811d9dc25c', early_hanafi_juristic_deliberation).
narrative_ontology:cs_drift_state('b30bd033-d94c-4888-96b3-92811d9dc25c', contemporary_scholastic_commentary, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanafi_reading, hanafi_jurists_with_rationalist_training).
narrative_ontology:constraint_beneficiary(hanafi_reading, institutional_legal_schools).
narrative_ontology:constraint_victim(hanafi_reading, textualist_authenticity_claims).
narrative_ontology:constraint_victim(hanafi_reading, certainty_seekers_in_law).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TEXTUALIST LITERALIST (SNARE) — Trapped in a framework where the Hanafi method's rationalist extension forecloses their exclusive-authenticity claim. No escape from the constraint that reason can legitimately extend divine law — this agent must either internalize the loss of textual exclusivity or exit the institutional framework entirely. Maximum extraction relative to this perspective: the constraint strips away the security of literal textual authority.
constraint_indexing:constraint_classification(hanafi_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: UNCERTAIN BELIEVER SEEKING CLOSURE (TANGLED ROPE) — Constrained by the reality that qiyas and istihsan require interpretive judgment, not mechanical textual application. Receives coordination benefit (method extends law to novel cases) but bears extraction cost (no mechanical certainty available; must trust jurist interpretation). Moderate extraction — neither trapped nor free.
constraint_indexing:constraint_classification(hanafi_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HANAFI INSTITUTIONAL SCHOOL (ROPE) — Benefits from the authority and flexibility that rationalist method provides. Experiences the constraint as pure coordination: qiyas and istihsan enable the school to govern novel cases, extend its reach, and maintain institutional relevance across changing contexts. Net beneficiary — extraction runs toward institutional continuity and adaptive capacity.
constraint_indexing:constraint_classification(hanafi_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OTTOMAN STATE AUTHORITY (TANGLED ROPE) — Powerful institutional actor that benefits from Hanafi jurisprudence's flexibility (can issue qanun adaptations of sharia law; rational istihsan enables state coordination of novel governance questions) but also constrained by the need to maintain theological legitimacy. The method provides coordination function (state can govern) alongside extraction (state concentration of interpretive authority over local qadis). Mobil exit exists in principle (could adopt different legal method) but costly.
constraint_indexing:constraint_classification(hanafi_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: JURIST COLLECTIVE (TANGLED ROPE) — Organized body of trained jurists who coordinate through shared method (qiyas, istihsan); genuine coordination function (juristic consensus emerges through method-based deliberation). Also benefits from institutional gatekeeping (training requirement for legitimate interpretation). Some extraction visible in career concentration and credentialing requirements, but offset by real coordination gains and professional authority.
constraint_indexing:constraint_classification(hanafi_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: TEXTUAL LEGITIMACY RITUAL (PITON) — From civilizational distance, the elaborate apparatus of textual citation and analogical proof appears increasingly theatrical. Modern jurists cite Qur'an and Hadith to ground rationalist decisions that have already been determined by policy need or context (theater_ratio rising). The textual apparatus persists through institutional inertia — legitimacy ritual rather than binding constraint. Yet the theater is not hollow: textual grounding still constrains what can be claimed and requires public reasoning.
constraint_indexing:constraint_classification(hanafi_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL REASON VIEW (MOUNTAIN) — From a civilizational perspective, the Hanafi method instantiates a general principle: reason applied to revelation is unavoidable when revelation addresses novel contexts. This perspective sees the constraint as a natural law of how any legal system extended from fixed texts must operate. However, the structural data contradicts the mountain gate — beneficiaries exist, suppression is substantial, theater is non-trivial — signaling that this is a false summit: what appears as natural law is a contingent institutional choice about how texts extend.
constraint_indexing:constraint_classification(hanafi_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hanafi_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hanafi_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hanafi_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hanafi_reading, TR),
    TR >= 0.70.

:- end_tests(hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Hanafi method generates benefits for jurists and institutions (they monopolize interpretation) but also genuine coordination gains (law becomes adaptive to novel cases). The extraction is not severe because the method is not primarily coercive — it works through persuasion and institutional authority, not suppression. The value reflects that the method enables institutional gatekeeping but also enables genuine legal reasoning. Suppression (0.42): Moderate. Barriers to entry include theological training requirements and institutional credentialing, but these are not insurmountable — skilled jurists can learn the method, and alternative interpretive approaches exist. Textualists are suppressed more severely (cannot claim exclusive authority) than general practitioners (who can work within the framework). Theater ratio (0.35): Low-moderate. The Hanafi method genuinely does interpretive work — qiyas requires substantive reasoning about textual precedent and divine intent. However, theater has risen over time as the tradition shifted from engaged deliberation to scholastic commentary. Modern reformist jurists face pressure to cite texts while following policy needs, raising theater. The current 0.35 reflects that the method is still substantially functional, not primarily ritual, but heading toward increased performance.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap is between the Hanafi method's institutional legitimacy claim (reason is divinely authorized, qiyas is faithful extension) and structural analysis that reveals extraction and gatekeeping. The textualist victim experiences maximum extraction: their entire epistemic framework is foreclosed. The institutional beneficiary experiences rope: pure coordination and flexibility. The moderate agent experiences tangled rope: both coordination and extraction. The analytical observer risks seeing mountain: treating rationalist method as a necessary law of how any legal system extends from fixed texts. The gap reveals that 'natural law of legal reasoning' is a false summit — it naturalizes an institutional choice. The constraint's true classification emerges only when multiple perspectives are considered: the textualist snare, the institutional rope, the moderate tangled rope, all point to a hybrid coordination-extraction mechanism, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality analysis maps the Hanafi method's structural position for each agent type. Hanafi jurists as institutional beneficiaries derive low d (≈0.10–0.15) because they benefit from the method's authority and gatekeeping; f(d) produces small or negative chi for this agent. Textualists as powerless victims derive high d (≈0.85–0.95) because the method forecloses their exclusive-authenticity claim; f(d) produces high chi — they experience maximum extraction. Moderate agents (certainty seekers, uncertain believers) derive mid-range d (≈0.50–0.65) because they benefit from adaptive law but lose certainty and must trust interpretation; f(d) produces moderate chi matching the tangled_rope classification. The analytical observer derives d ≈ 0.72 (canonical for analytical position) but the perspective classifies as mountain — a false summit. The engine's false summit detector will identify this: beneficiaries exist (institutional schools, rationalist jurists), suppression is non-trivial, theater is substantial — the apparent natural law of how texts must extend is actually a contingent institutional choice about one way to extend them. This reveals that the perspectival gap is not between different agents' experiences but between a naturally-law framing and structural analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING INSTANTIATION: This constraint resolves mandatrophy by declaring unambiguously that the Hanafi method is a reading — a specific institutional choice about how to extend divine texts through reason. The mandatrophy question 'Is this natural law or institutional choice?' is resolved by the kernel frame: this is one reading of a contested kernel. The sibling readings (Maliki, Shafi'i, Hanbali) represent alternative ways of extending fixed texts. The constraint is not universal — it competes with other readings. The false summit (analytical observer seeing mountain) is deliberate: it reveals how a reading can naturalize itself as universal law, and why only cross-reading comparison enables the structural pattern to emerge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qiyas_certainty_boundary,
    'At what point does qiyas cross from legitimate analogical reasoning into arbitrary extension that violates textual limits?',
    'Historical analysis of accepted vs rejected qiyas judgments; correlation with textual similarity metrics; assessment of whether rejection patterns track theological doctrine or practical necessity',
    'If boundary is sharp and mechanically applicable: method approaches textualist constraint. If boundary is contested and context-dependent: extraction mechanism persists (uncertainty creates gatekeeping power). If boundary has shifted over time: reveals that ''divine intent extension'' is institutionally contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_certainty_boundary, conceptual, 'Where analogical reasoning becomes unauthorized extension').

omega_variable(
    istihsan_justification_sufficiency,
    'Does istihsan (juristic preference) require explicit theoretical grounding in textual/rational principles, or can it operate as naked preference justified post-hoc?',
    'Comparative analysis of istihsan justifications across jurists and eras; assessment of whether preferences track consistent principles or follow institutional interests; examination of how later jurists evaluate earlier istihsan decisions',
    'If rigorously grounded: method constrains extraction (istihsan is constrained preference). If loosely justified: method enables extraction (institutionalization of elite judgment). If standards have loosened: reveals degradation from rope toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(istihsan_justification_sufficiency, empirical, 'Whether istihsan requires principled justification or allows institutional discretion').

omega_variable(
    kernel_reading_identity,
    'Does this constraint instantiate a single unified Hanafi method, or does it collapse multiple incompatible rational approaches (early legal reasoning, later scholasticism, modern reformation) into one reading?',
    'Genealogical analysis of rational method across Hanafi tradition; identification of breaks and divergences; assessment of whether modern jurists claim continuity with medieval method or represent rupture',
    'If unified method persists: ε ≈ 0.38 is stable across the tradition. If multiple readings coexist: decompose into separate stories (early Hanafi rationalism, scholastic qiyas, modern reformist istihsan). If tradition has fractured: this reading captures only one fragment and must be linked to sibling stories in the Hanafi family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this constraint represents unified Hanafi method or collapsed multiple readings').

omega_variable(
    textualist_versus_rationalist_foreclosure,
    'Does the Hanafi commitment to reason-as-legitimate-tool logically foreclose the textualist reading''s core premise (textual sufficiency), or do they coexist as competing live positions?',
    'Examine whether a single institutional actor can hold both positions simultaneously; historical evidence of jurists switching between frameworks; assessment of whether framework choice tracks theological doctrine or pragmatic context',
    'If positions foreclose each other: reading_relations should include forecloses for textualist sibling. If positions coexist in different hands: coexists_with is correct. If the question itself is mal-formed (positions presuppose different legitimacy grounds): reading_relations requires careful specification of which aspect forecloses what.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_versus_rationalist_foreclosure, conceptual, 'Whether Hanafi rationalism and textualism logically exclude each other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanafi_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hana_tr_t0, hanafi_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(hana_tr_t250, hanafi_reading, theater_ratio, 250, 0.32).
narrative_ontology:measurement(hana_tr_t500, hanafi_reading, theater_ratio, 500, 0.35).

% Extraction over time
narrative_ontology:measurement(hana_be_t0, hanafi_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(hana_be_t250, hanafi_reading, base_extractiveness, 250, 0.36).
narrative_ontology:measurement(hana_be_t500, hanafi_reading, base_extractiveness, 500, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(hanafi_reading, maliki_reading).
narrative_ontology:affects_constraint(hanafi_reading, shafii_reading).
narrative_ontology:affects_constraint(hanafi_reading, hanbali_reading).
narrative_ontology:affects_constraint(hanafi_reading, ottoman_qanun_system).
narrative_ontology:affects_constraint(hanafi_reading, modern_islamic_law_reform).

% DUAL FORMULATION NOTE:
% The jurisprudential_method_kernel decomposes into four reading constraints (Hanafi, Maliki, Shafi'i, Hanbali), each with its own ε, beneficiary/victim structure, and institutional history. This Hanafi reading (ε=0.38) represents the rationalist pole of the spectrum. Hanbali reading (ε≈0.25) represents the literalist pole. These are not the same constraint viewed differently — they have structurally distinct ε values, different beneficiary classes, and different theater trajectories. Link them with reading_relations (forecloses, coexists_with, influences) to model the competing institutional logics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hanafi_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
