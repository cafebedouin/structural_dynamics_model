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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Qiyas and Istihsan as Valid Sources
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   The Hanafi jurisprudential method privileges systematic analogical
 *   reasoning (qiyas) and juristic preference (istihsan) as valid sources of
 *   law alongside textual authority (Quran and Hadith). This reading
 *   instantiates one interpretation of the contested kernel
 *   usul_al_fiqh_method (the foundational principles of Islamic
 *   jurisprudence). The Hanafi school, originating with Abu Hanifa (d. 150 AH
 *   / 767 CE) and systematized by his students and successors, developed the
 *   most expansive framework for qiyas and istihsan among the four major
 *   Sunni schools. This constraint exhibits the structural signature of a
 *   Tangled Rope: it coordinates genuine jurisprudential flexibility
 *   (enabling Islamic law to address novel circumstances) while
 *   simultaneously extracting institutional authority from the requirement
 *   that all jurisprudence be justified through these methods. Rationalist
 *   jurists benefit from the framework's flexibility and institutional
 *   legitimacy; strict textualists bear the cost of having their
 *   methodological objections systematically overridden. The theater ratio
 *   (0.38) reflects that later Hanafi jurisprudence increasingly performs
 *   elaborate qiyas chains that mimic textual reasoning without genuine
 *   analogical force, though the method retains more functional content than
 *   purely theatrical constraints. The suppression requirement (0.45)
 *   indicates moderate institutional enforcement: dissenting jurists must be
 *   managed through textual reinterpretation and institutional pressure, but
 *   the framework retains enough internal coherence to avoid appearing purely
 *   coercive.
 *
 * KEY AGENTS:
 *   - Rationalist Jurists: Primary beneficiary (institutional/arbitrage) — gain institutional authority and methodological flexibility; can shift emphasis between qiyas, istihsan, and textual reasoning as circumstances require
 *   - Strict Textualist Jurists: Primary victim (powerless/trapped) — trapped within a framework that privileges methods they reject; cannot exit without abandoning professional identity and scholarly lineage
 *   - Hanafi School Authority Structure: Institutional actor (institutional/constrained) — maintains legitimacy through qiyas/istihsan framework while extracting authority from the requirement that all jurisprudence be justified through these methods
 *   - Juristic Preference Practitioners: Secondary actor (moderate/constrained) — experience both coordination (method enables flexible jurisprudence) and extraction (must perform legitimacy through textual mimicry)
 *   - Legal Certainty Seekers: Secondary victim (moderate/constrained) — bear the cost of jurisprudential flexibility in the form of reduced predictability and increased discretionary authority
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional arrangement as an inevitable feature of jurisprudence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanafi_reading, 0.35).
domain_priors:suppression_score(hanafi_reading, 0.45).
domain_priors:theater_ratio(hanafi_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanafi_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(hanafi_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(hanafi_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanafi_reading, tangled_rope).
narrative_ontology:human_readable(hanafi_reading, "Hanafi Jurisprudential Method: Qiyas and Istihsan as Valid Sources").
narrative_ontology:topic_domain(hanafi_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hanafi_reading, 'fa883475-2027-48ba-861c-d61bc18e80ce').
narrative_ontology:cs_kernel_codification('fa883475-2027-48ba-861c-d61bc18e80ce', formalized).
narrative_ontology:cs_authority_grounding('fa883475-2027-48ba-861c-d61bc18e80ce', lineage).
narrative_ontology:cs_interpretation_layer_present('fa883475-2027-48ba-861c-d61bc18e80ce').
narrative_ontology:cs_reading_relation('fa883475-2027-48ba-861c-d61bc18e80ce', hanafi_reading__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa883475-2027-48ba-861c-d61bc18e80ce', hanafi_reading__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa883475-2027-48ba-861c-d61bc18e80ce', hanafi_reading__hanbali_reading, influences).
narrative_ontology:cs_axiom('fa883475-2027-48ba-861c-d61bc18e80ce', foundational, juristic_reason_as_legitimate_source).
narrative_ontology:cs_axiom_status(juristic_reason_as_legitimate_source, holdable).
narrative_ontology:cs_axiom_grounding('fa883475-2027-48ba-861c-d61bc18e80ce', juristic_reason_as_legitimate_source, deontological).
narrative_ontology:cs_axiom('fa883475-2027-48ba-861c-d61bc18e80ce', foundational, contextual_adaptation_doctrine).
narrative_ontology:cs_axiom_status(contextual_adaptation_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('fa883475-2027-48ba-861c-d61bc18e80ce', contextual_adaptation_doctrine, instrumental).
narrative_ontology:cs_reference_frame('fa883475-2027-48ba-861c-d61bc18e80ce', juristic_reason_as_legitimate_source).
narrative_ontology:cs_drift_state('fa883475-2027-48ba-861c-d61bc18e80ce', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fa883475-2027-48ba-861c-d61bc18e80ce', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanafi_reading, rationalist_jurists).
narrative_ontology:constraint_beneficiary(hanafi_reading, hanafi_school_institutional_authority).
narrative_ontology:constraint_victim(hanafi_reading, strict_textualist_jurists).
narrative_ontology:constraint_victim(hanafi_reading, legal_certainty_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hanafi_reading, hanafi_school_authority).
narrative_ontology:constraint_beneficiary(hanafi_reading, juristic_preference_practitioners).
narrative_ontology:constraint_victim(hanafi_reading, juristic_preference_practitioners).
narrative_ontology:constraint_vindicates(hanafi_reading, juristic_reason_as_legitimate_source).
narrative_ontology:constraint_vindicates(hanafi_reading, contextual_adaptation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rationalist jurists within the Hanafi school benefit from the qiyas/istihsan framework's flexibility and institutional legitimacy. They can develop jurisprudence that addresses novel circumstances while maintaining scholarly authority. They have high exit optionality: they can shift emphasis between methods, move to other schools, or develop new jurisprudential approaches. Their institutional position depends on the framework's legitimacy, but they are not trapped by it.
narrative_ontology:constraint_stakeholder(hanafi_reading, rationalist_jurists, beneficiary,
    institutional, generational, arbitrage, regional).

% Strict textualist jurists within the Hanafi school bear the cost of having their methodological objections systematically overridden by institutional authority. They cannot exit the framework without abandoning their entire professional identity and scholarly lineage. Their objections to qiyas and istihsan are treated as illegitimate within the Hanafi institutional structure, forcing them to either conform or leave the tradition entirely.
narrative_ontology:constraint_stakeholder(hanafi_reading, strict_textualist_jurists, payer,
    powerless, biographical, trapped, regional).

% The Hanafi school's institutional authority maintains legitimacy through the qiyas/istihsan framework while extracting authority from the requirement that all jurisprudence be justified through these methods. The authority structure sets the agenda for what counts as legitimate jurisprudential reasoning. It benefits from the framework's flexibility and institutional prestige, but cannot easily abandon it without losing legitimacy. It requires active enforcement to manage dissenting jurists and maintain the framework's authority.
narrative_ontology:constraint_stakeholder(hanafi_reading, hanafi_school_authority, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hanafi_reading, hanafi_school_authority, beneficiary).

% Legal certainty seekers (judges, administrators, ordinary believers seeking clear guidance) bear the cost of jurisprudential flexibility in the form of reduced predictability and increased discretionary authority. The qiyas/istihsan framework enables jurists to reach different conclusions in similar cases, creating uncertainty about what the law requires. They face high costs to exit (switching to stricter schools or secular law), but can constrain their reliance on Hanafi jurisprudence by seeking guidance from multiple schools.
narrative_ontology:constraint_stakeholder(hanafi_reading, legal_certainty_seekers, payer,
    moderate, biographical, constrained, regional).

% Juristic preference practitioners experience both coordination and extraction. They benefit from the method's flexibility in enabling contextual jurisprudence, but must perform elaborate justifications of their istihsan decisions through textual mimicry to maintain legitimacy. They are constrained by the need to justify their preferences through qiyas chains and textual reasoning, even when the actual reasoning is intuitive or pragmatic. They can exit to stricter schools but face career costs and loss of institutional position.
narrative_ontology:constraint_stakeholder(hanafi_reading, juristic_preference_practitioners, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hanafi_reading, juristic_preference_practitioners, payer).

% The formalized Hanafi doctrine (the canonical texts and institutional structures that codify the qiyas/istihsan framework) is not an agent but a non-agent entity kept for narrative completeness. It represents the accumulated institutional weight of centuries of jurisprudential tradition. Later jurists perform elaborate justifications of predetermined conclusions through qiyas chains that mimic textual reasoning without genuine analogical force. The doctrine persists through institutional inertia and canonical authority rather than functional necessity.
narrative_ontology:constraint_stakeholder(hanafi_reading, formalized_hanafi_doctrine, observer,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_non_agent(hanafi_reading, formalized_hanafi_doctrine).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Hanafi framework coordinates jurisprudential flexibility: it enables Islamic law to address novel circumstances and changing social conditions while maintaining scholarly legitimacy and connection to textual sources. The genuine coordination problem is how to extend divine law (revealed in specific historical contexts) to new situations that the original texts do not directly address.
% TRANSFER_FUNCTION: The framework transfers institutional authority from strict textualists to rationalist jurists. Rationalist jurists gain the authority to develop jurisprudence through qiyas and istihsan, while strict textualists lose the authority to constrain jurisprudence to literal textual meanings. The framework also transfers jurisprudential discretion from explicit textual rules to implicit juristic reasoning, creating uncertainty about what the law requires in specific cases.
% ABSENT_VOICES: Strict textualists who reject qiyas and istihsan as illegitimate sources are present in the Hanafi tradition but systematically excluded from institutional authority. Their objections are treated as methodologically illegitimate rather than substantively engaged. Ordinary believers seeking clear legal guidance are absent from the jurisprudential conversation — they experience the framework's effects (reduced predictability, increased discretionary authority) but have no voice in determining jurisprudential methodology.
% DISAPPEARANCE_RATIONALE: If the Hanafi framework disappeared overnight, Islamic jurisprudence would rearrange itself significantly. The framework enables the Hanafi school's distinctive jurisprudential approach and institutional authority. Without qiyas and istihsan, the Hanafi school would either collapse into strict textualism (converging with Hanbali methodology) or develop alternative mechanisms for jurisprudential flexibility. The framework's disappearance would eliminate the institutional basis for rationalist jurisprudence within the Hanafi tradition.
% FOUNDING_PROBLEM: The founding problem is how to extend Islamic law to novel circumstances and changing social conditions. Abu Hanifa and his successors developed the qiyas/istihsan framework to address the gap between the specific historical contexts of the original texts and the diverse circumstances of the expanding Islamic empire. The framework enables jurisprudence to adapt to new situations while maintaining connection to textual sources and scholarly legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live and is attested by multiple sources: (1) contemporary Islamic scholars across all schools acknowledge the need for jurisprudential flexibility to address novel circumstances; (2) the historical record shows that all four major Sunni schools developed mechanisms for extending law to new cases, though they differ in methodology; (3) modern Islamic jurisprudence (fiqh al-waqi'a, jurisprudence of contemporary reality) explicitly addresses the problem of applying classical jurisprudence to modern circumstances. The problem's persistence is corroborated by non-Hanafi sources (Maliki, Shafi'i, Hanbali schools) that developed their own solutions, confirming that the problem is genuine rather than a Hanafi invention.
narrative_ontology:disappearance_verdict(hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(hanafi_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRICT TEXTUALIST JURIST (SNARE) — Trapped within a jurisprudential framework that privileges methods they reject as illegitimate. Cannot exit the Hanafi school without abandoning their entire professional identity and scholarly lineage. Bears the cost of having their methodological objections systematically overridden by institutional authority. Maximum experienced extraction — no alternatives available within the tradition.
constraint_indexing:constraint_classification(hanafi_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: JURISTIC PREFERENCE PRACTITIONER (TANGLED ROPE) — Constrained by the need to justify istihsan decisions through elaborate reasoning chains that mimic textual authority. Experiences both coordination (the method enables flexible jurisprudence) and extraction (must perform legitimacy through textual mimicry). Can exit to stricter schools but faces career costs and loss of institutional position.
constraint_indexing:constraint_classification(hanafi_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RATIONALIST JURIST ELITE (ROPE) — Benefits from the method's flexibility and institutional authority. Experiences the constraint as pure coordination: qiyas and istihsan enable jurisprudence to adapt to novel circumstances while maintaining scholarly legitimacy. Net beneficiary with high exit optionality — can shift emphasis between methods as circumstances require.
constraint_indexing:constraint_classification(hanafi_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: HANAFI SCHOOL AUTHORITY STRUCTURE (TANGLED ROPE) — Maintains institutional legitimacy through the qiyas/istihsan framework while extracting authority from the requirement that all jurisprudence be justified through these methods. Requires active enforcement: dissenting jurists must be managed through institutional pressure and textual reinterpretation. Experiences coordination (the method enables coherent jurisprudence) alongside extraction (institutional control over legitimate reasoning).
constraint_indexing:constraint_classification(hanafi_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: FORMALIZED HANAFI DOCTRINE (PITON) — The qiyas/istihsan framework has become increasingly theatrical over centuries. Later jurists perform elaborate justifications of predetermined conclusions through qiyas chains that mimic textual reasoning without genuine analogical force. The method persists through institutional inertia and canonical authority rather than functional necessity. Theater ratio reflects the gap between the method's claimed reasoning power and its actual operation as post-hoc legitimation.
constraint_indexing:constraint_classification(hanafi_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, juristic reasoning necessarily requires methods beyond literal textuality: any legal system must develop mechanisms for analogical extension and contextual judgment. The qiyas/istihsan framework appears as an inevitable feature of jurisprudence itself, not a contingent institutional arrangement. However, the structural data reveals this as a false summit: the specific privilege granted to these methods over others is a contested institutional choice, not a natural law.
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
 *   Extractiveness (0.35): Moderate. The Hanafi framework extracts institutional authority from rationalist jurists' control over jurisprudential methodology, but the extraction is not severe because the framework genuinely enables flexible jurisprudence that addresses real coordination problems. The rationalist jurists' benefits (institutional legitimacy, methodological flexibility) are substantial enough that the constraint functions as coordination alongside extraction. Suppression (0.45): Moderate. Institutional enforcement is required to maintain the framework against textualist objections, but the framework retains enough internal coherence and scholarly legitimacy to avoid appearing purely coercive. Dissenting jurists can be managed through textual reinterpretation and institutional pressure rather than explicit prohibition. Theater ratio (0.38): Moderate. The method retains functional content — qiyas and istihsan do enable genuine jurisprudential reasoning — but later Hanafi jurisprudence increasingly performs elaborate justifications of predetermined conclusions. The theater has increased over the interval as the framework became more formalized and institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals how the same structural arrangement appears as coordination to beneficiaries and extraction to victims. Rationalist jurists see Rope (pure coordination enabling flexible jurisprudence). Strict textualists see Snare (extraction with no exit). Institutional authorities see Tangled Rope (coordination + extraction). The analytical observer risks seeing Mountain (natural law of jurisprudence), but the structural data reveals this as a false summit: the privilege granted to qiyas/istihsan is a contested institutional choice, not an inevitable feature of legal reasoning.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist jurists (institutional/arbitrage) derive low d from beneficiary status + high exit optionality: they can shift emphasis between methods or move to other schools if needed. Strict textualists (powerless/trapped) derive high d from victim status + no exit: they cannot leave the Hanafi framework without abandoning their entire professional identity. Hanafi institutional authorities (institutional/constrained) derive moderate d from beneficiary status + limited exit: they benefit from the framework but cannot easily abandon it without losing institutional legitimacy. The engine computes effective extraction (χ) by applying the sigmoid f(d) to the base extractiveness (ε = 0.35), scaled by scope and directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanafi reading resolves mandatrophy by showing that the constraint's mandate (enabling jurisprudential flexibility to address novel circumstances) remains live and functional. The method genuinely enables Islamic law to adapt to changing circumstances while maintaining scholarly legitimacy. However, the theater ratio (0.38) and rising suppression requirement (0.40 → 0.48) indicate that the method's functional content is being displaced by performative justification. Later Hanafi jurisprudence increasingly performs elaborate qiyas chains that mimic textual reasoning without genuine analogical force. The constraint has not yet reached full mandatrophy (where the original function is entirely atrophied), but the trajectory suggests increasing theatricality and institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qiyas_legitimacy_boundary,
    'What distinguishes valid qiyas (analogical reasoning grounded in textual principles) from invalid qiyas (arbitrary extension of rules to new cases)?',
    'Historical analysis of accepted vs rejected qiyas decisions; examination of the criteria jurists actually use to validate analogies vs stated criteria; comparison with other legal traditions'' analogical reasoning standards',
    'If boundary is clear and consistently applied: qiyas is a genuine coordination mechanism (Rope from more perspectives). If boundary is contested or applied inconsistently: qiyas becomes a cover story for discretionary authority (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_legitimacy_boundary, empirical, 'Criteria distinguishing valid from invalid qiyas').

omega_variable(
    istihsan_as_cover_story,
    'Does istihsan (juristic preference) function as a legitimate exception mechanism for cases where strict qiyas produces unjust results, or as a post-hoc legitimation device for predetermined conclusions?',
    'Textual analysis of istihsan justifications; comparison of istihsan outcomes with qiyas outcomes; examination of whether istihsan decisions precede or follow the reasoning that justifies them',
    'If legitimate exception mechanism: istihsan enables justice-oriented jurisprudence (Rope/Tangled Rope). If post-hoc legitimation: istihsan is pure extraction mechanism (Snare/Piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(istihsan_as_cover_story, empirical, 'Whether istihsan functions as legitimate exception or post-hoc cover').

omega_variable(
    reading_kernel_ambiguity,
    'Is this constraint one reading of a contested kernel (usul_al_fiqh_method), or does the Hanafi method represent a genuine natural law of jurisprudence?',
    'Comparison with other legal traditions'' analogical reasoning mechanisms; examination of whether non-Islamic legal systems develop similar qiyas/istihsan structures independently; analysis of whether the specific Hanafi formulation is contingent or inevitable',
    'If reading of contested kernel: the constraint is a committer-frame artifact; sibling readings (Maliki, Shafi''i, Hanbali) represent genuinely alternative jurisprudential frameworks. If natural law: all legal systems converge on qiyas/istihsan-like mechanisms regardless of tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether this is a reading of a contested kernel or a natural law of jurisprudence').

omega_variable(
    rationalist_beneficiary_capture,
    'Do rationalist jurists benefit from the qiyas/istihsan framework because it enables genuine jurisprudential flexibility, or because it grants them institutional authority to override textualist objections?',
    'Comparison of jurisprudential outcomes under Hanafi vs stricter schools; analysis of whether rationalist jurists'' conclusions differ from what strict textualism would produce; examination of whether rationalist authority is exercised for justice or for institutional control',
    'If genuine flexibility: the framework is Rope (coordination mechanism). If institutional authority: the framework is Snare (extraction mechanism with coordination cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationalist_beneficiary_capture, empirical, 'Whether rationalist beneficiaries gain from flexibility or institutional control').

omega_variable(
    false_summit_naturalization,
    'Is the analytical observer''s mountain classification a genuine natural law (jurisprudence necessarily requires qiyas/istihsan), or a naturalization of a contingent institutional arrangement?',
    'Cross-tradition comparison of analogical reasoning mechanisms; examination of whether non-Islamic legal systems develop qiyas/istihsan independently or through different mechanisms; analysis of whether the Hanafi formulation is the only possible jurisprudential method',
    'If natural law: mountain classification is correct; all legal systems converge on this structure. If naturalization: false summit detected; the constraint is actually Tangled Rope (coordination + extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, empirical, 'Whether mountain classification naturalizes a contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanafi_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanafi_tr_t0, hanafi_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hanafi_tr_t3, hanafi_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement(hanafi_tr_t6, hanafi_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(hanafi_tr_t9, hanafi_reading, theater_ratio, 9, 0.42).

% Extraction over time
narrative_ontology:measurement(hanafi_be_t0, hanafi_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hanafi_be_t3, hanafi_reading, base_extractiveness, 3, 0.31).
narrative_ontology:measurement(hanafi_be_t6, hanafi_reading, base_extractiveness, 6, 0.34).
narrative_ontology:measurement(hanafi_be_t9, hanafi_reading, base_extractiveness, 9, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(hanafi_su_t0, hanafi_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(hanafi_su_t5, hanafi_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(hanafi_su_t10, hanafi_reading, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanafi_reading, identity_coordination).
narrative_ontology:affects_constraint(hanafi_reading, maliki_reading).
narrative_ontology:affects_constraint(hanafi_reading, shafii_reading).
narrative_ontology:affects_constraint(hanafi_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The Hanafi reading is one of four structurally distinct constraints instantiating different readings of the usul_al_fiqh_method kernel. Each reading has its own ε value reflecting the empirical status of the specific jurisprudential framework; the Hanafi reading's ε (0.35) reflects moderate extraction alongside genuine coordination. The sibling readings (Maliki, Shafi'i, Hanbali) have different ε values reflecting their different methodological hierarchies and beneficiary/victim structures. All four readings are linked via network.affects_constraints to enable contamination propagation analysis across the jurisprudential schools.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
