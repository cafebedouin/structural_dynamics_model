% ============================================================================
% CONSTRAINT STORY: shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shafii_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: shafii_reading
 *   human_readable: Shafi'i Jurisprudential Method: Hadith Hierarchy and Source Grounding
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   The Shafi'i jurisprudential method represents one reading of a contested
 *   kernel in Islamic legal theory: the question of how jurisprudence should
 *   be grounded and what sources are legitimate. This reading insists on
 *   explicit hadith hierarchy and rejects juristic preference mechanisms
 *   (istihsan) and public interest reasoning (maslaha) that are not directly
 *   grounded in transmitted sources. The constraint exhibits tangled_rope
 *   structure: it coordinates jurisprudential practice within the Shafi'i
 *   school (genuine coordination function) while extracting authority from
 *   competing traditions that rely on customary practice and juristic
 *   reasoning. The method requires active enforcement through institutional
 *   teaching, textual transmission, and scholarly legitimacy claims.
 *   Customary practitioners and juristic preference traditions bear the
 *   extraction cost through delegitimization of their methods. Hadith
 *   scholars benefit from methodological primacy. The analytical observer
 *   risks naturalizing this institutional choice as a law of jurisprudence
 *   itself — a false summit that obscures the beneficiary structure.
 *
 * KEY AGENTS:
 *   - Hadith Scholars: Primary beneficiary (institutional/arbitrage) — gain methodological authority and gatekeeping power over jurisprudential legitimacy
 *   - Customary Practitioners: Primary victim (powerless/trapped) — local judges and muftis operating within established practice find their reasoning delegitimized; cannot exit without abandoning institutional identity
 *   - Juristic Preference Traditions: Secondary victim (moderate/constrained) — schools relying on istihsan and maslaha face systematic delegitimization; can theoretically migrate to hadith-based methods but at cost of abandoning foundational commitments
 *   - Shafi'i School Institutional Structure: Organized beneficiary (organized/constrained) — coordinates jurisprudential practice while extracting authority from competing schools; requires active enforcement
 *   - Islamic Legal Tradition: Institutional actor (institutional/constrained) — coordinates jurisprudential pluralism while the Shafi'i method extracts authority through claims of methodological superiority
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice as inherent requirement of rational jurisprudence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shafii_reading, 0.35).
domain_priors:suppression_score(shafii_reading, 0.48).
domain_priors:theater_ratio(shafii_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shafii_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(shafii_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(shafii_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shafii_reading, tangled_rope).
narrative_ontology:human_readable(shafii_reading, "Shafi'i Jurisprudential Method: Hadith Hierarchy and Source Grounding").
narrative_ontology:topic_domain(shafii_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shafii_reading, '2849f060-1c2a-4e37-a012-03a372f24949').
narrative_ontology:cs_kernel_codification('2849f060-1c2a-4e37-a012-03a372f24949', formalized).
narrative_ontology:cs_authority_grounding('2849f060-1c2a-4e37-a012-03a372f24949', lineage).
narrative_ontology:cs_interpretation_layer_present('2849f060-1c2a-4e37-a012-03a372f24949').
narrative_ontology:cs_reading_relation('2849f060-1c2a-4e37-a012-03a372f24949', shafii_reading__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('2849f060-1c2a-4e37-a012-03a372f24949', shafii_reading__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('2849f060-1c2a-4e37-a012-03a372f24949', shafii_reading__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('2849f060-1c2a-4e37-a012-03a372f24949', foundational, hadith_hierarchy_requirement).
narrative_ontology:cs_axiom_status(hadith_hierarchy_requirement, holdable).
narrative_ontology:cs_axiom_grounding('2849f060-1c2a-4e37-a012-03a372f24949', hadith_hierarchy_requirement, empirically_contingent).
narrative_ontology:cs_axiom('2849f060-1c2a-4e37-a012-03a372f24949', foundational, istihsan_maslaha_rejection).
narrative_ontology:cs_axiom_status(istihsan_maslaha_rejection, holdable).
narrative_ontology:cs_axiom_grounding('2849f060-1c2a-4e37-a012-03a372f24949', istihsan_maslaha_rejection, deontological).
narrative_ontology:cs_reference_frame('2849f060-1c2a-4e37-a012-03a372f24949', transmitted_source_primacy).
narrative_ontology:cs_drift_state('2849f060-1c2a-4e37-a012-03a372f24949', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2849f060-1c2a-4e37-a012-03a372f24949', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shafii_reading, hadith_scholars).
narrative_ontology:constraint_beneficiary(shafii_reading, transmitted_source_authority).
narrative_ontology:constraint_victim(shafii_reading, customary_practitioners).
narrative_ontology:constraint_victim(shafii_reading, juristic_preference_traditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shafii_reading, juristic_preference_traditions).
narrative_ontology:constraint_beneficiary(shafii_reading, shafii_school_institution).
narrative_ontology:constraint_beneficiary(shafii_reading, islamic_legal_tradition).
narrative_ontology:constraint_vindicates(shafii_reading, hadith_corpus_primacy).
narrative_ontology:constraint_vindicates(shafii_reading, source_grounding_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hadith scholars and transmitters occupy the methodological gatekeeping position. Their expertise in evaluating hadith authenticity becomes the foundation for jurisprudential legitimacy. They set the agenda for what counts as valid jurisprudential reasoning by establishing hadith hierarchies and authentication standards. They can arbitrage between different legal schools and maintain authority across jurisdictions.
narrative_ontology:constraint_stakeholder(shafii_reading, hadith_scholars, agenda_setter,
    institutional, biographical, arbitrage, global).

% Local judges, muftis, and community leaders operating within established customary practice ('urf) find their juristic reasoning systematically delegitimized. They cannot exit the constraint without abandoning their institutional position and identity as jurists. They bear the extraction cost through loss of methodological authority and institutional legitimacy.
narrative_ontology:constraint_stakeholder(shafii_reading, customary_practitioners, payer,
    powerless, biographical, trapped, local).

% Schools and practitioners relying on istihsan (juristic preference) and maslaha (public interest reasoning) face systematic delegitimization of their foundational methods. They can theoretically exit by adopting hadith-centric reasoning, but doing so requires abandoning core commitments and institutional identity. They also benefit from the broader coordination of Islamic legal tradition, which provides institutional framework for their operation.
narrative_ontology:constraint_stakeholder(shafii_reading, juristic_preference_traditions, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shafii_reading, juristic_preference_traditions, beneficiary).

% The Shafi'i school as an institutional structure sets the methodological agenda for its followers and coordinates jurisprudential practice across dispersed communities. It benefits from methodological distinctiveness and institutional authority. It is constrained by the need to maintain doctrinal coherence and defend the method against alternative approaches.
narrative_ontology:constraint_stakeholder(shafii_reading, shafii_school_institution, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(shafii_reading, shafii_school_institution, beneficiary).

% The broader Islamic legal tradition benefits from the coordination of jurisprudential pluralism — multiple schools coexist within a shared framework. The tradition is constrained by the need to maintain legitimacy across diverse communities and manage competing schools' claims to authority.
narrative_ontology:constraint_stakeholder(shafii_reading, islamic_legal_tradition, beneficiary,
    institutional, civilizational, constrained, global).

% The principle of grounding jurisprudence in transmitted sources (Quran, hadith, scholarly consensus) is vindicated by the Shafi'i method. This is not an agent but a doctrine that the constraint's operation vindicates. It collects no rents and must not be listed as a beneficiary.
narrative_ontology:constraint_stakeholder(shafii_reading, transmitted_source_authority, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(shafii_reading, transmitted_source_authority).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishing a coherent, systematic method for distinguishing authentic from fabricated hadith reports and for deriving jurisprudential rules from transmitted sources. This solves the genuine epistemic problem of how to ground legal reasoning in a reliable textual tradition.
% TRANSFER_FUNCTION: The method transfers methodological authority from customary practitioners and juristic preference traditions to hadith scholars and the Shafi'i school. It moves legitimacy from local, practice-based reasoning to centralized, source-based reasoning. It transfers institutional authority from competing schools to the Shafi'i framework.
% ABSENT_VOICES: Customary practitioners operating outside formal institutional structures have no voice in the methodological debate. Pre-Islamic Arabian legal traditions and non-Islamic legal systems are excluded from the conversation. Practitioners of istihsan and maslaha who are not part of organized schools have limited ability to defend their methods.
% DISAPPEARANCE_RATIONALE: If the Shafi'i methodological requirement disappeared, the Islamic legal tradition would rearrange itself: customary practitioners would regain methodological legitimacy, juristic preference traditions would revive, and the Shafi'i school would lose its distinctive institutional authority. The coordination of jurisprudential practice would shift from source-based to practice-based reasoning. Multiple competing methodologies would emerge without a shared framework.
% FOUNDING_PROBLEM: The founding problem was the need to establish a reliable method for distinguishing authentic hadith reports from fabrications and for systematically deriving jurisprudential rules from transmitted sources. In the early Islamic period, hadith fabrication was rampant, and jurisprudential reasoning was inconsistent across different communities. The Shafi'i method was developed to solve this problem by establishing explicit criteria for hadith authentication and systematic procedures for jurisprudential derivation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by early Islamic scholars including al-Shafi'i himself (Risala), later Shafi'i jurists (al-Nawawi, al-Subki), and comparative legal historians examining the development of Islamic jurisprudence. However, the problem's severity is contested: some scholars argue that hadith fabrication was less widespread than early sources suggest, and that customary practice provided adequate jurisprudential grounding. The corroboration is strongest from within the Shafi'i tradition itself; external corroboration from competing schools is weaker.
narrative_ontology:disappearance_verdict(shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(shafii_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CUSTOMARY PRACTITIONER (SNARE) — Local judges and muftis operating within established customary practice ('urf) find their juristic reasoning delegitimized. They cannot exit the constraint without abandoning their epistemic authority; the method requires explicit hadith grounding they may not possess. Trapped by institutional position and identity as jurist; bears full extraction cost of methodological invalidation.
constraint_indexing:constraint_classification(shafii_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: JURISTIC PREFERENCE TRADITION (SNARE) — Schools and practitioners relying on istihsan (juristic preference) and maslaha (public interest reasoning) face systematic delegitimization. They can theoretically exit by adopting hadith-centric methods, but doing so requires abandoning foundational commitments. Constrained by institutional identity and scholarly reputation; extraction is substantial but not total — some practitioners can migrate to hadith-based reasoning.
constraint_indexing:constraint_classification(shafii_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HADITH SCHOLARLY COMMUNITY (ROPE) — Hadith scholars and transmitters benefit from methodological primacy. Their expertise becomes the gatekeeper for jurisprudential legitimacy. They experience the constraint as coordination: establishing a shared method for evaluating transmitted sources solves the genuine problem of distinguishing authentic from fabricated reports. Net beneficiary with high exit optionality — they can arbitrage between different legal schools and maintain authority across jurisdictions.
constraint_indexing:constraint_classification(shafii_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SHAFI'I SCHOOL INSTITUTIONAL STRUCTURE (TANGLED ROPE) — The school itself coordinates jurisprudential practice (genuine coordination function: establishing consistent methodology across dispersed communities) while extracting authority from competing traditions. The school requires active enforcement through teaching, textual transmission, and institutional legitimacy claims. Constrained by need to maintain doctrinal coherence; benefits from methodological distinctiveness but also bears costs of defending the method against alternative approaches.
constraint_indexing:constraint_classification(shafii_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ISLAMIC LEGAL TRADITION INSTITUTIONAL VIEW (TANGLED ROPE) — The broader Islamic legal tradition coordinates jurisprudential pluralism (multiple schools coexist) while the Shafi'i method extracts authority by claiming methodological superiority. The tradition requires active enforcement through scholarly consensus, institutional recognition, and textual authority. Constrained by need to maintain legitimacy across diverse communities; benefits from methodological clarity but bears costs of managing competing schools.
constraint_indexing:constraint_classification(shafii_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the requirement for explicit source grounding appears as an immutable principle of rational jurisprudence: any legal system must ground its rules in identifiable sources to maintain coherence and prevent arbitrary decision-making. This perspective naturalizes the methodological requirement as inherent to law itself. However, the structural data reveals this as a false summit: the 'requirement' is a contingent institutional choice, not a law of nature.
constraint_indexing:constraint_classification(shafii_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shafii_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shafii_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shafii_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The Shafi'i method does extract authority from competing traditions, but the extraction is not severe because alternative schools remain institutionally viable and intellectually coherent. The method's beneficiaries (hadith scholars) gain real authority, but the cost to victims (customary practitioners) is primarily delegitimization rather than material deprivation. The trajectory shows slight accumulation over the interval (0.25 → 0.35) as the method becomes more institutionalized and enforcement mechanisms strengthen. Suppression (0.48): Moderate-high. Significant barriers exist to practicing jurisprudence outside the Shafi'i framework: institutional teaching privileges the method, textual authority is concentrated in Shafi'i sources, and scholarly legitimacy requires demonstrating hadith grounding. However, suppression is not total — alternative schools persist, and customary practitioners can continue operating despite delegitimization. The trajectory shows accumulation (0.35 → 0.48) as institutional enforcement mechanisms strengthen. Theater ratio (0.38): Moderate-low. The Shafi'i method has substantial functional content — it genuinely addresses the problem of distinguishing authentic from fabricated hadith reports and provides systematic procedures for jurisprudential reasoning. However, some performative elements exist: the method's claims about hadith corpus completeness may exceed reality, and some jurisprudential decisions rely on qiyas (analogical reasoning) despite methodological claims of hadith primacy. The theater ratio is stable across the interval, suggesting the performative content is structural rather than accumulating.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence. Hadith scholars see coordination (Rope) — establishing a shared method for evaluating transmitted sources solves a genuine epistemic problem. Customary practitioners see pure extraction (Snare) — their methods are delegitimized with no exit option. The Shafi'i school sees mixed coordination and extraction (Tangled Rope) — the method coordinates internal practice while extracting authority from competitors. The juristic preference traditions see extraction with some agency (Snare or Tangled Rope depending on exit capacity). The broader Islamic legal tradition sees institutional coordination with embedded extraction (Tangled Rope). The analytical observer risks seeing an immutable natural law (Mountain) — the requirement for explicit source grounding appears as inherent to rational jurisprudence — but the structural data reveals this as a false summit: the 'requirement' is a contingent institutional choice that benefits identifiable agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the constraint. Hadith scholars are beneficiaries with high exit optionality (arbitrage) — they derive d ≈ 0.1-0.2, experiencing low or negative effective extraction. Customary practitioners are victims with no exit (trapped) — they derive d ≈ 0.9, experiencing maximum extraction. The Shafi'i school is an organized beneficiary with constrained exit — it derives d ≈ 0.3-0.4, experiencing moderate extraction (the school benefits from methodological distinctiveness but bears costs of defending the method). The juristic preference traditions are moderate victims with constrained exit — they derive d ≈ 0.6-0.7, experiencing substantial extraction. The broader Islamic legal tradition is an institutional actor with constrained exit — it derives d ≈ 0.4-0.5, experiencing moderate extraction (the tradition benefits from methodological clarity but bears costs of managing competing schools). The analytical observer is analytical with analytical exit — d is undefined or neutral, reflecting the observer's position outside the constraint structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The Shafi'i method's mandate is to establish a coherent, source-grounded jurisprudential system that prevents arbitrary decision-making and maintains doctrinal consistency. This mandate remains live — the method continues to serve this function. However, the mandate has been partially superseded by institutional interests: the method now serves to maintain the Shafi'i school's institutional authority and to extract legitimacy from competing traditions. The constraint does not exhibit full mandatrophy (the original function has not completely atrophied), but it does exhibit partial mandatrophy: the institutional extraction function has become as important as the original coordination function. The theater ratio (0.38) is relatively low, suggesting the constraint is not primarily performative — the method genuinely coordinates jurisprudential practice. However, the performative elements (claims about hadith corpus completeness, the method's actual reliance on qiyas) indicate that some theatrical maintenance is occurring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_method,
    'Is the requirement for explicit hadith grounding a natural law of jurisprudence, or a constructed institutional arrangement that benefits hadith scholars?',
    'Historical analysis of pre-Shafi''i jurisprudential methods; comparison with non-Islamic legal traditions'' source requirements; examination of whether customary practice (''urf) can produce equally coherent jurisprudence without explicit hadith grounding',
    'If natural law: mountain classification confirmed; no beneficiaries needed. If constructed: false summit detected; hadith scholars are identifiable beneficiaries; classification shifts toward tangled_rope or snare depending on suppression mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_method, conceptual, 'Whether source grounding is natural law or constructed institutional choice').

omega_variable(
    istihsan_maslaha_foreclosure,
    'Does the Shafi''i rejection of istihsan and maslaha logically foreclose those methods, or do they coexist as alternative jurisprudential approaches?',
    'Textual analysis of Shafi''i foundational texts (Risala, Umm) vs Hanafi and Maliki sources; examination of whether a jurist can hold both Shafi''i methodology AND recognize istihsan/maslaha as valid in other contexts; historical documentation of whether schools explicitly claimed mutual foreclosure or coexistence',
    'If foreclosure: reading_relations should declare ''forecloses'' for hanafi_reading and maliki_reading. If coexistence: should declare ''coexists_with''. This determines whether the constraint is a logical contradiction or a pragmatic institutional choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_maslaha_foreclosure, conceptual, 'Whether Shafi''i method logically forecloses or coexists with istihsan/maslaha traditions').

omega_variable(
    hadith_corpus_authenticity_assumption,
    'Does the Shafi''i method''s primacy depend on an assumption that the hadith corpus is sufficiently authentic and complete to ground all necessary jurisprudential decisions?',
    'Empirical analysis of hadith corpus coverage: what percentage of jurisprudential questions have explicit hadith support vs require qiyas (analogical reasoning)? Historical documentation of hadith fabrication rates and scholarly disagreement on authenticity; examination of whether Shafi''i jurisprudence actually relies on qiyas despite methodological claims of hadith primacy',
    'If assumption is false: the method''s legitimacy depends on a false empirical claim; extraction mechanism becomes clearer (the method persists despite not delivering on its promise). If assumption is true: the method''s legitimacy is more robust; classification may shift toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hadith_corpus_authenticity_assumption, empirical, 'Whether hadith corpus is sufficiently complete for jurisprudential grounding').

omega_variable(
    sibling_reading_kernel_identity,
    'Do the Hanafi, Maliki, and Hanbali readings share the same kernel (usul_al_fiqh_method) or are they fundamentally different kernels?',
    'Textual analysis of foundational methodological texts across schools; examination of whether they are debating the SAME methodological question (how to ground jurisprudence) or different questions (what counts as valid evidence, how to weight sources, etc.); historical documentation of whether schools saw themselves as competing readings of a shared kernel or as incommensurable approaches',
    'If same kernel: the reading_relations declarations are appropriate. If different kernels: this constraint should be decomposed into separate constraint families with different kernel_ids. The network.affects_constraints should link to sibling constraints rather than treating them as readings of the same kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_kernel_identity, conceptual, 'Whether sibling schools share the same methodological kernel').

omega_variable(
    institutional_enforcement_mechanism,
    'What enforces the Shafi''i method''s authority: textual authority (the Risala''s logical force), institutional power (control of teaching positions and legal authority), or consensus (scholarly agreement)?',
    'Historical analysis of how Shafi''i methodology spread; examination of whether adoption was voluntary (scholars convinced by logical argument) or coercive (institutional pressure, political patronage); documentation of periods when the method was challenged or abandoned and what restored it',
    'If textual authority: enforcement is minimal; classification may shift toward rope. If institutional power: enforcement is substantial; classification remains tangled_rope or shifts toward snare. If consensus: enforcement is distributed; classification may shift toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_enforcement_mechanism, empirical, 'What mechanism enforces Shafi''i methodological authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shafii_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shafii_theater_t0, shafii_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(shafii_theater_t3, shafii_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement(shafii_theater_t6, shafii_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(shafii_theater_t10, shafii_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(shafii_extractiveness_t0, shafii_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(shafii_extractiveness_t3, shafii_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(shafii_extractiveness_t6, shafii_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(shafii_extractiveness_t10, shafii_reading, base_extractiveness, 10, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(shafii_suppression_t0, shafii_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(shafii_suppression_t3, shafii_reading, suppression_requirement, 3, 0.42).
narrative_ontology:measurement(shafii_suppression_t6, shafii_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(shafii_suppression_t10, shafii_reading, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shafii_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(shafii_reading, hanafi_reading).
narrative_ontology:affects_constraint(shafii_reading, maliki_reading).
narrative_ontology:affects_constraint(shafii_reading, hanbali_reading).
narrative_ontology:affects_constraint(shafii_reading, qiyas_analogical_reasoning).
narrative_ontology:affects_constraint(shafii_reading, hadith_authenticity_determination).

% DUAL FORMULATION NOTE:
% The Shafi'i reading is part of a constraint family decomposed from the contested kernel usul_al_fiqh_method. Each school's reading (Shafi'i, Hanafi, Maliki, Hanbali) represents a structurally distinct constraint with different ε values reflecting different beneficiary structures and suppression mechanisms. The Shafi'i reading's extractiveness (0.35) reflects moderate authority extraction from competing traditions; the Hanafi reading would show different extractiveness reflecting its different methodological commitments. These are not the same constraint viewed from different angles — they are genuinely different constraints with different ε values, linked through network.affects_constraints to show institutional coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shafii_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
