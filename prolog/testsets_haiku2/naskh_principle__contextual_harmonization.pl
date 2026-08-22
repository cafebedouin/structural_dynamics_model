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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: naskh_principle__contextual_harmonization
 *   human_readable: Quranic Contextual Harmonization Principle (Naskh via Context)
 *   domain: religious/legal/hermeneutical
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel 'naskh_principle'
 *   (Islamic abrogation doctrine). The Contextual Harmonization reading
 *   asserts that all Quranic verses remain valid within their specific
 *   revelatory and situational contexts; apparent contradictions are resolved
 *   through contextual specification rather than chronological supersession.
 *   This reading benefits scholars and schools seeking theological coherence
 *   and adaptive legal interpretation; it imposes costs on jurists who
 *   derived authority from definitive abrogation and litigants seeking legal
 *   predictability. The constraint is claimed as Tangled Rope because it
 *   solves a genuine coordination problem (preserving textual coherence)
 *   while also extracting from those who lose interpretive finality.
 *
 * KEY AGENTS:
 *   - Theological coherence framers (institutional beneficiary): maintain unified Quranic theology by contextually harmonizing verses
 *   - Adaptive jurisprudence schools (institutional agenda-setter + beneficiary): gain flexibility to adapt rulings to contemporary circumstance while preserving all textual authority
 *   - Classical jurist tradition (institutional payer): loses definitional authority to declare rulings permanently abrogated
 *   - Legal predictability seekers (moderate power payer): bear increased cost of case-by-case contextual analysis instead of stable final rulings
 *   - Classical abrogation adherents (institutional excluded): systematically outside the conversation; their method is treated as invalid
 *   - Scripture interpretation community (institutional observer): examines how different hermeneutical frameworks handle contradictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__contextual_harmonization, 0.62).
domain_priors:suppression_score(naskh_principle__contextual_harmonization, 0.41).
domain_priors:theater_ratio(naskh_principle__contextual_harmonization, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, extractiveness, 0.62).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__contextual_harmonization, tangled_rope).
narrative_ontology:human_readable(naskh_principle__contextual_harmonization, "Quranic Contextual Harmonization Principle (Naskh via Context)").
narrative_ontology:topic_domain(naskh_principle__contextual_harmonization, "religious/legal/hermeneutical").

domain_priors:requires_active_enforcement(naskh_principle__contextual_harmonization).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__contextual_harmonization, '65f7a09d-a368-4f84-aa4c-e21f8d26d847').
narrative_ontology:cs_kernel_codification('65f7a09d-a368-4f84-aa4c-e21f8d26d847', fixed_text).
narrative_ontology:cs_authority_grounding('65f7a09d-a368-4f84-aa4c-e21f8d26d847', lineage).
narrative_ontology:cs_interpretation_layer_present('65f7a09d-a368-4f84-aa4c-e21f8d26d847').
narrative_ontology:cs_reading_relation('65f7a09d-a368-4f84-aa4c-e21f8d26d847', naskh_principle__classical_abrogation, coexists_with).
narrative_ontology:cs_reading_relation('65f7a09d-a368-4f84-aa4c-e21f8d26d847', naskh_principle__progressive_restriction, influences).
narrative_ontology:cs_axiom('65f7a09d-a368-4f84-aa4c-e21f8d26d847', foundational, all_verses_retain_validity).
narrative_ontology:cs_axiom_status(all_verses_retain_validity, holdable).
narrative_ontology:cs_axiom_grounding('65f7a09d-a368-4f84-aa4c-e21f8d26d847', all_verses_retain_validity, deontological).
narrative_ontology:cs_axiom('65f7a09d-a368-4f84-aa4c-e21f8d26d847', foundational, context_determines_application).
narrative_ontology:cs_axiom_status(context_determines_application, holdable).
narrative_ontology:cs_axiom_grounding('65f7a09d-a368-4f84-aa4c-e21f8d26d847', context_determines_application, instrumental).
narrative_ontology:cs_reference_frame('65f7a09d-a368-4f84-aa4c-e21f8d26d847', quranic_textual_coherence_through_context).
narrative_ontology:cs_drift_state('65f7a09d-a368-4f84-aa4c-e21f8d26d847', contemporary_institutional_adoption, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('65f7a09d-a368-4f84-aa4c-e21f8d26d847', '').
narrative_ontology:cs_kernel_id(naskh_principle__contextual_harmonization, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, theological_coherence_framers).
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, adaptive_jurisprudence_schools).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, jurist_definitional_authority).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, legal_predictability_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, classical_jurist_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars and theologians who benefit from a reading framework that preserves all Quranic verses as simultaneously valid, avoiding textual contradiction and enabling unified theological systems. They maintain that contextual specification resolves apparent conflicts without requiring chronological abrogation. They produce scholarship, fatwas, and teaching that reinforce this interpretive gate.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, theological_coherence_framers, beneficiary,
    institutional, civilizational, arbitrage, global).

% Schools of Islamic law that leverage contextual interpretation to justify adapting Quranic rulings to changed circumstances without declaring earlier rulings invalid. They set fatwa precedent and educate judges; contextual harmonization grants them interpretive flexibility to issue rulings responsive to contemporary conditions while claiming fidelity to all Quranic text. They administer the constraint through teaching, judicial guidance, and institutional fatwa authority.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, adaptive_jurisprudence_schools, agenda_setter,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(naskh_principle__contextual_harmonization, adaptive_jurisprudence_schools, beneficiary).

% Classical jurists and their schools whose authority derived from identifying definitive rulings through abrogation (naskh) — the ability to say 'this verse no longer applies because a later verse abrogated it.' Under contextual harmonization, that closure power is lost; every earlier ruling potentially re-enters play if a new context is identified. Their ability to declare questions definitively settled is compromised. They bear the loss of interpretive finality and institutional marginalization.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, classical_jurist_tradition, payer,
    institutional, generational, constrained, regional).

% Litigants, judges, and jurists seeking stable legal rules: what ruling applies to my case? Under abrogation, the answer is clear and final (this ruling was abrogated, that one applies). Under contextual harmonization, the answer requires case-by-case contextual analysis; new contexts can revive old rulings, and interpretive drift is structural. They bear the cost of reduced predictability and increased litigation risk over rule interpretation.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, legal_predictability_seekers, payer,
    moderate, biographical, constrained, regional).

% Scholars committed to classical abrogation doctrine (naskh) who argue that chronological supersession is the only coherent way to resolve textual contradictions. They are outside the conversation under contextual harmonization; their method is treated as methodologically invalid. They would argue that contextual multiplicity collapses legal determinacy and attributes incoherence to the Divine text. Their exclusion is structural to the constraint's operation.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, classical_abrogation_adherents, excluded,
    institutional, civilizational, trapped, global).

% Academic and theological community studying Quranic interpretation across traditions and methodologies. They examine how different frameworks (abrogation, contextualization, progressive restriction) handle apparent contradictions and what each framework gains and loses. They do not produce binding rulings but provide methodological analysis.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, scripture_interpretation_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__contextual_harmonization, adaptive_jurisprudence_schools).
narrative_ontology:fixing_cost_class(naskh_principle__contextual_harmonization, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for resolving Quranic textual tensions without denying the authority of any verse: all verses remain part of the coherent divine message, with apparent contradictions dissolved through specification of historical, social, or circumstantial context. Enables theological systems to treat the Quran as a unified, non-contradictory text.
% TRANSFER_FUNCTION: Moves interpretive authority from definitive legal closure (abrogation-based finality) to contextual-case analysis. Jurists retain authority to interpret context but lose authority to declare rulings permanently invalid. Beneficiary schools gain flexibility to adapt rulings to contemporary circumstances while claiming textual fidelity; those seeking legal predictability lose stable answers to recurring questions.
% ABSENT_VOICES: Classical abrogation adherents and strict literalists are structurally excluded — their hermeneutical method is treated as invalid within the contextual harmonization framework. They would argue that contextual multiplicity makes the Quranic text incoherent and subjects divine law to subjective circumstantial judgment. Their testimony is not solicited in institutional spaces governed by contextual harmonization.
% DISAPPEARANCE_RATIONALE: If contextual harmonization disappeared, classical abrogation doctrine would re-emerge as the primary mechanism for resolving contradictions; scholarly consensus about textual coherence would collapse into explicit acknowledgment of textual contradiction. The constraint's disappearance would reorganize Quranic jurisprudence around definitive legal rulings rather than contextual adaptation — rulings would become final, authority would reconcentrate in traditional jurist schools, and adaptability to changed circumstances would require explicit doctrinal amendment rather than contextual reinterpretation.
% FOUNDING_PROBLEM: Early Islamic jurisprudence encountered apparent contradictions in Quranic rulings on the same topics (prohibition and permission of certain practices, differing legal consequences for similar acts). Chronological abrogation resolved contradictions but required determining revelation order from extra-Quranic sources and accepting that many verses are no longer valid law. Contextual harmonization was developed to preserve all verses as eternally valid while still resolving contradictions through context-specific application.
% FOUNDING_PROBLEM_CORROBORATION: Classical and medieval Islamic jurists (al-Shafi'i, al-Zamakhshari, Ibn Taymiyyah) attested that resolving apparent Quranic contradictions was a central hermeneutical challenge. Modern Quranic scholars from outside the benefiting schools (academic exegetes, comparative religion scholars) acknowledge that contextualization is one hermeneutical strategy for handling contradictions, but dispute whether it fully resolves them or merely relocates the problem to context-determination. Legal historians and practitioners note that contextual interpretation provides greater adaptability than abrogation-based closure, confirming the structural delta.
narrative_ontology:disappearance_verdict(naskh_principle__contextual_harmonization, contested).
narrative_ontology:founding_problem_status(naskh_principle__contextual_harmonization, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__contextual_harmonization, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__contextual_harmonization, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__contextual_harmonization, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.45 to 0.62 over the interval because institutionalization of contextual harmonization gradually shifts interpretive authority away from classical finality toward adaptive case-analysis. Early adoption (t=0) shows moderate extraction because both frameworks coexist in scholarly debate. As contextual harmonization becomes institutionalized in educational curricula and fatwa bodies (t=25–35), extraction peaks because the constraint actively suppresses classical abrogation methodology and forces adherents into excluded roles. The plateau at t=35–50 reflects equilibrium: adaptive schools have consolidated their interpretive authority and institutional suppression of alternatives has stabilized. Theater ratio rises gradually (0.12–0.29) because interpretive work must present contextual analysis as neutral methodology rather than as a deliberate extraction of definitional authority from classical schools; the theater is the scholarly apparatus of harmonization scholarship, which performs neutrality while systematically advantage-shifting. Suppression remains moderate (0.35–0.41) because the exclusion is methodological (classical methods are not valid in this framework) rather than direct coercion — classical scholars can still publish and teach, but institutional fatwa authority and educational gatekeeping are closed to them.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter (adaptive jurisprudence schools) seat: the constraint is coordination — solving the theological incoherence problem while enabling institutional flexibility. From the payer (classical jurist) seat: the constraint is extraction — loss of definitional authority, forced to operate under a new hermeneutical regime. From the legal predictability seeker seat: the constraint imposes diffuse costs through interpretive burden-shifting. The engine computes these divergent types from structural data; the authored claim (Tangled Rope) captures the coordination+extraction asymmetry but does not pre-adjudicate what each seat experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Theological coherence framers and adaptive jurisprudence schools are the structural beneficiaries: they gain the ability to harmonize all Quranic verses without contradiction (satisfying theological coherence demands) and to adapt rulings to contemporary circumstances while maintaining textual fidelity (enabling profitable institutional authority). Classical jurist schools lose the ability to say 'this verse is abrogated and no longer law' — their closure power is compromised. Legal predictability seekers lose stable final answers; every case requires contextual analysis, which raises dispute risk. The beneficiaries have higher institutional power and mobile exit options (they can exit by returning to abrogation doctrine); the payers have constrained exit (classical jurists are trapped within the institutional structure, legal predictability seekers cannot exit without abandoning Islamic jurisprudence). High suppression (0.41) reflects that the constraint actively excludes classical abrogation methodology from institutional spaces; adherents are not included in fatwa councils and teaching curricula that adopt contextual harmonization.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live but contested: classical jurists attest that resolving Quranic contradictions was their original concern; modern scholars attest that contextual harmonization is one strategy among several. The constraint persists because it serves the adaptive jurisprudence schools' institutional interests (it provides interpretive flexibility without denying textual authority), not because the founding problem requires it. The constraint exhibits mandatrophy signals: the ability to declare rules definitive (the original jurist function) is no longer exercised; instead, contextual analysis and reinterpretation have become continuous institutional work. However, the constraint is not purely theatrical — genuine theological coordination work happens (harmonizing apparently contradictory verses requires real intellectual effort), so theater_ratio remains moderate (0.28) rather than high. Classification as Tangled Rope rather than Piton reflects that the coordination function is genuine and the beneficiary schools actively maintain and develop the constraint, not just perform compliance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    context_determination_authority,
    'Who has the authority to determine context, and by what epistemological standards? Can the same verse be contextualized differently by different jurists, or is there a canonical context-determination method?',
    'Institutional study of how contextual harmonization is actually practiced in fatwa councils and judicial decisions: do judges converge on consistent context-interpretations, or does each judge contextualize independently?',
    'If context-determination is canonical and stable, extractiveness is lower (the constraint is merely substituting one closure mechanism for another). If context-determination is judge-dependent and variable, extractiveness is high (the constraint enables individualized interpretation while appearing neutral). This omega determines whether the constraint genuinely resolves contradictions or merely relocates interpretive discretion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(context_determination_authority, empirical, 'Whether contextual harmonization produces stable or variable legal outcomes.').

omega_variable(
    textual_contradiction_vs_harmonization,
    'Can all apparent Quranic contradictions be coherently harmonized through context-specification, or are some contradictions irresolvable through contextual analysis alone?',
    'Comparative hermeneutical study: identify Quranic passages where contextual specification is claimed to resolve contradiction, and examine whether the claimed context is textually-warranted or interpretively-imposed.',
    'If all contradictions can be coherently harmonized, the constraint genuinely solves the coordination problem (theological coherence). If some contradictions resist harmonization, the constraint is partially cover story — it provides flexibility but not the unified theology it claims. This determines whether the beneficiary-frame (theological coherence) or the payer-frame (flexible extraction) is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_contradiction_vs_harmonization, conceptual, 'Whether contextual harmonization fully resolves or merely masks textual contradictions.').

omega_variable(
    kernel_reading_foreclusion_ambiguity,
    'Does the contextual_harmonization reading logically foreclose the classical_abrogation reading, or do they represent incommensurable hermeneutical choices that can coexist in the same tradition?',
    'Logical analysis: if a verse is harmonized through context rather than abrogated, can a different jurist legitimately abrogate the same verse under classical doctrine? Can both methodologies be valid simultaneously for the same Quranic text?',
    'If they foreclose each other, the kernel contest is a binary choice and institutional adoption of one excludes the other (reading_relations: forecloses). If they coexist, multiple hermeneutical regimes operate simultaneously in Islamic jurisprudence (reading_relations: coexists_with). This determines the severity of exclusion experienced by classical abrogation adherents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclusion_ambiguity, conceptual, 'Whether contextual harmonization and classical abrogation are logically incompatible or methodologically coexistent.').

omega_variable(
    suppression_mechanism_externality,
    'Is the measured suppression (0.41) structural (institutional gatekeeping) or internalized (classical scholars accept contextual harmonization as superior methodology)?',
    'Survey of classical jurisprudence scholars: do they experience suppression as external barriers (excluded from councils, denied platform) or as internalized acceptance of contextual methodology as legitimate? Post-institutional-contact trajectory: do suppressed scholars maintain classical methodology or adopt contextual framework?',
    'If structural (external), the suppression is reversible by institutional change; the constraint''s persistence depends on ongoing enforcement. If internalized, the suppression carries forward even if institutional gatekeeping ends — classical methodology is seen as obsolete. This affects whether the constraint is sustainably maintained or vulnerable to disruption.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_externality, empirical, 'Whether suppression of classical abrogation is institutional gatekeeping or methodological internalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__contextual_harmonization, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naskh_contextual_tr_t0, naskh_principle__contextual_harmonization, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(naskh_contextual_tr_t0, observed).
narrative_ontology:measurement(naskh_contextual_tr_t8, naskh_principle__contextual_harmonization, theater_ratio, 8, 0.16).
narrative_ontology:measurement_basis(naskh_contextual_tr_t8, observed).
narrative_ontology:measurement(naskh_contextual_tr_t16, naskh_principle__contextual_harmonization, theater_ratio, 16, 0.21).
narrative_ontology:measurement_basis(naskh_contextual_tr_t16, observed).
narrative_ontology:measurement(naskh_contextual_tr_t25, naskh_principle__contextual_harmonization, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(naskh_contextual_tr_t25, observed).
narrative_ontology:measurement(naskh_contextual_tr_t35, naskh_principle__contextual_harmonization, theater_ratio, 35, 0.29).
narrative_ontology:measurement_basis(naskh_contextual_tr_t35, observed).
narrative_ontology:measurement(naskh_contextual_tr_t50, naskh_principle__contextual_harmonization, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(naskh_contextual_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(naskh_contextual_be_t0, naskh_principle__contextual_harmonization, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(naskh_contextual_be_t0, observed).
narrative_ontology:measurement(naskh_contextual_be_t8, naskh_principle__contextual_harmonization, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(naskh_contextual_be_t8, observed).
narrative_ontology:measurement(naskh_contextual_be_t16, naskh_principle__contextual_harmonization, base_extractiveness, 16, 0.57).
narrative_ontology:measurement_basis(naskh_contextual_be_t16, observed).
narrative_ontology:measurement(naskh_contextual_be_t25, naskh_principle__contextual_harmonization, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(naskh_contextual_be_t25, observed).
narrative_ontology:measurement(naskh_contextual_be_t35, naskh_principle__contextual_harmonization, base_extractiveness, 35, 0.63).
narrative_ontology:measurement_basis(naskh_contextual_be_t35, observed).
narrative_ontology:measurement(naskh_contextual_be_t50, naskh_principle__contextual_harmonization, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(naskh_contextual_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(naskh_contextual_su_t0, naskh_principle__contextual_harmonization, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(naskh_contextual_su_t0, observed).
narrative_ontology:measurement(naskh_contextual_su_t8, naskh_principle__contextual_harmonization, suppression_requirement, 8, 0.37).
narrative_ontology:measurement_basis(naskh_contextual_su_t8, observed).
narrative_ontology:measurement(naskh_contextual_su_t16, naskh_principle__contextual_harmonization, suppression_requirement, 16, 0.39).
narrative_ontology:measurement_basis(naskh_contextual_su_t16, observed).
narrative_ontology:measurement(naskh_contextual_su_t25, naskh_principle__contextual_harmonization, suppression_requirement, 25, 0.41).
narrative_ontology:measurement_basis(naskh_contextual_su_t25, observed).
narrative_ontology:measurement(naskh_contextual_su_t35, naskh_principle__contextual_harmonization, suppression_requirement, 35, 0.41).
narrative_ontology:measurement_basis(naskh_contextual_su_t35, observed).
narrative_ontology:measurement(naskh_contextual_su_t50, naskh_principle__contextual_harmonization, suppression_requirement, 50, 0.41).
narrative_ontology:measurement_basis(naskh_contextual_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__contextual_harmonization, identity_coordination).
narrative_ontology:boltzmann_floor_override(naskh_principle__contextual_harmonization, 0.12).
narrative_ontology:affects_constraint(naskh_principle__contextual_harmonization, naskh_principle__classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__contextual_harmonization, naskh_principle__progressive_restriction).

% DUAL FORMULATION NOTE:
% Part of the naskh_principle kernel family. Contextual Harmonization stands between Classical Abrogation (upstream, more established) and Progressive Restriction (downstream, more contested). All three readings address the same Quranic tensions; they differ on whether contradictions are resolved by chronological closure (abrogation), contextual multiplicity (harmonization), or progressive legal pedagogy (restriction). Each story carries distinct epsilon values because they have distinct beneficiary/victim structures and institutional consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__contextual_harmonization, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
