% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__performance_only, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Kodashim Commandment Status: Performance-Only Reading
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This constraint instantiates the performance-only reading of the kodashim
 *   commandment status kernel: sacrifice laws remain binding divine
 *   commandments despite Temple absence, but their performance is permanently
 *   suspended, leaving only textual study as the permissible engagement. This
 *   reading has dominated rabbinic institutional authority for centuries,
 *   justifying intensive scholarly focus on sacrificial procedure as the
 *   fulfillment of divine law. The claim is PITON: a former coordination
 *   response (preserving the complete halakhic corpus during crisis) that
 *   persists largely through institutional inertia and theatrical maintenance
 *   (the continued elaborate study of procedures that cannot be performed).
 *   The extractiveness is high (0.68) because institutional resources flow to
 *   this constraint despite its function being atrophied; the theater ratio
 *   is high (0.71) because much of the ongoing activity defends the
 *   interpretive hierarchy rather than achieving a live coordination
 *   function.
 *
 * KEY AGENTS:
 *   - rabbinic scholasticism: institutional agenda-setter, identity-locked in interpretive authority
 *   - displaced practical halakhists: moderate-power payers, constrained by institutional hierarchy
 *   - communities seeking actionable law: powerless payers, trapped in dependence on rabbinic guidance
 *   - messianic-deferral interpreters: excluded voices, moderate power, constrained alternatives
 *   - study-as-performance interpreters: excluded voices, moderate power, constrained alternatives
 *   - textual tradition bearers: institutional beneficiaries, analytical position on completeness
 *   - analytical observer: external seat, viewing institutional inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.68).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.42).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.68).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, piton).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Kodashim Commandment Status: Performance-Only Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, '6d361c5b-16e2-4263-8689-c3b510fa0244').
narrative_ontology:cs_kernel_codification('6d361c5b-16e2-4263-8689-c3b510fa0244', fixed_text).
narrative_ontology:cs_authority_grounding('6d361c5b-16e2-4263-8689-c3b510fa0244', lineage).
narrative_ontology:cs_interpretation_layer_present('6d361c5b-16e2-4263-8689-c3b510fa0244').
narrative_ontology:cs_reading_relation('6d361c5b-16e2-4263-8689-c3b510fa0244', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('6d361c5b-16e2-4263-8689-c3b510fa0244', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('6d361c5b-16e2-4263-8689-c3b510fa0244', foundational, sacrifice_performance_permanently_suspended).
narrative_ontology:cs_axiom_status(sacrifice_performance_permanently_suspended, holdable).
narrative_ontology:cs_axiom_grounding('6d361c5b-16e2-4263-8689-c3b510fa0244', sacrifice_performance_permanently_suspended, conventional).
narrative_ontology:cs_axiom('6d361c5b-16e2-4263-8689-c3b510fa0244', foundational, textual_study_substitutes_for_performance).
narrative_ontology:cs_axiom_status(textual_study_substitutes_for_performance, holdable).
narrative_ontology:cs_axiom_grounding('6d361c5b-16e2-4263-8689-c3b510fa0244', textual_study_substitutes_for_performance, deontological).
narrative_ontology:cs_reference_frame('6d361c5b-16e2-4263-8689-c3b510fa0244', temple_destroyed_performance_impossible).
narrative_ontology:cs_drift_state('6d361c5b-16e2-4263-8689-c3b510fa0244', contemporary_post_industrial, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6d361c5b-16e2-4263-8689-c3b510fa0244', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, rabbinic_scholasticism).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, displaced_practical_halakhists).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, communities_seeking_actionable_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, textual_tradition_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the interpretation that sacrifice laws remain divine commandments despite Temple absence, justifying continued intensive study of sacrificial procedures and altar operations as the only available performance of the law. Controls the curriculum of yeshivas and the validation of scholarly competence in halakhic reasoning. Collects institutional prestige, scholarly authority, and continued resource allocation to Talmudic study of Kodashim (the order of the Mishna and Talmud dealing with sacrifices).
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, rabbinic_scholasticism, agenda_setter,
    institutional, generational, identity_locked, continental).

% Historically and presently bear the cost of this interpretation: scholarly energy devoted to memorizing and debating sacrificial procedure details that cannot be performed, studied for centuries post-Temple with no prospect of application. Their alternative interpretations (that study-as-performance fulfills the commandment differently, or that the commandment is legitimately deferred until messianic restoration) are structurally marginalized in the institutional hierarchy. They remain inside the halakhic system but subordinated.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, displaced_practical_halakhists, payer,
    moderate, biographical, constrained, continental).

% Depend on rabbinic guidance for observance of the 613 commandments as applied to their lives. The performance-only reading diverts expert time and institutional prestige away from laws they can actually perform (prayer structures filling sacrificial roles, kashrut, Shabbat observance) and toward laws locked in obsolete procedure. They have no exit from reliance on the rabbinic system and cannot challenge the interpretive hierarchy.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, communities_seeking_actionable_law, payer,
    powerless, biographical, trapped, local).

% Hold the alternative reading that the commandment is temporally suspended pending messianic restoration, not permanently obsolete. This reading has scholarly defenders in Jewish tradition but is institutionally marginalized relative to the performance-only reading in dominant yeshiva curricula. They would advocate for a different resource-allocation priority if they had equal institutional voice.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, messianic_deferral_interpreters, excluded,
    moderate, civilizational, constrained, continental).

% Hold the alternative reading that studying sacrifice laws themselves fulfills the commandment, reframing intellectual engagement as the performance. This reading has historical precedent in Maimonidean and other traditions but is institutionally subordinated relative to the performance-only reading. Their alternative would compete for institutional legitimacy and resource allocation if given equal voice.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, study_as_performance_interpreters, excluded,
    moderate, civilizational, constrained, continental).

% The continued, intensive study of Kodashim preserves the textual corpus and hermeneutical sophistication required to engage with the full halakhic tradition. The performance-only reading justifies this preservation as study-without-application, maintaining the tradition's completeness even when application is suspended. This is the one genuine coordination benefit: comprehensive preservation of divine law as transmitted.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, textual_tradition_bearers, beneficiary,
    institutional, civilizational, analytical, universal).

% Views the constraint from outside the commitment system: observes that the performance-only reading allocates institutional resources to a practice (intensive memorization and reasoning about laws that cannot be performed) that persists largely through institutional inertia rather than lived necessity.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__performance_only, rabbinic_scholasticism).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the complete halakhic corpus in perpetuity, treating textual study as the divinely mandated way to maintain divine law when its application is suspended. Coordinates the scholarly community around a unified interpretive stance that prevents the abandonment of Kodashim as mere historical artifact or obsolete law.
% TRANSFER_FUNCTION: Moves scholarly time, institutional prestige, curriculum authority, and resource allocation from practical halakhic application to memorization and reasoning about sacrificial procedures that cannot be performed. The transfer runs from communities seeking actionable law and from alternative interpretive traditions toward rabbinic scholasticism and the institutional validation of textual expertise in sacrificial procedure.
% ABSENT_VOICES: Messianic-deferral interpreters and study-as-performance interpreters are structurally excluded from equal institutional standing in yeshiva hierarchies and credentialing systems. They would advocate for temporal deferral (preserving the law but marking it as genuinely suspended pending restoration) or for reframing the commandment's fulfillment (moving resources to understanding theological meaning rather than simulation of performance). Communities pragmatically seeking halakhic guidance for daily observance have no formal voice in interpretive priority-setting or curriculum design.
% DISAPPEARANCE_RATIONALE: If the performance-only reading's institutional dominance disappeared and was replaced by the study-as-performance reading, scholarly resources would partially redirect from memorizing procedure-detail to studying theological meaning and significance—the community of Kodashim scholars would contract (fewer people spending decades mastering sacrificial minutiae) but the law would remain engaged. If replaced by the messianic-deferral reading, Kodashim would be marked as legitimately suspended, scholarship would decline further, and institutional resource allocation would shift to practical halakhic application for daily observance. The constraint's disappearance would not dissolve the halakhic system, but it would reorganize resource allocation and interpretive hierarchy.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the halakhic system faced a foundational crisis: how to understand and engage with laws that commanded Temple-based actions when the Temple no longer existed, and what the continuation of divine law meant without the possibility of actual sacrifice. The foundational problem was existential for the interpretive tradition: could the law remain binding in perpetuity if its performance was impossible?
% FOUNDING_PROBLEM_CORROBORATION: All three reading interpreters (performance-only, study-as-performance, messianic-deferral) acknowledge the founding problem as live and unresolved. The persistent institutional competition between readings in contemporary Jewish scholarship, education, and jurisprudence attests that the question of how to understand Kodashim in the Temple's absence remains contested. The performance-only reading's institutional dominance is attested by yeshiva curricula, Talmudic commentary traditions, and the structure of halakhic credentialing. Alternative interpreters and external scholars (Hasidic critics, Haskalah historians, contemporary Jewish educators) contest this dominance and point to the accumulated resource cost. The problem persists because the Temple remains absent and no external fact resolves which interpretation is correct.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, contested).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__performance_only, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness accumulates historically (0.22 → 0.68 over 1,956 years) because the performance-only reading, initially a crisis response, became institutionalized and calcified. Immediately post-Temple, the reading was genuine coordination: it explained how the law continued when its application was impossible. By the medieval period (800), scholasticism had deepened, and by modernity (1700+), elaborate study of sacrificial detail with zero practical application became normative. The theater ratio rises sharply (0.15 → 0.71) because an increasing share of study-effort defends the interpretive hierarchy itself rather than preserving law per se—Talmudic disputations about minutiae of altar procedure became performative displays of interpretive virtuosity. Suppression is moderate (0.42) because the constraint is maintained through institutional prestige and identity-fusion rather than coercive exclusion: alternative readings are not forbidden, but are systematically subordinated in curriculum and credentialing. Accessibility collapse is low (0.38) because exit is theoretically available—one can adopt the messianic-deferral or study-as-performance readings—but identity-lock and institutional sanctions make exit costly.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (rabbinic scholasticism) computes as rope: the reading genuinely coordinates preservation of the complete halakhic corpus, and it offers a coherent interpretive solution to a foundational problem. The payer seats (displaced practical halakhists, communities) compute as snare: they experience resource diversion from actionable law and institutional subordination of their interpretive alternatives, without meaningful exit. The excluded seats (alternative interpreters) sit between: they acknowledge the coordination value of preservation but contest its dominance and would reallocate resources if given institutional voice. The engine should compute different types from each seat based on the structural asymmetry: beneficiary seat sees coordination; payer seat sees extraction; excluded seat sees captured alternative.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality ranges from ~0.05 (textual tradition bearers: pure beneficiaries of preservation) to ~0.95 (communities seeking actionable law: high extractive burden, trapped exit). Rabbinic scholasticism sits near 0.15 (institutional beneficiary with some extractive overhead). Displaced practical halakhists sit near 0.75 (moderate power but substantial extraction and constrained alternatives). Alternative interpreters sit near 0.60 (moderate bearing of institutional subordination, constrained but not trapped exit). The power atom 'institutional' for the agenda-setter and 'powerless' for the communities produce the widest directionality spread.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a classic piton case: the founding problem (how to understand sacrificial law post-Temple) was real and the performance-only reading was a coherent response. The coordination function (preserving the complete halakhic corpus) is genuine. However, the reading has outlived its primary justification: the community's foundational problem is no longer 'how do we understand this law' but 'why do we spend institutional resources on it when it cannot be performed.' The constraint persists by (1) institutional inertia—the study of Kodashim is embedded in yeshiva tradition and credentialing, (2) identity-fusion—Talmudic scholars identify their expertise through mastery of complex textual domains, and (3) theatrical maintenance—elaborate defenses of the interpretive hierarchy and periodic reaffirmations of its value in the tradition. The theater ratio (0.71) and the accumulated extractiveness (0.68) reflect this atrophied function maintained performatively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_vs_study_boundary,
    'Is textual study of sacrificial procedure genuinely a form of performance (satisfying the commandment), or is it distinct from the commandment itself (merely studying about what the commandment requires)?',
    'Genealogical analysis of Talmudic and medieval sources to determine whether the performance-only reading explicitly rejected study-as-performance or left it ambiguous. Systematic comparison with how other performed commandments (tefillin, sukkah) are treated in halakhic literature.',
    'If study and performance are shown to be conceptually distinct in the tradition, the performance-only reading''s justification weakens—it would be a pure husk maintained by institutional inertia rather than a coherent interpretation. If they are shown to overlap, the reading maintains partial force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_study_boundary, conceptual, 'Whether the performance-only reading rests on a defensible distinction between study and performance or conflates them.').

omega_variable(
    institutional_capture_of_interpretive_authority,
    'Is the performance-only reading''s institutional dominance maintained because it is the best interpretation, or because alternative readings threaten the institutional prestige and resource allocation of rabbinic scholasticism?',
    'Comparative analysis of how the three readings are treated in yeshiva curricula, publishing, and credentialing across different Jewish communities and historical periods. Historical investigation of whether alternative readings were suppressed or marginalized through institutional power rather than textual argument.',
    'If dominance is shown to rest on institutional power rather than interpretive force, the reading''s claim to be the authentic continuation of the tradition weakens, and the constraint becomes more clearly a case of institutional extraction overriding genuine alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_of_interpretive_authority, empirical, 'Whether the performance-only reading''s hegemony is justified by superior interpretation or by institutional power.').

omega_variable(
    coordination_function_persistence,
    'Is the preservation of the complete halakhic corpus—the coordination function this reading justifies—actually dependent on the performance-only reading''s institutional dominance, or would the corpus be preserved equally well under an alternative reading?',
    'Natural experiment: examination of Jewish communities where the study-as-performance or messianic-deferral readings dominate (Hasidic communities, some Sephardic traditions) to determine whether Kodashim is preserved, studied, and transmitted with fidelity comparable to performance-only communities.',
    'If the corpus is equally well-preserved under alternative readings, the coordination justification for the performance-only reading fails, and the constraint becomes pure institutional extraction—a piton maintained by prestige rather than function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_persistence, empirical, 'Whether the coordinate function (textual preservation) actually requires the performance-only reading''s institutional dominance.').

omega_variable(
    alternative_reading_suppression_mechanism,
    'Is the institutional marginalization of messianic-deferral and study-as-performance readings the result of explicit doctrinal argument, or the result of curriculum design, credentialing requirements, and resource allocation that do not need to suppress the alternative readings explicitly?',
    'Archival and contemporary analysis of how yeshiva curricula prioritize Kodashim study, how scholarly credentials are earned, and what institutional incentives exist for focusing on performance-only interpretive detail versus alternative frameworks.',
    'If suppression is implicit (structural exclusion rather than explicit doctrine), the constraint''s extractiveness is harder to detect and resist—victims cannot point to a forbidden reading. If suppression is explicit, it is more vulnerable to challenge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_suppression_mechanism, empirical, 'Whether institutional marginalization of alternative readings operates through doctrinal authority or structural incentives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t70, kodashim_commandment_status__performance_only, theater_ratio, 70, 0.15).
narrative_ontology:measurement(koda_tr_t400, kodashim_commandment_status__performance_only, theater_ratio, 400, 0.35).
narrative_ontology:measurement(koda_tr_t800, kodashim_commandment_status__performance_only, theater_ratio, 800, 0.52).
narrative_ontology:measurement(koda_tr_t1300, kodashim_commandment_status__performance_only, theater_ratio, 1300, 0.61).
narrative_ontology:measurement(koda_tr_t1700, kodashim_commandment_status__performance_only, theater_ratio, 1700, 0.68).
narrative_ontology:measurement(koda_tr_t1900, kodashim_commandment_status__performance_only, theater_ratio, 1900, 0.7).
narrative_ontology:measurement(koda_tr_t2026, kodashim_commandment_status__performance_only, theater_ratio, 2026, 0.71).

% Extraction over time
narrative_ontology:measurement(koda_be_t70, kodashim_commandment_status__performance_only, base_extractiveness, 70, 0.22).
narrative_ontology:measurement(koda_be_t400, kodashim_commandment_status__performance_only, base_extractiveness, 400, 0.38).
narrative_ontology:measurement(koda_be_t800, kodashim_commandment_status__performance_only, base_extractiveness, 800, 0.52).
narrative_ontology:measurement(koda_be_t1300, kodashim_commandment_status__performance_only, base_extractiveness, 1300, 0.61).
narrative_ontology:measurement(koda_be_t1700, kodashim_commandment_status__performance_only, base_extractiveness, 1700, 0.65).
narrative_ontology:measurement(koda_be_t1900, kodashim_commandment_status__performance_only, base_extractiveness, 1900, 0.66).
narrative_ontology:measurement(koda_be_t2026, kodashim_commandment_status__performance_only, base_extractiveness, 2026, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__performance_only, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__performance_only, resource_allocation).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__performance_only, 0.25).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% The kodashim_commandment_status kernel decomposes into three structurally distinct constraints, each instantiating a different reading. The performance-only reading (this constraint) claims high extractiveness and theater ratio because it justifies resource allocation to a permanently-suspended practice. The study-as-performance reading claims lower extractiveness by reframing the commandment itself. The messianic-deferral reading claims legitimately deferred status, reducing both extraction and theater. Each reading has a different beneficiary structure and different implications for how institutional resources are allocated. All three compete for authority over the same kernel; they affect each other through their competing interpretive claims and their institutional struggle for credentialing authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_commandment_status__performance_only, institutional, 0.15).
constraint_indexing:directionality_override(kodashim_commandment_status__performance_only, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
