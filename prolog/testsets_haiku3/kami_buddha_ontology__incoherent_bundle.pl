% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__incoherent_bundle, []).

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
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Shinbutsu-shugo as Institutionally Sustained Incoherent Bundle
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   Shinbutsu-shugo (kami-buddha syncretism) is presented by institutional
 *   actors as a harmonious integration of kami and buddha cosmologies. This
 *   constraint story instantiates one reading of the kami-buddha ontology
 *   kernel: the incoherent-bundle reading, which holds that shinbutsu-shugo
 *   is NOT a coherent kernel but rather a sustained bundle of contradictory
 *   commitments — simultaneous fusion and separation, hierarchical and
 *   reciprocal, systematized and unsystematized — held in place by
 *   institutional interests and ritual efficacy rather than by doctrinal
 *   resolution. The constraint CLAIMS tangled rope (genuine coordination
 *   function plus asymmetric extraction) while the authored metrics describe
 *   substantially extractive, actively enforced operation driven increasingly
 *   by theater (rising theater_ratio from 0.38 to 0.61 over the interval,
 *   suggesting that performative maintenance of compatibility is replacing
 *   actual functional integration). The measurement series tracks how
 *   extractiveness and suppression intensity have risen since the early
 *   integration period, while theater rises faster, indicating that the
 *   system is increasingly maintained through doctrinal reinterpretation and
 *   ritual display rather than genuine cosmological work.
 *
 * KEY AGENTS:
 *   - Syncretic institutional class: Buddhist monasteries, Shinto shrines, mixed institutions that profit from incoherence
 *   - Priesthoods (both): Buddhist and Shinto clergy who maintain the bundle through institutional practice and interpretation
 *   - Systematic theology tradition: Scholars required to reconcile contradictions without resolution
 *   - Separation reform advocates: Those who attempt to impose coherence and face institutional suppression
 *   - Lay practitioners: Powerless beneficiaries who validate the system through non-reflective participation
 *   - Reform authorities: Occasional state attempts to impose separation (Meiji, postwar) that retreat when costs mount
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.68).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.72).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.68).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Shinbutsu-shugo as Institutionally Sustained Incoherent Bundle").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious/philosophical/institutional").

domain_priors:requires_active_enforcement(kami_buddha_ontology__incoherent_bundle).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, 'fac9f2fe-c702-411f-804b-1949eddebabb').
narrative_ontology:cs_kernel_codification('fac9f2fe-c702-411f-804b-1949eddebabb', distributed).
narrative_ontology:cs_authority_grounding('fac9f2fe-c702-411f-804b-1949eddebabb', extraction).
narrative_ontology:cs_interpretation_layer_present('fac9f2fe-c702-411f-804b-1949eddebabb').
narrative_ontology:cs_reading_relation('fac9f2fe-c702-411f-804b-1949eddebabb', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('fac9f2fe-c702-411f-804b-1949eddebabb', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_axiom('fac9f2fe-c702-411f-804b-1949eddebabb', foundational, incoherence_constitutive_of_arrangement).
narrative_ontology:cs_axiom_status(incoherence_constitutive_of_arrangement, holdable).
narrative_ontology:cs_axiom_grounding('fac9f2fe-c702-411f-804b-1949eddebabb', incoherence_constitutive_of_arrangement, empirically_contingent).
narrative_ontology:cs_axiom('fac9f2fe-c702-411f-804b-1949eddebabb', secondary, practice_prior_to_doctrine_ideology).
narrative_ontology:cs_axiom_status(practice_prior_to_doctrine_ideology, holdable).
narrative_ontology:cs_axiom_grounding('fac9f2fe-c702-411f-804b-1949eddebabb', practice_prior_to_doctrine_ideology, conventional).
narrative_ontology:cs_reference_frame('fac9f2fe-c702-411f-804b-1949eddebabb', integrative_institutional_flexibility).
narrative_ontology:cs_drift_state('fac9f2fe-c702-411f-804b-1949eddebabb', contemporary_reform_pressure_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fac9f2fe-c702-411f-804b-1949eddebabb', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, syncretic_institutional_class).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, priesthoods_both).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, systematic_theology_tradition).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, separation_reform_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, lay_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buddhist monasteries, Shinto shrines, and mixed institutions that operate under shinbutsu-shugo derive institutional legitimacy, territorial holdings, ritual authority, and patronage from the arrangement's flexibility — they can invoke Buddhist cosmology when it suits their claims, invoke Shinto purification when it suits their claims, and avoid doctrinal consistency requirements that would force choice. They benefit from sustained incoherence because coherence would require doctrinal specification, which would eliminate some of their institutional options and expose their extraction.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, syncretic_institutional_class, beneficiary,
    institutional, generational, arbitrage, national).

% Buddhist and Shinto priests benefit from the institutional bundling because it allows them to maintain professional jurisdiction over overlapping domains without explicit competition or reconciliation. A Shinto priest conducts life-cycle rituals; a Buddhist priest conducts death rituals; a mixed institution conducts both. The incoherence allows this to persist without requiring theoretical justification. They also maintain the bundle actively through ritual performance and textual reinterpretation that sustains the appearance of compatibility.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, priesthoods_both, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, priesthoods_both, agenda_setter).

% Buddhist philosophers and Shinto theorists who operate in the systematic theology tradition face a structural constraint: they must either accept the incoherence (making their theoretical work subordinate to institutional practice) or reject it (making them marginalized from the institutions they work within). The constraint extracts intellectual labor — they spend effort reconciling contradictions or suppressing them in published work — while institutional actors profit from the unsystematized state. Leaving the tradition is identity-loss for these scholars.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, systematic_theology_tradition, payer,
    moderate, biographical, identity_locked, national).

% Reformers who advocate for clear separation of kami and buddhas (meiji-era shintoists, contemporary systematic religionists) face institutional resistance and are branded as disruptive to harmony. Their attempts to impose coherence are suppressed through institutional pressure, doctrinal reinterpretation that reabsorbs their proposals, and social censure. They must either abandon the reform, accept marginality, or leave the field entirely.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, separation_reform_advocates, payer,
    moderate, biographical, constrained, national).

% Individual worshippers participate in both kami rituals and Buddhist rituals without requiring coherence — they visit shrines for life transitions and protection, visit temples for death rituals and merit accumulation, and do not carry intellectual commitment to resolving the contradiction. Their participation sustains the arrangement and validates its practical efficacy. They benefit from a system that addresses their needs through both cosmologies without forcing choice.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, lay_practitioners, beneficiary,
    powerless, biographical, mobile, local).

% State actors (Meiji Restoration, postwar religious administration) have periodically attempted to impose separations or coherence and have backed away when institutional costs mounted. They observe the arrangement's persistence and capacity to absorb reform pressure.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, reform_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__incoherent_bundle, priesthoods_both).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__incoherent_bundle, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Shinbutsu-shugo coordinates the functional division of religious labor in Japanese society: kami handle life-cycle protection and purification; buddhas handle death, impurity, and afterlife. Multiple priesthoods and institutions share territorial and functional domains without requiring unified doctrinal commitment, allowing simultaneous specialization and flexibility.
% TRANSFER_FUNCTION: The arrangement transfers intellectual labor from systemizers and reformers to institutional actors: systematic theologians produce reconciliation narratives that mask incoherence; reformers expend effort on separation proposals that institutions reabsorb; lay adherents perform rituals that validate the system's practical efficacy without requiring theoretical consent.
% ABSENT_VOICES: Pure doctrinal philosophers who insist on single-framework coherence are structurally excluded; they would argue that shinbutsu-shugo must either be honji-suijaku (kami as manifestations of buddha-nature) or domain-partitioned (separate ontologies), but such insistence is treated as pedantic, disharmonious, or foreign to Japanese practice. Their exclusion is maintained by the institutional claim that 'practice is prior to doctrine' and that theoretical purity is less important than ritual efficacy.
% DISAPPEARANCE_RATIONALE: If the incoherent bundle vanished overnight and forced coherence, institutional territories would be redrawn: either kami would be subordinated to Buddhist cosmology (all living kami as bodhisattva manifestations, with Buddhist priests gaining authority over Shinto), or kami and buddhas would be declared ontologically separate (Shinto reclaiming autonomous authority, Buddhism confined to death-handling and merit). The current priesthoods' shared institutional space would collapse into competition or clear hierarchy. Lay practice would reorganize around explicit choice between systems rather than participation in both without reflection.
% FOUNDING_PROBLEM: Early Japanese religious integration faced overlapping priesthoods (Buddhist monks and Shinto priests) with different cosmologies, different ritual functions, and different institutional resources. Rather than merge or subordinate one to the other, the arrangement allowed both to operate simultaneously, with priests drawing on both traditions as circumstances required. This solved the problem of integrating two powerful institutional classes without forcing political and theological choice.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Japanese Buddhism (Rambelli, Grapard, Teeuws) attest that the founding problem — integrating kami worship and Buddhist practice — was real and structurally difficult in early medieval Japan. However, they divide on whether the incoherent-bundle reading is the outcome: institutional historians emphasize practical integration (the reading you are generating); theological historians emphasize that honji-suijaku provided genuine doctrinal resolution and that the incoherence is a later failure to maintain it. Reform advocates (Meiji shintoists, contemporary separatists) attest that the founding problem persists unresolved and that the bundle masks deeper dysfunction. Corroboration from OUTSIDE the benefiting priesthoods is weak — those with strong incentive to defend the arrangement are the ones who benefit from it.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__incoherent_bundle, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__incoherent_bundle_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__incoherent_bundle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is CLAIMED as tangled rope because it genuinely coordinates religious labor (kami handle purity/life, buddhas handle impurity/death) while simultaneously extracting from those who would systematize it. Extraction is high (0.68 at interval end) because institutional actors avoid doctrinal costs that coherence would impose — a single coherent reading (honji-suijaku or domain-partition) would restrict institutional options and expose extraction logic. Suppression is nearly as high (0.72) because reform attempts are systematically reabsorbed: Meiji-era separation proposals were later reframed as temporary; contemporary doctrinal proposals are met with the claim that 'practice transcends doctrine' and 'harmony matters more than consistency.' Theater is the telling metric: it rises from 0.38 to 0.61 over the interval, faster than extraction itself. This indicates the system is increasingly maintained through PERFORMANCE of compatibility rather than genuine theoretical or functional work. High theater suggests a Piton pattern (institutional inertia maintaining a form whose function has atrophied), but the extraction metrics rule that out — there is active institutional interest in maintenance, not mere drift. The tangled rope classification captures this: genuine coordination function (proven by lay satisfaction and ritual efficacy) bundled with substantial extraction (institutional actors profit from avoiding coherence costs) and active enforcement (suppression of coherence-seeking reform).
 *
 * PERSPECTIVAL GAP:
 *   Institutional beneficiaries and theoretical payers occupy opposite structural positions: beneficiaries extract institutional flexibility from maintaining incoherence; payers are extracted from when they attempt to impose coherence. The constraint's effective operation depends on keeping these two seats from recognizing each other's situation. Institutions maintain this through the narrative that 'practice is prior to doctrine' (a claim that deprioritizes systematic work) and through selective reabsorption of reform proposals (reinterpreting reform as compatible with the bundle rather than as a challenge to it). The engine detects this via the power/directionality asymmetry and the rising theater ratio — the system is increasingly PERFORMING compatibility rather than achieving it.
 *
 * DIRECTIONALITY LOGIC:
 *   Syncretic institutions: beneficiaries with institutional power and arbitrage exit (they can invoke either cosmology as needed, can operate monasteries or shrines or both). Directionality near beneficiary end (d ≈ 0.15–0.25). Priesthoods: beneficiaries with institutional power and constrained exit (they benefit from the arrangement but cannot easily exit their professional tradition without identity loss). Directionality moderate-to-beneficiary (d ≈ 0.25–0.35). Systematic theologians: payer with moderate power and identity-locked exit (they must choose between suppressing their theoretical commitments to remain inside, or leaving the field entirely). Directionality target-end (d ≈ 0.65–0.75). Reform advocates: payer with moderate power and constrained exit (they work within the system to change it, but institutional suppression makes change costly and uncertain). Directionality target-end (d ≈ 0.60–0.70). Lay practitioners: diffusely beneficiary with powerless position and mobile exit (they participate without theoretical commitment and can switch between systems easily). Directionality beneficiary-end (d ≈ 0.10–0.20).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is whether kami and buddhas can coexist without doctrinal choice. The incoherent-bundle reading holds that they CANNOT coexist in a single coherent framework, but institutional actors have sustained the attempt for centuries because the incoherence is profitable. Mandatrophy appears via: (1) founding_problem_status = 'contested' — the problem that motivated shinbutsu-shugo is not agreed to be solved; (2) disappearance_verdict = 'world_rearranges' — if the incoherent bundle vanished, institutional territories would be redrawn, indicating that current arrangements depend on the constraint; (3) rising theater_ratio — the system is increasingly maintained through performance of compatibility rather than functional integration; (4) suppression of coherence-seeking reform — attempts to resolve the founding problem are systematically suppressed rather than engaged. The tangled rope classification requires active enforcement (present: institutional suppression of reform) and asymmetric extraction (present: institutions extract from systematic tradition by requiring reconciliation without resolution). The mandatrophy emerges when the founding problem persists unresolved while institutional actors profit from its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honji_suijaku_latency,
    'Is honji-suijaku a genuine doctrinal resolution of the kami-buddha relationship, or is it a sophisticated cover story that institutional actors invoke selectively while operating on contradictory premises?',
    'Trace textual development of honji-suijaku across Buddhist and Shinto sources: if it provides consistent, generative guidance for institutional practice, it is a genuine resolution; if institutional practice contradicts it while invoking it rhetorically, it functions as a cover story.',
    'If honji-suijaku is a genuine resolution, the constraint reclassifies from incoherent_bundle toward rope (coherence achieved, albeit hierarchically); if it is a cover story, it is evidence that institutional actors KNOW the incoherence and suppress the knowing. This determines whether the constraint is sustained by institutional amnesia or by institutional profit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honji_suijaku_latency, empirical, 'Doctrinal coherence vs. rhetorical deployment of honji-suijaku').

omega_variable(
    reformability_asymmetry,
    'Why do Meiji-era separation reforms, postwar religious reorganizations, and contemporary doctrinal proposals consistently fail to impose coherence, despite periodic institutional support?',
    'Process-trace the mechanism of reabsorption: track how reform proposals are reinterpreted, how they lose institutional backing, and which actors switch positions once reform pressure relaxes. If the pattern is consistent across cases, identify the structural incentive that drives reabsorption.',
    'If reformation fails due to coordination problems (institutional actors can''t agree on direction), the constraint is a Rope with failed exit attempts. If it fails due to active suppression and strategic reabsorption by benefiting actors, the constraint is a Snare or Tangled Rope extracting from reformers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reformability_asymmetry, empirical, 'Why attempts to impose coherence are systematically reabsorbed rather than institutionalized').

omega_variable(
    practice_validation_circularity,
    'Does the claim that ''shinbutsu-shugo works in practice, so theoretical coherence doesn''t matter'' mask a circular justification where institutional actors define ''working'' as ''sustaining current institutional arrangements''?',
    'Examine lay-reported religious satisfaction, ritual efficacy outcomes, and practical problem-solving across kami-only, buddha-only, and syncretic settings. If outcomes differ systematically by setting, practice has genuine validating power. If outcomes are similar across all three, the claim that syncretism is necessary for practical success is falsified.',
    'If practice-validation is genuine, the constraint is rope (real coordination benefit). If it is circular (defined by institutional interest), it is a rhetorical cover masking extraction. This determines whether lay participation is validating the constraint or being instrumentalized by it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_validation_circularity, empirical, 'Whether practical efficacy claims are empirically grounded or institutional definitions').

omega_variable(
    identity_lock_mechanism,
    'For systematic theologians in the kami-buddha tradition, is identity-lock purely professional (career dependent on working within the tradition) or also epistemic (worldview formed through training in contradictory premises)?',
    'Compare career trajectories and post-field intellectual work for scholars who remained in the tradition vs. those who exited. If exit scholars continue grappling with the incoherence, epistemic lock is present; if they move easily to other frameworks, the lock is primarily professional.',
    'Epistemic lock means the suppression of coherence-seeking is partly internalized; the scholar carries the constraint even after exit. Professional lock means the suppression is structural; exiting the field releases it. This determines how much of the measured suppression is structural coercion vs. internalized commitment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity-lock in the systematic theology tradition is epistemic or professional').

omega_variable(
    reading_contestation_asymmetry,
    'Is the incoherent_bundle reading structurally disadvantaged in institutional discourse relative to honji_suijaku and domain_partition readings, even if it describes actual practice more accurately?',
    'Analyze institutional publications, reform-era policy statements, and contemporary religious discourse for frequency and rhetorical treatment of each reading. If incoherent_bundle is underrepresented or rhetorically marginalized despite better explanatory fit, measure the institutional incentives driving the asymmetry.',
    'If the reading is suppressed despite accuracy, this is direct evidence that institutional actors benefit from advancing alternative readings and profit from the incoherence those alternatives mask. The suppression itself becomes part of the constraint''s active enforcement mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contestation_asymmetry, empirical, 'Institutional suppression of the incoherent_bundle reading despite its explanatory fit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__incoherent_bundle, theater_ratio, 0, 0.38).
narrative_ontology:measurement(kami_tr_t5, kami_buddha_ontology__incoherent_bundle, theater_ratio, 5, 0.43).
narrative_ontology:measurement(kami_tr_t10, kami_buddha_ontology__incoherent_bundle, theater_ratio, 10, 0.49).
narrative_ontology:measurement(kami_tr_t15, kami_buddha_ontology__incoherent_bundle, theater_ratio, 15, 0.55).
narrative_ontology:measurement(kami_tr_t20, kami_buddha_ontology__incoherent_bundle, theater_ratio, 20, 0.59).
narrative_ontology:measurement(kami_tr_t25, kami_buddha_ontology__incoherent_bundle, theater_ratio, 25, 0.61).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(kami_be_t5, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(kami_be_t10, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(kami_be_t15, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(kami_be_t20, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(kami_be_t25, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(kami_su_t5, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(kami_su_t10, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(kami_su_t15, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(kami_su_t20, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(kami_su_t25, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__incoherent_bundle, 0.14).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__domain_partition).

% DUAL FORMULATION NOTE:
% The kami_buddha_ontology kernel is contested across three structurally distinct readings: (1) honji_suijaku_monism — kami and buddhas are ontologically identical, hierarchically organized; (2) domain_partition — kami and buddhas are ontologically distinct, functionally divided; (3) incoherent_bundle (this story) — kami-buddha syncretism sustains contradictory commitments without resolution, driven by institutional profit. Each reading has different ε, different beneficiary/victim structure, different classification. The three are not alternative measures of one constraint; they are three constraints, linked by the kernel they contest. This story (incoherent_bundle) affects the other two because its core claim — that neither honji-suijaku nor domain-partition provides genuine resolution — changes the legitimacy conditions for invoking them: they become rhetorical deployments rather than resolved ontologies. The three readings coexist in institutional practice, with different actors invoking different readings depending on context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__incoherent_bundle, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
