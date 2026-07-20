% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanafi_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi Reading of Usul al-Fiqh (Expansive Qiyas, Ra'y, and Istihsan)
 *   domain: legal/religious/comparative
 *
 * SUMMARY:
 *   This constraint instantiates the hanafi_reading of the contested kernel
 *   usul_al_fiqh_method. In this reading, qiyas (analogical reasoning) is
 *   expansively applicable when textual sources are silent; ra'y (reasoned
 *   opinion) supplements analogy where it reaches limits; and istihsan
 *   (juristic preference) permits departure from strict analogy for public
 *   interest. The reading has the lowest textual restrictiveness and the
 *   highest scope for jurist-driven analogical expansion among the four
 *   canonical readings of the kernel. It is historically anchored in the
 *   rationalist circles of Kufa and Basra, later institutionalized under
 *   Abbasid and Ottoman state patronage.
 *
 * KEY AGENTS:
 *   - hanafi_jurist_class: agenda_setter and beneficiary (institutional/identity_locked) â administers expansive analogy and collects jurist authority
 *   - textualist_scholars: payer (organized/constrained) â bear the cost of methodological marginalization
 *   - state_appointers: beneficiary (powerful/mobile) â gain legal adaptability for governance
 *   - lay_muslim_community: payer (powerless/constrained) â subject to jurist-derived rulings with limited exit
 *   - non_hanafi_jurists: excluded (moderate/constrained) â structurally absent from dominant institutions
 *   - comparative_legal_historians: observer (analytical) â trace the kernel's institutional divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.58).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Reading of Usul al-Fiqh (Expansive Qiyas, Ra'y, and Istihsan)").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "legal/religious/comparative").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, 'c2dd326c-1bae-4cc1-8ebe-12e2932965c4').
narrative_ontology:cs_kernel_codification('c2dd326c-1bae-4cc1-8ebe-12e2932965c4', formalized).
narrative_ontology:cs_authority_grounding('c2dd326c-1bae-4cc1-8ebe-12e2932965c4', lineage).
narrative_ontology:cs_interpretation_layer_present('c2dd326c-1bae-4cc1-8ebe-12e2932965c4').
narrative_ontology:cs_reading_relation('c2dd326c-1bae-4cc1-8ebe-12e2932965c4', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2dd326c-1bae-4cc1-8ebe-12e2932965c4', usul_al_fiqh_method__shafii_reading, influences).
narrative_ontology:cs_reading_relation('c2dd326c-1bae-4cc1-8ebe-12e2932965c4', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('c2dd326c-1bae-4cc1-8ebe-12e2932965c4', foundational, qiyas_expansively_applicable).
narrative_ontology:cs_axiom_status(qiyas_expansively_applicable, holdable).
narrative_ontology:cs_axiom_grounding('c2dd326c-1bae-4cc1-8ebe-12e2932965c4', qiyas_expansively_applicable, conventional).
narrative_ontology:cs_axiom('c2dd326c-1bae-4cc1-8ebe-12e2932965c4', foundational, istihsan_overrides_qiyas).
narrative_ontology:cs_axiom_status(istihsan_overrides_qiyas, holdable).
narrative_ontology:cs_axiom_grounding('c2dd326c-1bae-4cc1-8ebe-12e2932965c4', istihsan_overrides_qiyas, conventional).
narrative_ontology:cs_reference_frame('c2dd326c-1bae-4cc1-8ebe-12e2932965c4', expansive_qiyas_framework).
narrative_ontology:cs_drift_state('c2dd326c-1bae-4cc1-8ebe-12e2932965c4', mature_classical_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c2dd326c-1bae-4cc1-8ebe-12e2932965c4', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, state_appointers).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, lay_muslim_community).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, rationalist_jurist_authority).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, analogical_expansion_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Hanafi methodological framework through teaching, ifta, and judicial appointment; derives rulings for unprecedented cases via expansive qiyas, ra'y, and istihsan; trains successors in rationalist legal science; collects institutional prestige, state-backed appointments, and the authority to define legal outcomes where texts are silent.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class, beneficiary).

% Uphold strict scriptural limits on legal innovation; bear the cost of methodological marginalization in jurisdictions where Hanafi rationalism dominates courts, endowments, and state appointments; find their claim to limit innovation treated as epistemic disobedience rather than legitimate difference.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, textualist_scholars, payer,
    organized, generational, constrained, global).

% Appoint judges and muftis to state positions; benefit from a flexible legal toolkit that can generate rulings for novel administrative problems and legitimate state policy through learned legal reasoning; rely on the Hanafi jurist class to supply adaptability without overt textual violation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, state_appointers, beneficiary,
    powerful, biographical, mobile, national).

% Receives fatwas and court rulings derived through analogical expansion and juristic preference; lacks training to interrogate the usul al-fiqh chain; depends on the jurist class for legal guidance; indirectly bears the cost when textual clarity is overridden by istihsan for asserted public interest.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, lay_muslim_community, payer,
    powerless, biographical, constrained, national).

% Maliki, Shafi'i, and Hanbali jurists whose methodological premises are structurally excluded from official legal education and judicial appointment in jurisdictions where Hanafi rationalism is the established usul; their alternative readings of the kernel are treated as minority or dissenting positions rather than co-equal frameworks.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, non_hanafi_jurists, excluded,
    moderate, generational, constrained, global).

% Analyze the structural divergence between Hanafi rationalism and textualist methodologies; document how the Hanafi reading expanded jurist authority while marginalizing strict scriptural limits; trace the institutional embedding of the kernel across Abbasid, Ottoman, and Mughal jurisdictions.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a reproducible method for deriving legal rulings in the absence of explicit textual guidance, enabling jurists across diverse regions to coordinate on novel cases through shared analogical procedures and reasoned opinion.
% TRANSFER_FUNCTION: Moves epistemic authority and institutional prestige from the scriptural text and textualist scholars to the rationalist jurist class trained in analogical expansion; transfers the power to define legal limits from the textual corpus to the jurist's methodological discretion and public-interest assessment.
% ABSENT_VOICES: Textualist scholars and non-Hanafi jurists who would argue for stricter scriptural limits are structurally excluded from dominant institutions where Hanafi rationalism governs appointments and curricula; lay communities lacking usul training cannot interrogate the analogical chain.
% DISAPPEARANCE_RATIONALE: If the Hanafi methodological reading vanished, the distribution of authority within Islamic legal institutions would shift toward textualist schools; the jurist class would lose its expansive analogical toolkit, state-legal adaptability would contract, and the textualist claim to limit innovation would regain institutional ground in the domains where Hanafi method had dominated.
% FOUNDING_PROBLEM: The foundational texts (Quran and authenticated hadith) do not explicitly address the vast majority of specific legal cases arising in rapidly expanding Muslim societies; a reproducible method was needed to generate normative guidance from sources while maintaining claim to divine authority.
% FOUNDING_PROBLEM_CORROBORATION: The problem of textual silence is attested by early Hanafi jurists and historians of Islamic law. However, textualist scholars contest that textual silence is as extensive as Hanafi method assumes, arguing instead for greater epistemic humility and alternative textual sourcing. Modern comparative legal historians corroborate that the founding problem was partly genuine administrative need and partly a construct enabling jurist authority.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanafi_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as tangled_rope because it carries a genuine coordination function (deriving law for unprecedented cases through shared analogical method) alongside asymmetric extraction that concentrates authority in the rationalist jurist class while marginalizing textualist scholars. Extractiveness (0.68) is substantial: the scope for jurist-driven expansion decouples legal output from textual source density. Suppression (0.58) is moderate-to-high because the reading's persistence in dominant institutions depends on excluding textualist alternatives from appointments and curricula. Theater ratio (0.42) reflects that a growing share of later Hanafi reasoning performs analogy to justify positions reached through other methodological shortcuts. Accessibility collapse (0.65) is significant: once inside the Hanafi framework, strict textual alternatives appear methodologically naive. Resistance (0.55) is present from textualist scholars and competing schools. The measurement series use one shared time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   From the Hanafi jurist seat, the constraint is experienced as a necessary intellectual apparatus for extending divine guidance to new cases; from the textualist scholar seat, the same structure operates as an epistemic extraction mechanism that replaces textual limits with jurist discretion. The state seat experiences it as a flexible governance resource. The lay community experiences it as an opaque but unavoidable authority layer. The engine computes these divergences from the structural data rather than from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The hanafi_jurist_class is the primary beneficiary (low directionality) because the constraint subsidizes their epistemic authority and institutional position. The textualist_scholars are the primary victims (high directionality) because the constraint extracts from their claim to limit innovation and marginalizes their methodological standing. The lay_muslim_community sits toward the target end due to powerlessness and constrained exit. State_appointers are mixed: they benefit from flexibility but are not the primary extractive seat. Non-hanafi_jurists are excluded rather than directly targeted, placing them at moderate-high directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure rope (which would ignore the asymmetric extraction from textualist scholars and the lay community) or as pure snare (which would deny the genuine coordination problem of deriving law from silent sources). The founding problemâtextual silence on novel casesâwas real, but its solution was captured by a specific professional class. If the founding problem were dead and the arrangement persisted purely by inertia with no concentrated beneficiary, it would compute toward piton; instead, the jurist class remains a concentrated beneficiary, keeping it in tangled_rope territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hanafi_reading_kernel_position,
    'This constraint is the Hanafi reading of the usul_al_fiqh_method kernel. Would classification change if the Maliki, Shafi''i, or Hanbali reading were adopted as the dominant institutional framework?',
    'Comparative structural analysis across the constraint family, examining how beneficiary/victim arrays redistribute under each reading.',
    'If a sibling reading were dominant, the beneficiary/victim structure would invert or redistribute; for example, textualist scholars would become beneficiaries in the Hanbali reading while rationalist jurists would shift toward the payer/excluded seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hanafi_reading_kernel_position, conceptual, 'Position of this reading within the contested usul al-fiqh kernel').

omega_variable(
    jurist_expansion_motive,
    'Does expansive qiyas reflect a genuine epistemic necessity imposed by textual silence, or does it construct textual silence to expand jurist authority?',
    'Historical corpus analysis of cases where qiyas was applied versus where textual sources were arguably sufficient; comparison with Hanbali handling of the same case set.',
    'If the latter, extractiveness is higher than the coordination story suggests, pushing the computed type toward snare; if the former, the coordination function is more robust and the tangled_rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurist_expansion_motive, empirical, 'Whether expansive analogy serves genuine necessity or jurist power').

omega_variable(
    state_patronage_dependency,
    'To what extent does the Hanafi reading''s persistence depend on state patronage rather than methodological superiority?',
    'Historical comparison of Hanafi jurisprudential output and institutional reproduction in state-backed versus non-state-backed contexts.',
    'If primarily state-dependent, the constraint is more extractive and enforcement-driven; if organically reproduced across diverse contexts, the coordination function is stronger and the rope component is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_patronage_dependency, empirical, 'Role of state patronage in Hanafi method persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_hanafi_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(usul_hanafi_tr_t20, usul_al_fiqh_method__hanafi_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(usul_hanafi_tr_t40, usul_al_fiqh_method__hanafi_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(usul_hanafi_tr_t60, usul_al_fiqh_method__hanafi_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement(usul_hanafi_tr_t80, usul_al_fiqh_method__hanafi_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(usul_hanafi_tr_t100, usul_al_fiqh_method__hanafi_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(usul_hanafi_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(usul_hanafi_be_t20, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(usul_hanafi_be_t40, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(usul_hanafi_be_t60, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(usul_hanafi_be_t80, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 80, 0.65).
narrative_ontology:measurement(usul_hanafi_be_t100, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(usul_hanafi_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(usul_hanafi_su_t20, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(usul_hanafi_su_t40, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(usul_hanafi_su_t60, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(usul_hanafi_su_t80, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 80, 0.56).
narrative_ontology:measurement(usul_hanafi_su_t100, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__hanbali_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__maliki_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'usul al-fiqh method' conflates four structurally distinct methodological readings. This story isolates the Hanafi reading (expansive qiyas, ra'y, istihsan); sibling stories isolate the Maliki, Shafi'i, and Hanbali readings. Each has distinct epsilon, beneficiary/victim structure, and authority grounding. They form a constraint family linked by shared kernel history but separated by Îµ-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
