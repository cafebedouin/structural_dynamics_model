% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__shafii_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__shafii_reading
 *   human_readable: Shafi'i Reading of Usul al-Fiqh Source Hierarchy
 *   domain: religious/legal/comparative
 *
 * SUMMARY:
 *   This constraint instantiates the Shafi'i reading of the
 *   usul_al_fiqh_method kernel: a systematized meta-discipline governing
 *   Islamic legal source hierarchy in which authenticated hadith is
 *   prerequisite to legal derivation, qiyas is permitted only when
 *   authenticated hadith is absent, and ijma is restricted to the consensus
 *   of the Prophet's Companions. The reading concentrates gatekeeping
 *   authority in hadith transmission specialists and subordinates jurists who
 *   claim authority through rationalist methods alone. It is one of four
 *   structurally distinct readings (hanafi, maliki, shafii, hanbali) that
 *   decompose the same kernel into separate constraints with different
 *   epsilon profiles.
 *
 * KEY AGENTS:
 *   - hadith_transmission_specialists: Primary beneficiary/agenda_setter (institutional/constrained) â controls source authentication and curriculum.
 *   - rationalist_jurists: Primary target (organized/constrained) â bears methodological subordination and loss of autonomous authority.
 *   - legal_students: Secondary target/beneficiary (moderate/identity_locked) â bears training costs and identity fusion, receives systematized method.
 *   - jurisprudential_historians: Analytical observer (analytical/analytical) â sees the full structural asymmetry from outside the doctrinal commitment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.65).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.63).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafi'i Reading of Usul al-Fiqh Source Hierarchy").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "religious/legal/comparative").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, '241bf7a8-473e-4a09-98c6-89c657352d40').
narrative_ontology:cs_kernel_codification('241bf7a8-473e-4a09-98c6-89c657352d40', fixed_text).
narrative_ontology:cs_authority_grounding('241bf7a8-473e-4a09-98c6-89c657352d40', lineage).
narrative_ontology:cs_interpretation_layer_present('241bf7a8-473e-4a09-98c6-89c657352d40').
narrative_ontology:cs_reading_relation('241bf7a8-473e-4a09-98c6-89c657352d40', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('241bf7a8-473e-4a09-98c6-89c657352d40', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('241bf7a8-473e-4a09-98c6-89c657352d40', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('241bf7a8-473e-4a09-98c6-89c657352d40', foundational, authenticated_hadith_prerequisite).
narrative_ontology:cs_axiom_status(authenticated_hadith_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('241bf7a8-473e-4a09-98c6-89c657352d40', authenticated_hadith_prerequisite, deontological).
narrative_ontology:cs_axiom('241bf7a8-473e-4a09-98c6-89c657352d40', foundational, companions_only_ijma).
narrative_ontology:cs_axiom_status(companions_only_ijma, holdable).
narrative_ontology:cs_axiom_grounding('241bf7a8-473e-4a09-98c6-89c657352d40', companions_only_ijma, conventional).
narrative_ontology:cs_reference_frame('241bf7a8-473e-4a09-98c6-89c657352d40', prophetic_textual_supremacy).
narrative_ontology:cs_drift_state('241bf7a8-473e-4a09-98c6-89c657352d40', post_classical_synthesis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('241bf7a8-473e-4a09-98c6-89c657352d40', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, rationalist_jurists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, legal_students).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, legal_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authenticate hadith through isnad and matn criticism; their certification determines whether a report enters the legal source hierarchy. They teach and enforce the usul al-fiqh curriculum that makes their expertise prerequisite to legal derivation. Their institutional authority grows as the meta-discipline spreads; exiting the framework would mean abandoning their gatekeeping position and scholarly identity.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, beneficiary).

% Previously exercised legal authority through qiyas, ra'y, and istihsan. Under the Shafi'i reading, their methods are permitted only when authenticated hadith is absent, and ijma is restricted to Companions' consensus, curtailing their ability to generate novel rulings. They must either master hadith science to regain authority, accept subordinate status, or migrate to schools that preserve rationalist methods.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, rationalist_jurists, payer,
    organized, biographical, constrained, global).

% Must master hadith authentication before advancing to legal derivation; their career trajectory and scholarly identity are formed within the usul curriculum. They receive a systematized method that reduces arbitrary reasoning, but bear the cost of prolonged training and acceptance of the source hierarchy as given. Exit is difficult because professional identity is fused with the curriculum.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, legal_students, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, legal_students, beneficiary).

% Analyze the formation and operation of usul al-fiqh as a historical and comparative legal phenomenon. They observe the concentration of authority in hadith specialists and the methodological subordination of rationalist jurists without being bound by the school's doctrinal commitments.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, jurisprudential_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Systematizes legal derivation from revealed sources by establishing a fixed hierarchy: Quran, authenticated hadith, Companion consensus, and subordinated analogical reasoning; prevents arbitrary juristic opinion by requiring textual authentication as a prerequisite.
% TRANSFER_FUNCTION: Moves gatekeeping authority over legal sources from general jurists to hadith authentication specialists; transfers methodological prestige and curricular control from rationalist tools (expansive qiyas, ra'y) to textual-authentication disciplines.
% ABSENT_VOICES: Jurists advocating unrestricted rationalism (ra'y), customary-practice jurists relying on 'urf, and proponents of expansive ijma beyond the Companions are structurally marginalized; their exclusion is necessary for the source hierarchy to function as designed.
% DISAPPEARANCE_RATIONALE: If the hadith-authentication prerequisite vanished, rationalist jurists would regain autonomous authority to derive law from reason and analogy, legal training would abandon the isnad-focused prerequisite, the gatekeeping power of hadith specialists would collapse, and the entire usul architecture would reorganize around different source priorities.
% FOUNDING_PROBLEM: Early Islamic jurisprudence lacked a systematic method for ranking sources; conflicting rulings proliferated from weak hadith reports, unmoored juristic opinion (ra'y), and local customs, producing legal uncertainty and inter-school conflict.
% FOUNDING_PROBLEM_CORROBORATION: Hadith specialists attest the problem remains live through ongoing forgery and weak reports. Rationalist jurists and modern historians attest the problem was substantially addressed by systematization but that the arrangement now concentrates authority; external corroboration from comparative legal historians supports the view that usul al-fiqh solved coordination while creating a guild-based gatekeeping structure.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__shafii_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__shafii_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is moderately high because the hadith-authentication prerequisite transfers gatekeeping power to a specialized guild, creating asymmetric extraction from rationalist jurists who must now submit to textual certification. Suppression (0.63) tracks the active methodological suppression required to keep rationalist tools (expansive qiyas, ra'y, istihsan) subordinated to isnad criticism. Theater ratio (0.25) is relatively low because the hadith-authentication function is genuinely operational, though some share of activity performs guild boundary maintenance rather than epistemic filtration. Accessibility collapse (0.60) reflects that once inside the Shafi'i framework, rationalist alternatives largely collapse as valid legal options. Resistance (0.50) captures ongoing methodological contestation from rationalist jurists and rival schools. The measurement series run on a single shared time grid (0-30) to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The hadith-transmission-specialist seat experiences the constraint as necessary epistemic order: without authentication, the prophetic sunnah dissolves into conjecture, and legal chaos returns. The rationalist-jurist seat experiences the same structure as guild capture: a power transfer that dresses methodological monopoly in the language of textual fidelity. The engine computes this divergence from identical structural data using directionality and exit modulations; the authored claim does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith transmission specialists are declared beneficiaries and agenda-setters with constrained exit; the engine derives low directionality (near the beneficiary pole), dampening effective extraction into a coordination subsidy. Rationalist jurists are declared victims and payers with constrained exit; the engine derives high directionality (near the target pole), amplifying effective extraction. Legal students sit near the symmetric middle because they both pay (prolonged training, identity lock) and benefit (systematized method), though identity_locked exit nudges them toward the target side.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this constraint as either pure rope or pure snare. The founding problem â legal chaos from unranked sources â was genuine, and the systematization of usul al-fiqh solved a real coordination problem (source hierarchy, reduced arbitrary reasoning). That genuine coordination function rules out snare classification. However, the solution asymmetrically concentrated authority in hadith specialists and methodologically subordinated rationalist jurists, creating identifiable victims and active enforcement requirements. That asymmetric extraction rules out rope classification. Tangled rope is the structurally accurate classification because the coordination and extraction travel through the same institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    usul_kernel_reading_ambiguity,
    'Is the Shafi''i systematization of usul al-fiqh the necessary structure of Islamic legal reasoning, or one reading among several competing framings of the same kernel?',
    'Cross-reading comparison of legal outcomes and institutional authority distribution across the four classical madhhabs.',
    'If necessary structure, the constraint approaches rope; if one reading among many, it is a contested commitment system with extraction potential.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(usul_kernel_reading_ambiguity, conceptual, 'Whether the Shafi''i reading is structurally necessary or one contingent reading of the usul kernel.').

omega_variable(
    hadith_authenticity_empirical_basis,
    'Is the authentication process (isnad criticism) a reliable empirical filter on prophetic origin, or a conventional gatekeeping mechanism?',
    'Text-critical and historical analysis of the hadith corpus (isnad vs. matn criticism, comparison with early manuscripts, and forgery detection studies).',
    'If empirically unreliable, the gatekeeping authority is extraction dressed as epistemology; if reliable, the coordination function is stronger and the constraint leans toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hadith_authenticity_empirical_basis, empirical, 'Whether hadith authentication is epistemically grounded or guild convention.').

omega_variable(
    rationalist_subordination_mechanism,
    'Is the subordination of qiyas and ra''y a necessary epistemic hierarchy, or a power transfer to a specialized guild of hadith transmitters?',
    'Comparative sociology of jurists versus hadith specialists in the 9th-10th centuries; analysis of institutional incentives and curriculum control.',
    'If power transfer, the constraint is more extractive; if epistemic necessity, more coordinative. Would shift effective extraction estimates for rationalist jurist seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rationalist_subordination_mechanism, conceptual, 'Whether methodological subordination is guild capture or epistemic discipline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_shafii_tr_t0, usul_al_fiqh_method__shafii_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(usul_shafii_tr_t5, usul_al_fiqh_method__shafii_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(usul_shafii_tr_t10, usul_al_fiqh_method__shafii_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(usul_shafii_tr_t15, usul_al_fiqh_method__shafii_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(usul_shafii_tr_t20, usul_al_fiqh_method__shafii_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(usul_shafii_tr_t25, usul_al_fiqh_method__shafii_reading, theater_ratio, 25, 0.24).
narrative_ontology:measurement(usul_shafii_tr_t30, usul_al_fiqh_method__shafii_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(usul_shafii_be_t0, usul_al_fiqh_method__shafii_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(usul_shafii_be_t5, usul_al_fiqh_method__shafii_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(usul_shafii_be_t10, usul_al_fiqh_method__shafii_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(usul_shafii_be_t15, usul_al_fiqh_method__shafii_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(usul_shafii_be_t20, usul_al_fiqh_method__shafii_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(usul_shafii_be_t25, usul_al_fiqh_method__shafii_reading, base_extractiveness, 25, 0.64).
narrative_ontology:measurement(usul_shafii_be_t30, usul_al_fiqh_method__shafii_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(usul_shafii_su_t0, usul_al_fiqh_method__shafii_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(usul_shafii_su_t5, usul_al_fiqh_method__shafii_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(usul_shafii_su_t10, usul_al_fiqh_method__shafii_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(usul_shafii_su_t15, usul_al_fiqh_method__shafii_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(usul_shafii_su_t20, usul_al_fiqh_method__shafii_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(usul_shafii_su_t25, usul_al_fiqh_method__shafii_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(usul_shafii_su_t30, usul_al_fiqh_method__shafii_reading, suppression_requirement, 30, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, identity_coordination).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The usul_al_fiqh_method kernel decomposes into four structurally distinct readings (hanafi, maliki, shafii, hanbali) because each assigns different priority weights to textual sources, rational methods, and customary practice, producing different epsilon profiles and beneficiary/victim structures. The Shafi'i reading is distinguished by its subordination of qiyas to authenticated hadith and its restriction of ijma to Companions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
