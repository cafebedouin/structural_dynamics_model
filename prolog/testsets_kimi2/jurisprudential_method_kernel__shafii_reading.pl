% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__shafii_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__shafii_reading
 *   human_readable: Shafi'i Four-Tier Hierarchy with Hadith Authentication Arbiter
 *   domain: Islamic Jurisprudence / Legal Philosophy / Institutional History
 *
 * SUMMARY:
 *   The Shafi'i reading of the jurisprudential method kernel
 *   institutionalizes a strict four-tier hierarchy of legal sources: Qur'an,
 *   Hadith, Ijma, then Qiyas. Al-Shafi'i's methodological standardization
 *   resolves earlier schools' inconsistencies by making hadith transmission
 *   the arbiter of disputes, concentrating authentication authority in the
 *   hadith scholarly class. This constraint story models the standing
 *   arrangement under the Shafi'i reading: medium-high extractiveness
 *   directed at customary practice and analogical reasoning, which lose
 *   independent source status, while hadith scholars gain gatekeeping
 *   authority. The coordination function is genuineâsource inconsistency is
 *   reducedâbut distributed asymmetrically.
 *
 * KEY AGENTS:
 *   - hadith_scholars: Primary beneficiary/agenda-setter (organized/constrained) â control hadith authentication and enforce the hierarchy
 *   - customary_practitioners: Primary target (moderate/constrained) â lose independent source standing for community practice
 *   - analogical_jurists: Secondary target (organized/constrained) â qiyas demoted to fourth tier, istihsan rejected
 *   - rationalist_jurists: Excluded voice (moderate/constrained) â independent reason excluded from the hierarchy
 *   - historical_critical_scholars: Analytical observer (analytical/analytical) â evaluates authenticity claims from outside the tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, 0.68).
domain_priors:suppression_score(jurisprudential_method_kernel__shafii_reading, 0.72).
domain_priors:theater_ratio(jurisprudential_method_kernel__shafii_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__shafii_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__shafii_reading, "Shafi'i Four-Tier Hierarchy with Hadith Authentication Arbiter").
narrative_ontology:topic_domain(jurisprudential_method_kernel__shafii_reading, "Islamic Jurisprudence / Legal Philosophy / Institutional History").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__shafii_reading, 'ad14b874-493e-49cf-b610-4f406b1598b2').
narrative_ontology:cs_kernel_codification('ad14b874-493e-49cf-b610-4f406b1598b2', fixed_text).
narrative_ontology:cs_authority_grounding('ad14b874-493e-49cf-b610-4f406b1598b2', lineage).
narrative_ontology:cs_interpretation_layer_present('ad14b874-493e-49cf-b610-4f406b1598b2').
narrative_ontology:cs_reading_relation('ad14b874-493e-49cf-b610-4f406b1598b2', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad14b874-493e-49cf-b610-4f406b1598b2', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad14b874-493e-49cf-b610-4f406b1598b2', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('ad14b874-493e-49cf-b610-4f406b1598b2', foundational, strict_four_tier_revelatory_hierarchy).
narrative_ontology:cs_axiom_status(strict_four_tier_revelatory_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('ad14b874-493e-49cf-b610-4f406b1598b2', strict_four_tier_revelatory_hierarchy, theological).
narrative_ontology:cs_axiom('ad14b874-493e-49cf-b610-4f406b1598b2', foundational, isnad_as_epistemic_arbiter).
narrative_ontology:cs_axiom_status(isnad_as_epistemic_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('ad14b874-493e-49cf-b610-4f406b1598b2', isnad_as_epistemic_arbiter, instrumental).
narrative_ontology:cs_reference_frame('ad14b874-493e-49cf-b610-4f406b1598b2', prophetic_transmission_authority).
narrative_ontology:cs_drift_state('ad14b874-493e-49cf-b610-4f406b1598b2', post_orientalist_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ad14b874-493e-49cf-b610-4f406b1598b2', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, hadith_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, customary_practitioners).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, analogical_jurists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the authentication of prophetic reports through isnad criticism and biographical evaluation. Their certification determines whether a report enters the second tier of the legal hierarchy. They teach, transmit, and judge chain reliability, and their authority is amplified when hadith becomes the arbiter of disputes among sources.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hadith_scholars, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, hadith_scholars, beneficiary).

% Rely on the continuous practice of Muslim communities, especially Medinan custom, as an independent source of law. Under the strict hierarchy, their practice is subordinated to authenticated hadith and loses independent standing; they must either justify custom through hadith or accept its demotion to a subsidiary evidentiary role.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, customary_practitioners, payer,
    moderate, biographical, constrained, regional).

% Extend divine intent through qiyas and istihsan. The four-tier hierarchy demotes qiyas to the fourth tier and rejects istihsan as an invalid independent source. Their analogical reasoning must yield to Qur'an, authenticated hadith, and consensus, sharply restricting its legitimate domain.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, analogical_jurists, payer,
    organized, biographical, constrained, continental).

% Would argue for the independent legitimacy of human reason (ra'y) and juristic preference (istihsan) in deriving law. They are structurally excluded from the four-tier hierarchy because independent reasoning is not recognized as a valid source; their methodological objections are ruled out by the standardization.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, rationalist_jurists, excluded,
    moderate, biographical, constrained, regional).

% Apply historical and textual criticism to the hadith corpus from outside the traditional isnad framework. They examine the provenance of prophetic reports using documentary, comparative, and linguistic evidence rather than classical chain-of-transmission evaluation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, historical_critical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__shafii_reading, hadith_scholars).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves inconsistencies between Qur'anic injunctions, prophetic reports, communal consensus, and analogical reasoning by establishing a strict priority order and making hadith authentication the arbiter of conflicts among sources.
% TRANSFER_FUNCTION: Moves jurisprudential authority and source-status from customary practice and independent analogical reasoning to the hadith scholars who control authentication chains and the interpretive hierarchy.
% ABSENT_VOICES: Rationalist jurists who treat juristic preference and independent reason as legitimate sources, and regional customary jurists whose local practice is delegitimized; they are excluded by the methodological boundary that admits only authenticated transmission.
% DISAPPEARANCE_RATIONALE: If the strict four-tier hierarchy vanished, legal methodology would revert to the pre-standardization state where Qur'anic text, local custom, ra'y, and prophetic reports competed as unranked sources; the concentrated authority of hadith scholars would collapse and customary practice would regain independent legal standing.
% FOUNDING_PROBLEM: Early Islamic jurisprudence suffered from source inconsistency: jurists in different regions relied on contradictory combinations of Qur'an, local custom, personal reasoning, and prophetic reports with no agreed method for resolving conflicts between them.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary historians of Islamic law from outside the benefiting parties (e.g., Wael Hallaq, George Makdisi) attest that the problem of source inconsistency was substantially resolved by the tenth century. Hadith scholars and Shafi'i jurists attest the problem remains live because new cases continually require authentication and adjudication; the corroboration from external historical scholarship supports a shifted-function reading.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__shafii_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is medium-high because the hierarchy does not merely organize sources but actively subordinates customary practice and independent analogical reasoning to hadith authentication, extracting authority from those seats. Suppression (0.72) reflects the active enforcement required: isnad criticism, exclusion of weak reports, scholarly gatekeeping, and the delegitimation of non-conforming methods. Theater ratio (0.30) is moderate: authentication is functionally real but increasingly performative as the scholarly apparatus defends its monopoly against historical criticism. Accessibility collapse (0.60) captures that alternatives (custom as independent source, istihsan) are substantially collapsed but persist in rival schools. Resistance (0.55) reflects ongoing methodological dispute with Hanafi and Maliki jurists defending their alternative source hierarchies.
 *
 * PERSPECTIVAL GAP:
 *   From the hadith scholars' seat, the constraint is necessary coordination that preserves prophetic practice against arbitrary reasoning and regional inconsistency. From the customary practitioners' and analogical jurists' seats, the same structure extracts their independent authority and forces their sources through a filter controlled by another class. The engine computes this divergence: agenda-setters with constrained exit experience low effective extraction; payers with constrained exit experience high effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith scholars are declared beneficiaries and agenda-setters; their structural directionality is near the beneficiary pole. Customary practitioners and analogical jurists are declared victims/payers; their directionality is near the target pole. The extraction is amplified for the payers because their exit is constrainedâthey cannot easily leave the scholarly field or the Islamic legal economyâand their scope is regional. The beneficiary's extraction is damped into subsidy because they control the authentication machinery and the hierarchical rules.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâsource inconsistency in early jurisprudenceâwas substantially resolved by the standardization. The hierarchy could have become a piton if it persisted purely by inertia after obsolescence. However, the coordination function remains live because new legal cases continually require source adjudication, and the enforcement infrastructure continues to coordinate scholarly activity. The risk of mandatrophy is present but not dominant: the constraint is not merely theatrical maintenance but continues to coordinate, even as it extracts asymmetrically from subordinated sources.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hadith_authenticity_empirical_status,
    'Does the classical isnad authentication system accurately identify prophetic origin, or does modern historical analysis demonstrate substantial post-prophetic fabrication in the authenticated corpus?',
    'Comparative historical-linguistic analysis of hadith texts against dated documentary evidence, archaeological corroboration, and statistical motif analysis.',
    'If mass fabrication is demonstrated, the foundational axiom that authenticated hadith is revelation-derived is overridden, collapsing the authority structure and reclassifying the extraction from customary practice as unjustified coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hadith_authenticity_empirical_status, empirical, 'Whether the hadith corpus is historically authentic or substantially fabricated').

omega_variable(
    shafii_hanafi_logical_relation,
    'Does the Shafi''i strict hierarchy foreclose the Hanafi expansive-qiyas reading within a single framework, or do the readings merely coexist as institutionalized alternatives?',
    'Analysis of cross-madhhab jurists'' methodological commitments: whether any historical jurist successfully held both the strict four-tier hierarchy and expansive istihsan simultaneously.',
    'If foreclosed, the engine''s computed foreclosure flag should fire; if coexistent, the readings form a pluralist constraint family rather than mutually exclusive competitors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shafii_hanafi_logical_relation, conceptual, 'Logical relation between Shafi''i and Hanafi readings of the same kernel').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem of source inconsistency been permanently solved by the four-tier hierarchy, or does the hierarchy address an ongoing coordination need?',
    'Historical analysis of legal uniformity post-standardization versus continued inter-madhhab dispute; functional analysis of whether novel cases still require the hierarchy to avoid arbitrariness.',
    'If dead, the constraint shows mandatrophy signs and piton drift potential; if live, the coordination function remains genuine despite asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding problem is solved or ongoing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__shafii_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__shafii_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(juri_tr_t20, jurisprudential_method_kernel__shafii_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__shafii_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(juri_tr_t60, jurisprudential_method_kernel__shafii_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(juri_tr_t80, jurisprudential_method_kernel__shafii_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__shafii_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(juri_be_t20, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement(juri_be_t60, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(juri_be_t80, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 80, 0.66).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(juri_su_t20, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(juri_su_t60, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 60, 0.64).
narrative_ontology:measurement(juri_su_t80, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 80, 0.69).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__shafii_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The jurisprudential_method_kernel decomposes into four sibling readings (hanafi, maliki, shafii, hanbali) of the same usul al-fiqh kernel, each instantiating a distinct hierarchy of sources with different epsilon profiles and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
