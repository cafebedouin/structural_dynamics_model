% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__harm_limited_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: First Amendment Harm-Limited Reading: Protection Yields to Demonstrable Unconsented-to Harm
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the harm_limited_reading of the First
 *   Amendment speech protection kernel. It holds that the First Amendment's
 *   protection of speech contracts around a boundary of demonstrable,
 *   unconsented-to harm: when speech crosses that boundary, protection yields
 *   and regulation is permitted. This reading competes with an absolutist
 *   reading ('no law means no law') and a categorical_balancing_reading
 *   (case-by-case weighing of speech value against harm). The structural
 *   delta is that vulnerable minorities are the beneficiaries of the harm
 *   boundary, while speakers whose expression causes harm are the victims of
 *   reduced constitutional protection. The constraint requires active
 *   judicial enforcement to maintain the boundary and is contested in
 *   contemporary culture-war and campus-speech contexts.
 *
 * KEY AGENTS:
 *   - Vulnerable minorities (beneficiary/organized/identity_locked) â gain legal recourse against speech-based harm
 *   - Harm-causing speakers (payer/moderate/constrained) â bear loss of First Amendment protection and potential liability
 *   - Federal judiciary (agenda_setter/institutional/analytical) â draws and enforces the harm boundary
 *   - Free speech advocates (observer/organized/mobile) â contest expansion of the harm exception
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.62).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.6).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "First Amendment Harm-Limited Reading: Protection Yields to Demonstrable Unconsented-to Harm").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, 'e86f5d7d-abc9-4cc0-ac80-693cd6dffc55').
narrative_ontology:cs_kernel_codification('e86f5d7d-abc9-4cc0-ac80-693cd6dffc55', formalized).
narrative_ontology:cs_authority_grounding('e86f5d7d-abc9-4cc0-ac80-693cd6dffc55', lineage).
narrative_ontology:cs_interpretation_layer_present('e86f5d7d-abc9-4cc0-ac80-693cd6dffc55').
narrative_ontology:cs_reading_relation('e86f5d7d-abc9-4cc0-ac80-693cd6dffc55', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e86f5d7d-abc9-4cc0-ac80-693cd6dffc55', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('e86f5d7d-abc9-4cc0-ac80-693cd6dffc55', foundational, speech_protection_yields_to_demonstrable_harm).
narrative_ontology:cs_axiom_status(speech_protection_yields_to_demonstrable_harm, holdable).
narrative_ontology:cs_axiom_grounding('e86f5d7d-abc9-4cc0-ac80-693cd6dffc55', speech_protection_yields_to_demonstrable_harm, empirically_contingent).
narrative_ontology:cs_axiom('e86f5d7d-abc9-4cc0-ac80-693cd6dffc55', foundational, unconsented_harm_vindicates_regulation).
narrative_ontology:cs_axiom_status(unconsented_harm_vindicates_regulation, holdable).
narrative_ontology:cs_axiom_grounding('e86f5d7d-abc9-4cc0-ac80-693cd6dffc55', unconsented_harm_vindicates_regulation, instrumental).
narrative_ontology:cs_reference_frame('e86f5d7d-abc9-4cc0-ac80-693cd6dffc55', speech_liberty_with_harm_boundary).
narrative_ontology:cs_drift_state('e86f5d7d-abc9-4cc0-ac80-693cd6dffc55', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e86f5d7d-abc9-4cc0-ac80-693cd6dffc55', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, harm_causing_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups targeted by speech that causes demonstrable, unconsented-to harm. They benefit from a legal avenue to seek redress or regulatory protection when speech crosses the harm boundary. Their identity as minorities is the reason they are targeted, making exit from the protected class impossible; they rely on judicial enforcement to maintain the harm boundary.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities, beneficiary,
    organized, generational, identity_locked, national).

% Speakers whose expression is alleged or proven to cause demonstrable, unconsented-to harm to vulnerable minorities. They bear the cost of lost First Amendment protection, potential civil liability, or criminal sanction. Their exit options are constrained because the legal framework applies nationwide and preemptive self-censorship is costly to their expressive and political interests.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, harm_causing_speakers, payer,
    moderate, biographical, constrained, national).

% Federal courts that interpret the First Amendment and draw the harm boundary case by case. They administer the constraint by determining what counts as demonstrable, unconsented-to harm and whether regulation is permitted. They are bound by precedent and textual commitment but exercise substantial interpretive discretion in operationalizing the harm limit.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Civil liberties organizations and legal scholars who monitor and contest expansions of the harm exception. They file amicus briefs, publish critiques, and litigate to narrow the harm boundary. They are neither the primary beneficiaries nor the direct payers but shape the public and legal contest over the constraint's scope.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, free_speech_advocates, observer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the protection of vulnerable minorities from speech that causes demonstrable, unconsented-to harm by establishing a legal boundary where First Amendment protection yields, allowing regulation or redress without requiring the state to treat all speech as unprotected.
% TRANSFER_FUNCTION: Transfers legal immunity away from speakers whose expression causes demonstrable, unconsented-to harm and toward vulnerable minorities, who gain a actionable entitlement to regulatory or judicial remedy that they would lack under broader speech protection.
% ABSENT_VOICES: Absolutist free speech advocates who reject any harm-based exception to First Amendment protection; speakers whose expression is restricted preemptively before harm is actually demonstrated. These voices are present in public discourse but are systematically overruled in jurisdictions and institutions adopting the harm-limited framework.
% DISAPPEARANCE_RATIONALE: If the harm-limited constraint vanished overnight, vulnerable minorities would lose a recognized legal basis to seek redress for speech-based harms, and speakers would operate under broader protection; First Amendment jurisprudence would shift toward either categorical protection or unstructured balancing, reorganizing the legal incentives around expressive conduct.
% FOUNDING_PROBLEM: How to protect individuals and minority groups from tangible, unconsented-to harm caused by speech while preserving a robust sphere of protected expression against government overreach.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations and critical legal scholars outside the immediate beneficiary class attest that unremedied speech-based harm was the doctrinal impetus for exceptions like fighting words and true threats. First Amendment advocacy groups such as FIRE and libertarian legal scholars contest that the constraint is necessary, arguing that existing tort law and counterspeech suffice; corroboration is therefore split across ideological lines.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__harm_limited_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderately high because the constraint removes a constitutional shield from a class of speakers and exposes them to state and civil action; it is not maximal because the harm must be demonstrable and unconsented-to, limiting the extraction surface. Suppression (0.60) reflects the active judicial and regulatory enforcement required to maintain the harm boundary against resistant speakers and institutions. Theater_ratio (0.35) captures the performative dimension: some enforcement actions are genuine harm remediation, while a growing share involves symbolic boundary-policing that functions more to affirm the constraint's legitimacy than to remediate concrete harm. Accessibility_collapse (0.42) is moderate because alternative readings (absolutist, balancing) remain legally and politically live; the harm-limited reading has not extinguished them. Resistance (0.55) reflects persistent legal and cultural opposition from free-speech advocates and affected speakers.
 *
 * PERSPECTIVAL GAP:
 *   The vulnerable-minority seat and the speaker seat should compute to markedly different constraint types: from the minority seat, the arrangement is protective coordination that reduces exposure to harm; from the speaker seat, the same arrangement is extractive enforcement that narrows the expressive sphere. The federal judiciary experiences it as an interpretive duty. The engine derives this divergence from the structural asymmetry in beneficiary/victim declarations and exit options, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable minorities are structurally situated as beneficiaries: the constraint subsidizes their security against speech-based harm by supplying a legal remedy, yielding a directionality near the beneficiary end (low d, low Ï). Harm-causing speakers are structurally situated as victims: the constraint extracts constitutional protection from them and exposes them to sanction, yielding a directionality near the target end (high d, high Ï). The federal judiciary sits near the center as agenda_setter: it does not collect rents but wields interpretive power, and its directionality reverts to the institutional fallback. Free speech advocates are observers with mobile exit; their directionality is analytical and does not feed extraction computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â protecting minorities from speech-based harm â remains live and contested, so the constraint has not undergone simple mandatrophy. However, the theater_ratio trajectory suggests that a growing fraction of the constraint's operation is performative boundary-maintenance rather than concrete harm remediation. The classification as tangled_rope prevents mislabeling the constraint as pure extraction (it does coordinate genuine protection for vulnerable groups) and as pure coordination (it actively extracts constitutional protection from speakers).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_boundary_empirical_verifiability,
    'Can demonstrable unconsented-to harm from speech be verified independently of the ideological framework of the evaluator?',
    'Cross-ideological replication studies measuring harm from identical speech content across different political communities; judicial reliability audits of harm findings.',
    'If harm determinations are framework-dependent, the constraint''s extraction from speakers varies by who decides, making the boundary structurally unstable and susceptible to capture by partisan interpretation; if harm is independently verifiable, the boundary is objective and the constraint stabilizes as genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_boundary_empirical_verifiability, empirical, 'Whether speech-harm is measured objectively or ideologically').

omega_variable(
    kernel_reading_contest,
    'Is this constraint a genuine harm-limited exception to First Amendment protection, or does it collapse into a balancing test or absolutism under doctrinal pressure?',
    'Comparative analysis of judicial outcomes under the three sibling readings (absolutist, categorical_balancing, harm_limited) to determine which reading best predicts actual speech regulation patterns.',
    'If outcomes map better to categorical_balancing, this constraint''s claimed harm-boundary is operationally a discretionary balance; if outcomes map to absolutism, the harm exception has been judicially hollowed out. Either resolution would shift the constraint''s classification and Îµ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural location of this reading within the contested First Amendment kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fasp_harm_limited_tr_t0, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fasp_harm_limited_tr_t15, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(fasp_harm_limited_tr_t30, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(fasp_harm_limited_tr_t45, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 45, 0.33).
narrative_ontology:measurement(fasp_harm_limited_tr_t60, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(fasp_harm_limited_be_t0, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(fasp_harm_limited_be_t15, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(fasp_harm_limited_be_t30, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(fasp_harm_limited_be_t45, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 45, 0.6).
narrative_ontology:measurement(fasp_harm_limited_be_t60, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fasp_harm_limited_su_t0, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fasp_harm_limited_su_t15, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(fasp_harm_limited_su_t30, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(fasp_harm_limited_su_t45, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 45, 0.58).
narrative_ontology:measurement(fasp_harm_limited_su_t60, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 60, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one member of the first_amendment_speech_protection kernel family, alongside absolutist_reading and categorical_balancing_reading. The kernel decomposes into structurally distinct constraints because each reading assigns different Îµ, beneficiary/victim structures, and enforcement logics to the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
