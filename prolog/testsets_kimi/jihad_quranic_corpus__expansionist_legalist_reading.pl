% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__expansionist_legalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__expansionist_legalist_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: jihad_quranic_corpus__expansionist_legalist_reading
 *   human_readable: Expansionist Legalist Reading of Jihad
 *   domain: Islamic Jurisprudence / Comparative Religious Law / Political Theology
 *
 * SUMMARY:
 *   This constraint instantiates the expansionist legalist reading of the
 *   jihad quranic corpus, a classical Sunni jurisprudential framework that
 *   treats offensive military campaigns to establish Islamic governance as a
 *   communal obligation (fard kifaya) under caliphal monopoly, subject to
 *   conditions of prior invitation, proportionality, and imam authority. It
 *   is one reading of a contested kernel; the defensive spiritual reading and
 *   the revolutionary vanguard reading are structurally distinct sibling
 *   constraints. The framework coordinates military expansion under
 *   centralized authority while extracting sovereignty, territory, and
 *   subordinate legal status from non-Muslim populations. The caliphal state
 *   and classical jurist class derive concentrated authority and material
 *   benefit from the arrangement, whereas non-Muslim populations are assigned
 *   liminal statuses (ahl al-harb or dhimmi) without voice in the juridical
 *   discourse that fixes their condition. The authored metrics describe a
 *   constraint that is highly extractive and actively enforced, with
 *   substantial legal-theological theater providing legitimizing cover for
 *   conquest â the engine will compute whether this divergence from the
 *   rope-like coordination claim produces tangled_rope or snare
 *   classification per seat.
 *
 * KEY AGENTS:
 *   - caliphate_authority: Primary agenda_setter (institutional/global scope) â monopolizes declaration, commands enforcement, collects extracted surplus
 *   - classical_jurists: Primary analytical beneficiary (powerful/national scope) â legitimate the framework through fiqh, derive dhimmi rules, benefit from interpretive authority
 *   - muslim_community: Diffuse beneficiary (organized/national scope) â fulfills religious obligation, gains territorial and spiritual expansion
 *   - non_muslim_populations: Primary payer/target (powerless/regional scope) â bear subordinate status, jizya, and liminal categorization
 *   - defensive_dissenters: Excluded voices (moderate/national scope) â hold defensive/spiritual readings marginalized by the legalist framework
 *   - non_muslim_scholars: Excluded observers (powerless/regional scope) â entirely outside the fiqh discourse determining their political status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.78).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.76).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Expansionist Legalist Reading of Jihad").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "Islamic Jurisprudence / Comparative Religious Law / Political Theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, '23707541-7555-4fcb-b05d-1706ef61b912').
narrative_ontology:cs_kernel_codification('23707541-7555-4fcb-b05d-1706ef61b912', fixed_text).
narrative_ontology:cs_authority_grounding('23707541-7555-4fcb-b05d-1706ef61b912', lineage).
narrative_ontology:cs_interpretation_layer_present('23707541-7555-4fcb-b05d-1706ef61b912').
narrative_ontology:cs_reading_relation('23707541-7555-4fcb-b05d-1706ef61b912', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('23707541-7555-4fcb-b05d-1706ef61b912', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('23707541-7555-4fcb-b05d-1706ef61b912', foundational, communal_obligation_to_expand_islamic_governance).
narrative_ontology:cs_axiom_status(communal_obligation_to_expand_islamic_governance, holdable).
narrative_ontology:cs_axiom_grounding('23707541-7555-4fcb-b05d-1706ef61b912', communal_obligation_to_expand_islamic_governance, deontological).
narrative_ontology:cs_axiom('23707541-7555-4fcb-b05d-1706ef61b912', foundational, caliphal_monopoly_on_offensive_jihad).
narrative_ontology:cs_axiom_status(caliphal_monopoly_on_offensive_jihad, holdable).
narrative_ontology:cs_axiom_grounding('23707541-7555-4fcb-b05d-1706ef61b912', caliphal_monopoly_on_offensive_jihad, conventional).
narrative_ontology:cs_reference_frame('23707541-7555-4fcb-b05d-1706ef61b912', classical_caliphal_expansion).
narrative_ontology:cs_drift_state('23707541-7555-4fcb-b05d-1706ef61b912', modern_westphalian_order, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('23707541-7555-4fcb-b05d-1706ef61b912', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, caliphate_authority).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, muslim_community).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, classical_jurists).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_jurisprudence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Monopolizes the declaration of offensive jihad, commands military campaigns, collects jizya and spoils, and enforces the juridical distinction between dar al-islam and dar al-harb. Legitimacy depends on maintaining the expansionist legal framework and preventing alternative claimants to military authority.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, caliphate_authority, agenda_setter,
    institutional, generational, constrained, global).

% Fulfills religious obligation through support of caliphal expansion; receives spiritual merit and access to an expanding dar al-islam. Apostasy or public rejection carries severe social and legal penalties, limiting exit from the communal obligation framework.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, muslim_community, beneficiary,
    organized, biographical, constrained, national).

% Derive rules for prior invitation, proportionality, and dhimmi status; legitimate conquest through fiqh. Benefit from institutional patronage, scholarly prestige, and the exclusive authority to define liminal categories for non-Muslim populations.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, classical_jurists, beneficiary,
    powerful, generational, constrained, national).

% Receive invitation to Islam; if refused, face military campaign or dhimmi subordination. Pay jizya, accept limited legal rights, and occupy a permanent liminal category. Conversion or flight are the only exits, both coercively structured by the framework.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations, payer,
    powerless, biographical, constrained, regional).

% Muslim scholars and mystics who hold that jihad is primarily internal or strictly defensive. Marginalized in the classical legalist corpus; their views are acknowledged but systematically subordinated in political jurisprudence and denied institutional influence.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, defensive_dissenters, excluded,
    moderate, generational, constrained, national).

% Theological and political voices from outside the Islamic juridical tradition. They have no standing in the fiqh discourse that categorizes their peoples as combatants or dhimmis, and no mechanism to contest the legal framework from within its epistemic structure.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_scholars, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__expansionist_legalist_reading, caliphate_authority).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__expansionist_legalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes legitimate military expansion under caliphal authority, providing a rule-bound mechanism to establish Islamic governance, prevent decentralized tribal raiding, and integrate conquered territories into a unified juridical order governed by sharia.
% TRANSFER_FUNCTION: Moves sovereignty, territory, and subordinate legal status from non-Muslim populations to the Islamic state and Muslim community; transfers jizya revenue and war spoils to the caliphal treasury; transfers interpretive authority and institutional prestige to the classical jurist class.
% ABSENT_VOICES: Non-Muslim juridical and political voices are structurally excluded from the fiqh discourse that determines their status. Muslim scholars emphasizing defensive or spiritual jihad are present in the broader tradition but excluded from the expansionist legalist political framework that dominates state doctrine.
% DISAPPEARANCE_RATIONALE: The caliphate's monopoly on legitimate expansion would dissolve, the dhimmi/ahl al-harb categorical architecture would collapse, non-Muslim populations would no longer be assigned liminal statuses by this framework, and the classical jurist class would lose a central pillar of political jurisprudence and patronage.
% FOUNDING_PROBLEM: Absence of Islamic governance in territories beyond the original polity, combined with the need to regulate tribal military energies and establish a scalable, rule-bound mechanism for territorial integration under divine law.
% FOUNDING_PROBLEM_CORROBORATION: Classical jurists of the Hanafi, Shafi'i, and Maliki schools attest the problem as live and foundational. Modern critical historians and legal scholars outside the beneficiary set argue the framework emerged as post-facto legitimation of imperial conquest rather than a response to a pre-existing governance vacuum; no non-Muslim corroboration from the formative period exists.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__expansionist_legalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the framework systematically transfers sovereignty, territory, and revenue from non-Muslim populations to the Islamic state under threat of force, with jizya and dhimmi status constituting durable extraction. Suppression (0.76) is high because the constraint persists only through active caliphal enforcement and the juridical suppression of alternative political orders (non-Islamic governance is categorically illegitimate). Theater_ratio (0.48) reflects the substantial legal formalism (invitation, proportionality, imam authority) that performs legitimacy while the underlying structure remains conquest. Accessibility_collapse (0.72) is high because once the framework is applied, non-Muslim populations face a collapsed choice set: conversion, flight, combat, or subordinate dhimmi status â all structured by the constraint. Resistance (0.58) is moderate because non-Muslim populations resist militarily and culturally, but the caliphal-jurist alliance maintains epistemic and coercive dominance. The temporal series show extraction, theater, and enforcement maturing together from the early conquests through classical codification, stabilizing at high levels.
 *
 * PERSPECTIVAL GAP:
 *   The caliphal and jurist seats experience this constraint as a rope-like coordination mechanism â it prevents chaotic tribal violence, establishes rule-bound governance, and channels military energy into a centralized, legally accountable structure. From the non-Muslim population seat, the same framework operates as extraction: the invitation is a performative prelude to subordination, proportionality is calibrated to conquest efficiency, and the legal categories exist to rationalize dispossession. The engine computes this divergence from identical structural data; the authored claim (tangled_rope) does not adjudicate the gap but names the hybrid structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Caliphate_authority, classical_jurists, and muslim_community are declared beneficiaries with constrained or institutional exits, yielding low directionality (subsidy/near-beneficiary end). Non_muslim_populations are declared victims with powerless status and constrained exit, yielding high directionality (near full-target). Defensive_dissenters and non_muslim_scholars are excluded rather than victimized by direct extraction, but their structural position is target-like because the constraint suppresses their voice and alternatives. No directionality overrides are needed: the automatic derivation from beneficiary/victim declarations plus exit options accurately captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resists snare classification because it possesses a genuine coordination function: it centralizes military force under caliphal authority, preventing fragmented tribal raiding, and provides a rule-bound procedure for integrating conquered territories. A pure snare would lack this coordination backbone and rely entirely on coercion without juridical infrastructure. However, it is not a rope because the coordination is inseparable from asymmetric extraction: the same legal framework that integrates territories also fixes non-Muslim populations in a permanently subordinate, rent-paying status. The R5 genealogy interview shows the founding problem (absence of governance) is contested, with external scholars arguing post-facto legitimation â this contestation prevents mandatrophy from being declared resolved and blocks reclassification to piton despite the high theater ratio.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the expansionist legalist reading of the jihad quranic corpus. Does its extractiveness derive from the textual kernel itself or from the interpretive layer that constructs caliphal monopoly and dhimmi status?',
    'Textual-linguistic analysis of the Quranic corpus independent of classical fiqh, compared against the juridical superstructure erected by Hanafi, Shafi''i, and Maliki jurists in the first three centuries AH.',
    'If extraction is kernel-embedded, sibling readings face foreclosure pressure; if interpretive-layer-dependent, the expansionist reading is a constructed constraint and its classification as tangled_rope versus snare depends on the layer''s separability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Whether extraction resides in kernel or interpretive layer').

omega_variable(
    kernel_codification_ambiguity,
    'Is the Quranic kernel best framed as fixed_text (stable recension with interpretive layer) or distributed (inherently ambiguous, no single adjudicating authority)?',
    'Historical-textual analysis of Quranic redaction and the emergence of tafsir tradition; if the text is demonstrably stabilized, fixed_text holds; if the text underdetermines the legalist reading without the interpretive layer, distributed is more accurate.',
    'If distributed, authority_grounding shifts from lineage to distributed, interpretation_layer_present becomes invalid, and the constraint''s extraction is harder to attribute to a single authoritative reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_ambiguity, conceptual, 'Whether the kernel is fixed text or distributed authority').

omega_variable(
    expansionist_vs_defensive_coexistence,
    'Can the expansionist legalist reading and the defensive spiritual reading coexist within a single Muslim actor''s commitment framework, or are they structurally mutually exclusive?',
    'Survey of classical jurists who also authored tasawwuf or defensive-only treatises; if the same historical figures held both, coexistence is established.',
    'If coexistent, the constraint''s effective suppression is lower than if the expansionist reading forecloses internal dissent; if mutually exclusive, the constraint operates as a more totalizing extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansionist_vs_defensive_coexistence, conceptual, 'Coexistence of expansionist and defensive jihad readings').

omega_variable(
    state_monopoly_coordination_or_extraction,
    'Is the caliphal monopoly on jihad declaration structurally necessary to prevent decentralized violence, or does it concentrate extractive power while performing coordination?',
    'Comparative historical analysis of decentralized Islamic military movements versus caliphal campaigns, measuring civilian harm, territorial stability, and revenue extraction.',
    'If the monopoly genuinely prevents worse violence, part of the measured extraction is coordination cost; if not, the monopoly is pure extraction and the constraint trends toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_monopoly_coordination_or_extraction, empirical, 'Whether caliphal monopoly coordinates or extracts').

omega_variable(
    dhimmi_status_structural_necessity,
    'Is the liminal dhimmi/ahl al-harb status categorization structurally necessary for the governance function, or is it a separable extractive layer?',
    'Historical cases where Islamic governance accommodated non-Muslim legal equality versus dhimmi subordination; if governance persisted without dhimmi status, the category is separable extraction.',
    'If separable, the constraint''s extractiveness includes a removable layer; if inseparable, the extraction is constitutive of the coordination type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dhimmi_status_structural_necessity, conceptual, 'Whether non-Muslim liminal status is necessary or extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(jiha_tr_t60, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(jiha_tr_t80, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 80, 0.45).
narrative_ontology:measurement(jiha_tr_t100, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 100, 0.48).
narrative_ontology:measurement(jiha_tr_t120, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 120, 0.48).
narrative_ontology:measurement(jiha_tr_t140, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 140, 0.48).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(jiha_be_t60, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(jiha_be_t80, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 80, 0.74).
narrative_ontology:measurement(jiha_be_t100, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 100, 0.77).
narrative_ontology:measurement(jiha_be_t120, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 120, 0.78).
narrative_ontology:measurement(jiha_be_t140, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 140, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(jiha_su_t60, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(jiha_su_t80, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 80, 0.74).
narrative_ontology:measurement(jiha_su_t100, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 100, 0.76).
narrative_ontology:measurement(jiha_su_t120, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 120, 0.76).
narrative_ontology:measurement(jiha_su_t140, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 140, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'jihad' conflates three structurally distinct constraints: defensive spiritual struggle, state-monopolized expansionist legalism, and revolutionary vanguard violence. Each has distinct beneficiaries, victims, authority structures, and epsilon values. They are linked as a constraint family sharing the Quranic kernel but instantiating different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
