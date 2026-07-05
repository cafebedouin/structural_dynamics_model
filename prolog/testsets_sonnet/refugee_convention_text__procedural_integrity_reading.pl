% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__procedural_integrity_reading, []).

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
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: 1951 Refugee Convention — Procedural Integrity Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Refugee Convention
 *   text kernel: the procedural-integrity reading, under which the
 *   Convention's core commitment is fair individualized process rather than
 *   any fixed substantive protection threshold. States may narrow who
 *   qualifies as a refugee in substance so long as each claimant receives an
 *   individualized, appealable hearing. This is structurally distinct from
 *   the restrictive-sovereignty reading (which permits narrowing the
 *   substantive definition itself, not just the process applied to it) and
 *   the expansive-humanitarian reading (which treats the substantive
 *   protection threshold as the non-negotiable element). Do not read this
 *   file as describing or averaging over those readings — they are separate
 *   constraint stories linked via network.affects_constraints. Over the
 *   interval, the procedural apparatus has grown heavier (more tiers, more
 *   documentation, more expedited pathways) even as substantive protection
 *   has not measurably expanded, consistent with procedure increasingly
 *   substituting for outcome.
 *
 * KEY AGENTS:
 *   - host_state_asylum_bureaucracies: agenda_setter (institutional/arbitrage) — designs and administers the procedural apparatus
 *   - procedural_compliance_consultancies: beneficiary (organized/mobile) — monetizes procedural defensibility on both sides
 *   - asylum_seekers_in_offshore_processing: payer (powerless/trapped) — receives degraded process dressed as adequate process
 *   - undocumented_border_crossers_denied_hearing_access: payer (powerless/trapped) — receives no process at all
 *   - unhcr_and_treaty_monitoring_bodies: observer/excluded (institutional/analytical) — sets benchmarks with no enforcement teeth
 *   - receiving_state_publics: beneficiary (organized/constrained) — gets rule-of-law legitimacy without ceding admission control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.52).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.58).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "1951 Refugee Convention — Procedural Integrity Reading").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, 'ddce95da-a649-4e43-917a-ff88b4ed6322').
narrative_ontology:cs_kernel_codification('ddce95da-a649-4e43-917a-ff88b4ed6322', fixed_text).
narrative_ontology:cs_authority_grounding('ddce95da-a649-4e43-917a-ff88b4ed6322', practice).
narrative_ontology:cs_interpretation_layer_present('ddce95da-a649-4e43-917a-ff88b4ed6322').
narrative_ontology:cs_reading_relation('ddce95da-a649-4e43-917a-ff88b4ed6322', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('ddce95da-a649-4e43-917a-ff88b4ed6322', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_axiom('ddce95da-a649-4e43-917a-ff88b4ed6322', foundational, process_integrity_is_the_non_negotiable_element).
narrative_ontology:cs_axiom_status(process_integrity_is_the_non_negotiable_element, holdable).
narrative_ontology:cs_axiom_grounding('ddce95da-a649-4e43-917a-ff88b4ed6322', process_integrity_is_the_non_negotiable_element, conventional).
narrative_ontology:cs_axiom('ddce95da-a649-4e43-917a-ff88b4ed6322', foundational, substantive_outcome_is_severable_from_procedural_adequacy).
narrative_ontology:cs_axiom_status(substantive_outcome_is_severable_from_procedural_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('ddce95da-a649-4e43-917a-ff88b4ed6322', substantive_outcome_is_severable_from_procedural_adequacy, instrumental).
narrative_ontology:cs_reference_frame('ddce95da-a649-4e43-917a-ff88b4ed6322', individualized_hearing_minimum_standard).
narrative_ontology:cs_drift_state('ddce95da-a649-4e43-917a-ff88b4ed6322', post_2015_expedited_and_offshore_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ddce95da-a649-4e43-917a-ff88b4ed6322', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, host_state_asylum_bureaucracies).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, procedural_compliance_consultancies).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, asylum_seekers_in_offshore_processing).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, undocumented_border_crossers_denied_hearing_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, receiving_state_publics).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, due_process_supremacy_doctrine).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, procedure_outcome_separability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the status-determination procedure — interview protocols, appeal tiers, evidentiary standards, timelines. They can narrow substantive definitions of persecution or particular social group while still claiming compliance, as long as a hearing, a record, and an appeal exist. They control what counts as 'fair process' and therefore control the practical protection rate without ever touching the treaty's substantive language.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, host_state_asylum_bureaucracies, agenda_setter,
    institutional, generational, arbitrage, national).

% Law firms, audit bodies, and NGO-adjacent contractors that certify offshore and expedited processing regimes as 'procedurally sound.' They are paid by states to design defensible hearing architectures and by advocacy groups to litigate procedural defects. Either way their business model depends on procedure remaining the site of contest rather than outcome.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, procedural_compliance_consultancies, beneficiary,
    organized, biographical, mobile, global).

% Held in third-country processing centers where a hearing technically occurs — an interpreter, a form, a review — but access to counsel, evidence gathering, and appeal is structurally degraded by distance, detention, and resource scarcity. Under this reading they are not wronged if the procedure was 'fair,' even when the substantive outcome denies protection to someone with a genuine claim; procedural adequacy substitutes for substantive vindication.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_seekers_in_offshore_processing, payer,
    powerless, immediate, trapped, regional).

% Intercepted, summarily returned, or funneled into expedited removal before any individualized hearing occurs. Under a procedural-integrity reading these are the clearest violations — no assessment occurred at all — yet enforcement of the standard depends entirely on the same state apparatus accused of the shortcut, and the affected people usually have no standing or access to challenge it before removal.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, undocumented_border_crossers_denied_hearing_access, payer,
    powerless, immediate, trapped, national).

% Issue guidance on what constitutes a fair individualized procedure and monitor state compliance, but have no binding enforcement power over sovereign asylum systems. Their procedural benchmarks are cited by both compliant and non-compliant states, and their findings of procedural deficiency carry reputational but rarely operational consequences — they observe the standard's application but cannot compel it.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, unhcr_and_treaty_monitoring_bodies, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, unhcr_and_treaty_monitoring_bodies, excluded).

% Benefit from a framework that lets states demonstrate rule-of-law legitimacy ('everyone gets a fair hearing') while still controlling aggregate admission numbers through procedural design choices — detention conditions, documentation burdens, timeline compression — that never require amending or violating the Convention's substantive text.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, receiving_state_publics, beneficiary,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__procedural_integrity_reading, host_state_asylum_bureaucracies).
narrative_ontology:fixing_cost_class(refugee_convention_text__procedural_integrity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine multilateral problem: without a shared minimum procedural standard, states could deny protection through pure administrative fiat with zero individualized review, and claimants would have no common benchmark to invoke across jurisdictions. Procedural integrity gives every claimant, everywhere, a floor of individualized process regardless of how generously or restrictively the receiving state reads the substantive definitions.
% TRANSFER_FUNCTION: Moves the site of contest from substantive protection outcomes to procedural adequacy. States retain the discretion to narrow who qualifies as a refugee in substance, provided the process by which that narrowing is applied to each individual is defensible. This transfers real protection away from claimants in weak-procedure jurisdictions toward states and the compliance industry that certifies their processes, while transferring reputational insulation to states that can point to a functioning hearing apparatus regardless of aggregate outcomes.
% ABSENT_VOICES: Undocumented crossers who are removed before any hearing exists have no voice in adjudicating whether procedure was followed — they are outside the system the standard purports to govern. Offshore detainees have degraded access to the legal counsel needed to contest procedural deficiencies in the first place, so the class most affected by procedural failure is structurally least able to litigate it.
% DISAPPEARANCE_RATIONALE: If the procedural-integrity reading disappeared, states would lose the primary legitimating vocabulary that lets them narrow substantive protection without appearing to breach the Convention; asylum litigation would either collapse into pure substantive-outcome disputes (favoring the expansive humanitarian reading) or into pure sovereign-discretion claims (favoring the restrictive reading) — the current equilibrium of narrow-substance-but-fair-process would no longer be available as a stable compliance posture, and compliance consultancies built around 'defensible process design' would lose their function.
% FOUNDING_PROBLEM: Post-WWII drafters needed a standard that could bind diverse legal systems with different substantive protection philosophies to a common minimum: that no one should be returned to persecution without at least an individualized look at their case, regardless of what a state's domestic politics said about who deserved refuge.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR executive committee conclusions and independent refugee law scholarship (outside both state administrations and the compliance consultancy sector) affirm that individualized assessment remains a live and necessary safeguard against summary return; but the same outside scholarship documents that 'procedural integrity' has increasingly been invoked by destination states to justify offshore and expedited regimes that satisfy the letter of individualized review while producing outcomes indistinguishable from blanket exclusion — suggesting the founding problem persists in principle but the reading's operational deployment has partly decoupled from it.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__procedural_integrity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__procedural_integrity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) and theater_ratio (0.44) both track the same drift: as procedural apparatus has thickened (more forms, more tiers, more expedited/offshore variants), an increasing share of that apparatus performs fairness without delivering it, particularly for populations with least capacity to invoke it. Suppression (0.58) reflects that access to the procedure itself — not just outcomes within it — is increasingly gated by detention, geography, and documentation burden. accessibility_collapse is moderate (0.4) rather than high because formal avenues to contest procedural deficiency still nominally exist in most jurisdictions (litigation, UNHCR referral); resistance is substantial (0.62) because refugee law advocates and monitoring bodies actively contest procedural adequacy claims in courts and international fora. All temporal metrics share one time grid (1951, 1975, 1995, 2005, 2015, 2024) as required.
 *
 * DIRECTIONALITY LOGIC:
 *   Host state bureaucracies sit near the beneficiary end: they set the procedural rules and can adjust them to manage aggregate outcomes without touching substantive treaty text, i.e. minimal d. Compliance consultancies similarly benefit — their function depends on procedure remaining contestable terrain. Asylum seekers in offshore processing and undocumented crossers denied any hearing sit at the target end: trapped exit options, immediate time horizon, and structurally degraded capacity to contest the procedure applied (or not applied) to them drive d toward the full-target value. UNHCR sits as an analytical observer with no capture from the arrangement, though its standard-setting role gives it a partial stake, hence the excluded secondary role — it can criticize but not compel.
 *
 * MANDATROPHY ANALYSIS:
 *   The procedural-integrity reading was built to solve the summary-return problem (states denying protection without any individualized look at the claim). That problem is only partly dead: outright summary return without any hearing persists for undocumented crossers, meaning the founding problem remains live for exactly the population least covered by the procedural apparatus supposedly protecting them. For asylum seekers formally inside the system, the founding problem has been substantially answered (they do get individualized hearings) even as substantive protection outcomes have not improved in step — meaning the reading's own success (procedural coverage) has been repurposed to legitimate substantive restriction. The tangled_rope classification captures this: genuine coordination function (a real floor against pure fiat) coexists with genuine asymmetric extraction (procedure as legitimating cover for restriction) requiring active bureaucratic and legal enforcement to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedure_outcome_decoupling,
    'Can procedural adequacy be genuinely decoupled from substantive protection outcomes, or does ''fair process'' inevitably import a de facto substantive threshold through the standards used to judge fairness?',
    'Comparative empirical study of protection grant rates across jurisdictions with formally similar procedural safeguards but different substantive definitional narrowness — if grant rates diverge sharply despite procedural parity, decoupling is real; if procedural rigor and grant rates move together, the distinction is largely rhetorical.',
    'If decoupling is illusory, this reading collapses into a disguised version of the restrictive_sovereignty_reading (procedure as cover for substantive narrowing); if decoupling is real, the reading identifies a genuinely independent coordination function distinct from both sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedure_outcome_decoupling, conceptual, 'Whether procedural fairness is separable from substantive protection threshold in practice.').

omega_variable(
    offshore_procedural_guarantee_sufficiency,
    'Does offshore processing with nominal procedural guarantees (interpreter, hearing, appeal) satisfy this reading''s non-negotiable process-integrity requirement, or does distance and detention degrade the guarantees below the threshold that makes them meaningful?',
    'Litigation outcomes and UNHCR audits of specific offshore processing regimes assessing effective (not nominal) access to counsel, evidence-gathering, and appeal within realistic timeframes.',
    'If offshore guarantees are found systematically insufficient, the beneficiary/victim structure sharpens — offshore processing regimes reclassify from tangled_rope toward snare for that subpopulation; if sufficient, the tangled_rope coordination function is more robust than the metrics here assume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offshore_procedural_guarantee_sufficiency, empirical, 'Whether offshore processing can structurally satisfy procedural-integrity standards.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the choice of procedural-integrity framing (over the restrictive or expansive readings) itself driven by which reading best legitimates a state''s existing enforcement posture, rather than by independent legal reasoning about what the 1951 Convention text requires?',
    'Track which reading individual states and tribunals invoke over time relative to shifts in migration enforcement policy — if reading selection correlates with policy shifts rather than preceding them, selection pressure is evidenced.',
    'If reading selection is policy-driven, all three kernel readings function partly as legitimating vocabularies chosen after the fact, which would recontextualize this story''s claimed_type as more contested than the isolated metrics suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether reading choice among the three kernel readings is causally downstream of enforcement policy rather than upstream of it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement(refu_tr_t1975, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(refu_tr_t1995, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1995, 0.24).
narrative_ontology:measurement(refu_tr_t2005, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(refu_tr_t2015, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(refu_tr_t2024, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1951, 0.18).
narrative_ontology:measurement(refu_be_t1975, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1975, 0.24).
narrative_ontology:measurement(refu_be_t1995, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1995, 0.33).
narrative_ontology:measurement(refu_be_t2005, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(refu_be_t2015, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2015, 0.46).
narrative_ontology:measurement(refu_be_t2024, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1951, 0.2).
narrative_ontology:measurement(refu_su_t1975, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1975, 0.28).
narrative_ontology:measurement(refu_su_t1995, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1995, 0.37).
narrative_ontology:measurement(refu_su_t2005, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(refu_su_t2015, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(refu_su_t2024, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, expansive_humanitarian_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the refugee_convention_text kernel. restrictive_sovereignty_reading treats the Convention as a floor permitting maximal sovereign discretion over substantive definitions; expansive_humanitarian_reading treats it as an unbendable humanitarian mandate with a broad, fixed substantive threshold; this file (procedural_integrity_reading) relocates the non-negotiable element to individualized process fairness, leaving substantive definitional narrowness largely open. Each reading has its own ε, its own beneficiary/victim structure, and its own classification — they are not the same constraint measured three ways; they are three constructions of the ambiguous treaty text that different courts, states, and advocates actually deploy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
