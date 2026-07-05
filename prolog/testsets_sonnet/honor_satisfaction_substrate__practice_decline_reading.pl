% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__practice_decline_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Honor-Satisfaction Code as Persisting Normative Substrate (Practice-Decline Reading)
 *   domain: historical_sociology/legal_history
 *
 * SUMMARY:
 *   Formal dueling as a status-adjudication mechanism declined sharply across
 *   the 19th and early 20th centuries in most Western jurisdictions. This
 *   reading holds that the decline tracks legal prohibition, prosecution
 *   risk, and professional/institutional exclusion (bar associations, officer
 *   corps regulations, insurance and inheritance law) rather than a change in
 *   the underlying honor logic itself — the code survived in attenuated,
 *   non-lethal forms (military honor courts, regional cultures of honor,
 *   informal reputational violence) precisely where the exogenous suppression
 *   was weaker or absent. The rising theater_ratio in the measurement series
 *   reflects increasing performative/symbolic honor practice (formal
 *   apologies, ritualized reconciliation, honor-court proceedings)
 *   substituting for the removed lethal remedy as legal suppression
 *   intensified.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.42).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.58).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor-Satisfaction Code as Persisting Normative Substrate (Practice-Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "historical_sociology/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, '0c16cc86-ff1b-40d7-8e5b-6dba9b25b5ed').
narrative_ontology:cs_kernel_codification('0c16cc86-ff1b-40d7-8e5b-6dba9b25b5ed', distributed).
narrative_ontology:cs_authority_grounding('0c16cc86-ff1b-40d7-8e5b-6dba9b25b5ed', practice).
narrative_ontology:cs_interpretation_layer_present('0c16cc86-ff1b-40d7-8e5b-6dba9b25b5ed').
narrative_ontology:cs_reading_relation('0c16cc86-ff1b-40d7-8e5b-6dba9b25b5ed', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c16cc86-ff1b-40d7-8e5b-6dba9b25b5ed', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('0c16cc86-ff1b-40d7-8e5b-6dba9b25b5ed', foundational, honor_code_content_stable_under_suppression).
narrative_ontology:cs_axiom_status(honor_code_content_stable_under_suppression, holdable).
narrative_ontology:cs_axiom_grounding('0c16cc86-ff1b-40d7-8e5b-6dba9b25b5ed', honor_code_content_stable_under_suppression, empirically_contingent).
narrative_ontology:cs_axiom('0c16cc86-ff1b-40d7-8e5b-6dba9b25b5ed', secondary, exogenous_cost_sufficient_to_explain_decline).
narrative_ontology:cs_axiom_status(exogenous_cost_sufficient_to_explain_decline, holdable).
narrative_ontology:cs_axiom_grounding('0c16cc86-ff1b-40d7-8e5b-6dba9b25b5ed', exogenous_cost_sufficient_to_explain_decline, empirically_contingent).
narrative_ontology:cs_reference_frame('0c16cc86-ff1b-40d7-8e5b-6dba9b25b5ed', codes_duello_formal_remedy_era).
narrative_ontology:cs_drift_state('0c16cc86-ff1b-40d7-8e5b-6dba9b25b5ed', post_criminalization_institutional_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0c16cc86-ff1b-40d7-8e5b-6dba9b25b5ed', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, elite_male_status_claimants).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, honor_culture_regions).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, would_be_challengers_facing_prosecution).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, subordinate_status_men_denied_recourse).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, elite_male_status_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains an attenuated honor code in formal codes of conduct, courts of honor, and cashiering-for-cowardice norms; still administers status adjudication internally even though the dueling remedy itself is barred, so the underlying coordination function (settling questions of personal reliability among men who must trust each other under fire) survives inside institutional channels the civilian world lost.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps, agenda_setter).

% Still measure reputation, insult, and 'satisfaction' by honor-code logic (apologies demanded, slights tracked, standing defended) but can no longer discharge the code's traditional remedy without risking prosecution, social ostracism from respectable institutions, or forfeiting career and property; the code persists as the language they use to interpret disputes even as its enforcement mechanism has been foreclosed.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, elite_male_status_claimants, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, elite_male_status_claimants, payer).

% Communities (e.g. the rural American South) where the underlying normative substrate — reputation as a scarce, defensible good; violence as a legitimate response to disrespect — persists largely intact and continues to shape informal dispute resolution, homicide patterns, and masculinity norms, independent of whether the formal duel survives; they experience the code as living practice, not relic.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_culture_regions, beneficiary,
    organized, generational, constrained, regional).

% Individuals who, under the surviving honor logic, feel structurally obligated to answer an insult but who now face criminal liability (assault, murder charges), loss of civil standing, or exclusion from professional bodies (bar, medicine, clergy) if they act on the code; the code tells them what honor requires while the state makes acting on it ruinous, leaving them with an obligation they cannot lawfully discharge.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, would_be_challengers_facing_prosecution, payer,
    moderate, immediate, trapped, national).

% Men below the gentleman/officer class were historically excluded from the formal dueling code's protections (dueling was a marker of equal-status recognition) and remain excluded now that informal honor codes persist in attenuated regional forms; they absorb the code's expectations of manly deference and reputation-defense without ever having had legitimate access to its formal remedy, before or after decline.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, subordinate_status_men_denied_recourse, payer,
    powerless, biographical, trapped, regional).

% Criminalized dueling, prosecuted participants and seconds, and stripped legal cover from honor-based violence, which is the exogenous mechanism this reading identifies as the actual cause of practice decline; the state did not attempt to delegitimate the underlying honor norms themselves, only to raise the cost and remove the legal permissiveness that had let the practice function as a tolerated parallel justice system.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Study court records, dueling codes (codes duello), regimental honor courts, and Southern homicide statistics to trace whether the code's underlying content changed or only its permitted expression did; their comparative work across jurisdictions and eras is the primary evidence this reading draws on.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, historians_of_honor_culture, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code solved (and in attenuated form still solves) a real coordination problem among status-equals: how to settle questions of personal reliability, trustworthiness, and reputational standing without appeal to a state whose courts were seen as inadequate to matters of personal honor — a function the military officer corps still needs and still administers internally.
% TRANSFER_FUNCTION: In its dueling-era form the code transferred risk of death or injury onto challengers and seconds in exchange for reputational settlement; in its surviving attenuated form it transfers social and professional cost onto those who feel bound by honor obligations they can no longer lawfully discharge, while honor-culture communities and institutions retain the reputational-sorting benefit without bearing the violence cost that legal prohibition removed.
% ABSENT_VOICES: Subordinate-status men were never inside the formal code's protections and have no seat in either the historical dueling record or the modern attenuated version; their exclusion predates and survives the practice's decline and is not addressed by explanations focused on elite exogenous suppression.
% DISAPPEARANCE_RATIONALE: If the surviving honor-code substrate vanished, military honor courts, Southern culture-of-honor dispute patterns, and elite reputational-defense norms would need replacement mechanisms — the world does not obviously rearrange for civilians who already comply with law-mediated dispute resolution, but it would rearrange substantially for the institutions and regions where the substrate remains the operative normative language; parties dispute how much of the world actually still depends on it.
% FOUNDING_PROBLEM: Pre-modern elites lacked a trusted third-party mechanism for adjudicating insults to personal reputation and status-equality claims; dueling and its honor code provided a self-enforcing, peer-administered remedy that also signaled status (only recognized equals could duel).
% FOUNDING_PROBLEM_CORROBORATION: Military institutions and honor-culture-region ethnographers attest the founding problem (reputational adjudication among status peers) remains live in attenuated form. Legal historians and criminologists studying Southern homicide data attest, from outside the beneficiary group, that the formal remedy died from external suppression rather than internal obsolescence — supporting this reading — while separately noting the underlying reputational logic itself may have weakened, which is the rival cultural-contraction reading's territory.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, contested).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__practice_decline_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__practice_decline_reading_tests).
:- end_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) because the surviving substrate imposes real reputational costs on those bound by it, but does not organize systematic rent extraction the way a tangled rope would. Suppression is authored higher (0.58) than a pure rope would typically carry because this reading's central claim IS that suppression (legal, institutional) is the causal driver of practice decline — the metric reflects that mechanism directly, not merely residual friction. Theater ratio rises over the interval (0.10 to 0.55) tracking the substitution of symbolic/ritual honor-satisfaction (formal apology, resignation, court-martial, social exile) for the banned physical remedy — this is Goodhart-style proxy substitution under external constraint, not endogenous delegitimation.
 *
 * PERSPECTIVAL GAP:
 *   From the military officer corps' seat, this looks like a genuine, still-functioning rope: a coordination mechanism that adapted to legal constraint without losing its purpose. From the seat of a would-be challenger facing prosecution, the same substrate looks closer to a trap — an obligation the culture still enforces informally while the state forecloses its legitimate discharge, producing exactly the kind of seat divergence the engine is built to compute rather than average away.
 *
 * DIRECTIONALITY LOGIC:
 *   Military institutions and elite status claimants sit toward the beneficiary end because they retain access to the code's coordination value (internal status adjudication, reputational sorting) largely undiminished. Would-be challengers facing prosecution and subordinate-status men sit toward the target end: the former because they are bound by an obligation they cannot lawfully discharge without severe cost, the latter because they were never granted the code's formal protections and remain excluded under its surviving form. The state legal apparatus is the exogenous suppressor, not a beneficiary or victim in the code's own terms — it acts on the practice from outside the honor logic entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (peer adjudication of reputational disputes among status-equals) is authored as contested rather than flatly dead, because military and regional-culture corroboration holds it live while legal-historian corroboration from outside the beneficiary set holds the formal remedy dead but the underlying substrate persisting — this is precisely the mismatch the R5 consumer is built to flag rather than resolve by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exogenous_vs_endogenous_causal_weight,
    'Did dueling decline primarily because legal/institutional suppression raised its cost (this reading), because the honor code''s own normative content transformed (cultural_contraction_reading), or because both mechanisms operated jointly and non-independently (composite_overdetermined_reading)?',
    'Comparative jurisdictional analysis: compare decline timing and rate in regions/periods where legal suppression was strong but cultural content plausibly unchanged (e.g. military codes) against regions where suppression was weak but dignity-culture diffusion was strong; divergent timing would support decomposing the causal weight.',
    'If cultural transformation dominates, this story''s claimed_type and beneficiary structure would need revision toward the cultural_contraction_reading''s mountain-erosion framing rather than rope/coordination-failure-under-suppression framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_vs_endogenous_causal_weight, empirical, 'Kernel-level ambiguity: which reading of honor_satisfaction_substrate carries the true causal weight for practice decline.').

omega_variable(
    substrate_persistence_measurement,
    'Is the honor code''s persistence in military codes and ''culture of honor'' regions genuine substrate continuity, or is it a distinct, functionally narrower successor norm that merely uses inherited honor vocabulary?',
    'Content analysis of officer honor-code language and Southern homicide-pattern studies across the interval, tracking whether the operative logic (reputation as defensible scarce good; violence as legitimate response) remains structurally identical or has been reduced to symbolic residue.',
    'If the surviving forms are functionally distinct successor norms rather than the same substrate, the claim that ''the code persists'' collapses and this reading converges toward cultural_contraction_reading''s account of transformation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_persistence_measurement, conceptual, 'Whether attenuated survivals constitute the same normative substrate or a distinct successor.').

omega_variable(
    subordinate_exclusion_baseline,
    'Was the exclusion of subordinate-status men from the formal honor code''s protections a constant feature across the whole interval, or did exogenous suppression change the terms of that exclusion?',
    'Legal and social-history record of who was permitted to issue/accept challenges before versus after criminalization, and whether subordinate men gained or lost standing as the formal remedy receded.',
    'If exclusion terms shifted with suppression, the victim declaration for subordinate_status_men_denied_recourse needs refinement to distinguish pre-existing from suppression-era exclusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subordinate_exclusion_baseline, empirical, 'Whether subordinate exclusion is a fixed background condition or itself shaped by the suppression mechanism this reading centers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 1800, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(hono_tr_t1830, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1830, 0.18).
narrative_ontology:measurement(hono_tr_t1860, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1860, 0.3).
narrative_ontology:measurement(hono_tr_t1890, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1890, 0.42).
narrative_ontology:measurement(hono_tr_t1920, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1920, 0.5).
narrative_ontology:measurement(hono_tr_t1950, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1950, 0.55).

% Extraction over time
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1800, 0.3).
narrative_ontology:measurement(hono_be_t1830, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1830, 0.34).
narrative_ontology:measurement(hono_be_t1860, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1860, 0.38).
narrative_ontology:measurement(hono_be_t1890, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1890, 0.4).
narrative_ontology:measurement(hono_be_t1920, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1920, 0.41).
narrative_ontology:measurement(hono_be_t1950, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1950, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1800, 0.25).
narrative_ontology:measurement(hono_su_t1830, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1830, 0.4).
narrative_ontology:measurement(hono_su_t1860, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1860, 0.5).
narrative_ontology:measurement(hono_su_t1890, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1890, 0.56).
narrative_ontology:measurement(hono_su_t1920, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1920, 0.58).
narrative_ontology:measurement(hono_su_t1950, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1950, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__practice_decline_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__cultural_contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the honor_satisfaction_substrate kernel. practice_decline_reading (this story) attributes dueling's decline to exogenous legal/institutional suppression while the honor code substrate persists in attenuated form; cultural_contraction_reading attributes it to endogenous transformation of the honor code itself (culture-of-honor to culture-of-dignity); composite_overdetermined_reading holds both mechanisms operated jointly and non-independently. Each carries its own epsilon, beneficiary/victim structure, and claimed_type; they are linked via affects_constraints rather than merged, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
