% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__institutional_displacement_reading, []).

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
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Dueling's Decline by Institutional Displacement
 *   domain: historical_sociology/legal_history
 *
 * SUMMARY:
 *   This constraint story describes the decline of dueling as a primary
 *   dispute-resolution mechanism due to the rise and increasing efficacy of
 *   alternative institutional structures like courts, banking systems, and
 *   libel law. This reading posits that dueling was not primarily suppressed
 *   by moral condemnation or legal prohibition, but rather outcompeted by
 *   superior, less costly, and more reliable alternatives. The constraint is
 *   a 'rope' because it represents a coordination on a new, more efficient
 *   social protocol for dispute resolution, with minimal extraction from
 *   those who voluntarily adopted the alternatives. Dueling persisted in
 *   institutional gaps but became a fringe practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.25).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Dueling's Decline by Institutional Displacement").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "historical_sociology/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, 'ede1fda9-7d3b-47e1-9cfd-cee5f91f16ea').
narrative_ontology:cs_kernel_codification('ede1fda9-7d3b-47e1-9cfd-cee5f91f16ea', implicit).
narrative_ontology:cs_authority_grounding('ede1fda9-7d3b-47e1-9cfd-cee5f91f16ea', practice).
narrative_ontology:cs_interpretation_layer_present('ede1fda9-7d3b-47e1-9cfd-cee5f91f16ea').
narrative_ontology:cs_reading_relation('ede1fda9-7d3b-47e1-9cfd-cee5f91f16ea', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('ede1fda9-7d3b-47e1-9cfd-cee5f91f16ea', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('ede1fda9-7d3b-47e1-9cfd-cee5f91f16ea', foundational, institutional_efficacy_drives_social_adoption).
narrative_ontology:cs_axiom_status(institutional_efficacy_drives_social_adoption, holdable).
narrative_ontology:cs_axiom_grounding('ede1fda9-7d3b-47e1-9cfd-cee5f91f16ea', institutional_efficacy_drives_social_adoption, empirically_contingent).
narrative_ontology:cs_reference_frame('ede1fda9-7d3b-47e1-9cfd-cee5f91f16ea', dueling_as_primary_dispute_resolution).
narrative_ontology:cs_drift_state('ede1fda9-7d3b-47e1-9cfd-cee5f91f16ea', late_19th_century, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ede1fda9-7d3b-47e1-9cfd-cee5f91f16ea', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, courts).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, banking_system).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, libel_law_practitioners).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, disputants_seeking_non_violent_resolution).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__institutional_displacement_reading, honor_culture_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who previously might have resorted to dueling now have more reliable, less lethal, and often more effective institutional avenues for resolving disputes concerning honor, reputation, or financial claims. They benefit from the reduced social pressure to duel and the availability of alternatives.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, disputants_seeking_non_violent_resolution, beneficiary,
    moderate, biographical, mobile, local).

% The judicial system provided a formal, state-backed mechanism for dispute resolution, offering legal remedies for grievances that previously might have led to duels. Its increasing efficacy and legitimacy gradually displaced dueling as a primary means of redress.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% The development of formal credit and banking systems provided mechanisms for resolving financial disputes and enforcing contracts, reducing the need for duels over monetary honor. It benefited from a more stable and predictable commercial environment.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, banking_system, beneficiary,
    institutional, generational, analytical, national).

% Lawyers and legal institutions specializing in libel and slander provided a civil mechanism for defending reputation, offering damages or retractions instead of physical combat. They benefited from the expansion of this legal domain.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, libel_law_practitioners, beneficiary,
    organized, biographical, mobile, national).

% Individuals who still adhered to traditional honor codes found their preferred method of dispute resolution increasingly marginalized and legally proscribed. While not directly 'victims' of extraction, they bore the cost of social and legal disapproval for maintaining dueling practices.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, honor_culture_adherents, payer,
    powerless, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a set of alternative, non-violent, and legally sanctioned mechanisms (courts, libel law, commercial arbitration) for resolving disputes, particularly those involving honor, reputation, and financial claims, thereby coordinating social order around state-backed institutions.
% TRANSFER_FUNCTION: Transferred the authority and social legitimacy of dispute resolution from individual honor codes and private combat to formal state and commercial institutions. It transferred the 'cost' of resolution from physical risk to legal fees or judicial process.
% ABSENT_VOICES: Proponents of dueling as a necessary component of a robust honor culture, who saw institutional alternatives as inadequate for certain types of grievances, were increasingly marginalized from mainstream discourse and legal recognition.
% DISAPPEARANCE_RATIONALE: If the institutional alternatives (courts, libel law, banking) had not emerged or been widely adopted, the social landscape for dispute resolution would be vastly different, likely retaining dueling or similar forms of private combat as more prominent features, particularly in areas where state authority was weak or distrusted.
% FOUNDING_PROBLEM: The problem of resolving disputes, particularly those involving honor and reputation, in a manner that was perceived as just and effective within a given social context, without necessarily resorting to lethal violence.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars corroborate that the problem of dispute resolution is perennial. The institutional displacement reading argues that while the *form* of resolution changed, the underlying social need for mechanisms to address grievances remains live, now met by different institutions. No single party benefits from this claim; it is an analytical observation.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).
:- end_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the shift was largely voluntary, driven by the superior utility of the new institutions rather than direct coercion. Suppression (0.25) reflects the increasing legal proscription of dueling, but this was secondary to the institutional pull. Theater ratio is negligible (0.05) as the new institutions were genuinely functional, not performative. Accessibility collapse is high (0.7) because the alternatives effectively closed off the 'need' for dueling for most grievances. Resistance is low (0.1) because the shift was largely accepted by the majority of disputants.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the emerging institutional beneficiaries, the decline of dueling was a clear societal improvement, a shift towards more rational and orderly dispute resolution. From the perspective of honor culture adherents, it was a loss of a vital mechanism for maintaining personal dignity, even if they eventually adopted the new norms. The classification as 'rope' reflects the overall societal benefit and voluntary adoption, while acknowledging the 'cost' to a specific cultural group.
 *
 * DIRECTIONALITY LOGIC:
 *   The new institutions (courts, banking, libel law) are beneficiaries as they gained legitimacy and expanded their scope. Disputants seeking non-violent resolution are also beneficiaries, as they gained safer and more effective options. Adherents of honor culture, while not 'victims' in the extractive sense, bore the cost of their preferred mechanism becoming socially and legally disfavored, hence their 'payer' role.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_of_institutional_pull_vs_legal_push,
    'To what extent did the ''pull'' of superior institutional alternatives (this reading) outweigh the ''push'' of legal prohibition and moral condemnation (contraction reading) in dueling''s decline?',
    'Comparative historical analysis of regions with varying legal enforcement against dueling but similar institutional development, or vice versa. Quantitative analysis of legal cases vs. adoption rates of alternative dispute resolution.',
    'If legal push was primary, the constraint might lean more towards a ''tangled_rope'' or ''snare'' due to active suppression. If institutional pull was primary, the ''rope'' classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_of_institutional_pull_vs_legal_push, empirical, 'Distinguishing the relative causal weight of institutional attraction versus legal/moral suppression in dueling''s decline.').

omega_variable(
    cultural_vs_structural_shift_primacy,
    'Is the institutional displacement a primary structural cause, or a secondary effect of a deeper cultural shift in honor codes (as argued by the contraction reading)?',
    'Tracing the temporal precedence: did institutional alternatives emerge and gain traction before or after significant shifts in cultural attitudes towards honor and violence? This requires detailed historical and anthropological data.',
    'If cultural shift was primary, the ''institutional displacement'' might be re-framed as a consequence, not the root cause, potentially shifting the focus to the ''contraction_reading'' as more foundational.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_vs_structural_shift_primacy, conceptual, 'Determining the causal primacy between structural institutional change and cultural shifts in explaining dueling''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(duel_tr_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1750, 0.05).
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(duel_tr_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1850, 0.05).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1900, 0.05).

% Extraction over time
narrative_ontology:measurement(duel_be_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1700, 0.1).
narrative_ontology:measurement(duel_be_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1750, 0.12).
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1850, 0.14).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1900, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1700, 0.1).
narrative_ontology:measurement(duel_su_t1750, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1750, 0.15).
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1800, 0.2).
narrative_ontology:measurement(duel_su_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1850, 0.23).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1900, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
