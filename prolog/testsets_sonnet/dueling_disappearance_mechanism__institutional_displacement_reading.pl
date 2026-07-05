% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Institutional Displacement of Dueling as Dispute-Resolution Protocol
 *   domain: historical_sociology/legal_history
 *
 * SUMMARY:
 *   This story instantiates ONE reading within the
 *   dueling_disappearance_mechanism kernel: dueling declined because courts,
 *   banking systems, and libel law came to offer a cheaper, lower-risk,
 *   functionally equivalent way to resolve the same disputes (honor injury,
 *   debt default, reputational damage) that dueling previously adjudicated.
 *   On this reading dueling remains a coordination mechanism (a rope)
 *   throughout — it never becomes extractive or coercive — it simply loses
 *   ground competitively as substitute institutions mature. It survives at
 *   the margins (frontier regions, institutional gaps, military subcultures)
 *   precisely where the substitutes have not yet reached. This is distinct
 *   from the sibling contraction_reading, which holds that dueling became
 *   culturally unthinkable via a shift in the underlying honor-to-dignity
 *   value system independent of institutional availability, and from the
 *   overdetermined_composite_reading, which holds that no single sufficient
 *   condition explains the decline. This reading's ε is low and stable
 *   because dueling-as-coordination was never substantially extractive in
 *   this account — the interesting dynamic is competitive displacement, not
 *   increasing extraction.
 *
 * KEY AGENTS:
 *   - gentleman_disputants: primary users of the substituted coordination mechanism (moderate/mobile)
 *   - court_systems: primary institutional substitute, agenda-setting on dispute jurisdiction (institutional/analytical)
 *   - banking_institutions: primary institutional substitute for debt-honor disputes (institutional/analytical)
 *   - libel_law_claimants: beneficiaries of the reputational-vindication substitute (moderate/mobile)
 *   - professional_duelists_and_seconds: bear the cost of an obsoleted social role, not extraction victims (moderate/constrained)
 *   - rural_and_frontier_communities: excluded from the institutional substitution timeline (powerless/trapped)
 *   - historians_of_institutional_change: analytical observers reconstructing the causal account (analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.18).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.12).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Institutional Displacement of Dueling as Dispute-Resolution Protocol").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "historical_sociology/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, '90f519c7-1242-429b-ad7e-ed4dc46e4be8').
narrative_ontology:cs_kernel_codification('90f519c7-1242-429b-ad7e-ed4dc46e4be8', distributed).
narrative_ontology:cs_authority_grounding('90f519c7-1242-429b-ad7e-ed4dc46e4be8', distributed).
narrative_ontology:cs_reading_relation('90f519c7-1242-429b-ad7e-ed4dc46e4be8', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('90f519c7-1242-429b-ad7e-ed4dc46e4be8', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('90f519c7-1242-429b-ad7e-ed4dc46e4be8', foundational, functional_substitution_drives_institutional_change).
narrative_ontology:cs_axiom_status(functional_substitution_drives_institutional_change, holdable).
narrative_ontology:cs_axiom_grounding('90f519c7-1242-429b-ad7e-ed4dc46e4be8', functional_substitution_drives_institutional_change, empirically_contingent).
narrative_ontology:cs_axiom('90f519c7-1242-429b-ad7e-ed4dc46e4be8', secondary, dueling_remains_available_option_absent_superior_substitute).
narrative_ontology:cs_axiom_status(dueling_remains_available_option_absent_superior_substitute, holdable).
narrative_ontology:cs_axiom_grounding('90f519c7-1242-429b-ad7e-ed4dc46e4be8', dueling_remains_available_option_absent_superior_substitute, empirically_contingent).
narrative_ontology:cs_reference_frame('90f519c7-1242-429b-ad7e-ed4dc46e4be8', honor_code_as_functioning_dispute_protocol).
narrative_ontology:cs_drift_state('90f519c7-1242-429b-ad7e-ed4dc46e4be8', post_institutional_maturation_1900, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('90f519c7-1242-429b-ad7e-ed4dc46e4be8', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, gentleman_disputants).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, commercial_creditors).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, court_systems).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, banking_institutions).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, libel_law_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__institutional_displacement_reading, professional_duelists_and_seconds).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__institutional_displacement_reading, institutional_substitution_dominance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals of honor-bound social rank who previously had to duel to resolve insult or debt disputes now have access to courts, credit-reporting mechanisms, and libel actions that resolve the same underlying disputes (reputational damage, unpaid debts, slander) at lower personal risk. They retain the option to duel where institutions do not reach, but increasingly find it unnecessary.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, gentleman_disputants, beneficiary,
    moderate, biographical, mobile, national).

% Civil and criminal courts expand jurisdiction over slander, assault, and breach-of-honor claims, offering an adjudicated, state-backed alternative to private combat. They administer the substitute mechanism and gradually absorb the dispute types dueling once handled.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, court_systems, agenda_setter,
    institutional, generational, analytical, national).

% Credit-reporting and commercial banking systems provide a way to resolve disputes over debt and financial reputation without recourse to violence — a defaulting debtor's creditworthiness is now settled through institutional record rather than a challenge to combat. Banks benefit from being the recognized settlement mechanism for the class of disputes dueling used to arbitrate.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, banking_institutions, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__institutional_displacement_reading, banking_institutions, beneficiary).

% Individuals whose honor or reputation is impugned can sue for libel or slander and recover damages or a public retraction through the courts, achieving the same restorative function dueling once served (vindication of honor) without personal physical risk.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, libel_law_claimants, beneficiary,
    moderate, biographical, mobile, national).

% Those whose social standing or livelihood depended on the dueling code (fencing masters, professional seconds, honor-code arbiters) see their function displaced as institutional alternatives absorb the demand for dispute resolution. They are not victims of extraction but bear the cost of an obsoleted role — their exit is constrained by narrow, declining demand for dueling expertise.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, professional_duelists_and_seconds, payer,
    moderate, biographical, constrained, regional).

% Communities without ready access to functioning courts, credit institutions, or a robust press retain dueling as the only viable mechanism for dispute resolution well after urban centers institutionalize alternatives. Their voice on the pace of substitution is largely absent from national-level accounts of dueling's decline, which tend to be written from the vantage of institutionally-served regions.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, rural_and_frontier_communities, excluded,
    powerless, biographical, trapped, regional).

% Study court records, bank correspondence, and libel case dockets to trace the substitution of dueling by institutional mechanisms, distinguishing this causal account from cultural-shift and overdetermination accounts of the same historical decline.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, historians_of_institutional_change, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a socially legible protocol for resolving disputes over honor, debt, and reputational injury — a coordination problem every society with concentrated status-competition must solve somehow, whether by combat, litigation, or institutional record-keeping.
% TRANSFER_FUNCTION: Moves the locus of dispute resolution from private, symmetric-risk combat between disputants to institutional adjudication (courts award damages/injunctions; banks record and price creditworthiness; libel judgments transfer reputational vindication) — the transfer is of resolution authority itself, from the dueling code to the substituting institutions, not a redistribution of resources between winners and losers of duels.
% ABSENT_VOICES: Rural and frontier populations lacking court access, banking infrastructure, or a functioning press are underrepresented in the historical record of 'dueling's decline,' which is disproportionately documented from the perspective of institutionally-dense regions where substitution occurred earliest and most visibly.
% DISAPPEARANCE_RATIONALE: If courts, credit institutions, and libel law vanished overnight, this reading predicts dueling (or something functionally like it) would re-emerge to fill the dispute-resolution gap in the affected domains — the coordination function does not disappear, only its institutional carrier. Whether the world 'rearranges' or 'stays the same' is genuinely contested between this reading (world_rearranges — a real function reverts) and the sibling contraction reading (world_unchanged — dueling is culturally unthinkable regardless of institutional gaps, so no reversion would occur).
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 0.18, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction stays low and rises only marginally (0.10 to 0.18) across the interval because this reading treats dueling and its substitutes as competing coordination mechanisms, not as an extraction relationship — the modest rise reflects the accumulating institutional overhead (court fees, banking recordkeeping costs) that a mature dispute-resolution system carries relative to a nearly-free duel. Theater ratio climbs modestly (0.05 to 0.20) as some vestigial dueling codes (formalized challenges, published 'codes of honor') persist as performance in military and Southern gentry circles even as the practical function has migrated to courts and banks. Suppression is low throughout: no one is coerced into using courts or banks instead of dueling — the substitution is presented, on this reading, as a voluntary competitive uptake, consistent with the 'no victim set' structural delta specified for this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the vantage of institutionally-served gentleman disputants and libel claimants, the substitution looks like frictionless competitive improvement. From the vantage of professional duelists/seconds it looks like a career-ending market shift with no institutional replacement for their specific role. From the vantage of frontier communities, nothing has changed at all — the 'decline of dueling' is a story about places they do not live.
 *
 * DIRECTIONALITY LOGIC:
 *   Gentleman disputants, libel claimants, and the institutions themselves sit near the beneficiary end: courts and banks gain jurisdiction and legitimacy: disputants gain a lower-risk resolution channel. Professional duelists and seconds bear a real cost (obsolescence of their social function) but are not targets of extraction in the classic sense — no one benefits FROM their displacement, it is a side effect of superior substitute availability, which is why no victims array is authored for this reading. Rural and frontier communities are excluded rather than harmed: the institutional substitute simply has not reached them yet.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in the other direction from a snare/tangled-rope story: because the coordination function (resolving honor/debt/reputation disputes) remains genuinely live throughout the interval, and courts/banks/libel-law demonstrably absorb it, there is no basis for reading dueling's decline as the collapse of a pure extraction racket, nor is the surviving vestige (dueling codes, ceremonial challenge rituals) sufficient to call the whole arrangement a piton — the underlying coordination need was met by better tools, and the story does not require an enforcement narrative to explain persistence of the substitute institutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    displacement_vs_contraction_locus,
    'Is the primary causal driver of dueling''s decline the AVAILABILITY of substitute institutions (this reading) or a prior SHIFT in the underlying honor-culture value system that made dueling unthinkable independent of substitute availability (contraction_reading)?',
    'Compare regions/periods where institutional substitutes (courts, banks, libel law) were available but dueling persisted anyway, versus regions where substitutes were unavailable but dueling declined regardless. If institutional availability predicts decline better than value-system indicators (sermon literature, private correspondence on honor), this reading is favored.',
    'If contraction dominates, this reading''s coordination-function framing is largely epiphenomenal — institutions would have displaced dueling regardless of their comparative efficiency, because the demand for the dueling-mechanism itself had already evaporated culturally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_vs_contraction_locus, empirical, 'Locates the disagreement between the institutional_displacement_reading and contraction_reading in a testable regional/temporal comparison.').

omega_variable(
    single_sufficient_cause_vs_overdetermination,
    'Does institutional substitution alone constitute a SUFFICIENT explanation for dueling''s decline, or is it one of several independently sufficient conditions (per overdetermined_composite_reading) such that removing it would not have changed the outcome?',
    'Counterfactual/comparative case analysis: identify jurisdictions where institutional substitutes matured LATE relative to the decline of dueling, and test whether decline still tracked institutional maturation timing or proceeded independently of it (e.g., driven by Civil War-era casualty aversion or legal prohibition alone).',
    'If decline timing tracks institutional maturation closely across multiple independent jurisdictions, this reading''s causal claim is strengthened as (at least) a necessary component; if decline proceeds on a similar timeline regardless of institutional maturation, the overdetermined_composite_reading''s claim that no single factor is doing the causal work is strengthened instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(single_sufficient_cause_vs_overdetermination, empirical, 'Tests whether institutional displacement is causally load-bearing or merely one strand within an overdetermined outcome.').

omega_variable(
    coordination_function_naturalness_of_substitutes,
    'Is the beneficiary structure declared here (court systems, banks, libel claimants) evidence that the ''coordination'' framing is itself a constructed narrative serving the interests of the institutions that displaced dueling, rather than a neutral description of superior dispute-resolution technology?',
    'Examine whether courts, banks, and libel-law regimes actively lobbied against dueling (self-interested displacement) versus passively absorbed demand that shifted to them for independent reasons.',
    'If active lobbying/self-interested promotion is found, part of what this reading treats as neutral competitive displacement would be better modeled as institutions manufacturing legitimacy for their own jurisdictional expansion — though this would not by itself convert the constraint to snare/tangled_rope absent an identifiable victim class, which this reading does not authorize.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_naturalness_of_substitutes, conceptual, 'Probes whether the declared beneficiary structure understates institutional self-interest in the displacement narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 1780, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1780, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1780, 0.05).
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1800, 0.07).
narrative_ontology:measurement(duel_tr_t1820, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1820, 0.1).
narrative_ontology:measurement(duel_tr_t1840, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1840, 0.14).
narrative_ontology:measurement(duel_tr_t1860, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1860, 0.17).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1900, 0.2).

% Extraction over time
narrative_ontology:measurement(duel_be_t1780, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1780, 0.1).
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1800, 0.12).
narrative_ontology:measurement(duel_be_t1820, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1820, 0.14).
narrative_ontology:measurement(duel_be_t1840, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1840, 0.15).
narrative_ontology:measurement(duel_be_t1860, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1860, 0.17).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1900, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dueling_disappearance_mechanism__institutional_displacement_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__institutional_displacement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__institutional_displacement_reading, 0.12).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the dueling_disappearance_mechanism kernel. institutional_displacement_reading treats the decline as competitive substitution by superior dispute-resolution institutions (courts, banking, libel law), preserving dueling's classification as rope throughout and authoring no victim set. contraction_reading treats the decline as a value-system shift (honor culture to dignity culture) that made dueling unthinkable independent of institutional availability. overdetermined_composite_reading treats the decline as jointly caused by multiple independently sufficient conditions (legal prohibition, institutional modernization, cultural shift, Civil War trauma) such that no single reading captures the full causal structure. Each reading is authored as its own constraint with its own ε and stakeholder structure; they are linked here rather than merged, per the ε-invariance decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
