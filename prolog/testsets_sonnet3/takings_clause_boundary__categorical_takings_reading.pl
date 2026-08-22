% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__categorical_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Categorical-Plus-Balancing Takings Doctrine (Loretto/Lucas Per Se Rules + Penn Central Factors)
 *   domain: constitutional/property_regulatory
 *
 * SUMMARY:
 *   This story instantiates the categorical-takings reading of the Takings
 *   Clause boundary kernel: the doctrinal architecture beginning with Penn
 *   Central Transportation Co. v. City of New York (1978) and refined by
 *   Loretto v. Teleprompter Manhattan CATV Corp. (1982) and Lucas v. South
 *   Carolina Coastal Council (1992), under which permanent physical
 *   occupations and total economic wipeouts are per se takings requiring
 *   compensation, while every other regulatory burden on property is
 *   evaluated under the ad hoc, multi-factor Penn Central balancing test
 *   (economic impact, interference with investment-backed expectations,
 *   character of the government action). This is a distinct constraint from
 *   the physical_appropriation_reading (which would recognize only direct
 *   physical seizure as compensable) and from the regulatory_takings_reading
 *   (which would extend compensation to any regulation that 'goes too far' in
 *   diminishing value, without the sharp categorical/balancing split). The
 *   three readings share the same constitutional text but instantiate
 *   structurally different extraction and predictability profiles, and are
 *   linked here only through network.affects_constraints and
 *   cs_structure.reading_relations, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - regulatory_agencies: agenda_setter (institutional/analytical) — draft and enforce regulation under the doctrine's shelter
 *   - municipal_land_use_planners: beneficiary (institutional/analytical) — use the wide middle band for aggressive land use control
 *   - mid_diminution_property_owners: payer (moderate/constrained) — bear substantial uncompensated loss in the unpredictable balancing zone
 *   - small_developers_facing_discretionary_review: payer (moderate/constrained) — cannot afford to litigate Penn Central to resolution
 *   - reviewing_courts: observer (institutional/analytical) — sort claims into categorical or balancing tracks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.42).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.38).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Categorical-Plus-Balancing Takings Doctrine (Loretto/Lucas Per Se Rules + Penn Central Factors)").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional/property_regulatory").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, '19c0a641-75f1-4e10-ab49-79e00e0134ca').
narrative_ontology:cs_kernel_codification('19c0a641-75f1-4e10-ab49-79e00e0134ca', fixed_text).
narrative_ontology:cs_authority_grounding('19c0a641-75f1-4e10-ab49-79e00e0134ca', lineage).
narrative_ontology:cs_interpretation_layer_present('19c0a641-75f1-4e10-ab49-79e00e0134ca').
narrative_ontology:cs_reading_relation('19c0a641-75f1-4e10-ab49-79e00e0134ca', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('19c0a641-75f1-4e10-ab49-79e00e0134ca', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('19c0a641-75f1-4e10-ab49-79e00e0134ca', foundational, administrable_categorical_lines_required_at_extremes).
narrative_ontology:cs_axiom_status(administrable_categorical_lines_required_at_extremes, holdable).
narrative_ontology:cs_axiom_grounding('19c0a641-75f1-4e10-ab49-79e00e0134ca', administrable_categorical_lines_required_at_extremes, instrumental).
narrative_ontology:cs_axiom('19c0a641-75f1-4e10-ab49-79e00e0134ca', foundational, residual_regulatory_burden_governed_by_multifactor_balancing_not_bright_line).
narrative_ontology:cs_axiom_status(residual_regulatory_burden_governed_by_multifactor_balancing_not_bright_line, holdable).
narrative_ontology:cs_axiom_grounding('19c0a641-75f1-4e10-ab49-79e00e0134ca', residual_regulatory_burden_governed_by_multifactor_balancing_not_bright_line, conventional).
narrative_ontology:cs_reference_frame('19c0a641-75f1-4e10-ab49-79e00e0134ca', penn_central_tripartite_synthesis).
narrative_ontology:cs_drift_state('19c0a641-75f1-4e10-ab49-79e00e0134ca', post_lucas_post_lingle_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('19c0a641-75f1-4e10-ab49-79e00e0134ca', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, regulatory_agencies).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, municipal_land_use_planners).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, settled_property_owners_near_poles).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, mid_diminution_property_owners).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, small_developers_facing_discretionary_review).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, large_institutional_developers).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, large_institutional_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce land use, environmental, and zoning regulations under the doctrine's shelter: as long as a rule stops short of permanent physical occupation or total value wipeout, it is very unlikely to be a per se taking, so agencies can regulate aggressively in the broad middle zone and rely on Penn Central's multi-factor balancing (which they usually win) to survive challenge.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Use the doctrine's wide middle band to impose density limits, historic preservation restrictions, and environmental overlays without triggering automatic compensation obligations, since almost no ordinary regulation reaches Lucas-level total wipeout or Loretto-level permanent occupation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, municipal_land_use_planners, beneficiary,
    institutional, generational, analytical, regional).

% Owners whose property is either physically occupied outright or reduced to zero economically viable use get a clean, litigation-light path to compensation because the categorical rules apply automatically, without weighing investment-backed expectations or public interest against them.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, settled_property_owners_near_poles, beneficiary,
    moderate, biographical, constrained, local).

% Owners who suffer substantial but not total value loss (60-90% diminution, say) fall into the Penn Central zone, where the multi-factor test is notoriously unpredictable and courts weigh 'reasonable investment-backed expectations' and 'character of the government action' against them in ways that are difficult to forecast before litigating for years. They absorb real loss with no reliable compensation path and bear the litigation cost of finding out.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, mid_diminution_property_owners, payer,
    moderate, biographical, constrained, local).

% Lack the legal budget of large developers to mount a multi-year Penn Central challenge, so they either abandon contested projects, redesign to fit whatever a planning board will approve, or settle for far less than the diminution they suffered. The categorical rules do not protect them because their harm rarely reaches total wipeout.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, small_developers_facing_discretionary_review, payer,
    moderate, biographical, constrained, local).

% Can afford protracted Penn Central litigation and often win favorable settlements or variances that smaller owners cannot obtain; they also benefit from the doctrine's predictability at the poles when acquiring land near environmentally sensitive or heavily regulated zones, pricing risk into acquisition costs that smaller owners cannot spread across a portfolio.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, large_institutional_developers, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__categorical_takings_reading, large_institutional_developers, payer).

% Support the wide middle zone that permits aggressive regulation, but are not parties to individual takings litigation and have no direct voice in how Penn Central balancing resolves in specific disputes; their interest is served by the doctrine's structure without their participation in its application.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, environmental_and_preservation_advocates, excluded,
    organized, generational, analytical, national).

% Apply the tripartite structure case by case, sorting claims into the per se categories or the Penn Central balancing track, and in doing so determine which owners get automatic compensation and which absorb loss through unpredictable multi-factor analysis.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, reviewing_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__categorical_takings_reading, diffuse).
narrative_ontology:fixing_cost_class(takings_clause_boundary__categorical_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable division of labor between bright-line certainty (for the extreme cases of physical occupation and total wipeout) and flexible case-by-case balancing (for everything else), allowing courts to avoid either compensating every regulation that reduces value or permitting government to seize property outright without paying for it.
% TRANSFER_FUNCTION: Moves compensation certainty toward owners at the extremes (physical occupation, total wipeout) and moves regulatory discretion and litigation risk toward owners in the middle band, who bear diminution without a predictable right to be paid for it, effectively subsidizing regulators' freedom to act with owners' uncompensated losses.
% ABSENT_VOICES: Mid-diminution owners and small developers who cannot afford to litigate the Penn Central factors to a resolution are functionally unheard even when nominally entitled to sue; their claims settle cheap or are abandoned, so the doctrine's operation in the middle band is shaped disproportionately by the well-resourced litigants who can afford to test it.
% DISAPPEARANCE_RATIONALE: If the tripartite structure vanished, regulatory agencies would lose the doctrinal shelter that lets them impose value-diminishing rules with only diffuse balancing exposure; either compensation claims would flood in under a purer per-se or 'goes too far' regime, or agencies would lose predictability about what survives review. Land use planning, environmental regulation, and real estate transaction pricing would all reorganize around whatever replaced it.
% FOUNDING_PROBLEM: Courts needed a way to reconcile Pennsylvania Coal's 'too far' principle with the practical reality that almost every regulation reduces some property value; a pure ad hoc standard offered no guidance, while a pure physical-invasion rule left government free to regulate value to zero without compensation.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court majorities in Loretto and Lucas attest the categorical rules solve a genuine predictability problem at the poles. Property rights scholars and mid-diminution litigants attest, from outside the regulatory-agency beneficiary set, that the doctrine mainly functions to insulate the vast majority of value-destroying regulation from any compensation obligation by keeping it inside the unpredictable Penn Central zone, where litigation cost itself operates as a filter favoring well-resourced parties.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__categorical_takings_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__categorical_takings_reading_tests).
:- end_tests(takings_clause_boundary__categorical_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high: the doctrine genuinely delivers on its categorical promise for the poles (physical occupation, total wipeout), and Penn Central balancing, while unpredictable, is not designed as pure extraction — it does sometimes find takings. The extraction is concentrated in the structural asymmetry of the middle band, where the burden of uncertainty and litigation cost falls predictably on less-resourced owners even though the doctrine is formally neutral. Suppression is moderate (0.38): there is no bar to bringing a takings claim, but the practical cost of proving a Penn Central claim functions as a soft suppression mechanism against smaller owners. Theater ratio (0.30) reflects that a meaningful share of the doctrine's activity is genuinely functional (courts do apply and sometimes grant relief under the factors) but a growing share over time has become predictable pattern-matching where outcomes track resources more than the stated factors.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory agencies and municipal planners sit near the beneficiary end: the tripartite structure gives them wide berth to regulate without triggering the per se rules, and Penn Central's multi-factor balancing test is famously government-favorable in application. Owners at the poles (total wipeout, physical occupation) are structurally protected and thus closer to symmetric or even beneficiary-leaning on this specific axis, since the categorical rule works in their favor. Mid-diminution owners and small developers are the clear targets: they bear real value loss with no reliable compensation path, and their exit options (litigate for years, redesign the project, or sell at a discount) are all costly. Large institutional developers occupy a dual position — sometimes payers in Penn Central litigation, but with resources to make the litigation viable and sometimes to negotiate favorable outcomes unavailable to smaller owners.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling Pennsylvania Coal's 'too far' principle with the practical need for regulatory flexibility — remains partially live: land use regulation still needs some accommodation with property rights. But the specific tripartite solution has, over nearly five decades, hardened into a structure whose predictable failure mode (the vast unpredictable middle band) primarily serves regulatory certainty for agencies rather than owner protection. This is not a case of the mandate having fully died; it is a case of a genuine coordination function (avoiding both over-compensation and under-compensation extremes) persisting alongside an asymmetric cost distribution that the doctrine's proponents rarely name directly — hence the tangled_rope claim rather than snare or pure rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    penn_central_predictability_ambiguity,
    'Is the Penn Central multi-factor test a genuine, if imprecise, balancing mechanism that tracks real differences in regulatory burden, or is it a discretion-preserving proxy whose outcomes are predictable mainly from litigant resources and judicial priors rather than from the stated factors?',
    'Empirical case-outcome analysis across decades of Penn Central litigation, coding outcomes against litigant resource level, government defendant type, and stated factor findings, to test whether resources predict outcomes independent of the doctrinal factors.',
    'If outcomes track resources more than factors, the tangled_rope classification is strengthened (structural extraction hidden inside a formally neutral balancing test); if outcomes track the stated factors reliably regardless of resources, the doctrine is closer to a genuine rope with incidental cost asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penn_central_predictability_ambiguity, empirical, 'Whether Penn Central balancing tracks doctrine or resources.').

omega_variable(
    kernel_framing_alternative_boundary_line,
    'Is the categorical/balancing split the correct unit of analysis, or should the boundary instead be drawn around the underlying legitimacy claim — that SOME administrable line between compensable and non-compensable regulation is constitutionally required at all — with the categorical_takings_reading being just one of several administrable-line proposals?',
    'Compare this reading''s structural profile against a hypothetical fourth reading built around a pure ad hoc ''reasonableness'' standard with no categorical carve-outs, to see whether the categorical/balancing split itself, versus the administrability requirement, is doing the classificatory work.',
    'If the administrability requirement (rather than the specific categorical/balancing split) is the load-bearing commitment, this story''s cs_structure axioms should be recentered on administrability rather than on the specific bright-line thresholds chosen, which would not change ε but would change which axiom is ''foundational'' versus ''secondary''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_boundary_line, conceptual, 'Alternative framing of what the kernel''s contested commitment actually is.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement(taki_tr_t1992, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1992, 0.24).
narrative_ontology:measurement(taki_tr_t2001, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2001, 0.27).
narrative_ontology:measurement(taki_tr_t2010, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2010, 0.29).
narrative_ontology:measurement(taki_tr_t2017, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2017, 0.3).
narrative_ontology:measurement(taki_tr_t2024, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1978, 0.3).
narrative_ontology:measurement(taki_be_t1992, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(taki_be_t2001, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2001, 0.38).
narrative_ontology:measurement(taki_be_t2010, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(taki_be_t2017, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2017, 0.41).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1978, 0.3).
narrative_ontology:measurement(taki_su_t1992, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1992, 0.33).
narrative_ontology:measurement(taki_su_t2001, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2001, 0.35).
narrative_ontology:measurement(taki_su_t2010, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2010, 0.37).
narrative_ontology:measurement(taki_su_t2017, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2017, 0.38).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__categorical_takings_reading, 0.12).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, regulatory_takings_reading).

% DUAL FORMULATION NOTE:
% This story is the middle-synthesis reading of the takings_clause_boundary kernel. physical_appropriation_reading would narrow compensable takings to direct seizure/permanent occupation only (lower ε for regulators, higher for owners denied balancing relief); regulatory_takings_reading would broaden compensation to any substantial value-diminishing regulation (higher ε for regulators, lower for owners). This reading's ε (0.42) sits between what those siblings would author because it preserves regulatory flexibility in the broad middle band while offering categorical protection only at the extremes — the actual doctrinal compromise adopted by controlling U.S. Supreme Court precedent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(takings_clause_boundary__categorical_takings_reading, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
