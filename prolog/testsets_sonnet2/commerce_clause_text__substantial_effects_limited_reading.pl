% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Substantial Effects Doctrine with Economic/Non-Economic Category Limit
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This story instantiates the substantial-effects-limited reading of the
 *   Commerce Clause kernel: federal power reaches intrastate activity that
 *   substantially affects interstate commerce, but only where the regulated
 *   activity is genuinely economic (not merely aggregated to manufacture a
 *   jurisdictional hook) and where the regulation is not a pretext for
 *   exercising a general federal police power over matters of traditionally
 *   local concern. This is a middle reading — narrower than the expansive
 *   aggregation-of-any-activity view, broader than the originalist
 *   border-crossing-only view. The category-boundary line (economic vs.
 *   non-economic) is the constraint's actual operating mechanism and its
 *   chief vulnerability: it must be actively policed case by case, which is
 *   why this reading — unlike a clean coordination rope — carries an
 *   enforcement requirement and an identifiable payer class (actors whose
 *   conduct sits at the boundary and is characterized against their
 *   interest).
 *
 * KEY AGENTS:
 *   - federal_regulatory_agencies: primary agenda_setter/beneficiary (institutional/arbitrage) — administers and benefits from the doctrine's reach
 *   - local_noneconomic_regulatees: primary payer (powerless/trapped) — bears reclassification risk without having engaged interstate commerce
 *   - states_seeking_police_power_autonomy: institutional payer (institutional/constrained) — loses jurisdictional autonomy at the margin
 *   - federal_judiciary: analytical observer — administers the economic/non-economic line that is the constraint's actual mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.42).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.38).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Substantial Effects Doctrine with Economic/Non-Economic Category Limit").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, 'b5841271-e3ce-46fb-8d91-97b059d329ac').
narrative_ontology:cs_kernel_codification('b5841271-e3ce-46fb-8d91-97b059d329ac', fixed_text).
narrative_ontology:cs_authority_grounding('b5841271-e3ce-46fb-8d91-97b059d329ac', lineage).
narrative_ontology:cs_interpretation_layer_present('b5841271-e3ce-46fb-8d91-97b059d329ac').
narrative_ontology:cs_reading_relation('b5841271-e3ce-46fb-8d91-97b059d329ac', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5841271-e3ce-46fb-8d91-97b059d329ac', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_axiom('b5841271-e3ce-46fb-8d91-97b059d329ac', foundational, economic_activity_categorical_gate_required).
narrative_ontology:cs_axiom_status(economic_activity_categorical_gate_required, holdable).
narrative_ontology:cs_axiom_grounding('b5841271-e3ce-46fb-8d91-97b059d329ac', economic_activity_categorical_gate_required, conventional).
narrative_ontology:cs_axiom('b5841271-e3ce-46fb-8d91-97b059d329ac', secondary, pretext_review_meaningfully_constrains_federal_reach).
narrative_ontology:cs_axiom_status(pretext_review_meaningfully_constrains_federal_reach, holdable).
narrative_ontology:cs_axiom_grounding('b5841271-e3ce-46fb-8d91-97b059d329ac', pretext_review_meaningfully_constrains_federal_reach, instrumental).
narrative_ontology:cs_reference_frame('b5841271-e3ce-46fb-8d91-97b059d329ac', post_new_deal_substantial_effects_framework).
narrative_ontology:cs_drift_state('b5841271-e3ce-46fb-8d91-97b059d329ac', contemporary_federalism_revival_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b5841271-e3ce-46fb-8d91-97b059d329ac', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, national_market_participants).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, interstate_commercial_actors).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, local_noneconomic_regulatees).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, states_seeking_police_power_autonomy).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, activity_reclassified_as_economic).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, dual_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, enumerated_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce regulations reaching intrastate conduct by characterizing it as part of an interstate economic class of activity (e.g. aggregated production, commercial transactions). They benefit directly from courts sustaining the substantial-effects nexus and from favorable line-drawing on what counts as 'economic.' They also administer the doctrine's own limiting principle when challenged, giving them a hand in shaping the boundary that constrains them.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies, agenda_setter).

% Large interstate firms benefit from uniform federal rules displacing a patchwork of state regulation, since local production and sale that would otherwise escape federal reach get pulled into a single national regulatory and market frame that favors scale.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, national_market_participants, beneficiary,
    organized, generational, mobile, national).

% Businesses operating across state lines gain predictable federal preemption of inconsistent local economic regulation once an activity is classified as substantially affecting commerce; the doctrine's coordination function serves them directly.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, interstate_commercial_actors, beneficiary,
    powerful, biographical, mobile, continental).

% Individuals and small local actors engaged in conduct that is not itself economic (e.g. possession, non-commercial local behavior) but gets swept toward federal reach when a regulatory scheme aggregates their conduct with a broader economic class. They bear prosecution or regulation without having entered any interstate transaction themselves, and their only defense is winning the categorical fight over whether their conduct is 'economic.'
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, local_noneconomic_regulatees, payer,
    powerless, biographical, trapped, local).

% State governments attempting to regulate matters of traditionally local concern (family law, education, non-commercial local conduct, general criminal law) find their exclusive domain narrowed whenever federal actors successfully characterize an adjacent activity as economic enough to aggregate into interstate effect. Their remedy is litigation over the pretext and economic-activity requirements, which they do not control.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, states_seeking_police_power_autonomy, payer,
    institutional, generational, constrained, national).

% Conduct or actors sitting at the boundary — home production, subsistence use, small-scale non-market activity — that courts or agencies redescribe as 'economic' in order to bring it within the aggregation principle. Whether they are inside or outside federal reach turns entirely on a characterization exercise they do not control and cannot predict in advance.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, activity_reclassified_as_economic, payer,
    moderate, biographical, constrained, national).

% Adjudicates whether a given exercise of federal power is genuinely economic regulation with a jurisdictional nexus or is pretextual police-power regulation dressed as commerce regulation. This is the doctrine's actual operating mechanism: the court draws and redraws the economic/non-economic line case by case, which is both the constraint's coordination function and the site of its contestation.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a single national regulatory framework to reach intrastate conduct that, in the aggregate, meaningfully affects interstate markets, avoiding a race-to-the-bottom or free-rider problem that would arise if every actor could exempt itself from federal economic regulation by keeping its individual conduct technically intrastate.
% TRANSFER_FUNCTION: Moves regulatory authority from state governments and from actors whose conduct is locally situated but economically classifiable, to federal agencies and courts; correspondingly moves the benefit of uniform national economic regulation to interstate commercial actors and away from local decision-makers and non-economic local actors caught in the aggregation net.
% ABSENT_VOICES: Individuals and communities whose conduct sits at the disputed economic/non-economic boundary rarely have a seat in defining the category itself — the boundary is set by federal agencies proposing regulation and federal courts adjudicating challenges, with the affected local actors present only as litigants defending after the fact, never as co-authors of where the line falls.
% DISAPPEARANCE_RATIONALE: If the substantial-effects/economic-activity limited reading disappeared and were replaced by either unlimited aggregation or a hard border-crossing requirement, entire regulatory regimes (environmental, labor, certain criminal statutes reaching local conduct) would either lose their constitutional footing or federal reach would expand dramatically into traditionally local domains — state and federal regulatory jurisdiction would visibly reallocate.
% FOUNDING_PROBLEM: The problem of enabling a national economy to be regulated coherently despite economic activity being formally conducted 'intrastate,' while preventing Congress from using the commerce power as a general police power to reach any conduct it disfavors.
% FOUNDING_PROBLEM_CORROBORATION: Federal agencies and interstate commercial beneficiaries attest the founding problem (need for coherent national economic regulation) remains fully live. State governments and civil liberties scholars outside the regulatory beneficiary class attest that the 'non-pretextual economic activity' limiting principle has proven manipulable in practice — the categorical line has moved opportunistically with the regulatory objective, per academic commentary and dissenting judicial opinions documenting inconsistent aggregation reasoning across cases.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).
:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) because the reading's own limiting principle constrains how far federal reach can extend — this is not an unlimited aggregation doctrine. Suppression is moderate (0.38): the doctrine's persistence depends on continued judicial willingness to sustain the categorical line, and on states/local actors not successfully overturning the substantial-effects framework itself. Theater ratio is moderate (0.31) and rising: as the doctrine ages, an increasing share of judicial and litigant effort goes into characterization battles (is this activity 'economic'?) rather than substantive commerce analysis, which is a structural feature of a limiting principle that depends on a fuzzy category boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulatory agencies and interstate commercial actors sit at the beneficiary end: they gain uniform national reach and predictable preemption once conduct is classified as economic. Local non-economic regulatees and boundary-adjacent actors sit at the target end: their exposure depends entirely on a characterization exercise they do not control, and their exit options are trapped or constrained because the classification, once made, is binding. States occupy an institutional-payer position: they lose autonomy incrementally each time the boundary shifts toward 'economic,' but retain some capacity to litigate the line.
 *
 * MANDATROPHY ANALYSIS:
 *   The limiting principle (economic-activity requirement, non-pretext requirement) exists specifically to prevent the mandate — genuine national economic coordination — from mutating into unbounded federal police power. Whether that limiting principle still does real work, or has become a rhetorical formality that yields to whatever the reviewing court wants to reach, is exactly the founding_problem contest recorded above: agencies say the problem (economic coherence) is live and the limit functions; outside observers document doctrinal drift in how 'economic' gets defined case to case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_noneconomic_line_stability,
    'Is the economic-activity/non-economic-activity distinction a stable, principled category boundary, or does it functionally track the desired regulatory outcome in each case?',
    'Longitudinal doctrinal analysis of how courts classify borderline activities (e.g. possession, home production, non-market conduct) across cases, checking for correlation between the classification outcome and the regulatory result the court appears to favor, independent of stated reasoning.',
    'If the line tracks outcome rather than principle, the ''limiting'' reading functions as a discretionary override rather than a genuine constraint, which would push this reading''s computed type toward snare (unprincipled extraction dressed as principled limitation) rather than tangled_rope (genuine coordination plus bounded extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_noneconomic_line_stability, empirical, 'Whether the economic/non-economic boundary is principled or outcome-driven.').

omega_variable(
    pretext_detection_reliability,
    'Can courts reliably distinguish non-pretextual economic regulation from police-power regulation dressed in economic language, or is the pretext inquiry itself manipulable by careful statutory drafting?',
    'Compare regulatory statutes struck down for pretext against similar statutes upheld, examining whether drafting technique (e.g. adding jurisdictional findings, economic-effects recitals) rather than substantive difference explains divergent outcomes.',
    'If pretext review is defeated by drafting technique alone, the non-pretext requirement is largely theatrical, which would raise the effective theater_ratio and support reclassification toward a more purely extractive reading of federal reach under this doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pretext_detection_reliability, empirical, 'Whether the non-pretext safeguard functions substantively or is defeated by drafting.').

omega_variable(
    reading_selection_as_political_outcome,
    'Is the choice among the three sibling readings (expansive, narrow originalist, substantial-effects-limited) itself best understood as a stable jurisprudential commitment, or as a contingent outcome of judicial composition that shifts with appointments?',
    'Track which reading a given Court majority applies across successive commerce clause cases and correlate with changes in Court composition versus stated doctrinal reasoning.',
    'If reading selection tracks composition rather than principle, all three readings (including this one) are better understood as contested political settlements rather than as stable constitutional interpretations, which affects how much weight the ''non-pretextual'' requirement can bear as an actual constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_as_political_outcome, conceptual, 'Whether the kernel''s reading is a stable doctrine or a composition-contingent political outcome.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comm_tr_t8, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(comm_tr_t16, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(comm_tr_t24, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(comm_tr_t32, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(comm_tr_t40, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(comm_be_t8, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(comm_be_t16, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(comm_be_t24, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(comm_be_t32, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(comm_be_t40, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comm_su_t8, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(comm_su_t16, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(comm_su_t24, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 24, 0.35).
narrative_ontology:measurement(comm_su_t32, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 32, 0.37).
narrative_ontology:measurement(comm_su_t40, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__substantial_effects_limited_reading, 0.12).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__originalist_narrow_reading).

% DUAL FORMULATION NOTE:
% This story is the middle reading in a three-member constraint family sharing the commerce_clause_text kernel. expansive_federal_reading would show substantially higher extractiveness and a thinner limiting structure (no economic/non-economic gate); originalist_narrow_reading would show near-mountain-level accessibility_collapse against federal reach into intrastate activity and a much smaller beneficiary set for federal agencies. Each sibling carries its own ε; do not average across them — see the ε-invariance principle and the kernel_context note above.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
