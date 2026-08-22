% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__popular_constitutionalism_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism Interpretive Regime
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the popular constitutionalism reading of the
 *   U.S. Constitution interpretive kernel. It treats constitutional meaning
 *   as emerging from popular political movements and democratic contestation
 *   rather than from judicial pronouncement alone. The arrangement contests
 *   judicial supremacy by distributing interpretive authority across branches
 *   and social mobilization. The claim is tangled_rope because the regime
 *   genuinely coordinates democratic legitimacy and popular sovereignty, yet
 *   it asymmetrically extracts stable settlement and counter-majoritarian
 *   protection from judicial finality advocates and vulnerable minorities.
 *   The metrics and claim are authored independently: the structural claim is
 *   tangled_rope, while the metrics describe the actual operation of the
 *   interpretive regime.
 *
 * KEY AGENTS:
 *   - Popular movements (beneficiary/organized/mobile) â gain interpretive authority through democratic contestation
 *   - Legislative majorities (beneficiary/institutional/constrained) â acquire constitutional interpretive leverage against judicial override
 *   - Anti-elitist claimants (beneficiary/moderate/mobile) â displace elite legal monopoly on constitutional meaning
 *   - Judicial finality advocates (payer/institutional/identity_locked) â bear loss of exclusive interpretive authority and institutional prestige
 *   - Rule-of-law adherents (payer/organized/constrained) â lose stable constitutional settlement and predictability
 *   - Minorities seeking judicial protection (payer/powerless/constrained) â lose counter-majoritarian judicial shelter under majoritarian constitutionalism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.58).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.48).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "Popular Constitutionalism Interpretive Regime").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, '0e31031e-8788-4bd9-95f7-bde15cf3db0f').
narrative_ontology:cs_kernel_codification('0e31031e-8788-4bd9-95f7-bde15cf3db0f', formalized).
narrative_ontology:cs_authority_grounding('0e31031e-8788-4bd9-95f7-bde15cf3db0f', distributed).
narrative_ontology:cs_reading_relation('0e31031e-8788-4bd9-95f7-bde15cf3db0f', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e31031e-8788-4bd9-95f7-bde15cf3db0f', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('0e31031e-8788-4bd9-95f7-bde15cf3db0f', foundational, popular_sovereignty_interpretive_authority).
narrative_ontology:cs_axiom_status(popular_sovereignty_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('0e31031e-8788-4bd9-95f7-bde15cf3db0f', popular_sovereignty_interpretive_authority, conventional).
narrative_ontology:cs_axiom('0e31031e-8788-4bd9-95f7-bde15cf3db0f', secondary, judicial_finality_contestable).
narrative_ontology:cs_axiom_status(judicial_finality_contestable, holdable).
narrative_ontology:cs_axiom_grounding('0e31031e-8788-4bd9-95f7-bde15cf3db0f', judicial_finality_contestable, conventional).
narrative_ontology:cs_reference_frame('0e31031e-8788-4bd9-95f7-bde15cf3db0f', popular_sovereignty_framework).
narrative_ontology:cs_drift_state('0e31031e-8788-4bd9-95f7-bde15cf3db0f', contemporary_judicial_power_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0e31031e-8788-4bd9-95f7-bde15cf3db0f', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, rule_of_law_adherents).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, minorities_seeking_judicial_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in protest, electoral mobilization, and public discourse to shape constitutional meaning outside courts. They gain interpretive leverage when elected officials and cultural narratives adopt their constitutional claims. Exit is mobileâthey can redirect energy to non-constitutional arenas or form new movements.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements, beneficiary,
    organized, biographical, mobile, national).

% Assert constitutional interpretations through legislation, oversight, and political pressure on courts. They benefit from a theory that legitimates congressional and popular override of judicial interpretations. Exit is constrained by electoral cycles and partisan lock-in.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, generational, constrained, national).

% Invoke constitutional arguments against expert or judicial control of fundamental values. They benefit from the displacement of elite legal interpretive monopoly. Exit involves retreating to local or non-political community organization.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants, beneficiary,
    moderate, biographical, mobile, national).

% Legal elites, judges, and scholars who maintain that judicial interpretation provides the final, authoritative resolution of constitutional questions. They bear the cost of eroded institutional prestige and loss of exclusive interpretive role. Exit is identity-locked because professional identity is constituted through the judicial supremacy framework.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates, payer,
    institutional, civilizational, identity_locked, national).

% Advocates and institutions depending on stable, predictable constitutional settlement for planning and rights protection. They bear the cost of interpretive instability as constitutional meaning shifts with electoral winds. Exit is constrained because there is no alternative source of stable higher-law settlement in this regime.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, rule_of_law_adherents, payer,
    organized, generational, constrained, national).

% Groups that historically relied on counter-majoritarian judicial review to protect rights against legislative majorities. They bear the cost of reduced judicial insulation from popular pressure, facing heightened vulnerability when constitutional meaning is determined by majoritarian contestation. Exit is constrained by their minority status and dependence on institutional protections.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, minorities_seeking_judicial_protection, payer,
    powerless, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes constitutional interpretive authority across elected branches and popular political movements, coordinating democratic legitimacy and popular sovereignty against the concentration of meaning-making power in unelected courts.
% TRANSFER_FUNCTION: Moves interpretive authority and constitutional meaning-making capacity from the federal judiciary and legal elites to legislative majorities and organized popular movements; moves stability, predictability, and counter-majoritarian protection away from minorities and rule-of-law adherents.
% ABSENT_VOICES: Minority groups lacking mobilization infrastructure are structurally underrepresented in the majoritarian contest even when they are affected by constitutional outcomes; legal formalists and technocratic administrative elites are marginal in this framing.
% DISAPPEARANCE_RATIONALE: If constitutional meaning were no longer shaped by popular movements and democratic contestation, interpretive authority would recentralize in the judiciary, legislative constitutionalism would lose its normative footing, and the present alignment between social movements and constitutional argument would fracture â the architecture of American constitutional politics would reorganize around judicial supremacy.
% FOUNDING_PROBLEM: The counter-majoritarian difficulty: unelected judges with life tenure exercising final authority over constitutional meaning disconnected from ongoing democratic will, producing elite control over fundamental values.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and democratic theorists outside the judiciary corroborate the democratic deficit of judicial supremacy; however, minority-rights advocates and legal process theorists attest that judicial review is the necessary solution to majority tyranny, and no consensus exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because interpretive authority is a real governance resource and minorities lose counter-majoritarian shelter. Suppression is moderate (0.48) because the reading must actively contest entrenched judicial supremacy norms through political mobilization and institutional resistance, not mere preference. Theater ratio is low (0.20) because democratic contestation is substantively oriented toward shifting constitutional meaning rather than performative maintenance. Accessibility collapse is moderate (0.45) because originalism and judicial supremacy remain visible and institutionally defended alternatives. Resistance is high (0.72) due to entrenched legal professional culture and institutionalized judicial self-defense. The measurement series share a single time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and beneficiary seats should compute to different constraint types. Judicial finality advocates and minorities experience the regime as extraction of stability and protective finality; their high directionality and constrained or identity-locked exit amplify effective extraction. Popular movements and legislative majorities experience the same arrangement as democratic empowerment and interpretive subsidy; their mobile or institutionally embedded positions yield low directionality. The engine derives this divergence from the structural data rather than the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are situated near the full-beneficiary end: popular movements have mobile exit and organized power, legislative majorities hold institutional power even if exit is constrained by politics. Victims are situated near the full-target end: judicial finality advocates are identity-locked to the professional framework of judicial supremacy, and minorities seeking judicial protection are powerless with constrained exit. Rule-of-law adherents sit between organized and constrained. The structural asymmetry drives the engine's per-seat classification divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled_rope classification, this arrangement could be misread as a pure rope (democratic coordination) by noticing only the popular sovereignty function, or as a pure snare (majoritarian extraction) by noticing only the costs to minorities. The classification prevents both errors by requiring both a genuine coordination function (democratic legitimacy) and asymmetric extraction (loss of judicial protection and stability) to be present simultaneously. Active enforcement is required because judicial supremacy is the default institutional equilibrium in the American legal order; popular constitutionalism persists only through continuous political contestation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarian_minority_paradox,
    'Does popular constitutionalism genuinely empower all popular movements, or does it systematically advantage majority-aligned movements at the expense of minority-protective constitutionalism?',
    'Comparative historical analysis of which movements successfully shape constitutional meaning under this regime, weighted by minority status and electoral alignment.',
    'If majority-aligned movements dominate, the constraint''s extraction from minorities is structurally higher than the base metric suggests, strengthening the snare-like features relative to its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_minority_paradox, empirical, 'Whether popular constitutionalism advantages majority movements over minorities').

omega_variable(
    judicial_resistance_capacity,
    'Can judicial institutions effectively resist popular constitutionalist pressures, or does sustained political contestation erode judicial independence irreversibly?',
    'Longitudinal study of judicial behavior, institutional legitimacy polling, and compliance rates under periods of intense popular mobilization.',
    'If judicial resistance collapses, the constraint''s suppression metric may be higher than authored because judicial exit options close; if courts maintain autonomy, the constraint remains contested and its extraction is dampened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_resistance_capacity, empirical, 'Capacity of judicial institutions to resist popular constitutionalist pressure').

omega_variable(
    framing_under_determination,
    'Is the authority structure better framed as distributed popular sovereignty or as legislative supremacy with extra-institutional pressure?',
    'Examination of where binding constitutional decisions actually settle under this reading â in legislatures, in social movements, or in dispersed public discourse â and whether any single locus dominates.',
    'A legislative-supremacy framing would concentrate extraction on courts and minorities more sharply, shifting the computed directionality for legislative majorities downward toward full beneficiary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'Alternative framing of where interpretive authority ultimately sits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(us_c_tr_t6, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(us_c_tr_t12, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(us_c_tr_t18, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 18, 0.18).
narrative_ontology:measurement(us_c_tr_t24, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(us_c_tr_t36, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 36, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(us_c_be_t6, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(us_c_be_t12, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(us_c_be_t18, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 18, 0.54).
narrative_ontology:measurement(us_c_be_t24, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(us_c_be_t30, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(us_c_be_t36, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 36, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(us_constitution_interpretive__popular_constitutionalism_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, living_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the us_constitution_interpretive kernel, decomposed per the Îµ-invariance principle because the label 'constitutional interpretation' conflates three structurally distinct claims: originalist fixation, living adaptation, and popular contestation. Each reading has distinct Îµ, beneficiaries, and victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
