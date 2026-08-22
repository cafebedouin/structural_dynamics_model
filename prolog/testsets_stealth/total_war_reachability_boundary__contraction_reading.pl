% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contraction_reading, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Contraction Reading: Winnable Total War Outside the Feasible Set
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the
 *   total_war_reachability_boundary kernel: the contraction_reading, under
 *   which thermonuclear arsenals with assured second-strike capability
 *   contracted the strategic space until winnable total war between great
 *   powers left the feasible set entirely. The constraint is the closure
 *   itself — the standing arrangement under contest is the present-day
 *   unavailability of total war, and epsilon is authored for that arrangement
 *   as this reading sees it: a structural limit that extracts from no one,
 *   employs no one, and requires no enforcement machinery, because
 *   retaliation physics does the enforcing. The sibling readings
 *   (dropping_reading, contingent_reachability_reading) are separate
 *   constraints in separate files; per the single-reading discipline, no
 *   hedge across readings appears in the metrics. The expected structural
 *   delta is honored: no beneficiary structure is declared because no actor
 *   collects from the boundary's operation, and the universal victim set
 *   attaches to the boundary's FAILURE mode, not to its standing operation —
 *   that distinction is carried in the universal_tail_risk_accounting omega
 *   rather than in a victims declaration, because the standing arrangement
 *   imposes no costs on anyone. KEY AGENTS (by structural relationship): -
 *   nuclear_powers_command_authorities: Administrator of the boundary's
 *   physical substrate (institutional/identity_locked) — maintains the
 *   second-strike forces whose survivability closes the outcome set; did not
 *   choose the boundary, inherited it - civilian_populations_nuclear_states:
 *   Sheltered background party (moderate/trapped) — receives the absence of
 *   total war as an unchosen condition; their cities are the hostages that
 *   make retaliation credible - extended_deterrence_allies: Umbrella
 *   beneficiary bearing sovereignty costs (powerful/constrained) — plans
 *   defense around the ceiling and forgoes independent arsenals -
 *   nonaligned_non_nuclear_states: Voiceless beneficiary-at-risk
 *   (moderate/trapped) — enjoys damped great-power war with no seat and no
 *   arsenal; bears full exposure to failure -
 *   counterforce_restoration_community: Recurring challenger paying futile
 *   program costs (institutional/mobile) — invests in restoring damage
 *   limitation that the closure prices out - disarmament_movements: Excluded
 *   objector (organized/constrained) — contests the permanence framing from
 *   outside the planning rooms - strategic_studies_community: Analytical
 *   observer (analytical/analytical) — formalized the reading and audits it
 *   against new technology
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.05).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.8).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Contraction Reading: Winnable Total War Outside the Feasible Set").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, '6968312d-384f-47de-802f-22c12ee990ce').
narrative_ontology:cs_kernel_codification('6968312d-384f-47de-802f-22c12ee990ce', distributed).
narrative_ontology:cs_authority_grounding('6968312d-384f-47de-802f-22c12ee990ce', expertise).
narrative_ontology:cs_interpretation_layer_present('6968312d-384f-47de-802f-22c12ee990ce').
narrative_ontology:cs_reading_relation('6968312d-384f-47de-802f-22c12ee990ce', total_war_reachability_boundary__dropping_reading, forecloses).
narrative_ontology:cs_reading_relation('6968312d-384f-47de-802f-22c12ee990ce', total_war_reachability_boundary__contingent_reachability_reading, forecloses).
narrative_ontology:cs_axiom('6968312d-384f-47de-802f-22c12ee990ce', foundational, offense_dominance_is_permanent).
narrative_ontology:cs_axiom_status(offense_dominance_is_permanent, holdable).
narrative_ontology:cs_axiom_grounding('6968312d-384f-47de-802f-22c12ee990ce', offense_dominance_is_permanent, empirically_contingent).
narrative_ontology:cs_axiom('6968312d-384f-47de-802f-22c12ee990ce', secondary, deterrence_without_agreement).
narrative_ontology:cs_axiom_status(deterrence_without_agreement, holdable).
narrative_ontology:cs_axiom_grounding('6968312d-384f-47de-802f-22c12ee990ce', deterrence_without_agreement, instrumental).
narrative_ontology:cs_reference_frame('6968312d-384f-47de-802f-22c12ee990ce', assured_second_strike_equilibrium).
narrative_ontology:cs_drift_state('6968312d-384f-47de-802f-22c12ee990ce', contemporary_counterforce_modernization, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('6968312d-384f-47de-802f-22c12ee990ce', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.05, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_reachability_boundary__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.05) because nothing flows through the boundary: no seat collects, no seat is bled. Suppression is high (0.80) but must be read correctly: it is the boundary's self-enforcing force — assured retaliation suppressing the total-war option — and suppression is a raw structural property, unscaled by power or scope; here the high value describes the mechanism working, not coercion applied by any agent to maintain the arrangement. Theater ratio (0.30) reflects the growing share of boundary-adjacent activity that is performative (parades, doctrine statements, modernization announcements) relative to the functional core (survivability engineering); it rises over the interval as the boundary became taken for granted. Accessibility collapse is high (0.90): once the arithmetic of thermonuclear exchange is understood, winnable-total-war alternatives collapse almost completely — forty years of counterforce and missile-defense programs have not reopened them. Resistance is low (0.12): the counterforce restoration community probes the boundary persistently but unsuccessfully. Claim and metrics are independently authored: the claimed type is mountain because this reading holds the closure to be structural, and the metric profile happens to be consistent with that claim — no value was tuned to secure certification. All three tracked series run on one shared seven-point grid (1950–2025) so every metric is authored at every examined time point; the suppression_requirement series is included because the story specifically traces the boundary's enforcement consolidation (thin bomber-era deterrence through SLBM-matured assured destruction, then plateau), not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   With extraction near zero, the seats diverge less in what they pay or collect than in what the boundary IS to them. From the command authorities' seat the closure is the constitution of their institutions — their missions, careers, and budgets are fused with stewardship of the substrate, and unilateral exit (disarmament) would expose them to the very war the boundary forecloses, hence identity_locked exit. From the civilian populations' seat it is an invisible background condition they never negotiated and cannot leave. From the counterforce community's seat it is a wall — a standing refutation their programs keep failing to overturn, borne by people mobile enough to pivot to other work. From the excluded abolitionist seat it is a life sentence: a permanence framing that entrenches the arsenals indefinitely. The engine computes per-seat classifications from power and exit atoms; with epsilon this low every seat should compute mountain-adjacent, but the exit-option spread differentiates the texture of each seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries and no victims are declared, deliberately. The directionality derivation chain finds no structural relationship data pointing any seat toward the target end: nobody is extracted from, and nobody sits at the beneficiary end collecting rents either — the populations and allies who benefit do so incidentally, receiving a public good rather than a transfer taken from anyone. Because the derivation would fall back to canonical defaults that could misread incidental beneficiaries as rent-collectors, the honest authoring choice was to leave the structural arrays empty and let epsilon's near-zero value dominate: effective extraction stays negligible for every seat regardless of derived directionality. No directionality_overrides are authored for the same reason — there is no seat whose derived d would be wrong in a way that matters at this epsilon.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline here guards against two opposite mislabels. Reading the closure as a rope (the dropping_reading's move) would render the ceiling a negotiated equilibrium that diplomacy, signaling, or erosion of resolve could unwind — an invitation to test it. Reading it as a piton (the contingent_reachability_reading's move) would license restoration spending on the theory that the boundary is inertial residue awaiting reversal. Conversely, the temporal series watches for the genuine degradation path: if the arsenal substrate rots while rhetoric continues, theater_ratio climbs past functional maintenance and the piton characterization would become correct — the 0.05-to-0.30 theater drift is the early-warning channel. Mandatrophy is not resolved: the founding problem (preventing recurrence of great-power total war) remains live precisely because the arrangement is load-bearing, so no sunset or obsolescence flag is authored.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the contraction_reading of the total_war_reachability_boundary kernel; how would classification shift under the sibling readings?',
    'Generate and compare the sibling stories: dropping_reading (total war improbable but reachable; deterrence as coordination equilibrium) and contingent_reachability_reading (current contraction as a reversible technological artifact). Cross-read epsilon and computed type across the family.',
    'Under dropping_reading the same observables classify as a rope with moderate epsilon (forbearance maintained by coordinated expectation); under contingent_reachability_reading as a piton awaiting technological reversal. Only the contraction reading yields a mountain with no beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: one reading of a three-reading kernel; siblings are separate constraints, not folded into this one.').

omega_variable(
    constructed_vs_physical_boundary,
    'Is the closure of the total-war outcome set a genuine structural limit of thermonuclear physics, or a contingent artifact of current arsenal configurations that better technology could reopen?',
    'Adversarial technical assessment: can any deployable defense architecture (wide-area missile defense, orbital interception, layered airburst) plausibly reduce assured retaliation below punishment thresholds against a peer''s full arsenal? Four decades of program history (Sentinel, SDI, GMD intercept testing) supplies the base rate.',
    'If some architecture could restore damage limitation against assured retaliation, the boundary is contingent rather than structural, the mountain claim fails, and the story migrates toward the contingent_reachability_reading''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_physical_boundary, empirical, 'Natural-law versus constructed-condition ambiguity at the heart of the mountain claim.').

omega_variable(
    deliberate_vs_accidental_pathways,
    'Does the feasible-set closure bind only deliberate policy, or also the accidental and unauthorized-use pathways that the historical record shows repeatedly approached activation?',
    'Systematic coding of near-miss incidents (1962 ExComm record, 1979 NORAD false alarm, 1983 Petrov episode, Able Archer 83) for how close the system came to total exchange absent any decision to fight one.',
    'If inadvertent pathways are live, the boundary is porous at the margins: the closure holds for chosen wars but not for the full route-space to total exchange, lowering effective closure and weakening the mountain profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deliberate_vs_accidental_pathways, empirical, 'Whether the closure covers the whole route-space to total war or only the deliberate branch.').

omega_variable(
    universal_tail_risk_accounting,
    'This reading declares no current victims, yet boundary failure carries a species-level victim set; should the existential tail risk carried by the arsenals count as extraction imposed on everyone by the arrangement''s mere existence?',
    'An explicit accounting decision: standard expected-value extraction (near-zero, since failure probability is very small) versus tail-weighted accounting (catastrophic loss multiplied by small probability).',
    'Tail-weighted accounting would make every living person simultaneously beneficiary and payer, dissolving the clean no-beneficiary structure; expected-value accounting preserves the mountain profile with negligible extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_tail_risk_accounting, conceptual, 'How to book the species-level tail risk the arsenal substrate carries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1950, total_war_reachability_boundary__contraction_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__contraction_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(tota_tr_t1972, total_war_reachability_boundary__contraction_reading, theater_ratio, 1972, 0.14).
narrative_ontology:measurement(tota_tr_t1985, total_war_reachability_boundary__contraction_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(tota_tr_t1995, total_war_reachability_boundary__contraction_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(tota_tr_t2010, total_war_reachability_boundary__contraction_reading, theater_ratio, 2010, 0.26).
narrative_ontology:measurement(tota_tr_t2025, total_war_reachability_boundary__contraction_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(tota_be_t1950, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1950, 0.02).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1962, 0.04).
narrative_ontology:measurement(tota_be_t1972, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1972, 0.05).
narrative_ontology:measurement(tota_be_t1985, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1985, 0.05).
narrative_ontology:measurement(tota_be_t1995, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1995, 0.04).
narrative_ontology:measurement(tota_be_t2010, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2010, 0.05).
narrative_ontology:measurement(tota_be_t2025, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2025, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1950, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1962, 0.55).
narrative_ontology:measurement(tota_su_t1972, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1972, 0.75).
narrative_ontology:measurement(tota_su_t1985, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(tota_su_t1995, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1995, 0.78).
narrative_ontology:measurement(tota_su_t2010, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2010, 0.79).
narrative_ontology:measurement(tota_su_t2025, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the nuclear revolution' decomposes into three structurally distinct claims about the same observables — categorical exclusion (this story: mountain, epsilon ~0.05, no beneficiary structure), probabilistic reduction (dropping_reading: rope, moderate epsilon, coordination equilibrium), and contingent reversal (contingent_reachability_reading: piton, reversible artifact). Each member carries its own epsilon, its own stakeholder surface, and its own claimed type; they share the kernel and are linked here per the family rule. The upstream record is common: the 1950–1972 consolidation series (survivable second-strike forces maturing through the crisis decade) is cited by this reading as evidence of physical closure, by the dropping_reading as evidence of successful coordination, and by the contingent reading as evidence of a merely technological balance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
