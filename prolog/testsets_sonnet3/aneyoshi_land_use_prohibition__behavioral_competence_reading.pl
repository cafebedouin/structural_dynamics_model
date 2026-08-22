% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__behavioral_competence_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   In 1933 a massive tsunami struck the Sanriku coast of Japan, and
 *   survivors in the hamlet of Aneyoshi erected an inscribed stone marking
 *   the high-water line, with an accompanying injunction not to build below
 *   it. This reading holds that the injunction was not merely commemorative
 *   text on rock but an operationally enforced land-use rule, sustained
 *   across 78 years by oral transmission (elders recounting the disaster to
 *   children), social pressure on siting decisions, and observable settlement
 *   patterns that respected the line. When the 2011 Tōhoku tsunami struck,
 *   houses built above the stone survived; those below it, including some
 *   newer construction, were destroyed — providing an unusually clean natural
 *   experiment on whether the rule was still live.
 *
 * KEY AGENTS:
 *   - aneyoshi_hamlet_residents: Primary beneficiary and self-administering agenda-setter (moderate/constrained) — transmits and is protected by the rule
 *   - elders_and_tradition_bearers: Transmission mechanism (moderate/identity_locked) — identity constituted by carrying the warning forward
 *   - downslope_land_developers: Minor dissenting preference, unorganized (powerless/constrained)
 *   - coastal_engineering_researchers: Analytical observer (institutional/analytical) — documents the outcome that adjudicates between the two kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.04).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.04).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:requires_active_enforcement(aneyoshi_land_use_prohibition__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'bea17ede-54dd-48ae-91f7-e7f27d3efd9a').
narrative_ontology:cs_kernel_codification('bea17ede-54dd-48ae-91f7-e7f27d3efd9a', implicit).
narrative_ontology:cs_authority_grounding('bea17ede-54dd-48ae-91f7-e7f27d3efd9a', practice).
narrative_ontology:cs_interpretation_layer_present('bea17ede-54dd-48ae-91f7-e7f27d3efd9a').
narrative_ontology:cs_reading_relation('bea17ede-54dd-48ae-91f7-e7f27d3efd9a', aneyoshi_land_use_prohibition__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('bea17ede-54dd-48ae-91f7-e7f27d3efd9a', foundational, oral_transmission_sustains_operative_constraint_across_generational_gap).
narrative_ontology:cs_axiom_status(oral_transmission_sustains_operative_constraint_across_generational_gap, holdable).
narrative_ontology:cs_axiom_grounding('bea17ede-54dd-48ae-91f7-e7f27d3efd9a', oral_transmission_sustains_operative_constraint_across_generational_gap, empirically_contingent).
narrative_ontology:cs_axiom('bea17ede-54dd-48ae-91f7-e7f27d3efd9a', secondary, settlement_pattern_is_direct_evidence_of_rule_compliance_not_coincidence).
narrative_ontology:cs_axiom_status(settlement_pattern_is_direct_evidence_of_rule_compliance_not_coincidence, holdable).
narrative_ontology:cs_axiom_grounding('bea17ede-54dd-48ae-91f7-e7f27d3efd9a', settlement_pattern_is_direct_evidence_of_rule_compliance_not_coincidence, empirically_contingent).
narrative_ontology:cs_reference_frame('bea17ede-54dd-48ae-91f7-e7f27d3efd9a', post_1933_survivor_transmission_norm).
narrative_ontology:cs_drift_state('bea17ede-54dd-48ae-91f7-e7f27d3efd9a', pre_2011_contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('bea17ede-54dd-48ae-91f7-e7f27d3efd9a', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_hamlet_residents).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, oral_tradition_can_transmit_operative_land_use_constraints_across_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in and around the small hamlet on the Sanriku coast where the inscribed stone marks the line below which the 1933 tsunami reached. Successive generations were told, and told their children in turn, not to build dwellings below the marker. This transmitted rule is a real input into where houses are actually sited; it is enforced through community memory, elder instruction, and visible settlement pattern rather than by any external agency. They are simultaneously the ones who benefit from the constraint (fewer inundation deaths in 2011) and the ones who administer it (deciding, household by household, where to build).
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_hamlet_residents, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_hamlet_residents, agenda_setter).

% Serve as the living transmission mechanism for the stone's warning — recounting the 1933 and 1896 tsunamis, explaining why the line matters, and socially discouraging construction below it. Their identity and standing in the community are partly constituted by being the carriers of this knowledge; abandoning the transmission role would mean the rule's operative force lapses within a generation, since the stone's inscription alone does not compel behavior without the narrated context.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, elders_and_tradition_bearers, agenda_setter,
    moderate, generational, identity_locked, local).

% Would prefer to build closer to the coast where land is flatter, more convenient, and closer to fishing infrastructure, but face informal social pressure and the accumulated weight of oral warning against building below the marker. Their preference is not organized into an articulated objection within the community; they simply build elsewhere or face quiet disapproval if they don't.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, downslope_land_developers, excluded,
    powerless, biographical, constrained, local).

% Study Aneyoshi as a rare, empirically confirmed case where a low-tech, orally transmitted land-use rule produced a measurable survival outcome — every house above the stone survived the 2011 tsunami; every structure below it, including newer construction that had crept downslope in recent decades, was destroyed. They analyze the case to compare social-transmission mechanisms against engineered infrastructure (seawalls, evacuation towers) as tsunami-mitigation strategies.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, coastal_engineering_researchers, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_hamlet_residents).
narrative_ontology:fixing_cost_class(aneyoshi_land_use_prohibition__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves an intergenerational information problem: tsunami recurrence intervals (multiple decades to a century) exceed a single living memory, so a physical marker plus a narrated obligation transmits the empirical inundation line to residents who will never personally witness a tsunami before deciding where to build.
% TRANSFER_FUNCTION: Moves no resources between parties; it moves a behavioral constraint (a location prohibition) forward in time from the generation that experienced the 1933 tsunami to generations that did not, at zero extraction — the cost is diffuse (marginally less convenient home siting) and the benefit accrues to the same population that bears the cost.
% ABSENT_VOICES: Individual households who might have preferred to build below the line for convenience or economic reasons are not organized as a dissenting faction; the constraint operates through diffuse social pressure rather than formal prohibition, so there is no adjudicating body an objector could appeal to — dissent, where it existed, simply resulted in construction below the line and, in 2011, in loss.
% DISAPPEARANCE_RATIONALE: If the oral-transmission mechanism and the social force behind the marker disappeared, land immediately below the stone would be judged by ordinary economic logic (flatter land, proximity to the harbor, convenience) rather than by inundation history, and residential construction would migrate downslope within one or two building cycles — which is precisely what happened in nearby communities without an operative equivalent, and partly what had already begun happening even in Aneyoshi among newer households less exposed to the narrative.
% FOUNDING_PROBLEM: Recurring tsunamis on the Sanriku coast (1896, 1933, and earlier events) killed large fractions of coastal hamlets whose settlement patterns had drifted back toward the shoreline in the intervals between disasters; the stone and its accompanying oral rule were erected after 1933 specifically to prevent that drift from recurring.
% FOUNDING_PROBLEM_CORROBORATION: Independent post-2011 field surveys and disaster-anthropology research (conducted by academic researchers with no stake in the hamlet's land values) documented that houses sited above the inscribed line survived the 2011 tsunami while those below it, including recently built structures, did not — corroboration external to the residents themselves, based on physical outcome rather than on the community's own account of its practice.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.04, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).
:- end_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.04) because no party captures rents from the constraint's operation — the same population bears the (small) inconvenience cost of building on less convenient upslope land and receives the (large) survival benefit. Suppression is low-moderate (0.12) and diffuse: it is social disapproval and narrative pressure, not coercive enforcement by any administering authority, and it has mildly declined over the interval as the immediate memory of 1933 receded even under successful transmission. Theater ratio is very low (0.08) because the overwhelming majority of the constraint's operation is functional — actual siting decisions were shaped by the rule, not by performative deference to it — though a small and slowly rising theatrical component is honest: some later households likely treated the marker with respectful acknowledgment while personal urgency to obey weakened generationally. Accessibility collapse is high (0.72): once a household internalizes the inundation-line logic, building below it is not a live option they seriously consider, which is exactly the natural-law-adjacent texture of a well-transmitted physical constraint. Resistance is very low (0.05) — there is essentially no organized objection to the rule, only quiet non-compliance by a minority.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi residents are declared as both beneficiary and agenda_setter because the rule is self-administered: the same community that transmits and polices the constraint is the community that benefits from it. There is no extractive third party. Elders are identity-locked in their exit options because their social role is partly constituted by being the narrative's carriers — but this identity-lock serves the coordination function rather than trapping anyone in an extractive relationship, an important structural contrast with identity-lock patterns in extractive constraints. Downslope developers are the closest thing to a dissenting seat, but their preference was never organized into a competing framework; the derivation correctly keeps their directionality mild since the cost to them (marginal inconvenience) is small relative to the benefit even they eventually receive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (periodic tsunami-driven mortality from settlement drift toward the shoreline) is authored as live rather than dead, and the disappearance verdict is world_rearranges, precisely because the 2011 event supplied a rare confirming test: the rule's removal (hypothetically, or its de facto lapse in neighboring communities without an equivalent living tradition) is observably correlated with worse outcomes. This blocks the mandatrophy misreading in which a long-running, unenforced-looking folk custom is dismissed as empty ritual — the outcome data available at t=2011 (which this reading, uniquely among the two siblings, treats as confirming operative force) is what prevents this constraint from being mislabeled as a piton or commemorative husk. The claim/metric divergence check: claimed_type is rope and the authored metrics (very low extraction, high accessibility collapse, low resistance, low theater) are consistent with that claim rather than tuned to produce it — this is the case where claim and metrics agree, which is itself informative given the corpus expects some genuine ropes to compute cleanly as ropes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_symbolic_kernel_reading,
    'Was the tsunami stone''s prohibition still an operative behavioral constraint on land use in the years and decades immediately preceding 2011, or had it decayed into a symbolic/commemorative object by the time of the event, with the 2011 survival pattern explained by other factors (topography, coincidental settlement history, road access) rather than active rule-following?',
    'Oral-history interviews conducted with Aneyoshi residents prior to 2011 (if any exist), land-registry and construction-permit records showing when specific parcels below the line were developed relative to household composition and stated reasoning, and comparison with nearby hamlets that had similar markers but different outcomes.',
    'If resolved toward operative behavioral force, this reading (behavioral_competence_reading) is the structurally accurate one and the sibling commemorative_husk_reading should be understood as describing a different, non-actual counterfactual. If resolved toward symbolic decay with 2011 survival driven by confounding factors, this reading over-attributes causal force to the oral tradition and the husk reading becomes the operative account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_vs_symbolic_kernel_reading, empirical, 'Whether the prohibition was behaviorally live or already symbolically decayed before 2011 — the central fact this reading asserts and its sibling denies.').

omega_variable(
    post_hoc_narrative_construction,
    'Is the ''78 years of continuous enforcement'' narrative itself partly a post-2011 reconstruction — i.e., did the dramatic confirming outcome in 2011 cause communities and journalists to retroactively describe pre-2011 practice as more rule-governed and continuous than it actually was?',
    'Cross-reference contemporaneous (pre-2011) local government planning documents, newspaper archives, and academic ethnographies of Sanriku coast settlement patterns against post-2011 retrospective accounts, looking for discontinuities in how the practice is described before versus after the confirming event.',
    'If the pre-2011 record shows the rule was inconsistently followed or barely discussed, this reading''s extractiveness and accessibility_collapse figures would need revision downward, and the constraint would sit closer to the husk reading with a confirming-bias narrative layered on after the fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_hoc_narrative_construction, conceptual, 'Whether the operative-rule narrative is itself partly a survivorship-driven retrospective construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1933, 0.02).
narrative_ontology:measurement_basis(aney_tr_t1933, observed).
narrative_ontology:measurement(aney_tr_t1948, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1948, 0.03).
narrative_ontology:measurement_basis(aney_tr_t1948, observed).
narrative_ontology:measurement(aney_tr_t1963, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1963, 0.04).
narrative_ontology:measurement_basis(aney_tr_t1963, observed).
narrative_ontology:measurement(aney_tr_t1978, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1978, 0.05).
narrative_ontology:measurement_basis(aney_tr_t1978, observed).
narrative_ontology:measurement(aney_tr_t1994, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1994, 0.06).
narrative_ontology:measurement_basis(aney_tr_t1994, observed).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 2011, 0.08).
narrative_ontology:measurement_basis(aney_tr_t2011, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1933, 0.02).
narrative_ontology:measurement_basis(aney_be_t1933, observed).
narrative_ontology:measurement(aney_be_t1948, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1948, 0.02).
narrative_ontology:measurement_basis(aney_be_t1948, observed).
narrative_ontology:measurement(aney_be_t1963, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1963, 0.03).
narrative_ontology:measurement_basis(aney_be_t1963, observed).
narrative_ontology:measurement(aney_be_t1978, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1978, 0.03).
narrative_ontology:measurement_basis(aney_be_t1978, observed).
narrative_ontology:measurement(aney_be_t1994, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1994, 0.04).
narrative_ontology:measurement_basis(aney_be_t1994, observed).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 2011, 0.04).
narrative_ontology:measurement_basis(aney_be_t2011, observed).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1933, 0.2).
narrative_ontology:measurement_basis(aney_su_t1933, observed).
narrative_ontology:measurement(aney_su_t1948, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1948, 0.18).
narrative_ontology:measurement_basis(aney_su_t1948, observed).
narrative_ontology:measurement(aney_su_t1963, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1963, 0.16).
narrative_ontology:measurement_basis(aney_su_t1963, observed).
narrative_ontology:measurement(aney_su_t1978, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1978, 0.15).
narrative_ontology:measurement_basis(aney_su_t1978, observed).
narrative_ontology:measurement(aney_su_t1994, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1994, 0.13).
narrative_ontology:measurement_basis(aney_su_t1994, observed).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 2011, 0.12).
narrative_ontology:measurement_basis(aney_su_t2011, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This story and aneyoshi_land_use_prohibition__commemorative_husk_reading are sibling readings of the same kernel (the tsunami stone and its inscription). They deliberately author different ε (0.04 here vs. a near-zero-function husk elsewhere) and different structural claims (rope with a genuine coordination function here vs. a decayed piton-adjacent symbol there) because they disagree about a matter of fact: whether the prohibition was behaviorally operative across the 78-year interval. Per the ε-invariance principle, this disagreement is modeled as two constraints, not one constraint with an ambiguous ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
