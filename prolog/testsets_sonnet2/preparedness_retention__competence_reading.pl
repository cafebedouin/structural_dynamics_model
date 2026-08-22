% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Flood Preparedness as Live Exercised Competence (Competence Reading)
 *   domain: governance/disaster_preparedness
 *
 * SUMMARY:
 *   This story instantiates the competence reading of the
 *   preparedness_retention kernel: drills, dike inspections, and joint
 *   emergency exercises are read as live, diagnostic rehearsals that actually
 *   maintain operational capacity to respond to flooding, storm surge, and
 *   related infrastructure failure. Under this reading the coordination
 *   function is real and dominant — the ceremony-to-competence ratio is low,
 *   resources track skill retention rather than sign-off, and the beneficiary
 *   is population safety with no identified victim (the flag case,
 *   over-investment producing fiscal inefficiency, is not observed in the
 *   authored metrics). This is a distinct constraint from the husk_reading
 *   (same kernel, same practices, read as ceremonial performance with
 *   negligible live competence) and the hybrid_reading (same kernel, read as
 *   stratified — technical competence retained in specialized bodies while
 *   broader societal memory goes ceremonial). Per the ε-invariance principle,
 *   these are three separate constraint stories sharing one kernel, not one
 *   story with a measurement parameter; ε for this reading is low and stable
 *   because the reading's own evidentiary lights (audit correlation between
 *   drill realism and response outcomes) support genuine retention.
 *
 * KEY AGENTS:
 *   - water_management_agencies: agenda_setter (institutional/constrained) — designs and runs the exercise regime, accountable for infrastructure failure
 *   - coastal_and_riverine_populations: primary beneficiary (moderate/trapped) — depends on genuine readiness, cannot verify it directly
 *   - emergency_response_agencies: beneficiary and co-designer (organized/constrained) — personnel skill is the object being preserved
 *   - independent_engineering_auditors: analytical observer (organized/analytical) — external evidentiary check on whether the competence reading holds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.12).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Flood Preparedness as Live Exercised Competence (Competence Reading)").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "governance/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, '3e69d711-8eb2-4c2d-aba4-ec4a9eb5a48d').
narrative_ontology:cs_kernel_codification('3e69d711-8eb2-4c2d-aba4-ec4a9eb5a48d', distributed).
narrative_ontology:cs_authority_grounding('3e69d711-8eb2-4c2d-aba4-ec4a9eb5a48d', expertise).
narrative_ontology:cs_interpretation_layer_present('3e69d711-8eb2-4c2d-aba4-ec4a9eb5a48d').
narrative_ontology:cs_reading_relation('3e69d711-8eb2-4c2d-aba4-ec4a9eb5a48d', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e69d711-8eb2-4c2d-aba4-ec4a9eb5a48d', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('3e69d711-8eb2-4c2d-aba4-ec4a9eb5a48d', foundational, live_repetition_produces_transferable_skill).
narrative_ontology:cs_axiom_status(live_repetition_produces_transferable_skill, holdable).
narrative_ontology:cs_axiom_grounding('3e69d711-8eb2-4c2d-aba4-ec4a9eb5a48d', live_repetition_produces_transferable_skill, empirically_contingent).
narrative_ontology:cs_axiom('3e69d711-8eb2-4c2d-aba4-ec4a9eb5a48d', secondary, audited_performance_correlation_validates_the_practice).
narrative_ontology:cs_axiom_status(audited_performance_correlation_validates_the_practice, holdable).
narrative_ontology:cs_axiom_grounding('3e69d711-8eb2-4c2d-aba4-ec4a9eb5a48d', audited_performance_correlation_validates_the_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('3e69d711-8eb2-4c2d-aba4-ec4a9eb5a48d', post_1953_engineered_readiness_standard).
narrative_ontology:cs_drift_state('3e69d711-8eb2-4c2d-aba4-ec4a9eb5a48d', contemporary_climate_intensification_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3e69d711-8eb2-4c2d-aba4-ec4a9eb5a48d', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, coastal_and_riverine_populations).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, emergency_response_agencies).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, national_treasury).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, operational_readiness_requires_live_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, fund, and run the drill and inspection cycle: dike inspections, storm-surge barrier tests, evacuation exercises, joint exercises with emergency services. Sets standards for what counts as a passed inspection and allocates budget between live exercises and paper compliance. Held accountable when infrastructure fails, which keeps the practices tied to actual performance rather than sign-off.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, water_management_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Live behind the dikes and barriers whose failure would be catastrophic. Cannot personally verify readiness and depend entirely on the agencies' exercised competence being real rather than ceremonial. Benefit directly and disproportionately from genuine retained capacity; bear the risk if the reading is wrong, but under this reading that risk is well-managed.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, coastal_and_riverine_populations, beneficiary,
    moderate, generational, trapped, regional).

% Fire brigades, civil protection units, and military engineering corps participate in and partly design the joint exercises. Their personnel's actual skill at flood response depends on repetition under realistic conditions; they benefit from drills that are genuinely difficult and diagnostic rather than scripted, and push back when exercises drift toward theater.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, emergency_response_agencies, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, emergency_response_agencies, agenda_setter).

% Funds the inspection and drill regime. Under this reading, money spent on live exercises is efficiently converted into retained capacity rather than wasted on ceremony, which is the fiscal justification for continued investment at current levels.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, national_treasury, beneficiary,
    institutional, generational, analytical, national).

% External bodies (e.g. Delta Commissioner reviews, technical audit panels) assess whether inspection results correlate with actual structural performance and whether personnel demonstrate live problem-solving during exercises rather than following scripts. Their reports are the primary evidence for or against the competence reading.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, independent_engineering_auditors, observer,
    organized, biographical, analytical, national).

% Fund the system through general taxation without direct exposure to flood risk and without technical means to evaluate whether the drills are competence-preserving or ceremonial. Not part of the audit or design conversation, though they would object if the answer turned out to be ceremony.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, general_public_outside_flood_zones, excluded,
    powerless, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a population of trained personnel and verified infrastructure capable of executing a real flood response under time pressure, by repeatedly rehearsing the response under conditions close enough to the real emergency to expose and correct gaps.
% TRANSFER_FUNCTION: Moves public funds into recurring, resource-intensive exercises and inspections; in return moves risk of catastrophic failure away from the protected population by keeping response capacity current. No party extracts a rent from this reading of the arrangement; the transfer is fiscal input for safety output.
% ABSENT_VOICES: Taxpayers outside flood-risk zones bear the cost without direct benefit and are not consulted on whether the ratio of drilling to actual risk is well-calibrated; under this reading their absence is not exploitative since the coordination genuinely produces the safety it claims, but their exclusion means the efficiency question goes unaudited by anyone with a cost-minimizing incentive.
% DISAPPEARANCE_RATIONALE: If the exercise regime vanished, personnel skill would decay on a predictable curve (established response-training literature), infrastructure faults would go undetected between catastrophic events, and actual response capability during the next major flood would be materially lower — the protected population's risk exposure would rise measurably, not just symbolically.
% FOUNDING_PROBLEM: Historical flood disasters (notably the 1953 North Sea flood) demonstrated that infrastructure and personnel untested under realistic conditions fail catastrophically when the real event arrives; the drill/inspection regime was built to ensure capacity is verified live rather than assumed from design specifications alone.
% FOUNDING_PROBLEM_CORROBORATION: Independent engineering audits and post-incident reviews (including near-miss events since 1953) continue to find that response outcomes correlate with recency and realism of exercises, corroborating the founding problem's persistence from outside the funded agencies themselves; international disaster-response researchers studying comparable systems report the same skill-decay dynamic absent live practice.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__competence_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).
:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) and suppression low (0.12) because, under this reading, the arrangement's coordination function is genuine and dominant: the coordination story is not cover, and no party's exit is being blocked to protect a rent. Theater ratio is authored low (0.15) and essentially flat across the interval, reflecting the reading's central empirical claim — that ceremony has not displaced competence over time. Accessibility collapse is moderate (0.35) rather than mountain-high because alternative preparedness models (e.g. purely infrastructural hardening without live drilling) remain conceptually available, just judged inferior by this reading's own evidence. Resistance is low (0.2): the arrangement meets little organized opposition because, under this reading, it is not extracting from anyone in a way that would generate resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, the practice is unambiguous functioning coordination. The excluded general public outside flood zones would, if consulted, likely ask whether the drill/inspection intensity is well-calibrated to actual risk — a fiscal-efficiency question rather than a competence-versus-ceremony question, and one this reading treats as a legitimate but separate concern rather than evidence against the competence claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Water management agencies and emergency response agencies sit near the coordination-benefit end: they run and participate in the practice and are structurally accountable for its output, which under this reading is real capacity rather than captured rents. Coastal and riverine populations are beneficiaries whose directionality is pulled toward the subsidized end because the constraint's entire function, under this reading, is protecting them — their trapped exit option (they cannot relocate en masse) would ordinarily push directionality toward target/victim, but the beneficiary declaration and absence of any extraction mechanism keeps them on the benefit side. No agent is declared a victim; the fiscal-inefficiency victim case flagged in the structural delta is not present in these authored metrics, which is itself part of what makes this the competence reading and not the hybrid or husk reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (verified live capacity distinct from paper compliance, motivated by 1953-scale failure) is authored as still live, corroborated by independent audits outside the funding agencies — this blocks a mandatrophy verdict under this reading. The classification prevents mislabeling genuine, still-functioning coordination as pure ceremony merely because it involves recurring, costly, state-run rituals; the diagnostic question the sibling readings exist to test is precisely whether that corroboration would hold up under closer audit, which this story does not adjudicate — it only authors the competence reading's own account.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drill_realism_vs_scripted_exercise,
    'Do the exercises and inspections this reading treats as competence-preserving actually expose personnel and infrastructure to conditions realistic enough to produce transferable skill, or are they scripted to a degree that the husk_reading''s ceremonial characterization would better fit?',
    'Independent audit comparing exercise design (branching scenarios, unscripted failure injection, blind evaluation) against outcomes in actual incidents and near-misses; compare personnel performance in drills versus real deployments.',
    'If exercises are found to be substantially scripted with predictable pass conditions, this story''s low theater_ratio and low extractiveness would be wrong and the constraint would need to be re-authored as the husk_reading or hybrid_reading instead of amended in place.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_realism_vs_scripted_exercise, empirical, 'Whether the authored low theater_ratio reflects genuine drill realism or under-detected ceremony.').

omega_variable(
    stratification_boundary_unaddressed,
    'Even if specialized technical agencies genuinely retain live competence, does the broader civic/societal layer of preparedness (household evacuation knowledge, local volunteer readiness) retain comparable competence, or has that layer degraded to ceremony while only the technical core remains live?',
    'Survey and drill-participation data disaggregated by institutional layer (technical agency vs. civic/volunteer vs. general public), compared over multiple exercise cycles.',
    'If the civic layer shows ceremonial degradation while the technical layer stays live, the correct reading for the full preparedness system is the hybrid_reading, not this uniform competence_reading — the boundary this omega names is exactly the structural question the hybrid_reading exists to answer as a separate constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_boundary_unaddressed, conceptual, 'Whether competence is uniform across institutional layers or the system is better modeled as stratified.').

omega_variable(
    fiscal_overinvestment_flag,
    'Is the current level of drill/inspection investment calibrated to actual marginal risk reduction, or does the arrangement over-invest relative to the safety gain, producing a fiscal-efficiency victim not currently declared in this story''s victims array?',
    'Cost-benefit analysis comparing exercise/inspection expenditure growth against measured reduction in response-time and failure-rate metrics over the same interval.',
    'If over-investment is found, the story should be re-authored with fiscal_efficiency-bearing taxpayers as a declared victim group, which could shift the computed type toward tangled_rope depending on enforcement structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_overinvestment_flag, empirical, 'Whether resource allocation is efficiently matched to retained capacity or over-invested relative to benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(prep_tr_t8, preparedness_retention__competence_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(prep_tr_t16, preparedness_retention__competence_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(prep_tr_t24, preparedness_retention__competence_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(prep_tr_t32, preparedness_retention__competence_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__competence_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(prep_be_t8, preparedness_retention__competence_reading, base_extractiveness, 8, 0.15).
narrative_ontology:measurement(prep_be_t16, preparedness_retention__competence_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement(prep_be_t24, preparedness_retention__competence_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(prep_be_t32, preparedness_retention__competence_reading, base_extractiveness, 32, 0.18).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__competence_reading, base_extractiveness, 40, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_retention__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the preparedness_retention kernel: this file (competence_reading, low ε, rope-shaped, no victim), preparedness_retention__husk_reading (memorial performance, high theater_ratio, likely piton or snare depending on beneficiary capture), and preparedness_retention__hybrid_reading (stratified competence, mixed profile across institutional layers). Each authors its own ε and stakeholder structure per the ε-invariance principle; they are linked here rather than merged because measuring 'preparedness' by different observables (audited technical performance vs. civic ceremony vs. layered institutional structure) yields materially different extraction profiles — exactly the BGS-style decomposition case.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
