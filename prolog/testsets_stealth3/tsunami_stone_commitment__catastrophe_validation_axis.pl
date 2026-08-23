% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__catastrophe_validation_axis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__catastrophe_validation_axis, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: tsunami_stone_commitment__catastrophe_validation_axis
 *   human_readable: Sanriku Tsunami-Stone Hazard Line: Catastrophe-Validation Axis
 *   domain: disaster_anthropology/commitment_systems/institutional_memory
 *
 * SUMMARY:
 *   Along the Sanriku coast, villages that rebuilt after the 1896 and 1933
 *   tsunamis carved the observed waterlines into stone steles bearing
 *   injunctions to descendants: keep dwellings above the mark. For roughly
 *   six decades of high-growth development the markers were ceremonially
 *   maintained while settlement pushed seaward past them; on 11 March 2011
 *   the Tōhoku tsunami ran the experiment the stones had pre-registered,
 *   destroying the lowland districts and sparing many settlements that had
 *   held the line. This story instantiates ONE reading of that commitment —
 *   the catastrophe-validation axis — under which the 2011 event constitutes
 *   a decisive empirical adjudication delivering binary validation evidence
 *   for the inscribed line, and under which the operative structure is the
 *   physical hazard boundary itself serving as a commitment-test mechanism.
 *   KEY AGENTS (by structural relationship): - meiji_era_stone_erectors:
 *   Founding registrants (powerless/trapped) — carved and funded the markers,
 *   bore the founding cost, never collected - heeding_line_settlements: Net
 *   beneficiaries (organized/constrained) — held the line, collected survival
 *   in 2011, paid the siting premium - hazard_zone_developers: Concentrated
 *   loss-bearers (powerful/constrained) — built below the marks, absorbed the
 *   2011 adjudication outcome - post_tsunami_reconstruction_authorities:
 *   Current administrators (institutional/constrained) — restate the line in
 *   engineering units and control rebuilding -
 *   paleotsunami_research_community: Analytical observer
 *   (analytical/analytical) — tests the inscribed record against the
 *   deep-time deposit record Constraint family note: this file is one member
 *   of a three-reading family over the tsunami_stone_commitment kernel. Its
 *   epsilon (near-zero) is authored for THIS reading only; the sibling files
 *   author their own epsilon over their own referents, and the differences
 *   are the data.
 *
 * KEY AGENTS:
 *   - meiji_era_stone_erectors: founding registrants (powerless/trapped) — bore founding cost, collected nothing
 *   - heeding_line_settlements: net beneficiaries (organized/constrained) — survival collected, siting premium paid
 *   - hazard_zone_developers: concentrated loss-bearers (powerful/constrained) — sixty years of waterside gain, one afternoon of total loss
 *   - post_tsunami_reconstruction_authorities: current administrators (institutional/constrained) — hold the line's present-day administration
 *   - paleotsunami_research_community: analytical observer (analytical/analytical) — adjudicates the instrument against the prehistoric record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__catastrophe_validation_axis, 0.06).
domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, 0.03).
domain_priors:theater_ratio(tsunami_stone_commitment__catastrophe_validation_axis, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, extractiveness, 0.06).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__catastrophe_validation_axis, mountain).
narrative_ontology:human_readable(tsunami_stone_commitment__catastrophe_validation_axis, "Sanriku Tsunami-Stone Hazard Line: Catastrophe-Validation Axis").
narrative_ontology:topic_domain(tsunami_stone_commitment__catastrophe_validation_axis, "disaster_anthropology/commitment_systems/institutional_memory").

domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__catastrophe_validation_axis, '393f835c-1a79-4f0c-aa6f-1168e40b87db').
narrative_ontology:cs_kernel_codification('393f835c-1a79-4f0c-aa6f-1168e40b87db', fixed_text).
narrative_ontology:cs_authority_grounding('393f835c-1a79-4f0c-aa6f-1168e40b87db', lineage).
narrative_ontology:cs_interpretation_layer_present('393f835c-1a79-4f0c-aa6f-1168e40b87db').
narrative_ontology:cs_reading_relation('393f835c-1a79-4f0c-aa6f-1168e40b87db', tsunami_stone_commitment__behavioral_competence_reading, influences).
narrative_ontology:cs_reading_relation('393f835c-1a79-4f0c-aa6f-1168e40b87db', tsunami_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('393f835c-1a79-4f0c-aa6f-1168e40b87db', foundational, runup_line_constitutes_decisive_test).
narrative_ontology:cs_axiom_status(runup_line_constitutes_decisive_test, holdable).
narrative_ontology:cs_axiom_grounding('393f835c-1a79-4f0c-aa6f-1168e40b87db', runup_line_constitutes_decisive_test, empirically_contingent).
narrative_ontology:cs_axiom('393f835c-1a79-4f0c-aa6f-1168e40b87db', secondary, physical_adjudication_requires_no_enforcement).
narrative_ontology:cs_axiom_status(physical_adjudication_requires_no_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('393f835c-1a79-4f0c-aa6f-1168e40b87db', physical_adjudication_requires_no_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('393f835c-1a79-4f0c-aa6f-1168e40b87db', observed_runup_injunction).
narrative_ontology:cs_drift_state('393f835c-1a79-4f0c-aa6f-1168e40b87db', post_2011_adjudication, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('393f835c-1a79-4f0c-aa6f-1168e40b87db', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, heeding_line_settlements).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, hazard_zone_developers).
narrative_ontology:constraint_victim(tsunami_stone_commitment__catastrophe_validation_axis, heeding_line_settlements).
narrative_ontology:constraint_victim(tsunami_stone_commitment__catastrophe_validation_axis, hazard_zone_developers).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, inscribed_runup_record_accuracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fishing households of the Sanriku coast who rebuilt after the 1896 and 1933 waves and carved the observed waterlines into stone steles at the edges of fields and lanes, with instructions to descendants to keep homes above the mark. They financed the markers with village labor and temple donations. None of them lived to see the arrangement repay itself, and a coast-bound fishery gave them no realistic option to move the community inland.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, meiji_era_stone_erectors, agenda_setter,
    powerless, generational, trapped, local).

% Hamlet clusters that kept houses, schools, and meeting halls on the high side of the inscribed line, accepting longer carries to the boats and colder winter wind in exchange for the marker's promise. In March 2011 many of these settlements stood dry above the debris line while lowland neighbors did not. Leaving the fishery for inland work was possible but meant abandoning livelihood and kin networks, so compliance, not departure, was the practical path.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, heeding_line_settlements, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__catastrophe_validation_axis, heeding_line_settlements, payer).

% Postwar municipal governments, port authorities, and processing firms that extended wharves, cannery rows, hospitals, and housing onto reclaimed flats at or below the old marks, treating the inscriptions as relics of a poorer era. They drew the short-run gains of waterside logistics for roughly sixty years and absorbed the concentrated losses of March 2011: swept plants, drowned districts, erased town centers. Capital and staff were regionally mobile before the event; afterward, survival posed the question of return versus relocation to people whose homes and workplaces were the sunk assets.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, hazard_zone_developers, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__catastrophe_validation_axis, hazard_zone_developers, beneficiary).

% National and prefectural reconstruction bodies now deciding where rebuilding is permitted: buyout zones, elevated platform districts, seawall alignments, and hazard maps that restate the stone line in engineering units. They administer the boundary today under budget ceilings, landowner litigation, and residents' attachment to ancestral plots; their instrument set is wide but nothing on it is inexpensive.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, post_tsunami_reconstruction_authorities, agenda_setter,
    institutional, generational, constrained, regional).

% Geologists and hazard scientists who read the prehistoric record — the 869 Jōgan sand sheet beneath the Sendai plain, stratified tsunami deposits in the Sanriku bays — and compare reconstructed thousand-year inundation envelopes against the inscribed lines. Findings circulate through journals and hazard-model updates worldwide; the community holds no stake in any settlement decision.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, paleotsunami_research_community, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(tsunami_stone_commitment__catastrophe_validation_axis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stores an observed inundation boundary in durable public markers and makes it available to descendants who cannot witness the hazard firsthand, aligning settlement siting across generations with a recurrent physical limit that no single lifetime spans often enough to learn directly.
% TRANSFER_FUNCTION: Moves hazard information from past observers to future settlers via inscribed markers; allocates the cost of safety as a permanent siting premium (higher ground, distance from landing points) paid by every compliant generation, and concentrates episodic total loss on whichever generation settles inside the marked boundary.
% ABSENT_VOICES: The ancestors speak materially through the stones, yet postwar planning treated the inscriptions as folklore rather than testimony — the excluded seat is ancestral witness, alongside the future residents of lowland landfill who were sited inside the boundary by decisions taken before they existed. Fisher households needing shoreline access were offered no compliant siting option and were absent from zoning deliberations.
% DISAPPEARANCE_RATIONALE: Remove the hazard boundary and coastal settlement logic reorganizes immediately: port-adjacent lowland becomes prime land, the marker network loses its referent, and the post-2011 relocation-and-elevation program loses its justification. The postwar seaward expansion already demonstrates the direction of rearrangement whenever the boundary stops binding attention.
% FOUNDING_PROBLEM: After the 1896 Meiji Sanriku tsunami (roughly 22,000 dead) and again after 1933 (roughly 3,000 dead), survivor villages faced a transmission failure no individual memory could solve: the next damaging wave would arrive after everyone who remembered the last one was gone. They carved the observed waterline into stone with injunctions to build and live above it.
% FOUNDING_PROBLEM_CORROBORATION: Paleotsunami research (the 869 Jōgan deposit record) and national hazard-mapping programs attest, from entirely outside the stone-raising communities, that rare outsized tsunamis remain live and that living-memory transmission fails on exactly these timescales; the 2011 inundation itself corroborated the founders' observed line at numerous matched sites. Geological strata and government hazard registers carry the attestation, so no corroboration from the settlements that heeded the line is required for the problem statement to stand.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__catastrophe_validation_axis, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__catastrophe_validation_axis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tsunami_stone_commitment__catastrophe_validation_axis, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 0.06, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, ExtMetricName, E),
    domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is near-zero (0.06) because the referent — the standing hazard-line arrangement as adjudicated — collects rents from no one: the siting premium is a voluntary price paid for survival odds, and no seat receives what compliant settlers give up. Suppression is 0.03 because the arrangement prevents nothing; it warns. The water enforces ex post and requires no preventive apparatus, which is why no suppression_requirement series is authored — the enforcement picture is static, and that stability is already carried by the scalar. Theater (0.14 at endpoint) traces a full generational cycle on one shared ten-point grid: markers freshly functional at 1896 and re-vindicated at 1933 and 1960, then increasingly maintained as ritual while zoning ignored them through the high-growth decades (peak 0.53 in 2003), collapsing back to functional at the 2011 adjudication. The oscillation is partly the mechanism itself — each quiet decade deepens discounting, each event resets it — so the cycle is documented rather than smoothed away. Extraction ticks from 0.05 to 0.06 across the growth decades solely because rising waterfront land value raised the opportunity premium the line charges; the boundary itself never moved. Accessibility collapse is 0.85: once the boundary is credited, the alternative of settling safely inside the zone is physically closed, and the engineered reopenings (seawalls, elevated platforms) exist but at trillion-yen cost. Resistance is 0.04 because hydrodynamics cannot be argued with; the visible post-2011 friction attaches to relocation politics, not to the boundary. Claim and metrics are independent authored facts: the mountain claim states what this reading holds structurally true; the metrics state what is descriptively observable; the engine computes per-seat types from the structural data and owns any divergence.
 *
 * PERSPECTIVAL GAP:
 *   From the erector seat the stones are a completed duty awaiting adjudication; from the heeding seat they are a cheap insurance policy vindicated in 2011; from the developer seat the same inscriptions read as a brake on recovery economics that failed to stop the water wherever run-up exceeded the carvings; from the reconstruction-authority seat they are a legacy dataset to be restated in engineering units. Same object, four different constraint experiences — the engine computes the per-seat divergence from the power, horizon, exit, and scope atoms, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   No base-property beneficiary or victim declarations are authored because no actor collects from the boundary's operation: the physical line subsidizes no seat, and the 2011 losses transferred value to no one. Position therefore rides on stakeholder roles and the override table. Heeding settlements derive toward the beneficiary pole (roughly d 0.2): survival gain dominates the siting premium, and their payer secondary role keeps them from the extreme. Developers derive toward the target pole (roughly d 0.8): concentrated episodic loss with constrained exit. Reconstruction authorities sit mildly cost-bearing (roughly d 0.4): they administer and fund the line's modern restatement without collecting it. The single explicit override (powerless -> 0.55) covers the erector seat: the derivation chain is blind here because a natural-limit arrangement generates no victim declaration, and the erectors' position — founding costs sunk in labor and grief, payoff realized only by descendants, neither subsidized nor deliberately taxed — is invisible to beneficiary/victim derivation. The override records their near-symmetric, slightly target-leaning stance.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement invites a degraded-institution misread: weathered stones, annual cleaning ceremonies, shrinking readership — and the theater series obligingly spikes through the late Showa decades. But the 2011 adjudication separates performance from function: the marker's informational payload stayed true and load-bearing even while readership collapsed, which is the opposite of the inertial condition in which ceremony substitutes for a lost function. Mandatrophy therefore resolves as NOT resolved: the founding mandate — carry the observed line to descendants — was shown live by the very event most likely to have buried it. The mismatch consumer finds founding_problem_status=live paired with disappearance_verdict=world_rearranges, flagging no zombie; the theater excursion is explained as a memory-cycle phase, not as mandate death.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which constraint does this story classify: the catastrophe-validation reading of the tsunami-stone kernel, or the stone commitment under one of its sibling readings?',
    'Corpus-level cross-reading comparison: compile the sibling stories (behavioral competence, commemorative husk) and diff their epsilon values, victim structures, and computed types against this file; the delta is the reading index.',
    'Under the husk sibling the same stones classify with substantially higher extraction and a named misled-settler victim structure; under the competence sibling enforcement-based coordination enters the metric set. This file''s near-zero extraction is valid only for the validation-axis reading — epsilon is a property of the reading, not of the stones simpliciter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexicality: this story instantiates one reading of the tsunami_stone_commitment kernel, and its values do not transfer to sibling readings.').

omega_variable(
    validation_decisiveness_confounds,
    'Was the 2011 survival differential at stone-respecting settlements a decisive binary validation of the inscribed line, or an artifact of covariates (ground elevation, distance from river mouths, settlement scale, reclamation history) that would predict survival with no behavioral content?',
    'Matched-pair and regression designs over Sanriku settlements controlling for elevation, run-up distance, reclamation history, and population; robustness checks on districts where the stone line and modern hazard maps disagree.',
    'If confounded, the reading''s foundational axiom loses empirical warrant and the story degrades toward the husk sibling''s classification; if robust, the binary-validation claim stands and the adjudication-device role attributed to the physical line is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(validation_decisiveness_confounds, empirical, 'Whether the 2011 evidence is decisive validation or confound-driven correlation.').

omega_variable(
    instrument_adequacy_vs_megaquake,
    'Does the inscribed line constitute an adequate test instrument, given that 2011 run-up exceeded the 1896/1933 marks at numerous sites and paleotsunami work identifies a roughly thousand-year recurrence class the stones never recorded?',
    'Sediment-core and deposit-thickness reconstruction compared against stone elevations; distribution of exceedances across the marker network.',
    'If the instrument systematically undershoots the true hazard tail, part of the arrangement''s low extraction rests on false confidence — compliance above the stone line was insufficient, not merely costly — and the validation is partial rather than binary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrument_adequacy_vs_megaquake, empirical, 'The stone line as measuring instrument versus the full hazard tail.').

omega_variable(
    post_adjudication_memory_cycle,
    'Will the post-2011 revival persist through the coming quiet decades, or does the theater series'' generational cycle predict renewed discounting before the next event?',
    'Longitudinal monitoring of reconstruction siting compliance, marker maintenance budgets, and school-curriculum retention across the 2020s–2050s.',
    'Renewed decay would reproduce the pre-2011 theater spike and set up the same adjudication debt for the next generation; persistence would mark the cycle as broken by institutionalized transmission through hazard maps, museums, and relocated districts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_adjudication_memory_cycle, empirical, 'Whether the 2011 adjudication resets the generational forgetting cycle or merely restarts it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__catastrophe_validation_axis, 1896, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t1896, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1896, 0.05).
narrative_ontology:measurement_basis(tsun_tr_t1896, observed).
narrative_ontology:measurement(tsun_tr_t1915, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1915, 0.07).
narrative_ontology:measurement_basis(tsun_tr_t1915, observed).
narrative_ontology:measurement(tsun_tr_t1933, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1933, 0.09).
narrative_ontology:measurement_basis(tsun_tr_t1933, observed).
narrative_ontology:measurement(tsun_tr_t1947, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1947, 0.13).
narrative_ontology:measurement_basis(tsun_tr_t1947, observed).
narrative_ontology:measurement(tsun_tr_t1960, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1960, 0.18).
narrative_ontology:measurement_basis(tsun_tr_t1960, observed).
narrative_ontology:measurement(tsun_tr_t1972, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1972, 0.31).
narrative_ontology:measurement_basis(tsun_tr_t1972, observed).
narrative_ontology:measurement(tsun_tr_t1983, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1983, 0.42).
narrative_ontology:measurement_basis(tsun_tr_t1983, observed).
narrative_ontology:measurement(tsun_tr_t1994, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1994, 0.48).
narrative_ontology:measurement_basis(tsun_tr_t1994, observed).
narrative_ontology:measurement(tsun_tr_t2003, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 2003, 0.53).
narrative_ontology:measurement_basis(tsun_tr_t2003, observed).
narrative_ontology:measurement(tsun_tr_t2011, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 2011, 0.14).
narrative_ontology:measurement_basis(tsun_tr_t2011, observed).

% Extraction over time
narrative_ontology:measurement(tsun_be_t1896, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1896, 0.05).
narrative_ontology:measurement_basis(tsun_be_t1896, observed).
narrative_ontology:measurement(tsun_be_t1915, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1915, 0.05).
narrative_ontology:measurement_basis(tsun_be_t1915, observed).
narrative_ontology:measurement(tsun_be_t1933, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1933, 0.05).
narrative_ontology:measurement_basis(tsun_be_t1933, observed).
narrative_ontology:measurement(tsun_be_t1947, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1947, 0.05).
narrative_ontology:measurement_basis(tsun_be_t1947, observed).
narrative_ontology:measurement(tsun_be_t1960, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement_basis(tsun_be_t1960, observed).
narrative_ontology:measurement(tsun_be_t1972, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1972, 0.06).
narrative_ontology:measurement_basis(tsun_be_t1972, observed).
narrative_ontology:measurement(tsun_be_t1983, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1983, 0.06).
narrative_ontology:measurement_basis(tsun_be_t1983, observed).
narrative_ontology:measurement(tsun_be_t1994, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1994, 0.06).
narrative_ontology:measurement_basis(tsun_be_t1994, observed).
narrative_ontology:measurement(tsun_be_t2003, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 2003, 0.06).
narrative_ontology:measurement_basis(tsun_be_t2003, observed).
narrative_ontology:measurement(tsun_be_t2011, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 2011, 0.06).
narrative_ontology:measurement_basis(tsun_be_t2011, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(tsunami_stone_commitment__catastrophe_validation_axis, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__catastrophe_validation_axis, information_standard).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__behavioral_competence_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the tsunami stones' conflates three structurally distinct constraints sharing one kernel: (1) the physical hazard boundary as adjudication device — this file, a natural-limit structure with near-zero epsilon whose 2011 execution supplied the binary evidence; (2) the behavioral competence reading — the transmission regime that kept the injunction actionable, a coordination/enforcement story with its own epsilon; (3) the commemorative husk reading — the decayed-symbol account under which compliance was coincidental, carrying a misled-settler victim structure and materially higher epsilon. Decomposition follows the epsilon-invariance principle: measuring the stones as physical adjudicator yields one stable epsilon; measuring them as social norm or as dead letter yields others, so the label is split into linked files rather than averaged inside one. This file sits upstream: its adjudication output is the evidential input both siblings consume, and its foundational axiom directly contradicts the husk sibling's core premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tsunami_stone_commitment__catastrophe_validation_axis, powerless, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
