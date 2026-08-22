% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__sovereigntist_reading, []).

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
 *   constraint_id: paris_article_4_ndc__sovereigntist_reading
 *   human_readable: Paris Agreement Article 4 NDCs as Sovereign Self-Determined Pledges
 *   domain: international_climate_governance/treaty_law
 *
 * SUMMARY:
 *   This story instantiates the sovereigntist reading of the Paris Agreement
 *   Article 4 NDC kernel: NDCs as voluntary, nationally self-determined
 *   pledges whose central legal-political value is the preservation of
 *   national energy sovereignty and domestic control over development
 *   pathways. Under this reading the mechanism is a low-extraction
 *   coordination device — a common reporting and comparison format — rather
 *   than a binding obligation or an equity-differentiating instrument. The
 *   rising theater_ratio over the interval reflects that the
 *   pledge-and-review architecture increasingly performs the function of
 *   demonstrating international climate engagement while the substantive
 *   ambition ceiling remains domestically set and, in aggregate, insufficient
 *   relative to stated temperature goals — a drift the sovereigntist reading
 *   attributes to legitimate national prerogative rather than to enforcement
 *   failure.
 *
 * KEY AGENTS:
 *   - fossil_dependent_state_governments: primary agenda-setter and beneficiary (institutional/mobile) — sets and revises own pledge
 *   - national_development_planning_ministries: beneficiary (institutional/mobile) — sequences climate action behind development
 *   - extractive_sector_domestic_industries: beneficiary (organized/constrained) — protected from external phase-out compulsion
 *   - small_island_and_frontline_states: excluded (powerless/trapped) — bear aggregate risk without standing over others' pledges
 *   - unfccc_secretariat: analytical observer (institutional/analytical) — records and compares but does not adjudicate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__sovereigntist_reading, 0.18).
domain_priors:suppression_score(paris_article_4_ndc__sovereigntist_reading, 0.1).
domain_priors:theater_ratio(paris_article_4_ndc__sovereigntist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__sovereigntist_reading, rope).
narrative_ontology:human_readable(paris_article_4_ndc__sovereigntist_reading, "Paris Agreement Article 4 NDCs as Sovereign Self-Determined Pledges").
narrative_ontology:topic_domain(paris_article_4_ndc__sovereigntist_reading, "international_climate_governance/treaty_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__sovereigntist_reading, 'ea56199b-42cb-41b9-b6fd-bdbdec01d85c').
narrative_ontology:cs_kernel_codification('ea56199b-42cb-41b9-b6fd-bdbdec01d85c', fixed_text).
narrative_ontology:cs_authority_grounding('ea56199b-42cb-41b9-b6fd-bdbdec01d85c', distributed).
narrative_ontology:cs_reading_relation('ea56199b-42cb-41b9-b6fd-bdbdec01d85c', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea56199b-42cb-41b9-b6fd-bdbdec01d85c', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_axiom('ea56199b-42cb-41b9-b6fd-bdbdec01d85c', foundational, national_self_determination_of_mitigation_ambition).
narrative_ontology:cs_axiom_status(national_self_determination_of_mitigation_ambition, holdable).
narrative_ontology:cs_axiom_grounding('ea56199b-42cb-41b9-b6fd-bdbdec01d85c', national_self_determination_of_mitigation_ambition, conventional).
narrative_ontology:cs_axiom('ea56199b-42cb-41b9-b6fd-bdbdec01d85c', secondary, voluntary_participation_supersedes_binding_ratchet).
narrative_ontology:cs_axiom_status(voluntary_participation_supersedes_binding_ratchet, holdable).
narrative_ontology:cs_axiom_grounding('ea56199b-42cb-41b9-b6fd-bdbdec01d85c', voluntary_participation_supersedes_binding_ratchet, instrumental).
narrative_ontology:cs_reference_frame('ea56199b-42cb-41b9-b6fd-bdbdec01d85c', post_kyoto_voluntary_universalism).
narrative_ontology:cs_drift_state('ea56199b-42cb-41b9-b6fd-bdbdec01d85c', post_2023_global_stocktake, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('ea56199b-42cb-41b9-b6fd-bdbdec01d85c', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_state_governments).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, national_development_planning_ministries).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, extractive_sector_domestic_industries).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, national_sovereignty_over_energy_policy).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, bottom_up_pledge_and_review_architecture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and submit their own NDC target, choosing baseline year, scope, and ambition without external override. Can revise the pledge downward at any five-year cycle without formal penalty, and retain full domestic authority over energy mix, subsidy structure, and fossil development timelines. Treat the self-determined character of the pledge as the core value protected by the treaty design.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_state_governments, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_state_governments, beneficiary).

% Use NDC flexibility to sequence climate commitments behind industrialization, poverty reduction, and energy-access programs. The absence of binding external ratchets lets them frame emissions trajectories as subordinate to development planning rather than as an externally imposed ceiling.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, national_development_planning_ministries, beneficiary,
    institutional, generational, mobile, national).

% Coal, oil, and gas industries operating inside a state whose NDC was set with their continued operation in view. They benefit from the absence of a supranational enforcement body that could compel faster phase-out, and lobby domestically to keep the pledge modest at each revision cycle.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, extractive_sector_domestic_industries, beneficiary,
    organized, biographical, constrained, national).

% Argue within domestic politics for more ambitious targets but have no treaty-level standing to challenge a state's self-determined pledge; their only lever is domestic electoral or legal pressure, which the sovereigntist reading treats as the correct and sufficient check.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, domestic_climate_advocacy_groups, excluded,
    moderate, biographical, constrained, national).

% Face existential exposure to aggregate global emissions but hold no authority over any other state's NDC content or ambition level under this reading; their appeals for binding ratchets or differentiated obligations are treated, from this reading's own premises, as a different reading's argument rather than a claim on this constraint.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, small_island_and_frontline_states, excluded,
    powerless, generational, trapped, global).

% Compiles and publishes submitted NDCs, tracks aggregate ambition gaps, and administers the global stocktake, but under this reading has no authority to reject, modify, or compel revision of any state's pledge — its function is recording and comparison, not adjudication.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, unfccc_secretariat, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common reporting format and five-year cycle so that unilateral national pledges become mutually legible and comparable, allowing states to calibrate ambition against what peers are doing without ceding control over their own targets.
% TRANSFER_FUNCTION: Moves almost nothing coercively between parties: no binding finance, technology, or emissions-reduction obligation is transferred by the NDC mechanism itself under this reading. What is preserved, rather than transferred, is domestic control over energy-sector development pathways.
% ABSENT_VOICES: Small island states and frontline developing nations exposed to aggregate emissions have no standing under this reading to compel higher ambition from other parties; domestic advocacy groups inside fossil-dependent states are structurally confined to national political channels with no treaty-level recourse.
% DISAPPEARANCE_RATIONALE: If the NDC pledge-and-review architecture vanished overnight, fossil-dependent governments would lose a legitimating international frame for their existing domestic energy plans but would face little material constraint removal, since the reading holds the mechanism was never binding on them; supranational-reading proponents would say the world barely changes because enforcement was already absent; frontline states would say the diplomatic pressure channel — thin as it is — disappears entirely. The verdict differs by which reading is asked.
% FOUNDING_PROBLEM: Prior top-down binding-target architectures (Kyoto Protocol) collapsed under withdrawal (US 2001) and non-ratification by major emitters, and produced a legitimacy crisis where states with large populations and development needs refused externally imposed caps; Paris Article 4 was built to secure near-universal participation by letting every state set its own pledge.
% FOUNDING_PROBLEM_CORROBORATION: Fossil-dependent state negotiators and development ministries attest the founding problem (universal participation without externally imposed caps) remains live and correctly solved. Independent treaty-law scholars and IPCC-adjacent assessment bodies — outside the beneficiary set — attest that voluntary self-determination has produced an aggregate ambition gap inconsistent with the 1.5C/2C goal the same treaty states, i.e. that the founding problem of universal participation was solved at the cost of creating a new, unaddressed problem of aggregate insufficiency.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__sovereigntist_reading, contested).
narrative_ontology:founding_problem_status(paris_article_4_ndc__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__sovereigntist_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__sovereigntist_reading_tests).
:- end_tests(paris_article_4_ndc__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because, under this reading's own premises, no party is coerced into a target or penalized for revising one downward — the mechanism transfers almost nothing compulsorily. Suppression is low (0.10) for the same reason: there is no enforcement apparatus suppressing alternatives, since sovereignty over the pledge IS the declared design. Theater ratio is authored moderate-to-high and rising (0.35 to 0.55) because the review and stocktake apparatus performs an accountability function whose substantive teeth are limited by design — the performance of scrutiny grows even as the underlying compulsory content does not. Accessibility collapse is low (0.20) and resistance is low (0.15): states retain a live, exercised option to revise or under-deliver, and little active resistance is mounted against the sovereigntist architecture from within states that benefit from it.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil-dependent state governments and their domestic development and extractive-sector constituencies are coded as beneficiaries: the constraint's chief effect, on this reading, is to shield their domestic policy discretion from external override, so their directionality sits near the full-beneficiary end. No group is coded as a victim of THIS reading's own construction, because the reading's premise is precisely that no external party can compel a cost onto any state — costs borne by frontline states are, on this reading's own terms, a consequence of aggregate physical emissions, not of this treaty provision extracting from them. Frontline states and domestic advocacy groups are coded 'excluded' rather than 'payer' to reflect that the sovereigntist reading structurally denies them standing to contest another state's pledge, which is a different structural fact than being extracted from by this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing near-universal participation after the binding-target Kyoto architecture collapsed under non-ratification — is genuinely addressed by the voluntary pledge design and, on the sovereigntist reading, remains live: participation-without-coercion is still the operative value being protected. Classifying this reading as a low-epsilon rope (rather than importing the aggregate-insufficiency critique that properly belongs to the supranational and equity readings) prevents mislabeling a working coordination device as pure extraction; the rising theater_ratio is flagged honestly as a symptom worth tracking without forcing a snare or tangled_rope classification onto a reading whose own structural premises do not name a victim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereigntist_vs_supranational_framing_choice,
    'Is the correct reading of Article 4''s legal character genuinely the self-determined, non-binding pledge architecture this story assumes, or does the treaty''s ratchet mechanism and five-year review cycle in fact commit states to a binding trajectory that this reading understates?',
    'International Court of Justice or arbitral tribunal ruling on the binding character of NDC submission and revision obligations; alternatively, a state''s formal withdrawal or persistent non-submission tested against treaty consequences would reveal whether real enforcement exists.',
    'If the supranational reading''s binding-trajectory premise is legally correct, this story''s low epsilon and low suppression values understate the constraint''s actual coercive content, and the constraint would need to be reclassified toward tangled_rope or scaffold rather than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereigntist_vs_supranational_framing_choice, conceptual, 'Whether Article 4''s self-determination clause is genuinely non-binding or merely appears so pending stronger enforcement precedent.').

omega_variable(
    sovereignty_as_cover_for_extraction,
    'Does the sovereignty framing genuinely reflect a value the treaty design protects, or does it function as institutional cover allowing fossil-dependent governments and extractive industries to avoid costs that would otherwise be imposed by a binding architecture — making the declared beneficiaries here evidence of a false summit rather than a genuine mountain-adjacent coordination good?',
    'Compare aggregate NDC ambition trajectories against domestic fossil-subsidy and extraction-permitting trends in the same states; a strong negative correlation (ambition stagnates while extraction expands) would support the cover-story reading.',
    'If sovereignty framing is substantially cover, the claimed_type of rope is too generous and the constraint''s real operation is closer to tangled_rope, with frontline states functioning as unacknowledged victims rather than merely excluded parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_as_cover_for_extraction, empirical, 'Whether the sovereignty rationale is a genuine coordination value or an extraction cover story.').

omega_variable(
    aggregation_gap_naturalness,
    'Is the aggregate ambition gap (sum of NDCs falling short of 1.5C/2C pathways) a natural, expected feature of any voluntary bottom-up system, or is it a constructed outcome of specific drafting choices (no binding floor, no penalty for downward revision) that could have been designed differently within a still-voluntary framework?',
    'Comparative institutional analysis against other voluntary international regimes with stronger peer-review or naming-and-shaming penalty structures (e.g., trade policy review mechanisms) to see whether voluntariness necessarily implies this magnitude of gap.',
    'If the gap is a contingent drafting choice rather than an inherent feature of voluntary pledging, the theater_ratio drift documented here is better explained as a specific institutional design failure than as an unavoidable cost of preserving sovereignty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aggregation_gap_naturalness, conceptual, 'Whether the ambition gap is inherent to voluntary architecture or a contingent, correctable design choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__sovereigntist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pari_tr_t4, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 4, 0.42).
narrative_ontology:measurement(pari_tr_t8, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 8, 0.47).
narrative_ontology:measurement(pari_tr_t12, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement(pari_tr_t16, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 16, 0.53).
narrative_ontology:measurement(pari_tr_t20, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(pari_be_t4, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 4, 0.14).
narrative_ontology:measurement(pari_be_t8, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 8, 0.16).
narrative_ontology:measurement(pari_be_t12, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 12, 0.17).
narrative_ontology:measurement(pari_be_t16, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 16, 0.18).
narrative_ontology:measurement(pari_be_t20, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 20, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(paris_article_4_ndc__sovereigntist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__sovereigntist_reading, 0.08).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__equity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the paris_article_4_ndc kernel. paris_article_4_ndc__sovereigntist_reading (this file) authors low epsilon and treats sovereignty preservation as the coordination good; paris_article_4_ndc__supranational_reading authors higher epsilon and treats the ratchet mechanism as a binding trajectory requiring active enforcement; paris_article_4_ndc__equity_reading authors differentiated epsilon by development status, naming developing states as bearing disproportionate structural burden absent CBDR-consistent implementation. Each carries its own stakeholder set, ε, and claimed_type; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
