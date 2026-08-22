% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__commemorative_husk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__commemorative_husk, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: stone_land_use_rule__commemorative_husk
 *   human_readable: Tsunami-Warning Stone as Commemorative Husk (Land Use Decoupled)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   Along tsunami-prone coastlines, stones inscribed centuries ago mark the
 *   maximum inundation line reached by past catastrophic waves, with
 *   instructions not to build homes below that point. This story authors the
 *   'commemorative husk' reading of that kernel: the stone's original
 *   behavioral function — a lived, checked, daily-practiced land-use
 *   prohibition — has decayed into a heritage object. The line the stone
 *   marks is no longer treated by planning authorities, developers, or
 *   purchasers as an operative constraint on where structures may be built.
 *   Waterfront value has reasserted itself over the exposed zone; the stone
 *   is photographed, ceremonially maintained, and cited in tourism marketing,
 *   while permits are issued and condominiums rise below its line. This is a
 *   distinct constraint from the sibling 'behavioral_competence' reading,
 *   which holds that the stone still functions as a live prohibition enforced
 *   through daily spatial practice in some communities. The two readings are
 *   not the same constraint measured two ways — the behavioral_competence
 *   reading would score near-zero extraction (functioning coordination
 *   against real hazard); this reading scores extraction high because, by its
 *   own lights, the coordination function has failed and the arrangement has
 *   become a rent-generating heritage prop riding on top of accumulating risk
 *   transfer.
 *
 * KEY AGENTS:
 *   - coastal_resort_developers: Primary beneficiary (powerful/arbitrage) — builds and profits below the marked line at zero present cost
 *   - municipal_tourism_board: Secondary beneficiary (institutional/arbitrage) — monetizes the stone's symbolic value without maintaining its behavioral function
 *   - waterfront_condo_purchasers: Primary near-term payer (moderate/trapped) — commits capital to exposed land, often unaware of the stone's original meaning
 *   - future_tsunami_exposed_residents: Ultimate victim (powerless/trapped) — inherits accumulated physical risk with no voice in its creation
 *   - descendant_families_of_stone_erectors: Excluded voice (powerless/constrained) — retains literal understanding of the inscription, unheeded in planning process
 *   - disaster_anthropology_researchers: Analytical observer (analytical/analytical) — documents the compliance-decay pattern across coastal regions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.78).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.12).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.87).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.78).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.87).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, piton).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Tsunami-Warning Stone as Commemorative Husk (Land Use Decoupled)").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, 'b799c428-fe34-4c44-ab2c-5a65258475b3').
narrative_ontology:cs_kernel_codification('b799c428-fe34-4c44-ab2c-5a65258475b3', fixed_text).
narrative_ontology:cs_authority_grounding('b799c428-fe34-4c44-ab2c-5a65258475b3', practice).
narrative_ontology:cs_reading_relation('b799c428-fe34-4c44-ab2c-5a65258475b3', stone_land_use_rule__behavioral_competence, coexists_with).
narrative_ontology:cs_axiom('b799c428-fe34-4c44-ab2c-5a65258475b3', foundational, inscribed_warning_requires_living_practice_to_bind).
narrative_ontology:cs_axiom_status(inscribed_warning_requires_living_practice_to_bind, holdable).
narrative_ontology:cs_axiom_grounding('b799c428-fe34-4c44-ab2c-5a65258475b3', inscribed_warning_requires_living_practice_to_bind, empirically_contingent).
narrative_ontology:cs_axiom('b799c428-fe34-4c44-ab2c-5a65258475b3', secondary, heritage_commemoration_discharges_obligation_to_warn).
narrative_ontology:cs_axiom_status(heritage_commemoration_discharges_obligation_to_warn, holdable).
narrative_ontology:cs_axiom_grounding('b799c428-fe34-4c44-ab2c-5a65258475b3', heritage_commemoration_discharges_obligation_to_warn, conventional).
narrative_ontology:cs_reference_frame('b799c428-fe34-4c44-ab2c-5a65258475b3', stone_as_binding_intergenerational_prohibition).
narrative_ontology:cs_drift_state('b799c428-fe34-4c44-ab2c-5a65258475b3', contemporary_waterfront_development_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b799c428-fe34-4c44-ab2c-5a65258475b3', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, coastal_resort_developers).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, municipal_tourism_board).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, waterfront_condo_purchasers).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, future_tsunami_exposed_residents).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, ancestral_wisdom_was_once_actionable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build hotels, condominiums, and resort infrastructure below the stone's inscribed line because nothing legally or practically stops them. The stone is photographed for brochures as proof of the area's storied resilience while the land beneath the historical high-water mark is sold at premium waterfront prices. They incur zero cost from the stone's original warning.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, coastal_resort_developers, beneficiary,
    powerful, biographical, arbitrage, regional).

% Markets the stone as a heritage site and disaster-memory tourism draw. Funds a small annual ceremony and a plaque. Has no budget line, ordinance, or staff tasked with enforcing the boundary the stone marks; its interest in the stone is entirely representational.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, municipal_tourism_board, beneficiary,
    institutional, generational, arbitrage, regional).

% Buy or rent units below the stone's marked line, often without knowing the stone was ever a prohibition rather than a monument. Once purchased, their capital is committed to the site; they cannot cheaply relocate if the original warning's rationale reasserts itself in the next event.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, waterfront_condo_purchasers, payer,
    moderate, biographical, trapped, local).

% Have not yet arrived or been born, but will inherit whatever is built in the zone the stone once forbade. They bear the eventual physical risk without any voice in present-day permitting or purchase decisions.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, future_tsunami_exposed_residents, payer,
    powerless, generational, trapped, local).

% Some descendants of the communities that placed the original stones still live locally and understand the inscriptions as literal instruction, not folklore. They raise objections at planning meetings but hold no formal role in zoning or building permits and are treated as sentimental rather than authoritative.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, descendant_families_of_stone_erectors, excluded,
    powerless, generational, constrained, local).

% Issues building permits under modern zoning codes that make no reference to the stone's line. Could, in principle, adopt the stone's marked elevation as a hard setback but has not done so; treats the stone as a heritage object under a different department than the one that approves construction.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, municipal_planning_department, agenda_setter,
    institutional, biographical, constrained, regional).

% Document the gap between the stone's original behavioral function and its present ceremonial status, publishing comparative studies of tsunami-stone compliance decay across coastal regions. Have no power to alter zoning but generate the record other seats cite.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, disaster_anthropology_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__commemorative_husk, coastal_resort_developers).
narrative_ontology:fixing_cost_class(stone_land_use_rule__commemorative_husk, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None remaining in practice. Historically the stone coordinated a shared, legible boundary for where not to build, verified daily by residents living below it; the coordination problem it solved (transmitting hazard knowledge past living memory) is real, but this reading holds that the stone no longer performs it.
% TRANSFER_FUNCTION: Moves construction and habitation risk from present-day developers and buyers, who profit from waterfront proximity now, onto future residents and purchasers who will occupy the exposed zone when the next event occurs.
% ABSENT_VOICES: Descendant families who treat the inscription literally, and the not-yet-existing future residents who will occupy the zone the stone once forbade, have no seat in current permitting decisions; their objections are received as cultural sentiment, not planning input.
% DISAPPEARANCE_RATIONALE: If the physical stone were removed or destroyed tomorrow, building permits, land prices, and construction patterns in the zone would not change at all — under this reading, nothing in the present land-use system references the stone's line as an operative constraint. Only the tourism board's ceremony and the plaque's photo-op value would be lost.
% FOUNDING_PROBLEM: Communities historically inscribed maximum-inundation lines on stone markers, instructing descendants never to build below that line, because oral warnings alone did not reliably survive across generations following the last catastrophic tsunami.
% FOUNDING_PROBLEM_CORROBORATION: Disaster anthropology researchers, working from outside both the tourism board and the developers, corroborate through comparative fieldwork that the hazard the stones warned against remains geologically live and recurrent on multi-generational timescales; descendant families independently corroborate the same claim from oral tradition. Neither the tourism board nor the developers, who benefit from the stone's inert status, offer any corroboration of the problem's status one way or another — their silence on the matter is itself part of the record.
narrative_ontology:disappearance_verdict(stone_land_use_rule__commemorative_husk, world_unchanged).
narrative_ontology:founding_problem_status(stone_land_use_rule__commemorative_husk, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(stone_land_use_rule__commemorative_husk, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__commemorative_husk, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__commemorative_husk_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(stone_land_use_rule__commemorative_husk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises from 0.15 to 0.78 across the sixty-year interval as the theater ratio climbs from 0.10 to 0.87 on the same shared grid — the two series move together because the mechanism is the same: as fewer people alive remember the stone as instruction rather than monument, more of the activity around it (ceremonies, plaques, tourism framing) substitutes for the vanished behavioral constraint. Suppression is low (0.12) because nothing coercive is holding the arrangement in place — no one is stopped from building below the line, no one is punished for citing the stone's original meaning. This is not a snare; it is a piton. Accessibility collapse is low (0.20) because the alternative — actually treating the stone's line as a hard setback — remains fully available and cheap to adopt; nothing structural forecloses it, which is exactly why fixing_cost is authored as cheap. Resistance is moderate (0.35), carried entirely by descendant families and researchers rather than by any party with permitting power.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers and the tourism board sit at the beneficiary end: they extract present value (land price, tourism revenue) from a symbol whose behavioral cost has been suspended. Condo purchasers and future residents sit at the target end: purchasers commit trapped capital now, and future residents inherit compounding physical risk they had no part in creating. Descendant families are excluded rather than positioned as payers or beneficiaries in the market sense — their loss is epistemic and cultural, not yet financial, which is why they are marked excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as piton (rather than snare) turns on the absence of a concentrated beneficiary capturing extraction through active enforcement of the husk's status. No one is fighting to suppress the stone's original reading or to actively prevent a setback ordinance — the arrangement persists through pure administrative and cultural inertia: the planning department could adopt the line tomorrow at negligible institutional cost, and no party bears enough diffuse pain today to force the issue, since the deferred cost lands on people who do not yet exist. gain_flow is authored to coastal_resort_developers rather than 'diffuse' because the beneficiary is identifiable and concentrated even though enforcement is absent — this is the signature the framework flags for review: a piton with a locatable capturer sits close to the boundary with snare and should be periodically re-examined as the pattern matures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_competence_boundary,
    'At what point does a stone''s warning transition from a live, practiced land-use constraint (behavioral_competence reading) to a merely commemorative object (commemorative_husk reading) — is there an objective threshold, or is the transition only visible in retrospect after a failure event?',
    'Comparative ethnographic survey across multiple stone sites measuring actual construction density below the marked line as a proxy for behavioral compliance, tracked longitudinally; a sharp discontinuity in construction rates would support a threshold model, gradual decline would support a continuous-decay model.',
    'If a discontinuity model holds, individual communities could be scored and flagged before drift becomes irreversible; if continuous decay holds, no single ''crossing point'' exists and the two readings are better understood as endpoints of a spectrum, complicating the decomposition into two discrete constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_competence_boundary, conceptual, 'Whether husk vs. competence is a discrete threshold or continuous decay, and what that means for treating them as separate constraints.').

omega_variable(
    reversibility_of_husk_status,
    'Can a commemorative husk be reconverted into a behaviorally operative constraint (e.g., via a new zoning ordinance formally adopting the stone''s line), or does symbolic decay create irreversible path dependency through sunk waterfront investment?',
    'Case study of any coastal jurisdiction that has attempted to retroactively codify a historical hazard marker into binding zoning after a period of commemorative-only status; measure political and legal resistance encountered.',
    'If reversible at low cost, this supports the fixing_cost: cheap authoring and strengthens the piton (not snare) classification. If sunk investment and property-rights litigation make reversal effectively prohibitive once development has occurred, fixing_cost should be revisited toward prohibitive and the classification pressure shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_of_husk_status, empirical, 'Whether commemorative decay is reversible or creates path-dependent lock-in through development.').

omega_variable(
    natural_hazard_vs_constructed_neglect,
    'Is the extraction measured here properly attributed to a constructed institutional failure (planning department inaction, market incentives favoring waterfront value) or is some portion attributable to the genuinely irreducible difficulty of enforcing multi-generational hazard memory against economic pressure, which would be present in any similarly-aged warning system regardless of institutional design?',
    'Cross-cultural comparison of hazard-marker durability against institutional strength (measured by planning department budget, legal setback authority, land tenure security) to isolate whether stronger institutions show materially lower compliance decay.',
    'If institutional strength strongly predicts lower decay, extraction here is substantially attributable to correctable municipal_planning_department choices, not merely unavoidable erosion of memory. If institutional strength shows weak correlation, some portion of the 0.78 extractiveness reflects an inherent difficulty of intergenerational hazard transmission that would persist under any plausible policy regime.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_hazard_vs_constructed_neglect, empirical, 'How much of the measured extraction is attributable to correctable institutional neglect versus inherent difficulty of multi-generational hazard memory transmission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__commemorative_husk, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(ston_tr_t0, observed).
narrative_ontology:measurement(ston_tr_t10, stone_land_use_rule__commemorative_husk, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(ston_tr_t10, observed).
narrative_ontology:measurement(ston_tr_t20, stone_land_use_rule__commemorative_husk, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(ston_tr_t20, observed).
narrative_ontology:measurement(ston_tr_t30, stone_land_use_rule__commemorative_husk, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(ston_tr_t30, observed).
narrative_ontology:measurement(ston_tr_t40, stone_land_use_rule__commemorative_husk, theater_ratio, 40, 0.71).
narrative_ontology:measurement_basis(ston_tr_t40, observed).
narrative_ontology:measurement(ston_tr_t50, stone_land_use_rule__commemorative_husk, theater_ratio, 50, 0.81).
narrative_ontology:measurement_basis(ston_tr_t50, observed).
narrative_ontology:measurement(ston_tr_t60, stone_land_use_rule__commemorative_husk, theater_ratio, 60, 0.87).
narrative_ontology:measurement_basis(ston_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(ston_be_t0, observed).
narrative_ontology:measurement(ston_be_t10, stone_land_use_rule__commemorative_husk, base_extractiveness, 10, 0.28).
narrative_ontology:measurement_basis(ston_be_t10, observed).
narrative_ontology:measurement(ston_be_t20, stone_land_use_rule__commemorative_husk, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(ston_be_t20, observed).
narrative_ontology:measurement(ston_be_t30, stone_land_use_rule__commemorative_husk, base_extractiveness, 30, 0.56).
narrative_ontology:measurement_basis(ston_be_t30, observed).
narrative_ontology:measurement(ston_be_t40, stone_land_use_rule__commemorative_husk, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(ston_be_t40, observed).
narrative_ontology:measurement(ston_be_t50, stone_land_use_rule__commemorative_husk, base_extractiveness, 50, 0.73).
narrative_ontology:measurement_basis(ston_be_t50, observed).
narrative_ontology:measurement(ston_be_t60, stone_land_use_rule__commemorative_husk, base_extractiveness, 60, 0.78).
narrative_ontology:measurement_basis(ston_be_t60, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(stone_land_use_rule__commemorative_husk, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, information_standard).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__commemorative_husk, 0.02).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, stone_land_use_rule__behavioral_competence).

% DUAL FORMULATION NOTE:
% This story and stone_land_use_rule__behavioral_competence decompose one colloquial concept ('the tsunami stone's warning') into two structurally distinct constraints per the epsilon-invariance principle. The husk reading (this file) authors near-zero coordination function and high, rising extraction (0.78 at interval end) because the behavioral mechanism has failed in this instance. The competence reading authors near-zero extraction and a genuine, actively practiced coordination function. They are linked rather than merged because measuring 'the stone's warning' by whether construction below the line still occurs yields two incompatible epsilon values depending on which community's stone is observed — that divergence is the signal that two constraints exist, not one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
