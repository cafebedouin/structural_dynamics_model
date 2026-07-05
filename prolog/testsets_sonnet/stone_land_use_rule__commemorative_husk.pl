% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__commemorative_husk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Tsunami Stone as Memorial Object — Warning Function Atrophied to Ceremony
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   This story is the 'commemorative husk' reading of the stone land-use
 *   kernel: the constraint as it operates once the marked boundary has
 *   decayed from a lived, daily-enforced prohibition into a monument that
 *   carries no behavioral force. Under this reading, building decisions are
 *   structurally independent of the stone's location — developers, permitting
 *   bodies, and buyers treat it as heritage furniture rather than a
 *   constraint on land use. This is a distinct constraint from the sibling
 *   reading (behavioral_competence), where the same stone still functions as
 *   a live prohibition enforced through daily spatial practice. The two
 *   readings do not average into one ε — they are different structural facts
 *   about different communities (or the same community at different
 *   historical moments), and this file describes only the husk state: high
 *   extractiveness, near-zero suppression, and a theater ratio that rises
 *   toward the interval's end as commemorative activity (plaques, festivals,
 *   heritage tourism) intensifies precisely as behavioral compliance
 *   collapses.
 *
 * KEY AGENTS:
 *   - waterfront_developers: beneficiary (organized/arbitrage) — build below the marked line without consequence
 *   - tourism_boards: beneficiary (organized/mobile) — profit from the stone's symbolic charge, not its function
 *   - municipal_land_registries: agenda_setter (institutional/constrained) — could codify the boundary into zoning but has not
 *   - future_coastal_residents: payer (powerless/trapped) — inherit tsunami exposure the stone was built to prevent
 *   - descendants_of_stone_setters: excluded (powerless/identity_locked) — the moral authors of the warning, with no procedural voice
 *   - disaster_anthropologists: observer (analytical) — document the decay pattern across sites
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.71).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.12).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.86).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.71).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.86).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, piton).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Tsunami Stone as Memorial Object — Warning Function Atrophied to Ceremony").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, '5bb6c3e8-5f65-454c-a522-03e7e4a33743').
narrative_ontology:cs_kernel_codification('5bb6c3e8-5f65-454c-a522-03e7e4a33743', implicit).
narrative_ontology:cs_authority_grounding('5bb6c3e8-5f65-454c-a522-03e7e4a33743', practice).
narrative_ontology:cs_reading_relation('5bb6c3e8-5f65-454c-a522-03e7e4a33743', stone_land_use_rule__behavioral_competence, coexists_with).
narrative_ontology:cs_axiom('5bb6c3e8-5f65-454c-a522-03e7e4a33743', foundational, commemoration_discharges_the_warning_obligation).
narrative_ontology:cs_axiom_status(commemoration_discharges_the_warning_obligation, holdable).
narrative_ontology:cs_axiom_grounding('5bb6c3e8-5f65-454c-a522-03e7e4a33743', commemoration_discharges_the_warning_obligation, conventional).
narrative_ontology:cs_axiom('5bb6c3e8-5f65-454c-a522-03e7e4a33743', secondary, symbolic_preservation_satisfies_ancestral_duty).
narrative_ontology:cs_axiom_status(symbolic_preservation_satisfies_ancestral_duty, holdable).
narrative_ontology:cs_axiom_grounding('5bb6c3e8-5f65-454c-a522-03e7e4a33743', symbolic_preservation_satisfies_ancestral_duty, conventional).
narrative_ontology:cs_reference_frame('5bb6c3e8-5f65-454c-a522-03e7e4a33743', post_tsunami_survivor_inscription_era).
narrative_ontology:cs_drift_state('5bb6c3e8-5f65-454c-a522-03e7e4a33743', contemporary_waterfront_development_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('5bb6c3e8-5f65-454c-a522-03e7e4a33743', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, waterfront_developers).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, tourism_boards).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, municipal_land_registries).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, future_coastal_residents).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, descendants_of_stone_setters).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, ancestral_wisdom_narrative).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, community_disaster_memory_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build housing, hotels, and commercial property below the stone's marked line because the prohibition carries no zoning force, no permitting check, and no insurance consequence. The stone's presence is cited in marketing copy as evidence of community resilience even as its actual boundary is ignored in site selection.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, waterfront_developers, beneficiary,
    organized, biographical, arbitrage, local).

% Promote the stone as a heritage site and photo destination — a symbol of the community's disaster memory. Their interest is in the stone remaining visible and narratively potent, not in its message being followed; a stone that actually stopped construction would remove the waterfront inventory tourism depends on.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, tourism_boards, beneficiary,
    organized, biographical, mobile, regional).

% Administer zoning and building permits for the coastal zone. They could encode the stone's line into binding setback law but have not; maintaining the stone as unenforced tradition avoids the political and fiscal cost of down-zoning valuable waterfront parcels, while still allowing the registry to point to the stone during disaster-preparedness audits.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, municipal_land_registries, agenda_setter,
    institutional, generational, constrained, local).

% Will occupy homes and businesses built below the historical high-water line, having no way to know at purchase time that the stone once marked a lived prohibition rather than a plaque. They bear the eventual tsunami exposure that the stone was erected to prevent, without having consented to or even perceived the substitution of symbol for rule.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, future_coastal_residents, payer,
    powerless, generational, trapped, local).

% Some are direct descendants of the survivors who placed the stones after historical tsunamis, inscribing explicit instructions not to build below that point. They sometimes object publicly when construction proceeds near a stone, but have no standing in the permitting process and are treated as sentimental rather than authoritative voices.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, descendants_of_stone_setters, excluded,
    powerless, civilizational, identity_locked, local).

% Study the stones as a case of institutional memory decay — comparing villages where the marked line still governs construction against villages where it has become a monument. They document the transition from behavioral rule to commemorative object across generations.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, disaster_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None remains active. The stones originally coordinated a genuine collective-action problem — preventing generational amnesia about where past tsunamis reached — by embedding the boundary in a durable, publicly legible object rather than relying on oral transmission alone.
% TRANSFER_FUNCTION: Moves exposure to catastrophic tsunami risk from the present generation, which benefits from waterfront land value and tourism revenue, onto future residents who inherit the physical location without inheriting the warning's original force.
% ABSENT_VOICES: Descendants of the stone-setters and disaster survivors who explicitly inscribed the prohibition are treated as heritage custodians, not zoning authorities; their objections are recorded in local histories and occasionally in town-hall testimony but carry no procedural weight in permitting decisions.
% DISAPPEARANCE_RATIONALE: If the stone were removed tomorrow, building patterns, permitting practice, and land values would not shift — construction below the marked line is already proceeding as though the stone did not exist. The stone's disappearance would only remove a photo opportunity and a line in disaster-preparedness reports; no live arrangement depends on its behavioral force because that force is already gone.
% FOUNDING_PROBLEM: After a historical tsunami devastated the coast, survivors set stones at the maximum observed water line with inscriptions instructing descendants never to build homes below that point — solving the problem of transmitting lethal spatial knowledge across generations that would not personally remember the event.
% FOUNDING_PROBLEM_CORROBORATION: Disaster anthropologists studying comparable marker systems across multiple tsunami-affected regions attest, from outside any local benefiting party, that the underlying hazard the stones warn against remains fully live — sea-level and subduction-zone risk have not diminished. The founding problem's persistence is corroborated by seismological and hydrological hazard mapping, independent of the stone-setters' descendants or the tourism boards who now narrate the stone as resolved history.
narrative_ontology:disappearance_verdict(stone_land_use_rule__commemorative_husk, world_unchanged).
narrative_ontology:founding_problem_status(stone_land_use_rule__commemorative_husk, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(stone_land_use_rule__commemorative_husk, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__commemorative_husk, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.71) because the constraint, in this reading, transfers real catastrophic risk from present economic beneficiaries to future powerless residents while providing zero corresponding coordination benefit — the coordination function that would justify the transfer (actual setback enforcement) is precisely what has atrophied. Suppression is authored low (0.12): no one is coerced into building below the line or above it; the low suppression is the diagnostic signature of the husk reading, distinguishing it sharply from the sibling reading where suppression-like social enforcement (shunning noncompliant builders, communal monitoring) would be higher. Theater ratio is authored very high (0.86) and rising across the measurement series — commemorative activity (plaques, annual remembrance walks, heritage-tourism signage) increases even as the underlying behavioral compliance it ostensibly memorializes has already collapsed; this is the classic piton signature of performative maintenance replacing function. Accessibility collapse is low (0.2) because alternatives to ignoring the stone (i.e., actually not building below it) remain fully available and even actively chosen by the excluded stone-descendants — nothing about the constraint's operation forecloses compliance, it has simply stopped being practiced. Resistance is low (0.15): there is little organized resistance to the drift because the beneficiaries of ignoring the stone are exactly the organized, well-resourced actors (developers, tourism boards, the registry itself), while the parties who would resist (descendants, future residents) are diffuse, powerless, or not yet born.
 *
 * PERSPECTIVAL GAP:
 *   From the municipal registry's seat, the stone remains a symbol of responsible disaster preparedness — cited in reports, undisturbed, low political cost. From a future resident's seat (projected forward), the same object will retrospectively appear as a warning that was visible, legible, and ignored — a preventable catastrophe rather than an unforeseeable one. The engine computing these as different seat-classifications is the point: the registry's seat may compute closer to inert/mountain-adjacent (nothing to defend, nothing extracted) while the future-resident seat computes as heavily extractive, because the same structural data reads oppositely depending on who is asked to carry the eventual cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Waterfront developers and tourism boards sit near the full-beneficiary end: they extract economic value from waterfront proximity while the stone's continued visibility supplies a costless resilience narrative. The municipal land registry is a beneficiary-adjacent agenda-setter — it could change the constraint's force by codifying the setback into binding law, but bears none of the future cost of not doing so, and gains present political and fiscal ease from leaving it symbolic. Future coastal residents are the clearest targets: trapped by the fact that they cannot yet act to prevent their own future exposure, and by the fact that the property they will buy carries no disclosure that the marker once meant something operative. Descendants of the stone-setters are identity-locked targets in a different register — their identity as custodians of ancestral warning is itself devalued by the husk transition, and they cannot exit the role without abandoning the memory work entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as ongoing coordination (which would credit the stone with a protective function it no longer performs) and equally prevents mislabeling it as active predatory extraction requiring enforcement (there is no enforcer coercing anyone — the drift is closer to institutional inertia than to a designed extraction scheme). Piton captures the structure precisely: a former Rope (genuine coordination — durable multigenerational hazard transmission) whose function has atrophied, leaving behind theatrical maintenance (heritage tourism, commemorative ceremony) with no concentrated profiteer running the constraint as a business. The beneficiaries here (developers, tourism boards) do not administer the constraint or extract rents FROM it directly — they simply benefit from its absence of force, which is the piton signature: no party profits enough from actively maintaining the constraint to be a snare's agenda-setter, and no party is hurt enough YET (future residents are not yet resident) to force a fix.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_transition_timing,
    'At what point did this specific stone''s community transition from behavioral_competence (live enforcement) to commemorative_husk (symbolic decay), and was the transition gradual or triggered by a discrete event (e.g., a generational turnover, a land boom, an administrative reclassification)?',
    'Comparative historical land-registry analysis: cross-reference permitting records near the stone against the stone''s own inscription date and against oral-history interviews with descendants to identify when construction below the line first began without incident or objection.',
    'If the transition is discrete and traceable to a specific administrative decision (e.g., a rezoning vote), that decision itself becomes a superior locus for classification — the husk state may be better modeled as a downstream effect of an identifiable tangled_rope (the rezoning) rather than a standalone piton. If the transition is genuinely gradual and untraceable to any decision point, piton is the more accurate frame: inertial decay with no responsible agenda-setter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_transition_timing, empirical, 'Whether the husk state has a traceable causal decision point or is genuine gradual institutional drift.').

omega_variable(
    kernel_reading_boundary_location,
    'Is the behavioral_competence / commemorative_husk split a fact about DIFFERENT villages (some communities maintain the practice, others let it lapse) or a fact about the SAME community across TIME (all communities eventually drift from competence to husk as direct survivor memory dies out)?',
    'Cross-site comparative survey of tsunami-stone communities in Japan and comparable marker systems elsewhere, coded by time-since-founding-event and by presence/absence of surviving first-generation witnesses.',
    'If the split is cross-sectional (some villages remain in behavioral_competence indefinitely), the husk reading describes a subset of an otherwise-stable population and the FSM-style beneficiary framing is fully warranted as a distinct constraint. If the split is temporal and universal (every marker eventually decays to husk once direct witnesses die), the two readings are better modeled as sequential PHASES of one lifecycle than as independently persisting sibling constraints — which would argue for a stronger network link and possibly a lifecycle-transition annotation rather than pure sibling coexistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the two kernel readings are cross-sectional siblings or sequential lifecycle phases of the same underlying object.').

omega_variable(
    future_resident_disclosure_gap,
    'Do property transactions in the marked zone carry any legal disclosure obligation referencing the stone''s historical prohibition, and if not, does that silence itself constitute a form of suppression (informational rather than coercive)?',
    'Review of local real-estate disclosure law and title-transfer documentation for parcels within the historically marked zone; survey recent buyers on whether they were aware of the stone''s original meaning at time of purchase.',
    'If disclosure is absent and buyers are systematically unaware, the authored suppression value (0.12) may understate an informational suppression component distinct from the low behavioral/coercive suppression captured here — this would argue for treating informational opacity as a partially independent axis in a future revision, without changing the already-low coercive suppression score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_resident_disclosure_gap, empirical, 'Whether the low suppression score is masking an informational-disclosure gap distinct from coercive suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__commemorative_husk, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(ston_tr_t0, observed).
narrative_ontology:measurement(ston_tr_t10, stone_land_use_rule__commemorative_husk, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(ston_tr_t10, observed).
narrative_ontology:measurement(ston_tr_t20, stone_land_use_rule__commemorative_husk, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(ston_tr_t20, observed).
narrative_ontology:measurement(ston_tr_t30, stone_land_use_rule__commemorative_husk, theater_ratio, 30, 0.52).
narrative_ontology:measurement_basis(ston_tr_t30, observed).
narrative_ontology:measurement(ston_tr_t40, stone_land_use_rule__commemorative_husk, theater_ratio, 40, 0.67).
narrative_ontology:measurement_basis(ston_tr_t40, observed).
narrative_ontology:measurement(ston_tr_t50, stone_land_use_rule__commemorative_husk, theater_ratio, 50, 0.79).
narrative_ontology:measurement_basis(ston_tr_t50, observed).
narrative_ontology:measurement(ston_tr_t60, stone_land_use_rule__commemorative_husk, theater_ratio, 60, 0.86).
narrative_ontology:measurement_basis(ston_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(ston_be_t0, observed).
narrative_ontology:measurement(ston_be_t10, stone_land_use_rule__commemorative_husk, base_extractiveness, 10, 0.18).
narrative_ontology:measurement_basis(ston_be_t10, observed).
narrative_ontology:measurement(ston_be_t20, stone_land_use_rule__commemorative_husk, base_extractiveness, 20, 0.32).
narrative_ontology:measurement_basis(ston_be_t20, observed).
narrative_ontology:measurement(ston_be_t30, stone_land_use_rule__commemorative_husk, base_extractiveness, 30, 0.47).
narrative_ontology:measurement_basis(ston_be_t30, observed).
narrative_ontology:measurement(ston_be_t40, stone_land_use_rule__commemorative_husk, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(ston_be_t40, observed).
narrative_ontology:measurement(ston_be_t50, stone_land_use_rule__commemorative_husk, base_extractiveness, 50, 0.66).
narrative_ontology:measurement_basis(ston_be_t50, observed).
narrative_ontology:measurement(ston_be_t60, stone_land_use_rule__commemorative_husk, base_extractiveness, 60, 0.71).
narrative_ontology:measurement_basis(ston_be_t60, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(stone_land_use_rule__commemorative_husk, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, identity_coordination).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__commemorative_husk, 0.05).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, stone_land_use_rule__behavioral_competence).

% DUAL FORMULATION NOTE:
% This file and stone_land_use_rule__behavioral_competence are the two declared readings of the shared stone_land_use_rule kernel. They are linked rather than merged because their ε values, suppression profiles, and theater ratios differ structurally, not merely observationally: behavioral_competence describes a community where the marked boundary still governs daily building practice (low extractiveness, moderate social suppression, low theater), while commemorative_husk describes a community where the same object has decayed into monument (high extractiveness, near-zero suppression, high and rising theater). Per the ε-invariance principle, these are two constraints sharing one physical kernel, not one constraint measured two ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
