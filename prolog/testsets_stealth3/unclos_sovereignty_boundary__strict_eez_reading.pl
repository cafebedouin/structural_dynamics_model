% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__strict_eez_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__strict_eez_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: Strict EEZ Exclusivity Regime (UNCLOS Article 57 Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   Under the strict reading of the Law of the Sea convention, every coastal
 *   state holds exclusive rights to the living and non-living resources of a
 *   200-nautical-mile belt measured from agreed baselines, and no claim
 *   predicated on historic usage, mid-ocean feature occupation, or
 *   alternative entitlement construction carries validity inside another
 *   state's zone. The arrangement solves a real collective-action problem —
 *   it converted a widening free-for-all of unilateral claims into delimited,
 *   administrable zones — while simultaneously transferring control of
 *   immense resource wealth to whoever sits on the relevant coastline,
 *   regardless of need, history, or investment. Enforcement is continuous and
 *   material: patrols, licensing regimes, seizures, and adjudication all
 *   operate constantly to hold the exclusivity in place against both resource
 *   intruders and states that reject the exclusivity premise itself. KEY
 *   AGENTS (by structural relationship): - coastal_eez_states: Primary
 *   beneficiary-administrator (institutional/constrained) — administers
 *   zones, collects rents, bound by the treaty that founds their claims -
 *   small_island_developing_states: Concentrated beneficiary
 *   (organized/constrained) — zone revenues dominate budgets, no physical
 *   enforcement capacity - overlapping_maritime_claimants: Primary target
 *   (powerful/trapped) — asserted entitlements voided inside neighbors'
 *   zones, fixed by geography - distant_water_fishing_fleets: Secondary
 *   target (organized/mobile) — historic grounds enclosed, capital can
 *   relocate - landlocked_geographically_disadvantaged_states: Structural
 *   loser (moderate/trapped) — residual paper rights dependent on neighbor
 *   goodwill - historic_access_fishing_communities: Excluded voice
 *   (powerless/trapped) — cross-boundary grounds closed, no seat in the
 *   regime - maritime_delimitation_tribunals: Analytical observer
 *   (institutional/analytical) — adjudicates and defines what counts as valid
 *   entitlement
 *
 * KEY AGENTS:
 *   - - coastal_eez_states: Primary beneficiary-administrator (institutional/constrained) — bears administration cost, collects exclusive resource control
 *   - - small_island_developing_states: Concentrated beneficiary (organized/constrained) — collects zone rents without enforcement capacity
 *   - - overlapping_maritime_claimants: Primary target (powerful/trapped) — loses asserted access, cannot relocate the overlap
 *   - - distant_water_fishing_fleets: Secondary target (organized/mobile) — pays license walls, retains fleet mobility
 *   - - landlocked_geographically_disadvantaged_states: Structural loser (moderate/trapped) — bears exclusion with residual paper rights
 *   - - historic_access_fishing_communities: Excluded voice (powerless/trapped) — bore enclosure with no representation
 *   - - maritime_delimitation_tribunals: Analytical observer (institutional/analytical) — sees the full structure, adjudicates it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.62).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.7).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "Strict EEZ Exclusivity Regime (UNCLOS Article 57 Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, 'f647c98a-9d4d-43ee-a416-fdad9686b907').
narrative_ontology:cs_kernel_codification('f647c98a-9d4d-43ee-a416-fdad9686b907', fixed_text).
narrative_ontology:cs_authority_grounding('f647c98a-9d4d-43ee-a416-fdad9686b907', lineage).
narrative_ontology:cs_interpretation_layer_present('f647c98a-9d4d-43ee-a416-fdad9686b907').
narrative_ontology:cs_reading_relation('f647c98a-9d4d-43ee-a416-fdad9686b907', unclos_sovereignty_boundary__historical_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('f647c98a-9d4d-43ee-a416-fdad9686b907', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, influences).
narrative_ontology:cs_axiom('f647c98a-9d4d-43ee-a416-fdad9686b907', foundational, treaty_delimitation_supersedes_historic_usage).
narrative_ontology:cs_axiom_status(treaty_delimitation_supersedes_historic_usage, holdable).
narrative_ontology:cs_axiom_grounding('f647c98a-9d4d-43ee-a416-fdad9686b907', treaty_delimitation_supersedes_historic_usage, conventional).
narrative_ontology:cs_axiom('f647c98a-9d4d-43ee-a416-fdad9686b907', secondary, zones_require_agreement_or_equity).
narrative_ontology:cs_axiom_status(zones_require_agreement_or_equity, holdable).
narrative_ontology:cs_axiom_grounding('f647c98a-9d4d-43ee-a416-fdad9686b907', zones_require_agreement_or_equity, conventional).
narrative_ontology:cs_reference_frame('f647c98a-9d4d-43ee-a416-fdad9686b907', article_57_treaty_delimitation).
narrative_ontology:cs_drift_state('f647c98a-9d4d-43ee-a416-fdad9686b907', contemporary_post_arbitration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f647c98a-9d4d-43ee-a416-fdad9686b907', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, coastal_eez_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, small_island_developing_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_maritime_claimants).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_fleets).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, landlocked_geographically_disadvantaged_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, historic_access_fishing_communities).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__strict_eez_reading, unclos_article_57_exclusivity_doctrine).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__strict_eez_reading, equidistance_equitable_delimitation_practice).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__strict_eez_reading, treaty_precedence_over_historic_usage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ratified the convention and administer 200-nautical-mile zones off their coasts: issue fishing and drilling licenses, run patrol and surveillance programs, prosecute intrusions, and negotiate delimitation with neighbors. License fees, royalties, and resource rents flow to their treasuries. Departing the convention would strip their own zone claims of their legal foundation, so exit is available on paper but ruinous in practice even where specific rules chafe.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, coastal_eez_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, coastal_eez_states, beneficiary).

% Micro-states whose maritime zones dwarf their land area; access-agreement fees, tuna licensing, and seabed royalties fund large shares of government budgets. They coordinate as a bloc in convention bodies and in regional fisheries arrangements. They possess no naval capacity to defend their zones physically and depend on the legal framework holding and on partner-state enforcement cooperation; their entire fiscal model rides on the exclusivity remaining valid.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, small_island_developing_states, beneficiary,
    organized, generational, constrained, regional).

% States whose asserted maritime entitlements overlap neighbors' 200-mile zones through historic-use claims, mid-ocean feature claims, or expansive baseline constructions. Inside the neighbor's zone their overlay assertions carry no validity under the strict reading. Geography fixes them against the overlap: they cannot move away from it, so their live options are concession, prolonged litigation, or sustained physical presence calibrated to stay short of open war.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, overlapping_maritime_claimants, payer,
    powerful, generational, trapped, regional).

% Industrial fleets from states without nearby surplus stocks that once worked waters now enclosed by coastal zones. They buy access licenses where offered, shift effort to remaining high-seas grounds or other states' zones, or risk seizure and fines. Capital mobility gives them more exit than any shore-bound actor, but every new enclosure raises operating costs somewhere else in the system.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_fleets, payer,
    organized, biographical, mobile, global).

% States with no coastline or only marginal coastal access. They hold formal rights to participate in neighbors' surplus living resources and a revenue share from shelf areas beyond 200 miles, but exercising either depends on neighbor goodwill and domestic capacity they often lack. They traded away any claim to enclosed waters in exchange for navigation guarantees and these residual rights; geography offers no exit from the bargain.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, landlocked_geographically_disadvantaged_states, payer,
    moderate, generational, trapped, regional).

% Shore communities whose seasonal grounds crossed what are now several states' zones. Access ended when the lines closed; they hold no seat in convention bodies and surface only as domestic compensation cases or cultural-loss testimony. Their objection to the enclosure lives in oral tradition and displaced-fleet registries rather than anywhere in the treaty process that drew the lines.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, historic_access_fishing_communities, excluded,
    powerless, biographical, trapped, regional).

% International courts and arbitral panels that adjudicate boundary disputes and interpret the convention's zone rules. They take cases from consenting parties, publish reasoned rulings that define what counts as valid entitlement, and depend entirely on state compliance rather than coercive power. Their rulings reshape the practical meaning of the 200-mile rule without administering any part of it.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, maritime_delimitation_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__strict_eez_reading, coastal_eez_states).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__strict_eez_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes offshore resource jurisdiction: one 200-nautical-mile measure from agreed baselines converts a proliferating mass of unilateral claims into delimited, administrable zones; enables stock assessment, licensing, hydrocarbon development, and enforcement under known rules; and removes the most common immediate casus belli between adjacent states.
% TRANSFER_FUNCTION: Moves exclusive control of fisheries, hydrocarbons, and seabed minerals within 200 miles of a coast from open access (and from historically present users) to the adjacent coastal state; moves license fees, compliance costs, and foreclosed access value from foreign and excluded users to coastal treasuries.
% ABSENT_VOICES: Historic-access fishing communities, indigenous maritime peoples, and non-state resource users had no seats at the Law of the Sea conference, which admitted states only; landlocked delegations attended but were outnumbered by the coastal-state bloc; future generations bearing stock depletion under licensed intensive fishing were unrepresented. They remain outside the amendment machinery, which runs exclusively through state parties.
% DISAPPEARANCE_RATIONALE: If exclusive-zone limits vanished overnight, adjacent waters would revert to contested open access: naval standoffs over fishing grounds and hydrocarbon fields would multiply, coastal states would race to convert legal claims into physical garrisons, license-revenue-dependent island budgets would collapse, and distant-water fleets would surge into former zones until stocks crashed or force intervened. Arrangements across food supply, energy development, and naval posture depend on the lines holding.
% FOUNDING_PROBLEM: Mid-century technology extended fishing and drilling far beyond territorial seas while coastal states unilaterally claimed ever-wider belts; the founding problem was allocating offshore resources among states — and preventing an armed free-for-all — before extraction capacity outran any legal order.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: landlocked and distant-water state delegations accepted the zone concept in exchange for navigation guarantees (conference records show the trade, not beneficiary self-attestation); shipping states comply with zone limits they gain no resource rent from; and the regime's dispute bodies remain continuously engaged — behavior consistent with a still-live allocation problem rather than a solved one. No reliance is placed on coastal-state assertions of the problem's liveness.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__strict_eez_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__strict_eez_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 because the regime transfers control of fisheries, hydrocarbons, and seabed minerals to coastal states in proportion to geography alone — a rock generates the same 200-mile entitlement as a continent — while excluded users receive license fees charged back to them or nothing at all. The figure is well below snare territory because the same structure delivers broad, genuinely valued goods: delimited boundaries, stock-management units, investment security for offshore development, and a large reduction in interstate conflict over adjacent waters. Suppression is authored at 0.70 as a raw, unscaled structural property: holding exclusivity requires continuous patrol, prosecution, VMS monitoring, hot-pursuit agreements, and legal proceedings against both intruders and states asserting that the exclusivity premise does not govern. Theater ratio is low (0.20): the regime's activity is overwhelmingly functional (licenses issued, boundaries drawn, cases adjudicated), with a modest performative component — zones proclaimed but never policed, marine protected areas declared without enforcement, symbolic presence operations. Accessibility collapse is 0.60: within the treaty framework, alternatives collapse sharply (a ratifying party cannot consistently hold overlay claims), but non-party practice and customary-law argumentation keep partial alternatives alive outside the framework. Resistance is 0.55: sustained litigation, refusal to delimit, gray-zone presence operations, and non-ratification are real and ongoing but stop short of systemic rejection. The claimed type (tangled_rope) is stated from the structure — genuine coordination function plus asymmetric extraction plus enforcement dependence — independently of these metric values; the engine computes per-seat types from the data. All three temporal series run on one shared seven-point grid (1982–2024) so no metric is sampled against another metric's end-state value.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the small-island beneficiary seat the arrangement is close to pure subsidy: zone revenue arrives without enforcement burden, and the seat experiences the constraint as rope-like or better. From the overlapping-claimant seat the identical structure operates as enforced dispossession — a great-power actor whose asserted entitlements are declared void inside a neighbor's line, with no geographic exit; that seat computes snare-flavored. The distant-water fleet seat sits between: real loss, but capital mobility damps the trap. These are same-level actors — all sovereign states, nominally equal in the system — yet geography differentiates their exit options completely: the claimant and the landlocked state are trapped by coastlines they cannot move, the fleet is mobile, the coastal administrator is constrained by its own treaty obligations. The tribunal seat observes the whole structure without collecting or paying. The engine derives this divergence from the structural data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: coastal_eez_states and small_island_developing_states sit near the beneficiary end (d near 0.0) — the regime subsidizes them with exclusive control they did not pay open-market prices for. Victim declarations drive the target end: overlapping_maritime_claimants and landlocked_geographically_disadvantaged_states are trapped (geography supplies no exit), pushing them toward the full-target end (d near 1.0); distant_water_fishing_fleets are victims whose fleet mobility moderates their position below full-target; historic_access_fishing_communities are powerless and trapped, at the extreme target end despite extracting nothing back. The regime's global spatial scope makes verification of compliance harder everywhere, so the engine scales effective extraction upward modestly across seats. Suppression is deliberately left unscaled — it is a structural property of the enforcement machinery, not a per-seat quantity. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct qualitative ordering, and the arithmetic belongs to the engine.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — allocating offshore resources before extraction technology outran any legal order — remains live: fisheries pressure, energy security, and deep-sea prospecting have intensified since codification, so there is no mandate that has outlived its function and no mandatrophy declaration. The classification discipline cuts both ways here. Reading the regime as pure rope would erase the excluded: the landlocked state, the displaced fishing community, and the overlapping claimant all bear real, enforced losses that a coordination-only framing renders invisible. Reading it as pure snare would mispredict its persistence: the regime endures partly because even its losers receive navigation guarantees, delimited certainty, and residual rights, and a pure-extraction model cannot explain why shipping states and disadvantaged states continue to comply. Tangled rope holds both facts. The low theater ratio and live founding problem jointly argue against piton drift, and the enforcement dependence argues against any mountain reading — the exclusivity is a constructed, defended arrangement, not a natural limit, which is exactly what the emerges_naturally=false declaration records.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the strict_eez_reading of kernel unclos_sovereignty_boundary; what structural changes would adoption of a sibling reading produce?',
    'Generate and classify the sibling stories (historical_rights_reading, non_ratifier_enforcement_reading) and compare beneficiary/victim sets, epsilon, and computed types; the divergence locates the structural element the readings disagree on.',
    'Under historical_rights_reading, historic users enter the beneficiary set and enclosing coastal states become targets — a victim/beneficiary inversion of this story. Under non_ratifier_enforcement_reading, the enforcement basis detaches from ratification status and the payer set shifts toward coastal enforcement monopolies. Classification of this story is valid only for the strict reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: this story is one of three readings of the UNCLOS sovereignty kernel; sibling readings instantiate different constraints.').

omega_variable(
    customary_law_crystallization,
    'Has 200-nautical-mile exclusivity crystallized into customary international law binding even non-participating states, making the constraint self-sustaining rather than enforcement-dependent?',
    'State-practice and opinio juris surveys; behavior of persistent objectors; whether non-party states respect zone limits absent any enforcement contact.',
    'If crystallized, the suppression requirement falls over time as norm internalization replaces patrol-and-prosecution, and the constraint drifts toward stable coordination with low enforcement overhead; if not, permanently high enforcement cost confirms the tangled_rope profile with durable extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_crystallization, empirical, 'Whether the regime persists by norm internalization or by continuous coercive maintenance.').

omega_variable(
    enforcement_target_composition,
    'Is the measured suppression directed primarily at resource-access violations (unlicensed fishing, unpermitted survey) or at sovereignty-framework alternatives (assertions that zone limits do not govern entitlement)?',
    'Classify enforcement incidents, prosecutions, and diplomatic protests by target type across a sample period; compare ratios.',
    'If suppression mostly targets resource theft, it serves the coordination function and net extraction is lower than the scalar suggests; if suppression of alternative entitlement frameworks dominates, the constraint actively forecloses competing sovereignty conceptions and drifts toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_target_composition, empirical, 'Composition of the enforcement burden: coordination-serving versus framework-suppressing.').

omega_variable(
    coastal_beneficiary_cohesion,
    'Do coastal states constitute a single beneficiary class, or do enforcement-exporting great powers and rent-collecting small island states hold opposite incentive structures within the same nominal seat?',
    'Coalition and voting analysis in convention bodies; divergence in enforcement spending versus license revenue; compliance asymmetries between capable and incapable coastal states.',
    'If the seat splits, effective extraction concentrates differently than a unified-beneficiary derivation predicts — great powers may be net payers of enforcement cost while micro-states are pure rent recipients, changing which seats compute as subsidized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coastal_beneficiary_cohesion, empirical, 'Internal heterogeneity of the coastal-state beneficiary seat.').

omega_variable(
    baseline_climate_stability,
    'Does sea-level rise and coastline retreat destabilize the regime''s fixed-baseline premise, threatening to extinguish or shift the zones of low-lying coastal and island states?',
    'Track state practice on baseline freezing declarations, tribunal treatment of shifting baselines, and any convention-body response to ambulatory-versus-fixed baseline claims.',
    'If baselines shift with the coast, current island-state beneficiaries become victims as their zones contract — a beneficiary-set inversion driven by physics rather than politics; if baselines freeze, the regime absorbs climate drift and the beneficiary structure holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(baseline_climate_stability, empirical, 'Climate-driven instability in the geographic foundation of the zone system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(uncl_tr_t1990, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(uncl_tr_t1996, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1996, 0.14).
narrative_ontology:measurement(uncl_tr_t2002, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2002, 0.15).
narrative_ontology:measurement(uncl_tr_t2010, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2010, 0.17).
narrative_ontology:measurement(uncl_tr_t2017, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2017, 0.19).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1982, 0.4).
narrative_ontology:measurement(uncl_be_t1990, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1990, 0.47).
narrative_ontology:measurement(uncl_be_t1996, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1996, 0.51).
narrative_ontology:measurement(uncl_be_t2002, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2002, 0.54).
narrative_ontology:measurement(uncl_be_t2010, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2010, 0.57).
narrative_ontology:measurement(uncl_be_t2017, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2017, 0.6).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement(uncl_su_t1990, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement(uncl_su_t1996, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1996, 0.58).
narrative_ontology:measurement(uncl_su_t2002, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2002, 0.63).
narrative_ontology:measurement(uncl_su_t2010, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2010, 0.66).
narrative_ontology:measurement(uncl_su_t2017, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2017, 0.68).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the UNCLOS maritime sovereignty regime' decomposes into three structurally distinct readings of one kernel (a fixed treaty text): strict EEZ exclusivity (this story — treaty-valid zones, overlay claims void), historical rights (usage-based entitlement overriding zone limits), and non-ratifier enforcement (customary navigation law enforced by naval presence regardless of ratification). Per the epsilon-invariance principle each reading is authored as its own story with its own epsilon, beneficiary/victim structure, and classification; the edges declared here express the constraint-family linkage, not causal dominance. The 1982 codification (in force 1994) is the upstream text whose legitimacy conditions the sibling readings contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
