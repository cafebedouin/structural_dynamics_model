% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__strict_geographic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__strict_geographic_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: unclos_maritime_sovereignty__strict_geographic_reading
 *   human_readable: UNCLOS Natural-Feature Requirement — Strict Geographic Reading
 *   domain: international law / maritime governance / geopolitical strategy
 *
 * SUMMARY:
 *   The strict geographic reading holds that only naturally formed features
 *   above water at high tide are islands capable of generating a territorial
 *   sea and exclusive economic zone, and that artificial construction —
 *   reclamation, outposts, garrisons — never alters a feature's legal status.
 *   It is codified in UNCLOS Articles 13, 60(8), and 121 and was
 *   operationalized against a major claimant by the 2016 South China Sea
 *   arbitration. The arrangement solves a real collective-action problem: a
 *   verifiable, construction-independent criterion for what generates
 *   maritime zones prevents a dredging arms race in which entitlements track
 *   construction budgets, and it keeps baselines stable enough to negotiate
 *   delimitation against. The same structure imposes asymmetric costs: states
 *   that have invested heavily in building habitable outposts on marginal
 *   features recover nothing, while naval powers, established
 *   feature-holders, and open-access users collect. This file instantiates
 *   ONE reading of the unclos_maritime_sovereignty kernel; the expansive and
 *   hybrid readings are separate constraints with their own files, their own
 *   epsilon, and their own beneficiary/victim structure. Per the
 *   epsilon-referent rule, the epsilon authored here is for the standing
 *   arrangement under contest — the strict rule as it actually operates —
 *   assessed by this reading's own lights, which endorse the rule and
 *   therefore register its costs as real but largely legitimate; a sibling
 *   reading assessing the same referent would author a different, higher
 *   value. Interval mapping: t=0 is 1994 (UNCLOS entry into force), t=32 is
 *   2026; the 2016 award falls at t=22.
 *
 * KEY AGENTS:
 *   - - blue_water_naval_powers: Primary beneficiary (institutional/mobile) — receive guaranteed open access to waters that constructed outposts would otherwise enclose; fund enforcement patrols
 *   - - non_claimant_littoral_states: Secondary beneficiary (organized/constrained) — rely on predictable delimitation; a cap on what any neighbor can manufacture protects them from encirclement
 *   - - established_natural_feature_holders: Secondary beneficiary (organized/constrained) — hold large natural islands whose existing zones are secured against manufactured rivals
 *   - - small_island_developing_states: Conditional beneficiary (moderate/trapped) — entire economies zoned by natural-island EEZs; secured by the reading but exposed if their features submerge
 *   - - expansionist_coastal_states: Primary target (powerful/constrained) — construction investments generate no entitlements under this reading; bound by ratification yet defiant of adverse awards
 *   - - unclos_dispute_tribunals: Agenda-setter (institutional/analytical) — administer the rule's meaning through Annex VII and ITLOS proceedings; depend on party consent for compliance
 *   - - distant_water_fishing_fleets: Secondary beneficiary (organized/mobile) — retain access to grounds that would be enclosed EEZ waters if outposts generated zones
 *   - - low_lying_atoll_populations: Excluded voice (powerless/trapped) — communities whose entitlements and livelihoods depend on features facing submergence; unrepresented in adjudication
 *   - - maritime_law_scholars: Analytical observer (analytical/analytical) — document state practice and test the reading's doctrinal foundations from outside the dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.52).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.56).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Natural-Feature Requirement — Strict Geographic Reading").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international law / maritime governance / geopolitical strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, '3ad2bbf5-ec07-4769-8bdf-27a90d2c7eb1').
narrative_ontology:cs_kernel_codification('3ad2bbf5-ec07-4769-8bdf-27a90d2c7eb1', fixed_text).
narrative_ontology:cs_authority_grounding('3ad2bbf5-ec07-4769-8bdf-27a90d2c7eb1', lineage).
narrative_ontology:cs_interpretation_layer_present('3ad2bbf5-ec07-4769-8bdf-27a90d2c7eb1').
narrative_ontology:cs_reading_relation('3ad2bbf5-ec07-4769-8bdf-27a90d2c7eb1', unclos_maritime_sovereignty__expansive_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('3ad2bbf5-ec07-4769-8bdf-27a90d2c7eb1', unclos_maritime_sovereignty__hybrid_effective_control_reading, forecloses).
narrative_ontology:cs_axiom('3ad2bbf5-ec07-4769-8bdf-27a90d2c7eb1', foundational, natural_formation_necessary_condition).
narrative_ontology:cs_axiom_status(natural_formation_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('3ad2bbf5-ec07-4769-8bdf-27a90d2c7eb1', natural_formation_necessary_condition, conventional).
narrative_ontology:cs_axiom('3ad2bbf5-ec07-4769-8bdf-27a90d2c7eb1', foundational, human_works_do_not_create_maritime_entitlement).
narrative_ontology:cs_axiom_status(human_works_do_not_create_maritime_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('3ad2bbf5-ec07-4769-8bdf-27a90d2c7eb1', human_works_do_not_create_maritime_entitlement, conventional).
narrative_ontology:cs_reference_frame('3ad2bbf5-ec07-4769-8bdf-27a90d2c7eb1', natural_geography_entitlement_baseline).
narrative_ontology:cs_drift_state('3ad2bbf5-ec07-4769-8bdf-27a90d2c7eb1', post_south_china_sea_award_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3ad2bbf5-ec07-4769-8bdf-27a90d2c7eb1', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, blue_water_naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_littoral_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, established_natural_feature_holders).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, small_island_developing_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, distant_water_fishing_fleets).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, unclos_article_121_capacity_test).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, freedom_of_navigation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate navies with worldwide reach and depend on open sea lanes. The rule keeps waters around constructed outposts in international status, preserving transit and access they would otherwise have to negotiate for feature by feature. They bankroll the enforcement side — patrols, protests, legal support for claimants — and treat the spending as cheap relative to what closed baselines would cost them.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, blue_water_naval_powers, beneficiary,
    institutional, generational, mobile, global).

% Coastal states that make no expansive offshore claims. They negotiate boundaries with neighbors against a fixed yardstick: whatever anyone builds, the baseline does not move. Without the yardstick, a wealthier or more ambitious neighbor could manufacture entitlements ringing their coasts; with it, their exposure is capped by geography they can verify.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_littoral_states, beneficiary,
    organized, generational, constrained, regional).

% States holding large, genuinely natural islands and archipelagos whose existing zones are the economic substrate of their maritime economies. The rule insulates those holdings from rivals who would otherwise contest them by outbuilding them, converting a potential construction competition they might lose into a settled inventory they already own.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, established_natural_feature_holders, beneficiary,
    organized, generational, constrained, regional).

% Their fisheries, minerals, and revenue all sit inside zones generated by natural islands they cannot relocate. The rule secures those zones against larger neighbors' engineering projects, but it offers no answer to the ocean rising around the features themselves: if a natural island submerges, this reading gives them no frozen-baseline comfort. They are bound to the rule's protection and exposed to its blind spot simultaneously, with no exit from their own geography.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, small_island_developing_states, beneficiary,
    moderate, generational, trapped, regional).

% Invest heavily in dredging, reclamation, and garrisoned outposts on marginal features, intending to convert presence into entitlement. Under this reading those investments generate nothing beyond installation status. They are bound by ratification, defiant of adverse awards, and unable to exit cleanly: denouncing the convention would cost reputation and leave them facing the same rule as customary law. Their secondary position: the same rule shields their own home coasts and home features from other states' constructions.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    powerful, generational, constrained, regional).

% Annex VII arbitral tribunals and ITLOS chambers apply Articles 13, 60(8), and 121, deciding which features count and what construction is worth. The 2016 award operationalized the strict reading at scale. They set the rule's operative meaning but command no fleet: compliance depends on parties' consent, and a major party's open refusal tests the limits of their authority.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, unclos_dispute_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% Commercial operators working grounds far from home ports. Waters that stay international because outposts generate no zones remain open to them; the same waters, enclosed as someone's exclusive zone, would be closed or licensed at a price. Their interest tracks access season by season, and they follow the legal status of the water rather than shaping it.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, distant_water_fishing_fleets, beneficiary,
    organized, immediate, mobile, global).

% Communities living on and from low-lying natural features whose zones and livelihoods depend on the features staying above water. No seat in the adjudication represents them; the interpretive fight is conducted among states over construction and control, while the slower question — what happens to entitlements when the sea takes the feature — proceeds without them.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, low_lying_atoll_populations, excluded,
    powerless, generational, trapped, regional).

% Academic and institute-based specialists who document state practice, annotate the case law, and stress-test the reading's doctrinal foundations. They hold no stake in any feature, publish from outside the litigating governments, and supply the independent record that tribunals and foreign ministries cite.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, maritime_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__strict_geographic_reading, blue_water_naval_powers).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__strict_geographic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single verifiable criterion — natural formation, above water at high tide — for what generates maritime zones, so that every state can compute every other state's potential entitlements from geography alone, delimitation negotiations have a stable reference, and no state can convert a construction budget into a claim.
% TRANSFER_FUNCTION: Moves entitlement-generating capacity away from states willing to spend on offshore construction and toward states already holding natural features; moves enforcement costs onto naval powers and claimant litigants; leaves the waters around built structures in open status, transferring access from would-be enclosing states to all users.
% ABSENT_VOICES: Expansionist coastal states are formally present — they ratified the convention — but their interpretive position is excluded from this reading's framework: they would date the injustice earlier (habitable territory distributed under imperial conditions and now frozen) and demand credit for creating habitability. Low-lying atoll populations are absent entirely: no seat represents the people whose entitlements ride on features the sea may reclaim.
% DISAPPEARANCE_RATIONALE: If the natural-feature criterion vanished overnight, states with dredging capacity would race to manufacture entitlements on every shoal and reef, delimitation negotiations would collapse for lack of a fixed reference, sea lanes through contested waters would begin closing under new claimed zones, and fisheries access would re-fragment around whoever built fastest — the maritime order would reorganize around construction capability within a decade.
% FOUNDING_PROBLEM: Mid-twentieth-century engineering made it feasible for any coastal state to build structures far offshore; without a criterion tying entitlement to natural geography, maritime zones would track construction budgets, inviting an artificial-island arms race, closing historically open sea lanes, and destabilizing fisheries allocations that had followed natural geography for centuries. UNCLOS answered with Articles 13, 60(8), and 121.
% FOUNDING_PROBLEM_CORROBORATION: The strongest attestation comes from outside the beneficiary set: the states the rule burdens corroborate its existence by litigating against its application rather than denying it — a major claimant's rejection of the 2016 award presupposes the rule it refuses. The award's reasoning rests on decades of state practice compiled by neutral commentators, and academic syntheses (ILA studies, scholarly treatments of Article 121) independent of every beneficiary government attest both the founding problem's persistence and the rule's content. No purely beneficiary-authored genealogy is relied on here.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. I claim tangled_rope because the structure possesses both a genuine coordination function (stable, verifiable, construction-independent baselines that all negotiating parties can rely on) and asymmetric burden (a identifiable class of states pays — forfeited entitlement investments — through the same instrument that coordinates everyone else), and because persistence requires active enforcement: awards, diplomatic protest, and freedom-of-navigation operations, without which construction would simply proceed. Metrics describe actual operation: extractiveness 0.52 reflects real delivered coordination with substantial concentrated cost on targeted states; suppression 0.56 reflects enforcement machinery short of compulsion — exit exists (denunciation, defiance) but carries reputational and legal cost; theater_ratio 0.28 reflects a mostly functional rule with a growing performative component (signaling patrols, declaratory statements) after the 2016 award; accessibility_collapse 0.35 reflects that rival readings remain live and practiced — understanding this rule does not eliminate alternatives, because the treaty regime permits interpretive defection; resistance 0.62 reflects outright rejection of the award by the largest targeted state and continued construction in its face. All three tracked series share one time grid (t=0,5,10,16,22,27,32) so no metric is sampled against another's end-state. Suppression is authored as a raw structural property; only extractiveness is scaled downstream by directionality and scope. Coalition note: the victim class is currently a single fragmented seat; if expansionist states coalized behind a common interpretive front or amendment push, their effective power would rise and the enforcement picture would change — the analysis assumes the present fragmented configuration.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is sharp and structural. From the naval-power and feature-holder seats the arrangement presents as coordination they helped build and police: predictable baselines, open lanes, secure zones. From the expansionist seat the identical structure operates as confiscation of sunk investment and foreclosure of claimed entitlements. Same-level lateral dynamics: the Philippines and China hold comparable nominal standing as coastal Asia-Pacific states, yet sit at opposite ends of the structure — the claimant invoked the strict reading as shield, the expansionist bears it as burden; what differentiates them is not power but which side of the rule their geography and behavior place them on. The Japan case shows a single state straddling the structure: beneficiary regarding neighbors' outposts, quasi-target regarding its own reinforcement of Okinotorishima — evidence that the reading's incidence is feature-class-relative, not state-relative. Inter-institutionally, tribunals experience the rule as doctrine to administer, naval commands as a patrol justification, and foreign ministries as a bargaining chip; identical text, three operating realities.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the five beneficiary seats: naval powers (mobile exit, global scope) sit nearest the subsidized end; feature-holders and non-claimant states (constrained exit) somewhat less so; distant-water fleets benefit incidentally through access. The victim declaration drives the expansionist seat toward the full-target end. One override is authored: the only powerful-atom agent in this story is expansionist_coastal_states, and the structural derivation from its victim declaration alone would place it near d=0.9; it is overridden to 0.72 because the same rule secondarily shields its own home-coast and home-feature zones from other states' constructions — an offsetting indirect benefit the declaration cannot express. The net position remains target-dominant. Tribunals sit near symmetric as administrators who neither collect nor pay; scholars are analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, not dead: construction technology keeps advancing, disputes over marginal features keep arriving, and the rule's workload is growing rather than atrophying — so no mandatrophy resolution is declared and none is due. The classification's work here is preventive in both directions. Read from the naval seat alone, the arrangement masquerades as pure coordination (a rope) and the burdened class disappears from view; read from the expansionist seat alone, it masquerades as pure confiscation (a snare) and the genuine delimitation-stability function — which predates the current disputes and serves every negotiating party — disappears. The tangled-rope structure holds both truths simultaneously: coordination delivered, extraction imposed, enforcement required. The temporal series guards the third failure mode: theater is rising but from a low base and the functional core (delimitation reliance on natural baselines) remains dominant, so no drift-to-performance verdict is warranted on this record.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_of_entitlement_source,
    'This constraint is one reading of the unclos_maritime_sovereignty kernel: the disagreement between readings is located in whether human construction can be a source of maritime entitlement at all — would instantiating a sibling reading change the victim set and epsilon?',
    'Adjudication or negotiated clarification of whether Articles 13, 60(8), and 121 admit a construction-based path to entitlement; structured comparison of the three readings'' beneficiary/victim mappings over the same referent.',
    'Under the expansive_construction_reading, expansionist coastal states flip from victims to beneficiaries and naval and non-claimant states inherit the burdened side; epsilon and per-seat classification recompute for the sibling file over the identical standing arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_indexicality_of_entitlement_source, conceptual, 'Kernel-level framing uncertainty: which source-of-entitlement reading governs the arrangement.').

omega_variable(
    capacity_test_indeterminacy,
    'How demanding is Article 121(3)''s requirement that a feature sustain human habitation or economic life of its own — the hinge on which this reading''s reach over specific features turns?',
    'Accumulating jurisprudence and state practice on individual features (the Itu Aba line of cases), synthesized by bodies outside the litigating parties.',
    'A lenient capacity test shrinks the victim set (more features qualify as full islands generating zones); a stringent test extends the burdened class to any state relying on marginal features, raising measured burden on targeted seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_test_indeterminacy, conceptual, 'Indeterminacy of the rock-versus-island capacity threshold within the strict reading.').

omega_variable(
    sea_level_rise_baseline_erosion,
    'Does the strict reading survive climate-driven submergence of natural features — do disappearing islands take their generated zones with them, converting current beneficiaries into future victims?',
    'International Law Commission work on sea-level rise and baselines; state practice on frozen baselines; eventual tribunal treatment of a formerly natural feature that submerges.',
    'If zones extinguish with the feature, small island developing states migrate from the beneficiary side to the victim side over decades and the reading''s beneficiary coalition narrows; if baselines freeze, the reading stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sea_level_rise_baseline_erosion, empirical, 'Whether natural-feature baselines erode under sea-level rise within this reading.').

omega_variable(
    enforcement_without_compulsion,
    'Does the reading persist through consent and overlapping interest, or through great-power naval enforcement — would it hold if freedom-of-navigation operations ceased?',
    'Comparative compliance analysis in regions without a naval enforcement presence versus regions with one; behavior of claimants where enforcement capacity is absent.',
    'If enforcement-dependent, the burden on targeted states is structurally coerced and the arrangement hardens toward pure-extraction characteristics for those seats; if consent-based, it trends toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_without_compulsion, empirical, 'Consent-based versus enforcement-based persistence of the strict reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_strict_geo_tr_t0, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(unclos_strict_geo_tr_t5, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(unclos_strict_geo_tr_t10, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(unclos_strict_geo_tr_t16, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(unclos_strict_geo_tr_t22, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 22, 0.24).
narrative_ontology:measurement(unclos_strict_geo_tr_t27, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 27, 0.26).
narrative_ontology:measurement(unclos_strict_geo_tr_t32, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 32, 0.28).

% Extraction over time
narrative_ontology:measurement(unclos_strict_geo_be_t0, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(unclos_strict_geo_be_t5, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 5, 0.39).
narrative_ontology:measurement(unclos_strict_geo_be_t10, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(unclos_strict_geo_be_t16, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(unclos_strict_geo_be_t22, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 22, 0.52).
narrative_ontology:measurement(unclos_strict_geo_be_t27, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 27, 0.53).
narrative_ontology:measurement(unclos_strict_geo_be_t32, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 32, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(unclos_strict_geo_su_t0, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(unclos_strict_geo_su_t5, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(unclos_strict_geo_su_t10, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(unclos_strict_geo_su_t16, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(unclos_strict_geo_su_t22, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 22, 0.5).
narrative_ontology:measurement(unclos_strict_geo_su_t27, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 27, 0.54).
narrative_ontology:measurement(unclos_strict_geo_su_t32, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 32, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the law on islands and maritime zones' decomposes into three structurally distinct claims about whether human construction can source maritime entitlement. Each member carries its own epsilon over the same standing arrangement: this strict reading authors moderate reading-indexed burden (its own lights endorse the rule); the expansive reading authors the inverse beneficiary/victim mapping; the hybrid authors a maturation-contingent middle. The strict reading is upstream in legitimacy terms — the 2016 award applying it is cited as authority in disputes the siblings would resolve oppositely — so edges run from this file to both siblings. Decomposition follows the epsilon-invariance principle: one observable (whose construction counts?) per file, no averaging across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__strict_geographic_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
