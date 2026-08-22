% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__hybrid_effective_control_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__hybrid_effective_control_reading
 *   human_readable: Graduated Feature-Status Regime with Acquiescent Maturation (Hybrid Effective-Control Reading)
 *   domain: international_law/maritime_governance/geopolitical
 *
 * SUMMARY:
 *   A graduated maritime-entitlement regime: naturally formed above-water
 *   features anchor full territorial seas and exclusive economic zones;
 *   artificial installations carry a fixed 500-meter safety zone and nothing
 *   more — unless prolonged, unchallenged effective control matures them into
 *   recognized territorial claims. The regime solves a real coordination
 *   problem: it caps the artificial-enclosure race, gives every mariner a
 *   chartable rule, and channels would-be territorial conflict into a waiting
 *   game. Its maturation clause simultaneously converts the region's power
 *   gradient into legal entitlement: each year an installation stands and no
 *   one dislodges it, the weaker claimant's position converts into the
 *   occupier's. This file instantiates one reading of the
 *   maritime-sovereignty kernel — see commentary.kernel_context; the other
 *   readings are separate constraint files, not part of this one. The epsilon
 *   referent is the standing graduated arrangement as this reading assesses
 *   it, never any sibling's alternative. The interval spans the three decades
 *   since the regime's entry into force (T0 approximates entry into force,
 *   T30 the present); the steepest drift segment corresponds to the
 *   large-scale reclamation boom of the interval's second half. KEY AGENTS
 *   (by structural relationship) are enumerated in commentary.key_agents.
 *
 * KEY AGENTS:
 *   - construction_capable_regional_powers: agenda-setter and primary beneficiary (institutional/arbitrage) — builds, administers, patrols, and waits out protest; the maturation clock runs in its favor
 *   - militarily_weaker_claimant_states: primary target (moderate/trapped) — holds natural-feature entitlements the regime protects and overlapping claims the maturation clause drains; protest without dislodgement does not interrupt the clock
 *   - commercial_shipping_interests: secondary beneficiary (organized/mobile) — buys predictable corridors from the graduation cap; bears costs only episodically
 *   - traditional_fishing_communities: diffuse target (powerless/trapped) — loses grounds as maturing claims are enforced; no seat in any process
 *   - non_claimant_regional_states: excluded voice (moderate/constrained) — would object to occupation converting into title; not a party to the claimant conversation
 *   - international_tribunals: analytical observer (institutional/analytical) — clarifies the text, commands no fleet; awards register without dislodging installations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.58).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.62).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "Graduated Feature-Status Regime with Acquiescent Maturation (Hybrid Effective-Control Reading)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'ff4f4c37-14b0-4c80-a4d3-3cef197dfc1d').
narrative_ontology:cs_kernel_codification('ff4f4c37-14b0-4c80-a4d3-3cef197dfc1d', fixed_text).
narrative_ontology:cs_authority_grounding('ff4f4c37-14b0-4c80-a4d3-3cef197dfc1d', practice).
narrative_ontology:cs_interpretation_layer_present('ff4f4c37-14b0-4c80-a4d3-3cef197dfc1d').
narrative_ontology:cs_reading_relation('ff4f4c37-14b0-4c80-a4d3-3cef197dfc1d', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('ff4f4c37-14b0-4c80-a4d3-3cef197dfc1d', unclos_maritime_sovereignty__expansive_construction_reading, forecloses).
narrative_ontology:cs_axiom('ff4f4c37-14b0-4c80-a4d3-3cef197dfc1d', foundational, prolonged_unchallenged_control_matures_maritime_claims).
narrative_ontology:cs_axiom_status(prolonged_unchallenged_control_matures_maritime_claims, holdable).
narrative_ontology:cs_axiom_grounding('ff4f4c37-14b0-4c80-a4d3-3cef197dfc1d', prolonged_unchallenged_control_matures_maritime_claims, instrumental).
narrative_ontology:cs_axiom('ff4f4c37-14b0-4c80-a4d3-3cef197dfc1d', foundational, feature_origin_graduates_entitlement_generation).
narrative_ontology:cs_axiom_status(feature_origin_graduates_entitlement_generation, holdable).
narrative_ontology:cs_axiom_grounding('ff4f4c37-14b0-4c80-a4d3-3cef197dfc1d', feature_origin_graduates_entitlement_generation, conventional).
narrative_ontology:cs_axiom('ff4f4c37-14b0-4c80-a4d3-3cef197dfc1d', secondary, effective_challenge_interrupts_maturation).
narrative_ontology:cs_axiom_status(effective_challenge_interrupts_maturation, holdable).
narrative_ontology:cs_axiom_grounding('ff4f4c37-14b0-4c80-a4d3-3cef197dfc1d', effective_challenge_interrupts_maturation, conventional).
narrative_ontology:cs_reference_frame('ff4f4c37-14b0-4c80-a4d3-3cef197dfc1d', graduated_feature_status_framework).
narrative_ontology:cs_drift_state('ff4f4c37-14b0-4c80-a4d3-3cef197dfc1d', post_2016_arbitration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ff4f4c37-14b0-4c80-a4d3-3cef197dfc1d', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_regional_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, commercial_shipping_interests).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, traditional_fishing_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimant_states).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__hybrid_effective_control_reading, effective_occupation_doctrine).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__hybrid_effective_control_reading, prescription_by_acquiescence_doctrine).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__hybrid_effective_control_reading, graduated_feature_status_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dredge, fill, and pave reefs and low-tide elevations into multi-hectare installations with runways, harbors, and garrisons; administer them through coast guard patrols, administrative registries, and resettled civilian presence; maintain a continuous physical presence that outlasts any single protest cycle. Their construction fleets and blue-water coast guards are the region's largest, so they set the pace of occupation and can wait out diplomatic objection indefinitely. When neighbors protest they keep building; when tribunals rule against them they decline compliance while the installations remain garrisoned. What flows to them is de facto administration of the waters and seabed around their features and, if no one ever forcibly removes them, a settled position that hardens with each unopposed year.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_regional_powers, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_regional_powers, beneficiary).

% Hold claims to naturally formed islands and reefs that under the natural-feature rule generate full territorial seas and exclusive economic zones — entitlements their fishing fleets and hydrocarbon prospects depend on. They file diplomatic protests against neighbors' construction, bring arbitration proceedings when they can, and patrol what their fleets allow. They cannot match the dredging output of the larger powers, cannot physically remove established installations, and cannot abandon their claims without domestic political collapse; their protests are on the record, but a protest on paper does not dislodge a garrison. Each year an installation stands unopposed, their own overlapping claim weakens.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimant_states, payer,
    moderate, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimant_states, beneficiary).

% Move a large share of global seaborne trade through the straits and corridors the disputed waters contain. They need predictable passage: the 500-meter safety zones around installations are legible and chartable, and the rule that only natural features generate wide entitlement zones keeps the corridors from being parceled into a patchwork of artificial enclaves. They hold no claim of their own and bear costs only episodically, when a dispute flares and insurers reprice a route.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, commercial_shipping_interests, beneficiary,
    organized, biographical, mobile, global).

% Have worked reefs, shoals, and seasonal grounds for generations without regard to which flag claims them. As installations mature into administered zones, patrols push them off grounds their grandparents fished, and access they never had to negotiate for is closed by enforcement they have no standing to contest. They appear in the dispute only as statistics in state filings; no seat at any negotiating table represents them.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, traditional_fishing_communities, payer,
    powerless, immediate, trapped, regional).

% Neighbor the disputed waters without holding claims of their own. Their navies and merchants depend on the same corridors, and their own future claims or resource interests would be prejudiced by a settled map drawn by the strongest builder. They would object to a regime in which prolonged occupation converts into title, but the claimant conversation runs state-to-state among the parties and they are not parties; their options are general diplomatic statements and coalition-building in broader fora.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, non_claimant_regional_states, excluded,
    moderate, generational, constrained, regional).

% Adjudicate feature status, entitlement generation, and the legality of construction when a claimant seises them. Their awards clarify what the treaty text means — which features count, what construction can and cannot generate — but they command no fleet: an award against the strongest builder is declined, and the installations it addressed remain garrisoned. Their findings register in the legal record whether or not the physical situation changes.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_regional_powers).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__hybrid_effective_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, predictable rule for what generates maritime jurisdiction: naturally formed above-water features anchor full territorial seas and exclusive economic zones; artificial installations carry a fixed 500-meter safety zone every mariner can chart and avoid. The graduation caps the artificial-enclosure race — no party can convert dredging into unlimited sea — and the maturation clause gives prolonged occupation a lawful path to recognition, channeling would-be territorial conflict into a waiting game rather than open war.
% TRANSFER_FUNCTION: Moves fisheries access, hydrocarbon and seabed entitlements, and jurisdictional authority from militarily weaker claimants and traditional users toward the states with the dredging capacity and patrol fleets to occupy and hold — through the acquiescence pathway: each year a contested occupation goes unchallenged in fact, the weaker party's position converts into the stronger party's recognized entitlement.
% ABSENT_VOICES: Traditional fishing communities lose grounds as maturing claims are enforced but hold no seat in any state-to-state process; non-claimant regional states would object to occupation converting into title but are not parties to the claimant conversation; and the weaker claimants' own protests are structurally discounted — under the effective-control standard, a protest that does not dislodge the occupation does not interrupt it, so the voices that would reset the clock are the voices the standard renders inaudible.
% DISAPPEARANCE_RATIONALE: If the graduation rule and its maturation pathway vanished overnight, every artificial installation's status would become immediately contestable, the construction-capable powers would either race to consolidate by force or lose their accumulated positions, boundary negotiations would restart from zero across every disputed sea, and shipping would reprice routes around a sudden fog of overlapping claims — naval deployments, fishing access, and hydrocarbon development would all rearrange around the vacuum.
% FOUNDING_PROBLEM: After decolonization and the erosion of freedom-of-the-seas limits, coastal states raced to claim fisheries and hydrocarbons farther offshore, and disputed features invited occupation and fortification; the treaty generation needed a shared rule for what a feature is worth — which specks of land generate wide zones, what artificial construction can generate, and how long an occupation must last before the map accepts it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the continuing docket of maritime delimitation cases at international tribunals, the treaty's own preparatory history, and weaker claimants' resort to adjudication they cannot enforce all attest that the underlying coordination problem — what generates entitlement — remains live. No source outside the construction-capable powers attests that the maturation-through-unchallenged-occupation clause specifically serves that founding problem; that clause is defended in practice mainly by the states it benefits.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (epsilon 0.58) is intermediate by construction: the natural-feature half and the 500-meter cap are genuinely load-bearing coordination that even the payer seats rely on — the cap is why artificial dredging has not parceled the corridors — while the maturation clause is the extractive core, time-indexed and compounding with every unchallenged year of occupation, its value flowing to exactly the seats with the capacity to occupy. The series shows base extractiveness rising from 0.45 to 0.58, steepest mid-interval as large-scale reclamation began feeding the pathway. Suppression (0.62) is authored as a raw structural property (only extractiveness is scaled by directionality and scope downstream): it is structural rather than internalized — the effective-challenge standard is written into the rule's operation, so the rule's own evidence requirements do the suppressing, backed by a patrol and coast-guard presence that hardened across the interval (suppression_requirement 0.45 to 0.62; the enforcement build-up is the dynamic being traced, hence the series). Theater (0.32) is real but partial: occupation performance — ceremonies, administrative registries, patrol schedules — carries a performative surplus that grew as claim-building became the game, but the installations, runways, and garrisons are physically operative, so theater is a component of the activity, not its substance. Accessibility_collapse (0.45): alternatives exist and are used — arbitration, diplomatic notes, freedom-of-navigation operations, counter-patrols — but understanding the rule opens no workable exit for the weaker seats, because the standard discounts every alternative they can actually afford. Resistance (0.60) is correspondingly substantial and sustained. The claim is authored independently of the metrics: tangled_rope because both a genuine coordination function and asymmetric extraction are structural, with active enforcement required to hold the occupations that feed the pathway. Fixing is prohibitive: the clause can be removed only by treaty amendment or physical dislodgement, and the parties with the capacity to do either are the parties it pays.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the maturation clause as the stabilization of effective situations — restraint it accepts (the 500-meter cap) plus recognition it has earned (prolonged occupation). The payer seats experience the identical clause as dispossession by clock: from the weaker claimant's position, the same silence that reads as restraint from the builder's position reads as the conversion of its claim into the rival's title. Same-level divergence among the weaker claimants: those whose natural islands lie inside their own patrol reach experience the natural-feature rule as protection (their secondary beneficiary position) and the maturation threat as distant; those whose claims overlap active construction experience only the clock. Coalition potential among the weaker claimants exists — joint protest, regional caucusing — but is undercut by their own overlapping claims with each other, which is part of why the clock runs. Identity fusion binds the payer seats: the sovereignty claim is constitutive of each state's self-conception, so renunciation — the only exit the clause leaves open — is domestically unthinkable even where it would be materially rational; identity lock is layered on structural closure. Tribunals encounter the structure as text awaiting clarification; the fishing communities encounter it as water that closed. The engine computes these per-seat classifications from the structural data; the divergence between the builder's seat and the protestor's seat is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   construction_capable_regional_powers: declared beneficiary and agenda-setter with arbitrage-grade exit (they can build elsewhere and pace their own occupations) — derivation places them near the beneficiary end; their own enforcement costs keep them off zero. militarily_weaker_claimant_states: declared victim with trapped exit, carrying a secondary beneficiary position (the natural-feature rule protects their islands and the entitlements their fleets and prospects depend on); the two pulls oppose, and the net sits near the target end because the maturation clause is the term that moves value — their protected entitlements are static while their overlapping claims drain. traditional_fishing_communities: pure payer, powerless, no alternative grounds, no standing — full-target end. commercial_shipping_interests: beneficiary with mobile exit — the cap and the natural-feature rule are what keep their corridors from being parceled; episodic repricing during flare-ups keeps them off pure zero. non_claimant_regional_states: declared neither beneficiary nor victim — their stake runs through future claims the current map would prejudice; the derivation has no structural declaration for them, and that absence is precisely what the excluded role records. international_tribunals: analytical seat, neither collecting nor paying. Scope amplification runs against the payer seats: the regime operates at regional-to-global scope where verification of challenge is weakest, so the effective extraction the payer seats experience exceeds the base epsilon; the beneficiaries, operating at the same scope from the enforcing side, experience no such amplification. No directionality overrides are authored: the derivation from the declared structure produces the correct relationships, and the commentary documents the one dual-positioned seat rather than overriding it.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the regime as pure rope — the construction-capable powers' framing, that they merely stabilize effective situations — erases the acquiescence mechanism, whose function is to convert unchallengeable power into uncontestable title. Reading it as pure snare — the weaker claimants' framing, the law of the strongest — erases the real coordination: the cap that keeps artificial dredging from parceling the commons and the natural-feature rule that protects the payer seats' own islands. No mandatrophy: the founding problem (what generates entitlement) is live and corroborated from outside the beneficiary set, the maturation clause is operative rather than atrophied, and the arrangement carries no sunset — it is not transitional scaffolding and not a maintained corpse. The signature to watch is the time-indexed extraction: if the maturation pathway completes across every occupied feature, the regime's residual coordination function shrinks toward the expansive sibling's world and the classification should migrate; the omega on the challenge-evidence standard gates exactly that migration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading — hybrid_effective_control_reading — of the kernel unclos_maritime_sovereignty; what would each sibling reading change structurally if adopted in its place?',
    'Generate and classify the sibling readings as separate constraint files (unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__expansive_construction_reading) and compare computed per-seat classifications across the family.',
    'The strict sibling deletes the maturation pathway: the construction-capable seats lose the acquiescence route and this file''s victim set shrinks toward the expansive reading''s. The expansive sibling deletes the 500-meter cap: extraction becomes immediate rather than time-indexed and rises well above the authored value. The epsilon of 0.58 is valid only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of three mutually exclusive readings of the maritime-sovereignty kernel; siblings are separate constraints.').

omega_variable(
    effective_challenge_evidence_standard,
    'What counts as a challenge capable of interrupting maturation, and what duration counts as prolonged — and who observes either?',
    'Comparative analysis of maturation episodes: which protest forms (diplomatic notes, arbitration filings, physical interposition, resupply interdiction) have historically reset or failed to reset occupation clocks, and over what durations.',
    'If written protest suffices, the maturation clause almost never completes in contested waters and operates as near-dead text, so extraction falls well below the authored value; if only physical or adjudicative challenge suffices, militarily weaker claimants can never interrupt maturation and the clause operates as a pure power-asymmetry converter, pushing effective extraction far above 0.58.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_challenge_evidence_standard, conceptual, 'The reading''s load-bearing ambiguity: the challenge and duration standards that gate maturation.').

omega_variable(
    capacity_asymmetry_endogeneity,
    'Does the maturation pathway amplify the underlying capacity asymmetry into legal entitlement, or merely register an asymmetry that would decide the same disputes by force anyway?',
    'Counterfactual comparison of disputed-feature outcomes before and after the reading''s norms crystallized, and of contested versus uncontested occupations with matched capacity gaps.',
    'If amplifying, the rule adds extractive force beyond the raw power gradient and the authored epsilon understates it; if merely registering, much of the measured extraction belongs to the power distribution rather than to the rule, and the tangled_rope reading overstates the rule''s own contribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capacity_asymmetry_endogeneity, empirical, 'Whether the rule converts power into law or merely records what power already decided.').

omega_variable(
    natural_artificial_boundary_stability,
    'Is the natural/artificial feature distinction stable enough to carry the graduation, or do erosion, subsidence, and ever-larger reclamation blur it into one of the sibling readings?',
    'Track feature-status disputes where a natural island has been substantially augmented or a natural feature has eroded below high-tide line, and how tribunals and states classify them.',
    'If the boundary dissolves, the reading''s foundational graduation axiom fails and the constraint collapses toward the strict sibling (status frozen at natural origin) or the expansive sibling (origin irrelevant); the graduated structure and its intermediate epsilon would no longer be coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_artificial_boundary_stability, conceptual, 'Stability of the natural/artificial distinction under reclamation technology and sea-level rise.').

omega_variable(
    acquiescence_mechanism_structural_vs_learned,
    'Is the weaker claimants'' non-interruption of maturation structural (no capacity to physically challenge) or learned (protest fatigue — diplomatic establishments that have internalized futility and under-protest even where protest is cheap)?',
    'Compare protest intensity across weaker claimants facing identical occupation facts; if some protest persistently and others fall silent under matched conditions the learned component is real. Post-settlement protest trajectories would show whether silence persists after capacity changes.',
    'If substantially learned, suppression persists after the capacity gap closes and the measured suppression understates the arrangement''s hold on the payer seats; if structural, closing the capacity gap restores challenge and the maturation clause self-limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acquiescence_mechanism_structural_vs_learned, empirical, 'Structural versus internalized component of the acquiescence that feeds maturation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_hybrid_reading_tr_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(unclos_hybrid_reading_tr_t0, observed).
narrative_ontology:measurement(unclos_hybrid_reading_tr_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(unclos_hybrid_reading_tr_t5, observed).
narrative_ontology:measurement(unclos_hybrid_reading_tr_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(unclos_hybrid_reading_tr_t10, observed).
narrative_ontology:measurement(unclos_hybrid_reading_tr_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(unclos_hybrid_reading_tr_t15, observed).
narrative_ontology:measurement(unclos_hybrid_reading_tr_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement_basis(unclos_hybrid_reading_tr_t20, observed).
narrative_ontology:measurement(unclos_hybrid_reading_tr_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement_basis(unclos_hybrid_reading_tr_t25, observed).
narrative_ontology:measurement(unclos_hybrid_reading_tr_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(unclos_hybrid_reading_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(unclos_hybrid_reading_be_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(unclos_hybrid_reading_be_t0, observed).
narrative_ontology:measurement(unclos_hybrid_reading_be_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement_basis(unclos_hybrid_reading_be_t5, observed).
narrative_ontology:measurement(unclos_hybrid_reading_be_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(unclos_hybrid_reading_be_t10, observed).
narrative_ontology:measurement(unclos_hybrid_reading_be_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement_basis(unclos_hybrid_reading_be_t15, observed).
narrative_ontology:measurement(unclos_hybrid_reading_be_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(unclos_hybrid_reading_be_t20, observed).
narrative_ontology:measurement(unclos_hybrid_reading_be_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement_basis(unclos_hybrid_reading_be_t25, observed).
narrative_ontology:measurement(unclos_hybrid_reading_be_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(unclos_hybrid_reading_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(unclos_hybrid_reading_su_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(unclos_hybrid_reading_su_t0, observed).
narrative_ontology:measurement(unclos_hybrid_reading_su_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(unclos_hybrid_reading_su_t5, observed).
narrative_ontology:measurement(unclos_hybrid_reading_su_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(unclos_hybrid_reading_su_t10, observed).
narrative_ontology:measurement(unclos_hybrid_reading_su_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(unclos_hybrid_reading_su_t15, observed).
narrative_ontology:measurement(unclos_hybrid_reading_su_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(unclos_hybrid_reading_su_t20, observed).
narrative_ontology:measurement(unclos_hybrid_reading_su_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement_basis(unclos_hybrid_reading_su_t25, observed).
narrative_ontology:measurement(unclos_hybrid_reading_su_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(unclos_hybrid_reading_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__expansive_construction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'UNCLOS maritime sovereignty' conflates three structurally distinct claims about feature-generated entitlement (epsilon-invariance decomposition into a constraint family): the strict reading (this file's upstream baseline — its natural-feature half restates strict's treaty-text premise), the hybrid reading (this file), and the expansive reading (downstream in practice — this reading's maturation pathway is the ratchet by which occupation outcomes drift toward expansive results without expansive's immediate-generation premise). Each file carries its own epsilon, beneficiaries, and victims; this file's epsilon (0.58, intermediate) is valid only for the graduated arrangement with the maturation clause — the strict sibling's arrangement would measure lower for the construction-capable seats (no acquiescence route) and the expansive sibling's higher (immediate generation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
