% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Hybrid Effective-Control Reading of Maritime Sovereignty Generation
 *   domain: international law/maritime governance/geopolitical strategy
 *
 * SUMMARY:
 *   The constraint is the hybrid effective-control reading of maritime
 *   sovereignty generation under the law of the sea: naturally formed
 *   features above water at high tide generate full territorial sea and EEZ;
 *   artificial features generate only 500-meter safety zones; and prolonged
 *   effective control of a feature, absent sustained challenge, may ripen
 *   into a territorial claim. This is ONE reading of the kernel
 *   unclos_maritime_sovereignty — the sibling readings
 *   (strict_geographic_reading, expansive_construction_reading) are separate
 *   stories with their own epsilon values, not positions folded into this
 *   one. The epsilon referent is the standing feature-based arrangement under
 *   contest, assessed by this reading's own lights: the reading endorses
 *   graduated sovereignty, and its authored epsilon reflects that endorsement
 *   applied to the arrangement as it actually operates — the maturation
 *   pathway is where this reading's own framework locates the burden. Claim
 *   and metrics are independent authored facts: claimed_type is tangled_rope
 *   from this seat because the arrangement demonstrably both allocates (every
 *   party's existing EEZ map runs through the natural-feature rule, including
 *   the victims') and accretes (capacity converts to sovereignty while the
 *   burden of interruption falls on the weak); the metrics are authored
 *   descriptively of its operation, not reconciled to the claim.
 *
 * KEY AGENTS:
 *   - construction_capable_regional_powers: agenda-setter and primary collector (institutional/arbitrage) — builds features, curates the control record, collects the widening entitlement envelope
 *   - militarily_weaker_claimant_states: primary target (moderate/constrained) — holds legitimate claims it cannot physically defend or interrupt
 *   - traditional_fishing_communities: primary target (powerless/trapped) — bears the daily enforcement cost where the claims are made real
 *   - international_shipping_industry: beneficiary (organized/mobile) — collects predictable lanes and open transit
 *   - freedom_of_navigation_naval_powers: beneficiary and cost-bearer (powerful/mobile) — collects access and credibility, pays in operational risk
 *   - small_island_developing_states: dual-positioned (organized/constrained) — subsidized by the natural-feature baseline, exposed by the maturation pathway
 *   - maritime_boundary_tribunals: analytical observer (institutional/analytical) — adjudicates; its awards are the formal interruption a maturation record must survive
 *   - coral_reef_conservation_organizations: excluded (organized/constrained) — documents the ecological cost, holds no seat in allocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.55).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.62).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "Hybrid Effective-Control Reading of Maritime Sovereignty Generation").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international law/maritime governance/geopolitical strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'c195a92f-6145-46af-8a38-ce09428a3892').
narrative_ontology:cs_kernel_codification('c195a92f-6145-46af-8a38-ce09428a3892', fixed_text).
narrative_ontology:cs_authority_grounding('c195a92f-6145-46af-8a38-ce09428a3892', lineage).
narrative_ontology:cs_interpretation_layer_present('c195a92f-6145-46af-8a38-ce09428a3892').
narrative_ontology:cs_reading_relation('c195a92f-6145-46af-8a38-ce09428a3892', unclos_maritime_sovereignty__strict_geographic_reading, coexists_with).
narrative_ontology:cs_reading_relation('c195a92f-6145-46af-8a38-ce09428a3892', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_axiom('c195a92f-6145-46af-8a38-ce09428a3892', foundational, natural_feature_entitlement_baseline).
narrative_ontology:cs_axiom_status(natural_feature_entitlement_baseline, holdable).
narrative_ontology:cs_axiom_grounding('c195a92f-6145-46af-8a38-ce09428a3892', natural_feature_entitlement_baseline, conventional).
narrative_ontology:cs_axiom('c195a92f-6145-46af-8a38-ce09428a3892', foundational, prescriptive_maturation_of_control).
narrative_ontology:cs_axiom_status(prescriptive_maturation_of_control, holdable).
narrative_ontology:cs_axiom_grounding('c195a92f-6145-46af-8a38-ce09428a3892', prescriptive_maturation_of_control, empirically_contingent).
narrative_ontology:cs_axiom('c195a92f-6145-46af-8a38-ce09428a3892', secondary, safety_zone_interim_protection).
narrative_ontology:cs_axiom_status(safety_zone_interim_protection, holdable).
narrative_ontology:cs_axiom_grounding('c195a92f-6145-46af-8a38-ce09428a3892', safety_zone_interim_protection, conventional).
narrative_ontology:cs_reference_frame('c195a92f-6145-46af-8a38-ce09428a3892', graduated_feature_entitlement_compromise).
narrative_ontology:cs_drift_state('c195a92f-6145-46af-8a38-ce09428a3892', post_arbitral_award_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c195a92f-6145-46af-8a38-ce09428a3892', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_regional_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_shipping_industry).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, traditional_fishing_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, freedom_of_navigation_naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, small_island_developing_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, small_island_developing_states).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__hybrid_effective_control_reading, prescription_by_acquiescence_doctrine).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__hybrid_effective_control_reading, graduated_feature_entitlement_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dredge and reclaim submerged reefs and low-tide elevations into fortified islands, garrison them, and administer them as territory: runways, emplacements, administrative districts, civilian tours. Maintain a standing record of control — patrols, registrations, standardized naming — designed to accumulate while challenges are weathered or waited out. Collect the widening envelope of waters these features command, and can reject adverse rulings while continuing the physical program. Exit is a change of strategy, not of position: the same capacity shifts between legal argument and completed facts.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_regional_powers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_regional_powers, beneficiary).

% Hold overlapping claims to the same features and waters, grounded in geography and history, with diplomatic standing and legal recourse but without the dredging fleets or blue-water navies to alter facts on the water. Their instruments are protest notes, arbitral proceedings, alliance balancing, and coast guard presence — each of which costs them and, at best, interrupts the accumulation they object to. They cannot leave the neighborhood; adverse physical change arrives whether or not they litigate.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimant_states, payer,
    moderate, generational, constrained, regional).

% Work the shoals and grounds their families have fished for generations, now inside declared safety zones and patrolled claimed waters. Face interdiction, water cannons, detention, and being driven off by enforcement vessels. Livelihood is tied to specific grounds; deep-water alternatives require boats and gear they do not have. They bear the day-to-day cost of the presence that maintains control, and have no seat in any forum where the claims are argued.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, traditional_fishing_communities, payer,
    powerless, biographical, trapped, local).

% Moves most of the world's seaborne trade through the sea lanes these regimes govern. Collects predictable zone boundaries, collision-avoidance rules, and open transit through contested waters. Can re-route around trouble spots at cost, and presses for navigation freedom through industry bodies and flag states.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_shipping_industry, beneficiary,
    organized, biographical, mobile, global).

% Project naval power globally and treat unimpeded transit as a core interest. Collect open sea lanes and alliance credibility from challenging excessive claims; pay for the challenge in operational tempo, standoffs, and escalation risk. Their presence is one of the few levers that raises the cost of the physical program without their owning any claim.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, freedom_of_navigation_naval_powers, beneficiary,
    powerful, generational, mobile, global).

% Exist as jurisdictions largely because natural features generate the wide maritime zones their economies and food security run on. The same rulebook that underwrites them lets any capable state manufacture new features near someone else's coast. They gain from the baseline and are exposed to the pathway, and can coordinate diplomatically but cannot physically contest.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, small_island_developing_states, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, small_island_developing_states, payer).

% Adjudicate feature status and entitlement when parties consent or are brought in: ITLOS, annex VII arbitral panels, boundary commissions. Render reasoned awards that bind consenting parties in law but carry no enforcement arm. An adverse award is one of the few acts that formally interrupts a control record; whether it changes anything on the water is a question their own dockets keep answering.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, maritime_boundary_tribunals, observer,
    institutional, generational, analytical, global).

% Document and protest the dredging that buries reef systems — thousands of hectares destroyed in single building campaigns — through environmental conventions and scientific assessment. Hold standing in biodiversity forums and none in sovereignty allocation; their objection is recorded, cited, and structurally outside the conversation that decides who gets the waters.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, coral_reef_conservation_organizations, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_regional_powers).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__hybrid_effective_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates maritime entitlement space among states with overlapping geography by a shared rule: natural above-water features anchor wide zones, artificial installations receive 500-meter safety zones, and possession held and administered over time without sustained challenge can ripen into recognized claim. Gives shipping predictable lane rules and gives claimants a common vocabulary for negotiation.
% TRANSFER_FUNCTION: Moves command over waters, fisheries, hydrocarbon prospects, and lane leverage from militarily weaker claimants and traditional users toward states with dredging capacity and naval projection; and moves the burden of interruption — continuous protest, litigation, patrol — onto the parties least able to sustain it.
% ABSENT_VOICES: Reef-dependent communities and conservation science have documented standing but no seat in sovereignty allocation; fishing communities of non-claimant states work the same grounds with no voice at all; future generations of claimant states inherit completed facts they were never positioned to contest. Their objections exist in the record — arbitral environmental findings among them — and outside the room where entitlements are decided.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand the built features without their legal pathway: garrisons would hold rocks with 500-meter zones and no maturation prospect, shipping would lose the zone vocabulary it routes by, weaker claimants would reopen claims from the natural baseline, and every party's existing EEZ map — drawn from natural features — would need renegotiation. Arrangements demonstrably run through this rule.
% FOUNDING_PROBLEM: The law of the sea needed to answer what artificial installations on reefs and low-tide elevations are worth: protect them for safety and navigation without letting any state manufacture sovereignty by pouring concrete. The hybrid reading's founding bargain: fixed limited zones for artificial works, plus the older general-law idea that possession held without challenge can ripen into title.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: the 2016 South China Sea annex VII award holds that artificial features on low-tide elevations and submerged features generate no maritime zones and are not capable of appropriation — a reasoned legal corroboration, from a tribunal a claimant state itself convened, that the maturation pathway lacks doctrinal foundation for the features it most invites. ITLOS jurisprudence and the law-of-the-sea literature corroborate the natural-feature baseline from outside any benefiting party. No corroborating source outside the states positioned to use maturation attests the maturation clause itself as live law; its attestors are its beneficiaries. That asymmetry is itself the signal.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.55 is intermediate by structure: the natural-feature half operates near a coordination baseline (geography as given, near-universal acceptance, negligible burden), while the maturation half carries the burden — capacity plus time converts to sovereignty, and the 'absent challenge' condition prices silence into the rule. Suppression 0.62 is authored as a raw structural property, unscaled by power or scope in this data (only extractiveness is scaled downstream, by directionality and scope): it combines physical enforcement (coast guard interdiction, militarized features, detention of fishers) with a softer mechanism — the acquiescence condition weaponizes silence, making continuous protest the price of stopping accumulation, a price weakest where claims are weakest. Theater_ratio 0.40: a growing share of activity is record-building performance — administrative districts declared, features named, tourist flights staged — whose function is to manufacture the effective-control evidentiary file; the construction and enforcement underneath remain real. Accessibility_collapse 0.38: alternatives persist (arbitration, ITLOS, negotiation, naval challenge, alliance balancing) but their efficacy is degraded — an adverse award is rejectable by the party it binds in fact. Resistance 0.68: arbitration proceedings, freedom-of-navigation operations, protest notes at every maturation step, and recurring confrontations at sea. The measurement series run on one shared seven-point grid (1994-2026) so every metric is authored at every examined time point. Suppression_requirement is authored as a series because enforcement-capacity change is the story's traced dynamic: the interval spans the build-up from occasional patrols to a purpose-built coast guard fleet, militarized outposts, and normalized interdiction — a rising enforcement ratchet, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently, and the engine computes this from the structural data rather than from the claim. From the construction-capable seat the arrangement is order it built and administers: it converted capital and patience into jurisdiction, by rules it can cite. From the weaker claimant seats the same structure operates as dispossession with a legal facade — they hold awards that change nothing on the water and bear a protest burden calibrated to their endurance. Small island developing states straddle: the baseline half is the foundation of their existence as zone-holding jurisdictions, the pathway half is the threat to it. Shipping experiences the arrangement as nearly pure coordination. The same four positional atoms, different computed types per seat. Coalition note: the weaker claimants and fishing communities could in principle pool resistance — they partially do, through regional declarations — but bilateral economic dependence on the constructing power and collective-action costs keep the coalition latent rather than effective.
 *
 * DIRECTIONALITY LOGIC:
 *   The construction-capable power is agenda-setter and collector: it runs the arrangement and the maturation gains demonstrably accrue to it, placing it near the beneficiary end (gain_flow names this seat). Traditional fishing communities are pure targets — no benefit channel, trapped exit, concentrated local burden — sitting near the full-target end. Militarily_weaker_claimant_states are victims of the maturation pathway but internal beneficiaries of the reading's own first clause: the same arrangement that burdens them subsidizes the natural-feature entitlements their economies run on. The derivation would read victims-plus-constrained-exit as near-full target; the directionality override (moderate atom to 0.7) damps for that internal subsidy, and no other moderate-atom agent shares the dual position. International shipping is a mobile beneficiary at low directionality. Freedom-of-navigation naval powers collect access and credibility while paying voluntary challenge costs — mid-low. Small island developing states sit mid: subsidized baseline, exposed pathway. Tribunals are analytical observers whose awards function as the formal interruption a maturation record must survive. Reef conservation organizations are excluded: they bear diffuse ecological costs with no allocation lever.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — let installations be protected without letting construction manufacture sovereignty — is contested, not dead: the strict camp holds it answered by the treaty text and the 2016 award, while the arrangement on the water continues to perform its allocation function daily for every party, shipping included. This is therefore not a resolved mandatrophy, but the mismatch flag is live: if the founding problem is doctrinally dead (the strict reading prevails) while the world keeps rearranging around the maturation pathway, the arrangement persists as capacity-converted title with its allocation half as cover. The tangled_rope classification prevents both mislabels: reading it as pure coordination (the law-of-the-sea-as-order story) misses the accretion burden the pathway places on the weak; reading it as pure extraction misses that the victims themselves hold their own entitlements — their EEZs, their fisheries — through the very structure that burdens them. The coordination and the accretion are the same structure, which is exactly the hybrid's signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kernel unclos_maritime_sovereignty — the hybrid_effective_control_reading. What do the sibling readings change structurally, and where exactly does the disagreement sit?',
    'Comparative classification of the sibling stories: strict_geographic_reading deletes the maturation pathway entirely (construction never alters legal status; only natural high-tide features generate zones); expansive_construction_reading deletes both the natural/artificial distinction and the maturation condition (occupation and administration alone generate de facto waters). The disagreement is located in two structural elements: whether feature type limits what control can generate, and whether time-plus-acquiescence can substitute for geography as a sovereignty-generating fact.',
    'Under the strict sibling the burden shifts from allocation to enforcement of the rule against capable states; under the expansive sibling the maturation condition drops out and the burden on weaker claimants rises sharply. This story''s intermediate epsilon is indexed to the hybrid''s graduated structure and is not comparable across readings without that indexing — cross-reading epsilon comparison is a category error the family decomposition exists to prevent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification: this epsilon belongs to the hybrid effective-control reading, not to the maritime-sovereignty kernel itself.').

omega_variable(
    maturation_threshold_ambiguity,
    'What quantum and duration of control, and what quality of challenge, does ''prolonged effective control absent challenge'' require — and does a diplomatic note, an arbitral award, or a standoff at sea interrupt the clock?',
    'Tribunal articulation of prescription criteria, or crystallized state practice on what interrupts acquiescence; comparative study of features where challenge was continuous versus episodic versus absent, holding construction constant.',
    'A demanding threshold collapses most maturation claims and moves this reading toward its strict sibling; a lax threshold converts it toward the expansive sibling. The vagueness is load-bearing: the constructing state curates the control record while the challenger bears the continuous-protest burden, so the undefined threshold itself allocates cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maturation_threshold_ambiguity, conceptual, 'The maturation condition is undefined, and the ambiguity prices interruption onto the weaker party.').

omega_variable(
    challenge_cost_asymmetry,
    'Is the ''absent challenge'' condition structurally neutral — any protest suffices, once — or does it operate as a cost-transfer mechanism, ripening claims against parties least able to sustain continuous, escalating protest?',
    'Compare maturation outcomes across features where weaker claimants protested continuously versus intermittently versus not at all, controlling for construction scale and duration; measure whether episodic protest actually resets accumulation or merely delays it.',
    'If cost-asymmetric, silence is penalized and the measured burden on weaker seats rises above what the allocation rules alone predict, pushing per-seat classifications toward the extractive end for those seats; if neutral, the pathway is closer to ordinary acquisitive prescription and the intermediate epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(challenge_cost_asymmetry, empirical, 'Whether the acquiescence condition functionally prices the weak out of interrupting accumulation.').

omega_variable(
    natural_baseline_naturality,
    'Is the reading''s first clause — natural features generate full entitlements — a given of geography all parties accept as coordination baseline, or does it too carry constructed interests (continental shape, decolonization-era boundary draws) that particular states are simply lucky under?',
    'Comparative acceptance analysis: examine entitlement disputes where the natural-feature rule favors each party in turn, and whether challenges target the rule itself rather than only feature classification.',
    'If the baseline is itself contested, part of what this story attributes to the maturation pathway is baseline contest, and the reading''s coordination half — and its claim to genuine allocation function — is smaller than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_baseline_naturality, conceptual, 'Whether the natural-feature half is genuinely common ground or constructed luck.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 1994, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1994, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 1994, 0.15).
narrative_ontology:measurement_basis(uncl_tr_t1994, observed).
narrative_ontology:measurement(uncl_tr_t2000, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement_basis(uncl_tr_t2000, observed).
narrative_ontology:measurement(uncl_tr_t2006, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2006, 0.24).
narrative_ontology:measurement_basis(uncl_tr_t2006, observed).
narrative_ontology:measurement(uncl_tr_t2012, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2012, 0.3).
narrative_ontology:measurement_basis(uncl_tr_t2012, observed).
narrative_ontology:measurement(uncl_tr_t2016, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2016, 0.36).
narrative_ontology:measurement_basis(uncl_tr_t2016, observed).
narrative_ontology:measurement(uncl_tr_t2021, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2021, 0.4).
narrative_ontology:measurement_basis(uncl_tr_t2021, observed).
narrative_ontology:measurement(uncl_tr_t2026, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2026, 0.4).
narrative_ontology:measurement_basis(uncl_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1994, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 1994, 0.28).
narrative_ontology:measurement_basis(uncl_be_t1994, observed).
narrative_ontology:measurement(uncl_be_t2000, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2000, 0.31).
narrative_ontology:measurement_basis(uncl_be_t2000, observed).
narrative_ontology:measurement(uncl_be_t2006, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2006, 0.36).
narrative_ontology:measurement_basis(uncl_be_t2006, observed).
narrative_ontology:measurement(uncl_be_t2012, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2012, 0.44).
narrative_ontology:measurement_basis(uncl_be_t2012, observed).
narrative_ontology:measurement(uncl_be_t2016, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2016, 0.5).
narrative_ontology:measurement_basis(uncl_be_t2016, observed).
narrative_ontology:measurement(uncl_be_t2021, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2021, 0.53).
narrative_ontology:measurement_basis(uncl_be_t2021, observed).
narrative_ontology:measurement(uncl_be_t2026, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2026, 0.55).
narrative_ontology:measurement_basis(uncl_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1994, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 1994, 0.25).
narrative_ontology:measurement_basis(uncl_su_t1994, observed).
narrative_ontology:measurement(uncl_su_t2000, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement_basis(uncl_su_t2000, observed).
narrative_ontology:measurement(uncl_su_t2006, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2006, 0.33).
narrative_ontology:measurement_basis(uncl_su_t2006, observed).
narrative_ontology:measurement(uncl_su_t2012, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2012, 0.42).
narrative_ontology:measurement_basis(uncl_su_t2012, observed).
narrative_ontology:measurement(uncl_su_t2016, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2016, 0.52).
narrative_ontology:measurement_basis(uncl_su_t2016, observed).
narrative_ontology:measurement(uncl_su_t2021, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement_basis(uncl_su_t2021, observed).
narrative_ontology:measurement(uncl_su_t2026, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2026, 0.62).
narrative_ontology:measurement_basis(uncl_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, expansive_construction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (what generates maritime sovereignty under the law of the sea), three readings with different epsilon. strict_geographic_reading: lowest epsilon as allocation law — geography as given, no construction pathway — but shifts the burden to enforcing the rule against capable states. expansive_construction_reading: highest epsilon — capacity converts to sovereignty directly, no challenge mechanism protects the weak. This hybrid reading: intermediate epsilon — capacity converts slowly, and the 'absent challenge' condition transfers the interruption burden to the weaker party. The natural-language label 'maritime sovereignty under UNCLOS' conflates these three structurally distinct claims; each story carries its own epsilon, beneficiaries, and victims, linked here. The upstream story (strict) is cited as authority against the downstream (expansive), with this hybrid occupying the contested middle. Epsilon values are not comparable across the family without reading-indexing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
